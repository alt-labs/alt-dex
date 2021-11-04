{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module SimulatorActions
    (
        altswapMintingContract,
        altswapStartContract,
        altswapUserContract,
        altswapLiquidityPoolContract
    ) where

import           Control.Monad                           (forM, void)
import           Control.Monad.Freer                     (Eff, Member, interpret, type (~>))
import           Control.Monad.Freer.Error               (Error)
import           Control.Monad.Freer.Extras.Log          (LogMsg)
import           Control.Monad.IO.Class                  (MonadIO (..))
import qualified Data.Aeson.Encode.Pretty                as JSON
import           Data.Aeson                              (FromJSON, Result (..), ToJSON, encode, fromJSON)
import qualified Data.Map.Strict                         as Map
import qualified Data.Monoid                             as Monoid
import qualified Data.Semigroup                          as Semigroup
import           Data.Text                               (Text)
import           GHC.Generics                            (Generic)
import           Ledger.Ada                              (adaSymbol, adaToken)
import           Plutus.Contract
import           AltDex.Contracts.Base
import qualified AltDex.Contracts.Monetary               as Monetary
import qualified AltDex.Contracts.Swap                   as AltSwap
import           AltDex.Contracts.Swap                   (AltSwapContracts (..))
import           AltDex.Contracts.OffChain               (UserContractState (..), CreateParams (..))
import           Plutus.PAB.Effects.Contract             (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin     (Builtin, SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin     as Builtin
import           Plutus.PAB.Monitoring.PABLogMsg         (PABMultiAgentMsg)
import Plutus.PAB.Simulator ( SimulatorEffectHandlers, Simulation )
import qualified Plutus.PAB.Simulator                    as Simulator
import           Plutus.PAB.Types                        (PABError (..))
import qualified Plutus.PAB.Webserver.Server             as PAB.Server
import qualified Plutus.PAB.Webserver.Handler            as Webserver
import           Plutus.PAB.Webserver.Types              (ContractSignatureResponse, FullReport)
import           Prelude                                 hiding (init)
import           Wallet.Emulator.Types                   (Wallet (..), knownWallets, knownWallet)
import qualified Data.ByteString.Lazy                    as BSL
import           Data.Text.Prettyprint.Doc
import qualified Data.Text                                  as Text
import           Wallet.Types                               (ContractInstanceId (..))
import           Playground.Types                            (FunctionSchema, Simulation)
import           Schema                                      (FormSchema)
import System.Directory                                  (getCurrentDirectory)
import System.FilePath                                   ((</>))
import Plutus.PAB.Core                                   (PABEffects, PABAction)
import Ledger                                            (CurrencySymbol)

import qualified Control.Concurrent.STM          as STM
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import Control.Concurrent (threadDelay, MVar, newEmptyMVar, putMVar, tryTakeMVar)
import Data.Maybe (isJust)

altswapMintingContract :: Eff (PABEffects
     (Builtin AltSwapContracts)
     (Simulator.SimulatorState (Builtin AltSwapContracts)))
  (ContractInstanceId, CurrencySymbol)
altswapMintingContract = do
    cidInit  <- Simulator.activateContract (knownWallet 1) Init
    cs       <- flip Simulator.waitForState cidInit $ \json -> case fromJSON json of
                    Success (Just (Semigroup.Last cur)) -> Just $ Monetary.currencySymbol cur
                    _                                   -> Nothing
    _        <- Simulator.waitUntilFinished cidInit

    Simulator.logString @(Builtin AltSwapContracts) $ "Initialization finished. Minted: " ++ show cs

    return (cidInit, cs)

altswapStartContract :: Eff (PABEffects
     (Builtin AltSwapContracts)
     (Simulator.SimulatorState (Builtin AltSwapContracts)))
  (ContractInstanceId, AltSwap)
altswapStartContract = do
    cidStart <- Simulator.activateContract (knownWallet 1) AltSwapStart
    us       <- flip Simulator.waitForState cidStart $ \json -> case (fromJSON json :: Result (Monoid.Last (Either Text AltSwap))) of
                    Success (Monoid.Last (Just (Right us))) -> Just us
                    _                                       -> Nothing
    Simulator.logString @(Builtin AltSwapContracts) $ "AltSwap instance created: " ++ show us

    pure (cidStart, us)

altswapUserContract us =
    fmap Map.fromList $ forM knownWallets $ \w -> do
    cid <- Simulator.activateContract w $ AltSwapUser us
    Simulator.logString @(Builtin AltSwapContracts) $ "AltSwap user contract started for " ++ show w
    Simulator.waitForEndpoint cid "funds"
    _ <- Simulator.callEndpointOnInstance cid "funds" ()
    v <- flip Simulator.waitForState cid $ \json -> case (fromJSON json :: Result (Monoid.Last (Either Text UserContractState))) of
            Success (Monoid.Last (Just (Right (Funds v)))) -> Just v
            _                                                      -> Nothing
    Simulator.logString @(Builtin AltSwapContracts) $ "initial funds in wallet " ++ show w ++ ": " ++ show v
    return (w, cid)

altswapLiquidityPoolContract cids cs = do
    let coins = Map.fromList [(tn, Monetary.mkCoin cs tn) | tn <- AltSwap.tokenNames]
        ada   = Monetary.mkCoin adaSymbol adaToken

    let cp = CreateParams ada (coins Map.! "A") 100000 500000
    Simulator.logString @(Builtin AltSwapContracts) $ "creating liquidity pool: " ++ show (encode cp)
    -- _  <- Simulator.callEndpointOnInstance (cids Map.! knownWallet 2) "create" cp
    let cid2 = cids Map.! knownWallet 2
    Simulator.waitForEndpoint cid2 "create"

    _  <- Simulator.callEndpointOnInstance cid2 "create" cp
    flip Simulator.waitForState (cids Map.! knownWallet 2) $ \json -> case (fromJSON json :: Result (Monoid.Last (Either Text UserContractState))) of
        Success (Monoid.Last (Just (Right Created))) -> Just ()
        _                                                    -> Nothing

    Simulator.logString @(Builtin AltSwapContracts) "liquidity pool created"
