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

module AltDex.Roundtrip.PAB.Contracts
    ( AltSwapDemoContracts
    ) where

import Prelude
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.OpenApi.Schema qualified as OpenApi
import Prettyprinter

import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (..), HasDefinitions (..), SomeBuiltin (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run.PSGenerator (HasPSTypes (..))
import Ledger (PubKeyHash(..))
import Language.PureScript.Bridge (equal, genericShow, mkSumType, order)

import           Data.Proxy                              (Proxy (..))
import           Control.Monad                           (forM, void)
import           Control.Monad.Freer                     (Eff, Member, interpret, type (~>))
import           Control.Monad.Freer.Error               (Error)
import           Control.Monad.Freer.Extras.Log          (LogMsg)
import           Control.Monad.IO.Class                  (MonadIO (..))
import           Data.Default                            (Default (def))
import qualified Data.Aeson.Encode.Pretty                as JSON
import           Data.Aeson                              (FromJSON, Result (..), ToJSON, encode, fromJSON)
import qualified Data.Map.Strict                         as Map
import qualified Data.Monoid                             as Monoid
import qualified Data.Semigroup                          as Semigroup
import           Data.Text                               (Text)
import           Data.Text.Encoding                      ( encodeUtf8 )
import           GHC.Generics                            (Generic)
import           Ledger.Ada                              (adaSymbol, adaToken)
import           Plutus.Contract
import qualified AltDex.Contracts.Monetary               as Monetary
import qualified AltDex.Contracts.Swap                   as AltSwap
import           AltDex.Contracts.OffChain
import           Plutus.PAB.Effects.Contract             (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin     (Builtin, BuiltinHandler (..), HasDefinitions (..),
                                                          SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin     as Builtin
import           Plutus.PAB.Monitoring.PABLogMsg         (PABMultiAgentMsg)
import Plutus.PAB.Simulator ( SimulatorEffectHandlers, Simulation )
import qualified Plutus.PAB.Simulator                    as Simulator
import           Plutus.PAB.Types                        (PABError (..))
import qualified Plutus.PAB.Webserver.Server             as PAB.Server
import qualified Plutus.PAB.Webserver.Handler            as Webserver
import           Plutus.PAB.Webserver.Types              (ContractSignatureResponse, FullReport)
import           Prelude                                 hiding (init)
import           Wallet.Emulator.Types                   (Wallet (..))
import qualified Data.ByteString.Lazy                    as BSL
import qualified Data.ByteString.Encoding                as BSE
import qualified Data.ByteString                         as BS
import           Data.Text.Prettyprint.Doc
import qualified Data.Text                                  as Text
import           Wallet.Types                               (ContractInstanceId (..))
import           Playground.Types                            (FunctionSchema, Simulation)
import           Schema                                      (FormSchema)
import System.Directory                                  (getCurrentDirectory)
import System.FilePath                                   ((</>))
import Plutus.PAB.Core                                   (PABEffects, PABAction)
import Ledger                                            (CurrencySymbol)

import           Ledger.Fee                                     (FeeConfig)
import qualified Ledger.Index                                   as UtxoIndex
import           Ledger.TimeSlot                                (SlotConfig (..))

import Ledger.Value                                      (unCurrencySymbol)

import qualified Control.Concurrent.STM          as STM
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import Control.Concurrent (threadDelay, MVar, newEmptyMVar, putMVar, tryTakeMVar)
import Data.Maybe (isJust)
-- import Control.Lens.Indexed (ifor_)
import Data.Foldable (for_)
import Data.Map (Map,toList)

import AltDex.Contracts.Base (AltSwap)
-- import AltDex.Contracts.Swap

import Data.Text                  as T
import Data.Text.Lazy             as LT

-- import Mlabs.Roundtrip.PKH qualified as Mlabs.Contracts
-- import Mlabs.Roundtrip.RountripP2W qualified as Mlabs.Contracts
-- import Mlabs.Roundtrip.RoundtripSpending qualified as Mlabs.Contracts
import AltDex.Roundtrip.MintTokens
-- import AltDex.Roundtrip.CreatePool

data AltSwapDemoContracts =
      Init
    | AltSwapStart
    | AltSwapUser AltSwap
    | Mint
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty AltSwapDemoContracts where
    pretty = viaShow

instance HasPSTypes AltSwapDemoContracts where
    psTypes p =
        [ (equal <*> (genericShow <*> mkSumType)) p
        -- These types come from the Altswap contract and need to be available in PS
        , (equal <*> (genericShow <*> mkSumType)) (Proxy @AltSwap)
        , (equal <*> (genericShow <*> mkSumType)) (Proxy @(Monetary.Coin Monetary.A))
        , (equal <*> (genericShow <*> mkSumType)) (Proxy @Monetary.SwapCoin)
        ]

instance HasDefinitions AltSwapDemoContracts where
  getDefinitions = [Init, AltSwapStart]
  getSchema = \case
    AltSwapUser _ -> Builtin.endpointsToSchemas @AltSwapUserSchema
    AltSwapStart  -> Builtin.endpointsToSchemas @AltSwapOwnerSchema
    Init          -> Builtin.endpointsToSchemas @Empty
    Mint          -> Builtin.endpointsToSchemas @Monetary.CurrencySchema
  getContract = \case
    AltSwapUser altswap -> SomeBuiltin . awaitPromise $ userEndpoints altswap
    AltSwapStart        -> SomeBuiltin ownerEndpoint
    Init                -> SomeBuiltin AltSwap.initContract
    Mint                -> SomeBuiltin $ demoMintingContract
