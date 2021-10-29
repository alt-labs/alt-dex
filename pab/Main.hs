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

module Main
    ( main
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
import           Data.Text.Encoding                      ( encodeUtf8 )
import           GHC.Generics                            (Generic)
import           Ledger.Ada                              (adaSymbol, adaToken)
import           Plutus.Contract
import qualified AltDex.Contracts.Monetary               as Monetary
import qualified AltDex.Contracts.Swap                   as Swap
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
import Ledger.Value                                      (unCurrencySymbol)
import Plutus.Contracts.Data

import qualified Control.Concurrent.STM          as STM
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import Control.Concurrent (threadDelay, MVar, newEmptyMVar, putMVar, tryTakeMVar)
import Data.Maybe (isJust)
import SimulatorActions
-- import Control.Lens.Indexed (ifor_)
import Data.Foldable (for_)
import Data.Map (Map,toList)

cidFile :: Wallet -> FilePath
cidFile w = "./tmp/W" ++ show (getWallet w) ++ ".cid"

cidsToFile :: Map Wallet ContractInstanceId -> IO ()
cidsToFile cids = do
  for_ (toList cids) $ \(w, cid) -> do
    let text = Text.pack $ show $ unContractInstanceId cid
    let bs = BSE.encode BSE.utf8 text
    BS.writeFile (cidFile w) bs

startDebugServer :: MVar ()
    -> Eff
     (PABEffects
        (Builtin AltSwapContracts)
        (Simulator.SimulatorState (Builtin AltSwapContracts)))
     (FullReport AltSwapContracts,
      ContractSignatureResponse AltSwapContracts)
startDebugServer stop = do
    Simulator.logString @(Builtin AltSwapContracts) "Starting AltSwap PAB webserver on port 8080. Press enter to exit."

    shutdown <- PAB.Server.startServerDebug

    -- UNI: MINT
    (cidInit, cs) <- aswpMintingContract
    -- liftIO $ BSL.writeFile "./tmp/symbol.json" $ encode cs
    liftIO $ BS.writeFile "./tmp/symbol" $ BSE.encode BSE.utf8 $ Text.pack $ show cs

    -- UNI: START
    (cidStart, us) <- aswpStartContract

    -- UNI: USER
    cids <- aswpUserContract us
    liftIO $ cidsToFile cids

    -- UNI: POOL
    void (aswpLiquidityPoolContract cids cs)

    _ <- liftIO $ loop stop
    shutdown

    report :: FullReport AltSwapContracts <- Webserver.getFullReport
    schema :: ContractSignatureResponse AltSwapContracts <- Webserver.contractSchema (Text.pack $ show $ unContractInstanceId cidInit)
    pure (report, schema)

startServer :: MVar () -> IO
  (FullReport AltSwapContracts,
   ContractSignatureResponse AltSwapContracts)
startServer stop =
    either (error . show) id <$> Simulator.runSimulationWith handlers (startDebugServer stop)

onSigInt :: MVar () -> IO ()
onSigInt stop = do
  putStrLn "got sigINT"
  putMVar stop ()

onSigTerm :: MVar () -> IO ()
onSigTerm stop = do
  putStrLn "got sigTERM"
  putMVar stop ()

main :: IO ()
main = do
    stop <- newEmptyMVar
    _ <- installHandler sigINT (Catch $ onSigInt stop) Nothing
    _ <- installHandler sigTERM (Catch $ onSigTerm stop) Nothing

    (fullReport, currencySchema) <- startServer stop

    outputDir <- getCurrentDirectory
    print outputDir
    BSL.writeFile
        (outputDir </> "./log/full_report_response.json")
        (JSON.encodePretty fullReport)

    print outputDir
    BSL.writeFile
        (outputDir </> "./log/currency_schema.json")
        (JSON.encodePretty currencySchema)

loop :: MVar () -> IO ()
loop stop = do
  stopRequested <- isJust <$> tryTakeMVar stop
  if stopRequested
    then putStrLn "stop :: MVar — mutated! halting..."
    else do
      threadDelay 1000000
      loop stop

handleAltSwapContract ::
    ( Member (Error PABError) effs
    , Member (LogMsg (PABMultiAgentMsg (Builtin AltSwapContracts))) effs
    )
    => ContractEffect (Builtin AltSwapContracts)
    ~> Eff effs
handleAltSwapContract = Builtin.handleBuiltin getSchema getContract where
  getSchema = \case
    AltSwapUser _ -> Builtin.endpointsToSchemas @AltSwap.AltSwapUserSchema
    AltSwapStart  -> Builtin.endpointsToSchemas @AltSwap.AltSwapOwnerSchema
    Init          -> Builtin.endpointsToSchemas @Empty
  getContract = \case
    AltSwapUser us -> SomeBuiltin $ AltSwap.userEndpoints us
    AltSwapStart   -> SomeBuiltin AltSwap.ownerEndpoint
    Init           -> SomeBuiltin Swap.initContract

handlers :: SimulatorEffectHandlers (Builtin AltSwapContracts)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin AltSwapContracts) [] -- [Init, AltSwapStart, AltSwapUser ???]
    $ interpret handleAltSwapContract