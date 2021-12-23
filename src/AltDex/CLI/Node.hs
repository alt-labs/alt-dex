{-# LANGUAGE FlexibleContexts #-}

module AltDex.CLI.Node
( getCurrentSlotSynced, showSyncStatus )
  where

import Data.Time.Clock.POSIX
import Control.Monad.Reader
import Ledger hiding (getPOSIXTime)
import Prelude hiding (print)
import Control.Monad.Reader ( MonadReader, MonadIO(..), asks )
import Cardano.Api
import Ledger hiding (getPOSIXTime)
import AltDex.CLI.Environment
import AltDex.CLI.Common (printLn)

getCurrentSlotSynced :: (MonadIO m) => LocalNodeConnectInfo CardanoMode -> m Slot
getCurrentSlotSynced nodeInfo = do
    liftIO (getLocalChainTip nodeInfo)
        >>= \case
            ChainTipAtGenesis -> error "Got ChainTipAtGenesis as a last Slot..."
            ChainTip (SlotNo slot) _ _ -> (return . Slot . fromIntegral ) slot

showSyncStatus ::Environment -> IO ()
showSyncStatus environment = do
    syncSlot@(Slot synSlotAsInt) <- getCurrentSlotSynced $ localNodeConnectInfo environment
    let syncTime  = toPosixTime environment syncSlot
    localTime <- liftIO getPOSIXTime
    printLn $ "\t-Last Slot Synced : " <> show synSlotAsInt <> " (ᛝ " <> show (localTime - syncTime)  <> " s)"
    printLn $ "\t-Local Time : " <> formatISO8601 localTime
    printLn $ "\t-Last Sync  : " <> formatISO8601 syncTime
