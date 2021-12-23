{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module AltDex.CLI.Environment (
      getTestnetEnvironmment
    , getMainnetEnvironmment
    , Environment (..)
    , toPosixTime
    , getFirstShelleySlot
    , getFirstShelleySlotTime
    , convertToInternalPosix
    , convertToExternalPosix
    , formatISO8601
    , testnetLocalNodeConnInfo) where

import Control.Monad.Reader ( MonadIO(..), MonadReader(ask) )

import Cardano.Api
  ( executeLocalStateQueryExpr,
    queryExpr,
    LocalNodeConnectInfo(..),
    CardanoMode,
    ConsensusModeParams(CardanoModeParams),
    QueryInMode(QuerySystemStart),
    EpochSlots(EpochSlots),
    NetworkMagic(NetworkMagic))
import qualified Cardano.Api
import qualified Cardano.Api.Shelley


import qualified Cardano.Api.Shelley  as Shelley
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..) )
import           Ledger ( Slot(Slot), POSIXTime(POSIXTime) )

import           Data.Coerce ( coerce )

import qualified Data.Time.ISO8601     as ExternalPosix
import qualified Data.Time.Clock.POSIX as ExternalPosix
import qualified Data.Time.Clock       as ExternalPosix

import           System.Environment (getEnv)
import           Prelude

data Environment = Testnet
                  { magicNumber :: Integer
                  , localNodeConnectInfo :: LocalNodeConnectInfo CardanoMode
                  , preShelleyEpochs :: Integer
                  , byronSlotsPerEpoch :: Integer
                  , byronSecondsPerSlot :: Integer
                  , systemStart :: ExternalPosix.POSIXTime }
              |  Mainnet
                  { magicNumber :: Integer
                  , localNodeConnectInfo :: LocalNodeConnectInfo CardanoMode
                  , preShelleyEpochs :: Integer
                  , byronSlotsPerEpoch :: Integer
                  , byronSecondsPerSlot :: Integer
                  , systemStart :: ExternalPosix.POSIXTime }

getMainnetEnvironmment :: MonadIO m => Integer -> m Environment
getMainnetEnvironmment magicNumber = do
    socketPath <- liftIO $ getEnv "CARDANO_NODE_SOCKET_PATH"
    let localNodeConnectInfo = LocalNodeConnectInfo {
                                        localConsensusModeParams = CardanoModeParams (EpochSlots 21600),
                                        localNodeNetworkId       = Shelley.Mainnet,
                                        localNodeSocketPath      = socketPath}
        preShelleyEpochs = 208
        byronSlotsPerEpoch = 21600
        byronSecondsPerSlot = 20
    systemStart <- ExternalPosix.utcTimeToPOSIXSeconds . coerce <$> getSystemStart' localNodeConnectInfo

    return $ Mainnet {..}

getTestnetEnvironmment :: MonadIO m => Integer -> m Environment
getTestnetEnvironmment magicNumber = do
    socketPath <- liftIO $ getEnv "CARDANO_NODE_SOCKET_PATH"
    let localNodeConnectInfo = LocalNodeConnectInfo {
                                        localConsensusModeParams = CardanoModeParams (EpochSlots 21600),
                                        localNodeNetworkId       = Shelley.Testnet  (NetworkMagic (fromIntegral magicNumber)),
                                        localNodeSocketPath      = socketPath}
        preShelleyEpochs = 74
        byronSlotsPerEpoch = 21600
        byronSecondsPerSlot = 20
    systemStart <- ExternalPosix.utcTimeToPOSIXSeconds . coerce <$> getSystemStart' localNodeConnectInfo

    return $ Testnet {..}

getSystemStart' :: MonadIO m => LocalNodeConnectInfo mode -> m SystemStart
getSystemStart' localNodeConnectInfo = do
    liftIO $ executeLocalStateQueryExpr localNodeConnectInfo Nothing (\_ -> queryExpr QuerySystemStart)
        >>= \case
                Left x -> error $ show x
                Right systemStart -> return systemStart

toPosixTime :: Environment ->  Slot -> ExternalPosix.POSIXTime
toPosixTime environment slot = do
    let shelleyDurationInS = toShelleyDurationInS environment slot
    let byronDurationInS = getTotalByronDurationInS environment
    systemStart environment + ExternalPosix.secondsToNominalDiffTime (fromIntegral (byronDurationInS + shelleyDurationInS))

toShelleyDurationInS :: Environment -> Slot -> Integer
toShelleyDurationInS environment slot = do
    let firstShelleySlot = getFirstShelleySlot environment
    coerce (slot - firstShelleySlot)

getFirstShelleySlot :: Environment -> Slot
getFirstShelleySlot environment = do
    Slot (byronSlotsPerEpoch environment * preShelleyEpochs environment)

getTotalByronDurationInS :: Environment -> Integer
getTotalByronDurationInS environment = do
    let firstShelleySlot = getFirstShelleySlot environment
    coerce $ firstShelleySlot * fromIntegral (byronSecondsPerSlot environment)

getFirstShelleySlotTime :: Environment -> ExternalPosix.POSIXTime
getFirstShelleySlotTime environment = do
    let byronDurationInS = getTotalByronDurationInS environment
    systemStart environment + ExternalPosix.secondsToNominalDiffTime (fromIntegral byronDurationInS)

convertToInternalPosix :: ExternalPosix.POSIXTime -> POSIXTime
convertToInternalPosix = POSIXTime . (* 1000) . truncate -- losing the milliseconds precision.

convertToExternalPosix :: POSIXTime -> ExternalPosix.POSIXTime
convertToExternalPosix p = ExternalPosix.secondsToNominalDiffTime (fromIntegral p / 1000.0)

formatISO8601 :: ExternalPosix.POSIXTime -> String
formatISO8601 = ExternalPosix.formatISO8601 . ExternalPosix.posixSecondsToUTCTime

testnetLocalNodeConnInfo :: FilePath -> LocalNodeConnectInfo CardanoMode
testnetLocalNodeConnInfo = LocalNodeConnectInfo (CardanoModeParams (EpochSlots 21600))  (Cardano.Api.Testnet $ NetworkMagic 1097911063)