
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}

module AltDex.Contracts.Serialise(
      zltTokenName, dktTokenName,
      tokensMinterTxOutRef, tokensCurrencySymbol,
      writeTokensMintingScript, showMeTehDatum, tokensCurrency,
      tokensMintingScript,
      tokensMintingValidatorAsCBOR,
      swapFactoryCoin, swapFactoryTxOutRef,
      showSwapFactoryEmptyData, showSwapFactoryExampleDataJSON,
      swapFactoryInstance, swapFactoryNFTCurrencySymbol,
      swapInstance,
      validatorAsCBOR,
      swapTokenScript, swapTokenScriptShortBs,
      writeSwapScript, writeSwapFactoryNFTMintingScript,
      lp, lpToken, lpCoin, lpStateCoin, mintLpTokensData, writeLpMintingScript,
      showSwapRedeemerJSON, showAfterCreatePoolDatum, showAfterCreatePooFactorylDatum,
      main
    ) where

import           Control.Lens
import qualified Data.Aeson                     as JSON
import qualified Data.OpenApi.Schema            as OpenApi
import qualified Data.Traversable               as DT
import           Plutus.Contract                as Contract
import           Plutus.Contract.Wallet         (getUnspentOutput)
import qualified PlutusTx
import           PlutusTx.Prelude               hiding (Monoid (..),
                                                 Semigroup (..))

import           Ledger                         (CurrencySymbol, PubKeyHash,
                                                 TxId, TxOutRef (..),
                                                 pubKeyHash, pubKeyHashAddress,
                                                 scriptCurrencySymbol, txId)
import qualified Ledger.Constraints             as Constraints
import qualified Ledger.Contexts                as V
import           Ledger.Scripts
import           Ledger.Value                   (AssetClass (..), TokenName,
                                                 Value, assetClass,
                                                 assetClassValue,
                                                 assetClassValueOf)

import qualified Ledger.Typed.Scripts           as Scripts
import qualified Ledger.Value                   as Value

import           Data.Semigroup                 (Last (..))

import           GHC.Generics                   (Generic)

import qualified PlutusTx.AssocMap              as AssocMap
import           Prelude                        (Functor (..), Semigroup (..))
import qualified Prelude                        as Haskell


import           Schema                         (ToSchema)

import           Text.Printf                    (PrintfArg)

import           Wallet.Emulator.Wallet

import           Plutus.Trace.Emulator          (EmulatorRuntimeError (GenericError),
                                                 EmulatorTrace)
import qualified Plutus.Trace.Emulator          as Emulator

import           Control.Monad                  (forever)

import           Cardano.Api.Shelley            (PlutusScript (..), PlutusScriptV1, ScriptData)
import           Codec.Serialise
import qualified Data.ByteString.Lazy           as LB
import qualified Data.ByteString.Short          as SBS

-- import           Cardano.Api
import           Cardano.Api.Shelley            (ScriptDataJsonSchema (..),
                                                 displayError, fromPlutusData,
                                                 scriptDataToJson, toAlonzoData,
                                                 writeFileTextEnvelope)

import qualified Cardano.Ledger.Alonzo.Data     as Alonzo
import qualified Plutus.V1.Ledger.Api           as Plutus


import qualified AltDex.Contracts.Base          as Base
import qualified AltDex.Contracts.LiquidityPool as LP
import qualified AltDex.Contracts.Monetary      as Monetary
import qualified AltDex.Contracts.OffChain      as OffChain
import qualified AltDex.Contracts.Swap          as Swap
import qualified Data.ByteString.Lazy.Char8     as LB

type FiniteCurency = Monetary.LimitedSupplyCurrency

-------------------------------------------------------------------------------
-- SWAP EXCHANGE TOKENS MINTING SCRIPT
-------------------------------------------------------------------------------
ztnTokenName :: TokenName
ztnTokenName = "ZTN"

zltTokenName :: TokenName
zltTokenName = "ZLT"

dktTokenName :: TokenName
dktTokenName = "DKT"

bonTokenName :: TokenName
bonTokenName = "BON"

tokenOneName :: TokenName
tokenOneName = "MAC"

tokenTwoName :: TokenName
tokenTwoName = "SRK"

tokensMinterTxOutRef :: TxOutRef
tokensMinterTxOutRef = TxOutRef "86616b7707fc9e08ff76e54b5e728933f59c5f16b826174f45bdabbc02ad0de9" 0

-- tokensCurrency :: FiniteCurency
-- tokensCurrency = Monetary.mkCurrency tokensMinterTxOutRef tokens
--     where
--         tokens :: [(TokenName, Integer)]
--         tokens = [(ztnTokenName, amt), (zltTokenName, amt), (dktTokenName, amt), (bonTokenName, amt)]

--         amt = 1_000_000

tokensCurrency :: FiniteCurency
tokensCurrency = Monetary.mkCurrency tokensMinterTxOutRef tokens
    where
        tokens :: [(TokenName, Integer)]
        tokens = [(tokenOneName, amt), (tokenTwoName, amt)]

        amt = 1_000_000

tokensCurrencySymbol :: CurrencySymbol
tokensCurrencySymbol = Monetary.currencySymbol tokensCurrency

mintTokensData :: ScriptData
mintTokensData = fromPlutusData $ Plutus.builtinDataToData (Plutus.toBuiltinData tokensCurrency)

showMeTehDatum :: LB.ByteString
showMeTehDatum =  JSON.encode $ scriptDataToJson ScriptDataJsonDetailedSchema mintTokensData

tokensMintingValidator :: FiniteCurency -> Validator
tokensMintingValidator = Validator . unMintingPolicyScript . Monetary.monetaryPolicy

tokensMintingScript :: FiniteCurency -> Script
tokensMintingScript = unMintingPolicyScript . Monetary.monetaryPolicy

tokensMintingValidatorAsCBOR :: LB.ByteString
tokensMintingValidatorAsCBOR = serialise $ tokensMintingValidator tokensCurrency

tokensPlutusMintingScript :: PlutusScript PlutusScriptV1
tokensPlutusMintingScript = PlutusScriptSerialised . SBS.toShort $ LB.toStrict tokensMintingValidatorAsCBOR

writeTokensMintingScript :: Haskell.IO ()
writeTokensMintingScript = writeScriptToFile "altswap-tokens.plutus" tokensPlutusMintingScript mintTokensData

-------------------------------------------------------------------------------
-- SWAP NFT MINTING SCRIPT
-------------------------------------------------------------------------------
aswpTokenName :: TokenName
aswpTokenName = "SWP"

swapFactoryTxOutRef :: TxOutRef
swapFactoryTxOutRef = TxOutRef "e899b80b6e68f36f655e1877733a6391b40b5c9a8f197d0a37b15b6940a3436b" 0

swapFactoryNFTCurrency :: FiniteCurency
swapFactoryNFTCurrency =  Monetary.mkCurrency swapFactoryTxOutRef [(aswpTokenName, 1)]

swapFactoryNFTCurrencySymbol :: CurrencySymbol
swapFactoryNFTCurrencySymbol = Monetary.currencySymbol swapFactoryNFTCurrency

swapFactoryNFTData :: ScriptData
swapFactoryNFTData = fromPlutusData $ Plutus.builtinDataToData (Plutus.toBuiltinData swapFactoryNFTCurrency)

swapFactoryCoin :: Monetary.Coin a
swapFactoryCoin = Monetary.mkCoin swapFactoryNFTCurrencySymbol aswpTokenName

swapFactoryValue :: Value
swapFactoryValue = Monetary.unitValue swapFactoryCoin

swapFactoryNFTMintingValidator :: FiniteCurency -> Validator
swapFactoryNFTMintingValidator = Validator . unMintingPolicyScript . Monetary.monetaryPolicy

swapFactoryNFTMintingValidatorAsCBOR :: LB.ByteString
swapFactoryNFTMintingValidatorAsCBOR = serialise $ swapFactoryNFTMintingValidator swapFactoryNFTCurrency

swapFactoryNFTPlutusMintingScript :: PlutusScript PlutusScriptV1
swapFactoryNFTPlutusMintingScript = PlutusScriptSerialised . SBS.toShort $ LB.toStrict swapFactoryNFTMintingValidatorAsCBOR

writeSwapFactoryNFTMintingScript :: Haskell.IO ()
writeSwapFactoryNFTMintingScript = writeScriptToFile "altswap-nft.plutus" swapFactoryNFTPlutusMintingScript swapFactoryNFTData

----------------------------------------------------------------------------------------
-- Liquidity Pool Minting Script
----------------------------------------------------------------------------------------

-- srkiCurrencySymbol :: CurrencySymbol
-- srkiCurrencySymbol = "ace06a09ce02f65f6a292e54871a2d827066d62933195c33eab4a468"

-- lp :: LP.LiquidityPool
-- lp = LP.LiquidityPool coinA coinB
--   where
--     coinA = Monetary.mkCoin srkiCurrencySymbol zltTokenName
--     coinB = Monetary.mkCoin srkiCurrencySymbol dktTokenName

lp :: LP.LiquidityPool
lp = LP.LiquidityPool coinA coinB
  where
    coinA = Monetary.mkCoin tokensCurrencySymbol tokenOneName
    coinB = Monetary.mkCoin tokensCurrencySymbol tokenTwoName

lpToken :: TokenName
lpToken = LP.lpTicker lp

lpStateCoin :: Monetary.Coin LP.PoolState
lpStateCoin = OffChain.poolStateCoin swapFactoryInstance

lpCoin :: Monetary.Coin LP.Liquidity
lpCoin =  Monetary.mkCoin (OffChain.liquidityCurrency swapFactoryInstance) $ LP.lpTicker lp

lpPolicy :: MintingPolicy
lpPolicy = OffChain.liquidityPolicy swapFactoryInstance

lpMintingValidator :: Validator
lpMintingValidator = Validator $ unMintingPolicyScript lpPolicy

lpMintingValidatorAsCBOR :: LB.ByteString
lpMintingValidatorAsCBOR = serialise lpMintingValidator

lpPlutusMintingScript :: PlutusScript PlutusScriptV1
lpPlutusMintingScript = PlutusScriptSerialised . SBS.toShort $ LB.toStrict lpMintingValidatorAsCBOR

mintLpTokensData :: ScriptData
mintLpTokensData = fromPlutusData $ Plutus.builtinDataToData (Plutus.toBuiltinData ())

writeLpMintingScript :: Haskell.IO ()
writeLpMintingScript = writeScriptToFile "altswap-lp-create-mint.plutus" lpPlutusMintingScript mintLpTokensData

----------------------------------------------------------------
-- SWAP Script
----------------------------------------------------------------
showSwapFactoryEmptyData :: LB.ByteString
showSwapFactoryEmptyData =  JSON.encode $ scriptDataToJson ScriptDataJsonDetailedSchema swapFactoryEmptyData

swapFactoryEmptyData :: ScriptData
swapFactoryEmptyData = fromPlutusData $ Plutus.builtinDataToData (Plutus.toBuiltinData $ Swap.Factory [])

swapFactoryExampleData :: ScriptData
swapFactoryExampleData = fromPlutusData $ Plutus.builtinDataToData (Plutus.toBuiltinData $ Swap.Factory [lp])

showSwapFactoryExampleDataJSON :: LB.ByteString
showSwapFactoryExampleDataJSON = JSON.encode $ scriptDataToJson ScriptDataJsonDetailedSchema swapFactoryExampleData

swapCreateRedeemerData :: ScriptData
swapCreateRedeemerData = fromPlutusData $ Plutus.builtinDataToData (Plutus.toBuiltinData $ Swap.Create lp)

showSwapRedeemerJSON :: LB.ByteString
showSwapRedeemerJSON =  JSON.encode $ scriptDataToJson ScriptDataJsonDetailedSchema swapCreateRedeemerData

swapFactoryInstance :: Base.AltSwap
swapFactoryInstance = OffChain.aswp swapFactoryNFTCurrencySymbol

swapInstance :: Scripts.TypedValidator OffChain.AltXChange
swapInstance = OffChain.aswpInstance swapFactoryInstance

validator :: Plutus.Validator
validator = OffChain.aswpScript swapFactoryInstance

script :: Plutus.Script
script = Plutus.unValidatorScript validator

swapTokenScriptShortBs :: SBS.ShortByteString
swapTokenScriptShortBs = SBS.toShort . LB.toStrict $ serialise script

swapTokenScript :: PlutusScript PlutusScriptV1
swapTokenScript = PlutusScriptSerialised swapTokenScriptShortBs

swapScriptData :: ScriptData
swapScriptData = fromPlutusData $ Plutus.builtinDataToData (Plutus.toBuiltinData swapFactoryInstance)

validatorAsCBOR :: LB.ByteString
validatorAsCBOR = serialise $ script

plutusMintingScript :: PlutusScript PlutusScriptV1
plutusMintingScript = PlutusScriptSerialised . SBS.toShort $ LB.toStrict validatorAsCBOR

writeSwapScript :: Haskell.IO ()
writeSwapScript = writeScriptToFile "altswap.plutus" plutusMintingScript swapScriptData

---------------------------------------------------------------------------------------------------------------
-- Create LP Output(s) Datums
----------------------------------------
afterCreatePoolDatum :: Swap.AltSwapDatum
afterCreatePoolDatum = Swap.Pool lp (Monetary.Amount 1415)

afterCreatePoolData :: ScriptData
afterCreatePoolData = fromPlutusData $ Plutus.builtinDataToData (Plutus.toBuiltinData afterCreatePoolDatum)

showAfterCreatePoolDatum :: LB.ByteString
showAfterCreatePoolDatum =  JSON.encode $ scriptDataToJson ScriptDataJsonDetailedSchema afterCreatePoolData


afterCreatePooFactorylDatum :: Swap.AltSwapDatum
afterCreatePooFactorylDatum = Swap.Factory [lp]

afterCreatePooFactorylData:: ScriptData
afterCreatePooFactorylData = fromPlutusData $ Plutus.builtinDataToData (Plutus.toBuiltinData afterCreatePooFactorylDatum)

showAfterCreatePooFactorylDatum :: LB.ByteString
showAfterCreatePooFactorylDatum =  JSON.encode $ scriptDataToJson ScriptDataJsonDetailedSchema afterCreatePooFactorylData

-- liquidityPolicy ::

-- create :: forall w s. AltSwap -> CreateParams -> Contract w s Text ()
-- create altswap CreateParams{..} = do
--     when (Monetary.swpCoin cpCoinA == Monetary.swpCoin cpCoinB) $ throwError "coins must be different"
--     when (cpAmountA <= 0 || cpAmountB <= 0) $ throwError "amounts must be positive"

--     (oref, o, lps) <- findAltSwapFactory altswap

--     let liquidity = calculateInitialLiquidity cpAmountA cpAmountB
--         lp        = LiquidityPool {lpCoinA = cpCoinA, lpCoinB = cpCoinB}

--     let swpInst  = aswpInstance altswap
--         swpScript = aswpScript altswap
--         swpDat1  = Factory $ lp : lps
--         swpDat2  = Pool lp liquidity
--         psC      = poolStateCoin altswap
--         lC       = Monetary.mkCoin (liquidityCurrency altswap) $ lpTicker lp
--         aswpVal  = Monetary.unitValue $ aswpCoin altswap
--         lpVal    = Monetary.valueOf cpCoinA cpAmountA <> Monetary.valueOf cpCoinB cpAmountB <> Monetary.unitValue psC

--         lookups  = Constraints.typedValidatorLookups swpInst        <>
--                    Constraints.otherScript swpScript                <>
--                    Constraints.mintingPolicy (liquidityPolicy altswap) <>
--                    Constraints.unspentOutputs (Map.singleton oref o)

--         tx       = Constraints.mustPayToTheScript swpDat1 aswpVal                                     <>
--                    Constraints.mustPayToTheScript swpDat2 lpVal                                     <>
--                    Constraints.mustMintValue (Monetary.unitValue psC <> Monetary.valueOf lC liquidity)              <>
--                    Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Create lp)

--     ledgerTx <- submitTxConstraintsWith lookups tx
--     void $ awaitTxConfirmed $ txId ledgerTx

--     logInfo $ "created liquidity pool: " ++ show lp

---------------------------------------------------------------------------------------------------------------

writeScriptToFile
  :: Haskell.String
  -> PlutusScript PlutusScriptV1
  -> ScriptData
  -> Haskell.IO ()
writeScriptToFile outFile plutusScript scriptData = do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData scriptData
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m swapTokenScriptShortBs [pData]
          in do Haskell.print ("Log output" :: Haskell.String) >> Haskell.print logout
                case e of
                  Left evalErr -> Haskell.print ("Eval Error" :: Haskell.String) >> Haskell.print evalErr
                  Right exbudget -> Haskell.print ("Ex Budget" :: Haskell.String) >> Haskell.print exbudget
        Nothing -> Haskell.error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope outFile Nothing plutusScript
  case result of
    Left err -> Haskell.print $ displayError err
    Right () -> return ()


main :: Haskell.IO ()
main = () Haskell.<$ seq
  where
    writers = [writeSwapFactoryNFTMintingScript, writeLpMintingScript, writeSwapScript]
    seq = Haskell.sequence writers
