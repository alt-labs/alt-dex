{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

module AltDex.Contracts.Monetary(
      LimitedSupplyCurrency(..)
    , CurrencySchema
    , CurrencyError(..)
    , AsCurrencyError(..)
    , monetaryPolicy
    -- * Actions etc
    , mintContract
    , mintedValue
    , currencySymbol
    , SimpleMPS(..)
    , mintCurrency
    , Coin (..), Amount (..),
    A, B, BON, DKT, ZTN, ZLT
    ) where

import qualified Data.OpenApi.Schema as OpenApi
import           Control.Lens

import           PlutusTx.Prelude       hiding (Monoid (..), Semigroup (..))
import           Plutus.Contract        as Contract
import           Plutus.Contract.Wallet (getUnspentOutput)
import qualified PlutusTx

import           Ledger                 (CurrencySymbol, PubKeyHash, TxId, TxOutRef (..), pubKeyHash, pubKeyHashAddress,
                                         scriptCurrencySymbol, txId)
import           Ledger.Value
               ( AssetClass(..),
                 assetClass,
                 assetClassValue,
                 assetClassValueOf,
                 TokenName,
                 Value )
import qualified Ledger.Constraints     as Constraints
import qualified Ledger.Contexts        as V
import           Ledger.Scripts

import qualified Ledger.Typed.Scripts   as Scripts
import qualified Ledger.Value           as Value

import           Data.Semigroup         (Last (..))

import           GHC.Generics           (Generic)

import qualified PlutusTx.AssocMap      as AssocMap
import           Prelude                (Semigroup (..))
import qualified Prelude                as Haskell

import           Schema                 (ToSchema)

import           Text.Printf         (PrintfArg)
import           Playground.Contract (FromJSON, Generic, ToJSON, ToSchema)

-- | SwapCoin
data SwapCoin = SwapCoin deriving (Haskell.Show, Haskell.Eq, Generic)
PlutusTx.makeIsDataIndexed ''SwapCoin [('SwapCoin, 0)]
PlutusTx.makeLift ''SwapCoin

-- | A
data A = A
PlutusTx.makeIsDataIndexed ''A [('A, 0)]
PlutusTx.makeLift ''A
-- | B
data B = B
PlutusTx.makeIsDataIndexed ''B [('B, 0)]
PlutusTx.makeLift ''B
-- | Dukat
data DKT = DKT
PlutusTx.makeIsDataIndexed ''DKT [('DKT, 0)]
PlutusTx.makeLift ''DKT
-- | Zlatnik
data ZLT = ZLT
PlutusTx.makeIsDataIndexed ''ZLT [('ZLT, 0)]
PlutusTx.makeLift ''ZLT
-- | Zeton
data ZTN = ZTN
PlutusTx.makeIsDataIndexed ''ZTN [('ZTN, 0)]
PlutusTx.makeLift ''ZTN
-- | Bon
data BON = BON
PlutusTx.makeIsDataIndexed ''BON [('BON, 0)]
PlutusTx.makeLift ''BON

-- | A single 'AssetClass'
newtype Coin a = Coin { swpCoin :: AssetClass }
  deriving stock   (Haskell.Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToSchema, Eq, Haskell.Eq, Haskell.Ord, OpenApi.ToSchema)
PlutusTx.makeIsDataIndexed ''Coin [('Coin, 0)]
PlutusTx.makeLift ''Coin

data LimitedSupplyCurrency = LimitedSupplyCurrency
  {
    cRefTxOut :: (TxId, Integer)
  , cCount    :: AssocMap.Map TokenName Integer
  }
  deriving stock (Generic, Haskell.Show, Haskell.Eq)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''LimitedSupplyCurrency

validate :: LimitedSupplyCurrency -> () -> V.ScriptContext -> Bool
validate c@(LimitedSupplyCurrency (refHash, refIdx) _) _ ctx@V.ScriptContext{V.scriptContextTxInfo=txinfo} =
  forgeOK && txOutputSpent
  where
      ownSymbol = V.ownCurrencySymbol ctx
      forged = V.txInfoMint txinfo
      expected = unwrapCurrencyValue ownSymbol c
      forgeOK =
          let v = expected == forged
          in traceIfFalse "Value forged different from expected" v
      txOutputSpent =
          let v = V.spendsOutput txinfo refHash refIdx
          in  traceIfFalse "Pending transaction does not spend the designated transaction output" v

mkCurrency :: TxOutRef -> [(TokenName, Integer)] -> LimitedSupplyCurrency
mkCurrency (TxOutRef h i) amts =
    LimitedSupplyCurrency
        { cRefTxOut = (h, i)
        , cCount    = AssocMap.fromList amts
        }

unwrapCurrencyValue :: CurrencySymbol -> LimitedSupplyCurrency -> Value
unwrapCurrencyValue s LimitedSupplyCurrency{cCount = amts} =
    let
        values = map (uncurry (Value.singleton s)) (AssocMap.toList amts)
    in fold values

-- | The 'Value' minted by the 'LimitedSupplyCurrency' contract
mintedValue :: LimitedSupplyCurrency -> Value
mintedValue cur = unwrapCurrencyValue (currencySymbol cur) cur

currencySymbol :: LimitedSupplyCurrency -> CurrencySymbol
currencySymbol = scriptCurrencySymbol . monetaryPolicy

monetaryPolicy :: LimitedSupplyCurrency -> MintingPolicy
monetaryPolicy cur = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . validate ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode cur

newtype CurrencyError =
    CurContractError ContractError
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''CurrencyError

instance AsContractError CurrencyError where
    _ContractError = _CurContractError

-- | @mint [(n1, c1), ..., (n_k, c_k)]@ creates a new currency with
--   @k@ token names, minting @c_i@ units of each token @n_i@.
--   If @k == 0@ then no value is minted. A one-shot minting policy
--   script is used to ensure that no more units of the currency can
--   be minted afterwards.
mintContract
    :: forall w s e.
    ( AsCurrencyError e
    )
    => PubKeyHash
    -> [(TokenName, Integer)]
    -> Contract w s e LimitedSupplyCurrency
mintContract pk amounts = mapError (review _CurrencyError) $ do
    txOutRef <- getUnspentOutput
    utxos <- utxosAt (pubKeyHashAddress pk)
    let newCurrency = mkCurrency txOutRef amounts
        curVali     = monetaryPolicy newCurrency
        lookups     = Constraints.mintingPolicy curVali
                        <> Constraints.unspentOutputs utxos
        mintTx      = Constraints.mustSpendPubKeyOutput txOutRef
                        <> Constraints.mustMintValue (mintedValue newCurrency)
    tx <- submitTxConstraintsWith @Scripts.Any lookups mintTx
    _ <- awaitTxConfirmed (txId tx)
    pure newCurrency

-- | Minting policy for a currency that has a fixed amount of tokens issued
--   in one transaction
data SimpleMPS =
    SimpleMPS
        { tokenName :: TokenName
        , amount    :: Integer
        }
        deriving stock (Haskell.Eq, Haskell.Show, Generic)
        deriving anyclass (FromJSON, ToJSON, ToSchema)

type CurrencySchema =
        Endpoint "Create native token" SimpleMPS

-- | Use 'mintContract' to create the currency specified by a 'SimpleMPS'
mintCurrency
    :: Promise (Maybe (Last LimitedSupplyCurrency)) CurrencySchema CurrencyError LimitedSupplyCurrency
mintCurrency = endpoint @"Create native token" $ \SimpleMPS{tokenName, amount} -> do
    ownPK <- pubKeyHash <$> ownPubKey
    cur <- mintContract ownPK [(tokenName, amount)]
    tell (Just (Last cur))
    pure cur

-- | Likewise for 'Integer'; the corresponding amount we have of the
-- particular 'Coin'.
newtype Amount a = Amount { swpAmount :: Integer }
  deriving stock   (Haskell.Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToSchema, Eq, Ord, PrintfArg)
  deriving newtype (Haskell.Eq, Haskell.Ord, Haskell.Num)
  deriving newtype (AdditiveGroup, AdditiveMonoid, AdditiveSemigroup, MultiplicativeSemigroup)
PlutusTx.makeIsDataIndexed ''Amount [('Amount, 0)]
PlutusTx.makeLift ''Amount

{-# INLINABLE valueOf #-}
valueOf :: Coin a -> Amount a -> Value
valueOf c a = assetClassValue (swpCoin c) (swpAmount a)

{-# INLINABLE unitValue #-}
unitValue :: Coin a -> Value
unitValue c = valueOf c 1

{-# INLINABLE isUnity #-}
isUnity :: Value -> Coin a -> Bool
isUnity v c = amountOf v c == 1

{-# INLINABLE amountOf #-}
amountOf :: Value -> Coin a -> Amount a
amountOf v = Amount . assetClassValueOf v . swpCoin

{-# INLINABLE mkCoin #-}
mkCoin:: CurrencySymbol -> TokenName -> Coin a
mkCoin c = Coin . assetClass c