{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# options_ghc -Wno-redundant-constraints #-}
{-# options_ghc -fno-specialise            #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module AltDex.Contracts.Swap
  where

import           Control.Monad             (forM_, when, void)
import qualified Data.Semigroup            as Semigroup
import           Ledger.Constraints
import           Ledger.Value              as Value
import           Plutus.Contract as Contract hiding (throwError)
import           AltDex.Contracts.Monetary
import      qualified     AltDex.Contracts.Monetary as Monetary
import           AltDex.Contracts.LiquidityPool
import           Wallet.Emulator.Types     (Wallet (..))

import qualified Data.OpenApi.Schema as OpenApi
import           Ledger
import           Ledger.Value        (AssetClass (..), assetClass, assetClassValue, assetClassValueOf)
import           Playground.Contract (FromJSON, Generic, ToJSON, ToSchema)
import qualified PlutusTx
-- import           PlutusTx.Prelude
import  Prelude
import  qualified Prelude as Haskell

import           Text.Printf         (PrintfArg)
import qualified Wallet.Emulator as Wallet.Emulator.Wallet
import Wallet.Emulator.Wallet (knownWallets)
import           Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)
import AltDex.Contracts.Monetary (SwapCoin)
import AltDex.Contracts.Base

data AltSwapDatum =
      Factory [LiquidityPool]
    | Pool LiquidityPool (Amount Liquidity)
    deriving stock (Haskell.Show)
PlutusTx.makeIsDataIndexed ''AltSwapDatum [ ('Factory, 0)
                                     , ('Pool,    1)
                                     ]
PlutusTx.makeLift ''AltSwapDatum

data AltSwapAction = Create LiquidityPool | Close | Swap | Remove | Add
    deriving Haskell.Show
PlutusTx.makeIsDataIndexed ''AltSwapAction [ ('Create , 0)
                                           , ('Close,   1)
                                           , ('Swap,    2)
                                           , ('Remove,  3)
                                           , ('Add,     4)
                                           ]
PlutusTx.makeLift ''AltSwapAction

initContract :: Contract (Maybe (Semigroup.Last Monetary.LimitedSupplyCurrency)) Monetary.CurrencySchema Monetary.CurrencyError ()
initContract = do
    ownPK <- Contract.ownPubKeyHash
    cur   <- Monetary.mintContract ownPK [(tn,fromIntegral (length knownWallets) * amount) | tn <- tokenNames]
    let cs = Monetary.currencySymbol cur
        v  = mconcat [Value.singleton cs tn amount | tn <- tokenNames]

    forM_ knownWallets $ \w -> do
        let pkh = Wallet.Emulator.Wallet.walletPubKeyHash w
        when (pkh /= ownPK) $ do
            tx <- submitTx $ mustPayToPubKey pkh v
            awaitTxConfirmed $ getCardanoTxId tx

    tell $ Just $ Semigroup.Last cur

    void $ waitNSlots 1
  where
    amount = 1000000

-- wallets :: [Wallet]
-- wallets = knownWallets

tokenNames :: [TokenName]
tokenNames = ["A", "B", "C", "D"]

data AltSwapContracts =
      Init
    | AltSwapStart
    | AltSwapUser AltSwap
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty AltSwapContracts where
    pretty = viaShow