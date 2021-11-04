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

module AltDex.Contracts.Base
  where

import qualified Data.OpenApi.Schema as OpenApi
import           Ledger
import           Ledger.Value        (AssetClass (..), assetClass, assetClassValue, assetClassValueOf)
import           Playground.Contract (FromJSON, Generic, ToJSON, ToSchema)
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude             as Haskell
import           Text.Printf         (PrintfArg)
import           AltDex.Contracts.Monetary

newtype AltSwap = AltSwap
    { aswpCoin :: Coin SwapCoin
    } deriving stock    (Haskell.Show, Generic)
      deriving anyclass (ToJSON, FromJSON, ToSchema, OpenApi.ToSchema)
      deriving newtype  (Haskell.Eq, Haskell.Ord)
PlutusTx.makeIsDataIndexed ''AltSwap [('AltSwap, 0)]
PlutusTx.makeLift ''AltSwap

aswpTokenName, poolStateTokenName :: TokenName
aswpTokenName = "AltSwap"
poolStateTokenName = "LP_S_TOK"

