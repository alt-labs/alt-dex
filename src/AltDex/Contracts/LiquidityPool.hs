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
{-# LANGUAGE ScopedTypeVariables              #-}
{-# options_ghc -Wno-redundant-constraints #-}
{-# options_ghc -fno-specialise            #-}

module AltDex.Contracts.LiquidityPool
   (
    LiquidityPool (..), Liquidity
  ) where

import qualified Data.OpenApi.Schema as OpenApi
import Ledger ( TokenName )
import           Ledger.Value        (TokenName (..), AssetClass (..), assetClass, assetClassValue, assetClassValueOf,  unAssetClass, unCurrencySymbol)
import           Playground.Contract (FromJSON, Generic, ToJSON, ToSchema)
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude             as Haskell
import           Text.Printf         (PrintfArg)
import           AltDex.Contracts.Monetary
import           PlutusTx.Sqrt

-- | Pool-state coin token
data PoolState = PoolState
PlutusTx.makeIsDataIndexed ''PoolState [('PoolState, 0)]
PlutusTx.makeLift ''PoolState

-- | Liquidity-state coin token
data Liquidity = Liquidity
PlutusTx.makeIsDataIndexed ''Liquidity [('Liquidity, 0)]
PlutusTx.makeLift ''Liquidity

data LiquidityPool = LiquidityPool
    { lpCoinA :: Coin A
    , lpCoinB :: Coin B
    }
    deriving (Haskell.Show, Generic, ToJSON, FromJSON, ToSchema)
PlutusTx.makeIsDataIndexed ''LiquidityPool [('LiquidityPool, 0)]
PlutusTx.makeLift ''LiquidityPool

instance Eq LiquidityPool where
    {-# INLINABLE (==) #-}
    x == y = (lpCoinA x == lpCoinA y && lpCoinB x == lpCoinB y) ||
              -- Make sure the underlying coins aren't equal.
             (swpCoin (lpCoinA x) == swpCoin (lpCoinB y) && swpCoin (lpCoinB x) == swpCoin (lpCoinA y))

{-# INLINABLE calculateInitialLiquidity #-}
-- | The initial liquidity is 'ceil( sqrt(x*y) )' where 'x' is the amount of
-- 'Coin A' and y the amount of 'Coin B'.  See Eq. 13 of the Uniswap v2 paper.
calculateInitialLiquidity :: Amount A -> Amount B -> Amount Liquidity
calculateInitialLiquidity outA outB = Amount $ case isqrt (swpAmount outA * swpAmount outB) of
    Exactly l
        | l > 0 -> l
    Approximately l
        | l > 0 -> l + 1
    _           -> traceError "insufficient liquidity"

{-# INLINABLE calculateAdditionalLiquidity #-}
calculateAdditionalLiquidity :: Amount A -> Amount B -> Amount Liquidity -> Amount A -> Amount B -> Amount Liquidity
calculateAdditionalLiquidity oldA' oldB' liquidity delA' delB' =
  case rsqrt ratio of
    Imaginary       -> traceError "insufficient liquidity"
    Exactly x       -> Amount x - liquidity
    Approximately x -> Amount x - liquidity
  where
    ratio = (swpAmount (liquidity * liquidity * newProd)) % swpAmount oldProd

    -- Unwrap, as we're combining terms
    oldA = swpAmount oldA'
    oldB = swpAmount oldB'
    delA = swpAmount delA'
    delB = swpAmount delB'

    oldProd, newProd :: Amount Liquidity
    oldProd = Amount $ oldA * oldB
    newProd = Amount $ (oldA + delA) * (oldB + delB)


{-# INLINABLE calculateRemoval #-}
-- | See Definition 3 of <https://github.com/runtimeverification/verified-smart-contracts/blob/c40c98d6ae35148b76742aaaa29e6eaa405b2f93/uniswap/x-y-k.pdf>.
calculateRemoval :: Amount A -> Amount B -> Amount Liquidity -> Amount Liquidity -> (Amount A, Amount B)
calculateRemoval inA inB liquidity' diff' = (f inA, f inB)
  where
    f :: Amount a -> Amount a
    f = Amount . g . swpAmount

    diff      = swpAmount diff'
    liquidity = swpAmount liquidity'

    g :: Integer -> Integer
    g x = x - divide (x * diff) liquidity

{-# INLINABLE checkSwap #-}
-- | A swap is valid if the fee is computed correctly, and we're swapping some
-- positive amount of A for B.  See: <https://uniswap.org/whitepaper.pdf> Eq (11) (Page 7.)
checkSwap :: Amount A -> Amount B -> Amount A -> Amount B -> Bool
checkSwap oldA' oldB' newA' newB' =
    traceIfFalse "expected positive oldA" (oldA > 0) &&
    traceIfFalse "expected positive oldB" (oldB > 0) &&
    traceIfFalse "expected positive-newA" (newA > 0) &&
    traceIfFalse "expected positive-newB" (newB > 0) &&
    traceIfFalse "expected product to increase"
        ((((newA * feeDen) - (inA * feeNum)) * ((newB * feeDen) - (inB * feeNum)))
         >= (feeDen * feeDen * oldA * oldB))
  where
    -- Unwrap; because we are mixing terms.
    oldA = swpAmount oldA'
    oldB = swpAmount oldB'
    newA = swpAmount newA'
    newB = swpAmount newB'

    inA = max 0 $ newA - oldA
    inB = max 0 $ newB - oldB
    -- The uniswap fee is 0.3%; here it is multiplied by 1000, so that the
    -- on-chain code deals only in integers.
    -- See: <https://uniswap.org/whitepaper.pdf> Eq (11) (Page 7.)
    feeNum, feeDen :: Integer
    feeNum = 3
    feeDen = 1000

{-# INLINABLE lpTicker #-}
-- | Generate a unique token name for this particular pool; based on the
-- tokens it exchanges. This should be such that looking for a pool exchanging
-- any two tokens always yields a unique name.
lpTicker :: LiquidityPool -> TokenName
lpTicker LiquidityPool{..}  = TokenName hash
    where
      cA@(csA, tokA) = unAssetClass (swpCoin lpCoinA)
      cB@(csB, tokB) = unAssetClass (swpCoin lpCoinB)
      ((x1, y1), (x2, y2))
        | cA < cB   = ((csA, tokA), (csB, tokB))
        | otherwise = ((csB, tokB), (csA, tokA))

      h1   = sha2_256 $ unTokenName y1
      h2   = sha2_256 $ unTokenName y2
      h3   = sha2_256 $ unCurrencySymbol x1
      h4   = sha2_256 $ unCurrencySymbol x2
      hash = sha2_256 $ h1 <> h2 <> h3 <> h4
