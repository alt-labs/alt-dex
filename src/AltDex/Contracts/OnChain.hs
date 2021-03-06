{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# options_ghc -fno-specialise         #-}

module AltDex.Contracts.OnChain
    ( mkAltSwapValidator
    , validateLiquidityMinting
    ) where

import           Ledger
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import           Ledger.Value                     (AssetClass (..), symbols)
import           AltDex.Contracts.Base
import           AltDex.Contracts.Monetary
import           AltDex.Contracts.LiquidityPool
import           AltDex.Contracts.Swap

import qualified PlutusTx
import           PlutusTx.Prelude

{-# INLINABLE findOwnInput' #-}
findOwnInput' :: ScriptContext -> TxInInfo
findOwnInput' ctx = fromMaybe (error ()) (findOwnInput ctx)

{-# INLINABLE valueWithin #-}
valueWithin :: TxInInfo -> Value
valueWithin = txOutValue . txInInfoResolved

{-# INLINABLE validateSwap #-}
-- | We check the swap is valid through 'validateSwap', and otherwise just make
-- sure that the pool token is passed through.
validateSwap :: LiquidityPool -> Coin PoolState -> ScriptContext -> Bool
validateSwap LiquidityPool{..} c ctx =
    checkSwap oldA oldB newA newB                                                       &&
    traceIfFalse "expected pool state token to be present in input" (isUnity inVal c)   &&
    traceIfFalse "expected pool state token to be present in output" (isUnity outVal c) &&
    traceIfFalse "did not expect AltSwap minting" noAltSwapMinting
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxInInfo
    ownInput = findOwnInput' ctx

    ownOutput :: TxOut
    ownOutput = case [ o
                     | o <- getContinuingOutputs ctx
                     , txOutDatumHash o == Just (snd $ ownHashes ctx)
                     ] of
        [o] -> o
        _   -> traceError "expected exactly one output to the same liquidity pool"

    oldA = amountA inVal
    oldB = amountB inVal
    newA = amountA outVal
    newB = amountB outVal

    amountA v = amountOf v lpCoinA
    amountB v = amountOf v lpCoinB

    inVal, outVal :: Value
    inVal  = valueWithin ownInput
    outVal = txOutValue ownOutput

    noAltSwapMinting :: Bool
    noAltSwapMinting =
      let
        AssetClass (cs, _) = swpCoin c
        minted             = txInfoMint info
      in
        all (/= cs) $ symbols minted

{-# INLINABLE validateCreate #-}
-- | Ths validates the creation of a liquidity pool to exchange coins. In order to be
-- valid,
--
--  1,2. We need to be dealing with the AltSwap coin,
--  3. We have to exchanging different coins,
--  4. The pool can't already exist,
--  5. The pool needs a single value as output,
--  6. The liquidity amount needs to be as-determined by 'calculateInitialLiquidity'
--      (i.e. the amount from the Uniswap V2 paper).
--  7,8. We need to be exchanging more than zero of each kind of coin.
--  9. It should output a pool with the determined properties
validateCreate :: AltSwap
               -> Coin PoolState
               -> [LiquidityPool]
               -> LiquidityPool
               -> ScriptContext
               -> Bool
validateCreate AltSwap{..} c lps lp@LiquidityPool{..} ctx = True
  --   traceIfFalse "AltSwap coin not present" (isUnity (valueWithin $ findOwnInput' ctx) aswpCoin)          && -- 1.
  --   traceIfFalse "Error: 2" (Constraints.checkOwnOutputConstraint ctx (OutputConstraint (Factory $ lp : lps) $ unitValue aswpCoin)) && -- 2.
  --   traceIfFalse "Error: 3" (swpCoin lpCoinA /= swpCoin lpCoinB)                                                                  && -- 3.
  --   traceIfFalse "Error: 4" (all (/= lp) lps)                                                                                       && -- 4.
  --   traceIfFalse "Error: 5" (isUnity minted c)                                                                                     && -- 5.
  --   traceIfFalse "Error: 6" (amountOf minted liquidityCoin' == liquidity)                                                         && -- 6.
  --   traceIfFalse "Error: 7" (outA > 0)                                                                                            && -- 7.
  --   traceIfFalse "Error: 8" (outB > 0)                                                                                            && -- 8.
  --   traceIfFalse "Error: 9" (Constraints.checkOwnOutputConstraint ctx (OutputConstraint (Pool lp liquidity) $                         -- 9.
  --       valueOf lpCoinA outA <> valueOf lpCoinB outB <> unitValue c))
  -- where
  --   poolOutput :: TxOut
  --   poolOutput = case [o | o <- getContinuingOutputs ctx, isUnity (txOutValue o) c] of
  --       [o] -> o
  --       _   -> traceError "expected exactly one pool output"

  --   outA      = amountOf (txOutValue poolOutput) lpCoinA
  --   outB      = amountOf (txOutValue poolOutput) lpCoinB
  --   liquidity = calculateInitialLiquidity outA outB

  --   minted :: Value
  --   minted = txInfoMint $ scriptContextTxInfo ctx

  --   liquidityCoin' :: Coin Liquidity
  --   liquidityCoin' = let AssetClass (cs,_) = swpCoin c in mkCoin cs $ lpTicker lp

{-# INLINABLE validateCloseFactory #-}
validateCloseFactory :: AltSwap -> Coin PoolState -> [LiquidityPool] -> ScriptContext -> Bool
validateCloseFactory AltSwap{..} c lps ctx =
    traceIfFalse "AltSwap coin not present" (isUnity (valueWithin $ findOwnInput' ctx) aswpCoin)                          && -- 1.
    traceIfFalse "wrong mint value"        (txInfoMint info == negate (unitValue c <>  valueOf lC (snd lpLiquidity))) && -- 2.
    traceIfFalse "factory output wrong"                                                                                    -- 3.
        (Constraints.checkOwnOutputConstraint ctx $ OutputConstraint (Factory $ filter (/= fst lpLiquidity) lps) $ unitValue aswpCoin)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    poolInput :: TxInInfo
    poolInput = case [ i
                     | i <- txInfoInputs info
                     , isUnity (valueWithin i) c
                     ] of
        [i] -> i
        _   -> traceError "expected exactly one pool input"

    lpLiquidity :: (LiquidityPool, Amount Liquidity)
    lpLiquidity = case txOutDatumHash . txInInfoResolved $ poolInput of
        Nothing -> traceError "pool input witness missing"
        Just h  -> findPoolDatum info h

    lC :: Coin Liquidity
    lC  = let AssetClass (cs, _) = swpCoin c in mkCoin cs (lpTicker $ fst lpLiquidity)

{-# INLINABLE validateClosePool #-}
validateClosePool :: AltSwap -> ScriptContext -> Bool
validateClosePool us ctx = hasFactoryInput
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasFactoryInput :: Bool
    hasFactoryInput =
        traceIfFalse "AltSwap factory input expected" $
        isUnity (valueSpent info) (aswpCoin us)

{-# INLINABLE validateRemove #-}
validateRemove :: Coin PoolState -> LiquidityPool -> Amount Liquidity -> ScriptContext -> Bool
validateRemove c lp liquidity ctx =
    traceIfFalse "zero removal"                        (diff > 0)                                     &&
    traceIfFalse "removal of too much liquidity"       (diff < liquidity)                             &&
    traceIfFalse "pool state coin missing"             (isUnity inVal c)                              &&
    traceIfFalse "wrong liquidity pool output"         (fst lpLiquidity == lp)                        &&
    traceIfFalse "pool state coin missing from output" (isUnity outVal c)                             &&
    traceIfFalse "liquidity tokens not burnt"          (txInfoMint info == negate (valueOf lC diff)) &&
    traceIfFalse "non-positive liquidity"              (outA > 0 && outB > 0)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxInInfo
    ownInput = findOwnInput' ctx

    output :: TxOut
    output = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one AltSwap output"

    inVal, outVal :: Value
    inVal  = valueWithin ownInput
    outVal = txOutValue output

    lpLiquidity :: (LiquidityPool, Amount Liquidity)
    lpLiquidity = case txOutDatumHash output of
        Nothing -> traceError "pool output witness missing"
        Just h  -> findPoolDatum info h

    lC :: Coin Liquidity
    lC = let AssetClass (cs, _) = swpCoin c in mkCoin cs (lpTicker lp)

    diff         = liquidity - snd lpLiquidity
    inA          = amountOf inVal $ lpCoinA lp
    inB          = amountOf inVal $ lpCoinB lp
    (outA, outB) = calculateRemoval inA inB liquidity diff

{-# INLINABLE validateAdd #-}
-- | See 'AltDex.Contracts.OffChain.add'.
validateAdd :: Coin PoolState -> LiquidityPool -> Amount Liquidity -> ScriptContext -> Bool
validateAdd c lp liquidity ctx =
    traceIfFalse "pool stake token missing from input"          (isUnity inVal c)                                                    &&
    traceIfFalse "output pool for same liquidity pair expected" (lp == fst outDatum)                                                 &&
    traceIfFalse "must not remove tokens"                       (delA >= 0 && delB >= 0)                                             &&
    traceIfFalse "insufficient liquidity"                       (delL >= 0)                                                          &&
    traceIfFalse "wrong amount of liquidity tokens"             (delL == calculateAdditionalLiquidity oldA oldB liquidity delA delB) &&
    traceIfFalse "wrong amount of liquidity tokens minted"      (txInfoMint info == valueOf lC delL)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxInInfo
    ownInput = findOwnInput' ctx

    ownOutput :: TxOut
    ownOutput = case [ o
                     | o <- getContinuingOutputs ctx
                     , isUnity (txOutValue o) c
                     ] of
        [o] -> o
        _   -> traceError "expected exactly on pool output"

    outDatum :: (LiquidityPool, Amount Liquidity)
    outDatum = case txOutDatum ownOutput of
        Nothing -> traceError "pool output datum hash not found"
        Just h  -> findPoolDatum info h

    inVal, outVal :: Value
    inVal  = valueWithin ownInput
    outVal = txOutValue ownOutput

    oldA = amountOf inVal aC
    oldB = amountOf inVal bC
    delA = amountOf outVal aC - oldA
    delB = amountOf outVal bC - oldB
    delL = snd outDatum - liquidity

    aC = lpCoinA lp
    bC = lpCoinB lp

    lC :: Coin Liquidity
    lC = let AssetClass (cs, _) = swpCoin c in mkCoin cs $ lpTicker lp

{-# INLINABLE findPoolDatum #-}
findPoolDatum :: TxInfo -> DatumHash -> (LiquidityPool, Amount Liquidity)
findPoolDatum info h = case findDatum h info of
    Just (Datum d) -> case PlutusTx.unsafeFromBuiltinData d of
        (Pool lp a) -> (lp, a)
        _           -> traceError "error decoding data"
    _              -> traceError "pool input datum not found"

{-# INLINABLE mkAltSwapValidator #-}
mkAltSwapValidator :: AltSwap
                   -> Coin PoolState
                   -> AltSwapDatum
                   -> AltSwapAction
                   -> ScriptContext
                   -> Bool
mkAltSwapValidator altswap coin (Factory lps) (Create lp) ctx = validateCreate altswap coin lps lp ctx
mkAltSwapValidator _       coin (Pool lp _)   Swap        ctx = validateSwap lp coin ctx
mkAltSwapValidator altswap coin (Factory lps) Close       ctx = validateCloseFactory altswap coin lps ctx
mkAltSwapValidator altswap _    (Pool _  _)   Close       ctx = validateClosePool altswap ctx
mkAltSwapValidator _       coin (Pool lp a)   Remove      ctx = validateRemove coin lp a ctx
mkAltSwapValidator _       coin (Pool lp a)   Add         ctx = validateAdd coin lp a ctx
mkAltSwapValidator _       _  _               _           _   = traceIfFalse "Meow" False

{-# INLINABLE validateLiquidityMinting #-}
validateLiquidityMinting :: AltSwap -> TokenName -> () -> ScriptContext -> Bool
validateLiquidityMinting AltSwap{..} tn _ ctx
  = case [ i
         | i <- txInfoInputs $ scriptContextTxInfo ctx
         , let v = valueWithin i
         , isUnity v aswpCoin || isUnity v lpC
         ] of
    [_]    -> True
    [_, _] -> True
    _      -> traceError "pool state minting without AltSwap input"
  where
    lpC :: Coin Liquidity
    lpC = mkCoin (ownCurrencySymbol ctx) tn
