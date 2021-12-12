{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module AltDex.Contracts.CreatePool
    ( create
    ) where

import           Control.Lens                     (view)
import           Control.Monad                    hiding (fmap)
import qualified Data.Map                         as Map
import           Data.Monoid                      (Last (..))
import           Data.Proxy                       (Proxy (..))
import           Data.Text                        (Text, pack)
import           Data.Void                        (Void, absurd)
import           Ledger                           hiding (singleton)
import           Ledger.Constraints               as Constraints
import qualified Ledger.Typed.Scripts             as Scripts
import           Plutus.Contract
import           AltDex.Contracts.Base
import qualified AltDex.Contracts.Monetary        as Monetary
import           AltDex.Contracts.Monetary        (A, B)
import           AltDex.Contracts.OnChain         (mkAltSwapValidator, validateLiquidityMinting)
import           AltDex.Contracts.LiquidityPool
import           AltDex.Contracts.Swap
import           AltDex.Contracts.Common
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..), dropWhile, flip, unless)
import           Prelude                          as Haskell (Int, Semigroup (..), String, div, dropWhile, flip, show,
                                                              (^))
import           Text.Printf                      (printf)
import           AltDex.Contracts.OffChain        (CreateParams)


create :: forall w s. AltSwap -> CreateParams -> Contract w s Text ()
create altswap lp = do
  runError (run altswap lp) >>= \case
      Left err -> logWarn @Hask.String (Hask.show @ContractError err)
      Right () -> pure ()
  where
    run altswap CreateParams{..} = do
      when (Monetary.swpCoin cpCoinA == Monetary.swpCoin cpCoinB) $ throwError "coins must be different"
      when (cpAmountA <= 0 || cpAmountB <= 0) $ throwError "amounts must be positive"

      (oref, o, lps) <- findAltSwapFactory altswap

      let liquidity = calculateInitialLiquidity cpAmountA cpAmountB
          lp        = LiquidityPool {lpCoinA = cpCoinA, lpCoinB = cpCoinB}

      let swpInst  = aswpInstance altswap
          swpScript = aswpScript altswap
          swpDat1  = Factory $ lp : lps
          swpDat2  = Pool lp liquidity
          psC      = poolStateCoin altswap
          lC       = Monetary.mkCoin (liquidityCurrency altswap) $ lpTicker lp
          aswpVal  = Monetary.unitValue $ aswpCoin altswap
          lpVal    = Monetary.valueOf cpCoinA cpAmountA <> Monetary.valueOf cpCoinB cpAmountB <> Monetary.unitValue psC

          lookups  = Constraints.typedValidatorLookups swpInst        <>
                    Constraints.otherScript swpScript                <>
                    Constraints.mintingPolicy (liquidityPolicy altswap) <>
                    Constraints.unspentOutputs (Map.singleton oref o)

          tx       = Constraints.mustPayToTheScript swpDat1 aswpVal                                     <>
                    Constraints.mustPayToTheScript swpDat2 lpVal                                     <>
                    Constraints.mustMintValue (Monetary.unitValue psC <> Monetary.valueOf lC liquidity)              <>
                    Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Create lp)

      -- ledgerTx <- submitTxConstraintsWith lookups tx

      logInfo @Hask.String $ "Yielding Tx for [Liquidity Pool -> Create]:"
      -- void $ awaitTxConfirmed $ txId ledgerTx
      yieldUnbalancedTx pUtx
      -- logInfo $ "created liquidity pool: " ++ show lp
