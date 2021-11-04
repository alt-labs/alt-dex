{-# LANGUAGE OverloadedStrings #-}
-- | A decentralized exchange for arbitrary token pairs following the
-- [Uniswap protocol](https://uniswap.org/whitepaper.pdf).
--
-- Details:
--
--  - 'OffChain' contains the instance endpoints and client functionality
--  - 'OnChain' contains the validation logic
--  - 'Types' conains a few common datatypes for working with this contract
--  - 'Pool' contains functions needed by both on-chain and off-chain code
--    related to working with liquidity pools.
module AltDex.Contracts
  ( module OnChain
  , module OffChain
  , module Swap
  , module LiqudityPool
  ) where

import           AltDex.Contracts.LiquidityPool  as Base
import           AltDex.Contracts.LiquidityPool  as LiquidityPool
import           AltDex.Contracts.OffChain       as OffChain
import           AltDex.Contracts.Swap           as Swap
import           AltDex.Contracts.OnChain        as OnChain
