{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-| Example trace for the uniswap contract
-}
module AltDex.Contracts.MintTrace(
    mintTokensTrace,
    main
    ) where

import Control.Monad (forM_, when)
import Control.Monad.Freer.Error (throwError)
import Data.Map qualified as Map
import Data.Monoid qualified as Monoid
import Data.Semigroup qualified as Semigroup
import Data.Void (Void)
import Ledger
import Ledger.Ada (adaSymbol, adaToken)
import Ledger.Constraints
import Ledger.Value as Value
import Plutus.Contract as Contract hiding (throwError)
import Plutus.Contracts.Currency qualified as Currency
import Plutus.Contracts.Uniswap.OffChain as OffChain
import Plutus.Contracts.Uniswap.Types as Types
import Plutus.Trace.Emulator (EmulatorRuntimeError (GenericError), EmulatorTrace)
import Plutus.Trace.Emulator qualified as Emulator
import Wallet.Emulator (Wallet (..), knownWallet, knownWallets, walletPubKeyHash)
import Prelude

import Data.Text
import Data.String


import qualified AltDex.Contracts.Serialise as S
import qualified AltDex.Contracts.Monetary as Cash

-- | Set up a liquidity pool and call the "add" endpoint
mintTokensTrace :: EmulatorTrace ()
mintTokensTrace = do
    cidInit <- Emulator.activateContract (knownWallet 1) mintC "init"
    _ <- Emulator.waitNSlots 2
    cs <- Emulator.observableState cidInit >>= \case
                Just (Semigroup.Last cur) -> pure (Cash.currencySymbol cur)
                _                         -> throwError $ GenericError "failed to create currency"
    -- let coins = Map.fromList [(tn, Types.mkCoin cs tn) | tn <- tokenNames]
    --     ada   = Types.mkCoin adaSymbol adaToken

    -- cidStart <- Emulator.activateContract (knownWallet 1) ownerEndpoint "start"
    -- _ <- Emulator.waitNSlots 5
    -- us <- Emulator.observableState cidStart >>= \case
    --             Monoid.Last (Just (Right v)) -> pure v
    --             _                            -> throwError $ GenericError "initialisation failed"
    -- cid1 <- Emulator.activateContractWallet (knownWallet 2) (awaitPromise $ userEndpoints us)
    -- cid2 <- Emulator.activateContractWallet (knownWallet 3) (awaitPromise $ userEndpoints us)
    -- _ <- Emulator.waitNSlots 5

    -- let cp = OffChain.CreateParams ada (coins Map.! "A") 20_000_000 500000

    -- Emulator.callEndpoint @"create" cid1 cp
    -- _ <- Emulator.waitNSlots 5

    -- let ap = AddParams{apCoinA = ada, apCoinB = coins Map.! "A", apAmountA = 1000, apAmountB = 5000}
    -- Emulator.callEndpoint @"add" cid2 ap
    -- _ <- Emulator.waitNSlots 5
    pure ()
  where
    pkh = walletPubKeyHash $ knownWallet 1

    -- mintC :: forall w s e. ((Cash.AsCurrencyError e, Data.String.IsString Cash.CurrencyError)) => Contract w s Cash.CurrencyError Cash.LimitedSupplyCurrency
    mintC :: Contract (Maybe (Semigroup.Last Cash.LimitedSupplyCurrency)) Cash.CurrencySchema Cash.CurrencyError Cash.LimitedSupplyCurrency
    mintC = Cash.mintContract pkh tokenAmounts

    -- activateMintingContract = Emulator.activateContract
-- | Create some sample tokens and distribute them to
--   the emulated wallets
-- setupTokens :: Contract (Maybe (Semigroup.Last Currency.OneShotCurrency)) Currency.CurrencySchema Currency.CurrencyError ()
-- setupTokens = do
--     ownPK <- Contract.ownPaymentPubKeyHash
--     cur   <- Currency.mintContract ownPK [(tn, fromIntegral (length wallets) * amount) | tn <- tokenNames]
--     let cs = Currency.currencySymbol cur
--         v  = mconcat [Value.singleton cs tn amount | tn <- tokenNames]

--     forM_ wallets $ \w -> do
--         let pkh = mockWalletPaymentPubKeyHash w
--         when (pkh /= ownPK) $ do
--             mkTxConstraints @Void mempty (mustPayToPubKey pkh v)
--               >>= submitTxConfirmed . adjustUnbalancedTx

--     tell $ Just $ Semigroup.Last cur

--   where
--     amount = 1000000

wallets :: [Wallet]
wallets = Prelude.take 1 knownWallets

tokenAmounts :: [(TokenName, Integer)]
tokenAmounts = [
  (S.zltTokenName, 1000000),
  (S.dktTokenName, 1000000)]

main :: IO ()
main =
  Emulator.runEmulatorTraceIO mintTokensTrace