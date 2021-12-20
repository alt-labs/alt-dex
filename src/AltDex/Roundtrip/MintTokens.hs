module AltDex.Roundtrip.MintTokens
    ( demoMintingContract
    ) where
import           Control.Monad                    hiding (fmap)
import qualified Data.Map                         as Map
import           Data.Monoid                      (Last (..))
import           Data.Proxy                       (Proxy (..))
import           Data.Text                        (Text, pack)
import           Data.Void                        (Void, absurd)
import           Ledger                           hiding (singleton)
import           Ledger.Constraints               as Constraints
import qualified Ledger.Typed.Scripts             as Scripts

import           Plutus.Contract qualified        as Contract
import           Plutus.Contract

-- import Plutus.Contract (
--   EmptySchema,
--   ContractError,
--   Endpoint,
--   Contract,
--   Promise,
--   endpoint,
--   mkTxConstraints,
-- --   yieldUnbalancedTx,
-- --   ownPubKeyHash,
--   logInfo
--   )

import           AltDex.Contracts.Base
import qualified AltDex.Contracts.Monetary        as Monetary
import           AltDex.Contracts.LiquidityPool
import           AltDex.Contracts.Swap
import           AltDex.Contracts.Common
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..), dropWhile, flip, unless)
import           Prelude                          as Haskell (Int, Semigroup (..), String, div, dropWhile, flip, show,
                                                              (^))
import           Text.Printf                      (printf)
import           AltDex.Contracts.Monetary
import           Plutus.Contract.Wallet           (getUnspentOutput)
import           Control.Lens

import qualified AltDex.Roundtrip.Nami as Nami
import qualified AltDex.Roundtrip.Hardcoded as HC


import Data.String

data DemoMintingContract

instance Scripts.ValidatorTypes DemoMintingContract where
    type instance RedeemerType DemoMintingContract = Void
    type instance DatumType DemoMintingContract = Void

getAddress :: forall w s e. (AsCurrencyError e, IsString e) => Contract w s e Address
getAddress =
  case HC.mintAddress of
    Just a -> pure a
    _ -> Contract.throwError "Can not parse source UTXO address for minting TxOutRef owner"


demoMintingContract :: forall w s e. (AsCurrencyError e, IsString CurrencyError)
    => PubKeyHash
    -> [(TokenName, Integer)]
    -> Contract w s e LimitedSupplyCurrency
demoMintingContract pk amounts = mapError (review _CurrencyError) $ do
    txOutRef <- HC.getUnspentOutput'
    addr <- getAddress
    utxos <- utxosAt addr

    let lookups     = Constraints.mintingPolicy curVali
                        <> Constraints.unspentOutputs utxos

        mintTx      = Constraints.mustSpendPubKeyOutput txOutRef
                        <> Constraints.mustMintValue (mintedValue newCurrency)

    -- tx <- submitTxConstraintsWith @Scripts.Any lookups mintTx
    utx <- mkTxConstraints @DemoMintingContract lookups mintTx

    -- _ <- awaitTxConfirmed (txId tx)

    pure newCurrency

    where
      newCurrency = mkCurrency txOutRef amounts
      curVali     = monetaryPolicy newCurrency
