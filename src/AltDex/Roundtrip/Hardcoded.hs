module AltDex.Roundtrip.Hardcoded(
  mintAddress
) where

import           Control.Lens
import           Control.Monad.Error.Lens
import           Data.Aeson               (FromJSON, ToJSON)
import qualified Data.Map                 as Map
import           GHC.Generics             (Generic)

import           Ledger                   hiding (initialise, to)
import           Ledger.Tx (CardanoTx, TxOutRef, getCardanoTxInputs, txInRef)
import           Ledger.Contexts          as V
import           Ledger.Typed.Scripts     (TypedValidator)
import qualified Ledger.Typed.Scripts     as Scripts
import qualified PlutusTx

import qualified Ledger.Constraints       as Constraints
import           Plutus.Contract          as Contract
import           Prelude

import qualified AltDex.Roundtrip.Nami as Nami

import Ledger.Ada qualified as Ada
import Data.Void (Void)
import Data.Set qualified as Set
import Data.String

addr = "addr_test1qpg63zkhl7uqhp9kwqtn4ycwe6lts48x0m9xn5s2daqkffmjjswtzzefnd4p8khegmxe4qztsvk7u2vxqs8zdzfvjfms4eajxs"

mintAddress :: Maybe Address
mintAddress = Nami.parseAddress addr

mintPubKeyHash :: Maybe PubKeyHash
mintPubKeyHash = Nami.parsePubKeyHash addr

getPubKeyHash' :: forall w s e. (AsContractError e, Data.String.IsString e) => Contract w s e PubKeyHash
getPubKeyHash' =
  case mintPubKeyHash of
    Just a -> pure a
    _ -> Contract.throwError "Can not parse source UTXO address for minting TxOutRef owner"

-- getUnspentOutput' :: AsContractError e => Contract w s e TxOutRef
-- getUnspentOutput' = do
--     ownPK <- getPubKeyHash'
--     let constraints = Constraints.mustPayToPubKey ownPK (Ada.lovelaceValueOf 1)
--     utx <- either (throwing _ConstraintResolutionError) pure (Constraints.mkTx @Void mempty constraints)
--     tx <- Contract.balanceTx (adjustUnbalancedTx utx)
--     case Set.lookupMin (getCardanoTxInputs tx) of
--         Just inp -> pure $ txInRef inp
--         Nothing  -> throwing _OtherError "Balanced transaction has no inputs"