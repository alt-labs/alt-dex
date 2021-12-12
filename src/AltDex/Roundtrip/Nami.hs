module AltDex.Roundtrip.Nami
    (
        parseAddress,
        parsePubKeyHash
    ) where

import Cardano.Api (AsType (AsAddressInEra, AsAlonzoEra))
import Cardano.Api qualified
import Plutus.Contract.CardanoAPI (FromCardanoError, fromCardanoAddress, toCardanoAddress)
import Prelude
import Ledger (TxOut(..), TxOutRef, PubKeyHash (PubKeyHash), Value, Address, toPubKeyHash, outputs)
import Control.Monad (liftM)
import Data.Text
import Control.Monad.Freer.Error (Error, throwError)

parseAddress :: Text -> Maybe Address
parseAddress addr =
  case fromCardanoAddress <$> Cardano.Api.deserialiseAddress (AsAddressInEra AsAlonzoEra) addr of
    Just (Right a) -> pure a
    _ -> Nothing

parsePubKeyHash :: Text -> Maybe PubKeyHash
parsePubKeyHash addr = case parseAddress addr of
    Just a -> toPubKeyHash a
    _ -> Nothing