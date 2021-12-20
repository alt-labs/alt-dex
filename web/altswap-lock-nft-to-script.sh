#!/bin/bash
set -xe

TESTNET_MAGIC_NUM=1097911063

FROM_UTXO="e899b80b6e68f36f655e1877733a6391b40b5c9a8f197d0a37b15b6940a3436b#0"
TX_IN_COLLATERAL="e899b80b6e68f36f655e1877733a6391b40b5c9a8f197d0a37b15b6940a3436b#0"
SCRIPT_ADDRESS=$($CARDANO_CLI address build --payment-script-file plutus-scripts/altswap.plutus --testnet-magic $TESTNET_MAGIC_NUM)
DATUM_HASH=$($CARDANO_CLI transaction hash-script-data --script-data-file altswap-factory-empty-datum.json)
TO_ADDR=$SCRIPT_ADDRESS

FROM_WALLET_NAME="igor2-pay"
FROM_WALLET_ADDRESS=$(cat wallets/igor2-pay.addr)

TOKEN_AMOUNT=1
TOKEN_NAME=535750

POLICY_FILE="plutus-scripts/altswap-nft.plutus"
POLICY_ID_PATH="policy/swap-nft-policy-id"
cardano-cli transaction policyid --script-file $POLICY_FILE > $POLICY_ID_PATH
POLICY_ID=$(< $POLICY_ID_PATH)


MINT_ARG="$TOKEN_AMOUNT $POLICY_ID.$TOKEN_NAME"

cardano-cli transaction build \
  --tx-in ${FROM_UTXO} \
  --tx-out ${TO_ADDR}"+1689618+$TOKEN_AMOUNT $POLICY_ID.$TOKEN_NAME" \
  --tx-out-datum-hash ${DATUM_HASH} \
  --change-address ${FROM_WALLET_ADDRESS} \
  --testnet-magic ${TESTNET_MAGIC_NUM} \
  --minting-script-file $POLICY_FILE \
  --mint-redeemer-file plutus/redeemer-void.json \
  --tx-in-collateral $TX_IN_COLLATERAL \
  --mint="$MINT_ARG" \
  --out-file tx.build \
  --protocol-params-file pparams.json \
  --alonzo-era

cardano-cli transaction sign \
--tx-body-file tx.build \
--signing-key-file ./wallets/${FROM_WALLET_NAME}.skey \
--out-file tx.signed

echo "All good, submit Tx via:"
echo "cardano-cli transaction submit --tx-file tx.signed --testnet-magic $TESTNET_MAGIC_NUM"
