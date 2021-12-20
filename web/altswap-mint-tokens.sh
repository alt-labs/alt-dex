#!/bin/bash
set -xe

FROM_UTXO="86616b7707fc9e08ff76e54b5e728933f59c5f16b826174f45bdabbc02ad0de9#0"
TX_IN_COLLATERAL="92c2a77af3bcaede0ba899662422e9eb99e7c3a6f10979a3d7711005ef2a12a7#0"
FROM_WALLET_NAME="payment"
FROM_WALLET_ADDRESS=$(cat wallet/payment.addr)
TESTNET_MAGIC_NUM=1097911063
TOKEN_AMOUNT=1000000
TOKEN_NAME1=4d4143
TOKEN_NAME2=53524b
# TOKEN_NAME3=444b54
# TOKEN_NAME4=424f4e


POLICY_ID_PATH="policy/altswapTokensPolicyID"
POLICY_FILE="./altswap-tokens.plutus"
cardano-cli transaction policyid --script-file $POLICY_FILE > $POLICY_ID_PATH
POLICY_ID=$(< $POLICY_ID_PATH)

# --tx-out ${FROM_WALLET_ADDRESS}+$TOKEN_COUNT+"$TOKEN_COUNT ${POLICY_ID}.${COIN_NAME}" \

cardano-cli transaction build \
--tx-in ${FROM_UTXO} \
--tx-out ${FROM_WALLET_ADDRESS}+1517208+"$TOKEN_AMOUNT $POLICY_ID.$TOKEN_NAME1 + $TOKEN_AMOUNT $POLICY_ID.$TOKEN_NAME2" \
--change-address ${FROM_WALLET_ADDRESS} \
--testnet-magic ${TESTNET_MAGIC_NUM} \
--minting-script-file $POLICY_FILE \
--mint-redeemer-file plutus/redeemer-void.json \
--tx-in-collateral $TX_IN_COLLATERAL \
--mint="$TOKEN_AMOUNT $(< $POLICY_ID_PATH).$TOKEN_NAME1 + $TOKEN_AMOUNT $(< $POLICY_ID_PATH).$TOKEN_NAME2" \
--out-file tx.build \
--protocol-params-file pparams.json \
--alonzo-era

cardano-cli transaction sign \
--tx-body-file tx.build \
--signing-key-file ./wallet/${FROM_WALLET_NAME}.skey \
--out-file tx.signed

echo "cardano-cli transaction submit --tx-file tx.signed --testnet-magic $TESTNET_MAGIC_NUM"

