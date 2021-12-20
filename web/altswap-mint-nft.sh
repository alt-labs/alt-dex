#!/bin/bash
set -xe

TESTNET_MAGIC_NUM=1097911063

FROM_UTXO="24ad046839b0dc606c9c5ae73bb0e8e586820f1cbe0559bdf5afb0ec94bacbdd#0"
TX_IN_COLLATERAL="48241b3cd8b04821b3d18d86c58581f7aa78cd3e143be22bfaca866dd3c68026#0"

FROM_WALLET_NAME="payment"
FROM_WALLET_ADDRESS=$(cat wallet/payment.addr)

TOKEN_AMOUNT=1
TOKEN_NAME=535750

POLICY_FILE="plutus-scripts/altswap-nft.plutus"
POLICY_ID_PATH="policy/swap-nft-policy-id"
cardano-cli transaction policyid --script-file $POLICY_FILE > $POLICY_ID_PATH
POLICY_ID=$(< $POLICY_ID_PATH)

cardano-cli transaction build \
--tx-in ${FROM_UTXO} \
--tx-in "fe9322ec4d0c08dd38220e7d3e3bc70cb68b88bc895b45589898b4260433eedf#0" \
--tx-out ${FROM_WALLET_ADDRESS}+"1928132"+"$TOKEN_AMOUNT $POLICY_ID.$TOKEN_NAME" \
--change-address ${FROM_WALLET_ADDRESS} \
--testnet-magic ${TESTNET_MAGIC_NUM} \
--minting-script-file $POLICY_FILE \
--mint-redeemer-file plutus/redeemer-void.json \
--tx-in-collateral $TX_IN_COLLATERAL \
--mint="$TOKEN_AMOUNT $(< $POLICY_ID_PATH).$TOKEN_NAME" \
--out-file tx.build \
--protocol-params-file pparams.json \
--alonzo-era

cardano-cli transaction sign \
--tx-body-file tx.build \
--signing-key-file ./wallet/${FROM_WALLET_NAME}.skey \
--out-file tx.signed

echo "All good, submit Tx via:"
echo "cardano-cli transaction submit --tx-file tx.signed --testnet-magic $TESTNET_MAGIC_NUM"
