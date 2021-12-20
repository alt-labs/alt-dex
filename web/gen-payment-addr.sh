#!/bin/bash

set -ex

TESTNET_MAGIC_NUM=1097911063

WALLET_NAME=$1
WALLET_DIR="wallets"
PAY_KEY="${WALLET_NAME}-pay"
STAKE_KEY="${WALLET_NAME}-stake"
PAY_ADDR_FILE="${WALLET_NAME}-pay.addr"

cardano-cli address key-gen \
    --verification-key-file "${WALLET_DIR}/${PAY_KEY}.vkey" \
		--signing-key-file "${WALLET_DIR}/${PAY_KEY}.skey"

cardano-cli stake-address key-gen \
    --verification-key-file "$WALLET_DIR/$STAKE_KEY.vkey" \
    --signing-key-file "$WALLET_DIR/$STAKE_KEY.skey"

cardano-cli address build \
    --payment-verification-key-file "$WALLET_DIR/${PAY_KEY}.vkey" \
    --stake-verification-key-file "$WALLET_DIR/${STAKE_KEY}.vkey" \
    --out-file "$WALLET_DIR/${PAY_ADDR_FILE}" \
    --testnet-magic $TESTNET_MAGIC_NUM

cat "$WALLET_DIR/$PAY_ADDR_FILE"
