#!/bin/bash

set -xe
W_NAME="$1-pay.addr"
ADDR=$(cat "./wallets/$W_NAME")
cardano-cli query utxo --address $ADDR  --testnet-magic 1097911063
