#!/bin/bash

TESTNET_MAGIC_NUM=1097911063

SCRIPT_ADDR=$(cardano-cli address build --payment-script-file plutus-scripts/altswap.plutus --testnet-magic $TESTNET_MAGIC_NUM)
cardano-cli query utxo --address $SCRIPT_ADDR --testnet-magic $TESTNET_MAGIC_NUM
