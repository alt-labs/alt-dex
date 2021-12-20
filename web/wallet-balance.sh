#!/bin/bash

cardano-cli query utxo --address $(cat wallet/payment.addr) --testnet-magic 1097911063
