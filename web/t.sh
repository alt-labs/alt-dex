#!/bin/bash

LIQUIDITY_TOKEN_NAME="0x082b8cd3ea6ec4736e11d250e22336643d372b3d72b3a9481bd69be69795c9eb"
LIQUIDITY_TOKEN_HEX=$(echo LIQUIDITY_TOKEN_NAME | xxd -p)

echo $LIQUIDITY_TOKEN_HEX
