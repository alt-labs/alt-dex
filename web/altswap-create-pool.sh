# #!/bin/bash
set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YEL='\033[0;36m'
BLU='\033[0;33m'
NC='\033[0m' # No Color

TESTNET_MAGIC_NUM=1097911063

# Nami yummy
TOKENS_UTXO="fece0e5da9d8b4c1ed4e5831ffc5f04c2e6299083c9c27da271d34772a364367#0"
TOKENS_ADDR="addr_test1qrq235kp03whg4ln6ngg2m6fpuefx0h8un04wmn25xugda0z6j8vdpphevjyl9yfkc4fsnffwkw2fghww3sm8zgu92nqzpxlw4"
FEE=1500000
LOVELACE_IN=63626474
LOVELACE_OUT=60195482

TX_IN_COLLATERAL="92c2a77af3bcaede0ba899662422e9eb99e7c3a6f10979a3d7711005ef2a12a7#0"

SWAP_SCRIPT_FILE="plutus-scripts/altswap.plutus"
SWAP_SCRIPT_ADDRESS=$(cardano-cli address build --payment-script-file $SWAP_SCRIPT_FILE --testnet-magic $TESTNET_MAGIC_NUM)
SWAP_POOL_DATUM_FILE="plutus/altswap-lp-create-pool-datum.json"
SWAP_POOL_REDEEMER_FILE="plutus/altswap-create-pool-redeemer.json"

printf "${GREEN}Script balance: ${NC} $SWAP_SCRIPT_ADDRESS:\n"
./altswap-script-balance.sh

SWAP_FACTORY_NFT_CURRENCY_SYMBOL=$(cardano-cli transaction policyid --script-file plutus-scripts/altswap-nft.plutus)
SWAP_FACTORY_NFT_TOKEN_NAME="SWP"
SWAP_FACTORY_NFT_TOKEN_HEX=$(printf $SWAP_FACTORY_NFT_TOKEN_NAME | xxd -p)
printf "${BLU}SWAP_FACTORY_NFT_TOKEN_HEX: ${NC} $SWAP_FACTORY_NFT_TOKEN_HEX:\n"
SWAP_FACTORY_NFT=$SWAP_FACTORY_NFT_CURRENCY_SYMBOL"."$SWAP_FACTORY_NFT_TOKEN_HEX
SWAP_FACTORY_DATUM_FILE="plutus/altswap-lp-create-factory-datum.json"
SWAP_FACTORY_DATUM_HASH=$(cardano-cli transaction hash-script-data --script-data-file $SWAP_FACTORY_DATUM_FILE)


LP_STATE_COIN_SCRIPT_FILE="plutus-scripts/altswap-lp-create-mint.plutus"
LP_STATE_COIN_CURRENCY_SYMBOL=$(cardano-cli transaction policyid --script-file $LP_STATE_COIN_SCRIPT_FILE)
LP_STATE_COIN_TOKEN_NAME="LPS"
LP_STATE_COIN_TOKEN_HEX=`printf "${LP_STATE_COIN_TOKEN_NAME}" | xxd -p`
LP_STATE_COIN_VALUE=$LP_STATE_COIN_CURRENCY_SYMBOL"."$LP_STATE_COIN_TOKEN_HEX
printf "${BLU}LP_STATE_COIN_VALUE:${NC} $LP_STATE_COIN_VALUE\n"

TOKEN_A_BALANCE=1000000
TOKEN_B_BALANCE=1000000

TOKEN_AMOUNT_A=1000
TOKEN_AMOUNT_B=2000

TOKEN_A_CHANGE=$(python -c "print(int($TOKEN_A_BALANCE-$TOKEN_AMOUNT_A))")
TOKEN_B_CHANGE=$(python -c "print(int($TOKEN_B_BALANCE-$TOKEN_AMOUNT_B))")

TOKEN_NAME_A="MAC"
TOKEN_NAME_B="SRK"
TOKEN_NAME_A_HEX=$(printf $TOKEN_NAME_A | xxd -p)
TOKEN_NAME_B_HEX=$(printf $TOKEN_NAME_B | xxd -p)

TOKENS_CURRENCY_SYMBOL=$(cardano-cli transaction policyid --script-file plutus-scripts/altswap-tokens.plutus)

TOKEN_A_VALUE=$TOKENS_CURRENCY_SYMBOL"."$TOKEN_NAME_A_HEX
TOKEN_B_VALUE=$TOKENS_CURRENCY_SYMBOL"."$TOKEN_NAME_B_HEX

LIQUIDITY_TOKEN_NAME="0xc1de70fe12e37dfde62bd6b8e4b56049a661c56b39e7a26009afa846e96c0aa6"
LIQUIDITY_TOKEN_HEX=$(echo LIQUIDITY_TOKEN_NAME | xxd -p)
LIQUDIITY_CURRENCY_SYMBOL=$LP_STATE_COIN_CURRENCY_SYMBOL
LIQUIDITY_COIN_VALUE=$LIQUDIITY_CURRENCY_SYMBOL"."$LIQUIDITY_TOKEN_HEX
LIQUIDITY_AMOUNT=$(python -c "from math import ceil,sqrt; print(int(ceil(sqrt($TOKEN_AMOUNT_A*$TOKEN_AMOUNT_B))))")

printf "${GREEN}Calculated Liquidity: ${NC} ${LIQUIDITY_AMOUNT}\n"
printf "${GREEN}LIQUIDITY_COIN_VALUE: ${NC} $LIQUIDITY_COIN_VALUE\n"

FROM_WALLET_NAME="payment"
FROM_WALLET_ADDRESS=$(cat wallet/payment.addr)

OUT_1="${SWAP_SCRIPT_ADDRESS} + 1930992 + 1 $SWAP_FACTORY_NFT"
OUT_2="${SWAP_SCRIPT_ADDRESS} + 1930992 + 1 $LP_STATE_COIN_VALUE + $TOKEN_AMOUNT_A $TOKEN_A_VALUE + $TOKEN_AMOUNT_B $TOKEN_B_VALUE"
OUT_3="${FROM_WALLET_ADDRESS} + 1930992 + $LIQUIDITY_AMOUNT $LIQUIDITY_COIN_VALUE" \

OUT_TOKENS_CHANGE="${TOKENS_ADDR} + $LOVELACE_OUT + ${TOKEN_A_CHANGE} ${TOKEN_A_VALUE} + ${TOKEN_B_CHANGE} ${TOKEN_B_VALUE}"
printf "${BLU}OUT_TOKENS_CHANGE: ${NC} $OUT_TOKENS_CHANGE\n"


printf "${BLU}LP_STATE_COIN_VALUE: ${NC} $LP_STATE_COIN_VALUE:\n"
printf "${BLU}TOKEN_AMOUNT_A: ${NC} $TOKEN_AMOUNT_A:\n"
printf "${BLU}TOKEN_A_VALUE: ${NC} $TOKEN_A_VALUE:\n"
printf "${BLU}TOKEN_B_VALUE: ${NC} $TOKEN_B_VALUE:\n"

printf "${RED}-----------------------${NC}\n"
printf "${YEL}SHOULD: ${NC}\n\$SWAP_SCRIPT_ADDRESS + 1930992 +1 \$SWAP_FACTORY_NFT\n"
printf "${GREEN}OUT #1: ${NC}\n${OUT_1}\n"

printf "${RED}-----------------------${NC}\n"
printf "${YEL}SHOULD: ${NC}\nSWAP_SCRIPT_ADDRESS + 1930992 + 1 \$LP_STATE_COIN_VALUE + \$TOKEN_AMOUNT_A \$TOKENS_A_VALUE + \$TOKEN_AMOUNT_B \$TOKEN_B_VALUE\n"
printf "${GREEN}OUT #2: ${NC}\n${OUT_2}\n"

printf "${RED}-----------------------${NC}\n"
printf "${GREEN}OUT #3: ${NC}\n${OUT_3}\n"
printf "${RED}-----------------------${NC}\n"

set -x

cardano-cli transaction build-raw \
--fee $FEE \
--tx-in "e252520ea3a5d674bc67d7c74a8aae1c00979f20a7ca09e5edd11d40abc5fcf4#0" \
--tx-in ${TX_IN_COLLATERAL} \
--tx-in "05b9ccd48b6ab40d05433b29f7ca58036f635013a7a1d12e73afee8a4bec7c2e#1" \
--tx-in-script-file $SWAP_SCRIPT_FILE \
--tx-in-datum-file altswap-factory-empty-datum.json \
--tx-in-redeemer-file $SWAP_POOL_REDEEMER_FILE \
--tx-in-execution-units "(820000000,2400000)" \
--tx-in-collateral $TX_IN_COLLATERAL \
\
--tx-out "${OUT_1}" \
--tx-out-datum-embed-file $SWAP_FACTORY_DATUM_FILE \
\
--tx-in ${TOKENS_UTXO} \
--tx-out-datum-embed-file $SWAP_POOL_DATUM_FILE \
--tx-out "${OUT_2}" \
--tx-out "${OUT_TOKENS_CHANGE}" \
\
--tx-out "${OUT_3}" \
\
--minting-script-file $LP_STATE_COIN_SCRIPT_FILE \
--mint-redeemer-file plutus/redeemer-void.json \
--mint "1 $LP_STATE_COIN_VALUE + $LIQUIDITY_AMOUNT $LIQUIDITY_COIN_VALUE" \
\
--mint-execution-units "(820000000,2400000)" \
--out-file tx.build \
--protocol-params-file pparams.json \
--alonzo-era

#
# --tx-in-collateral $TX_IN_COLLATERAL \
#

cardano-cli transaction sign \
--tx-body-file tx.build \
--signing-key-file ./wallet/${FROM_WALLET_NAME}.skey \
--out-file tx.signed

echo "All good, copy generated CBOR and sign via Nami Wallet"
# echo "cardano-cli transaction submit --tx-file tx.signed --testnet-magic $TESTNET_MAGIC_NUM"


# # 18033d8530fb12a354ac85895e2fc9097ac6af689b2c98dd7dadc141c0c2834c
