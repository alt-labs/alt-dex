import type * as WasmNamespace from "@emurgo/cardano-serialization-lib-browser";
import { Wallet } from "../context/nami-wallet";
import {
  buildMint,
  buildTx,
  finalizeTx,
  transformTransactionOutputsToTxOutList,
  transformTransactionUnspentOutputToTxInList,
} from "./tx-builder";
import {
  address as swapAddress,
  scriptObj as altswapScript,
} from "../plutus/altswap.plutus";
import { scriptObj as createLpScript } from "../plutus/create-lp.plutus";
import {
  createCollateralUtxos,
  getCoinSelection,
  getFactoryUtxo,
  getWalletAddress,
  getWalletUtxos,
  initTx,
  swapAssetsToValue,
  transformAssetToSwapAsset,
} from "./utils";
import {
  getCreateLPOutDatum,
  getFactoryInDatum,
  getFactoryOutDatum,
} from "./datums";
import httpClient from "./tx-builder/client";

import { getCreateFactoryInRedeemer, getVoidRedeemer } from "./redeemers";
import toHex from "to-hex";

type WasmT = typeof WasmNamespace;

export const createLp = async (wasm: WasmT, wallet: Wallet) => {
  // basics
  const { outputs } = initTx(wasm);
  const walletUtxos = await getWalletUtxos(wasm, wallet);
  const walletAddress = await getWalletAddress(wasm, wallet);
  const scriptAddress = wasm.Address.from_bech32(swapAddress);
  const changeAddress = walletAddress.to_bech32();
  const txInCollateral = await createCollateralUtxos(wasm, wallet);
  const coinSelection = getCoinSelection(wasm);
  const factoryUtxo = await getFactoryUtxo(wasm);
  const factoryUtxoTxHash = factoryUtxo
    ? toHex(factoryUtxo?.input().transaction_id().to_bytes())
    : undefined;

  // TODO: Load this from frontend
  const swapAssetFirst = {
    policyId: "fece0e5da9d8b4c1ed4e5831ffc5f04c2e6299083c9c27da271d34772a364367",
    nameHex: "4d4143",
    name: "MAC",
    amount: 100,
    unit: "",
  };
  const swapAssetSecond = {
    policyId: "fece0e5da9d8b4c1ed4e5831ffc5f04c2e6299083c9c27da271d34772a364367",
    nameHex: "53524b",
    name: "SRK",
    amount: 100,
    unit: "",
  };

  const mintLiquidityAsset = {
    asset: `eabf4269f386c4940cd9345886dd79dc4eb2a34ba591995bd61e3c66.c1de70fe12e37dfde62bd6b8e4b56049a661c56b39e7a26009afa846e96c0aa6`,
    quantity: Math.sqrt(swapAssetFirst.amount * swapAssetSecond.amount),
  };
  const mintPoolToken = {
    asset: `eabf4269f386c4940cd9345886dd79dc4eb2a34ba591995bd61e3c66.4c5053`,
    quantity: 1,
  };

  const liquidityAsset = transformAssetToSwapAsset(mintLiquidityAsset);
  const poolAsset = transformAssetToSwapAsset(mintPoolToken);

  // tx in datums & redeemers
  const mintRedeemerJSON = getVoidRedeemer();
  const factoryInDatum = getFactoryInDatum();
  const factoryInRedeemer = getCreateFactoryInRedeemer(
    swapAssetFirst,
    swapAssetSecond
  );
  // tx out datums
  const factoryOutDatum = getFactoryOutDatum(swapAssetFirst, swapAssetSecond);
  const lpOutDatum = getCreateLPOutDatum(
    liquidityAsset,
    swapAssetFirst,
    swapAssetFirst
  );
  // hash tx out datums
  const factoryOutDatumHash = await httpClient.hashData(factoryOutDatum);
  const lpDatumOutHash = await httpClient.hashData(lpOutDatum);

  // create minting transactions
  const mintLiquidityTokens = buildMint(
    mintLiquidityAsset,
    createLpScript,
    undefined,
    mintRedeemerJSON
  );
  const mintPoolTokens = buildMint(
    mintPoolToken,
    createLpScript,
    undefined,
    mintRedeemerJSON
  );

  // create outputs
  const outputValueFirst = swapAssetsToValue(wasm, swapAssetFirst);
  const outputValueSecond = swapAssetsToValue(wasm, swapAssetSecond);
  const outputValueLiquidityTokens = swapAssetsToValue(wasm, liquidityAsset);
  const outputValuePoolTokens = swapAssetsToValue(wasm, poolAsset);

  // add first token output
  const spendingOutputFirst = wasm.TransactionOutput.new(
    scriptAddress,
    outputValueFirst
  );

  // add second token output
  const spendingOutputSecond = wasm.TransactionOutput.new(
    scriptAddress,
    outputValueSecond
  );

  // send newly minted liquidity tokens to creator
  const liquidityOutput = wasm.TransactionOutput.new(
    walletAddress,
    outputValueLiquidityTokens
  );
  liquidityOutput.set_data_hash(
    wasm.DataHash.from_bytes(Buffer.from(lpDatumOutHash.data, "hex"))
  );

  // send newly minted pool tokens to script address
  const poolOutput = wasm.TransactionOutput.new(
    scriptAddress,
    outputValuePoolTokens
  );

  // factory input goes back to script address
  const factoryOutput = wasm.TransactionOutput.new(
    scriptAddress,
    factoryUtxo?.output().amount() || wasm.Value.zero()
  );

  factoryOutput.set_data_hash(
    wasm.DataHash.from_bytes(Buffer.from(factoryOutDatumHash.data, "hex"))
  );

  // first add spending outputs
  outputs.add(spendingOutputFirst);
  outputs.add(spendingOutputSecond);
  outputs.add(factoryOutput);

  const { input, output, change } = coinSelection.randomImprove(
    walletUtxos,
    outputs,
    8,
    factoryUtxo ? [factoryUtxo] : []
  );

  // add minting outputs after balancing to prevent exhausting inputs
  output.add(liquidityOutput);
  output.add(poolOutput);

  const txOut = transformTransactionOutputsToTxOutList(output);
  const txIn = transformTransactionUnspentOutputToTxInList(input);

  // attach script, datum and redeemer tx-in holding factory token
  txIn.map((tx) => {
    if (tx.txHash === factoryUtxoTxHash) {
      Object.assign(tx, {
        script: altswapScript,
        redeemerJSON: factoryInRedeemer,
        datumJSON: factoryInDatum,
      });
    }

    return tx;
  });

  // build tx payload for sending to tx builder over http
  const txPayload = buildTx(
    txIn,
    txOut,
    undefined, // fee
    [mintLiquidityTokens, mintPoolTokens], // mint
    changeAddress,
    txInCollateral
  );

  console.log("Tx Payload", txPayload);

  // finalize tx
  await finalizeTx(wasm, wallet, txPayload);
};
