import {
  GqlUtxo,
  TransactionPayload,
  TxIn,
  TxOut,
  Utxo,
  Address,
  Datum,
  ExecutionUnits,
  Redeemer,
  PlutusScript,
  Mint,
  MintScript,
  Value,
  Asset,
} from "./types";
import toHex from "to-hex";

import type * as WasmNamespace from "@emurgo/cardano-serialization-lib-browser";
import { getWalletAddress } from "../utils";
import { Wallet } from "../../context/nami-wallet";
type WasmT = typeof WasmNamespace;

import httpClient from "./client";

// transformers
export const transformGqlUtxoToUtxo = (gqlUtxo: GqlUtxo): Utxo => ({
  txHash: gqlUtxo.txHash,
  txId: gqlUtxo.index,
  value: {
    lovelace: Number.parseInt(gqlUtxo.value),
  },
});

export const transformValueToValueMap = (value: WasmNamespace.Value): Value => {
  const multiassetMap = {};
  const multiAsset = value.multiasset();
  const mutliAssetKeys = multiAsset?.keys();

  if (mutliAssetKeys) {
    for (let i = 0; i < mutliAssetKeys.len(); i++) {
      const scriptHash = mutliAssetKeys.get(i);
      const assets = multiAsset?.get(scriptHash);
      const assetNames = assets?.keys();

      if (assetNames) {
        for (let j = 0; j < assetNames.len(); j++) {
          const assetName = assetNames?.get(j);

          if (assetName) {
            const asset = assets?.get(assetName);

            const name = Buffer.from(assetName.name()).toString("hex");
            const key = `${toHex(scriptHash.to_bytes())}.${name}`;
            const quantity = asset?.to_str();

            Object.assign(multiassetMap, { [key]: quantity });
          }
        }
      }
    }
  }

  return { ...multiassetMap, lovelace: Number.parseInt(value.coin().to_str()) };
};

export const transformTransactionOutputsToTxOutList = (
  txOutputs: WasmNamespace.TransactionOutputs
): TxOut[] => {
  const res: TxOut[] = [];

  for (let i = 0; i < txOutputs.len(); i++) {
    const tOutput = txOutputs.get(i);
    const datumHashBytes = tOutput.data_hash()?.to_bytes();

    res.push({
      address: tOutput.address().to_bech32(),
      datumHash: datumHashBytes ? toHex(datumHashBytes) : undefined,
      value: transformValueToValueMap(tOutput.amount()),
    });
  }
  return res;
};

export const transformTransactionUnspentOutputToTxInList = (
  txUnspentOutputs: WasmNamespace.TransactionUnspentOutput[]
): TxIn[] => {
  return txUnspentOutputs.map((utxo) => ({
    txId: utxo.input().index(),
    txHash: toHex(utxo.input().transaction_id().to_bytes()),
    value: transformValueToValueMap(utxo.output().amount()),
  }));
};

export const buildMint = (
  asset: Asset,
  script?: MintScript,
  redeemer?: Redeemer,
  redeemerJSON?: Redeemer,
  executionUnits?: ExecutionUnits
): Mint => ({
  ...asset,
  script,
  redeemer,
  redeemerJSON,
  executionUnits,
  action: "mint",
});

export const buildTxIn = (
  utxo: Utxo,
  script?: PlutusScript,
  redeemer?: Redeemer,
  datum?: Datum,
  executionUnits?: ExecutionUnits
): TxIn => ({
  ...utxo,
  script,
  datum,
  redeemer,
  executionUnits,
});

export const buildTx = (
  txIn: TxIn[],
  txOut: TxOut[],
  fee?: number,
  mint?: Mint[],
  changeAddress?: Address,
  txInCollateral?: Utxo[],
  witnessCount?: number
): TransactionPayload => ({
  txIn,
  txOut,
  fee,
  mint,
  changeAddress,
  txInCollateral,
  witnessCount,
});

export const finalizeTx = async (
  wasm: WasmT,
  wallet: Wallet,
  txPayload: TransactionPayload
) => {
  const decodedAddress = await getWalletAddress(wasm, wallet);
  const response = await httpClient.sendTransactionRequest(txPayload);
  const txParsed = JSON.parse(
    Buffer.from(response.data.data, "hex").toString()
  );

  /*
   * Load and sign transaction
   */
  const txCli = wasm.Transaction.from_bytes(
    Buffer.from(txParsed.cborHex, "hex")
  );
  const txBody = txCli.body();

  const witnessSet = txCli.witness_set();

  witnessSet.vkeys()?.free();

  const walletAddress = wasm.BaseAddress.from_address(decodedAddress);
  const requiredSigners = wasm.Ed25519KeyHashes.new();
  // @ts-ignore
  requiredSigners.add(walletAddress.payment_cred().to_keyhash());
  txBody.set_required_signers(requiredSigners);

  const tx = wasm.Transaction.new(txBody, witnessSet);

  // encode tx and sign it using wallet
  const encodedTx = Buffer.from(tx.to_bytes()).toString("hex");
  const encodedTxVkeyWitnesses = await wallet.signTx(encodedTx, true);

  const txVkeyWitnesses = wasm.TransactionWitnessSet.from_bytes(
    Buffer.from(encodedTxVkeyWitnesses, "hex")
  );

  // @ts-ignore
  witnessSet.set_vkeys(txVkeyWitnesses.vkeys());

  const txSigned = wasm.Transaction.new(tx.body(), witnessSet);

  const encodedSignedTx = Buffer.from(txSigned.to_bytes()).toString("hex");

  const txHash = await wallet.submitTx(encodedSignedTx);
  return txHash;
};
