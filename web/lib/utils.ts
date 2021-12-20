import type * as WasmNamespace from "@emurgo/cardano-serialization-lib-browser";
import CoinSelection from "./coinSelection";
import toHex from "to-hex";

import { Asset, GqlUtxo, SwapAsset, Utxo } from "./tx-builder/types";
import { address as swapAddress } from "../plutus/altswap.plutus";
import { factoryAsset } from "../plutus/create-lp.plutus";
import { Wallet } from "../context/nami-wallet";
import { getScriptUtxos } from "../graphql/scriptUtxos";
import { query } from "../graphql/client";

type WasmT = typeof WasmNamespace;

export const fromHex = (hex: string) => Buffer.from(hex, "hex");

export const getFactoryUtxo = async (
  wasm: WasmT
): Promise<WasmNamespace.TransactionUnspentOutput | undefined> => {
  const utxos: GqlUtxo[] = await query(
    getScriptUtxos.name,
    getScriptUtxos.document,
    {
      address: swapAddress,
    }
  );

  const factoryUtxo = filterFactoryUtxo(utxos);

  return factoryUtxo
    ? transformGqlUtxoToTxUnspentOutput(wasm, factoryUtxo)
    : undefined;
};

export const filterFactoryUtxo = (utxos: GqlUtxo[]): GqlUtxo | undefined =>
  utxos.find((utxo) =>
    utxo.tokens.find(
      (tokens) =>
        tokens.asset.policyId === factoryAsset.policyId &&
        tokens.asset.assetName === factoryAsset.nameHex
    )
  );

export const transformGqlUtxoToTxUnspentOutput = (
  wasm: WasmT,
  gqlUtxo: GqlUtxo
): WasmNamespace.TransactionUnspentOutput => {
  return wasm.TransactionUnspentOutput.new(
    wasm.TransactionInput.new(
      wasm.TransactionHash.from_bytes(fromHex(gqlUtxo.txHash)),
      gqlUtxo.index
    ),
    wasm.TransactionOutput.new(
      wasm.Address.from_bech32(gqlUtxo.address),
      transformGqlUtxoAssetsToValue(wasm, gqlUtxo)
    )
  );
};

type MutliAssetMap = {
  [policyId: string]: {
    [assetName: string]: string;
  };
};

export const transformGqlUtxoAssetsToValue = (
  wasm: WasmT,
  gqlUtxo: GqlUtxo
): WasmNamespace.Value => {
  const val = wasm.Value.new(
    wasm.BigNum.from_str(gqlUtxo.value) // lovelace
  );

  const multiAsset = wasm.MultiAsset.new();

  const multiAssetMap = gqlUtxo.tokens.reduce((acc, t) => {
    const {
      asset: { policyId, assetName },
      quantity,
    } = t;

    const val = acc[policyId] || {};

    acc[policyId] = { ...val, [assetName]: quantity };
    return acc;
  }, {} as MutliAssetMap);

  // @ts-ignore
  Object.entries(multiAssetMap).forEach(([key, value]) => {

    const scriptHash = wasm.ScriptHash.from_bytes(
      fromHex(key)
    )
    const assets = wasm.Assets.new();

    Object.entries(value).forEach(([name, quantity]) => {
      const assetName = wasm.AssetName.new(
        Buffer.from(name, 'hex')
      );
      assets.insert(assetName, wasm.BigNum.from_str(quantity));
    });

    multiAsset.insert(scriptHash, assets);
  });

  val.set_multiasset(multiAsset);

  return val;
};

export const encodeCbor = (val: any): string =>
  Buffer.from(val.to_bytes()).toString("hex");

export const decodeAddress =
  (wasm: WasmT) =>
  (encodedAddress: string): string =>
    wasm.Address.from_bytes(Buffer.from(encodedAddress, "hex")).to_bech32();

export const decodeUtxo =
  (wasm: WasmT) =>
  (encodedUtxo: string): WasmNamespace.TransactionUnspentOutput =>
    wasm.TransactionUnspentOutput.from_bytes(Buffer.from(encodedUtxo, "hex"));

export const protocolParameters = {
  linearFee: {
    minFeeA: "44",
    minFeeB: "155381",
  },
  minUtxo: "34482",
  poolDeposit: "500000000",
  keyDeposit: "2000000",
  maxValSize: 5000,
  maxTxSize: 16384,
  priceMem: 0.0577,
  priceStep: 7.21e-5,
  utxoCostPerWord: 34482,
};

export const MAX_VALUE_BYTES = 5000;
export const MAX_TX_BYTES = 16384;

export const initTx = (wasm: WasmT) => {
  const outputs = wasm.TransactionOutputs.new();
  const inputs = wasm.TransactionInputs.new();

  return {
    outputs,
    inputs,
  };
};

export const getCoinSelection = (wasm: WasmT) => {
  CoinSelection.setWasm(wasm);
  CoinSelection.setProtocolParameters(
    protocolParameters.minUtxo,
    protocolParameters.linearFee.minFeeA,
    protocolParameters.linearFee.minFeeB,
    protocolParameters.maxTxSize.toString()
  );

  return CoinSelection;
};

export const createCollateralUtxos = async (
  wasm: WasmT,
  wallet: Wallet
): Promise<Utxo[]> => {
  const txInCollateral: Utxo[] = [];
  const collateralUtxos = (await wallet.getCollateral()).map(decodeUtxo(wasm));

  collateralUtxos.forEach((utxo) =>
    txInCollateral.push({
      txHash: toHex(utxo.input().transaction_id().to_bytes()),
      txId: utxo.input().index(),
      value: {
        lovelace: Number.parseInt(utxo.output().amount().coin().to_str()),
      },
    })
  );

  return txInCollateral;
};

export const getWalletUtxos = async (
  wasm: WasmT,
  wallet: Wallet
): Promise<WasmNamespace.TransactionUnspentOutput[]> => {
  const encodedUtxos = await wallet?.getUtxos();
  const decodedUtxos: WasmNamespace.TransactionUnspentOutput[] =
    encodedUtxos.map(decodeUtxo(wasm));

  return decodedUtxos;
};

export const getWalletAddress = async (
  wasm: WasmT,
  wallet: Wallet
): Promise<WasmNamespace.Address> => {
  const encodedAddress = (await wallet?.getUsedAddresses())[0];
  const decodedAddress = wasm.Address.from_bech32(
    decodeAddress(wasm)(encodedAddress)
  );

  return decodedAddress;
};

export const transformAssetToSwapAsset = (asset: Asset): SwapAsset => {
  const [policyId, nameHex] = asset.asset.split('.')

  return {
    policyId,
    nameHex,
    name: nameHex ? Buffer.from(nameHex, 'hex').toString() : "",
    unit: "",
    amount: asset.quantity,
    ratio: 0
  }
}

export const swapAssetsToValue = (
  wasm: WasmT,
  swap: SwapAsset
): WasmNamespace.Value => {
  const val = wasm.Value.zero();
  const multiAssets = wasm.MultiAsset.new();

  const swapAssetToMultiasset = (asset: SwapAsset) => {
    const assetName = wasm.AssetName.new(Buffer.from(asset.nameHex || "", "hex"));
    const assets = wasm.Assets.new();
    assets.insert(assetName, wasm.BigNum.from_str(asset.amount.toString()));

    const key = wasm.ScriptHash.from_bytes(fromHex(asset.policyId || ""));
    multiAssets.insert(key, assets);
  };

  // '' is lovelaces
  swap.nameHex === ""
    ? val.set_coin(wasm.BigNum.from_str(swap.amount.toString()))
    : swapAssetToMultiasset(swap);

  val.set_multiasset(multiAssets);
  return val;
};

export const hexToAscii = (hex: string) => Buffer.from(hex, "hex").toString();

export const valueToAssets = async (value: WasmNamespace.Value): Promise<SwapAsset[]> => {
  const assets = [];
  const lovelaces = value.coin().to_str()
  assets.push({ unit: "lovelace", quantity: lovelaces, amount: Number.parseInt(lovelaces) , ratio: 0, name: "ADA"});
  const multiAsset = value.multiasset();

  if (multiAsset) {
    const multiAssets = multiAsset.keys();

    for (let j = 0; j < multiAssets.len(); j++) {
      const policy = multiAssets.get(j);
      const policyAssets = multiAsset.get(policy);

      if (policyAssets) {
        const assetNames = policyAssets.keys();

        for (let k = 0; k < assetNames.len(); k++) {
          const policyAsset = assetNames.get(k);
          const quantity = policyAssets.get(policyAsset);
          const _policy = Buffer.from(policy.to_bytes()).toString("hex");
          const _name = Buffer.from(policyAsset.name()).toString("hex");
          const unit = `${_policy}.${_name}`;

          const asset: SwapAsset = {
            unit,
            amount: Number.parseInt(quantity?.to_str() || "0"),
            policyId: _policy,
            name: hexToAscii(_name),
            ratio: 0
            // fingerprint
          };
          assets.push(asset);
        }
      }
    }
  }

  return assets as SwapAsset[];
};


type MergedAssetsMap = {
  [unit: string]: SwapAsset
}

export const mergeSwapAssets = (assets: SwapAsset[]): SwapAsset[] => {
  const merged = assets.reduce((acc, asset) => {
    if (acc[asset.unit]) {
        acc[asset.unit].amount = acc[asset.unit].amount + asset.amount
    } else {
      acc[asset.unit] = asset
    }

    return acc;
  }, {} as MergedAssetsMap)

  return Object.values(merged)
}
