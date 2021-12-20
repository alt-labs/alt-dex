import type * as WasmNamespace from "@emurgo/cardano-serialization-lib-browser";
import { Wallet } from "../context/nami-wallet";
import {
  buildMint,
  buildTx,
  finalizeTx,
} from "./tx-builder";
import {
  address as swapAddress,
  scriptObj as altswapScript,
} from "../plutus/altswap.plutus";
import { factoryAsset, scriptObj as createLpScript, policyId as createLpPolicy } from "../plutus/create-lp.plutus";
import {
  getFactoryUtxo,
  getWalletAddress,
  transformAssetToSwapAsset,
} from "./utils";
import {
  getCreateLPOutDatum,
  getFactoryInDatum,
  getFactoryOutDatum,
} from "./datums";

import { getCreateFactoryInRedeemer, getVoidRedeemer } from "./redeemers";
import toHex from "to-hex";
import { ExecutionUnits, TxIn, TxOut, Utxo } from "./tx-builder/types";

type WasmT = typeof WasmNamespace;

export const createLp = async (wasm: WasmT, wallet: Wallet) => {
  // basics
  const walletAddress = await getWalletAddress(wasm, wallet);
  const changeAddress = walletAddress.to_bech32();
  const factoryUtxo = await getFactoryUtxo(wasm);

  const EXEC_UNITS: ExecutionUnits = [820000000, 2400000]
  const FEE = 2_000_000 // 2 ADA fee;
  const IN_LOVELACE = 227_128_475;
  const IN_MAC_TOTAL = 1_000_000;
  const IN_SRK_TOTAL = 1_000_000;
  const OUT_LOVELACE = 2_000_000;
  const OUT_MAC = 100;
  const OUT_SRK = 100;
  const COLLATERAL_LOVELACE = 5_000_000;


  // TODO: Load this from frontend
  const swapAssetFirst = {
    policyId: "da00eb76155a782d6b216feccd5c5a69f30c0326239662b85ed88966",
    nameHex: "4d4143",
    name: "MAC",
    amount: 100,
    unit: "",
  };

  const swapAssetSecond = {
    policyId: "da00eb76155a782d6b216feccd5c5a69f30c0326239662b85ed88966",
    nameHex: "53524b",
    name: "SRK",
    amount: 100,
    unit: "",
  };

  const mintLiquidityAsset = {
    asset: `${createLpPolicy}.524557`,
    quantity: Math.sqrt(swapAssetFirst.amount * swapAssetSecond.amount),
  };
  const mintPoolToken = {
    asset: `${createLpPolicy}.4c5053`,
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
    swapAssetSecond
  );


  const factoryTxIn: TxIn = {
    txHash: toHex(factoryUtxo?.input().transaction_id().to_bytes()),
    txId: factoryUtxo?.input().index() || 0,
    value: {
      lovelace: 1689618,
      [`${factoryAsset.policyId}.${factoryAsset.nameHex}`]: 1
    },
    script: altswapScript,
    datumJSON: factoryInDatum,
    redeemerJSON: factoryInRedeemer,
    executionUnits: EXEC_UNITS,
  }


  const tokensTxIn: TxIn = {
    txHash: 'd608bf8c1de8b51cb13ba4157d9cbf2623223ded412fde3042f757341b7c319d',
    txId: 0,
    value: {
      lovelace: IN_LOVELACE,
      'da00eb76155a782d6b216feccd5c5a69f30c0326239662b85ed88966.4d4143': IN_MAC_TOTAL, // mac
      'da00eb76155a782d6b216feccd5c5a69f30c0326239662b85ed88966.53524b': IN_SRK_TOTAL, // srk
    },
  }

  const collateralTxIn: Utxo[] = [{
    txHash: '92c2a77af3bcaede0ba899662422e9eb99e7c3a6f10979a3d7711005ef2a12a7',
    txId: 0,
    value: {
      lovelace: COLLATERAL_LOVELACE,
    },
  }]

  const factoryTxOut: TxOut = {
    address: swapAddress,
    datumEmbedFile: factoryOutDatum,
    value: {
      ...factoryTxIn.value
    }
  }

  const lpTxOut: TxOut = {
    address: swapAddress,
    datumEmbedFile: lpOutDatum,
    value: {
      lovelace: OUT_LOVELACE,
      [mintPoolToken.asset]: mintPoolToken.quantity,
      'da00eb76155a782d6b216feccd5c5a69f30c0326239662b85ed88966.4d4143': OUT_MAC, // mac
      'da00eb76155a782d6b216feccd5c5a69f30c0326239662b85ed88966.53524b': OUT_SRK, // srk
    }
  }

  const changeTxOut: TxOut = {
    address: changeAddress,
    value: {
      lovelace: (IN_LOVELACE - OUT_LOVELACE) - FEE,
      [mintLiquidityAsset.asset]: mintLiquidityAsset.quantity,
      'da00eb76155a782d6b216feccd5c5a69f30c0326239662b85ed88966.4d4143':  IN_MAC_TOTAL - OUT_MAC, // mac
      'da00eb76155a782d6b216feccd5c5a69f30c0326239662b85ed88966.53524b': IN_SRK_TOTAL - OUT_SRK, // srk
    }
  }

  // create minting transactions
  const mintLiquidityTokens = buildMint(
    mintLiquidityAsset,
    createLpScript,
    undefined,
    mintRedeemerJSON,
    EXEC_UNITS
  );
  const mintPoolTokens = buildMint(
    mintPoolToken,
    createLpScript,
    undefined,
    mintRedeemerJSON,
    EXEC_UNITS
  );


  const txIn = [factoryTxIn, tokensTxIn]
  const txOut = [factoryTxOut, lpTxOut, changeTxOut]

    // build tx payload for sending to tx builder over http
  const txPayload = buildTx(
    txIn,
    txOut,
    FEE,
    [mintLiquidityTokens, mintPoolTokens], // mint
    changeAddress,
    collateralTxIn
  );

  console.log("Tx Payload", txPayload);

  // finalize tx
  const txId = await finalizeTx(wasm, wallet, txPayload);
  return txId;
};
