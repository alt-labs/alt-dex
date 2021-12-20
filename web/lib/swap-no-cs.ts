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
import { scriptObj as createLpScript } from "../plutus/create-lp.plutus";
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

export const swap = async (wasm: WasmT, wallet: Wallet) => {
  // basics
  const walletAddress = await getWalletAddress(wasm, wallet);
  const changeAddress = walletAddress.to_bech32();
  const factoryUtxo = await getFactoryUtxo(wasm);

  const EXEC_UNITS: ExecutionUnits = [820000000, 2400000]
  const FEE = 2_000_000 // 2 ADA fee;
  const IN_LOVELACE = 63_626_474;
  const IN_MAC_TOTAL = 1_000_000;
  const IN_SRK_TOTAL = 1_000_000;
  const OUT_LOVELACE = 2_000_000;
  const OUT_MAC = 100;
  const OUT_SRK = 100;
  const COLLATERAL_LOVELACE = 5_000_000;


  // TODO: Load this from frontend
  const swapAssetFirst = {
    policyId: "d6960f0ffe09bbf83dcfb7c21717b6a559fe4ab0fb5396e6fd35dd74",
    nameHex: "4d4143",
    name: "MAC",
    amount: 100,
    unit: "",
  };
  const swapAssetSecond = {
    policyId: "d6960f0ffe09bbf83dcfb7c21717b6a559fe4ab0fb5396e6fd35dd74",
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


  const factoryTxIn: TxIn = {
    txHash: toHex(factoryUtxo?.input().transaction_id().to_bytes()),
    txId: factoryUtxo?.input().index() || 0,
    value: {
      lovelace: 1689618,
      '2765cfa43a533eb39e68c7181757c0379b88843e0556800fcbcdef76.535750': 1
    },
    script: altswapScript,
    datumJSON: factoryInDatum,
    redeemerJSON: factoryInRedeemer,
    executionUnits: EXEC_UNITS,
  }


  const tokensTxIn: TxIn = {
    txHash: 'fece0e5da9d8b4c1ed4e5831ffc5f04c2e6299083c9c27da271d34772a364367',
    txId: 0,
    value: {
      lovelace: 63626474,
      'd6960f0ffe09bbf83dcfb7c21717b6a559fe4ab0fb5396e6fd35dd74.4d4143': IN_MAC_TOTAL, // mac
      'd6960f0ffe09bbf83dcfb7c21717b6a559fe4ab0fb5396e6fd35dd74.53524b': IN_SRK_TOTAL, // srk
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
      'd6960f0ffe09bbf83dcfb7c21717b6a559fe4ab0fb5396e6fd35dd74.4d4143': OUT_MAC, // mac
      'd6960f0ffe09bbf83dcfb7c21717b6a559fe4ab0fb5396e6fd35dd74.53524b': OUT_SRK, // srk
    }
  }

  const changeTxOut: TxOut = {
    address: changeAddress,
    value: {
      lovelace: (IN_LOVELACE - OUT_LOVELACE) - FEE,
      [mintLiquidityAsset.asset]: mintLiquidityAsset.quantity,
      'd6960f0ffe09bbf83dcfb7c21717b6a559fe4ab0fb5396e6fd35dd74.4d4143':  IN_MAC_TOTAL - OUT_MAC, // mac
      'd6960f0ffe09bbf83dcfb7c21717b6a559fe4ab0fb5396e6fd35dd74.53524b': IN_SRK_TOTAL - OUT_SRK, // srk
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
