// Next.js API route support: https://nextjs.org/docs/api-routes/introduction
import type { NextApiRequest, NextApiResponse } from "next";
import { query } from "../../graphql/client";
import { getScriptUtxos } from "../../graphql/scriptUtxos";
import { GqlUtxo, SwapAsset } from "../../lib/tx-builder/types";
import { fromHex } from "../../lib/utils";
import { address } from "../../plutus/altswap.plutus";
import { poolStateToken } from "../../plutus/create-lp.plutus";

type Pool = {
  name: string;
  first: SwapAsset;
  second: SwapAsset;
  ratio: number;
  utxo: GqlUtxo;
};

type Data = {
  pools: Pool[];
};

const convertPoolUtxoToPool = (poolUtxo: GqlUtxo): Pool => {
  const { tokens, value } = poolUtxo;

  const filteredTokens = tokens.filter(
    (token) =>
      token.asset.assetName !== poolStateToken.nameHex &&
      token.asset.policyId !== poolStateToken.policyId
  );

  const isAdaPool = filteredTokens.length < 2;


  const tokenNameFirst = isAdaPool ? 'ADA' : fromHex(filteredTokens[0].asset.assetName)
  const tokenNameSecond = isAdaPool ?  fromHex(filteredTokens[0].asset.assetName) : fromHex(filteredTokens[1].asset.assetName)
  const amountFirst = isAdaPool ? Number.parseInt(value) : Number.parseInt(filteredTokens[0].quantity)
  const amountSecond =  isAdaPool ? Number.parseInt(filteredTokens[0].quantity) : Number.parseInt(filteredTokens[1].quantity)


  const pool = {
    name: `${tokenNameFirst}/${tokenNameSecond}`,
    first: {
      unit: '',
      name: tokenNameFirst.toString(),
      amount: amountFirst,
    },
    second: {
      unit: '',
      name: tokenNameSecond.toString(),
      amount: amountSecond,
    },
    utxo: poolUtxo,
    ratio: amountFirst / amountSecond
  };

  return pool;
};

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse<Data>
) {
  const utxos: GqlUtxo[] = await query(
    getScriptUtxos.name,
    getScriptUtxos.document,
    {
      address: address,
    }
  );

  // exclude factory tx hash and that buggy one
  const excludedTxHash = [
    "6ce86b5d7d077c11d9779e52a09a130ebd771f3d24bc06252227f61eabab0c96#0", // factory
    "6ce86b5d7d077c11d9779e52a09a130ebd771f3d24bc06252227f61eabab0c96#2", // bug
  ];

  const poolUtxos = utxos.filter(
    (utxo) => !excludedTxHash.includes(`${utxo.txHash}#${utxo.index}`)
  );

  const pools = poolUtxos.map(convertPoolUtxoToPool);

  // poolUtxos.reduce((pools, poolUtxo) => {

  //   poolUtxo.tokens.

  // }, {})

  res.status(200).json({ pools });
}
