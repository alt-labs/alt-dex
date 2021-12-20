import { SwapAsset } from '../lib/tx-builder/types';
import { Option } from '../util/ts-belt/Option';

export type Token = {
  symbol: string;
  img: string;
  name: string;
  ratio: number;
};

export type MaybeTokenWithAmount = {
  tokenOption: Option<Token>;
  amount: number;
};

export type MaybeTokenPair = {
  firstToken: MaybeTokenWithAmount;
  secondToken: MaybeTokenWithAmount;
};

export type TokenWithAmount = {
  token: SwapAsset;
  amount: number;
};

export type TokenPair = {
  firstToken: TokenWithAmount;
  secondToken: TokenWithAmount;
};
