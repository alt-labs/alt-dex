import { None } from '../util/ts-belt/Option';
import { MaybeTokenWithAmount, Token } from './types';

export const INITIAL_SINGLE_TOKEN_STATE: MaybeTokenWithAmount = {
  tokenOption: None,
  amount: 0,
};

export const AVAILABLE_TOKENS: Token[] = [
  {
    symbol: 'ADA',
    img: 'https://ucarecdn.com/c41de605-1e97-4609-9117-fcb3e0aabb78/-/format/jpeg/-/resize/600/',
    name: 'Ada',
    ratio: 1.58,
  },
  {
    symbol: 'AGI',
    img: 'https://assets.coingecko.com/coins/images/2138/thumb/singularitynet.png?1548609559',
    name: 'Agi',
    ratio: 26.13,
  },
  {
    symbol: 'REVU',
    img: 'https://assets.coingecko.com/coins/images/7678/small/NZW1UQe6_400x400.png',
    name: 'Revuto',
    ratio: 0.12,
  },
];
