import { Dispatch, SetStateAction, useEffect } from 'react';
import { updateObjectField } from '../../util/object-utils';
import { TokenPair, TokenWithAmount } from '../types';

export const MAX_INPUT = 400;

type TokenWithAmountElem<
  K extends keyof TokenWithAmount = keyof TokenWithAmount,
> = TokenWithAmount[K];

export function updateTokenState<K extends keyof TokenWithAmount>(
  tokenKey: keyof TokenPair,
  tokenField: K,
  value: TokenWithAmountElem<K>,
) {
  return updateObjectField<TokenPair, keyof TokenPair>(
    tokenKey,
    (tokenState) => ({
      ...tokenState,
      [tokenField]: value,
    }),
  );
}

export function useUpdateSecondTokenAmount(
  firstToken: TokenWithAmount,
  secondToken: TokenWithAmount,
  setTokens: Dispatch<SetStateAction<TokenPair>>,
) {
  useEffect(() => {
    const amount =
      Math.round(
        ((firstToken.amount * 1.5) /
          1.5 +
          Number.EPSILON) *
          100,
      ) / 100;
    setTokens(updateTokenState('secondToken', 'amount', amount));
  }, [
    setTokens,
    firstToken.amount,
    1.5,
    1.5,
    // firstToken.token.ratio,
    // secondToken.token.ratio,
  ]);
}
