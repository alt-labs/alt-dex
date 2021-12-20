import { SwapAsset } from "./tx-builder/types";

/*
 * This datum is attached to tx-in that holds factory token.
 */
export const getFactoryInDatum = () => ({
  constructor: 0,
  fields: [
    {
      list: [],
    },
  ],
});

/*
 * This datum is attached to tx-out that holds factory token.
 */
export const getFactoryOutDatum = (first: SwapAsset, second: SwapAsset) => ({
  constructor: 0,
  fields: [
    {
      list: [
        {
          constructor: 0,
          fields: [
            {
              constructor: 0,
              fields: [
                {
                  constructor: 0,
                  fields: [
                    {
                      bytes: first.policyId,
                    },
                    {
                      bytes: first.nameHex,
                    },
                  ],
                },
              ],
            },
            {
              constructor: 0,
              fields: [
                {
                  constructor: 0,
                  fields: [
                    {
                      bytes: second.policyId,
                    },
                    {
                      bytes: second.nameHex,
                    },
                  ],
                },
              ],
            },
          ],
        },
      ],
    },
  ],
});

/*
 * This datum is attached to tx-out that holds liquidity pool tokens.
 */
export const getCreateLPOutDatum = (
  liqAsset: SwapAsset,
  first: SwapAsset,
  second: SwapAsset
) => ({
  constructor: 1,
  fields: [
    {
      constructor: 0,
      fields: [
        {
          constructor: 0,
          fields: [
            {
              constructor: 0,
              fields: [
                {
                  bytes: first.policyId,
                },
                {
                  bytes: first.nameHex,
                },
              ],
            },
          ],
        },
        {
          constructor: 0,
          fields: [
            {
              constructor: 0,
              fields: [
                {
                  bytes: second.policyId,
                },
                {
                  bytes: second.nameHex,
                },
              ],
            },
          ],
        },
      ],
    },
    {
      constructor: 0,
      fields: [
        {
          int: liqAsset.amount,
        },
      ],
    },
  ],
});

export const getVoidDatum = () => ({
  constructor: 0,
  fields: [],
});
