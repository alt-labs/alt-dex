type SwapAsset = {
  nameHex: string;
  policyId: string;
};

/*
 * This redeemer is attached to tx-in that holds factory token.
 */
export const getCreateFactoryInRedeemer = (
  swapFirst: SwapAsset,
  swapSecond: SwapAsset
) => ({
  constructor: 0,
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
                  bytes: swapFirst.policyId,
                },
                {
                  bytes: swapFirst.nameHex,
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
                  bytes: swapSecond.policyId,
                },
                {
                  bytes: swapSecond.nameHex,
                },
              ],
            },
          ],
        },
      ],
    },
  ],
});

export const getVoidRedeemer = () => ({
  constructor: 0,
  fields: [],
});
