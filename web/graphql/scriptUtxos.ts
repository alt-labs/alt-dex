import { gql } from "@apollo/client";

const UTXOS = gql`
  query GetUtxos($address: String!) {
    utxos(where: { address: { _eq: $address } }) {
      address
      addressHasScript
      index
      txHash
      value
      tokens {
        quantity
        asset {
          assetId
          policyId
          assetName
        }
      }
      transaction {
        inputs {
          address
        }
      }
    }
  }
`;

export const getScriptUtxos = {
  name: "utxos",
  document: UTXOS,
};
