import ApolloClient from 'apollo-client';
import { HttpLink } from 'apollo-link-http';
import { DocumentNode } from 'apollo-link';
import { InMemoryCache } from 'apollo-cache-inmemory';

type Query = <QV, RT>(name: string, query: DocumentNode, variables?: QV, isMultiple?: boolean) => Promise<RT>;

type Mutate = <MV, RT>(name: string, mutation: DocumentNode, variables?: MV) => Promise<RT>;

const link = new HttpLink({
  uri: `https://explorer-testnet.cardano-blue.com`,
  headers: {
    'content-type': 'application/json',
  },
});

export const client = new ApolloClient({
  link,
  cache: new InMemoryCache({
    addTypename: false,
    resultCaching: false,
  }),
  defaultOptions: {
    watchQuery: {
      fetchPolicy: 'no-cache',
    },
    mutate: {
      fetchPolicy: 'no-cache',
    },
  },
});

export const query: Query = (name, query, variables, isMultiple = false) => {
  return client
    .query({
      query,
      variables,
      fetchPolicy: 'no-cache',
    })
    .then(({ data }) => (isMultiple ? data : data[name]));
};

export const mutate: Mutate = (name, mutation, variables) => {
  return client
    .mutate({
      mutation,
      variables,
    })
    .then(({ data }) => data[name]);
};
