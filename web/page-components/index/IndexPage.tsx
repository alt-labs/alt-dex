import type { NextPage } from 'next';
import Head from 'next/head';
import { CardLink, Grid, Main, Title } from './styled';

const IndexPage: NextPage = () => {
  return (
    <>
      <Head>
        <title>Dex POC</title>
        <meta name="description" content="Generated by create next app" />
        <link rel="icon" href="/favicon.ico" />
      </Head>

      <Main>
        <Title>Welcome to DEX POC!</Title>
        <Grid>
          <CardLink href="/swap">
            <h2>Swap &rarr;</h2>
            <p>Swap tokens for other tokens</p>
          </CardLink>

          <CardLink href="/create-liquidity-pool">
            <h2>Create pool &rarr;</h2>
            <p>Create new liquidity pool</p>
          </CardLink>

          <CardLink href="/add-liquidity">
            <h2>Add liquidity &rarr;</h2>
            <p>Add liquidity to a pool</p>
          </CardLink>
        </Grid>
      </Main>
    </>
  );
};

export default IndexPage;
