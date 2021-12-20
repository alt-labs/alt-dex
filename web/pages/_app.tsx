import "../styles/globals.css";
import type { AppProps } from "next/app";
import Layout from "../components/Layout/Layout";
import { CardanoWasmContextProvider } from "../context/cardano-wasm";
import { NamiWalletContextProvider } from "../context/nami-wallet";

function MyApp({ Component, pageProps }: AppProps) {
  return (
    <CardanoWasmContextProvider>
      <NamiWalletContextProvider>
        <Layout>
          <Component {...pageProps} />
        </Layout>
      </NamiWalletContextProvider>
    </CardanoWasmContextProvider>
  );
}

export default MyApp;
