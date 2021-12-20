import React, { useContext, createContext, FC, useState, useEffect, useCallback } from 'react';
import type * as WasmNamespace from "@emurgo/cardano-serialization-lib-browser";
import { useCardanoWasmContext } from './cardano-wasm';

type WasmT = typeof WasmNamespace;

enum Network {
  TESTNET = 0,
  MAINNET = 1,
}

export type Wallet = {
  enable: () => Promise<boolean>;
  isEnabled: () => Promise<boolean>;
  getBalance: () => Promise<string>;
  getNetworkId: () => Promise<Network>;
  signTx: (encodedTx: string, partialSign?: boolean) => Promise<string>;
  getUsedAddresses: () => Promise<string[]>;
  getUtxos: () => Promise<string[]>;
  submitTx: (encodedSignedTx: string) => Promise<string>;
  onAccountChange: (_: () => Promise<void>) => Promise<void>;
  signData: (encodedAddress: string, message: string) => Promise<string>;
  getCollateral: () => Promise<string[]>
};

type NamiWalletContext = {
  wallet?: Wallet;
  useWallet: () => Wallet | undefined;
  setWallet: (wallet: Wallet) => void;
  address?: string;
  setAddress: (address: string) => void;
  isEnabled: boolean;
  setIsEnabled: (isEnabled: boolean) => void;
  checkConnection: () => void;
};

declare global {
  interface Window {
    cardano?: Wallet;
  }
}

const decodeAddress =
  (wasm?: WasmT) =>
  (encodedAddress: string): string | undefined =>
    wasm ? wasm.Address.from_bytes(Buffer.from(encodedAddress, "hex")).to_bech32() : undefined;

export const NamiWalletContext = createContext({} as NamiWalletContext);

type UseNamiWalletContext = () => NamiWalletContext;

export const useNamiWalletContext: UseNamiWalletContext = () => useContext(NamiWalletContext);

export const NamiWalletContextProvider: FC = ({ children }) => {
  const [address, setAddress] = useState<string>('');
  const [isEnabled, setIsEnabled] = useState(false);
  const [wallet, setWallet] = useState<Wallet>();
  const { wasm } = useCardanoWasmContext();


  const checkConnection = useCallback(async () => {
    if (window.cardano && (await window.cardano.isEnabled())) {

      const encodedAddress = (await window.cardano.getUsedAddresses())[0];
      const address = decodeAddress(wasm)(encodedAddress);
      setAddress(address || '');
      setIsEnabled(true);
    }
  }, [wasm]);

  useEffect(() => {
    if (isEnabled) {
      window?.cardano?.onAccountChange(async () => {
        const addresses = await window.cardano?.getUsedAddresses();
        const encodedAddress = addresses ? addresses[0] : 'Cant load wallet address';
        const address = decodeAddress(wasm)(encodedAddress);
        setAddress(address || '');
        setIsEnabled(!!address);
      });
    }
  }, [isEnabled, wasm]);

  useEffect(() => {
    checkConnection();
  }, [isEnabled, wallet, wasm, checkConnection]);

  useEffect(() => {
    window.cardano ? setWallet(window.cardano) : setTimeout(() => setWallet(window.cardano), 300);
  }, []);



  const useWallet = () => window.cardano;

  return (
    <NamiWalletContext.Provider
      value={{
        wallet,
        useWallet,
        setWallet,
        address,
        setAddress,
        isEnabled,
        setIsEnabled,
        checkConnection
      }}
    >
      {children}
    </NamiWalletContext.Provider>
  );
};
