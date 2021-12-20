import type * as CardanoWasmT from '@emurgo/cardano-serialization-lib-browser';
import { createContext, FC, Fragment, useContext, useState } from 'react';
import dynamic from 'next/dynamic';

type CardanoWasmContext = {
  wasm?: typeof CardanoWasmT;
};
const CardanoWasmContext = createContext({} as CardanoWasmContext);

export const useCardanoWasmContext = () => useContext(CardanoWasmContext);
export const CardanoWasmContextProvider: FC = ({ children }) => {
  const [wasm, setWasm] = useState<typeof CardanoWasmT>();
  const CardanoWasmLoader = dynamic(
    {
      loader: async () => {
        setWasm(await import('@emurgo/cardano-serialization-lib-browser'));

        return Fragment;
      },
    },
    { ssr: false },
  );

  return (
    <CardanoWasmContext.Provider value={{ wasm }}>
      <CardanoWasmLoader />
      {children}
    </CardanoWasmContext.Provider>
  );
};
