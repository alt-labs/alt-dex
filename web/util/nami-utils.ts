import { Token } from "../page-components/types";
import type * as WasmNamespace from "@emurgo/cardano-serialization-lib-browser";
type WasmT = typeof WasmNamespace;

export const fromHex = (encodedString: string) => Buffer.from(encodedString, 'hex');

export const decodeBalance = (wasm: WasmT, encodedBalance: string): Token[] => {

  const balanceBytes = fromHex(encodedBalance);
  const balance = wasm.Value.from_bytes(balanceBytes);

  return []
}
