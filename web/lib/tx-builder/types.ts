type GqlAsset = {
  policyId: string;
  assetId: string;
  assetName: string;
};

export type GqlUtxo = {
  address: string;
  addressHasScript: boolean;
  index: number;
  txHash: string;
  value: string;
  tokens: {
    quantity: string;
    asset: GqlAsset;
  }[];
  transaction: {
    inputs: {
      address: string;
    }[];
  };
};

export type Address = string;

export type SerializedOutput = {
  type: string;
  description: string;
  cborHex: string;
};

export type PlutusScript = SerializedOutput;

export type Value = {
  [assetName: string]: number;
};

export interface Utxo {
  txHash: string;
  txId: number;
  value: Value;
}

export enum Network {
  TESTNET = 0,
  MAINNET = 1,
}

export type StructuredJSON = {
  constructor: number;
  fields: {
    [type: string]: any;
  }[];
};

export type Datum = StructuredJSON | [] | any;
export type Redeemer = StructuredJSON | [] | any;
export type ExecutionUnits = [number, number];

export interface TxIn extends Utxo {
  script?: PlutusScript; // cbor hash
  datum?: Datum;
  redeemer?: Redeemer;
  datumJSON?: Datum;
  redeemerJSON?: Redeemer;
  executionUnits?: ExecutionUnits;
}

export type SwapAsset = {
  amount: number;
  unit: string;
  name: string;
  policyId?: string;
  nameHex?: string;
  ratio?: number;
};

export type Asset = {
  asset: string;
  quantity: number;
};

type DatumHash = string;

export type TxOut = {
  address: Address;
  value: Value;
  datumHash?: DatumHash;
  datumEmbedFile?: Datum;
  datumEmbedJSON?: Datum;
};

type MintAction = "burn" | "mint";
type MintAsset = string;
type Quantity = number;

export type MintScript = PlutusScript | JSON;

export type Mint = {
  action: MintAction;
  quantity: Quantity;
  asset: MintAsset;
  script?: MintScript;
  redeemer?: Redeemer;
  redeemerJSON?: Redeemer;
  executionUnits?: ExecutionUnits;
};

export type TransactionPayload = {
  txIn: TxIn[];
  txOut: TxOut[];
  fee?: number;
  mint?: Mint[];
  changeAddress?: Address;
  txInCollateral?: Utxo[];
  witnessCount?: number;
};

export type Opts = {
  dev: boolean;
};

export type Transaction = SerializedOutput;
