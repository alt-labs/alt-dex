import axios from "axios";
import { TransactionPayload } from "./types";

type SendTransactionRequest = (
  txRequestPayload: TransactionPayload
) => Promise<any>

type HashDataRequest = (
  dataPayload: any
) => Promise<any>

// const BASE = "https://builder-testnet.cblu.io"
const BASE = "http://localhost:4001"

const sendTransactionRequest: SendTransactionRequest = (txRequestPayload) =>
  axios.post(`${BASE}/builder`, txRequestPayload);


const hashData: HashDataRequest = (dataPayload) =>
  axios.post(`${BASE}/hash`, dataPayload);


const client = {
  sendTransactionRequest,
  hashData,
};

export default client;
