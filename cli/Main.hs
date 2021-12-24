{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (getArgs, getEnv)
import Control.Exception (try, handle, handleJust, throw)
import Control.Monad.Reader
import Control.Monad.Except
import Cardano.Api
    ( getLocalChainTip,
      queryNodeLocalState,
      ShelleyBasedEra(ShelleyBasedEraAlonzo),
      LocalNodeConnectInfo,
      CardanoMode,
      EraInMode(AlonzoEraInCardanoMode),
      QueryInEra(QueryInShelleyBasedEra),
      QueryInMode(QueryInEra),
      QueryInShelleyBasedEra(QueryProtocolParameters) )
import qualified Cardano.Api.Shelley
import Cardano.CLI.Run (renderClientCommandError, runClientCommand, ClientCommand (ShelleyCommand))
import Prelude
import System.IO.Extra (putStrLn)
import System.IO
import System.Exit
import System.Console.GetOpt
import Data.List (nub)
import Data.List.Split (splitOn)

-- import Control.Monad
-- import Data.Char
import qualified Data.Set as Set
-- import System.Console.GetOpt
-- import System.Environment
-- import System.Exit
-- import System.IO
-- import Text.Printf
-- import Control.Monad (set)

import Data.Csv
import Data.Vector (Vector, toList)

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as DBI

import AltDex.CLI.Common
import AltDex.CLI.Environment
import AltDex.CLI.Error
import AltDex.CLI.Node

import Plutus.V1.Ledger.Value (TokenName (..))
import Plutus.V1.Ledger.Tx (TxOutRef (..))
-- import Plutus.V1.Ledger.TxId (TxId)

import           AltDex.Contracts.Serialise hiding (main)
import qualified AltDex.Contracts.Monetary as Cash
import           AltDex.Contracts.Monetary (FiniteCurrency)

import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString), encodeUtf8)

import qualified PlutusTx
import Data.String
import qualified PlutusTx.Builtins as BI
import qualified Data.Text.Encoding as Data.String
import Data.Text.Encoding
import qualified Data.Text as DT
import qualified Data.Text as TE
import qualified Plutus.V1.Ledger.Api           as Plutus
import Playground.Contract (TxOutRef(TxOutRef))
import qualified Data.ByteString.Char8 as DBC
import qualified Plutus.V1.Ledger.Api as Plut
import qualified PlutusTx.Builtins.Class
import qualified Plutus.V1.Ledger.Api as Plut

-- parse :: [String] -> m (a, [String])
parse argv = case getOpt Permute flags argv of
    (args, fs, []) -> do
        let files = if null fs
                    then ["-"]
                    else fs
        if Help `elem` args
            then do hPutStrLn stderr (usageInfo header flags)
                    exitSuccess
            else return (nub (concatMap (: []) args), files)

    (_,_,errs)      -> do
        hPutStrLn stderr (concat errs ++ usageInfo header flags)
        exitWith (ExitFailure 1)

    where header = "Usage: aldex <command> [-o] [file ...]"

data Flag
        = Out                -- -o
        | Help               -- --help
        deriving (Eq,Ord,Enum,Show,Bounded)

flags :: [OptDescr Flag]
flags =
    [Option ['o'] []       (NoArg Out)
        "Specify output to a file."
    ,Option []    ["help"] (NoArg Help)
        "Print this help message"
    ]

main :: IO ()
main = do
  (opts, args) <- getArgs >>= parse
  putStrLn $ "Flags: " ++ show opts
  putStrLn $ "Files: " ++ show args
  showWelcome
  -- args <- getArgs
  sockEnv <- try $ getEnv "CARDANO_NODE_SOCKET_PATH"
  socketPath <- case sockEnv of
    Left (e :: IOError) -> do
      return $ "Error reading socat: " ++ show e
    Right s -> pure s
  putStrLn $ "[Found socket] = " ++ socketPath
  let conn  = testnetLocalNodeConnInfo socketPath
  cardanoEnvironment <- getTestnetEnvironmment 1097911063
  let firstArg = head . tail $ args

  let nargs = length args
  case nargs of
    0 -> showHelp
    _ ->
      case head args of
        "conn:check"    -> runConnCheck conn
        "sync:status"   -> showSyncStatus cardanoEnvironment
        "gens:mint"     -> do
          putStrLn $ "TxOutRef: " ++ show (parseTxOutRef firstArg)
          runExportMintingScript (parseTxOutRef firstArg) "altswap-tokens.plutus"
        "gens:factory"  -> do
            putStrLn "Generating AltSwap factory"
            runExportAltswapFactoryScripts (parseTxOutRef firstArg) "altswap-nft.plutus" "altswap.plutus"
        "mint"          -> putStrLn "MintEm' all"
        _               -> showHelp

parseTxOutRef :: String -> Plut.TxOutRef
parseTxOutRef s = TxOutRef (txInHash s) (txInIdx s)
  where
    splitTxIn :: String -> [String]
    splitTxIn = splitOn "#"

    txInHash :: String -> Plut.TxId
    txInHash str = fromString (head . splitTxIn $ str) :: Plut.TxId

    txInIdx :: String -> Integer
    txInIdx = read . head . tail . splitTxIn

showHelp :: IO ()
showHelp = do
    args <- getArgs
    putStrLn $ "Unknown options " ++ show args

showWelcome :: IO ()
showWelcome = do
  printLn "             /\\   |  _|_    |    _.  |_    _ "
  printLn "DEX PoC by  /--\\  |   |_    |_  (_|  |_)  _> "
  printLn "                                              "

runConnCheck :: LocalNodeConnectInfo CardanoMode -> IO ()
runConnCheck conn = do
  handleJust catcher handler ( getLocalChainTip conn >>= Prelude.print )
  v<-queryNodeLocalState conn Nothing protocolParamsQuery

  pParam<-case v of
    Left af -> throw $ SomeError  "Acquire Failure"
    Right e -> case e of
      Left em -> throw $ SomeError "Missmatched Era"
      Right pp -> return pp

  Prelude.print pParam

  where
    protocolParamsQuery= QueryInEra AlonzoEraInCardanoMode
                    $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo QueryProtocolParameters
    handler e = putStrLn e
    catcher e = case e of
      SomeError _  -> Nothing
      _            -> Just "Node error or something"


toBS' :: String -> BI.BuiltinByteString
toBS' = PlutusTx.Builtins.Class.stringToBuiltinByteString

-- main' :: IO ()
-- main' = do
--   Prelude.print $ toBS' "86616b7707fc9e08ff76e54b5e728933f59c5f16b826174f45bdabbc02ad0de9"

toBS :: String -> BI.BuiltinByteString
toBS =
    BI.unsafeDataAsB
    . PlutusTx.dataToBuiltinData
    . PlutusTx.B
    . Data.String.encodeUtf8
    . TE.pack

fromBS :: BI.BuiltinByteString -> String
fromBS = TE.unpack .  Plutus.fromBuiltin . BI.decodeUtf8

parseTokenName ::  DBI.ByteString -> Parser BI.BuiltinByteString
parseTokenName s = pure $ toBS $ show s

instance FromField TokenName where
    parseField tn = TokenName <$> parseTokenName tn

instance DefaultOrdered TokenName

readMintingTokens :: TxOutRef -> IO (Either String FiniteCurrency)
readMintingTokens minterTxOutRef = do
  s <- L.readFile "sample/tokens.csv"

  case decodedTokens s of
    (Left err)       ->
      pure $ Left $ "ERROR: " ++ err
    (Right tokens) -> do
      let currency = cur (toList tokens)
      pure $ Right currency

  where
    cur :: [(TokenName, Integer)] -> FiniteCurrency
    cur = Cash.mkCurrency minterTxOutRef

    decodedTokens :: L.ByteString -> Either String (Vector (TokenName, Integer))
    decodedTokens s = decode NoHeader s

runExportMintingScript :: TxOutRef -> String -> IO ()
runExportMintingScript minterTxOutRef outFilepath = do
  decodedTokens <- readMintingTokens minterTxOutRef
  case decodedTokens of
    (Left s)       -> putStr $ "ERROR: " ++ s
    (Right currency) -> do
      -- let currency = cur (toList tokens)
      -- Prelude.print currency
      writeTokensMintingScript' currency outFilepath

runExportAltswapFactoryScripts :: TxOutRef -> String -> String -> IO ()
runExportAltswapFactoryScripts swapFactoryTxOutRef nftPolicyOutFilepath swapScriptOutFilepath = do

  putStrLn "Hi mate!"

  where
    cur :: FiniteCurrency
    cur = Cash.mkCurrency swapFactoryTxOutRef [(TokenName "SWP", 1)]

