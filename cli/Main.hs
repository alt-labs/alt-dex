module Main (main) where

import           System.Environment (getArgs, getEnv)
import           Control.Exception (try, handle, handleJust, throw)
import           Control.Monad.Reader
import           Control.Monad.Except
import           Cardano.Api
import           qualified Cardano.Api.Shelley
import           Cardano.CLI.Run (renderClientCommandError, runClientCommand, ClientCommand (ShelleyCommand))
import           Prelude
import           System.IO.Extra (putStrLn)
import           AltDex.CLI.Common
import AltDex.CLI.Error

main :: IO ()
main = do
  showWelcome
  args <- getArgs
  sockEnv <- try $ getEnv "CARDANO_NODE_SOCKET_PATH"
  socketPath <- case sockEnv of
    Left (e :: IOError) -> do
      return $ "Error reading socat: " ++ show e
    Right s -> pure s
  putStrLn $ "[Found socket] = " ++ socketPath
  let conn  = localNodeConnInfo socketPath

  let nargs = length args
  case nargs of
    0 -> showHelp
    _ ->
      case head args of
        "conn:check" -> runConnCheck conn
        "mint" -> putStrLn "MintEm' all"
        _ -> showHelp

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
      SomeError _  ->Nothing
      _            -> Just "Node error or something"


localNodeConnInfo :: FilePath -> LocalNodeConnectInfo CardanoMode
localNodeConnInfo = LocalNodeConnectInfo (CardanoModeParams (EpochSlots 21600))  (Testnet  (NetworkMagic 1097911063))