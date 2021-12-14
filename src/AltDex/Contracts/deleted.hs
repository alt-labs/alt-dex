-- validatePlutusScript :: Script
-- validatePlutusScript = unMintingPolicyScript $ monetaryPolicy . mkCurrency
mintingScript :: LimitedSupplyCurrency -> Validator
mintingScript = Validator . unMintingPolicyScript . monetaryPolicy

hrabarTxOutRef :: TxOutRef
hrabarTxOutRef = TxOutRef "9e4bf82c37a46964c6171bfc0c5b019e5c9063faf4d52449c1f35c601ae54730" 0

ztnTokenName :: TokenName
ztnTokenName = "ZTN"

zltTokenName :: TokenName
zltTokenName = "ZLT"

dktTokenName :: TokenName
dktTokenName = "DKT"

bonTokenName :: TokenName
bonTokenName = "BON"

-- ScriptDataConstructor 1 [ScriptDataBytes "abc", ScriptDataBytes "def"]
minterCurrency :: LimitedSupplyCurrency
minterCurrency = mkCurrency hrabarTxOutRef tokens
    where
        tokens :: [(TokenName, Integer)]
        tokens = [(ztnTokenName, amt), (zltTokenName, amt), (dktTokenName, amt), (bonTokenName, amt)]

        amt = 1_000_000

validatorAsCBOR :: LB.ByteString
validatorAsCBOR = serialise $ mintingScript minterCurrency

plutusMintingScript :: PlutusScript PlutusScriptV1
plutusMintingScript = PlutusScriptSerialised . SBS.toShort $ LB.toStrict validatorAsCBOR

-- λ> sdata = fromPlutusData $ builtinDataToData (toBuiltinData dat)
-- λ> encode $ scriptDataToJson ScriptDataJsonDetailedSchema sdata
-- which produces something like:
--  "{\"constructor\":0,\"fields\":[{\"bytes\":\"d879909e57d267af861526550a1aa53a50b73d1a910700fb660d2beb\"},{\"bytes\":\"07ec3f8ec650ee1d1b672d24e67180aafe4f4081e73a0683d505e6e3\"},{\"int\":100000},{\"int\":100},{\"int\":100},{\"list\":[]}]}"

mintingScriptShortBs :: SBS.ShortByteString
mintingScriptShortBs = SBS.toShort . LB.toStrict $ validatorAsCBOR

serializedCurrencies:: LimitedSupplyCurrency -> LB.ByteString
serializedCurrencies d = serialise (Datum $ Plutus.toBuiltinData d)

showSerializedCurrencies :: Haskell.IO ()
showSerializedCurrencies = Haskell.print $ Haskell.show $ serializedCurrencies minterCurrency

sdata = fromPlutusData $ Plutus.builtinDataToData (Plutus.toBuiltinData minterCurrency)

showMeTehDatum :: LB.ByteString
showMeTehDatum =  JSON.encode $ scriptDataToJson ScriptDataJsonDetailedSchema sdata

writeScript :: Haskell.IO ()
writeScript = do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData sdata
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m mintingScriptShortBs [pData]
          in do Haskell.print ("Log output" :: Haskell.String) >> Haskell.print logout
                case e of
                  Left evalErr -> Haskell.print ("Eval Error" :: Haskell.String) >> Haskell.print evalErr
                  Right exbudget -> Haskell.print ("Ex Budget" :: Haskell.String) >> Haskell.print exbudget
        Nothing -> Haskell.error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope "minter.plutus" Nothing plutusMintingScript
  case result of
    Left err -> Haskell.print $ displayError err
    Right () -> return ()
