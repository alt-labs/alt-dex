{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module AltDex.Contracts.OffChain
    ( poolStateCoinFromAltSwapCurrency, liquidityCoin
    , CreateParams (..)
    , SwapParams (..)
    , CloseParams (..)
    , RemoveParams (..)
    , AddParams (..)
    , AltSwapUserSchema, UserContractState (..)
    , AltSwapOwnerSchema
    , start, create, add, remove, close, swap, pools
    , ownerEndpoint, aswperEndpoints
    ) where

import           Control.Lens                     (view)
import           Control.Monad                    hiding (fmap)
import qualified Data.Map                         as Map
import           Data.Monoid                      (Last (..))
import           Data.Proxy                       (Proxy (..))
import           Data.Text                        (Text, pack)
import           Data.Void                        (Void, absurd)
import           Ledger                           hiding (singleton)
import           Ledger.Constraints               as Constraints
import qualified Ledger.Typed.Scripts             as Scripts
import           Playground.Contract
import           Plutus.Contract
import qualified AltDex.Contracts.Monetary        as Monetary
import           AltDex.Contracts.OnChain (mkAltSwapValidator, validateLiquidityMinting)
import           AltDex.Contracts.LiquidityPool
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..), dropWhile, flip, unless)
import           Prelude                          as Haskell (Int, Semigroup (..), String, div, dropWhile, flip, show,
                                                              (^))
import           Text.Printf                      (printf)

data AltXchange
instance Scripts.ValidatorTypes AltSwap where
    type instance RedeemerType AltSwap = AltSwapAction
    type instance DatumType    AltSwap = AltSwapDatum

type AltSwapOwnerSchema = Endpoint "start" ()
type AltSwapUserSchema =
        Endpoint "create" CreateParams
        .\/ Endpoint "swap"   SwapParams
        .\/ Endpoint "close"  CloseParams
        .\/ Endpoint "remove" RemoveParams
        .\/ Endpoint "add"    AddParams
        .\/ Endpoint "pools"  ()
        .\/ Endpoint "funds"  ()
        .\/ Endpoint "stop"   ()

data UserContractState =
      Pools [((Coin A, Amount A), (Coin B, Amount B))]
    | Funds Value
    | Created
    | Swapped
    | Added
    | Removed
    | Closed
    | Stopped
    deriving (Show, Generic, FromJSON, ToJSON)

aswpTokenName, poolStateTokenName :: TokenName
aswpTokenName = "AltSwap"
poolStateTokenName = "LP_S_TOK"

aswpInstance :: AltSwap -> Scripts.TypedValidator AltXchange
aswpInstance aswp = Scripts.mkTypedValidator @AltXchange
    ($$(PlutusTx.compile [|| mkAltSwapValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode aswp
        `PlutusTx.applyCode` PlutusTx.liftCode c)
     $$(PlutusTx.compile [|| wrap ||])
  where
    c :: Coin PoolState
    c = poolStateCoin aswp

    wrap = Scripts.wrapValidator @AltSwapDatum @AltSwapAction

aswpScript :: AltSwap -> Validator
aswpScript = Scripts.validatorScript . aswpInstance

aswpAddress :: AltSwap -> Ledger.Address
aswpAddress = Ledger.scriptAddress . aswpScript

aswp :: CurrencySymbol -> AltSwap
aswp cs = AltSwap $ mkCoin cs aswpTokenName

liquidityPolicy :: AltSwap -> MintingPolicy
liquidityPolicy aswp = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \u t -> Scripts.wrapMintingPolicy (validateLiquidityMinting u t) ||])
        `PlutusTx.applyCode` PlutusTx.liftCode aswp
        `PlutusTx.applyCode` PlutusTx.liftCode poolStateTokenName

liquidityCurrency :: AltSwap -> CurrencySymbol
liquidityCurrency = scriptCurrencySymbol . liquidityPolicy

-- | Gets the liquidity token for a given liquidity pool.
liquidityCoin :: CurrencySymbol -- ^ The currency identifying the AltSwap instance.
              -> Coin A         -- ^ One coin in the liquidity pair.
              -> Coin B         -- ^ The other coin in the liquidity pair.
              -> Coin Liquidity
liquidityCoin cs coinA coinB = mkCoin (liquidityCurrency $ aswp cs) $ lpTicker $ LiquidityPool coinA coinB

-- | Parameters for the @create@-endpoint, which creates a new liquidity pool.
data CreateParams = CreateParams
    { cpCoinA   :: Coin A   -- ^ One 'Coin' of the liquidity pair.
    , cpCoinB   :: Coin B   -- ^ The other 'Coin'.
    , cpAmountA :: Amount A -- ^ Amount of liquidity for the first 'Coin'.
    , cpAmountB :: Amount B -- ^ Amount of liquidity for the second 'Coin'.
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)
-- | Parameters for the @swap@-endpoint, which allows swaps between the two different coins in a liquidity pool.
-- One of the provided amounts must be positive, the other must be zero.
data SwapParams = SwapParams
    { spCoinA   :: Coin A         -- ^ One 'Coin' of the liquidity pair.
    , spCoinB   :: Coin B         -- ^ The other 'Coin'.
    , spAmountA :: Amount A       -- ^ The amount the first 'Coin' that should be swapped.
    , spAmountB :: Amount B       -- ^ The amount of the second 'Coin' that should be swapped.
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)
-- | Parameters for the @close@-endpoint, which closes a liquidity pool.
data CloseParams = CloseParams
    { clpCoinA :: Coin A         -- ^ One 'Coin' of the liquidity pair.
    , clpCoinB :: Coin B         -- ^ The other 'Coin' of the liquidity pair.
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)
-- | Parameters for the @remove@-endpoint, which removes some liquidity from a liquidity pool.
data RemoveParams = RemoveParams
    { rpCoinA :: Coin A           -- ^ One 'Coin' of the liquidity pair.
    , rpCoinB :: Coin B           -- ^ The other 'Coin' of the liquidity pair.
    , rpDiff  :: Amount Liquidity -- ^ The amount of liquidity tokens to burn in exchange for liquidity from the pool.
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)
-- | Parameters for the @add@-endpoint, which adds liquidity to a liquidity pool in exchange for liquidity tokens.
data AddParams = AddParams
    { apCoinA   :: Coin A         -- ^ One 'Coin' of the liquidity pair.
    , apCoinB   :: Coin B         -- ^ The other 'Coin' of the liquidity pair.
    , apAmountA :: Amount A       -- ^ The amount of coins of the first kind to add to the pool.
    , apAmountB :: Amount B       -- ^ The amount of coins of the second kind to add to the pool.
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Creates a AltSwap "factory". This factory will keep track of the existing liquidity pools and enforce that there will be at most one liquidity pool
-- for any pair of tokens at any given time.
start :: forall w s. Contract w s Text AltSwap
start = do
    pkh <- pubKeyHash <$> ownPubKey
    cs  <- fmap Monetary.currencySymbol $
           mapError (pack . show @Monetary.CurrencyError) $
           Monetary.mintContract pkh [(aswpTokenName, 1)]
    let c    = mkCoin cs aswpTokenName
        aswp   = aswp cs
        inst = aswpInstance aswp
        tx   = mustPayToTheScript (Factory []) $ unitValue c
    ledgerTx <- submitTxConstraints inst tx
    void $ awaitTxConfirmed $ txId ledgerTx
    void $ waitNSlots 1

    logInfo @String $ printf "started AltSwap %s at address %s" (show aswp) (show $ aswpAddress aswp)
    return aswp

-- | Creates a liquidity pool for a pair of coins. The creator provides liquidity for both coins and gets liquidity tokens in return.
create :: forall w s. AltSwap -> CreateParams -> Contract w s Text ()
create aswp CreateParams{..} = do
    when (unCoin cpCoinA == swpCoin cpCoinB) $ throwError "coins must be different"
    when (cpAmountA <= 0 || cpAmountB <= 0) $ throwError "amounts must be positive"
    (oref, o, lps) <- findAltSwapFactory aswp
    let liquidity = calculateInitialLiquidity cpAmountA cpAmountB
        lp        = LiquidityPool {lpCoinA = cpCoinA, lpCoinB = cpCoinB}
    let aswpInst   = aswpInstance aswp
        aswpScript = aswpScript aswp
        aswpDat1   = Factory $ lp : lps
        aswpDat2   = Pool lp liquidity
        psC      = poolStateCoin aswp
        lC       = mkCoin (liquidityCurrency aswp) $ lpTicker lp
        aswpVal    = unitValue $ swpCoin aswp
        lpVal    = valueOf cpCoinA cpAmountA <> valueOf cpCoinB cpAmountB <> unitValue psC

        lookups  = Constraints.typedValidatorLookups aswpInst        <>
                   Constraints.otherScript aswpScript                <>
                   Constraints.mintingPolicy (liquidityPolicy aswp) <>
                   Constraints.unspentOutputs (Map.singleton oref o)

        tx       = Constraints.mustPayToTheScript aswpDat1 aswpVal                                     <>
                   Constraints.mustPayToTheScript aswpDat2 lpVal                                     <>
                   Constraints.mustMintValue (unitValue psC <> valueOf lC liquidity)              <>
                   Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Create lp)

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo $ "created liquidity pool: " ++ show lp

-- | Closes a liquidity pool by burning all remaining liquidity tokens in exchange for all liquidity remaining in the pool.
close :: forall w s. AltSwap -> CloseParams -> Contract w s Text ()
close aswp CloseParams{..} = do
    ((oref1, o1, lps), (oref2, o2, lp, liquidity)) <- findAltSwapFactoryAndPool aswp clpCoinA clpCoinB
    pkh                                            <- pubKeyHash <$> ownPubKey
    let aswpInst   = aswpInstance aswp
        aswpScript = aswpScript aswp
        aswpDat    = Factory $ filter (/= lp) lps
        aswpC      = swpCoin aswp
        psC      = poolStateCoin aswp
        lC       = mkCoin (liquidityCurrency aswp) $ lpTicker lp
        aswpVal    = unitValue aswpC
        psVal    = unitValue psC
        lVal     = valueOf lC liquidity
        redeemer = Redeemer $ PlutusTx.toBuiltinData Close

        lookups  = Constraints.typedValidatorLookups aswpInst        <>
                   Constraints.otherScript aswpScript                <>
                   Constraints.mintingPolicy (liquidityPolicy aswp) <>
                   Constraints.ownPubKeyHash pkh                   <>
                   Constraints.unspentOutputs (Map.singleton oref1 o1 <> Map.singleton oref2 o2)

        tx       = Constraints.mustPayToTheScript aswpDat aswpVal          <>
                   Constraints.mustMintValue (negate $ psVal <> lVal) <>
                   Constraints.mustSpendScriptOutput oref1 redeemer    <>
                   Constraints.mustSpendScriptOutput oref2 redeemer    <>
                   Constraints.mustIncludeDatum (Datum $ PlutusTx.toBuiltinData $ Pool lp liquidity)

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo $ "closed liquidity pool: " ++ show lp

-- | Removes some liquidity from a liquidity pool in exchange for liquidity tokens.
remove :: forall w s. AltSwap -> RemoveParams -> Contract w s Text ()
remove aswp RemoveParams{..} = do
    (_, (oref, o, lp, liquidity)) <- findAltSwapFactoryAndPool aswp rpCoinA rpCoinB
    pkh                           <- pubKeyHash <$> ownPubKey
    when (rpDiff < 1 || rpDiff >= liquidity) $ throwError "removed liquidity must be positive and less than total liquidity"
    let aswpInst       = aswpInstance aswp
        aswpScript     = aswpScript aswp
        dat          = Pool lp $ liquidity - rpDiff
        psC          = poolStateCoin aswp
        lC           = mkCoin (liquidityCurrency aswp) $ lpTicker lp
        psVal        = unitValue psC
        lVal         = valueOf lC rpDiff
        inVal        = view ciTxOutValue o
        inA          = amountOf inVal rpCoinA
        inB          = amountOf inVal rpCoinB
        (outA, outB) = calculateRemoval inA inB liquidity rpDiff
        val          = psVal <> valueOf rpCoinA outA <> valueOf rpCoinB outB
        redeemer     = Redeemer $ PlutusTx.toBuiltinData Remove

        lookups  = Constraints.typedValidatorLookups aswpInst          <>
                   Constraints.otherScript aswpScript                  <>
                   Constraints.mintingPolicy (liquidityPolicy aswp)   <>
                   Constraints.unspentOutputs (Map.singleton oref o) <>
                   Constraints.ownPubKeyHash pkh

        tx       = Constraints.mustPayToTheScript dat val          <>
                   Constraints.mustMintValue (negate lVal)        <>
                   Constraints.mustSpendScriptOutput oref redeemer

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo $ "removed liquidity from pool: " ++ show lp

-- | Adds some liquidity to an existing liquidity pool in exchange for newly minted liquidity tokens.
add :: forall w s. AltSwap -> AddParams -> Contract w s Text ()
add aswp AddParams{..} = do
    pkh                           <- pubKeyHash <$> ownPubKey
    (_, (oref, o, lp, liquidity)) <- findAltSwapFactoryAndPool aswp apCoinA apCoinB
    when (apAmountA < 0 || apAmountB < 0) $ throwError "amounts must not be negative"
    let outVal = view ciTxOutValue o
        oldA   = amountOf outVal apCoinA
        oldB   = amountOf outVal apCoinB
        newA   = oldA + apAmountA
        newB   = oldB + apAmountB
        delL   = calculateAdditionalLiquidity oldA oldB liquidity apAmountA apAmountB
        inVal  = valueOf apCoinA apAmountA <> valueOf apCoinB apAmountB
    when (delL <= 0) $ throwError "insufficient liquidity"
    logInfo @String $ printf "oldA = %d, oldB = %d, newA = %d, newB = %d, delL = %d" oldA oldB newA newB delL

    let aswpInst       = aswpInstance aswp
        aswpScript     = aswpScript aswp
        dat          = Pool lp $ liquidity + delL
        psC          = poolStateCoin aswp
        lC           = mkCoin (liquidityCurrency aswp) $ lpTicker lp
        psVal        = unitValue psC
        lVal         = valueOf lC delL
        val          = psVal <> valueOf apCoinA newA <> valueOf apCoinB newB
        redeemer     = Redeemer $ PlutusTx.toBuiltinData Add

        lookups  = Constraints.typedValidatorLookups aswpInst             <>
                   Constraints.otherScript aswpScript                     <>
                   Constraints.mintingPolicy (liquidityPolicy aswp)       <>
                   Constraints.ownPubKeyHash pkh                        <>
                   Constraints.unspentOutputs (Map.singleton oref o)

        tx       = Constraints.mustPayToTheScript dat val          <>
                   Constraints.mustMintValue lVal                  <>
                   Constraints.mustSpendScriptOutput oref redeemer

    logInfo @String $ printf "val = %s, inVal = %s" (show val) (show inVal)
    logInfo $ show lookups
    logInfo $ show tx

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo $ "added liquidity to pool: " ++ show lp

-- | Uses a liquidity pool two swap one sort of coins in the pool against the other.
swap :: forall w s. AltSwap -> SwapParams -> Contract w s Text ()
swap aswp SwapParams{..} = do
    unless (spAmountA > 0 && spAmountB == 0 || spAmountA == 0 && spAmountB > 0) $ throwError "exactly one amount must be positive"
    (_, (oref, o, lp, liquidity)) <- findAltSwapFactoryAndPool aswp spCoinA spCoinB
    let outVal = view ciTxOutValue o
    let oldA = amountOf outVal spCoinA
        oldB = amountOf outVal spCoinB
    (newA, newB) <- if spAmountA > 0 then do
        let outB = Amount $ findSwapA oldA oldB spAmountA
        when (outB == 0) $ throwError "no payout"
        return (oldA + spAmountA, oldB - outB)
                                     else do
        let outA = Amount $ findSwapB oldA oldB spAmountB
        when (outA == 0) $ throwError "no payout"
        return (oldA - outA, oldB + spAmountB)
    pkh <- pubKeyHash <$> ownPubKey

    logInfo @String $ printf "oldA = %d, oldB = %d, old product = %d, newA = %d, newB = %d, new product = %d" oldA oldB (unAmount oldA * aswpAmount oldB) newA newB (unAmount newA * aswpAmount newB)

    let inst    = aswpInstance aswp
        val     = valueOf spCoinA newA <> valueOf spCoinB newB <> unitValue (poolStateCoin aswp)

        lookups = Constraints.typedValidatorLookups inst                 <>
                  Constraints.otherScript (Scripts.validatorScript inst) <>
                  Constraints.unspentOutputs (Map.singleton oref o)      <>
                  Constraints.ownPubKeyHash pkh

        tx      = mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Swap) <>
                  Constraints.mustPayToTheScript (Pool lp liquidity) val

    logInfo $ show tx
    ledgerTx <- submitTxConstraintsWith lookups tx
    logInfo $ show ledgerTx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo $ "swapped with: " ++ show lp

-- | Finds all liquidity pools and their liquidity belonging to the AltSwap instance.
-- This merely inspects the blockchain and does not issue any transactions.
pools :: forall w s. AltSwap -> Contract w s Text [((Coin A, Amount A), (Coin B, Amount B))]
pools aswp = do
    utxos <- utxosAt (aswpAddress aswp)
    go $ snd <$> Map.toList utxos
  where
    go :: [ChainIndexTxOut] -> Contract w s Text [((Coin A, Amount A), (Coin B, Amount B))]
    go []       = return []
    go (o : os) = do
        let v = view ciTxOutValue o
        if isUnity v c
            then do
                d <- getAltSwapDatum o
                case d of
                    Factory _ -> go os
                    Pool lp _ -> do
                        let coinA = lpCoinA lp
                            coinB = lpCoinB lp
                            amtA  = amountOf v coinA
                            amtB  = amountOf v coinB
                            s     = ((coinA, amtA), (coinB, amtB))
                        logInfo $ "found pool: " ++ show s
                        ss <- go os
                        return $ s : ss
            else go os
      where
        c :: Coin PoolState
        c = poolStateCoin aswp

-- | Gets the caller's funds.
funds :: forall w s. Contract w s Text Value
funds = do
    pkh <- pubKeyHash <$> ownPubKey
    os  <- map snd . Map.toList <$> utxosAt (pubKeyHashAddress pkh)
    return $ mconcat [view ciTxOutValue o | o <- os]

getAltSwapDatum :: ChainIndexTxOut -> Contract w s Text AltSwapDatum
getAltSwapDatum o =
  case o of
      PublicKeyChainIndexTxOut {} ->
        throwError "no datum for a txout of a public key address"
      ScriptChainIndexTxOut { _ciTxOutDatum } -> do
        (Datum e) <- either getDatum pure _ciTxOutDatum
        maybe (throwError "datum hash wrong type")
              pure
              (PlutusTx.fromBuiltinData e)
  where
    getDatum :: DatumHash -> Contract w s Text Datum
    getDatum dh =
      datumFromHash dh >>= \case Nothing -> throwError "datum not found"
                                 Just d  -> pure d

findAltSwapInstance ::
    forall a b w s.
    AltSwap
    -> Coin b
    -> (AltSwapDatum -> Maybe a)
    -> Contract w s Text (TxOutRef, ChainIndexTxOut, a)
findAltSwapInstance aswp c f = do
    let addr = aswpAddress aswp
    logInfo @String $ printf "looking for AltSwap instance at address %s containing coin %s " (show addr) (show c)
    utxos <- utxosAt addr
    go  [x | x@(_, o) <- Map.toList utxos, isUnity (view ciTxOutValue o) c]
  where
    go [] = throwError "AltSwap instance not found"
    go ((oref, o) : xs) = do
        d <- getAltSwapDatum o
        case f d of
            Nothing -> go xs
            Just a  -> do
                logInfo @String $ printf "found AltSwap instance with datum: %s" (show d)
                return (oref, o, a)

findAltSwapFactory :: forall w s. AltSwap -> Contract w s Text (TxOutRef, ChainIndexTxOut, [LiquidityPool])
findAltSwapFactory aswp@AltSwap{..} = findAltSwapInstance aswp swpCoin $ \case
    Factory lps -> Just lps
    Pool _ _    -> Nothing

findAltSwapPool :: forall w s. AltSwap -> LiquidityPool -> Contract w s Text (TxOutRef, ChainIndexTxOut, Amount Liquidity)
findAltSwapPool aswp lp = findAltSwapInstance aswp (poolStateCoin aswp) $ \case
        Pool lp' l
            | lp == lp' -> Just l
        _               -> Nothing

findAltSwapFactoryAndPool :: forall w s.
                          AltSwap
                          -> Coin A
                          -> Coin B
                          -> Contract w s Text ( (TxOutRef, ChainIndexTxOut, [LiquidityPool])
                                               , (TxOutRef, ChainIndexTxOut, LiquidityPool, Amount Liquidity)
                                               )
findAltSwapFactoryAndPool aswp coinA coinB = do
    (oref1, o1, lps) <- findAltSwapFactory aswp
    case [ lp'
         | lp' <- lps
         , lp' == LiquidityPool coinA coinB
         ] of
        [lp] -> do
            (oref2, o2, a) <- findAltSwapPool aswp lp
            return ( (oref1, o1, lps)
                   , (oref2, o2, lp, a)
                   )
        _    -> throwError "liquidity pool not found"

findSwapA :: Amount A -> Amount B -> Amount A -> Integer
findSwapA oldA oldB inA
    | ub' <= 1   = 0
    | otherwise  = go 1 ub'
  where
    cs :: Integer -> Bool
    cs outB = checkSwap oldA oldB (oldA + inA) (oldB - Amount outB)

    ub' :: Integer
    ub' = head $ dropWhile cs [2 ^ i | i <- [0 :: Int ..]]

    go :: Integer -> Integer -> Integer
    go lb ub
        | ub == (lb + 1) = lb
        | otherwise      =
      let
        m = div (ub + lb) 2
      in
        if cs m then go m ub else go lb m

findSwapB :: Amount A -> Amount B -> Amount B -> Integer
findSwapB oldA oldB inB = findSwapA (switch oldB) (switch oldA) (switch inB)
  where
    switch = Amount . swpAmount

ownerEndpoint :: Contract (Last (Either Text AltSwap)) EmptySchema ContractError ()
ownerEndpoint = do
    e <- mapError absurd $ runError start
    void $ waitNSlots 1
    tell $ Last $ Just e

-- | Provides the following endpoints for aswpers of a AltSwap instance:
--
--      [@create@]: Creates a liquidity pool for a pair of coins. The creator provides liquidity for both coins and gets liquidity tokens in return.
--      [@swap@]: Uses a liquidity pool two swap one sort of coins in the pool against the other.
--      [@close@]: Closes a liquidity pool by burning all remaining liquidity tokens in exchange for all liquidity remaining in the pool.
--      [@remove@]: Removes some liquidity from a liquidity pool in exchange for liquidity tokens.
--      [@add@]: Adds some liquidity to an existing liquidity pool in exchange for newly minted liquidity tokens.
--      [@pools@]: Finds all liquidity pools and their liquidity belonging to the AltSwap instance. This merely inspects the blockchain and does not issue any transactions.
--      [@funds@]: Gets the caller's funds. This merely inspects the blockchain and does not issue any transactions.
--      [@stop@]: Stops the contract.
userEndpoints :: AltSwap -> Promise (Last (Either Text UserContractState)) AltSwapUserSchema Void ()
userEndpoints aswp =
    stop
        `select`
    (void (f (Proxy @"create") (const Created) create                 `select`
           f (Proxy @"swap")   (const Swapped) swap                   `select`
           f (Proxy @"close")  (const Closed)  close                  `select`
           f (Proxy @"remove") (const Removed) remove                 `select`
           f (Proxy @"add")    (const Added)   add                    `select`
           f (Proxy @"pools")  Pools           (\us' () -> pools aswp') `select`
           f (Proxy @"funds")  Funds           (\_us () -> funds))
     <> aswperEndpoints aswp)
  where
    f :: forall l a p.
         (HasEndpoint l p AltSwapUserSchema, FromJSON p)
      => Proxy l
      -> (a -> UserContractState)
      -> (AltSwap -> p -> Contract (Last (Either Text UserContractState)) AltSwapUserSchema Text a)
      -> Promise (Last (Either Text UserContractState)) AltSwapUserSchema Void ()
    f _ g c = handleEndpoint @l $ \p -> do
        e <- either (pure . Left) (runError . c aswp) p
        tell $ Last $ Just $ case e of
            Left err -> Left err
            Right a  -> Right $ g a

    stop :: Promise (Last (Either Text UserContractState)) AltSwapUserSchema Void ()
    stop = handleEndpoint @"stop" $ \e -> do
        tell $ Last $ Just $ case e of
            Left err -> Left err
            Right () -> Right Stopped
