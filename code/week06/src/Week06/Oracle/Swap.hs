{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week06.Oracle.Swap    -- smart contract that takes Ada and swaps it for USDT. The price in USDT for the Ada will change overtime depending on Oracle 
    ( SwapSchema
    , swap
    ) where

import           Control.Monad        hiding (fmap)
import           Data.List            (find)
import qualified Data.Map             as Map
import           Data.Maybe           (mapMaybe)
import           Data.Monoid          (Last (..))
import           Data.Text            (Text)
import           Plutus.Contract      as Contract hiding (when)
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), (<$>), unless, mapMaybe, find)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada hiding (divide)
import           Ledger.Value         as Value
import           Prelude              (Semigroup (..), (<$>))

import           Week06.Oracle.Core
import           Week06.Oracle.Funds

{-# INLINABLE price #-}
price :: Integer -> Integer -> Integer    -- helper function that calculates the price of Ada 
price lovelace exchangeRate = (lovelace * exchangeRate) `divide` 1000000

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer             -- helper function that converts a given value to lovelaces (Value to Ada to lovelaces) 
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE mkSwapValidator #-}
mkSwapValidator :: Oracle -> Address -> PubKeyHash -> () -> ScriptContext -> Bool    -- this contract takes two parameters Oracle and Oracle Address. Address (given explicitly) is the address of the Oracle
                                                                                     -- for Datum we use PubKeyHas of the seller of the Ada. 
mkSwapValidator oracle addr pkh () ctx =    -- takes two script inputs and another input: 1) Oracle for exchange rate 2) Swap outputs with lovelaces; other input is the funds from buyer
                                            -- produces three outputs: 1) Oracle output (taken care of by Oracle Validator), 2) seller gets USDT, 3) buyer gets lovelaces 
    txSignedBy info pkh ||                  -- This is the case where the seller signs the swap contract to get back the Ada that they intended to swap 
    (traceIfFalse "expected exactly two script inputs" hasTwoScriptInputs &&         -- This is the second case where the swap occurs. Check inputs of Oracle and Swap outputs 
     traceIfFalse "price not paid"                     sellerPaid)                   -- check seller gets paid 

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    oracleInput :: TxOut                    -- helper function to find the Oracle inputs (Oracle input consumes the Oracle output) 
    oracleInput =
      let
        ins = [ o
              | i <- txInfoInputs info      -- i gets a list of all inputs from the info field
              , let o = txInInfoResolved i  -- computes the corresponding outputs given a list of i inputs 
              , txOutAddress o == addr      -- gets all inputs with corresponding output o that sits at the Oracle Address (addr). This filter of the list comprehension keeps all the 'o's   
              ]
      in
        case ins of
            [o] -> o                                                   -- checks that there is exactly one 'o' which means there is only one Oracle input (since it is an NFT?)
            _   -> traceError "expected exactly one oracle input"      -- given any other result '_' throw an expection

    oracleValue' = case oracleValue oracleInput (`findDatum` info) of  -- check exchange rate with helper function oracleValue which given the output where the Oracle sits it comuptes the datum (exchange rate)
                                                                       -- findDatum info looks up datum to a hash
        Nothing -> traceError "oracle value not found"                 -- fails to find an Oracle input
        Just x  -> x                                                   -- succeeds to find Oracle input and Datum and returns the exchange rate

    hasTwoScriptInputs :: Bool
    hasTwoScriptInputs =
      let
        xs = filter (isJust . toValidatorHash . txOutAddress . txInInfoResolved) $ txInfoInputs info -- txInfoInputs info loops over all the inputs. Filter for all the outputs of the inputs (txInInfoResolved)
                                                                                                     -- Filter the address for the output (txOutAddress), validator hash of the output (toValidatorHash,
                                                                                                     -- hash if script and nothing if public key), isJust then returns True for something the False for nothing 
                                                                                                     -- xs is now a list of all script inputs 
      in
        length xs == 2                                                                               -- check that xs is length 2 as we expect the two inputs of the Oracle and the Swap itself

    minPrice :: Integer                                             -- compute the minimum price in USDT that needs to be paid by buyer
    minPrice =
      let
        lovelaceIn = case findOwnInput ctx of                       -- findOwnInput gives the input that has just been validated which is the swap input 
            Nothing -> traceError "own input not found"             -- if it doesn't find any then return error
            Just i  -> lovelaces $ txOutValue $ txInInfoResolved i  -- otherwise calculate the amount of lovelace to be sold by getting it from using the helper function lovalaces to compute the amount from
                                                                    -- the value found (txOutValue) from the outputs (txInInfoResolved i)
      in
        price lovelaceIn oracleValue'                               -- multiply amount of lovelace found by the current price from Oracle to get the minPrice to be paid for the lovelaces 

    sellerPaid :: Bool                                                         -- now we check that the seller has paid 
    sellerPaid =
      let
        pricePaid :: Integer
        pricePaid =  assetClassValueOf (valuePaidTo info pkh) (oAsset oracle)  -- using valuePaidTo info pkh to lookup all the available outputs of pkh's wallet in USDT (oAsset oracle)
      in
        pricePaid >= minPrice                                                  -- check the amount paid is larger than minPrice (allow for gratuity)

data Swapping
instance Scripts.ScriptType Swapping where
    type instance DatumType Swapping = PubKeyHash
    type instance RedeemerType Swapping = ()

swapInst :: Oracle -> Scripts.ScriptInstance Swapping
swapInst oracle = Scripts.validator @Swapping
    ($$(PlutusTx.compile [|| mkSwapValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode oracle
        `PlutusTx.applyCode` PlutusTx.liftCode (oracleAddress oracle))
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @PubKeyHash @()

swapValidator :: Oracle -> Validator
swapValidator = Scripts.validatorScript . swapInst

swapAddress :: Oracle -> Ledger.Address
swapAddress = scriptAddress . swapValidator

offerSwap :: forall w s. HasBlockchainActions s => Oracle -> Integer -> Contract w s Text ()
offerSwap oracle amt = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let tx = Constraints.mustPayToTheScript pkh $ Ada.lovelaceValueOf amt
    ledgerTx <- submitTxConstraints (swapInst oracle) tx
    awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "offered " ++ show amt ++ " lovelace for swap"

findSwaps :: HasBlockchainActions s => Oracle -> (PubKeyHash -> Bool) -> Contract w s Text [(TxOutRef, TxOutTx, PubKeyHash)]
findSwaps oracle p = do
    utxos <- utxoAt $ swapAddress oracle
    return $ mapMaybe g $ Map.toList utxos
  where
    f :: TxOutTx -> Maybe PubKeyHash
    f o = do
        dh        <- txOutDatumHash $ txOutTxOut o
        (Datum d) <- Map.lookup dh $ txData $ txOutTxTx o
        PlutusTx.fromData d

    g :: (TxOutRef, TxOutTx) -> Maybe (TxOutRef, TxOutTx, PubKeyHash)
    g (oref, o) = do
        pkh <- f o
        guard $ p pkh
        return (oref, o, pkh)

retrieveSwaps :: HasBlockchainActions s => Oracle -> Contract w s Text ()
retrieveSwaps oracle = do
    pkh <- pubKeyHash <$> ownPubKey
    xs <- findSwaps oracle (== pkh)
    case xs of
        [] -> logInfo @String "no swaps found"
        _  -> do
            let lookups = Constraints.unspentOutputs (Map.fromList [(oref, o) | (oref, o, _) <- xs]) <>
                          Constraints.otherScript (swapValidator oracle)
                tx      = mconcat [Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData () | (oref, _, _) <- xs]
            ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "retrieved " ++ show (length xs) ++ " swap(s)"

useSwap :: forall w s. HasBlockchainActions s => Oracle -> Contract w s Text ()
useSwap oracle = do
    funds <- ownFunds
    let amt = assetClassValueOf funds $ oAsset oracle
    logInfo @String $ "available assets: " ++ show amt

    m <- findOracle oracle
    case m of
        Nothing           -> logInfo @String "oracle not found"
        Just (oref, o, x) -> do
            logInfo @String $ "found oracle, exchange rate " ++ show x
            pkh   <- pubKeyHash <$> Contract.ownPubKey
            swaps <- findSwaps oracle (/= pkh)
            case find (f amt x) swaps of
                Nothing                -> logInfo @String "no suitable swap found"
                Just (oref', o', pkh') -> do
                    let v       = txOutValue (txOutTxOut o) <> lovelaceValueOf (oFee oracle)
                        p       = assetClassValue (oAsset oracle) $ price (lovelaces $ txOutValue $ txOutTxOut o') x
                        lookups = Constraints.otherScript (swapValidator oracle)                     <>
                                  Constraints.otherScript (oracleValidator oracle)                   <>
                                  Constraints.unspentOutputs (Map.fromList [(oref, o), (oref', o')])
                        tx      = Constraints.mustSpendScriptOutput oref  (Redeemer $ PlutusTx.toData Use) <>
                                  Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toData ())  <>
                                  Constraints.mustPayToOtherScript
                                    (validatorHash $ oracleValidator oracle)
                                    (Datum $ PlutusTx.toData x)
                                    v                                                                      <>
                                  Constraints.mustPayToPubKey pkh' p
                    ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
                    awaitTxConfirmed $ txId ledgerTx
                    logInfo @String $ "made swap with price " ++ show (Value.flattenValue p)
  where
    getPrice :: Integer -> TxOutTx -> Integer
    getPrice x o = price (lovelaces $ txOutValue $ txOutTxOut o) x

    f :: Integer -> Integer -> (TxOutRef, TxOutTx, PubKeyHash) -> Bool
    f amt x (_, o, _) = getPrice x o <= amt

type SwapSchema =
    BlockchainActions
        .\/ Endpoint "offer"    Integer
        .\/ Endpoint "retrieve" ()
        .\/ Endpoint "use"      ()
        .\/ Endpoint "funds"    ()

swap :: Oracle -> Contract (Last Value) SwapSchema Text ()
swap oracle = (offer `select` retrieve `select` use `select` funds) >> swap oracle
  where
    offer :: Contract (Last Value) SwapSchema Text ()
    offer = h $ do
        amt <- endpoint @"offer"
        offerSwap oracle amt

    retrieve :: Contract (Last Value) SwapSchema Text ()
    retrieve = h $ do
        endpoint @"retrieve"
        retrieveSwaps oracle

    use :: Contract (Last Value) SwapSchema Text ()
    use = h $ do
        endpoint @"use"
        useSwap oracle

    funds :: Contract (Last Value) SwapSchema Text ()
    funds = h $ do
        endpoint @"funds"
        v <- ownFunds
        tell $ Last $ Just v

    h :: Contract (Last Value) SwapSchema Text () -> Contract (Last Value) SwapSchema Text ()
    h = handleError logError
