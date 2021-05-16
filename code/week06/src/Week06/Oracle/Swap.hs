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

data Swapping                                                                  -- some boiler plate code
instance Scripts.ScriptType Swapping where
    type instance DatumType Swapping = PubKeyHash
    type instance RedeemerType Swapping = ()

swapInst :: Oracle -> Scripts.ScriptInstance Swapping                          -- only need to give one argument for template Haskell since Oracle's address is computed by oracleAddress below
swapInst oracle = Scripts.validator @Swapping
    ($$(PlutusTx.compile [|| mkSwapValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode oracle
        `PlutusTx.applyCode` PlutusTx.liftCode (oracleAddress oracle))         -- to compute the Oracle's Address 
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @PubKeyHash @()

swapValidator :: Oracle -> Validator                                           -- some more boiler plate 
swapValidator = Scripts.validatorScript . swapInst

swapAddress :: Oracle -> Ledger.Address                                        -- some more boiler plate 
swapAddress = scriptAddress . swapValidator

offerSwap :: forall w s. HasBlockchainActions s => Oracle -> Integer -> Contract w s Text ()    -- Seller uses OfferSwap which gets the Oracle and Integer (amount of lovelace to be sold) 
offerSwap oracle amt = do
    pkh <- pubKeyHash <$> Contract.ownPubKey                                                    -- Look up sellers public key 
    let tx = Constraints.mustPayToTheScript pkh $ Ada.lovelaceValueOf amt                       -- Locks the amount to be sold by taking pkh of seller and amt to be sold  
    ledgerTx <- submitTxConstraints (swapInst oracle) tx                                        -- Submit the tx to the blockchain
    awaitTxConfirmed $ txId ledgerTx                                                            -- Waits for confirmation
    logInfo @String $ "offered " ++ show amt ++ " lovelace for swap"                            -- Logs the amount of lovelace offered for swapping 

findSwaps :: HasBlockchainActions s => Oracle -> (PubKeyHash -> Bool) -> Contract w s Text [(TxOutRef, TxOutTx, PubKeyHash)]  -- helper function to find all swaps based on predicate (PubKeyHash -> Bool)
                                                                                                                              -- returns list of all the UTXOs that sit at swap address
                                                                                                                              -- reference to the UTXO (TxOutRef), UTXO (TxOutTx), Datum of UTXO (PubKeyHash)
findSwaps oracle p = do
    utxos <- utxoAt $ swapAddress oracle                             -- utxoAt gives all the UTXO at swap address 
    return $ mapMaybe g $ Map.toList utxos                           -- use mapMaybe on the list of the map of utxos 
  where
    f :: TxOutTx -> Maybe PubKeyHash                                 -- takes one of the UTXO we are looking at and gives a Maybe PubKeyHash (if succeed then get a Just PubKeyHash)
    f o = do                                                         -- we expect that all these swap UTXOs that the datum will be there and that it will be a pubKeyHash 
        dh        <- txOutDatumHash $ txOutTxOut o                   -- look up the datumHash attached to the outputs 
        (Datum d) <- Map.lookup dh $ txData $ txOutTxTx o            -- if succeeds then in the txData of the transaction (txOutTxTx) look up hash (dh) to get Datum d 
        PlutusTx.fromData d                                          -- then deserialize it from d to get pubKeyHash 

    g :: (TxOutRef, TxOutTx) -> Maybe (TxOutRef, TxOutTx, PubKeyHash) -- takes a pair of key and value and returns a Maybe key, value and pubKeyHash
    g (oref, o) = do
        pkh <- f o                                                    -- use the f helper function to get the public key hash which is the Datum now
        guard $ p pkh                                                 -- guard checks to see whether the Datum of the UTXO satisfys the predicate p and returns true if it does
        return (oref, o, pkh)                                         -- If true then returns the triple of oref, o, pkh (reference, UTXO and the Datum)

retrieveSwaps :: HasBlockchainActions s => Oracle -> Contract w s Text ()  -- function to help seller retreive their funds back if they don't want to proceed with swap 
retrieveSwaps oracle = do                                                  
    pkh <- pubKeyHash <$> ownPubKey                                        -- takes the public key hash of seller 
    xs <- findSwaps oracle (== pkh)                                        -- only takes the UTXOs sitting at that address that belong to seller (holder of pk)
    case xs of
        [] -> logInfo @String "no swaps found"                             -- if empty list (no swaps found that belong to the seller) then do nothing 
        _  -> do                                                           -- otherwise construct a transaction that retreives all the swaps that belong to seller 
            let lookups = Constraints.unspentOutputs (Map.fromList [(oref, o) | (oref, o, _) <- xs]) <>  -- finds all the UTXOs that belong to seller Map.fromList turns key value pairs into an action map 
                          Constraints.otherScript (swapValidator oracle)   -- provide the validator of the swap which is parametrized by the Oracle. Must provide the validator in order to consumer script outputs
                tx      = mconcat [Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData () | (oref, _, _) <- xs]  -- (oref, _, _) <- xs takes all the UTXOs in the list and for each of them
                                                                                                                                 -- a constraint is built to spend the UTXOs 
                                                                                                                                 -- mconcat combines a list of elements (doesn't matter how many) in a semigroup
                                                                                                                                 -- tx will get all the lovelaces from all the UTXOs that sit at the swap address
                                                                                                                                 -- belonging to the seller 
            ledgerTx <- submitTxConstraintsWith @Swapping lookups tx         -- submit transaction to blockchain   
            awaitTxConfirmed $ txId ledgerTx                                 -- wait for confirmation 
            logInfo @String $ "retrieved " ++ show (length xs) ++ " swap(s)" -- log the number of swaps retreived  

useSwap :: forall w s. HasBlockchainActions s => Oracle -> Contract w s Text ()    -- 
useSwap oracle = do
    funds <- ownFunds                                   -- looks up your own wallet and adds up all the funds available. In this case the total amount of USD tokens available for swaps 
    let amt = assetClassValueOf funds $ oAsset oracle   -- ownFunds is defined in Funds.hs. oAsset oracle specifies USD token and assetClassValueOf funds specifies the amount 
    logInfo @String $ "available assets: " ++ show amt

    m <- findOracle oracle                              -- findOracle is defined in the Core.hs. It finds the Oracle and the price of lovelaces 
    case m of
        Nothing           -> logInfo @String "oracle not found"  -- Oracle not found 
        Just (oref, o, x) -> do                                  -- Oracle is found and oref and o reference the Oracle's UTXO and x is the exchange rate 
            logInfo @String $ "found oracle, exchange rate " ++ show x
            pkh   <- pubKeyHash <$> Contract.ownPubKey           -- check our out public key 
            swaps <- findSwaps oracle (/= pkh)                   -- find all the swaps where we are not the owner of the swap 
            case find (f amt x) swaps of                         -- f is the predicate that takes the amount of funds available for swap and the exchange rate 'x' to find swaps we can afford  
                Nothing                -> logInfo @String "no suitable swap found"  -- can't find any affordable swaps 
                Just (oref', o', pkh') -> do                     -- if we do find a swap then just use the first one (not realistic, just an example for exercise)
                    let v       = txOutValue (txOutTxOut o) <> lovelaceValueOf (oFee oracle)  -- now we construct a transaction for the swap
                                                                                              -- v represents the total fees currently at the Oracle combined <> with our fees for using the Oracle 
                        p       = assetClassValue (oAsset oracle) $ price (lovelaces $ txOutValue $ txOutTxOut o') x  -- p is the price we need to pay. price function take lovelaces in swap 'o' 
                                                                                                                      -- and exchange rate 'x' to produce integer value that is converted to an assetClassValue USD 
                        lookups = Constraints.otherScript (swapValidator oracle)                     <>
                                  Constraints.otherScript (oracleValidator oracle)                   <>
                                  Constraints.unspentOutputs (Map.fromList [(oref, o), (oref', o')])           -- must provide the two UTXOs we want to consume, Oracle (oref, o) and the swap (oref', o')
                        tx      = Constraints.mustSpendScriptOutput oref  (Redeemer $ PlutusTx.toData Use) <>  -- must use the Oracle as input (oref) and must use the 'Use' redeemer
                                  Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toData ())  <>  -- must use the Swap input (oref'), which is a trivial Redeemer '()' in this case
                                  Constraints.mustPayToOtherScript                                             -- mustPayToOtherScript refers to the Oracle script not the swap script 
                                    (validatorHash $ oracleValidator oracle)                                   -- so we provide the Oracle hash 
                                    (Datum $ PlutusTx.toData x)                                                -- must use the existing 'x' Datum (exchange rate) and not change it 
                                    v                                                                      <>  -- 'v' computed as above
                                  Constraints.mustPayToPubKey pkh' p                                           -- pay the seller of the lovelace the price we calculated 'p'
                    ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
                    awaitTxConfirmed $ txId ledgerTx
                    logInfo @String $ "made swap with price " ++ show (Value.flattenValue p)                   -- show (Value.flattenValue p) logs the price we paid for the lovelaces 
  where
    getPrice :: Integer -> TxOutTx -> Integer
    getPrice x o = price (lovelaces $ txOutValue $ txOutTxOut o) x        -- price helper function is applied to the lovelaces contained in the swap output 'o' and the exchange rate 'x' 

    f :: Integer -> Integer -> (TxOutRef, TxOutTx, PubKeyHash) -> Bool    -- goes through the elements of swaps found to determine if suitable 
    f amt x (_, o, _) = getPrice x o <= amt                               -- suitability is defined as total cost of swap is less than or equal to amount of funds available 

type SwapSchema =                                                         -- the bundle that contains all the raw contracts above 
    BlockchainActions
        .\/ Endpoint "offer"    Integer                                   -- offer a swap for a given amount of lovelaces (Integer) 
        .\/ Endpoint "retrieve" ()                                        -- to retreive the funds of all the swap seller has on offer 
        .\/ Endpoint "use"      ()                                        -- to use a swap contract 
        .\/ Endpoint "funds"    ()                                        -- funds will give the amount of funds currently available for swapping 

swap :: Oracle -> Contract (Last Value) SwapSchema Text ()
swap oracle = (offer `select` retrieve `select` use `select` funds) >> swap oracle   -- 'select' operator waits until one endpoint is picked and executes that one. 
                                                                                     -- '>>' sequence operator then recursively calls swap oracle over and over again to offer the four endpoints
  where
    offer :: Contract (Last Value) SwapSchema Text ()
    offer = h $ do               -- 'h' is the error handler 
        amt <- endpoint @"offer" -- block with an offer endpoint until user provides the amt integer 
        offerSwap oracle amt     -- then call the offerSwap oracle amt defined above 

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
        v <- ownFunds         -- ownFunds function is called to get the value of funds availabe 
        tell $ Last $ Just v  -- tell is used to tell the outside world the value of funds available 

    h :: Contract (Last Value) SwapSchema Text () -> Contract (Last Value) SwapSchema Text ()
    h = handleError logError  -- just logs error and continues to avoid crashing the constract.  h is used to wrap all the endpoints above :
