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

module Week06.Oracle.Core
    ( Oracle (..)
    , OracleRedeemer (..)
    , oracleTokenName
    , oracleValue
    , oracleAsset
    , oracleInst
    , oracleValidator
    , oracleAddress
    , OracleSchema
    , OracleParams (..)
    , runOracle
    , findOracle
    ) where

import           Control.Monad             hiding (fmap)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Map                  as Map
import           Data.Monoid               (Last (..))
import           Data.Text                 (Text, pack)
import           GHC.Generics              (Generic)
import           Plutus.Contract           as Contract hiding (when)
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)
import           Ledger                    hiding (singleton)
import           Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Ledger.Ada                as Ada
import           Plutus.Contracts.Currency as Currency
import           Prelude                   (Semigroup (..))
import qualified Prelude                   as Prelude

data Oracle = Oracle                                                  -- The Oracle is a paramaterized contract and the data type called Oracle is the parameter
    { oSymbol   :: !CurrencySymbol                                    -- First of four fields is the oSymbol which is the currency symbol of the NFT. Token Name is just empty string 
    , oOperator :: !PubKeyHash                                        -- oOperator is the owner of the Oracle and is the only one that can make updates where as anyone can use
    , oFee      :: !Integer                                           -- oFee is the fees in Lovelace that is required everytime someone uses the Oracle
    , oAsset    :: !AssetClass                                        -- is the target of the swap contract and in this case is the USD token 
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord) -- boiler plate code of type classes required to be serializable

PlutusTx.makeLift ''Oracle                                            -- boiler plate for liftable

data OracleRedeemer = Update | Use                                    -- here we define the redeemer to have two use cases: update and use 
    deriving Show

PlutusTx.unstableMakeIsData ''OracleRedeemer                          -- use template haskell to implment IsData for the Oracle Redeemer data type 

{-# INLINABLE oracleTokenName #-}                                     -- starting from here are some helper definitions
oracleTokenName :: TokenName                                 
oracleTokenName = TokenName emptyByteString                           -- using emptyByteString for the token name 

{-# INLINABLE oracleAsset #-}
oracleAsset :: Oracle -> AssetClass                                   -- oracleAsset is used to uniquely identify the UTXO of the NFT with the Oracle value. Recall that AssetClass requires currency sym and tn 
oracleAsset oracle = AssetClass (oSymbol oracle, oracleTokenName)

{-# INLINABLE oracleValue #-}
oracleValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Integer   -- TxOut is the output of the UTXO that holds the Oracle. We want to look up the dataum and turn into an integer 
oracleValue o f = do                                                  -- do block is inside the Maybe Monad so the result of the bind can be nothing 
    dh      <- txOutDatum o                                           -- getting Datum from TxOut (can fail and result in nothing) or succeed in which case we get dh (datum hash)
    Datum d <- f dh                                                   -- f is the function we use to turn the dh into a Datum d
    PlutusTx.fromData d                                               -- Use the PlutusTx.fromData to maybe turn d into an integer 

{-# INLINABLE mkOracleValidator #-}
mkOracleValidator :: Oracle -> Integer -> OracleRedeemer -> ScriptContext -> Bool    -- mkOracleValidator gets the parameter Oracle, Integer datum, redeemer type OracleRedeemer, ScriptContext and returns a Bool 
mkOracleValidator oracle x r ctx =                                                   -- x is the old Oracle value 
    traceIfFalse "token missing from input"  inputHasToken  &&                       -- checks to see if the input holds the NFT
    traceIfFalse "token missing from output" outputHasToken &&                       -- checks to see if the output holds the NFT 
    case r of
        Update -> traceIfFalse "operator signature missing" (txSignedBy info $ oOperator oracle) &&    -- checks to see if the operator of the Oracle has signed withe the PubKeyHash from oOperator  
                  traceIfFalse "invalid output datum"       validOutputDatum                           -- chcecks that the outputDatum is an integer. since value is not checked operator can retreive fees
        Use    -> traceIfFalse "oracle value changed"       (outputDatum == Just x)              &&    -- checks to see if the Datum has changed. Use does not allow datum to change 
                  traceIfFalse "fees not paid"              feesPaid                                   -- checks if fee are paid 
  where
    info :: TxInfo                                       -- takes the context and extracts the TxInfo from it 
    info = scriptContextTxInfo ctx

    ownInput :: TxOut                                    -- the TxOut is the Oracle output that we are trying to consume 
    ownInput = case findOwnInput ctx of                  -- need to verify 
        Nothing -> traceError "oracle input missing"
        Just i  -> txInInfoResolved i

    inputHasToken :: Bool                                                               -- helper function to check if the NFT token is present. checks to see if Oracle input carries NFT
    inputHasToken = assetClassValueOf (txOutValue ownInput) (oracleAsset oracle) == 1   -- assetClassValueOf :: Value -> AssetClass -> Integer how many coins of that Asset Class are contained in the value
                                                                                        -- should be only 1 coin for NFT 
                                                                                        -- txOutValue ownInput is the Value attached to the input that we are consuming

    ownOutput :: TxOut                                            -- checks to see if the use and update redeemers will produce exactly only one Oracle output 
    ownOutput = case getContinuingOutputs ctx of                  -- getContinuingOutputs retreives a list from the context of outputs going to Oracle address 
        [o] -> o                                                  -- checks to see if there is only one output 
        _   -> traceError "expected exactly one oracle output"    -- if there are zero or more than one outputs then return an error 

    outputHasToken :: Bool
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (oracleAsset oracle) == 1    -- checks to see if Oracle output carries the NFT

    outputDatum :: Maybe Integer                                  -- checks to see if it is a valid integer 
    outputDatum = oracleValue ownOutput (`findDatum` info)        -- findDatum takes the info and the datum hash and tries to look up the corresponding datum 

    validOutputDatum :: Bool
    validOutputDatum = isJust outputDatum    -- checks that the outputDatum is not nothing.  If it is something then return true 

    feesPaid :: Bool
    feesPaid =
      let
        inVal  = txOutValue ownInput    -- check the value attached to the Oracle input 
        outVal = txOutValue ownOutput   -- check the value attached to the Oracle output 
      in
        outVal `geq` (inVal <> Ada.lovelaceValueOf (oFee oracle))    -- output should be at least as large as the input plus the fees. <> Semigroup operator to combine values. Ada.lovelaceValueOf to lovelace val

data Oracling                           -- boiler plate helper function to combine the Datum type and the Redeemer type 
instance Scripts.ScriptType Oracling where
    type instance DatumType Oracling = Integer
    type instance RedeemerType Oracling = OracleRedeemer

oracleInst :: Oracle -> Scripts.ScriptInstance Oracling    -- template Haskell used to compile into a script instance 
oracleInst oracle = Scripts.validator @Oracling            -- because it is parameterized the liftCode is necessary to lift it into the Plutus script 
    ($$(PlutusTx.compile [|| mkOracleValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode oracle)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @OracleRedeemer

oracleValidator :: Oracle -> Validator    -- boiler plate code to turn it into a validator 
oracleValidator = Scripts.validatorScript . oracleInst

oracleAddress :: Oracle -> Ledger.Address -- boiler plate code to turn validator into an script Oracle address 
oracleAddress = scriptAddress . oracleValidator

-- Above code is for the on-chain part of the Oracle and the below code is the off-chain part of the Oracle 
-- Note that the Oracle provider only needs to provision the "update" function. The "use" function is not part of the Oracle off-chain code 

data OracleParams = OracleParams    -- parameters required to start the Oracle 
    { opFees   :: !Integer          -- fees that we want to charge
    , opSymbol :: !CurrencySymbol   -- CS of the Asset that we would like to swap Ada for 
    , opToken  :: !TokenName        -- TokenName of Asset that we would like to swap Ada for 
    } deriving (Show, Generic, FromJSON, ToJSON)

startOracle :: forall w s. HasBlockchainActions s => OracleParams -> Contract w s Text Oracle -- startOracle only mints the NFT. To ensure Oracle has update price info, assigning value is left to update function 
startOracle op = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    osc <- mapError (pack . show) (forgeContract pkh [(oracleTokenName, 1)] :: Contract w s CurrencyError OneShotCurrency)
    -- show will get the string of the CurrencyError. pack converts the string into text. mapError is used for custom error messages. forgeContract is a function that mints various tokens of name and qty
    -- the CurrencyError type needs to be specified (e')
    let cs     = Currency.currencySymbol osc
        oracle = Oracle
            { oSymbol   = cs
            , oOperator = pkh
            , oFee      = opFees op
            , oAsset    = AssetClass (opSymbol op, opToken op)
            }
    logInfo @String $ "started oracle " ++ show oracle
    return oracle

updateOracle :: forall w s. HasBlockchainActions s => Oracle -> Integer -> Contract w s Text ()
updateOracle oracle x = do
    m <- findOracle oracle
    let c = Constraints.mustPayToTheScript x $ assetClassValue (oracleAsset oracle) 1
    case m of
        Nothing -> do
            ledgerTx <- submitTxConstraints (oracleInst oracle) c
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "set initial oracle value to " ++ show x
        Just (oref, o,  _) -> do
            let lookups = Constraints.unspentOutputs (Map.singleton oref o)     <>
                          Constraints.scriptInstanceLookups (oracleInst oracle) <>
                          Constraints.otherScript (oracleValidator oracle)
                tx      = c <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData Update)
            ledgerTx <- submitTxConstraintsWith @Oracling lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "updated oracle value to " ++ show x

findOracle :: forall w s. HasBlockchainActions s => Oracle -> Contract w s Text (Maybe (TxOutRef, TxOutTx, Integer))
findOracle oracle = do
    utxos <- Map.filter f <$> utxoAt (oracleAddress oracle)
    return $ case Map.toList utxos of
        [(oref, o)] -> do
            x <- oracleValue (txOutTxOut o) $ \dh -> Map.lookup dh $ txData $ txOutTxTx o
            return (oref, o, x)
        _           -> Nothing
  where
    f :: TxOutTx -> Bool
    f o = assetClassValueOf (txOutValue $ txOutTxOut o) (oracleAsset oracle) == 1

type OracleSchema = BlockchainActions .\/ Endpoint "update" Integer

runOracle :: OracleParams -> Contract (Last Oracle) OracleSchema Text ()
runOracle op = do
    oracle <- startOracle op
    tell $ Last $ Just oracle
    go oracle
  where
    go :: Oracle -> Contract (Last Oracle) OracleSchema Text a
    go oracle = do
        x <- endpoint @"update"
        updateOracle oracle x
        go oracle
