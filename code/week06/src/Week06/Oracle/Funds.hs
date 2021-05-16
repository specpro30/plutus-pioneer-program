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

module Week06.Oracle.Funds
    ( ownFunds
    , ownFunds'
    ) where

import           Control.Monad    hiding (fmap)
import qualified Data.Map         as Map
import           Data.Monoid      (Last (..))
import           Data.Text        (Text)
import           Plutus.Contract  as Contract hiding (when)
import           PlutusTx.Prelude hiding ((<$>))
import           Prelude          ((<$>))
import           Ledger           hiding (singleton)
import           Ledger.Value     as Value

ownFunds :: HasBlockchainActions s => Contract w s Text Value         -- this function runs forever and checks how much funds you have every slot and writes it to the log 
ownFunds = do
    pk    <- ownPubKey                                                -- looks up your own public key 
    utxos <- utxoAt $ pubKeyAddress pk                                -- looks up UTXOs at the address given by your public key and gives a map of utxos 
    let v = mconcat $ Map.elems $ txOutValue . txOutTxOut <$> utxos   -- gets the Value (txOutValue) of the Outputs (txOutTxOut) belonging to the utxos and returns the value elements only (with Map.elems)
                                                                      -- (keys of the utxos are discarded) mconcat adds up all the values to one integer v (now the sum of values of all the utxos of the pk) 
    logInfo @String $ "own funds: " ++ show (Value.flattenValue v)    -- log the result and returns it 
    return v

ownFunds' :: Contract (Last Value) BlockchainActions Text ()          -- variation of ownFunds that instead of returning the value it permananetly tells it 
ownFunds' = do
    handleError logError $ ownFunds >>= tell . Last . Just            -- calls ownFunds and tells, Just, Last the value with Monadic bind >>=. This is wrapped in an error handler 
    void $ Contract.waitNSlots 1                                      -- waits one slot and recurses 
    ownFunds'
