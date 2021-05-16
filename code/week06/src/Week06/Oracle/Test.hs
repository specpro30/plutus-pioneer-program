{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week06.Oracle.Test where -- this code provides the emulator testing code for running the swap smart contract

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Data.Monoid                (Last (..))
import           Data.Text                  (Text)
import           Ledger
import           Ledger.Value               as Value
import           Ledger.Ada                 as Ada
import           Plutus.Contract            as Contract hiding (when)
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (Semigroup(..))
import           Wallet.Emulator.Wallet

import           Week06.Oracle.Core
import           Week06.Oracle.Funds
import           Week06.Oracle.Swap

assetSymbol :: CurrencySymbol
assetSymbol = "ff"                   -- some arbitrary currency symbol for testing purposes

assetToken :: TokenName
assetToken = "USDT"                  -- the asset we are using for the swap 

test :: IO ()
test = runEmulatorTraceIO' def emCfg myTrace   -- using the prime version of runEmulatorTraceIO so that we can pass special arguments: def = default, emCfg = to configure the inital distribution of assets 
  where
    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig $ Left $ Map.fromList [(Wallet i, v) | i <- [1 .. 10]]

    v :: Value
    v = Ada.lovelaceValueOf                    100_000_000 <>   -- for the distribution everyone has 100 million lovelaces (100 Ada) 
        Value.singleton assetSymbol assetToken 100_000_000      -- everyone has 100 USDT tokens 

checkOracle :: Oracle -> Contract () BlockchainActions Text a            -- helper contract to constantly check the Oracle value and log it 
checkOracle oracle = do                                                  
    m <- findOracle oracle                                               -- finds the Oracle 
    case m of                           
        Nothing        -> return ()                                      -- returns nothing if Oracle not found
        Just (_, _, x) -> Contract.logInfo $ "Oracle value: " ++ show x  -- returns 'x' (the exchange rate) if Oracle is found 
    Contract.waitNSlots 1 >> checkOracle oracle

myTrace :: EmulatorTrace ()                 -- define the trace 
myTrace = do
    let op = OracleParams                   -- defines some parameters for the Oracle 
                { opFees = 1_000_000        -- fee for Oracle 
                , opSymbol = assetSymbol    -- define Currency Symbol 
                , opToken  = assetToken     -- define Token Name 
                }

    h1 <- activateContractWallet (Wallet 1) $ runOracle op        -- start the Oracle with these parameters for wallet 1 
    void $ Emulator.waitNSlots 1                                  -- wait for one slot
    oracle <- getOracle h1                                        -- use the getOracle helper function defined below 

    void $ activateContractWallet (Wallet 2) $ checkOracle oracle -- once we have the value from the Oracle then wallet two prints the value every slot 

    callEndpoint @"update" h1 1_500_000   -- initialize the value of the Oracle to 1500000 for testing purposes
    void $ Emulator.waitNSlots 3          -- wait for three slots 

    void $ activateContractWallet (Wallet 1) ownFunds'     -- call the ownFunds function on wallets 1-5 to check initial balances
    void $ activateContractWallet (Wallet 3) ownFunds'
    void $ activateContractWallet (Wallet 4) ownFunds'
    void $ activateContractWallet (Wallet 5) ownFunds'

    h3 <- activateContractWallet (Wallet 3) $ swap oracle   -- start the swap contract on h3, h4, h5 
    h4 <- activateContractWallet (Wallet 4) $ swap oracle
    h5 <- activateContractWallet (Wallet 5) $ swap oracle

    callEndpoint @"offer" h3 10_000_000  -- wallet 3 offers 10000000 for swap 
    callEndpoint @"offer" h4 20_000_000  -- wallet 4 offers 20000000 for swap 
    void $ Emulator.waitNSlots 3         -- wait 3 slots 

    callEndpoint @"use" h5 ()            -- wallet 5 uses the swap (according to value of Oracle) 
                                         -- and will pick either offer from wallet 3 or 4 depending on which one it finds first (not realistic but just for exercise) 
    void $ Emulator.waitNSlots 3         -- waits 3 slots 

    callEndpoint @"update" h1 1_700_000   -- wallet 1 updates Oracle value to 1.7
    void $ Emulator.waitNSlots 3

    callEndpoint @"use" h5 ()             -- wallet 5 uses the swap again 
    void $ Emulator.waitNSlots 3

    callEndpoint @"update" h1 1_800_000   -- wallet 1 updates Oracle value to 1.8.  All fees are consumed after this with fees of two ada going to Wallet 1
    void $ Emulator.waitNSlots 3

    callEndpoint @"retrieve" h3 ()        -- wallet 3 retreives swaps but nothing happens as swaps already used
    callEndpoint @"retrieve" h4 ()        -- wallet 4 retreives swaps but nothing happens as swaps already used 
    void $ Emulator.waitNSlots 3
  where
    getOracle :: ContractHandle (Last Oracle) OracleSchema Text -> EmulatorTrace Oracle -- (don't really understand this part)
    getOracle h = do
        l <- observableState h  -- takes the observableState of the runOracle contract which should contain the Oracle 
        case l of
            Last Nothing       -> Emulator.waitNSlots 1 >> getOracle h -- if its not there then wait one slot and try again
            Last (Just oracle) -> Extras.logInfo (show oracle) >> return oracle -- if it is there then we have the value oracle and log output and return oracle 
