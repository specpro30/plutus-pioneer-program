{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Main
    ( main
    ) where

import           Control.Monad                       (forM_, void, when)
import           Control.Monad.Freer                 (Eff, Member, interpret, type (~>))
import           Control.Monad.Freer.Error           (Error)
import           Control.Monad.Freer.Extras.Log      (LogMsg)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON, Result (..), fromJSON)
import           Data.Monoid                         (Last (..))
import           Data.Text                           (Text, pack)
import           Ledger
import           Ledger.Constraints
import qualified Ledger.Value                        as Value
import           Plutus.Contract                     hiding (when)
import           Plutus.PAB.Effects.Contract         (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), type (.\\), endpointsToSchemas, handleBuiltin)
import           Plutus.PAB.Monitoring.PABLogMsg     (PABMultiAgentMsg)
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import           Plutus.PAB.Types                    (PABError (..))
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import qualified Plutus.Contracts.Currency           as Currency

import           Wallet.Emulator.Types               (Wallet (..), walletPubKey)
import           Wallet.Types                        (ContractInstanceId (..))

import qualified Week06.Oracle.Core                  as Oracle
import           Week06.Oracle.PAB                   (OracleContracts (..))
import qualified Week06.Oracle.Swap                  as Oracle

main :: IO ()                                              -- as the code is in the main :: IO () it will be an executable 
main = void $ Simulator.runSimulationWith handlers $ do    -- uses the Simulator Monad that is specific to the PAB. Similar to the emulator trace Monad. Can start contracts on wallets, inspect states, etc 
                                                           -- The Simulator Monad will change soon (aligned with emulator trace monad) so check again when it is finalized 
    Simulator.logString @(Builtin OracleContracts) "Starting Oracle PAB webserver. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    -- Can do the below through the web instead. Use this scripted way for demonstration
    -- Can check what APIs are availabe at github.com/input-output-hk/plutus/blob/master/plutus-pab/src/Plutus/PAB/Webserver/API.hs 
    -- Makes use of Haskell libary Servant to write type safe web applications

    cidInit <- Simulator.activateContract (Wallet 1) Init  -- launches the Init Contract on Wallet 1 
    cs      <- waitForLast cidInit                         -- this line will block until Init contract has minted the USD and has distributed USD to other wallets, and then it will return cs (Currency Symbol)
    _       <- Simulator.waitUntilFinished cidInit         -- wait until init contract has finished 

    cidOracle <- Simulator.activateContract (Wallet 1) $ Oracle cs           -- start Oracle on Wallet 1.  Oracle cs runs Oracle with parameter cs (Currency Symbol)
                                                                             -- Need cidOracle (UUID handle to the Oracle contract) to talk to web interface
    liftIO $ writeFile "oracle.cid" $ show $ unContractInstanceId cidOracle  -- liftIO allows Simulator Monad to have side effects unlike emulator trace monad. UUID is writ into oracle.cid file for demo only
    oracle <- waitForLast cidOracle                                          -- use waitForLast to get the Oracle value (Swap contract is parameterized by this value) 

    forM_ wallets $ \w ->
        when (w /= Wallet 1) $ do                                                                       -- loop over other wallets besides wallet 1 which is running the Oracle 
            cid <- Simulator.activateContract w $ Swap oracle                                           -- activate swap contract for that wallet 
            liftIO $ writeFile ('W' : show (getWallet w) ++ ".cid") $ show $ unContractInstanceId cid   -- write the contract instance ids to a file like we did for oracle.cid 

    void $ liftIO getLine  -- block until user presses enter 
    shutdown               -- shutdown the server 

waitForLast :: FromJSON a => ContractInstanceId -> Simulator.Simulation t a
waitForLast cid =                           -- will block until we get a Just 
    flip Simulator.waitForState cid $ \json -> case fromJSON json of   -- use fromJSON to pass a JSON if succeed then get a 'Last a' 
        Success (Last (Just x)) -> Just x   -- if the result of applying the custom predicate to the json value is Just x then it returns the x. If succeed then we get the Last of Just of a state value 
                                            -- It can fail if we get a Last Nothing instead of a Just 
        _                       -> Nothing  -- if the result of applying the custom predicate to the json value is Nothing then it will wait more until state changes again 

wallets :: [Wallet]
wallets = [Wallet i | i <- [1 .. 5]]  -- the number of wallets used in simulation 

usdt :: TokenName
usdt = "USDT"

oracleParams :: CurrencySymbol -> Oracle.OracleParams
oracleParams cs = Oracle.OracleParams
    { Oracle.opFees   = 1_000_000                           -- fee is one Ada 
    , Oracle.opSymbol = cs                                  -- currency symbol cs is a provided argument 
    , Oracle.opToken  = usdt                                -- token is hardcoded USDT 
    }

handleOracleContracts ::                                                   -- boiler plate code to hook up the data types of contract instances with actual contracts 
    ( Member (Error PABError) effs
    , Member (LogMsg (PABMultiAgentMsg (Builtin OracleContracts))) effs
    )
    => ContractEffect (Builtin OracleContracts)
    ~> Eff effs
handleOracleContracts = handleBuiltin getSchema getContract where
    getSchema = \case
        Init     -> endpointsToSchemas @Empty                              -- Init has no schema and just Blockchain actions 
        Oracle _ -> endpointsToSchemas @(Oracle.OracleSchema .\\ BlockchainActions)
        Swap _   -> endpointsToSchemas @(Oracle.SwapSchema   .\\ BlockchainActions)
    getContract = \case
        Init        -> SomeBuiltin   initContract                          -- Init runs the initContract defined below 
        Oracle cs   -> SomeBuiltin $ Oracle.runOracle $ oracleParams cs    -- Oracle runs the runOracle contract and the oracleParams is defined above 
        Swap oracle -> SomeBuiltin $ Oracle.swap oracle                    -- Given an Oracle value will run the swap contract with this value as parameter

handlers :: SimulatorEffectHandlers (Builtin OracleContracts)              -- copy and pasted boiler plate code 
handlers =
    Simulator.mkSimulatorHandlers @(Builtin OracleContracts) []
    $ interpret handleOracleContracts

initContract :: Contract (Last CurrencySymbol) BlockchainActions Text ()   -- 
initContract = do
    ownPK <- pubKeyHash <$> ownPubKey  -- look up own pubkey 
    cur   <-
        mapError (pack . show)
        (Currency.forgeContract ownPK [(usdt, fromIntegral (length wallets) * amount)]     -- use forgeContract from Plutus Use Cases. Give each wallet USD 100.  Count how many wallets there are and multiply
        :: Contract (Last CurrencySymbol) BlockchainActions Currency.CurrencyError Currency.OneShotCurrency)
    let cs = Currency.currencySymbol cur    -- look up the currency symbol 
        v  = Value.singleton cs usdt amount -- provide the value of how much each wallet should get 
    forM_ wallets $ \w -> do                -- transactions to send each of the wallets the amount 
        let pkh = pubKeyHash $ walletPubKey w
        when (pkh /= ownPK) $ do            -- one wallet to mint all the USD and then send it to the other wallets 
            tx <- submitTx $ mustPayToPubKey pkh v
            awaitTxConfirmed $ txId tx
    tell $ Last $ Just cs                   -- tell the Currency Symbol that was the result of forging 
  where
    amount :: Integer
    amount = 100_000_000
