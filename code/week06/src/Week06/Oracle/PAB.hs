{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}

module Week06.Oracle.PAB        -- PAB (Plutus Application Backend) takes all the code and turns it into an executable to actually run the contracts. PAB gets deployed onto testnet or mainnet 
    ( OracleContracts (..)      -- for the exercise we deploy to a simulated blockchain, but the process is the same for testnet / mainnet 
    ) where

import           Data.Aeson                (FromJSON, ToJSON)       -- 
import           Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import           GHC.Generics              (Generic)
import           Ledger

import qualified Week06.Oracle.Core        as Oracle

data OracleContracts = Init | Oracle CurrencySymbol | Swap Oracle.Oracle   -- define the datatypes of the contracts that we eventually want to run 
                                                                           -- Init setup the initial funds in the wallets
                                                                           -- Oracle runs Oracle contracts with parameter of CurrencySumbol for the USDT Token
                                                                           -- Swap parameterized by Oracle will be used to run the Swap contracts 
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance Pretty OracleContracts where
    pretty = viaShow
