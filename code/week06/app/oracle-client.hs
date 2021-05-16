{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main
    ( main
    ) where

import Control.Concurrent
import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString        (ByteString)
import Data.ByteString.Char8  (unpack)
import Data.Proxy             (Proxy (..))
import Data.Text              (pack)
import Data.UUID
import Network.HTTP.Req -- need this to do web requests
import Text.Regex.TDFA

main :: IO ()
main = do
    uuid <- read <$> readFile "oracle.cid"                      -- read file to get UUID that has correct instance 
    putStrLn $ "oracle contract instance id: " ++ show uuid     -- logs it 
    go uuid Nothing                                             -- go loops forever.  Nothing denotes there is no exchange rate to begin with 
  where
    go :: UUID -> Maybe Integer -> IO a                         --
    go uuid m = do                                              -- m here is the old value of the exchange rate to check whether it has changed 
        x <- getExchangeRate                                    -- looks up ADAUSD exchange rate on coinmarketcap 
        let y = Just x
        when (m /= y) $                                         -- if exchange rate has changed it calls updateOracle endpoint on our contract 
            updateOracle uuid x
        threadDelay 5_000_000                                   -- waits for 5 seconds (should be 20 seconds since blocks appear on cardano every 20 seconds)
        go uuid y                                               -- loops 

updateOracle :: UUID -> Integer -> IO ()                        -- updateOracle demonstrates how to interact with a running contract 
updateOracle uuid x = runReq defaultHttpConfig $ do             -- utilizes the Network.HTTP.Req 
    v <- req                               
        POST                                                    -- prepare POST request 
        (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show uuid) /: "endpoint" /: "update")  -- goto local server/api/new...
        (ReqBodyJson x)                      -- value to be updated to 
        (Proxy :: Proxy (JsonResponse ()))   -- expect a Json format response 
        (port 8080)                          -- port number 
    liftIO $ putStrLn $ if responseStatusCode v == 200  -- if status code is 200 then log message accordingly 
        then "updated oracle to " ++ show x
        else "error updating oracle"

getExchangeRate :: IO Integer                                      -- function to get ADAUSD exchange rate from Coinmarketcap quick and dirty way instead of using their API 
getExchangeRate = runReq defaultHttpConfig $ do
    v <- req
        GET
        (https "coinmarketcap.com" /: "currencies" /: "cardano")
        NoReqBody
        bsResponse
        mempty
    let priceRegex      = "priceValue___11gHJ\">\\$([\\.0-9]*)" :: ByteString  -- user regular expression to grab the value for demo purposes 
        (_, _, _, [bs]) = responseBody v =~ priceRegex :: (ByteString, ByteString, ByteString, [ByteString])
        d               = read $ unpack bs :: Double
        x               = round $ 1_000_000 * d -- multiply the exchange rate by 1 million and round it 
    liftIO $ putStrLn $ "queried exchange rate: " ++ show d
    return x
