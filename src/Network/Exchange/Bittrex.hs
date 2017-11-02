{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Network.Exchange.Bittrex
   ( BittrexCreds (..)
   , Market (..)
   , Ticker (..)

   , market_getTicker
   )
where

import Control.Monad.Except ( runExcept, throwError, unless )
import Crypto.Hash.SHA512 ( hmac )
import Data.Aeson hiding ( encode )
import Data.Aeson.Types ( Options (fieldLabelModifier), defaultOptions, typeMismatch )
import Data.ByteString.Base16 ( encode )
import Data.Char ( toLower, toUpper )
import Data.Maybe ( fromJust )
import Data.String.Conv ( toS )
import Data.Text hiding ( head, map, tail, toLower, toUpper )
--import Data.Time ( defaultTimeLocale, formatTime, getCurrentTime )
import GHC.Generics ( Generic )
import Network.Curl ( CurlOption (CurlHttpHeaders), curlGetString )
import Network.Curl.Code ( CurlCode (CurlOK) )
--import System.IO ( hPutStrLn, stderr )
import Text.Printf ( printf )


baseUri :: String
baseUri = "https://bittrex.com/api/v1.1"


type Uri = String

type ApiSecret = String


data BittrexCreds = BittrexCreds
   { bcredsApiKey :: Text
   , bcredsApiSecret :: Text
   }
   deriving (Read, Show)


data BittrexResponse a = BittrexResponse
   { success :: Bool
   , message :: Text
   , result :: Maybe a
   }
   deriving (Generic, Show)

instance FromJSON a => FromJSON (BittrexResponse a)


data Market = Market Text Text

instance Show Market where
   show (Market cur1 cur2) = printf "%s-%s" cur1 cur2


newtype Amount = Amount Float
   deriving (FromJSON, Generic, Show)


data Ticker = Ticker
   { bid :: Amount
   , ask :: Amount
   , last :: Amount
   }
   deriving (Generic, Show)

instance FromJSON Ticker where
   parseJSON o@(Object _) = genericParseJSON tickerParseOptions o
      where
         capFirst s = (toUpper . head $ s) : (map toLower . tail $ s)
         tickerParseOptions = defaultOptions { fieldLabelModifier = capFirst }

   parseJSON invalid = typeMismatch "Ticker" invalid


{- Example public/getticker response document

   { "success" : true
   , "message" : ""
   , "result" :
      { "Bid" : 0.00731900
      , "Ask" : 0.00735317
      , "Last" : 0.00735317
      }
   }

   { "success" : false
   , "message" : "INVALID_MARKET"
   , "result" : null
   }

-}

market_getTicker :: Market -> IO (Either String Ticker)
market_getTicker market = do
   let uri = printf "%s/public/getticker?market=%s" baseUri (show market)
   (code, rawdoc) <- curlGetString uri []
   --print code
   --print rawdoc

   return . runExcept $ do
      unless (code == CurlOK) $
         throwError $ printf "curl call failed, code: %s, document returned:\n%s"
         (show code) ((toS rawdoc) :: String)
      br <- maybe (throwError "Unable to parse reply into a BittrexResponse Ticker")
         return $ decode . toS $ rawdoc
      unless (success br) $
         throwError $ "API call unsuccessful, message:\n" ++ (toS . message $ br)
      return . fromJust . result $ br


{-
main :: IO ()
main = do
   nonce <- formatTime defaultTimeLocale "%s" <$> getCurrentTime

   --let uri = printf "https://bittrex.com/api/v1.1/market/getopenorders?apikey=%s&nonce=%s" apiKey nonce
   let uri = printf "https://bittrex.com/api/v1.1/public/getticker?market=BTC-LTC"
   --let uri = printf "https://bittrex.com/api/v1.1/account/getbalance?apikey=%s&nonce=%s&currency=BCC" apiKey nonce

   resp <- curlGetString uri $ signUri apiSecret uri

   hPutStrLn stderr $ "Got response: " ++ (show . fst $ resp)
   putStrLn . snd $ resp
-}


-- Assuming the API secret is coming from something monadic, possibly impure, so pass it in
signUri :: ApiSecret -> Uri -> [CurlOption]
signUri apiSecret uri = [CurlHttpHeaders ["apisign:" ++ (toS . encode $ sign)]]
   where sign = hmac (toS apiSecret) (toS uri)
