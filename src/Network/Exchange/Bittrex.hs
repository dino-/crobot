{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Network.Exchange.Bittrex
   ( Balance (..)
   , BittrexCreds (..)
   , Currency
   , Market (..)
   , Order (..)
   , Ticker (..)

   , getBalance
   , getOpenOrders
   , getTicker
   )
where

import Control.Monad.Except ( runExcept, throwError, unless )
import Crypto.Hash.SHA512 ( hmac )
import Data.Aeson hiding ( encode )
import Data.Aeson.Types (Options (constructorTagModifier,
   fieldLabelModifier, tagSingleConstructors), camelTo2, defaultOptions )
import Data.ByteString.Base16 ( encode )
import Data.Char ( toUpper )
import Data.Maybe ( fromJust )
import Data.String.Conv ( toS )
import Data.Text hiding ( head, map, tail, toUpper )
import qualified Data.Text as T
import Data.Time ( UTCTime, defaultTimeLocale, formatTime,
   getCurrentTime, iso8601DateFormat, parseTimeM )
--import Debug.Trace ( trace )
import GHC.Generics ( Generic )
import Network.Curl ( CurlOption (CurlHttpHeaders), curlGetString )
import Network.Curl.Code ( CurlCode (CurlOK) )
--import System.IO ( hPutStrLn, stderr )
import Text.Printf ( printf )


baseUri :: String
baseUri = "https://bittrex.com/api/v1.1"


type Uri = String

type ApiSecret = Text


data BittrexCreds = BittrexCreds
   { bcredsApiKey :: ApiSecret
   , bcredsApiSecret :: ApiSecret
   }
   deriving (Read, Show)


data BittrexResponse a = BittrexResponse
   { success :: Bool
   , message :: Text
   , result :: Maybe a
   }
   deriving (Generic, Show)

instance FromJSON a => FromJSON (BittrexResponse a)


type Currency = Text


data Market = Market Currency Currency
   deriving Generic

instance Show Market where
   show (Market cur1 cur2) = printf "%s-%s" cur1 cur2

instance FromJSON Market where
   parseJSON = withText "Market" $ \t -> return $ Market
      (T.takeWhile (/= '-') t)
      (T.tail . T.dropWhile (/= '-') $ t)


capFirstParseOptions :: Options
capFirstParseOptions = defaultOptions
   { fieldLabelModifier = \s -> (toUpper . head $ s) : tail s }


newtype Amount = Amount Float
   deriving (FromJSON, Generic, Show)


data Ticker = Ticker
   { bid :: Amount
   , ask :: Amount
   , last :: Amount
   }
   deriving (Generic, Show)

instance FromJSON Ticker where
   parseJSON = genericParseJSON capFirstParseOptions


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

getTicker :: Market -> IO (Either String Ticker)
getTicker market = do
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


data OrderType
   = LimitSell
   deriving (Generic, Show)

instance FromJSON OrderType where
   parseJSON = genericParseJSON $ defaultOptions
      { constructorTagModifier = map toUpper . camelTo2 '_'
      , tagSingleConstructors = True
      }


newtype Timestamp = Timestamp UTCTime
   deriving (Generic, Show)

instance FromJSON Timestamp where
   parseJSON = withText "UTCTime" $ \t -> Timestamp <$> (parseTimeM True
      defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S%Q") . toS $ t)


data Order = Order
   { orderUuid :: Text
   , exchange :: Market
   , orderType :: OrderType
   , quantity :: Amount
   , quantityRemaining :: Amount
   , limit :: Amount
   , commissionPaid :: Amount
   , price :: Amount
   , pricePerUnit :: Maybe Amount
   , opened :: Timestamp
   , closed :: Maybe Timestamp
   , cancelInitiated :: Bool
   , immediateOrCancel :: Bool
   }
   deriving (Generic, Show)

instance FromJSON Order where
   parseJSON = withObject "Order" $ \v -> Order
      <$> v .: "OrderUuid"
      <*> v .: "Exchange"
      <*> v .: "OrderType"
      <*> v .: "Quantity"
      <*> v .: "QuantityRemaining"
      <*> v .: "Limit"
      <*> v .: "CommissionPaid"
      <*> v .: "Price"
      <*> v .: "PricePerUnit"
      <*> v .: "Opened"
      <*> v .: "Closed"
      <*> v .: "CancelInitiated"
      <*> v .: "ImmediateOrCancel"


--type Orders = [Order]


{- Example market/getopenorders response document

   {
       "success": true,
       "message": "",
       "result": [
           {
               "Uuid": null,
               "OrderUuid": "464a254c-9ecf-47fd-901c-020dd37ad003",
               "Exchange": "BTC-LGD",
               "OrderType": "LIMIT_SELL",
               "Quantity": 89.24166776,
               "QuantityRemaining": 89.24166776,
               "Limit": 0.00046335,
               "CommissionPaid": 0.00000000,
               "Price": 0.00000000,
               "PricePerUnit": null,
               "Opened": "2017-10-11T20:47:19.5",
               "Closed": null,
               "CancelInitiated": false,
               "ImmediateOrCancel": false,
               "IsConditional": false,
               "Condition": "NONE",
               "ConditionTarget": null
           },
           {
               "Uuid": null,
               "OrderUuid": "fc872c93-5f0e-4060-9a96-60fa0e0cb41d",
               "Exchange": "BTC-NEO",
               "OrderType": "LIMIT_SELL",
               "Quantity": 10.90780889,
               "QuantityRemaining": 10.90780889,
               "Limit": 0.00800000,
               "CommissionPaid": 0.00000000,
               "Price": 0.00000000,
               "PricePerUnit": null,
               "Opened": "2017-10-24T03:39:55.16",
               "Closed": null,
               "CancelInitiated": false,
               "ImmediateOrCancel": false,
               "IsConditional": false,
               "Condition": "NONE",
               "ConditionTarget": null
           }
       ]
   }
-}

getOpenOrders :: BittrexCreds -> IO (Either String [Order])
getOpenOrders (BittrexCreds apiKey apiSecret) = do
   nonce <- formatTime defaultTimeLocale "%s" <$> getCurrentTime

   let uri = printf "%s/market/getopenorders?apikey=%s&nonce=%s" baseUri apiKey nonce
   (code, rawdoc) <- curlGetString uri $ signUri apiSecret uri
   --print code
   --hPutStrLn stderr . toS $ rawdoc

   return . runExcept $ do
      unless (code == CurlOK) $
         throwError $ printf "curl call failed, code: %s, document returned:\n%s"
         (show code) ((toS rawdoc) :: String)
      br <- maybe (throwError "Unable to parse reply into a BittrexResponse [Order]")
         return $ decode . toS $ rawdoc
      unless (success br) $
         throwError $ "API call unsuccessful, message:\n" ++ (toS . message $ br)
      return . fromJust . result $ br


{- Example account/getbalance response document

   {
       "success": true,
       "message": "",
       "result": {
           "Currency": "BCC",
           "Balance": 0.98673048,
           "Available": 0.98673048,
           "Pending": 0.00000000,
           "CryptoAddress": "1Mblahblahblah"
       }
   }

-}


data Balance = Balance
   { currency :: Currency
   , balance :: Amount
   , available :: Amount
   , pending :: Amount
   , cryptoAddress :: Text
   }
   deriving (Generic, Show)

instance FromJSON Balance where
   parseJSON = genericParseJSON capFirstParseOptions


getBalance :: BittrexCreds -> Currency -> IO (Either String Balance)
getBalance (BittrexCreds apiKey apiSecret) currency' = do
   nonce <- formatTime defaultTimeLocale "%s" <$> getCurrentTime

   let uri = printf "%s/account/getbalance?apikey=%s&nonce=%s&currency=%s"
         baseUri apiKey nonce currency'
   (code, rawdoc) <- curlGetString uri $ signUri apiSecret uri
   --print code
   --hPutStrLn stderr . toS $ rawdoc

   return . runExcept $ do
      unless (code == CurlOK) $
         throwError $ printf "curl call failed, code: %s, document returned:\n%s"
         (show code) ((toS rawdoc) :: String)
      br <- maybe (throwError "Unable to parse reply into a BittrexResponse Balance")
         return $ decode . toS $ rawdoc
      unless (success br) $
         throwError $ "API call unsuccessful, message:\n" ++ (toS . message $ br)
      return . fromJust . result $ br


signUri :: ApiSecret -> Uri -> [CurlOption]
signUri apiSecret uri = [CurlHttpHeaders ["apisign:" ++ (toS . encode $ sign)]]
   where sign = hmac (toS apiSecret) (toS uri)
