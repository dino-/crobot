{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Network.Exchange.Bittrex
   ( Balance (..)
   , BittrexCreds (..)
   , Order (..)
   , Ticker (..)
   , Uuid (..)

   , buyLimit
   , cancel
   , getBalance
   , getOpenOrders
   , getOrder
   , getTicker
   , sellLimit
   )
where

import Control.Monad.Except ( runExcept, throwError, unless )
import Crypto.Hash.SHA512 ( hmac )
import Data.Aeson ( FromJSON, Object, (.:), decode, genericParseJSON,
   parseJSON, withObject, withText )
import Data.Aeson.Types (Options (constructorTagModifier,
   fieldLabelModifier, tagSingleConstructors), camelTo2, defaultOptions )
import Data.ByteString.Base16 ( encode )
import Data.Char ( toUpper )
import qualified Data.HashMap.Lazy as H
import Data.Maybe ( fromJust )
import Data.String.Conv ( toS )
import Data.Text ( Text )
import Data.Time ( UTCTime, defaultTimeLocale, formatTime,
   getCurrentTime, iso8601DateFormat, parseTimeM )
--import Debug.Trace ( trace )
import GHC.Generics ( Generic )
import Network.Curl ( CurlOption (CurlHttpHeaders), curlGetString )
import Network.Curl.Code ( CurlCode (CurlOK) )
--import System.IO ( hPutStrLn, stderr )
import Text.Printf ( printf )

import Cryptocurrency.Types ( Amount (..), Quantity (..), Currency, Market (..) )


baseUri :: String
baseUri = "https://bittrex.com/api/v1.1"


generateNonce :: IO String
generateNonce = formatTime defaultTimeLocale "%s" <$> getCurrentTime


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


capFirstParseOptions :: Options
capFirstParseOptions = defaultOptions
   { fieldLabelModifier = \s -> (toUpper . head $ s) : tail s }


newtype Uuid = Uuid Text
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
   curlGetString uri [] >>= tryParse "Ticker"


data OrderType
   = LimitBuy
   | LimitSell
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
   { orderUuid :: Uuid
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
   parseJSON = withObject "Order" $ \v' -> do
      let v = fixOrderType v'
      Order
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


{- Nice job at Bittrex using two different key strings for order
   type in otherwise similar JSON data

   'OrderType' in market/getopenorders
   'Type' in account/getorder.

   Sloppy work!
-}
fixOrderType :: Object -> Object
fixOrderType o = maybe o
   (\v -> H.insert "OrderType" v (H.delete "Type" o))
   $ H.lookup "Type" o


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
   nonce <- generateNonce

   let uri = printf "%s/market/getopenorders?apikey=%s&nonce=%s" baseUri apiKey nonce
   curlGetString uri (signUri apiSecret uri) >>= tryParse "[Order]"


getOrder :: BittrexCreds -> Uuid -> IO (Either String Order)
getOrder (BittrexCreds apiKey apiSecret) (Uuid uuid) = do
   nonce <- generateNonce

   let uri = printf "%s/account/getorder?apikey=%s&nonce=%s&uuid=%s" baseUri apiKey nonce uuid
   curlGetString uri (signUri apiSecret uri) >>= tryParse "Order"


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
   nonce <- generateNonce

   let uri = printf "%s/account/getbalance?apikey=%s&nonce=%s&currency=%s"
         baseUri apiKey nonce currency'

   curlGetString uri (signUri apiSecret uri) >>= tryParse "Balance"


{- Internal type to parse a JSON object with a single Uuid key/value
   pair in it into just a Uuid data structure but without screwing up
   our existing instancing of Uuid above.
-}
newtype ObjUuid = ObjUuid { uuid' :: Uuid }
   deriving (Generic, Show)

instance FromJSON ObjUuid where
   parseJSON = withObject "Uuid" $ \v -> ObjUuid <$> v .: "uuid"


buyLimit :: BittrexCreds -> Market -> Quantity -> Amount -> IO (Either String Uuid)
buyLimit (BittrexCreds apiKey apiSecret) market (Quantity quantity') (Amount rate) = do
   nonce <- generateNonce

   let uri = printf "%s/market/buylimit?apikey=%s&nonce=%s&market=%s&quantity=%f&rate=%f"
         baseUri apiKey nonce (show market) quantity' rate

   -- Parsing this into the temporary ObjUuid data structure above
   ou <- curlGetString uri (signUri apiSecret uri) >>= tryParse "Uuid"
   return $ uuid' <$> ou


sellLimit :: BittrexCreds -> Market -> Quantity -> Amount -> IO (Either String Uuid)
sellLimit (BittrexCreds apiKey apiSecret) market (Quantity quantity') (Amount rate) = do
   nonce <- generateNonce

   let uri = printf "%s/market/selllimit?apikey=%s&nonce=%s&market=%s&quantity=%f&rate=%f"
         baseUri apiKey nonce (show market) quantity' rate

   -- Parsing this into the temporary ObjUuid data structure above
   ou <- curlGetString uri (signUri apiSecret uri) >>= tryParse "Uuid"
   return $ uuid' <$> ou


cancel :: BittrexCreds -> Uuid -> IO (Either String ())
cancel (BittrexCreds apiKey apiSecret) (Uuid uuid) = do
   nonce <- generateNonce

   let uri = printf "%s/market/cancel?apikey=%s&nonce=%s&uuid=%s"
         baseUri apiKey nonce uuid

   em <- curlGetString uri (signUri apiSecret uri) >>= tryParse "()"
   return $ either Left (const . Right $ ()) (em :: Either String ())


tryParse :: FromJSON f => String -> (CurlCode, String) -> IO (Either String f)
tryParse typeName (code, rawdoc) = do
   --print code
   --hPutStrLn stderr . toS $ rawdoc

   return . runExcept $ do
      unless (code == CurlOK) $
         throwError $ printf "curl call failed, code: %s, document returned:\n%s"
         (show code) ((toS rawdoc) :: String)
      br <- maybe (throwError $ "Unable to parse reply into a BittrexResponse " ++ typeName)
         return $ decode . toS $ rawdoc
      unless (success br) $
         throwError $ "API call unsuccessful, message:\n" ++ (toS . message $ br)
      return . fromJust . result $ br


signUri :: ApiSecret -> Uri -> [CurlOption]
signUri apiSecret uri = [CurlHttpHeaders ["apisign:" ++ (toS . encode $ sign)]]
   where sign = hmac (toS apiSecret) (toS uri)
