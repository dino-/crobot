{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Cryptocurrency.Types
   ( Amount (..)
   , Quantity (..)
   , Currency
   , Market (..)
   )
where

import Data.Aeson ( FromJSON, parseJSON, withText )
import qualified Data.Text as T
import GHC.Generics ( Generic )
import Text.Printf ( printf )


type Currency = T.Text


data Market = Market Currency Currency
   deriving Generic

instance Show Market where
   show (Market cur1 cur2) = printf "%s-%s" cur1 cur2

instance FromJSON Market where
   parseJSON = withText "Market" $ \t -> return $ Market
      (T.takeWhile (/= '-') t)
      (T.tail . T.dropWhile (/= '-') $ t)


newtype Amount = Amount Float
   deriving (FromJSON, Generic, Show)


newtype Quantity = Quantity Float
   deriving Show
