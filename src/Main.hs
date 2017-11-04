{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad ( when )
import Data.Either ( fromRight, isRight )
import Data.String.Conv ( toS )
import System.Environment ( getEnv )
import System.FilePath ( (<.>), (</>) )
import Text.Printf ( printf )

import Network.Exchange.Bittrex


main :: IO ()
main = do
   creds <- do
      home <- getEnv "HOME"
      let path = home </> ".config" </> "crobot" </> "bittrex" <.> "creds"
      read <$> readFile path


   let mktBTC_LTC = Market "BTC" "LTC"
   printf "\ngetTicker for %s\n" (show mktBTC_LTC)
   print =<< getTicker mktBTC_LTC

   let mktFOO_BAR = Market "FOO" "BAR"
   printf "\ngetTicker for %s\n" (show mktFOO_BAR)
   print =<< getTicker mktFOO_BAR

   putStrLn "\ngetOpenOrders"
   orders <- getOpenOrders creds
   print orders


   putStrLn "\ngetBalance BCC"
   print =<< getBalance creds "BCC"

   putStrLn "\ngetBalance FOO"
   print =<< getBalance creds "FOO"


   when (isRight orders) $ do
      let orderUuid' = orderUuid . head . fromRight [] $ orders
      putStrLn $ "\ngetOrder " ++ (toS orderUuid')
      print =<< getOrder creds orderUuid'

   putStrLn "\ngetOrder FOO"
   print =<< getOrder creds "FOO"
