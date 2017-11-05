{-# LANGUAGE OverloadedStrings #-}

module Main where

--import Control.Concurrent ( threadDelay )
import Control.Monad ( when )
import Data.Either ( fromRight, isRight )
import System.Environment ( getEnv )
import System.FilePath ( (<.>), (</>) )
import Text.Printf ( printf )

import Network.Exchange.Bittrex


seconds2 :: Int
seconds2 = 2000000  -- 2s in microseconds


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
      printf "\ngetOrder (%s)\n" (show orderUuid')
      print =<< getOrder creds orderUuid'

   putStrLn "\ngetOrder FOO"
   print =<< getOrder creds (Uuid "FOO")


   -- WARNING: If the price in this test isn't low enough, this WILL buy some OMG with real money!
   {-
   let mktBTC_OMG = Market "BTC" "OMG"
   let quantity' = Quantity 1.0  -- Quantity of OMG About $6.45 on 2017-11-04
   let rate = Amount 0.0007
   printf "\nbuyLimit %s (%s) (%s)\n" (show mktBTC_OMG) (show quantity') (show rate)
   eOrderUuid'' <- buyLimit creds mktBTC_OMG quantity' rate
   print eOrderUuid''

   when (isRight eOrderUuid'') $ do
      let orderUuid'' = fromRight (Uuid "FOO") eOrderUuid''

      threadDelay seconds2
      printf "\ngetOrder (%s)\n" (show orderUuid'')
      print =<< getOrder creds orderUuid''

      threadDelay seconds2
      printf "\ncancel (%s)\n" (show orderUuid'')
      print =<< cancel creds orderUuid''
   -}


   -- WARNING: If the price in this test isn't high enough, this WILL sell some XZC with real money!
   {-
   let mktBTC_XZC = Market "BTC" "XZC"
   let quantity' = Quantity 1.0  -- Quantity of XZC
   let rate = Amount 0.002
   printf "\nsellLimit %s (%s) (%s)\n" (show mktBTC_XZC) (show quantity') (show rate)
   eOrderUuid'' <- sellLimit creds mktBTC_XZC quantity' rate
   print eOrderUuid''

   when (isRight eOrderUuid'') $ do
      let orderUuid'' = fromRight (Uuid "FOO") eOrderUuid''

      threadDelay seconds2
      printf "\ngetOrder (%s)\n" (show orderUuid'')
      print =<< getOrder creds orderUuid''

      threadDelay seconds2
      printf "\ncancel (%s)\n" (show orderUuid'')
      print =<< cancel creds orderUuid''
   -}
