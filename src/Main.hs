{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Printf ( printf )

import Network.Exchange.Bittrex


main :: IO ()
main = do
   let mktBTC_LTC = Market "BTC" "LTC"
   printf "\ngetTicker for %s\n" (show mktBTC_LTC)
   print =<< getTicker mktBTC_LTC

   let mktFOO_BAR = Market "FOO" "BAR"
   printf "\ngetTicker for %s\n" (show mktFOO_BAR)
   print =<< getTicker mktFOO_BAR

   creds <- ((read <$> readFile "/home/dino/.config/crobot/bittrex.creds") :: IO BittrexCreds)

   putStrLn "\ngetOpenOrders"
   print =<< getOpenOrders creds
