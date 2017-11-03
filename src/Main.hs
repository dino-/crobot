{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Printf ( printf )

import Network.Exchange.Bittrex


main :: IO ()
main = do
   {-
   creds <- ((read <$> readFile "/home/dino/.config/crobot/bittrex.creds") :: IO BittrexCreds)
   print creds
   -}

   let mktBTC_LTC = Market "BTC" "LTC"
   printf "\nmarket_getTicker for %s\n" (show mktBTC_LTC)
   print =<< getTicker mktBTC_LTC

   let mktFOO_BAR = Market "FOO" "BAR"
   printf "\nmarket_getTicker for %s\n" (show mktFOO_BAR)
   print =<< getTicker mktFOO_BAR
