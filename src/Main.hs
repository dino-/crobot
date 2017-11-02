{-# LANGUAGE OverloadedStrings #-}

module Main where

--import Text.Printf ( printf )

import Network.Exchange.Bittrex


main :: IO ()
main = do
   {-
   creds <- ((read <$> readFile "/home/dino/.config/crobot/bittrex.creds") :: IO BittrexCreds)
   print creds
   -}

   resp <- market_getTicker $ Market "BTC" "LTC"
   --resp <- market_getTicker $ Market "BTC" "FOO"

   putStrLn $ "Got response: " ++ (show resp)
