{-#LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B

data Place = Place { city :: String } deriving (Show, Generic)

instance FromJSON Place

placesJsonFile :: FilePath
placesJsonFile = "places.json"

getJSON :: IO B.ByteString
getJSON = B.readFile placesJsonFile

main :: IO ()
main = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Place])
  case d of
    Left err -> putStrLn err
    Right ps -> print ps