{-#LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B

data Place = Place { city :: String } deriving (Show, Generic)
data Product = Product { title :: String } deriving (Show, Generic)

instance FromJSON Place
instance FromJSON Product

placesJsonFile :: FilePath
placesJsonFile = "places.json"

productsJsonFile :: FilePath
productsJsonFile = "products.json"

getPlacesJSON :: IO B.ByteString
getPlacesJSON = B.readFile placesJsonFile

getProductsJSON :: IO B.ByteString
getProductsJSON = B.readFile productsJsonFile

main :: IO ()
main = do
  dpl <- (eitherDecode <$> getPlacesJSON) :: IO (Either String [Place])
  dpr <- (eitherDecode <$> getProductsJSON) :: IO (Either String [Product])
  case (dpl, dpr) of
    (Left e1, Left e2)     -> putStrLn (e1 ++ e2)
    (Left e1, _)           -> putStrLn e1
    (_, Left e2)           -> putStrLn e2
    (Right pls, Right prs) -> do
                                print pls
                                print prs