{-#LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Test.QuickCheck 
import System.Random
import Test.QuickCheck.Gen (Gen(unGen))
import Test.QuickCheck.Random (mkQCGen)

data Place = Place { city :: String } deriving (Show, Generic)
data Product = Product { title :: String } deriving (Show, Generic)

data Trip = Trip { fromCity    :: Place
                 , toCity      :: Place
                 , merchandise :: [(Product, Int)] } deriving Show

type Route    = [Trip]
type Places   = [Place]
type Products = [Product]

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

createRandomTrip :: Places -> Products -> Gen Trip
createRandomTrip pls prs = do
  let plsLen = length pls
  fromIdx <- choose (0, plsLen-1)
  toIdx   <- choose (0, plsLen-1)
  let randomTrip =  Trip { fromCity = pls !! fromIdx, toCity = pls !! toIdx, merchandise = []}
  return randomTrip

randomTrip :: Trip
randomTrip = unGen (createRandomTrip testPlaces testProducts) (mkQCGen 3) 1

testPlaces :: Places
testPlaces = [Place "Utrecht", Place "Amsterdam", Place "Arnheim", Place "Amersfoort"]

testProducts :: Products
testProducts = [Product "Milk", Product "Eggs", Product "Meat", Product "Cheese"]

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