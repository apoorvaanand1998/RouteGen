{-#LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Test.QuickCheck 
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

getPlacesAndProducts :: IO (Places, Products)
getPlacesAndProducts = do
  dpl <- (eitherDecode <$> getPlacesJSON) :: IO (Either String [Place])
  dpr <- (eitherDecode <$> getProductsJSON) :: IO (Either String [Product])
  case (dpl, dpr) of
    (Left e1, Left e2)     -> error (e1 ++ e2)
    (Left e1, _)           -> error e1
    (_, Left e2)           -> error e2
    (Right pls, Right prs) -> return (pls, prs)

generateRandomTrip :: Places -> Products -> Gen Trip
generateRandomTrip pls prs = do
  let plsLen = length pls
  fromIdx <- choose (0, plsLen-1)
  toIdx   <- suchThat (choose (0, plsLen-1)) (/= fromIdx)
  merchPs <- suchThat (sublistOf prs) (not . null)
  merchQs <- listOf1 (choose (1, 420 :: Int))
  let merchs = zip merchPs merchQs
  let randomTrip = Trip { fromCity = pls !! fromIdx, toCity = pls !! toIdx, merchandise = merchs}
  return randomTrip

generateStandardRoute :: Places -> Products -> Gen Route
generateStandardRoute pls prs = suchThat (listOf1 (generateRandomTrip pls prs)) sameToFromPred
  where
    sameToFromPred :: Route -> Bool
    sameToFromPred []           = True
    sameToFromPred [x]          = True
    sameToFromPred (x : y : xs) = 
      let
        toOfX   = city $ toCity x
        fromOfY = city $ fromCity y
      in
        toOfX == fromOfY && sameToFromPred (y : xs)

randomStandardRoute :: Int -> IO Route
randomStandardRoute seed = do
  (pls, prs) <- getPlacesAndProducts
  return $ unGen (generateStandardRoute pls prs) (mkQCGen seed) 5

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