{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE FlexibleInstances #-}

module Main where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Test.QuickCheck
import Test.QuickCheck.Gen (Gen(unGen))
import Test.QuickCheck.Random (mkQCGen)

data Place = Place { city :: String } deriving (Eq, Show, Generic)
data Product = Product { title :: String } deriving (Eq, Show, Generic)

data Trip = Trip { fromCity    :: Place
                 , toCity      :: Place
                 , merchandise :: [(Product, Int)] } deriving (Show, Generic)

type Route    = [Trip]
type Places   = [Place]
type Products = [Product]

instance FromJSON Place
instance FromJSON Product
instance ToJSON Place
instance ToJSON Product
instance ToJSON Trip

placesJsonFile :: FilePath
placesJsonFile = "places.json"

productsJsonFile :: FilePath
productsJsonFile = "products.json"

testRouteJsonFile :: FilePath
testRouteJsonFile = "testRoute.json"

createTest :: Route -> IO ()
createTest = encodeFile testRouteJsonFile

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

generateRandomPlace :: Places -> Places -> Gen (Place, Places)
generateRandomPlace allPlaces alreadyPlaces = do
  let plsLen = length allPlaces
  newPlace <- elements allPlaces `suchThat` (`notElem` alreadyPlaces)
  return (newPlace, newPlace : alreadyPlaces)

generateNextTrip :: Places -> Products -> Places -> Trip -> Gen (Trip, Places)
generateNextTrip pls prs alreadyPlaces seedTrip = do
  let alreadyPlaces' = alreadyPlaces ++ [fromCity seedTrip, toCity seedTrip]
  (nextToCity, alreadyPlaces'') <- generateRandomPlace pls alreadyPlaces'
  merchPs <- suchThat (sublistOf prs) (not . null)
  merchQs <- listOf1 (choose (1, 420 :: Int))
  let merchs = zip merchPs merchQs
  let prevMerch = merchandise seedTrip
  merchs' <- oneof [suchThat (sublistOf prevMerch) (not . null), suchThat (sublistOf (prevMerch ++ merchs)) (not . null) ]
  let nextTrip = Trip { fromCity = toCity seedTrip, toCity = nextToCity, merchandise = merchs'}
  return (nextTrip, alreadyPlaces'')

generateStandardRoute :: Places -> Products -> Gen Route
generateStandardRoute pls prs = do
  seedTrip <- generateRandomTrip pls prs
  times <- choose (3, 10)
  helperRes <- generateStandardRoute' times [] seedTrip [seedTrip]
  return $ reverse $ fst helperRes
  where
    generateStandardRoute' :: Int -> Places -> Trip -> Route -> Gen (Route, Places)
    generateStandardRoute' 0 alreadyPlaces _ route            = return (route, alreadyPlaces)
    generateStandardRoute' n alreadyPlaces seedTrip prevRoute = do
      (nextTrip, alreadyPlaces') <- generateNextTrip pls prs alreadyPlaces seedTrip
      generateStandardRoute' (n-1) alreadyPlaces' nextTrip (nextTrip : prevRoute)

getAlreadyPlaces :: Route -> Places
getAlreadyPlaces = undefined 

generateActualRoute :: Places -> Products -> Route -> Gen Route
generateActualRoute pls prs stdRoute = do
  nAddTrips <- choose (2, 5 :: Int)
  tripAddIndices <- vectorOf nAddTrips (choose (0, length stdRoute - 1))
  return []
  where
    addTrips :: [Int] -> Route -> Route
    addTrips addIndices r = undefined

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