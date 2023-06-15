{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE FlexibleInstances #-}

module Main where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Test.QuickCheck
import Test.QuickCheck.Gen (Gen(unGen))
import Test.QuickCheck.Random (mkQCGen)
import System.Environment

newtype Place = Place { city :: String } deriving (Eq, Show, Generic)
newtype Product = Product { title :: String } deriving (Eq, Show, Generic)
type Places   = [Place]
type Products = [Product]

data Trip = Trip { fromCity    :: Place
                 , toCity      :: Place
                 , tmerchandise :: [(Product, Int)] } deriving (Eq, Show, Generic)

newtype Route  = Route { theRoute :: [Trip] } deriving (Eq, Show, Generic)
data RoutePair = RoutePair { standardRoute :: Route
                           , actualRoute   :: Route } deriving (Eq, Show, Generic)

data SimpleRoutePair = SimpleRoutePair { stdRoute :: SimpleRoute
                                       , actRoute :: SimpleRoute } deriving (Eq, Show, Generic)

newtype SimpleRoute = SimpleRoute { sroute :: [SimpleTrip]} deriving (Eq, Show, Generic)

data SimpleTrip = SimpleTrip { from :: String
                             , to   :: String
                             , merchandise :: [(String, Int)] } deriving (Eq, Show, Generic)

data ProjectRoute = ProjectRoute { id    :: Integer
                                 , route :: [SimpleTrip] } deriving (Eq, Show, Generic)

instance FromJSON Place
instance FromJSON Product
instance ToJSON Place
instance ToJSON Product
instance ToJSON Trip
instance ToJSON Route
instance ToJSON RoutePair
instance ToJSON SimpleRoutePair
instance ToJSON SimpleRoute 
instance ToJSON SimpleTrip
instance ToJSON ProjectRoute

placesJsonFile :: FilePath
placesJsonFile = "places.json"

productsJsonFile :: FilePath
productsJsonFile = "products.json"

standardJsonFile :: String -> FilePath
standardJsonFile suffix = "RoutePairs/standard" ++ suffix ++ ".json"

actualJsonFile :: String -> FilePath
actualJsonFile suffix = "RoutePairs/actual" ++ suffix ++ ".json"

createStd :: String -> [ProjectRoute] -> IO ()
createStd s = encodeFile (standardJsonFile s)

createAct :: String -> [ProjectRoute] -> IO ()
createAct s = encodeFile (actualJsonFile s)

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
  let randomTrip = Trip { fromCity = pls !! fromIdx, toCity = pls !! toIdx, tmerchandise = merchs}
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
  let prevMerch = tmerchandise seedTrip
  merchs' <- oneof [suchThat (sublistOf prevMerch) (not . null), suchThat (sublistOf (prevMerch ++ merchs)) (not . null) ]
  let nextTrip = Trip { fromCity = toCity seedTrip, toCity = nextToCity, tmerchandise = merchs'}
  return (nextTrip, alreadyPlaces'')

generateStandardRoute :: Places -> Products -> Gen Route
generateStandardRoute pls prs = do
  seedTrip <- generateRandomTrip pls prs
  times <- choose (3, 10)
  helperRes <- generateStandardRoute' times [] seedTrip (Route [seedTrip])
  return $ Route (reverse $ theRoute $ fst helperRes)
  where
    generateStandardRoute' :: Int -> Places -> Trip -> Route -> Gen (Route, Places)
    generateStandardRoute' 0 alreadyPlaces _ route            = return (route, alreadyPlaces)
    generateStandardRoute' n alreadyPlaces seedTrip prevRoute = do
      (nextTrip, alreadyPlaces') <- generateNextTrip pls prs alreadyPlaces seedTrip
      generateStandardRoute' (n-1) alreadyPlaces' nextTrip (Route (nextTrip : theRoute prevRoute))

getAlreadyPlaces :: Route -> Places
getAlreadyPlaces r = foldr (\ x -> (++) [fromCity x, toCity x]) [] (theRoute r)

generateActualRoute :: Places -> Products -> Route -> Gen Route
generateActualRoute pls prs stdRoute = do
  n <- choose (2, 5 :: Int)
  merchChangeIs <- vectorOf n (choose (0, length (theRoute stdRoute) - 1))
  merchChangedRoute <- changeMerch merchChangeIs stdRoute
  tripChangeIs <- vectorOf n (choose (0, length (theRoute merchChangedRoute) - 3))
  tripChangedRoute <- changeTrip tripChangeIs merchChangedRoute
  tripAddIs <- vectorOf n (choose (0, length (theRoute tripChangedRoute ) - 2))
  addTrips tripAddIs tripChangedRoute
  where
    insertAt :: Int -> Trip -> [Trip] -> [Trip]
    insertAt i t before = take i before ++ t : drop i before

    changeAt :: Int -> Trip -> [Trip] -> [Trip]
    changeAt i t before = take i before ++ t : drop (i+1) before

    addTrips :: [Int] -> Route -> Gen Route
    addTrips []       r = return r
    addTrips (x : xs) r = do
      let route      = theRoute r
          tripSeeded = route !! x
      (newTrip, alreadyPlaces) <- generateNextTrip pls prs (getAlreadyPlaces r) tripSeeded
      let route'  = insertAt (x+1) newTrip route
          trip'   = route' !! (x+2)
          trip''  = trip' { fromCity = toCity newTrip }
          route'' = changeAt (x+2) trip'' route'
      return $ Route route''

    changeMerch :: [Int] -> Route -> Gen Route
    changeMerch []       r = return r
    changeMerch (x : xs) r = do
      let trip  = theRoute r !! x
          m     = tmerchandise trip
          asIs  = return m :: Gen [(Product, Int)]
      merchPs <- suchThat (sublistOf prs) (not . null)
      merchQs <- listOf1 (choose (1, 420 :: Int))
      let extraM       = zip merchPs merchQs
          strictlyMore = return (m ++ extraM) :: Gen [(Product, Int)]
          someOfPrev   = suchThat (sublistOf m) (not . null)
          mix          = suchThat (sublistOf (m ++ extraM)) (not . null)
      poss <- oneof [asIs, strictlyMore, someOfPrev, mix]
      let trip' = trip { tmerchandise = poss }
          r'    = Route $ changeAt x trip' (theRoute r)
      changeMerch xs r'

    changeTrip :: [Int] -> Route -> Gen Route
    changeTrip [] r       = return r
    changeTrip (x : xs) r = do
      let alreadyPlaces = getAlreadyPlaces r
      (newPlace, _) <- generateRandomPlace pls alreadyPlaces
      let trips     = theRoute r
          currTrip  = trips !! x
          nextTrip  = trips !! (x+1)
          currTrip' = currTrip { toCity = newPlace }
          nextTrip' = nextTrip { fromCity = newPlace }
          trips'    = changeAt x currTrip' trips
          trips''   = changeAt (x+1) nextTrip' trips'
      changeTrip xs (Route trips'')

generateRoutePair :: Places -> Products -> Gen RoutePair
generateRoutePair pls prs = do
  stdRoute <- generateStandardRoute pls prs
  actRoute <- generateActualRoute pls prs stdRoute
  return $ RoutePair stdRoute actRoute

randomRoutePairs :: [Int] -> IO [RoutePair]
randomRoutePairs []       = return []
randomRoutePairs (x : xs) = do
  (pls, prs) <- getPlacesAndProducts
  rest <- randomRoutePairs xs
  return $ unGen (generateRoutePair pls prs) (mkQCGen x) 5 : rest

convertRP :: RoutePair -> SimpleRoutePair
convertRP rp = 
  let
    sr = standardRoute rp
    ar = actualRoute rp
    ts = theRoute sr
    ta = theRoute ar

    tToSt :: Trip -> SimpleTrip
    tToSt (Trip f t m) = SimpleTrip (city f) (city t) (map g m)

    g :: (Product, Int) -> (String, Int)
    g (p, i) = (title p, i)

    sts = SimpleRoute $ map tToSt ts
    sta = SimpleRoute $ map tToSt ta
  in
    SimpleRoutePair sts sta
  
convertSRP :: Integer -> [SimpleRoutePair] -> ([ProjectRoute], [ProjectRoute])
convertSRP _ []             = ([], [])
convertSRP startId (x : xs) = 
  let
    spr    = sroute $ stdRoute x
    apr    = sroute $ actRoute x
    (f, s) = convertSRP (startId+1) xs
    currF  = ProjectRoute startId spr
    currS  = ProjectRoute startId apr
  in
    (currF : f, currS : s)

main :: IO ()
main = do
  print "cabal run RouteGen -- <startSeed endSeed filePathSuffix>"
  (startSeed : endSeed : filePathSuffix : _) <- getArgs
  let i = read startSeed :: Int
      j = read endSeed   :: Int
      k = read filePathSuffix :: Integer
  let seeds = [i..j]
  rps <- randomRoutePairs seeds
  let srps         = map convertRP rps
      (stds, acts) = convertSRP k srps
  createStd filePathSuffix stds
  createAct filePathSuffix acts