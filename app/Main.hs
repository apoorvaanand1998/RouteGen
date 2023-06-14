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
                 , merchandise :: [(Product, Int)] } deriving (Eq, Show, Generic)

newtype Route  = Route { theRoute :: [Trip] } deriving (Eq, Show, Generic)
data RoutePair = RoutePair { standardRoute :: Route
                           , actualRoute   :: Route } deriving (Eq, Show, Generic)

instance FromJSON Place
instance FromJSON Product
instance ToJSON Place
instance ToJSON Product
instance ToJSON Trip
instance ToJSON Route
instance ToJSON RoutePair

placesJsonFile :: FilePath
placesJsonFile = "places.json"

productsJsonFile :: FilePath
productsJsonFile = "products.json"

testRouteJsonFile :: FilePath
testRouteJsonFile = "RoutePairs.json"

createRoutePairs :: [RoutePair] -> IO ()
createRoutePairs = encodeFile testRouteJsonFile

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
          m     = merchandise trip
          asIs  = return m :: Gen [(Product, Int)]
      merchPs <- suchThat (sublistOf prs) (not . null)
      merchQs <- listOf1 (choose (1, 420 :: Int))
      let extraM       = zip merchPs merchQs
          strictlyMore = return (m ++ extraM) :: Gen [(Product, Int)]
          someOfPrev   = suchThat (sublistOf m) (not . null)
          mix          = suchThat (sublistOf (m ++ extraM)) (not . null)
      poss <- oneof [asIs, strictlyMore, someOfPrev, mix]
      let trip' = trip { merchandise = poss }
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

main :: IO ()
main = do
  print "cabal run RouteGen -- <n - number of RoutePairs you want generated>"
  n <- getArgs
  let n' = read (head n) :: Int
  let seeds = [0..n']
  rps <- randomRoutePairs seeds
  createRoutePairs rps

{-
testRoute1 = Route [Trip {fromCity = Place {city = "ChennaiHAHA"}, toCity = Place {city = "Houten"}, merchandise = [(Product {title = "Green smoothie"},57),(Product {title = "Baking cake"},394),(Product {title = "Homemade bread"},84),(Product {title = "Healthy breakfast"},361),(Product {title = "Green beans"},89)]},Trip {fromCity = Place {city = "Houten"}, toCity = Place {city = "Bolnes"}, merchandise = [(Product {title = "Green smoothie"},57),(Product {title = "Homemade bread"},84),(Product {title = "Healthy breakfast"},361),(Product {title = "Brown eggs"},59),(Product {title = "Lemon and salt"},157)]},Trip {fromCity = Place {city = "Bolnes"}, toCity = Place {city = "Hoensbroek"}, merchandise = [(Product {title = "Homemade bread"},84),(Product {title = "Brown eggs"},59),(Product {title = "Lemon and salt"},157),(Product {title = "Sweet fresh stawberry"},90),(Product {title = "Hazelnut in black ceramic bowl"},392),(Product {title = "Legums"},392)]},Trip {fromCity = Place {city = "Hoensbroek"}, toCity = Place {city = "Wageningen"}, merchandise = [(Product {title = "Brown eggs"},59),(Product {title = "Lemon and salt"},157),(Product {title = "Hazelnut in black ceramic bowl"},392),(Product {title = "Legums"},392)]},Trip {fromCity = Place {city = "Wageningen"}, toCity = Place {city = "Kwintsheul"}, merchandise = [(Product {title = "Lemon and salt"},157),(Product {title = "Legums"},392)]},Trip {fromCity = Place {city = "Kwintsheul"}, toCity = Place {city = "Waalre"}, merchandise = [(Product {title = "Legums"},392)]},Trip {fromCity = Place {city = "Waalre"}, toCity = Place {city = "Brunssum"}, merchandise = [(Product {title = "Legums"},392)]},Trip {fromCity = Place {city = "Brunssum"}, toCity = Place {city = "Surhuisterveen"}, merchandise = [(Product {title = "Legums"},392)]}]
testRoute2 = Route [Trip {fromCity = Place {city = "Badhoevedorp"}, toCity = Place {city = "Landgraaf"}, merchandise = [(Product {title = "Brown eggs"},178),(Product {title = "Sweet fresh stawberry"},18),(Product {title = "Asparagus"},408),(Product {title = "Green smoothie"},255),(Product {title = "Raw legums"},62)]},Trip {fromCity = Place {city = "Landgraaf"}, toCity = Place {city = "Berghem"}, merchandise = [(Product {title = "Sweet fresh stawberry"},18),(Product {title = "Asparagus"},408),(Product {title = "Green smoothie"},255),(Product {title = "Raw legums"},62),(Product {title = "Green smoothie"},317),(Product {title = "Raw legums"},190),(Product {title = "Pesto with basil"},138)]},Trip {fromCity = Place {city = "Berghem"}, toCity = Place {city = "Rijsenhout"}, merchandise = [(Product {title = "Green smoothie"},255),(Product {title = "Raw legums"},62),(Product {title = "Pesto with basil"},138)]},Trip {fromCity = Place {city = "Rijsenhout"}, toCity = Place {city = "Zuid-Scharwoude"}, merchandise = [(Product {title = "Green smoothie"},255)]},Trip {fromCity = Place {city = "Zuid-Scharwoude"}, toCity = Place {city = "Amersfoort"}, merchandise = [(Product {title = "Green smoothie"},255)]},Trip {fromCity = Place {city = "Amersfoort"}, toCity = Place {city = "Heer"}, merchandise = [(Product {title = "Green smoothie"},255)]},Trip {fromCity = Place {city = "Heer"}, toCity = Place {city = "Huissen"}, merchandise = [(Product {title = "Green smoothie"},255),(Product {title = "Pesto with basil"},204)]},Trip {fromCity = Place {city = "Huissen"}, toCity = Place {city = "Nieuwegein"}, merchandise = [(Product {title = "Green smoothie"},169),(Product {title = "Raw legums"},411)]},Trip {fromCity = Place {city = "Nieuwegein"}, toCity = Place {city = "Goutum"}, merchandise = [(Product {title = "Green smoothie"},169),(Product {title = "Raw legums"},411)]},Trip {fromCity = Place {city = "Goutum"}, toCity = Place {city = "Kerkrade"}, merchandise = [(Product {title = "Raw legums"},411),(Product {title = "Raw legums"},385)]},Trip {fromCity = Place {city = "Kerkrade"}, toCity = Place {city = "Vleuten"}, merchandise = [(Product {title = "Raw legums"},385)]}]
testRoute3 = Route [Trip {fromCity = Place {city = "Zwolle"}, toCity = Place {city = "Uithoorn"}, merchandise = [(Product {title = "Sweet fresh stawberry"},19),(Product {title = "Asparagus"},420),(Product {title = "Green smoothie"},39),(Product {title = "Raw legums"},64)]},Trip {fromCity = Place {city = "Uithoorn"}, toCity = Place {city = "Utrecht"}, merchandise = [(Product {title = "Sweet fresh stawberry"},19),(Product {title = "Asparagus"},420),(Product {title = "Green smoothie"},39)]},Trip {fromCity = Place {city = "Utrecht"}, toCity = Place {city = "Lelystad"}, merchandise = [(Product {title = "Sweet fresh stawberry"},19),(Product {title = "Asparagus"},420),(Product {title = "Green smoothie"},39)]},Trip {fromCity = Place {city = "Lelystad"}, toCity = Place {city = "Tilburg"}, merchandise = [(Product {title = "Sweet fresh stawberry"},19),(Product {title = "Green smoothie"},39)]},Trip {fromCity = Place {city = "Tilburg"}, toCity = Place {city = "Empel"}, merchandise = [(Product {title = "Sweet fresh stawberry"},19),(Product {title = "Green smoothie"},39)]},Trip {fromCity = Place {city = "Empel"}, toCity = Place {city = "\8217s-Gravenzande"}, merchandise = [(Product {title = "Sweet fresh stawberry"},19),(Product {title = "Green smoothie"},39),(Product {title = "Hazelnut in black ceramic bowl"},21),(Product {title = "Fresh stawberry"},232),(Product {title = "Homemade bread"},408)]},Trip {fromCity = Place {city = "\8217s-Gravenzande"}, toCity = Place {city = "Nieuwegein"}, merchandise = [(Product {title = "Sweet fresh stawberry"},19),(Product {title = "Green smoothie"},39),(Product {title = "Hazelnut in black ceramic bowl"},21)]},Trip {fromCity = Place {city = "Nieuwegein"}, toCity = Place {city = "Weesp"}, merchandise = [(Product {title = "Sweet fresh stawberry"},292)]},Trip {fromCity = Place {city = "Weesp"}, toCity = Place {city = "Beek"}, merchandise = [(Product {title = "Sweet fresh stawberry"},292),(Product {title = "Brown eggs"},157)]}]

testRoutes = [testRoute1, testRoute2, testRoute3]
-}