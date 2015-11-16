module GameOfLife where

import Control.MonadPlus
import Data.Function
import Data.Lens
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe.Unsafe (fromJust)
import Data.Monoid
import Data.Monoid.Endo
import Data.List
import Data.Foldable
import Data.String (fromCharArray)
import Prelude

newtype Location = Location {x :: Int , y :: Int}

instance eqLocation :: Eq Location where
  eq (Location a) (Location b) = a.x == b.x && a.y == b.y

instance ordLocation :: Ord Location where
  compare (Location a) (Location b)
    | a.x == b.x = compare a.y b.y
    | otherwise  = compare a.x b.x

newtype World    = World (List Location)

data Max a = Max a

runMax :: forall a. Max a -> a
runMax (Max a) = a

instance semigroupMax :: (Ord a) => Semigroup (Max a) where
  append (Max a) (Max b) = if a >= b then Max a else Max b

data Min a = Min a

runMin :: forall a. Min a -> a
runMin (Min a) = a

instance semigroupMin :: (Ord a) => Semigroup (Min a) where
  append (Min a) (Min b) = if a <= b then Min a else Min b

folds :: forall a f. (Semigroup a, Foldable f) => a -> f a -> a
folds = foldl (<>)

_x :: LensP Location Int
_x = lens (\(Location l) -> l.x) (\(Location l) newX -> Location (l { x=newX }))

_y :: LensP Location Int
_y = lens (\(Location l) -> l.y) (\(Location l) newY -> Location (l { y=newY }))

_locations :: IsoP World (List Location)
_locations = iso (\(World l) -> l) World

emptyWorld :: World
emptyWorld = World mempty

loc :: Int -> Int -> Location
loc x y = Location {x: x, y: y}

testWorld :: World
testWorld = World (toList [loc 1 1, loc 1 2, loc 2 2, loc 3 3, loc 4 4])

threeInARow :: World
threeInARow = World (toList [loc 1 1, loc 1 2, loc 1 3])

instance showLocation :: Show Location where
  show (Location l) = "x: " ++ show l.x ++ ", y: " ++ show l.y

instance showWorld :: Show World where
  show world = intercalate "\n" (prettyWorld world)

prettyWorld :: World -> List String
prettyWorld w =
  map (fromCharArray <<< fromList) (updater empty)
  where
    updater = runEndo $ foldMap (\l ->
      setLocation
        ((l ^. _x) - minX)
        ((l ^. _y) - minY))
      (w ^. _locations)
    empty = replicate (maxX - minX + 1) (replicate (maxY - minY + 1) '.')
    onLocations selector = w ^.. _locations <<< traversed <<< selector
    maxX = runMax $ folds (Max 0) (Max <$> onLocations _x)
    maxY = runMax $ folds (Max 0) (Max <$> onLocations _y)
    minX = runMin $ folds (Min 1000) (Min <$> onLocations _x)
    minY = runMin $ folds (Min 1000) (Min <$> onLocations _y)

    setLocation :: Int -> Int -> Endo (List (List Char))
    setLocation x y = Endo (\xss -> fromJust $ modifyAt x (fromJust <<< updateAt y 'X') xss)

evolve :: World -> World
evolve (World locations) = World newLocations
  where
    newLocations = rule locations (allNeighbours locations)

rule :: List Location -> List (List Location) -> List Location
rule w lss = map (fromJust <<< head) $ filter (\ls ->
  length ls == 3 ||
  (length ls == 2 && elem (fromJust (head ls)) w)) lss

allNeighbours :: List Location -> List (List Location)
allNeighbours = group <<< sort <<< concatMap neighbours

neighbours :: Location -> List Location
neighbours (Location l) = do
  x <- -1..1
  y <- -1..1
  guard (not (x == 0 && y == 0))
  return (loc (l.x + x) (l.y + y))


-- testUtil

stringifyWorld :: List String -> String
stringifyWorld = foldl (\b a -> b ++ "\n" ++ a) ""
