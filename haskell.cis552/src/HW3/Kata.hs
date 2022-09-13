{-
Return of the Data Munging Kata
===============================
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HW3.Kata where

-- Lists that are NonEmpty, see below

import Data.Char
import qualified Data.Char as Char
import Data.Foldable (maximumBy)
import qualified Data.Foldable as Foldable
import Data.Function (on)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Maybe as Maybe
import System.IO
import Test.HUnit (Test (TestList), (~?=))
import Text.Read (readMaybe)

data Weather = Weather
  { day :: Day,
    maxTemp :: Int,
    minTemp :: Int,
    dep :: Int,
    precipitation :: Prec,
    events :: Events
  }
  deriving (Eq, Show)

newtype Day = Day Int deriving (Eq, Show)

newtype Prec = Prec Bool deriving (Eq, Show)

{-
Using the library
-----------------

The main operation of this library is the function `parse`, defined below, of type

      parse :: ParseRecord a => String -> Maybe (NonEmpty a)

This function parses the input file into a non-empty list data records. In the case of
weather files, these records will be of type `Weather`, representing the weather data
in each row of the input. The parse function needs extra information to figure out how
to interpret the input string. The first line of input should be a *header row* that
specifies the columns of the file, separated by commas. Then each row
of the file can be split up into columns and parsed into the data model, using the
overloaded `parseRecord` function for that type.

For example, for the `Weather` type, the definition looks like this:
-}

-- | Parse weather information from rows of tabulated strings
instance ParseRecord Weather where
  parseRecord :: [(String, String)] -> Maybe Weather
  parseRecord row = Just Weather <**> row !? "DY" <**> row !? "MAX" <**> row !? "MIN" <**> row !? "DEP" <**> row !? "WTR" <**> row !? "WX"

(!?) :: [(String, String)] -> String -> Maybe String
row !? name = List.lookup name row

instance ParseField Day where
  parseField :: String -> Maybe Day
  parseField str = case readMaybe str of
    Nothing -> Nothing
    Just x -> if x < 0 || x > 31 then Nothing else Just (Day x)

parseWeather :: String -> Maybe (NonEmpty Weather)
parseWeather = parse

{-
Business logic
--------------

Once we have defined our data model, the main program is short and sweet. For example,
the program below computes the day of the month with the minimum temperature difference.
-}

-- | Compare two weather records based on the difference between their
-- minimum and maximum temperatures
compareTempDiff :: Weather -> Weather -> Ordering
compareTempDiff w1 w2 = compare (diff w1) (diff w2)
  where
    diff w = maxTemp w - minTemp w

-- >>> weatherProgram
-- The day with minimum temperature change was Day 18
--

-- | A program that computes information about the weather
split :: (Eq a) => a -> [a] -> [[a]]
split c xs = case break (== c) xs of
  (ls, []) -> [ls]
  (ls, x : rs) -> ls : split c rs

-- >>>  splitHeader "A,B,C"
-- ["A","B","C"]
--
splitHeader :: String -> [String]
splitHeader = split ','

-- >>> splitRow ["A","B","C"] "1,2,3"
-- [("A","1"),("B","2"),("C","3")]
-- >>> splitRow ["A", "B"] "1,2,3"
-- [("A","1"),("B","2")]
-- >>> splitRow ["A","B","C"] "1,2"
-- [("A","1"),("B","2")]

splitRow :: [String] -> String -> [(String, String)]
splitRow header row = zip header (split ',' row)

{-
Note that a real library for CSV parsing would ignore `,`s that appear
inside quotes and treat them as part of the data (or header name). You do not
need to do so for this exercise.

With these these functions, we can now define a general purpose parser for
data files. This parser can produce any sort of result data, as long as there
is an instance of the `ParseRecord` class (such as the one for `Weather`
records above).
-}

class ParseRecord a where
  -- Convert a list of string elements into row
  parseRecord :: [(String, String)] -> Maybe a

parse :: ParseRecord a => String -> Maybe (NonEmpty a)
parse str = NE.nonEmpty $ Maybe.mapMaybe parseRecord (tabulate str)
  where
    tabulate :: String -> [[(String, String)]]
    tabulate s = case lines s of
      [] -> []
      hd : rows -> map (splitRow header) rows
        where
          header = splitHeader hd

class ParseField a where
  parseField :: String -> Maybe a

instance ParseField Int where
  parseField = readMaybe

instance ParseField Bool where
  parseField = readMaybe

instance ParseField Double where
  parseField = readMaybe

instance ParseField String where
  parseField = Just

instance ParseField Prec where
  parseField [x] = Just $ Prec (x == 'T')
  parseField _ = Nothing

{-
Field chaining
--------------

The implementation of `ParseRecord` uses `ParseField` implicitly through the
use of the binary operator `<**>`. This operator, defined below, combines
together the various fields in the row. It uses `ParseField` to allow
the type of each field in the structure to determine how it should be
parsed as a Haskell value.

For example, suppose we have this example data model
-}

data Example = Example Int Day deriving (Show)

{-
Then we can build/validate input field by field, returning
`Nothing` if any of them fail to parse.
-}

-- >>> Just Example <**> Just "12 " <**> Just " 12"
-- Just (Example 12 (Day 12))
-- >>> Just Example <**> Just "47 " <**> Just " 47"
-- Nothing

infixl 4 <**>

(<**>) :: ParseField a => Maybe (a -> b) -> Maybe String -> Maybe b
(Just f) <**> (Just a) = f <$> parseField a
_ <**> _ = Nothing

{-
At this point you should be able to run the `weatherProgram` above. Make
sure that it works correctly before continuing.

Parsing different types of fields
---------------------------------

The `Weather` data constructor only takes `Day` and `Int` arguments, but
`parseRecord` is more general than that. All we need is an instance of
`ParseField` to parse other types of data.

For example, `String` fields need no conversion, so they can be returned
immediately.

A more full featured library would include additional instances for the basic
Haskell types.
-}

-----------------------------------------------------------------
-- Weather events

{-
However, let's do more domain-specific parsing.

For example, the weather file includes a column (marked `WX`) for weather events. We
can represent these events with a datatype. If multiple events happen on the same day,
there will be multiple entries in the column.
-}

data Event
  = Fog
  | IntenseFog
  | Thunder
  | Ice
  | Hail
  | FreezingRain
  | Duststorm
  | Smoke
  | BlowingSnow
  | Tornado
  deriving (Eq, Ord, Enum, Show)

{-
Your next job is to parse single events according to the legend shown in the
data file. More specifically, your implementation should
look at a single character ('1'-'9' or 'X') and produce the appropriate
event above.
-}

parseEventChar :: Char -> Maybe Event
parseEventChar 'X' = Just Tornado
parseEventChar c | c <= '9' && c >= '1' = Just $ toEnum (ord c - ord '1')
parseEventChar _ = Nothing

-- >>> parseEventChar '3'
-- Just Thunder
--

testParseEventChar :: Test
testParseEventChar =
  TestList
    [ parseEventChar '1' ~?= Just Fog,
      parseEventChar 'X' ~?= Just Tornado,
      parseEventChar 'Y' ~?= (Nothing :: Maybe Event)
    ]

{-
We can use this parser for events to parse the sequence of events in the WX
 column. We'll start by defining a new type for a list of events.
-}

newtype Events = Events {getEvents :: [Event]} deriving (Eq, Show)

{-
This newtype allows us to define a special purpose parser that parses each
character individually and ignores any invalid events.
-}

-- >>> parseField "12 " :: Maybe Events
-- Just (Events {getEvents = [Fog,IntenseFog]})
-- >>> parseField "1D3 " :: Maybe Events
-- Just (Events {getEvents = [Fog,Thunder]})

instance ParseField Events where
  parseField = fmap Events . traverse parseEventChar

{-
Above, you might find functions in the `Data.Maybe` library useful.

Working with the Library
------------------------

In the weather data file, the column marked `DEP` indicates the difference
between the day's average temperature and the usual average temperature for
that day.  Modify the types above (or define new ones!) and fill in the
definitions below so that we can also calculate the day where the temperature
is the most unseasonable... i.e. the day with the greatest departure from
normal.
-}

-- | return the day of the month with the largest departure from normal temperature
mostUnseasonable :: NonEmpty Weather -> Day
mostUnseasonable = day . maximumBy (compare `on` dep)

{-
Next, write a function that returns how many days of the month had
some sort of precipitation. The column marked `WTR` contains this information,
when that column has a `T`, that indicates a trace amount, which should be
included in the result.
-}

-- | return how many days had some sort of precipitation
numPrecip :: NonEmpty Weather -> Int
numPrecip = length . NE.filter ((== Prec True) . precipitation)

{-
Finally, write a function that returns the list of all foggy days. This
function should count all days that include `Fog` or `IntenseFog` as a
weather event.
-}

-- | return a list of all dates with fog events
foggyDays :: NonEmpty Weather -> [Day]
foggyDays = fmap day . NE.filter (f . events)
  where
    f (Events xs) = elem Fog xs || elem IntenseFog xs

-- >>>  weatherProgram
-- The day with minimum temperature change was Day 28
-- The day with mostUnseasonable was Day 20
-- numPrecip was 5
-- foggyDays was [Day 8,Day 9]
--

weatherProgram :: IO ()
weatherProgram = do
  chars <- readFile "./data/HW3/jul20.csv"
  case parse chars of
    Nothing ->
      putStrLn "Cannot parse weather file."
    Just ws -> do
      let ans = day (Foldable.minimumBy compareTempDiff ws)
      putStrLn $ "The day with minimum temperature change was " ++ show ans
      putStrLn $ "The day with mostUnseasonable was " ++ show (mostUnseasonable ws)
      putStrLn $ "numPrecip was " ++ show (numPrecip ws)
      putStrLn $ "foggyDays was " ++ show (foggyDays ws)

{-
Make sure that you write some tests for these functions! This time we've given you
two weather data files to play with: jul20.csv and jul21.csv. Make sure
that your implementation works for both of them.

-----------------------------------------------------------------

Premier League data
--------------------

Finally, to make sure that you understand how this library works, use it to
process Premier League tables, calculating the place `#` of the team with the smallest
absolute difference between the number of wins `W` and number of losses `L`. If there
are two teams with the same difference, the program should return the first one.
-}

data SoccerStats = SoccerStats
  { place :: Int,
    wins :: Int,
    losses :: Int
  }

instance ParseRecord SoccerStats where
  parseRecord row = Just SoccerStats <**> row !? "#" <**> row !? "W" <**> row !? "L"

computeDiff :: SoccerStats -> Int
computeDiff (SoccerStats _ wins losses) = abs (wins - losses)

soccer :: String -> Maybe Int
soccer chars = do
  stats <- parse chars
  return $ place (Foldable.minimumBy (compare `on` computeDiff) stats)

soccerProgram :: IO ()
soccerProgram = do
  chars <- readFile "./data/HW3/soccer20.csv"
  case soccer chars of
    Just answer ->
      putStrLn $ "The team with the smallest difference placed " ++ show answer ++ "."
    Nothing ->
      putStrLn "Couldn't read input file"

-- >>>  soccerProgram
-- The team with the smallest difference placed 11.
--
--
