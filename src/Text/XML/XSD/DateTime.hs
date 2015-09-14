{-# LANGUAGE OverloadedStrings #-}

-- | XSD @dateTime@ data structure <http://www.w3.org/TR/xmlschema-2/#dateTime>
module Text.XML.XSD.DateTime
       ( DateTime(..)
       , isoEither
       , isoEither'
       , fold
       , pUTCTime
       , pLocalTime
       , dateTime
       , dateTime'
       , isZoned
       , isUnzoned
       , fromZonedTime
       , toUTCTime
       , fromUTCTime
       , toLocalTime
       , fromLocalTime
       , utcTime'
       , utcTime
       , localTime'
       , localTime
       ) where

import           Prelude(Show(..), Read(..), Eq(..), Ord(..), Num(..), Int, Integer, String, (&&), (||), read, fromIntegral, realToFrac)
import           Control.Applicative (pure, (<$>), (*>), (<*), (<|>))
import           Control.Monad (Monad(..), when)
import           Control.Lens(Iso', Prism', _Left, _Right, iso, prism', from, isn't, (#), (^?))
import           Data.Attoparsec.Text(Parser, char, digit, parseOnly, endOfInput, takeWhile)
import           Data.Bool(Bool(..))
import           Data.Char (isDigit, ord)
import           Data.Either(Either(..), either)
import           Data.Fixed (Pico, showFixed)
import           Data.Function((.), id, ($), const)
import           Data.List((++))
import           Data.Maybe (Maybe(..), maybeToList, maybe)
import           Data.Monoid ((<>))
import           Data.Text(Text, pack, unpack, length, head)
import           Data.Text.Lazy as TL(toStrict)
import           Data.Text.Lazy.Builder(Builder, toLazyText, fromString)
import           Data.Text.Lazy.Builder.Int (decimal)
import qualified Data.Text.Read as TR(decimal)
import           Data.Time
import           Data.Time.Calendar.MonthDay (monthLength)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Test.QuickCheck.Instances ()
-- >>> let mkLocal :: Integer -> Int -> Int -> Int -> Int -> Pico -> LocalTime; mkLocal y m d hh mm ss = LocalTime (fromGregorian y m d) (TimeOfDay hh mm ss)
-- >>> let mkUTC :: Integer -> Int -> Int -> Int -> Int -> Pico -> UTCTime; mkUTC y m d hh mm ss = localTimeToUTC utc (mkLocal y m d hh mm ss)
-- >>> let mkZoned :: Integer -> Int -> Int -> Int -> Int -> Pico -> Int -> Int -> ZonedTime; mkZoned y m d hh mm ss zh zm = ZonedTime (mkLocal y m d hh mm ss) (TimeZone offset False "") where offset = signum zh * (abs zh * 60 + zm)

-- | XSD @dateTime@ data structure
-- <http://www.w3.org/TR/xmlschema-2/#dateTime>. Briefly, a @dateTime@
-- uses the Gregorian calendar and may or may not have an associated
-- timezone. If it has a timezone, then the canonical representation
-- of that date time is in UTC.
--
-- Note, it is not possible to establish a total order on @dateTime@
-- since non-timezoned are considered to belong to some unspecified
-- timezone.
data DateTime =
  DtZoned UTCTime
  | DtUnzoned LocalTime
  deriving (Eq, Ord)

-- | Internal helper that creates a date time. Note, if the given hour
-- is 24 then the minutes and seconds are assumed to be 0.
mkDateTime
  :: Integer           -- ^ Year
  -> Int               -- ^ Month
  -> Int               -- ^ Day
  -> Int               -- ^ Hours
  -> Int               -- ^ Minutes
  -> Pico              -- ^ Seconds
  -> Maybe Pico        -- ^ Time zone offset
  -> DateTime
mkDateTime y m d hh mm ss mz =
  case mz of
    Just z -> DtZoned $ addUTCTime (negate $ realToFrac z) uTime
    Nothing -> DtUnzoned lTime
  where
    day = addDays (if hh == 24 then 1 else 0) (fromGregorian y m d)
    tod = TimeOfDay (if hh == 24 then 0 else hh) mm ss
    lTime = LocalTime day tod
    uTime = UTCTime day (timeOfDayToTime tod)

instance Show DateTime where
  show = unpack . (dateTime #)

instance Read DateTime where
  readList s = [(maybeToList (pack s ^? dateTime), [])]

-- | The isomorphism between a @DateTime@ and @Either UTCTime LocalTime@
isoEither ::
  Iso'
  (Either UTCTime LocalTime)
  DateTime
isoEither =
  iso (either DtZoned DtUnzoned) (\t -> case t of DtZoned e -> Left e
                                                  DtUnzoned q -> Right q)

-- | The isomorphism between a @DateTime@ and @Either LocalTime UTCTime@
isoEither' ::
  Iso'
  (Either LocalTime UTCTime)
  DateTime
isoEither' =
  iso (either DtUnzoned DtZoned) (\t -> case t of DtUnzoned q -> Left q
                                                  DtZoned e -> Right e)

-- | Reduction on a @DateTime@.
fold ::
  (LocalTime -> a)
  -> (UTCTime -> a)
  -> DateTime
  -> a
fold z _ (DtUnzoned q) =
  z q
fold _ z (DtZoned e) =
  z e

-- | A prism that views the @UTCTime@ component of a @DateTime@.
pUTCTime ::
  Prism' DateTime UTCTime
pUTCTime =
  from isoEither . _Left

-- | A prism that views the @LocalTime@ component of a @DateTime@.
pLocalTime ::
  Prism' DateTime LocalTime
pLocalTime =
  from isoEither . _Right

-- | A prism that parses the string into a @DateTime@ and converts the
-- @DateTime@ into a string.
--
-- prop> Just (DtZoned t) == (dateTime # fromUTCTime t) ^? dateTime
--
-- prop> Just (DtUnzoned t) == (dateTime # fromLocalTime t) ^? dateTime
--
-- >>> "2009-10-10T03:10:10-05:00" ^? dateTime
-- Just 2009-10-10T08:10:10Z
--
-- >>> "2119-10-10T03:10:10.4-13:26" ^? dateTime
-- Just 2119-10-10T16:36:10.4Z
--
-- >>> "0009-10-10T03:10:10.783952+14:00" ^? dateTime
-- Just 0009-10-09T13:10:10.783952Z
--
-- >>> "2009-10-10T03:10:10Z" ^? dateTime
-- Just 2009-10-10T03:10:10Z
--
-- >>> "-2009-05-10T21:08:59+05:00" ^? dateTime
-- Just -2009-05-10T16:08:59Z
--
-- >>> "-19399-12-31T13:10:10-14:00" ^? dateTime
-- Just -19398-01-01T03:10:10Z
--
-- >>> "2009-12-31T13:10:10" ^? dateTime
-- Just 2009-12-31T13:10:10
--
-- >>> "2012-10-15T24:00:00" ^? dateTime
-- Just 2012-10-16T00:00:00
--
-- >>> "2002-10-10T12:00:00+05:00" ^? dateTime
-- Just 2002-10-10T07:00:00Z
--
-- >>> "2002-10-10T00:00:00+05:00" ^? dateTime
-- Just 2002-10-09T19:00:00Z
--
-- >>> "-0001-10-10T00:00:00" ^? dateTime
-- Just 000-1-10-10T00:00:00
--
-- >>> "0001-10-10T00:00:00" ^? dateTime
-- Just 0001-10-10T00:00:00
--
-- >>> "2009-10-10T03:10:10-05" ^? dateTime
-- Nothing
--
-- >>> "2009-10-10T03:10:10+14:50" ^? dateTime
-- Nothing
--
-- >>> "2009-10-10T03:10:1" ^? dateTime
-- Nothing
--
-- >>> "2009-10-10T03:1:10" ^? dateTime
-- Nothing
--
-- >>> "2009-10-10T0:10:10" ^? dateTime
-- Nothing
--
-- >>> "2009-10-1T10:10:10" ^? dateTime
-- Nothing
--
-- >>> "2009-1-10T10:10:10" ^? dateTime
-- Nothing
--
-- >>> "209-10-10T03:10:10" ^? dateTime
-- Nothing
--
-- >>> "2009-10-10T24:10:10" ^? dateTime
-- Nothing
--
-- >>> "0000-01-01T00:00:00" ^? dateTime
-- Nothing
--
-- >>> "2009-13-01T00:00:00" ^? dateTime
-- Nothing
--
-- >>> "+2009-10-01T04:20:40" ^? dateTime
-- Nothing
--
-- >>> "002009-10-01T04:20:40" ^? dateTime
-- Nothing
--
-- >>> dateTime # fromUTCTime (mkUTC 2119 10 10 16 36 10.4)
-- "2119-10-10T16:36:10.4Z"
--
-- >>> dateTime # fromZonedTime (mkZoned 2010 04 07 13 47 20.001 2 0)
-- "2010-04-07T11:47:20.001Z"
--
-- >>> dateTime # fromLocalTime (mkLocal 13 2 4 20 20 20)
-- "0013-02-04T20:20:20"
--
-- >>> (dateTime #) `fmap` ("2010-04-07T13:47:20.001+02:00" ^? dateTime) --  issue 2
-- Just "2010-04-07T11:47:20.001Z"
dateTime ::
  Prism' Text DateTime
dateTime =
  let buildUInt2 ::
        Int
        -> Builder
      buildUInt2 x =
        (if x < 10 then ("0" <>) else id) $ decimal x
      buildInt4 ::
        Integer
        -> Builder
      buildInt4 year =
        let absYear = abs year
            k x = if absYear < x then ("0" <>) else id
        in  k 1000 . k 100 . k 10 $ decimal year
      toText ::
        DateTime
        -> Text
      toText =
        toStrict . toLazyText . dtBuilder
          where
            dtBuilder (DtZoned uTime) = ltBuilder (utcToLocalTime utc uTime) <> "Z"
            dtBuilder (DtUnzoned lTime) = ltBuilder lTime
            ltBuilder (LocalTime day (TimeOfDay hh mm sss)) =
              let (y, m, d) = toGregorian day
              in  buildInt4 y
                  <> "-"
                  <> buildUInt2 m
                  <> "-"
                  <> buildUInt2 d
                  <> "T"
                  <> buildUInt2 hh
                  <> ":"
                  <> buildUInt2 mm
                  <> ":"
                  <> buildSeconds sss
  in prism' toText (either (const Nothing) Just . dateTime')

-- | Parses the string into a @dateTime@ or may fail with a parse error.
dateTime' ::
  Text
  -> Either String DateTime
dateTime' =
  parseOnly (parseDateTime <* endOfInput <|> fail "bad date time")

buildSeconds ::
  Pico
  -> Builder
buildSeconds secs =
  (if secs < 10 then ("0" <>) else id)
  $ fromString (showFixed True secs)

-- | Converts a zoned time to a @dateTime@.
fromZonedTime :: ZonedTime -> DateTime
fromZonedTime = fromUTCTime . zonedTimeToUTC

-- | Whether the given @dateTime@ is timezoned.
isZoned ::
  DateTime
  -> Bool
isZoned =
  isn't pLocalTime

-- | Whether the given @dateTime@ is non-timezoned.
isUnzoned ::
  DateTime
  -> Bool
isUnzoned =
  isn't pUTCTime

-- | Attempts to convert a @dateTime@ to a UTC time. The attempt fails
-- if the given @dateTime@ is non-timezoned.
toUTCTime ::
  DateTime
  -> Maybe UTCTime
toUTCTime =
  (^? pUTCTime)

-- | Converts a UTC time to a timezoned @dateTime@.
fromUTCTime ::
  UTCTime
  -> DateTime
fromUTCTime =
  (pUTCTime #)

-- | Attempts to convert a @dateTime@ to a local time. The attempt
-- fails if the given @dateTime@ is timezoned.
toLocalTime ::
  DateTime
  -> Maybe LocalTime
toLocalTime =
  (^? pLocalTime)

-- | Converts a local time to a non-timezoned @dateTime@.
fromLocalTime ::
  LocalTime
  -> DateTime
fromLocalTime =
  (pLocalTime #)

-- | Parses the string in a @dateTime@ then converts to a UTC time and
-- may fail with a parse error.
utcTime' ::
  Text
  -> Either String UTCTime
utcTime' txt =
  dateTime' txt >>= maybe (Left err) Right . toUTCTime
    where
      err = "input time is non-timezoned"

-- | Parses the string in a @dateTime@ then converts to a UTC time and
-- may fail.
utcTime ::
  Text
  -> Maybe UTCTime
utcTime txt =
  txt ^? dateTime >>= toUTCTime

-- | Parses the string in a @dateTime@ then converts to a local time
-- and may fail with a parse error.
localTime' ::
  Text
  -> Either String LocalTime
localTime' txt =
  dateTime' txt >>= maybe (Left err) Right . toLocalTime
    where
      err = "input time is non-timezoned"

-- | Parses the string in a @dateTime@ then converts to a local time
-- time and may fail.
localTime ::
  Text
  -> Maybe LocalTime
localTime txt =
  txt ^? dateTime >>= toLocalTime

-- | Parser of the @dateTime@ lexical representation.
parseDateTime ::
  Parser DateTime
parseDateTime =
  do yy <- yearParser
     _ <- char '-'
     mm <- p2imax 12
     _ <- char '-'
     dd <- p2imax (monthLength (isLeapYear $ fromIntegral yy) mm)
     _ <- char 'T'
     hhh <- p2imax 24
     _ <- char ':'
     mmm <- p2imax 59
     _ <- char ':'
     sss <- secondParser
     when (hhh == 24 && (mmm /= 0 || sss /= 0))
       $ fail "invalid time, past 24:00:00"
     o <- parseOffset
     return $ mkDateTime yy mm dd hhh mmm sss o

-- | Parse timezone offset.
parseOffset ::
  Parser (Maybe Pico)
parseOffset =
  (endOfInput *> pure Nothing)
  <|>
  (char 'Z' *> pure (Just 0))
   <|>
  (do sign <- (char '+' *> pure 1) <|> (char '-' *> pure (-1))
      hh <- fromIntegral <$> p2imax 14
      _ <- char ':'
      mm <- fromIntegral <$> p2imax (if hh == 14 then 0 else 59)
      return . Just $ sign * (hh * 3600 + mm * 60))

yearParser ::
  Parser Integer
yearParser =
  do sign <- (char '-' *> pure (-1)) <|> pure 1
     ds <- takeWhile isDigit
     when (length ds < 4)
       $ fail "need at least four digits in year"
     when (length ds > 4 && head ds == '0')
       $ fail "leading zero in year"
     let Right (absyear, _) = TR.decimal ds
     when (absyear == 0)
       $ fail "year zero disallowed"
     return $ sign * absyear

secondParser ::
  Parser Pico
secondParser =
  do d1 <- digit
     d2 <- digit
     frac <- readFrac <$> (char '.' *> takeWhile isDigit)
             <|> pure 0
     return (read [d1, d2] + frac)
    where
      readFrac ds = read $ '0' : '.' : unpack ds

p2imax ::
  Int
  -> Parser Int
p2imax m =
  do a <- digit
     b <- digit
     let n = 10 * val a + val b
     if n > m
       then fail $ "value " ++ show n ++ " exceeded maximum " ++ show m
       else return n
    where
      val c = ord c - ord '0'
