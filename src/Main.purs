module Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HashMap (fromFoldable, lookup)
import Data.List (List, catMaybes)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Traversable (for)
import Data.XML (class DecodeXML, decodeXML, (?>), (?>>), (??>))
import Data.XML (class DecodeXML, parseXML)
import Data.XML.Types (XML(..))
import Debug.Trace (spy, trace)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readFile, readTextFile)

newtype Waypoint
  = Waypoint
  { lat :: String
  , long :: String
  }

derive instance  genericWaypoint :: Generic Waypoint _

instance showWaypoint :: Show Waypoint where show = genericShow

parseWaypoint :: XML -> Either String (Maybe Waypoint)
parseWaypoint (XMLNode "wpt" attrs _) = do
  let m = fromFoldable attrs
  lat <- maybeToEither "lat not found" $ lookup  "lat" m
  long <- maybeToEither "long not found" $ lookup  "lon" m
  pure $ Just $ Waypoint {lat, long}
parseWaypoint (XMLNode "metadata" _ _) = Right Nothing
parseWaypoint s = Right Nothing

parseWaypoints :: XML -> Either String (List Waypoint)
parseWaypoints (XMLNode "gpx" _ children) = catMaybes <$>  for children parseWaypoint
parseWaypoints _ = Right mempty


parseFromString :: String -> Either String (List Waypoint)
parseFromString str = case parseXML str of
  Left err -> Left $ show err
  Right xml -> parseWaypoints xml
  
  
testParse :: String -> Effect Unit
testParse str = case parseXML str of
  Left err -> logShow err
  Right xml -> logShow $ parseWaypoints xml

maybeToEither :: ∀a. String -> Maybe a  -> Either String a
maybeToEither err Nothing = Left err
maybeToEither _ (Just a) = Right a

toUrl :: List Waypoint -> String
toUrl waypoints = "https://www.google.com/maps/dir/" <> joinWith "/" (Array.fromFoldable $ waypointToUrl <$> waypoints)
  where 
    waypointToUrl :: Waypoint -> String
    waypointToUrl (Waypoint {lat, long}) = "'" <> lat <> "," <> long <> "'"

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "file.gpx"
  let url = toUrl <$> parseFromString content 
  -- case (parseWaypoint "<a lat=\"asd\" long=\"as\"/>") of
  --   Left _ -> log "!"
  --   Right xml -> 
  case url of
    Left err -> log err
    Right res -> log res
