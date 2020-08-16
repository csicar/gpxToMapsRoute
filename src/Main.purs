module Main where

import Prelude
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HashMap (fromFoldable, lookup)
import Data.List (List, catMaybes)
import Data.Maybe (Maybe(..))
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Traversable (for)
import Data.XML (class DecodeXML, decodeXML, (?>), (?>>), (??>))
import Data.XML (class DecodeXML, parseXML)
import Data.XML.Types (XML(..))
import Debug.Trace (spy, trace)
import Effect (Effect)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff(..), fromEffectFnAff)
import Effect.Console (log, logShow)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readFile, readTextFile)
import Web.Event.Internal.Types (Event)

foreign import _fileFromInput :: Event -> EffectFnAff String

fileFromInput :: Event -> Aff String
fileFromInput = _fileFromInput >>> fromEffectFnAff

newtype Waypoint
  = Waypoint
  { lat :: String
  , long :: String
  }

derive instance genericWaypoint :: Generic Waypoint _

instance showWaypoint :: Show Waypoint where
  show = genericShow

parseWaypoint :: XML -> Either String (Maybe Waypoint)
parseWaypoint (XMLNode "wpt" attrs _) = do
  let
    m = fromFoldable attrs
  lat <- maybeToEither "lat not found" $ lookup "lat" m
  long <- maybeToEither "long not found" $ lookup "lon" m
  pure $ Just $ Waypoint { lat, long }

parseWaypoint (XMLNode "metadata" _ _) = Right Nothing

parseWaypoint s = Right Nothing

parseWaypoints :: XML -> Either String (List Waypoint)
parseWaypoints (XMLNode "gpx" _ children) = catMaybes <$> for children parseWaypoint

parseWaypoints _ = Right mempty

parseFromString :: String -> Either String (List Waypoint)
parseFromString str = case parseXML str of
  Left err -> Left $ show err
  Right xml -> parseWaypoints xml

testParse :: String -> Effect Unit
testParse str = case parseXML str of
  Left err -> logShow err
  Right xml -> logShow $ parseWaypoints xml

maybeToEither :: ∀ a. String -> Maybe a -> Either String a
maybeToEither err Nothing = Left err

maybeToEither _ (Just a) = Right a

toUrl :: List Waypoint -> String
toUrl waypoints = "https://www.google.com/maps/dir/" <> joinWith "/" (Array.fromFoldable $ waypointToUrl <$> waypoints)
  where
  waypointToUrl :: Waypoint -> String
  waypointToUrl (Waypoint { lat, long }) = "'" <> lat <> "," <> long <> "'"

cli :: Effect Unit
cli = do
  content <- readTextFile UTF8 "file.gpx"
  let
    url = toUrl <$> parseFromString content
  -- case (parseWaypoint "<a lat=\"asd\" long=\"as\"/>") of
  --   Left _ -> log "!"
  --   Right xml -> 
  case url of
    Left err -> log err
    Right res -> log res

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

data Action
  = LoadFile Event

component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = Nothing

  render state =
    HH.div [HP.classes [HH.ClassName "main"]]
      [ HH.h1_ [HH.text "Convert GPX File to Google Maps route"]
      , HH.input [ HP.classes [HH.ClassName "gpx-input"], HP.type_ InputFile, HE.onChange $ Just <<< LoadFile ]
      , HH.div_
          [ case state of
              Nothing -> HH.text ""
              Just file -> case toUrl <$> parseFromString file of
                Left err -> HH.text err
                Right url -> HH.a [ HP.classes [HH.ClassName "maps-link"], HP.href url ] [ HH.text url ]
          ]
      ]

  handleAction = case _ of
    LoadFile target -> do
      file <- H.liftAff $ fileFromInput target
      H.modify_ \state -> Just file
