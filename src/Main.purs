module Main where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Halogen (Component)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (HTML)
import Data.Array as Array
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Prelude (Unit, bind, unit, ($), (<$>), (<<<), (>>>))
import Waypoint (parseFromString, toUrl, toUrls)
import Web.Event.Internal.Types (Event)

foreign import _fileFromInput :: Event -> EffectFnAff String

fileFromInput :: Event -> Aff String
fileFromInput = _fileFromInput >>> fromEffectFnAff


main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

data Action
  = LoadFile Event

component :: forall m c b a. MonadAff m => Component HTML a b c m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = Nothing

  render state =
    HH.div
      [ HP.classes [ HH.ClassName "main" ]
      ]
      [ HH.h1_ [ HH.text "Convert GPX File to Google Maps route" ]
      , HH.input
          [ HP.classes [ HH.ClassName "gpx-input" ], HP.type_ InputFile
          , HE.onChange $ Just <<< LoadFile
          ]
      , HH.div_ $
           case state of
              Nothing -> [HH.text ""]
              Just file -> case toUrls <$> parseFromString file of
                Left err -> [HH.text err]
                Right urls -> Array.fromFoldable $ renderUrl <$> urls
          
      ]
  renderUrl url =  HH.a [ HP.classes [ HH.ClassName "maps-link" ], HP.href url ] [ HH.text url ]

  handleAction = case _ of
    LoadFile target -> do
      file <- H.liftAff $ fileFromInput target
      H.modify_ \state -> Just file
