module Component where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import DOM (DOM)
import Data.List (List)
import Data.Maybe (Maybe(..))

data Query a
  = GetExternalHtml a
  | EditHtml a
  | Finalize a
  | CommitHtml a

type Id  = String

type Element =
  { id :: Id
  , text :: String
  , classes :: String
  , styles :: String
  }

type State =
  {
    hasLoadedHtml :: Boolean
  , startEditing :: Maybe Id
  , commitEdit :: Maybe Element
  }

data Output = TextChanged (List Element)

-- | Effects
type Effects eff = (dom :: DOM, avar :: AVAR | eff)

-- | The component definition.
component :: forall eff. H.Component HH.HTML Query Unit Output (Aff (Effects eff))
component =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action GetExternalHtml)
    , finalizer: Just (H.action Finalize)
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { hasLoadedHtml: false, startEditing : Nothing, commitEdit : Nothing  }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "External Html Component Experiment" ]
      , HH.div [ HP.ref (H.RefLabel "stage") ] []
      ]

  eval :: Query ~> H.ComponentDSL State Query Output (Aff (Effects eff))
  eval = case _ of
    GetExternalHtml next -> do
      H.getHTMLElementRef (H.RefLabel "stage") >>= case _ of
        Nothing -> pure unit
        Just el' -> do
          -- call for html and insert here // effectfully
          H.modify (_ { hasLoadedHtml = true })
          -- create a listener for double click events with in the html
      pure next

    EditHtml next -> pure next

    Finalize next -> pure next

    CommitHtml next -> pure next
