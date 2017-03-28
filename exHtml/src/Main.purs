module Main where

import Prelude
import Halogen.Aff as HA
import Component (ExEffects, ExQuery, exComponent)
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Control.Monad.Aff (Aff)

-- | The application state, which in this case will contain the html document ID

type State = { documentId:: String }

-- | The query algebra for the app.
data Query a = LoadDocument String a

-- | The slot address type for the component.
data ExSlot = ExSlot
derive instance eqExSlot :: Eq ExSlot
derive instance ordExSlot :: Ord ExSlot


-- | The main UI component definition.
ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (ExEffects eff))
ui =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { documentId: "test" }

  render :: State -> H.ParentHTML Query Query ExSlot (Aff (ExEffects eff))
  render { documentId: id } =
    HH.div_
      [ HH.h1_
          [ HH.text ("External HTML editor"<> id) ]
      , HH.div_
          [ HH.slot ExSlot exComponent unit (const Nothing) ]
      ]

  eval :: Query ~> H.ParentDSL State Query ExQuery ExSlot Void (Aff (ExEffects eff))
  eval = case _ of
    LoadDocument id next -> do
      H.modify (_ { documentId = id })
      pure next

-- | Run the app!
main :: Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui unit body
