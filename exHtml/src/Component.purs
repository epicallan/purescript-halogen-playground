module Component where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Control.Monad.Aff (Aff)
import DOM (DOM)
import Data.Maybe (Maybe(..))
import Foreign (setHTML)
import Network.HTTP.Affjax (AJAX)


data ExQuery a
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

type ExState =
  {
    loading :: Boolean
  , startEditing :: Maybe Id
  , commitEdit :: Maybe Element
  }


type Output = Void

-- | ExEffects
type ExEffects eff = (dom :: DOM, ajax :: AJAX | eff)

-- | The component definition.
exComponent :: forall eff. H.Component HH.HTML ExQuery Unit Output (Aff (ExEffects eff))
exComponent =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action GetExternalHtml)
    , finalizer: Just (H.action Finalize)
    , receiver: const Nothing
    }
  where

  initialState :: ExState
  initialState = { loading: false, startEditing : Nothing, commitEdit : Nothing  }

  render :: ExState -> H.ComponentHTML ExQuery
  render = const $ HH.div [ HP.ref (H.RefLabel "stage") ] []

  eval :: ExQuery ~> H.ComponentDSL ExState ExQuery Output (Aff (ExEffects eff))
  eval = case _ of
    GetExternalHtml next -> do
      H.getHTMLElementRef (H.RefLabel "stage") >>= case _ of
        Nothing -> pure unit
        Just el' -> do
          response <- H.liftAff $ AX.get ("https://localhost:3001/api/docs/test")
          H.liftEff $ setHTML el' response.response
          H.modify (_ { loading = true })
      pure next

    EditHtml next -> pure next

    Finalize next -> pure next

    CommitHtml next -> pure next
