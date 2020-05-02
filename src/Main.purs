module Main where

import Prelude

import Control.Monad.State (get, put)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen (ComponentHTML)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

type State = Boolean

data Action = Toggle

toggleButton :: StateAndActionRenderer State Action
toggleButton isOn =
  let toggleLabel = if isOn then "On" else "Off"
  in
    HH.button
      [ HE.onClick \_ -> Just Toggle ]
      [ HH.text $ "The button is " <> toggleLabel ]

handleAction :: HandleSimpleAction State Action
handleAction =
  case _ of
    Toggle -> not <$> get >>= put

main :: Effect Unit
main =
  runStateAndActionComponent
    { initialState: false
    , render: toggleButton
    , handleAction: handleAction
    }

type DynamicHtml action = ComponentHTML action () Aff

type StateAndActionRenderer state action = (state -> DynamicHtml action)

type HandleSimpleAction state action =
  (action -> H.HalogenM state action () Void Aff Unit)

type SimpleChildComponent state action =
  { initialState :: state
  , render :: StateAndActionRenderer state action
  , handleAction :: HandleSimpleAction state action
  }

runStateAndActionComponent ::
  forall state action.
  SimpleChildComponent state action
  -> Effect Unit
runStateAndActionComponent childSpec =
  runHalogenAff do
     body <- awaitBody
     runUI (stateAndActionComponent childSpec) unit body

stateAndActionComponent ::
  forall state action.
    SimpleChildComponent state action
    -> H.Component HH.HTML (Const Void) Unit Void Aff
stateAndActionComponent spec =
  H.mkComponent
  { initialState: const spec.initialState
  , render: spec.render
  , eval: H.mkEval $ H.defaultEval { handleAction = spec.handleAction }
  }
