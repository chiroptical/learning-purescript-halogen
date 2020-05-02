module ZeroTwoZeroFour where

import Prelude

import Control.Monad.Error.Class (throwError)

import Data.Const (Const)
import Data.Maybe (maybe)

import Effect (Effect)
import Effect.Exception (error)
import Effect.Aff (Aff)

import Halogen as H
import Halogen.Aff (awaitLoad, selectElement, runHalogenAff)
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)

import Web.HTML (HTMLElement)
import Web.DOM.ParentNode (QuerySelector(..))

type State = Int

type StaticHTML = H.ComponentHTML Unit () Aff

simpleIntState :: State -> StaticHTML
simpleIntState value =
  HH.div_ [ HH.text $ show value ]

main :: Effect Unit
main = runStateOnlyDyamicRenderer 1 2 3 simpleIntState

type StateOnlyDynamicRenderer state = (state -> StaticHTML)

runStateOnlyDyamicRenderer ::
     forall state.
     state
  -> state
  -> state
  -> StateOnlyDynamicRenderer state
  -> Effect Unit
runStateOnlyDyamicRenderer first second third renderer =
  runHalogenAff do
     awaitLoad
     div1 <- selectElement' "could not find 'div#first" $ QuerySelector "#first"
     div2 <- selectElement' "could not find 'div#second" $ QuerySelector "#second"
     div3 <- selectElement' "could not find 'div#third" $ QuerySelector "#third"
     void $ runUI (stateOnlyStaticComponent first renderer) unit div1
     void $ runUI (stateOnlyStaticComponent second renderer) unit div2
     void $ runUI (stateOnlyStaticComponent third renderer) unit div3

stateOnlyStaticComponent ::
     forall state.
     state
  -> StateOnlyDynamicRenderer state
  -> H.Component HH.HTML (Const Void) Unit Void Aff
stateOnlyStaticComponent state dynamicRenderer =
  H.mkComponent
    { initialState: const state
    , render: dynamicRenderer
    , eval: H.mkEval H.defaultEval
    }

selectElement' :: String -> QuerySelector -> Aff HTMLElement
selectElement' errorMessage query = do
  maybeElem <- selectElement query
  maybe (throwError (error errorMessage)) pure maybeElem
