module ZeroOne where

-- Code from https://github.com/JordanMartinez/learn-halogen/tree/v5.0.2/src/01-Static-HTML

import Prelude
import Data.Const (Const)
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Halogen.HTML (ClassName(..))
import Halogen.HTML.Properties (ButtonType(..))
import Halogen.HTML.Properties as HP

import CSS (backgroundColor, fontSize, orange, px)
import Halogen.HTML.CSS as CSS

type StaticHTML
  = HH.ComponentHTML Unit () Aff

staticHtml :: StaticHTML
staticHtml =
  HH.div_
    [ HH.div_
        [ HH.span_ [ HH.text "This is purescript!" ] ]
    , HH.button_ [ HH.text "I'm a button!" ]
    ]

staticHtml' :: StaticHTML
staticHtml' =
  HH.div
    [ HP.id_ "top-div" ]
    [ HH.div
        [ HP.class_ $ ClassName "special-div" ]
        [ HH.span
            [ HP.classes
              [ ClassName "class1", ClassName "class2", ClassName "class3" ]
            , CSS.style do
                fontSize $ px 50.0
                backgroundColor orange
            ]
            [ HH.text "This is text in a span!" ]
        ]
    , HH.button [ HP.type_ ButtonButton ] [ HH.text "You can click, but nothing happens!" ]
    ]
  where
      spanStyle = CSS.style do
          fontSize $ px 20.0
          backgroundColor orange

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    runUI (staticComponent staticHtml') unit body

staticComponent :: StaticHTML -> H.Component HH.HTML (Const Void) Unit Void Aff
staticComponent renderHtml =
  H.mkComponent
    { initialState: const unit
    , render: \_ -> renderHtml
    , eval: H.mkEval H.defaultEval
    }
