module Bittygame where

import Html exposing (Html)
import Signal
import Mouse

main : Signal Html
main = Signal.map pointerView whereIsThePointer

whereIsThePointer: Signal (Int, Int)
whereIsThePointer = Mouse.position

pointerView: (Int, Int) -> Html
pointerView (x, y) = Html.text ("you are pointing at " ++ (toString x) ++ ", " ++ (toString y))
