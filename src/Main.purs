module Main where

import Prelude
import Color (Color, white)
import Data.Array ((..))
import Data.Foldable (foldMap)
import Data.Int (rem, toNumber)
import Data.JSDate (getHours, getMilliseconds, getMinutes, getSeconds, now)
import Data.List (List(..), fromFoldable, (:))
import Data.Maybe (Maybe(..))
import Data.Vector (Vec(..), fromPolar, mulScalar)
import Data.Vector.Utils (Pipeline, Step(..), render)
import Effect (Effect)
import Graphics.Canvas (CanvasElement, Dimensions, fillRect, getCanvasDimensions, getCanvasElementById, getContext2D, scale, setCanvasDimensions)
import Math (pi, tau, (%))
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (HTMLElement, window)
import Web.HTML.Window (innerHeight, innerWidth, requestAnimationFrame)

windowDims :: Effect Dimensions
windowDims = do
  w <- window
  width <- innerWidth w
  height <- innerHeight w
  pure { width: toNumber width, height: toNumber height }

fromCanvas :: CanvasElement -> HTMLElement
fromCanvas = unsafeCoerce

mkSectors ::
  Vec Number ->
  Int ->
  Int ->
  Number ->
  Number ->
  Number ->
  Color ->
  Pipeline
mkSectors origin n skip start end width color =
  flip foldMap (fromFoldable (0 .. (n - 1)))
    ( \i ->
        if i `rem` skip == 0 then
          Nil
        else
          let
            i' = toNumber i

            n' = toNumber n

            theta = i' / n' * (-tau)

            a = origin + fromPolar start theta

            b = origin + fromPolar end theta
          in
            Line a b
              : LineWidth width
              : StrokeStyle color
              : Stroke
              : Nil
    )

mkHand :: Vec Number -> Number -> Number -> Number -> Color -> Pipeline
mkHand origin len theta width color =
  let
    n = fromPolar 1.0 ((pi / 2.0 + theta) * (-1.0))
  in
    Line (origin - (len / 5.0) `mulScalar` n) (origin + (len `mulScalar` n))
      : LineWidth width
      : StrokeStyle color
      : Stroke
      : Nil

main :: Effect Unit
main =
  void $ unsafePartial
    $ do
        win <- window
        Just canvas <- getCanvasElementById "canvas"
        ctx <- getContext2D canvas
        dims <- windowDims
        setCanvasDimensions canvas dims
        { width, height } <- getCanvasDimensions canvas
        scale ctx { scaleX: 1.0, scaleY: (-1.0) }
        let
          viewport = { x: 0.0, y: 0.0, width, height: (-height) }

          hw = width / 2.0

          hh = -height / 2.0

          origin = Vec hw hh

          hourSectors = mkSectors origin 12 0 (0.77 * hh) (0.83 * hh) 15.0 white

          minuteSectors = mkSectors origin 60 5 (0.78 * hh) (0.82 * hh) 5.0 white

          face =
            Point origin 5.0 white
              : Circle origin (0.4 * height)
              : LineWidth 4.0
              : StrokeStyle white
              : Stroke
              : Nil
              <> hourSectors
              <> minuteSectors

          render' :: Effect Unit
          render' = do
            fillRect ctx viewport
            t <- now
            hrs <- getHours t
            mins <- getMinutes t
            secs <- getSeconds t
            ms <- getMilliseconds t
            let
              hourHand = mkHand origin (0.3 * hh) ((hrs % 12.0 + mins / 60.0) / 12.0 * tau) 10.0 white

              minHand = mkHand origin (0.6 * hh) ((mins + secs / 60.0) / 60.0 * tau) 8.0 white

              secHand = mkHand origin (0.7 * hh) ((secs + ms / 1000.0) / 60.0 * tau) 4.0 white
            render
              ctx
              $ face
              <> hourHand
              <> minHand
              <> secHand
            _ <- requestAnimationFrame render' win
            pure unit
        render'

{- clickListener <-
  eventListener
    $ \e -> do
        case fromEvent e of
          Just e' -> modify_ (\_ -> V { x: toNumber $ clientX e', y: (toNumber $ clientY e') * -1.0 }) mouse
addEventListener (EventType "mousemove") clickListener false (toEventTarget $ fromCanvas canvas) -}
