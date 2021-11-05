module Data.Vector.Utils where

import Prelude
import Color (Color, toHexString)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse_)
import Data.Vector (Vec(..))
import Effect (Effect)
import Graphics.Canvas
  ( Context2D
  , arc
  , beginPath
  , closePath
  , fill
  , lineTo
  , moveTo
  , setFillStyle
  , setLineWidth
  , setStrokeStyle
  , stroke
  , withContext
  )
import Math (tau)

data Step
  = StrokeStyle Color
  | FillStyle Color
  | LineWidth Number
  | Line (Vec Number) (Vec Number)
  | Circle (Vec Number) Number
  | Point (Vec Number) Number Color
  | Path (List (Vec Number)) {- this should be traversble -}
  | Shape (List (Vec Number))
  | Fill
  | Stroke

derive instance genericStep :: Generic Step _

instance showStep :: Show Step where
  show = genericShow

-- this should be traversble
type Pipeline
  = List Step

render :: Context2D -> Pipeline -> Effect Unit
render ctx pipeline = withContext ctx $ traverse_ render' pipeline
  where
  render' :: Step -> Effect Unit
  render' = case _ of
    Line (Vec x1 y1) (Vec x2 y2) -> do
      beginPath ctx
      moveTo ctx x1 y1
      lineTo ctx x2 y2
    LineWidth w -> setLineWidth ctx w
    StrokeStyle c -> setStrokeStyle ctx $ toHexString c
    FillStyle c -> setFillStyle ctx $ toHexString c
    Circle (Vec x y) radius -> do
      beginPath ctx
      arc ctx { x, y, start: 0.0, end: tau, radius }
    Path vs -> do
      beginPath ctx
      traverseWithIndex_
        ( \idx (Vec x y) -> case idx of
            0 -> moveTo ctx x y
            _ -> lineTo ctx x y
        )
        vs
    Shape vs -> do
      render' (Path vs)
      closePath ctx
    Fill -> fill ctx
    Point v r c -> render' `traverse_` (Circle v r : FillStyle c : Fill : Nil)
    Stroke -> stroke ctx
