{-# LANGUAGE PartialTypeSignatures #-}

module Diagram.Hilbert
    ( diagram
    , hilbert
    )where

import Diagrams.Backend.SVG (B)
import Diagrams.Prelude
import Numeric.Natural (Natural)

diagram :: Diagram B
diagram = hilbert 7

hilbert :: Natural -> Diagram B
hilbert = lw veryThin . pad 1.1 . centerXY . strokeLine . hilbert'

hilbert' :: Natural -> Trail' Line V2 Double
hilbert' 0 = mempty
hilbert' n =    h' # rotateBy (1 / 4)
             <> down
             <> h
             <> right
             <> h
             <> up
             <> h' # rotateBy (-1 / 4)
  where
    h     = hilbert' (pred n) # scale (1 / 2)
    h'    = reversing h
    up    = lineFromOffsets [V2 0 1] # scale (1 / 2 ^ pred n)
    down  = reversing up
    right = up # rotateBy (-1 / 4)
