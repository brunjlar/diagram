module Diagram.Sierpinski
    ( diagram
    )where

import           Diagrams.Backend.SVG (B)
import           Diagrams.Prelude
import           Numeric.Natural (Natural)

diagram :: Diagram B
diagram = sierpinski 8

sierpinski :: Natural -> Diagram B
sierpinski = pad 1.1 . go
  where
    go 0 = base
    go n = step $ go (n - 1)

base :: Diagram B
base = regPoly 3 1 # lw veryThin

step :: Diagram B -> Diagram B
step d =
    (d === ((d ||| d) # translateX (- 0.5))) # translateY (2 / 3 * height d) # scale (1 / 2)
    -- old height : h + h / 3 = (4 / 3) h
    -- new height : (2 / 3) h
    -- diff : (4 / 3) h - (2 / 3) h = (2 / 3) h
