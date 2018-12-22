{-# LANGUAGE PartialTypeSignatures #-}

module Diagram.Koch
    ( diagram
    , koch
    )where

import           Diagrams.Backend.SVG (B)
import           Diagrams.Prelude
import           Numeric.Natural (Natural)

diagram :: Diagram B
diagram =     (koch 0 ||| koch 1 ||| koch 2)
          === (koch 3 ||| koch 4 ||| koch 5)

koch :: Natural -> Diagram B
koch = lw veryThin . pad 1.1 . centerXY . strokeLine . close . go
  where
    go 0 = baseline
    go n = step $ go (n - 1)

baseline :: Trail' Line V2 Double
baseline = lineFromVertices [origin, 1 ^& 0]

step :: Trail' Line V2 Double -> Trail' Line V2 Double
step l =    l'
         <> l' # rotateBy (1 / 6)
         <> l' # rotateBy (5 / 6)
         <> l'
  where l' = l # scale (1 / 3)

close :: Trail' Line V2 Double -> Trail' Line V2 Double
close l =
       l # rotateBy (1 / 6)
    <> l # rotateBy (5 / 6)
    <> l # rotateBy (1 / 2)
