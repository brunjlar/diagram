{-# LANGUAGE PartialTypeSignatures #-}

module Diagram.Knot
    ( diagram
    , knot
    , Config
    , WallType (..)
    , initialConfig
    , addWall
    )where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
import           Diagrams.Backend.SVG (B)
import           Diagrams.Prelude
import           Numeric.Natural

diagram :: Diagram B
diagram = knot cfg
  where
    cfg = addWall ( 8, 3) Vertical
        $ addWall ( 8, 5) Vertical
        $ addWall ( 2, 1) Vertical
        $ addWall ( 2, 3) Vertical
        $ addWall (14, 5) Vertical
        $ addWall (14, 7) Vertical
        $ addWall ( 5, 6) Horizontal
        $ addWall (11, 2) Horizontal
        $ initialConfig (Width 16) (Height 8)

knot :: Config -> Diagram B
knot cfg =
    let r = getRectangle cfg
        p = getVertices cfg <> getWalls cfg
    in  pad 1.1 $ centerXY $ strokePath p # lw none # fc black # clipBy r

vertex :: PointType -> PointType -> PointType -> PointType -> Trail' Loop V2 Double
vertex n w s e = glueLine $
       mkPoint n
    <> mkPoint w # rotateBy ( 1/4)
    <> mkPoint s # rotateBy ( 1/2)
    <> mkPoint e # rotateBy (-1/4)

data PointType = Capped | LPoint | RPoint | NPoint
    deriving (Show, Eq, Ord)

mkPoint :: PointType -> Trail' Line V2 Double
mkPoint pt = lineFromVertices $ case pt of
    Capped -> [ origin
              , (-1/4) ^& (1/4)
              , (1/4) ^& (1/4)
              , origin
              ]
    LPoint -> [ origin
              , (-1/4) ^& (1/4)
              , (-1/8) ^& (3/8)
              , (-3/8) ^& (5/8)
              , (-1/4) ^& (3/4)
              , 0 ^& (1/2)
              , (1/4) ^& (1/4)
              , origin
              ]
    RPoint -> [ origin
              , (-1/4) ^& (1/4)
              , (1/4) ^& (3/4)
              , (3/8) ^& (5/8)
              , (1/8) ^& (3/8)
              , (1/4) ^& (1/4)
              , origin
              ]
    NPoint -> [ origin
              , (-1/4) ^& (1/4)
              , 0 ^& (1/2)
              , (1/4) ^& (1/4)
              , origin
              ]

data WallType = Horizontal | Vertical
    deriving (Show, Eq, Ord)

newtype Width = Width Natural
    deriving (Show, Eq, Ord)

newtype Height = Height Natural
    deriving (Show, Eq, Ord)

data Config = Config
    { cfgWidth  :: !Width
    , cfgHeight :: !Height
    , cfgWalls  :: !(Map (Natural, Natural) WallType)
    }

initialConfig :: Width -> Height -> Config
initialConfig w@(Width w') h@(Height h')
    | odd w' || w' <= 0 || odd h' || h' <= 0 = error "width and hight must be even and positive"
    | otherwise                              = Config
        { cfgWidth  = w
        , cfgHeight = h
        , cfgWalls  = Map.fromList $
               [((x , 0 ), Horizontal) | x <- [1, 3 .. w']]
            ++ [((x , h'), Horizontal) | x <- [1, 3 .. w']]
            ++ [((0 , y ), Vertical)   | y <- [1, 3 .. h']]
            ++ [((w', y ), Vertical)   | y <- [1, 3 .. h']]
        }

addWall :: (Natural, Natural) -> WallType -> Config -> Config
addWall xy@(x, y) wt cfg
    | x > w || y > h || even (x + y) = error $ "illegal wall position " <> show xy
    | otherwise                      = cfg
        { cfgWalls = Map.insert xy wt $ cfgWalls cfg }
  where
    Width w  = cfgWidth cfg
    Height h = cfgHeight cfg

getPointType :: Config -> (Int, Int) -> (Int, Int) -> PointType
getPointType cfg (x, y) (x', y')
    | wallStart = NPoint
    | facesWall = Capped
    | even x    = RPoint
    | odd x     = LPoint
    | otherwise = NPoint
  where
    wt
        | x' < 0 || y' < 0 = Nothing
        | otherwise        = Map.lookup (fromIntegral x', fromIntegral y') (cfgWalls cfg)

    wallStart = case wt of
        Nothing         -> False
        Just Horizontal -> x /= x'
        Just Vertical   -> y /= y'

    facesWall = isJust wt

getVertex :: Config -> (Int, Int) -> Trail' Loop V2 Double
getVertex cfg (x, y) = vertex
    (getPointType cfg (x, y) (x    , y + 1))
    (getPointType cfg (x, y) (x - 1, y    ))
    (getPointType cfg (x, y) (x    , y - 1))
    (getPointType cfg (x, y) (x + 1, y    ))

getVertices :: Config -> Path V2 Double
getVertices cfg =
    Path [ wrapLoop (getVertex cfg (x, y)) `at` fromIntegral x ^& fromIntegral y
         | (x, y) <-    [(x, y) | x <- [0, 2 .. fromIntegral w], y <- [0, 2 .. fromIntegral h]]
                     ++ [(x, y) | x <- [1, 3 .. fromIntegral w], y <- [1, 3 .. fromIntegral h]]
         ]
  where
    Width w  = cfgWidth cfg
    Height h = cfgHeight cfg

getWall :: WallType -> Trail' Loop V2 Double
getWall wt = glueLine $ case wt of
    Vertical   -> form
    Horizontal -> form # rotateBy (1/4)
  where
    form :: Trail' Line V2 Double
    form = lineFromVertices [ origin
                            , 1/16 ^& 0
                            , 1/16 ^& (-1)
                            , (-1/16) ^& (-1)
                            , (-1/16) ^& 1
                            , 1/16 ^& 1
                            , 1/16 ^& 0
                            , origin
                            ]

getWalls :: Config -> Path V2 Double
getWalls cfg = Path [ wrapLoop (getWall wt) `at` fromIntegral x ^& fromIntegral y
                    | ((x, y), wt) <- Map.toList $ cfgWalls cfg
                    ]

getRectangle :: Config -> Path V2 Double
getRectangle cfg = Path [ wrapLoop (glueLine $ lineFromVertices vs) `at` origin ]
  where
    Width w  = cfgWidth cfg
    Height h = cfgHeight cfg
    w' = fromIntegral w
    h' = fromIntegral h
    vs = [ origin
         , w' ^& 0
         , w' ^& h'
         , 0 ^& h'
         , origin
         ]
