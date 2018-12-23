{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Diagram.Labyrinth
    ( diagram
    , labyrinth
    )where

import           Control.Monad.Random
import           Data.List (foldl')
import           Data.Maybe (catMaybes)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Diagrams.Backend.SVG (B)
import           Diagrams.Prelude
import           Numeric.Natural (Natural)

diagram :: IO (Diagram B)
diagram = labyrinth 200 100

labyrinth :: forall m. MonadRandom m => Natural -> Natural -> m (Diagram B)
labyrinth w h =
    lw veryThin . pad 1.1 . centerXY . strokeP . toTrail <$> go initialPoints (initialLab w h)
  where
    go :: Set LabPoint -> LabPath -> m LabPath
    go ps l = do
        mp <- pick $ Set.toList ps
        case mp of
            Nothing -> return l
            Just p  -> do
                x <- genPath p l
                let ps' = Set.filter (\q -> not $ x `hasPoint` q) ps
                    l'  = l <> x
                go ps' l'

    initialPoints :: Set LabPoint
    initialPoints = Set.fromList
        [ LabPoint x y
        | x <- [1 .. w - 1]
        , y <- [1 .. h - 1]
        ]

data LabPoint = LabPoint Natural Natural
    deriving (Show, Eq, Ord)

data LabLine = LabLine LabPoint LabPoint
    deriving (Show, Eq, Ord)

labLine :: LabPoint -> LabPoint -> Maybe LabLine
labLine p q
    | p < q     = Just $ LabLine p q
    | p > q     = Just $ LabLine q p
    | otherwise = Nothing

data LabPath = LabPath
    { lpPoints :: !(Set LabPoint)
    , lpLines  :: !(Set LabLine)
    } deriving (Show, Eq, Ord)

hasPoint :: LabPath -> LabPoint -> Bool
hasPoint LabPath{..} p = Set.member p lpPoints

addLine :: LabLine -> LabPath -> LabPath
addLine l@(LabLine p q) LabPath{..} = LabPath
    { lpPoints = Set.insert p $ Set.insert q lpPoints
    , lpLines  = Set.insert l lpLines
    }

addLine' :: LabPoint -> LabPoint -> LabPath -> LabPath
addLine' p q = case labLine p q of
    Nothing -> id
    Just l  -> addLine l

instance Semigroup LabPath where
    x <> LabPath{..} = Set.foldl' (flip addLine) x lpLines

instance Monoid LabPath where
    mempty = LabPath
        { lpPoints = Set.empty
        , lpLines  = Set.empty
        }

fromLines :: [LabLine] -> LabPath
fromLines = foldl' (flip addLine) mempty

initialLab :: Natural -> Natural -> LabPath
initialLab w h = fromLines $ catMaybes $
       [labLine (LabPoint x h) (LabPoint (x + 1) h) | x <- [1 .. w - 1]]
    ++ [labLine (LabPoint x 0) (LabPoint (x + 1) 0) | x <- [0 .. w - 2]]
    ++ [labLine (LabPoint 0 y) (LabPoint 0 (y + 1)) | y <- [0 .. h - 1]]
    ++ [labLine (LabPoint w y) (LabPoint w (y + 1)) | y <- [0 .. h - 1]]

toPoint :: LabPoint -> Point V2 Double
toPoint (LabPoint x y) = fromIntegral x ^& fromIntegral y

toTrail :: LabPath -> Path V2 Double
toTrail lp = mconcat
    [ let p' = toPoint p
      in  (Trail $ lineFromVertices [p', toPoint q]) `pathFromTrailAt` p'
    | LabLine p q <- Set.toList $ lpLines lp
    ]

data LabDirection = North | East | South | West
    deriving (Show, Eq, Ord)

move :: LabDirection -> LabPoint -> LabPoint
move North (LabPoint x y) = LabPoint x       (y + 1)
move East  (LabPoint x y) = LabPoint (x + 1) y
move South (LabPoint x y) = LabPoint x       (y - 1)
move West  (LabPoint x y) = LabPoint (x - 1) y

pick :: MonadRandom m => [a] -> m (Maybe a)
pick [] = return Nothing
pick xs = do
    i <- getRandomR (0, length xs - 1)
    return $ Just $ xs !! i

genPath :: forall m. MonadRandom m => LabPoint -> LabPath -> m LabPath
genPath p l = go mempty p
  where
    go :: LabPath -> LabPoint -> m LabPath
    go x q = do
        let ds = filter (\d -> not $ x `hasPoint` move d q) [North, East, South, West]
        md <- pick ds
        case md of
            Nothing -> genPath p l
            Just d  -> do
                let q' = move d q
                    x' = addLine' q q' x
                if l `hasPoint` q'
                    then return x'
                    else go x' q'
