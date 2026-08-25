{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Use <$>" #-}

module Main where

import Data.Function qualified as Fun
-- import Control.Monad qualified as Monad
-- import Data.Coerce (coerce)
-- import Data.Foldable qualified as Fold
-- import Data.List qualified as List
-- import Data.Maybe qualified as Maybe
-- import Data.Map qualified as Map
-- import Data.Set qualified as Set
-- import Data.Tuple.Optics qualified as Optics
-- import Data.Vector qualified as Vector
-- import GHC.Stack (HasCallStack)
-- import Optics.Core qualified as Optics
-- import String.ANSI qualified as ANSI
import System.Random (Uniform)
import System.Random qualified as Random
-- import System.Random.Shuffle qualified as Random
import GHC.Generics (Generic)

-- import Debug.Pretty.Simple

-- TODO later: optimize!
-- https://en.wikipedia.org/wiki/Optimal_solutions_for_the_Rubik%27s_Cube#Kociemba's_algorithm

-- R is +x
-- U is +y
-- F is +z

data Cube = Cube
    { corners :: [Int]
    , cornerOrientations :: [Int]
    , edges :: [Int]
    , edgeOrientations :: [Int]
    } deriving (Show, Eq)

solved :: Cube
solved = Cube
    (take 8 [0 ..])
    (take 8 (repeat 0))
    (take 12 [0 ..])
    (take 12 (repeat 0))

data Move = R  | L  | U  | D  | F  | B
          | R' | L' | U' | D' | F' | B'
          | R2 | L2 | U2 | D2 | F2 | B2
          | X  | Y  | Z
          | X' | Y' | Z'
          | X2 | Y2 | Z2
          -- slice moves, wide moves, .... Should some of these just be pattern
          -- synonyms on [Move]? Interesting to see how the choices affect
          -- solutions.
          deriving (Show, Eq, Ord, Enum, Bounded, Generic, Uniform, Random.UniformRange, Random.Random)


data Corner = URF | UFL | ULB | UBR | DFR | DLF | DBL | DRB
    deriving (Show, Eq, Ord, Enum, Bounded)

data Edge = UR | UF | UL | UB | DR | DF | DL | DB | FR | FL | BL | BR
    deriving (Show, Eq, Ord, Enum, Bounded)


-- | Pull, then orient:
--
-- corners[i] is the cubie that gets pulled into slot i.
-- orientation[i] is the orientation that gets applied to the cubie after it
-- gets pulled into slot i.
asCube :: Move -> Cube

-- Two edge rotations written by hand
asCube R = Cube [4, 1, 2, 0, 7, 5, 6, 3] [1, 0, 0, 2, 2, 0, 0, 1]  [8, 1, 2, 3, 11, 5, 6, 7, 4, 9, 10, 0] (repeat 0)
asCube U = Cube [3, 0, 1, 2, 4, 5, 6, 7] (repeat 0)                [3, 0, 1, 2, 4, 5, 6, 7, 8, 9, 10, 11] (repeat 0)

-- Three cube rotations written by hand
asCube X = Cube [4, 5, 1, 0, 7, 6, 2, 3] [1, 2, 1, 2, 2, 1, 2, 1]  [8, 5, 9, 1, 11, 7, 10, 3, 4, 6, 2, 0] [0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0]
asCube Y = Cube [3, 0, 1, 2, 7, 4, 5, 6] (repeat 0)                [3, 0, 1, 2, 7, 4, 5, 6, 11, 8, 9, 10] (repeat 1)
asCube Z = Cube [1, 5, 6, 2, 0, 4, 7, 3] [2, 1, 2, 1, 1, 2, 1, 2]  [2, 9, 6, 10, 0, 8, 4, 11, 1, 5, 7, 3] (repeat 1)

-- The rest of the edge rotations, as cube rotation + edge rotation
asCube L = apply Y2 . apply R . apply Y2 $ solved
asCube D = apply X2 . apply U . apply X2 $ solved
asCube F = apply Y  . apply R . apply Y' $ solved
asCube B = apply Y' . apply R . apply Y  $ solved

-- The human conveniences
asCube R' = applyN R 3 solved
asCube L' = applyN L 3 solved
asCube U' = applyN U 3 solved
asCube D' = applyN D 3 solved
asCube F' = applyN F 3 solved
asCube B' = applyN B 3 solved
asCube X' = applyN X 3 solved
asCube Y' = applyN Y 3 solved
asCube Z' = applyN Z 3 solved
asCube R2 = applyN R 2 solved
asCube L2 = applyN L 2 solved
asCube U2 = applyN U 2 solved
asCube D2 = applyN D 2 solved
asCube F2 = applyN F 2 solved
asCube B2 = applyN B 2 solved
asCube X2 = applyN X 2 solved
asCube Y2 = applyN Y 2 solved
asCube Z2 = applyN Z 2 solved

-- As stated obove, a move is "pull, then orient".
-- orientation of cubie at i is:
--     orientation of cubie at move.corners[i], eli old.cornerOrientations[move.corners[i]]
--     plus move.cornerOrientations[i]
--     mod 3
-- and can be expressed as zipWith over move.corners and move.cornerOrientations
apply :: Move -> Cube -> Cube
apply (asCube -> move) old = Cube
    { corners = map (old.corners !!) move.corners
    , cornerOrientations = zipWith (\moveCorner moveOrientation ->
        (old.cornerOrientations !! moveCorner + moveOrientation) `mod` 3)
        move.corners
        move.cornerOrientations
    , edges = map (old.edges !!) move.edges
    , edgeOrientations = zipWith (\moveEdge moveOrientation ->
        (old.edgeOrientations !! moveEdge + moveOrientation) `mod` 2)
        move.edges
        move.edgeOrientations
    }

applyN :: Move -> Int -> Cube -> Cube
applyN x n c = iterate (apply x) c !! n

check :: Move -> Cube
check x = applyN x 4 solved

-- Next up is permutation parity.
--
-- First, calculate listPerms.
listPerms :: [Int] -> Int
listPerms [] = 0
listPerms (x:xs) = elemPerms x xs + listPerms xs where
    elemPerms z = sum . map (\y -> if z > y then 1 else 0)

-- Now we can check if a cube is solvable!
solvable :: Cube -> Bool
solvable c = and
    [ sum c.cornerOrientations `mod` 3 == 0
    , even $ sum c.edgeOrientations
    , ((==) `Fun.on` even) (listPerms c.corners) (listPerms c.edges)
    ]
--     even (totalEdgeParity c)
--     && even (permutationParity c)
--     && totalCornerParity c `mod` 3 == 0

data Color = Red | Green | Blue | Yellow | Orange | White deriving (Show, Eq, Ord)

main :: IO ()
main = do
    putStrLn "cubectl activated.\n"
    putStr "---- All moves x4 return to original: "
    print $ all (\x -> solved == check x) [minBound ..]
    putStr "---- All face moves result in a solvable cube: "
    print $ all (\x -> solvable (apply x solved)) [R .. B2]
