{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

-- import Data.Matrix.Static
import qualified Data.Map as Map
-- import qualified Data.Set as Set

import qualified Data.Tuple.Optics as Optics
import qualified Optics.Core as Optics

import qualified String.ANSI as ANSI

import Debug.Trace

-- TODO later: optimize!
-- https://en.wikipedia.org/wiki/Optimal_solutions_for_the_Rubik%27s_Cube#Kociemba's_algorithm

{- How to talk about a rubik's cube.
 -
 - A cube is comprised of cubelets. A cube itself has no orientation, but
 - cubelets do. They also have position and stickers. One, two, or three
 - stickers.
 -
 - Orientation means pointing a certain direction. Orientation is relative to an
 - observer, meaning that it is possible to rotate every cubelet simultaneously.
 -
 - Positions on a cube start in the center.
 -
 - Given this standard view of a cube,
 -
 -            +----------+
 -            |  0  1  2 |
 -            |  3  4  5 |
 -            |  6  7  8 |
 - +----------+----------+----------+----------+
 - |  9 10 11 | 18 19 20 | 27 28 29 | 45 46 47 |
 - | 12 13 14 | 21 22 23 | 30 31 32 | 48 49 50 |
 - | 15 16 17 | 24 25 26 | 33 34 35 | 51 52 53 |
 - +----------+----------+----------+----------+
 -            | 36 37 38 |
 -            | 39 40 41 |
 -            | 42 43 44 |
 -            +----------+
 - 
 - stickers 0, 9, and 57 are part of the cubelet at position (-1,1,-1).
 -
 - A cube also has faces and slices, which are the collection of stickers
 - sharing a particular X, Y, or Z coordinate. Rotating a face or slice means
 - rotating and translating the cubelets the stickers of the slice are found on.
 -
 - Actually, cubelets don't have orientation, either. Stickers do. Same with
 - position. It just so happens that one, two, or three stickers can share the
 - same position. The only problem with this formulation is that it might be
 - hard to generate valid arbitrary scrambles from arbitrary stickers. But
 - that can be tackled by smart constructors, I'm sure.
 -}

-- R is +x
-- U is +y
-- F is +z

data Cube = Cube Int [Sticker] deriving Show
data Sticker = Sticker Color Position Orientation deriving Show
newtype Position = Position (Int,Int,Int) deriving (Show, Eq, Ord)
newtype Orientation = Orientation (Int,Int,Int) deriving (Show, Eq, Ord)

pattern FaceU, FaceD, FaceF, FaceB, FaceL, FaceR :: Orientation
pattern FaceU = Orientation (0, 0,-1)
pattern FaceD = Orientation (0, 0,1)
pattern FaceF = Orientation (-1, 0,0)
pattern FaceB = Orientation (1, 0,0)
pattern FaceL = Orientation (0, -1,0)
pattern FaceR = Orientation (0, 1,0)

data Color = Red | Green | Blue | Yellow | Orange | White deriving (Show)

solved3x3 :: Cube
solved3x3 = Cube 3 $ front <> back <> left <> right <> up <> down where
    rng   = [-1..1]
    up    = [ Sticker White  (Position (x, -1, z)) FaceU | x <- rng, z <- rng ]
    down  = [ Sticker Yellow (Position (x, 1, z)) FaceD | x <- rng, z <- rng ]
    left  = [ Sticker Orange (Position (-1, y, z)) FaceL | y <- rng, z <- rng ]
    right = [ Sticker Red    (Position (1, y, z)) FaceR | y <- rng, z <- rng ]
    front = [ Sticker Blue   (Position (x, y, -1)) FaceF | x <- rng, y <- rng ]
    back  = [ Sticker Green  (Position (x, y, 1)) FaceB | x <- rng, y <- rng ]



ansi :: Color -> String
ansi Red    = ANSI.redBg " "
ansi Green  = ANSI.greenBg " "
ansi Blue   = ANSI.blueBg " "
ansi Yellow = ANSI.rgbBg 255 255 0 " "
ansi Orange = ANSI.rgbBg 255 165 0 " "
ansi White  = ANSI.rgbBg 255 255 255 " "

prettySticker :: Sticker -> String
prettySticker (Sticker c _ _) = ansi c

prettyCube :: Cube -> [Char]
prettyCube (Cube size stickers) = concat
    [ up
    , lfrb
    , down
    ]
    where

    uStickers = faceMap $ filter (\(Sticker _ _ o) -> o == FaceU) stickers
    dStickers = faceMap $ filter (\(Sticker _ _ o) -> o == FaceD) stickers
    lStickers = faceMap $ filter (\(Sticker _ _ o) -> o == FaceL) stickers
    rStickers = faceMap $ filter (\(Sticker _ _ o) -> o == FaceR) stickers
    fStickers = faceMap $ filter (\(Sticker _ _ o) -> o == FaceF) stickers
    bStickers = faceMap $ filter (\(Sticker _ _ o) -> o == FaceB) stickers

    -- | Given all stickers on a face, put them in a map of locations. Coordinates
    -- are relative to which face has been selected.
    faceMap :: [Sticker] -> Map.Map Position Sticker
    faceMap = Map.fromList . map (\s@(Sticker _ p _) -> (p,s))

    w = size `div` 2
    pos = [-w..w]
    neg = reverse pos

    spaces = replicate size ' '
    space x = spaces <> x

    -- The back row has z = 2 and is shown on top.
    up = unlines $ map (space . (\z -> concatMap (\x -> prettySticker $ uStickers Map.! Position (x, -w, z)) pos)) neg
    -- Mirrored. front row (z = -w) is shown first.
    down = unlines $ map (space . (\z -> concatMap (\x -> prettySticker $ dStickers Map.! Position (x, w, z)) pos)) pos

    -- negative z, positive y
    left = map (\y -> concatMap (\z -> prettySticker $ lStickers Map.! Position (-w, y, z)) neg) pos
    -- positive z, positive y
    right = map (\y -> concatMap (\z -> prettySticker $ rStickers Map.! Position (w, y, z)) pos) pos
    -- positive x, positive y
    front = map (\y -> concatMap (\x -> prettySticker $ fStickers Map.! Position (x, y, -w)) pos) pos
    -- negative x, positive y
    back = map (\y -> concatMap (\x -> prettySticker $ bStickers Map.! Position (x, y, w)) neg) pos

    lf = zipWith (<>) left front
    lfr = zipWith (<>) lf right
    lfrb = unlines $ zipWith (<>) lfr back

-- data Move = Ui Int | U'i Int | U2i Int
-- 
-- pattern U = Ui 1
-- pattern U' = U'i 1
-- pattern U2 = U2i 1

-- R is +x
-- U is +y
-- F is +z


{- Modifying a Rubik's cube
 -
 - Start with R as an example. This rotates the R face clockwise. It affects
 - stickers with position x = 1.
 -
 - Rotation: All stickers rotate around the x axis. FaceB becomes FaceF and so
 - on.
 -
 - Translation: a face at (1, 1, 1) moves to (1, 0, -1).
 -
 - Oh yeah, I remember now: it's sine and cosine.
 -}

pattern Coord :: Int -> Int -> Int -> (Int, Int, Int)
pattern Coord { cx, cy, cz } = (cx, cy, cz)

-- Rotating a sticker means rotating its position and orientation. Rotation
-- happens on an axis and has a magnitude.

data Axis = R | U | F deriving (Show)

-- Default math uses the following formula:
--
-- x' = x * cos phi - y * sin phi
-- y' = x * sin phi + y * cos phi
--
-- That's fine, but we have to negate phi because for a cube, a positive turn is
-- clockwise, not ccw. cos is symmetric around phi so it's just sine that needs
-- to change.

rotate :: Axis -> Int -> (Int,Int,Int) -> (Int,Int,Int)
rotate R = rotate' Optics._2 Optics._3
rotate U = rotate' Optics._3 Optics._1
rotate F = rotate' Optics._1 Optics._2

rotate' ax1 ax2 n coord =
    let val = Optics.view ax1 coord
        val2 = Optics.view ax2 coord
        val' = val * cosine n - val2 * sine n
        val2' = val * sine n + val2 * cosine n
    in Optics.set ax1 val' $ Optics.set ax2 val2' coord

-- Multiples of pi/2.
-- Sine is inverted to take CW as positive into account.
sine, cosine :: Int -> Int

sine 0 = 0
sine 1 = -1
sine 2 = 0
sine 3 = 1
sine n = sine (n `mod` 4)

cosine 0 = 1
cosine 1 = 0
cosine 2 = -1
cosine 3 = 0
cosine n = cosine (n `mod` 4)

main = do
    putStrLn "Rotating centers on their axis doesn't change them:"
    putStr "    R: "
    print $ all ((== (1,0,0)) . (\mag -> rotate R mag (1,0,0))) [-1..2]
    putStr "    U: "
    print $ all ((== (0,1,0)) . (\mag -> rotate U mag (0,1,0))) [-1..2]
    putStr "    F: "
    print $ all ((== (0,0,1)) . (\mag -> rotate F mag (0,0,1))) [-1..2]

    putStr "rotate R 1 (1,1,0) == (1,0,-1): "
    print $ rotate R 1 (1,1,0) == (1,0,-1)
    putStr "rotate R 2 (1,1,0) == (1,-1,0): "
    print $ rotate R 2 (1,1,0) == (1,-1,0)
    putStr "rotate R (-1) (1,1,0) == (1,0,1): "
    print $ rotate R (-1) (1,1,0) == (1,0,1)

    putStr "rotate U 1 (1,1,0) == (0,1,1): "
    print $ rotate U 1 (1,1,0) == (0,1,1)
    putStr "rotate U 2 (1,1,0) == (-1,1,0): "
    print $ rotate U 2 (1,1,0) == (-1,1,0)
    putStr "rotate U (-1) (1,1,0) == (0,1,-1): "
    print $ rotate U (-1) (1,1,0) == (0,1,-1)

    putStr "rotate F 1 (1,1,0) == (0,-1,0): "
    print $ rotate F 1 (1,1,0) == (0,-1,0)
    putStr "rotate F 2 (1,1,0) == (-1,1,0): "
    print $ rotate F 2 (1,1,0) == (-1,1,0)
    putStr "rotate F (-1) (1,1,0) == (0,1,-1): "
    print $ rotate F (-1) (1,1,0) == (0,1,-1)

    putStr "Null rotation on any axis causes no change: "
    let positions = filter (/= (0,0,0)) [Coord x y z | x <- [-1..1], y <- [-1..1], z <- [-1..1]]
    print $ and [ c1 == c2 | c1 <- positions, ax <- [R,U,F], let c2 = rotate ax 0 c1 ]
