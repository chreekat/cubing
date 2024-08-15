{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
-- import Data.Matrix.Static
import qualified Data.Map as Map
-- import qualified Data.Set as Set

import qualified String.ANSI as ANSI

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
 - Positions on a cube start in one corner (0,0,0) and span to (N,N,N) on an
 - N+1-cube. I.e. (2,2,2) on a classic 3x3.
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
 - stickers 0, 9, and 57 are part of the cubelet at position (0,0,0).
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

data Cube = Cube Int [Sticker] deriving Show
data Sticker = Sticker Color Position Orientation deriving Show
data Position = Position Int Int Int deriving (Show, Eq, Ord)
data Orientation = FaceU | FaceD | FaceF | FaceB | FaceL | FaceR deriving (Show, Eq)
data Color = Red | Green | Blue | Yellow | Orange | White deriving (Show)

solved3x3 :: Cube
solved3x3 = Cube 3 $ front <> back <> left <> right <> up <> down where
    up = [ Sticker White (Position x 0 z) FaceU | x <- [0..2], z <- [0..2] ]
    down = [ Sticker Yellow (Position x 2 z) FaceD | x <- [0..2], z <- [0..2] ]
    left = [ Sticker Orange (Position 0 y z) FaceL | y <- [0..2], z <- [0..2] ]
    right = [ Sticker Red (Position 2 y z) FaceR | y <- [0..2], z <- [0..2] ]
    front = [ Sticker Blue (Position x y 0) FaceF | x <- [0..2], y <- [0..2] ]
    back = [ Sticker Green (Position x y 2) FaceB | x <- [0..2], y <- [0..2] ]



ansi :: Color -> String
ansi Red    = ANSI.redBg " "
ansi Green  = ANSI.greenBg " "
ansi Blue   = ANSI.blueBg " "
ansi Yellow = ANSI.rgbBg 255 255 0 " "
ansi Orange = ANSI.rgbBg 255 165 0 " "
ansi White  = ANSI.rgbBg 255 255 255 " "

prettySticker (Sticker c _ _) = ansi c

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

    pos = [0 .. size-1]
    neg = reverse pos

    spaces = replicate size ' '

    space x = spaces <> x
    -- The back row has z = 2 and is shown on top.
    up = unlines $ map space $ map (\z -> concatMap (\x -> prettySticker $ uStickers Map.! (Position x 0 z)) pos) neg
    -- Mirrored. front row (z = 0) is shown first.
    down = unlines $ map space $ map (\z -> concatMap (\x -> prettySticker $ dStickers Map.! (Position x 2 z)) pos) pos

    -- negative z, positive y
    left = map (\y -> concatMap (\z -> prettySticker $ lStickers Map.! (Position 0 y z)) neg) pos
    -- positive z, positive y
    right = map (\y -> concatMap (\z -> prettySticker $ rStickers Map.! (Position 2 y z)) pos) pos
    -- positive x, positive y
    front = map (\y -> concatMap (\x -> prettySticker $ fStickers Map.! (Position x y 0)) pos) pos
    -- negative x, positive y
    back = map (\y -> concatMap (\x -> prettySticker $ bStickers Map.! (Position x y 2)) neg) pos

    lf = zipWith (<>) left front
    lfr = zipWith (<>) lf right
    lfrb = unlines $ zipWith (<>) lfr back




-- | Given all stickers on a face, put them in a map of locations. Coordinates
-- are relative to which face has been selected.
faceMap :: [Sticker] -> Map.Map Position Sticker
faceMap stickers = Map.fromList $ map (\s@(Sticker _ p _) -> (p,s)) stickers

-- x is on R
-- y is on U
-- z is on F

-- data Move = Ui Int | U'i Int | U2i Int
-- 
-- pattern U = Ui 1
-- pattern U' = U'i 1
-- pattern U2 = U2i 1

