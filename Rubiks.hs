{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Use <$>" #-}

module Main where

import Data.Coerce (coerce)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Tuple.Optics qualified as Optics
import Optics.Core qualified as Optics
import String.ANSI qualified as ANSI
import GHC.Stack (HasCallStack)

-- TODO later: optimize!
-- https://en.wikipedia.org/wiki/Optimal_solutions_for_the_Rubik%27s_Cube#Kociemba's_algorithm

{- How to talk about a rubik's cube.
 -
 - A cube is comprised of cubies. A cube itself has no orientation, but
 - cubies do. They also have position and stickers. One, two, or three
 - stickers.
 -
 - Orientation means pointing a certain direction. Orientation is relative to an
 - observer, meaning that it is possible to rotate every cubie simultaneously.
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
 - stickers 0, 9, and 57 are part of the cubie at position (-1,1,-1).
 -
 - A cube also has faces and slices, which are the collection of stickers
 - sharing a particular X, Y, or Z coordinate. Rotating a face or slice means
 - rotating and translating the cubies the stickers of the slice are found on.
 -
 - Actually, cubies don't have orientation, either. Stickers do. Same with
 - position. It just so happens that one, two, or three stickers can share the
 - same position. The only problem with this formulation is that it might be
 - hard to generate valid arbitrary scrambles from arbitrary stickers. But
 - that can be tackled by smart constructors, I'm sure.
 -}

-- R is +x
-- U is +y
-- F is +z

data Cube = Cube Int [Sticker] deriving Show
data Sticker = Sticker Color Position Orientation deriving (Show, Eq, Ord)
newtype Position = Position (Int,Int,Int) deriving (Show, Eq, Ord)
newtype Orientation = Orientation (Int,Int,Int) deriving (Show, Eq, Ord)

pattern FaceU, FaceD, FaceF, FaceB, FaceL, FaceR :: Orientation
pattern FaceR = Orientation (1,   0,  0)
pattern FaceL = Orientation (-1,  0,  0)
pattern FaceU = Orientation (0,   1,  0)
pattern FaceD = Orientation (0,  -1,  0)
pattern FaceF = Orientation (0,   0,  1)
pattern FaceB = Orientation (0,   0, -1)

data Color = Red | Green | Blue | Yellow | Orange | White deriving (Show, Eq, Ord)

solved3x3 :: Cube
solved3x3 = solvedNxN 3

solvedNxN :: Int -> Cube
solvedNxN n = Cube n $ front <> back <> left <> right <> up <> down where
    w = n `div` 2
    rng   = [-w..w] List.\\ [0|even n]
    right = [Sticker Green    (Position (w,  y,  z))  FaceR | y <- rng, z <- rng ]
    left  = [Sticker Blue (Position (-w, y,  z))  FaceL | y <- rng, z <- rng ]
    up    = [Sticker White  (Position (x,  w,  z))  FaceU | x <- rng, z <- rng ]
    down  = [Sticker Yellow (Position (x,  -w, z))  FaceD | x <- rng, z <- rng ]
    front = [Sticker Orange  (Position (x,  y,  w))  FaceF | x <- rng, y <- rng ]
    back  = [Sticker Red   (Position (x,  y,  -w)) FaceB | x <- rng, y <- rng ]

ansi :: Color -> String
ansi Red    = ANSI.redBg " "
ansi Green  = ANSI.greenBg " "
ansi Blue   = ANSI.blueBg " "
ansi Yellow = ANSI.rgbBg 255 255 0 " "
ansi Orange = ANSI.rgbBg 255 165 0 " "
ansi White  = ANSI.rgbBg 255 255 255 " "

prettySticker :: Sticker -> String
prettySticker (Sticker c _ _) = ansi c

-- | Given all stickers on a face, put them in a map of locations.
faceMap :: [Sticker] -> Map.Map Position Sticker
faceMap = Map.fromListWithKey spotError . map (\s@(Sticker _ p _) -> (p,s))
    where
    spotError k _ _ = error $ "Duplicate sticker at " <> show k

prettyCube :: HasCallStack => Cube -> [Char]
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

    w = size `div` 2
    pos = [-w..w] List.\\ [0|even size]
    neg = reverse pos

    spaces = replicate size ' '
    space x = spaces <> x

    mkPos a b c = {-traceShowId $-} Position (a,b,c)
    -- pos x, pos z
    up = unlines $ map (space . (\z -> concatMap (\x -> prettySticker $ uStickers Map.! mkPos x w z) pos)) pos
    -- Mirrored. front row (z = -w) is shown first.
    down = unlines $ map (space . (\z -> concatMap (\x -> prettySticker $ dStickers Map.! mkPos x -w z ) pos)) neg
    -- positive z, negative y
    left = map (\y -> concatMap (\z -> prettySticker $ lStickers Map.! mkPos -w y z) pos) neg
    -- neg z, neg y
    right = map (\y -> concatMap (\z -> prettySticker $ rStickers Map.! mkPos w y z) neg) neg
    -- positive x, neg y
    front = map (\y -> concatMap (\x -> prettySticker $ fStickers Map.! mkPos x y w) pos) neg
    -- neg x, neg y
    back = map (\y -> concatMap (\x -> prettySticker $ bStickers Map.! mkPos x y -w) neg) neg

    lf = zipWith (<>) left front
    lfr = zipWith (<>) lf right
    lfrb = unlines $ zipWith (<>) lfr back

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

-- Rotating a sticker means rotating its position and orientation. Rotation
-- happens on an axis and has a magnitude.

data Axis = Rx | Uy | Fz deriving (Show)

-- Default math uses the following formula:
--
-- x' = x * cos phi - y * sin phi
-- y' = x * sin phi + y * cos phi
--
-- That's fine, but we have to negate phi because for a cube, a positive turn is
-- clockwise, not ccw. cos is symmetric around phi so it's just sine that needs
-- to change.

rotate :: Axis -> Int -> (Int,Int,Int) -> (Int,Int,Int)
rotate Rx = rotate' Optics._2 Optics._3
rotate Uy = rotate' Optics._3 Optics._1
rotate Fz = rotate' Optics._1 Optics._2

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


-- Now we can actually rotate a sticker.
rotateSticker :: Axis -> Int -> Sticker -> Sticker
rotateSticker ax mag (Sticker c p o) = Sticker c (coerce rotate ax mag p) (coerce rotate ax mag o)

-- Having done that, we want to rotate a whole slice. A slice is all stickers at
-- a certain position along one axis.

data Slice = Slice Axis Int

rotateSlice :: Slice -> Int -> Cube -> Cube
rotateSlice slice@(Slice ax _) mag = modifySlice (rotateSticker ax mag) slice

modifySlice :: (Sticker -> Sticker) -> Slice -> Cube -> Cube
modifySlice f (Slice ax n) (Cube size stickers) = Cube size stickers'
  where
    stickers' =
        map (\s@(Sticker _ (Position p) _) ->
                if Optics.view (axis ax) p == n then f s else s)
            stickers

axis Rx = Optics._1
axis Uy = Optics._2
axis Fz = Optics._3

data Move = R | L | U | D | F | B
          | R' | L' | U' | D' | F' | B'

move :: Move -> Cube -> Cube
move R  = rotateSlice (Slice Rx 1)  1
move L  = rotateSlice (Slice Rx -1) -1
move U  = rotateSlice (Slice Uy 1)  1
move D  = rotateSlice (Slice Uy -1) -1
move F  = rotateSlice (Slice Fz 1)  1
move B  = rotateSlice (Slice Fz -1) -1
move R' = rotateSlice (Slice Rx 1)  -1
move L' = rotateSlice (Slice Rx -1) 1
move U' = rotateSlice (Slice Uy 1)  -1
move D' = rotateSlice (Slice Uy -1) 1
move F' = rotateSlice (Slice Fz 1)  -1
move B' = rotateSlice (Slice Fz -1) 1

moves :: [Move] -> Cube -> Cube
moves = foldr (flip (.) . move) id

-- | Knowing what move to do next will require finding cubies. We don't
-- actually store cubies -- we store stickers. So we need to make a map of
-- position to stickers, and then find the position that has desired colors.
findCubie :: [Color] -> Cube -> [Position]
findCubie colors (Cube _ stickers) =
    let posMap =
            Map.fromListWith (<>)
                (map (\(Sticker c p _) -> (p, Set.singleton c)) stickers)
    in Map.keys (Map.filter (== Set.fromList colors) posMap)


crossProduct :: (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int)
crossProduct (a_x, a_y, a_z) (b_x, b_y, b_z) = (a_y*b_z - a_z*b_y, a_z*b_x - a_x*b_z, a_x*b_y - a_y*b_x)

-- | The permutation of a corner.
--
-- Clockwise = 1, Counterclockwise = 2, None = 0
--
-- Find orientation with the cross product.
--
-- 1. Find the orientation O of the white or yellow sticker of a cubie at position P
-- 2. Calculate the cross product C of O × P
-- 3. If C_y is zero, permutation is None
--    If C_y is the the same sign as P_y, permutation is Clockwise.
--    Otherwise permutation is Counterclockwise.
cornerPerm :: Cube -> Position -> Int
cornerPerm (Cube _ stickers) p@(Position (_,p_y,_)) =
    let Sticker _ _ o = head $ filter (\(Sticker c p' _) -> c `elem` [Yellow, White] && p' == p) stickers
        (_,c_y,_) = crossProduct (coerce o) (coerce p)
    in case signum c_y of
        0 -> 0
        s -> if s == signum p_y then 2 else 1

totalCornerPerm :: Cube -> Int
totalCornerPerm c@(Cube size _) = sum $
    let w = size `div` 2
        a = [-w,w]
    in [cornerPerm c (Position (x,y,z)) | x <- a, y <- a, z <- a]

main = do
    putStrLn "Rotating centers on their axis doesn't change them:"
    putStr "    Rx: "
    print $ all ((== (1,0,0)) . (\mag -> rotate Rx mag (1,0,0))) [-1..2]
    putStr "    Uy: "
    print $ all ((== (0,1,0)) . (\mag -> rotate Uy mag (0,1,0))) [-1..2]
    putStr "    Fz: "
    print $ all ((== (0,0,1)) . (\mag -> rotate Fz mag (0,0,1))) [-1..2]

    putStr "rotate Rx 1 (1,1,0) == (1,0,-1): "
    print $ rotate Rx 1 (1,1,0) == (1,0,-1)
    putStr "rotate Rx 2 (1,1,0) == (1,-1,0): "
    print $ rotate Rx 2 (1,1,0) == (1,-1,0)
    putStr "rotate Rx -1 (1,1,0) == (1,0,1): "
    print $ rotate Rx -1 (1,1,0) == (1,0,1)

    putStr "rotate Uy 1 (1,1,0) == (0,1,1): "
    print $ rotate Uy 1 (1,1,0) == (0,1,1)
    putStr "rotate Uy 2 (1,1,0) == (-1,1,0): "
    print $ rotate Uy 2 (1,1,0) == (-1,1,0)
    putStr "rotate Uy -1 (1,1,0) == (0,1,-1): "
    print $ rotate Uy -1 (1,1,0) == (0,1,-1)

    putStr "rotate Fz 1 (1,1,0) == (1,-1,0): "
    putStr . show $ rotate Fz 1 (1,1,0)
    putStr ": "
    print $ rotate Fz 1 (1,1,0) == (1,-1,0)
    putStr "rotate Fz 2 (1,1,0) == (-1,-1,0): "
    putStr . show $ rotate Fz 2 (-1,-1,0)
    putStr ": "
    print $ rotate Fz 2 (1,1,0) == (-1,-1,0)
    putStr "rotate Fz -1 (1,1,0) == (-1,1,0): "
    putStr . show $ rotate Fz 3 (1,1,0)
    putStr ": "
    print $ rotate Fz -1 (1,1,0) == (-1,1,0)

    putStr "Null rotation on any axis causes no change: "
    let positions = filter (/= (0,0,0)) [(x,y,z) | x <- [-1..1], y <- [-1..1], z <- [-1..1]]
    print $ and [ c1 == c2 | c1 <- positions, ax <- [Rx,Uy,Fz], let c2 = rotate ax 0 c1 ]

    putStrLn $ prettyCube $ move R' $ move D' $ move B' $ move B $ move D $ move R solved3x3

    putStr "R rotation permutates WGO corner clockwise: "
    print $ cornerPerm (move R solved3x3) (Position (1,1,-1)) == 1
    putStr "R rotation permutates YGO corner counterclockwise: "
    print $ cornerPerm (move R solved3x3) (Position (1,1,1)) == 2
    putStr "totalCornerPerm (moves [R,U,L] solved3x3) == 9: "
    print $ totalCornerPerm (moves [R,U,L] solved3x3) == 9
