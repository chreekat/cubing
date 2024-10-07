{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Coerce (coerce)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Tuple.Optics qualified as Optics
import Optics.Core qualified as Optics
import String.ANSI qualified as ANSI
import System.Random qualified as Rand

import Debug.Trace
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
    rng   = [-w..w]
    right = [Sticker Red    (Position (w,  y,  z))  FaceR | y <- rng, z <- rng ]
    left  = [Sticker Orange (Position (-w, y,  z))  FaceL | y <- rng, z <- rng ]
    up    = [Sticker White  (Position (x,  w,  z))  FaceU | x <- rng, z <- rng ]
    down  = [Sticker Yellow (Position (x,  -w, z))  FaceD | x <- rng, z <- rng ]
    front = [Sticker Green  (Position (x,  y,  w))  FaceF | x <- rng, y <- rng ]
    back  = [Sticker Blue   (Position (x,  y,  -w)) FaceB | x <- rng, y <- rng ]


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
    pos = [-w..w]
    neg = reverse pos

    spaces = replicate size ' '
    space x = spaces <> x

    halp a b c = {-traceShowId-} (Position (a,b,c))
    -- pos x, pos z
    up = unlines $ map (space . (\z -> concatMap (\x -> prettySticker $ uStickers Map.! halp x w z) pos)) pos
    -- Mirrored. front row (z = -w) is shown first.
    down = unlines $ map (space . (\z -> concatMap (\x -> prettySticker $ dStickers Map.! halp x -w z ) pos)) neg

    -- positive z, negative y
    left = map (\y -> concatMap (\z -> prettySticker $ lStickers Map.! halp -w y z) pos) neg
    -- neg z, neg y
    right = map (\y -> concatMap (\z -> prettySticker $ rStickers Map.! halp w y z) neg) neg
    -- positive x, neg y
    front = map (\y -> concatMap (\x -> prettySticker $ fStickers Map.! halp x y w) pos) neg
    -- neg x, neg y
    back = map (\y -> concatMap (\x -> prettySticker $ bStickers Map.! halp x y -w) neg) neg

    lf = zipWith (<>) left front
    lfr = zipWith (<>) lf right
    lfrb = unlines $ zipWith (<>) lfr back

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

-- | Knowing what move to do next will require finding cubies. We don't
-- actually store cubies -- we store stickers. So we need to make a map of
-- position to stickers, and then find the position that has
findCubie :: [Color] -> Cube -> [Position]
findCubie colors (Cube _ stickers) =
    let posMap =
            Map.fromListWith (<>)
                (map (\(Sticker c p _) -> (p, Set.singleton c)) stickers)
    in Map.keys (Map.filter (== Set.fromList colors) posMap)


-- Next thing to do is generate a valid solvable scrambled cube.
-- First, a scramble, which may not be solvable.
-- To scramble, we need a list of cubies, which will then place randomly. A list
-- is easy -- we already did that in findCubie. But hang on, we also need to
-- keep the correct handedness of the corners, so we can't lose track of that.
-- Now I need to think of a way to represent a corner cubie that maintains handedness. If I stick to 3d, I can hard-code the creation of the 8 cubies like this:
corners = [[White,Red,Green],[White,Green,Orange],[White,Orange,Blue],[White,Blue,Red],[Yellow,Red,Blue],[Yellow,Blue,Orange],[Yellow,Orange,Green],[Yellow,Green,Red]]

-- If I wanted to support more dimensions, what would I do? Let's think about
-- that later. Edges are easier, they can't get messed up, though their number
-- changes based on the size of the cube:
edges n =
    concat $ replicate (4 - n) [[White,Red],[White,Green],[White,Orange],[White,Blue],[Yellow,Red],[Yellow,Green],[Yellow,Orange],[Yellow,Blue],[Red,Blue],[Green,Red],[Orange,Green],[Blue,Orange]]

faces n =
    let numFaces = (n - 2) ^ 2
    in concat $ replicate numFaces [[Red],[Green],[Orange],[Blue],[White],[Yellow]]

-- For corners, we favor the y axis (white/orange). A cubie can be rotated 0, 1,
-- or 2, which causes its favored sticker (head of the list) to be parallel to
-- the y axis, rotated 120 clockwise, or rotated 120 counterclockwise. Given a
-- position for the cubie and its rotation, we can calculate the orientation of
-- its stickers. E.g. Position = (1,1,1) and rotation = 1 on cubie
-- [Yellow,Blue,Orange] means Yellow has orientation (1,0,0), Blue has
-- orientation (0,0,1), and Orange has orientation (0,1,0). I presume there's a
-- matrix equation we can derive. What about in polar coordinates? Corner
-- positions are φ and θ, rotation is σ. Well, we were already using polar for
-- rotation. I don't see this helping. Let's stick to cartesian.
--
-- Check it: Symmetry.
--
-- At Position = (1,1,1), the orientations are Uy, Rx, Fz. Rotate these around
-- the axes to get orientations for the other positions. (1,1,-1) is rotation -1
-- around the y axis, which gives Uy, -Fz, Rx. We can already do this with
-- rotateSticker. So: start all cubies at 1,1,1. Independently rotate the cubies
-- 0,1, or 2. zip them with a list of all possible rotations to slide them into
-- their correct places.

-- | How to walk to all the corners, starting from (1,1,1)
cornerWalk = [[], [Uy],[Uy,Uy],[Uy,Uy,Uy],[Fz],[Fz,Uy],[Fz,Uy,Uy],[Fz,Uy,Uy,Uy]]


-- | Rotate a corner cubie.
rotateCorner :: Int -> [Color] -> [Color]
rotateCorner n = take 3 . drop n . cycle

randomRotateCorner :: [Color] -> IO [Color]
randomRotateCorner c = do
    n <- Rand.randomRIO (0,2)
    pure $ rotateCorner n c


-- So far a corner has been represented as 3 colors positioned at (1,1,1) and
-- oriented [Uy, Rx, Fz] respectively. Now let's turn 3 colors into Stickers.
cornerColorsAsStickers :: [Color] -> [Sticker]
cornerColorsAsStickers = zipWith toSticker [Uy, Rx, Fz] where
    toSticker o c = Sticker c (Position (1,1,1)) o

walkCorner sticker rotations = foldr (\s o -> rotateSticker o 1 s) sticker rotations

randomCorners :: IO [Sticker]
randomCorners = do
    randCorners <-
        (pure . cornerColorsAsStickers) =<< shuffle =<< mapM randomRotateCorner corners
    pure (zipWith walkCorner randcorners cornerWalk)

shuffle xs g =
    let ns :: [Int] = take (length xs) $ Rand.randoms g
    in map snd $ List.sortOn fst $ zip ns xs

randomCube :: Int -> Cube
randomCube n =
    let c@(Cube _ stickers) = solvedNxN n
        cubies = Map.fromListWith (<>) $
            map (\sticker@(Sticker _ p _) -> (p, Set.singleton sticker)) $ stickers
    in traceShow cubies c

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
