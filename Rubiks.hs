{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Use <$>" #-}

module Main where

import Control.Monad qualified as Monad
import Data.Coerce (coerce)
import Data.Foldable qualified as Fold
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Tuple.Optics qualified as Optics
import Data.Vector qualified as Vector
import GHC.Stack (HasCallStack)
import Optics.Core qualified as Optics
import String.ANSI qualified as ANSI
import System.Random (Uniform)
import System.Random qualified as Random
import System.Random.Shuffle qualified as Random
import GHC.Generics (Generic)

import Debug.Pretty.Simple

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

data Cube = Cube
    { cubeSize :: Word
    , cubeStickers :: [Sticker]
    , cubePositions :: [CubieIndex]
    } deriving Show
data Sticker = Sticker Color Position Orientation deriving (Show, Eq, Ord)
type CubieIndex = Position

newtype Position = Position (Int,Int,Int) deriving (Show, Eq, Ord)
newtype Orientation = Orientation (Int,Int,Int) deriving (Show, Eq, Ord)

{-# COMPLETE FaceU, FaceD, FaceF, FaceB, FaceL, FaceR #-}
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

cubiePositions :: Word -> [Position]
cubiePositions size =
    let w = fromIntegral size `div` 2
        rng = [-w..w] List.\\ [0 | even size]
        -- Center postions have only one nonzero element
        isCenter = (<= 1) . length . filter (/= 0)
    in [Position (x,y,z) | x <- rng, y <- rng, z <- rng, not $ isCenter [x,y,z]]

solvedNxN :: Word -> Cube
solvedNxN size = Cube size colorList indexList where
    w = fromIntegral size `div` 2
    rng   = [-w..w] List.\\ [0|even size]
    right = [Sticker Green  (Position (w,  y,  z))  FaceR | y <- rng, z <- rng ]
    left  = [Sticker Blue   (Position (-w, y,  z))  FaceL | y <- rng, z <- rng ]
    up    = [Sticker White  (Position (x,  w,  z))  FaceU | x <- rng, z <- rng ]
    down  = [Sticker Yellow (Position (x,  -w, z))  FaceD | x <- rng, z <- rng ]
    front = [Sticker Orange (Position (x,  y,  w))  FaceF | x <- rng, y <- rng ]
    back  = [Sticker Red    (Position (x,  y,  -w)) FaceB | x <- rng, y <- rng ]
    colorList = front <> back <> left <> right <> up <> down
    indexList = cubiePositions size

ansi :: Color -> String
ansi Red    = ANSI.redBg (ANSI.red "R")
ansi Green  = ANSI.greenBg (ANSI.green "G")
ansi Blue   = ANSI.blueBg (ANSI.blue "B")
ansi Yellow = ANSI.rgbBg 255 255 0 (ANSI.rgb 255 255 0 "Y")
ansi Orange = ANSI.rgbBg 255 165 0 (ANSI.rgb 255 165 0 "O")
ansi White  = ANSI.rgbBg 255 255 255 (ANSI.rgb 255 255 255 "W")

prettySticker :: Sticker -> String
prettySticker (Sticker c _ _) = ansi c

-- | Given all stickers on a face, put them in a map of locations.
faceMap :: [Sticker] -> Map.Map Position Sticker
faceMap = Map.fromListWithKey spotError . map (\s@(Sticker _ p _) -> (p,s))
    where
    spotError k _ _ = error $ "Duplicate sticker at " <> show k

prettyCube :: HasCallStack => Cube -> [Char]
prettyCube (Cube size stickers idxs) = concat
    [ up
    , lfrb
    , down
    , indices
    ]
    where

    indices = show $ map snd $ List.sortOn fst $ zip idxs [0..]
    uStickers = faceMap $ filter (\(Sticker _ _ o) -> o == FaceU) stickers
    dStickers = faceMap $ filter (\(Sticker _ _ o) -> o == FaceD) stickers
    lStickers = faceMap $ filter (\(Sticker _ _ o) -> o == FaceL) stickers
    rStickers = faceMap $ filter (\(Sticker _ _ o) -> o == FaceR) stickers
    fStickers = faceMap $ filter (\(Sticker _ _ o) -> o == FaceF) stickers
    bStickers = faceMap $ filter (\(Sticker _ _ o) -> o == FaceB) stickers

    w = fromIntegral size `div` 2
    pos = [-w..w] List.\\ [0|even size]
    neg = reverse pos

    spaces = replicate (fromIntegral size) ' '
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

printCube = putStrLn . prettyCube

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

data Axis = Rx | Uy | Fz deriving (Show, Eq)

-- Default math uses the following formula:
--
-- x' = x * cos phi - y * sin phi
-- y' = x * sin phi + y * cos phi
--
-- That's fine, but we have to negate phi because for a cube, a positive turn is
-- clockwise, not ccw. cos is symmetric around phi so it's just sine that needs
-- to change.

-- Radius in multiples of pi/2
rotate :: Axis -> Int -> (Int,Int,Int) -> (Int,Int,Int)
rotate Rx = rotate' Optics._2 Optics._3
rotate Uy = rotate' Optics._3 Optics._1
rotate Fz = rotate' Optics._1 Optics._2

rotate' ax1 ax2 p coord =
    let r = pi / 2 * fromIntegral p
        val = fromIntegral $ Optics.view ax1 coord
        val2 = fromIntegral $ Optics.view ax2 coord
        val' = round $ val * cos r + val2 * sin r
        val2' = round $ -val * sin r + val2 * cos r
    in Optics.set ax1 val' $ Optics.set ax2 val2' coord

-- Now we can actually rotate a sticker.
rotateSticker :: Axis -> Int -> Sticker -> Sticker
rotateSticker ax mag (Sticker c p o) = Sticker c (coerce rotate ax mag p) (coerce rotate ax mag o)

-- And a CubieIndex
rotateCubieIndex :: Axis -> Int -> CubieIndex -> CubieIndex
rotateCubieIndex = coerce rotate

-- Having done that, we want to rotate a whole slice. A slice is all
-- stickers/cubies at a certain position along one axis.

data Slice = Slice Axis Int

rotateSlice (Slice ax n) mag (Cube size stickers idxs) = Cube size stickers' idxs'
  where
    rotStick = rotateSticker ax mag
    rotCubIdx = rotateCubieIndex ax mag
    stickers' =
        map (\s@(Sticker _ (Position p) _) ->
                if Optics.view (axis ax) p == n then rotStick s else s)
            stickers
    idxs' =
        map (\idx@(Position p) -> if Optics.view (axis ax) p == n then rotCubIdx idx else idx)
            idxs

axis Rx = Optics._1
axis Uy = Optics._2
axis Fz = Optics._3

data Move = R | L | U | D | F | B
          | R' | L' | U' | D' | F' | B'
          deriving (Show, Generic, Uniform, Random.UniformRange, Random.Random)


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

-- Sanity check: The list of cubie indices corresponding to the same list of
-- stickers should always be the same. So let's find a cubie based on colors so
-- we can compare a cube at different states of scramble to double check the
-- above functions.
findCubie :: [Color] -> Cube -> [CubieIndex]
findCubie colors (Cube _ stickers idx) =
    let posMap =
            Map.fromListWith (<>)
                (map (\(Sticker c p _) -> (p, Set.singleton c)) stickers)
        positions = Map.keys (Map.filter (== Set.fromList colors) posMap)
    in filter (`elem` positions) idx


crossProduct :: (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int)
crossProduct (a_x, a_y, a_z) (b_x, b_y, b_z) = (a_y*b_z - a_z*b_y, a_z*b_x - a_x*b_z, a_x*b_y - a_y*b_x)

-- | The parity of a corner.
--
-- Clockwise = 1, Counterclockwise = 2, None = 0
--
-- Find orientation with the cross product.
--
-- 1. Find the orientation O of the white or yellow sticker of a cubie at position P
-- 2. Calculate the cross product C of O × P
-- 3. If C_y is zero, parity is None
--    If C_y is the the same sign as P_y, parity is Clockwise.
--    Otherwise parity is Counterclockwise.
cornerParity :: Cube -> Position -> Int
cornerParity (Cube _ stickers _) p@(Position (_,p_y,_)) =
    let Sticker _ _ o = head $ filter (\(Sticker c p' _) -> colorAxis c == Uy && p' == p) stickers
        (_,c_y,_) = crossProduct (coerce o) (coerce p)
    in case signum c_y of
        0 -> 0
        s -> if s == signum p_y then 2 else 1

totalCornerParity :: Cube -> Int
totalCornerParity c@(Cube size _ _) = sum (map (cornerParity c) (cornerPositions size))

cornerPositions :: Word -> [Position]
cornerPositions size =
    let w = fromIntegral size `div` 2
        a = [-w,w]
    in [Position (x,y,z) | x <- a, y <- a, z <- a]

-- Next up is permutation parity.
--
-- First, calculate listPerms.
listPerms [] = 0
listPerms (x:xs) = elemPerms x xs + listPerms xs where
    elemPerms z = sum . map (\y -> if z < y then 0 else 1)

-- Now that we've added cubie indices everywhere, and we've made a solved cube
-- have indices = [1..] by construction, *and* we've made an Ord instance for
-- CubieIndex that matches on index first, it's as easy as
permutationParity :: Cube -> Int
permutationParity = listPerms . cubePositions
-- But this is fragile! If I cared, it would be better to be explicit about the
-- Ord instance and about comparing to a solved cube.

-- Final parity check is edge parity. For this we need to compare edge-cubie
-- faces to the center faces they are next to. If at least one of them is next
-- to a center face of the same color (or its opposite), the parity is 0.
-- Otherwise, it's 1. To do this, we have to assign colors to axes. Then we need
-- to have an edge to check, which we can do by giving a position.
-- As an intermediate step, let's check whether a sticker matches its adjacent
-- center.
stickerEdgeParity (Sticker color _ orient) = colorAxis color == orientAxis orient


edgeParity :: Cube -> Position -> Int
edgeParity (Cube _ stickers _) p
    | all stickerEdgeParity stickers' = 1
    | otherwise = 0
    where stickers' = filter (\(Sticker _ p' _) -> p' == p) stickers

totalEdgeParity :: Cube -> Int
totalEdgeParity c@(Cube size _ _) = sum (map (edgeParity c) (edgePositions size))

edgePositions :: Word -> [Position]
edgePositions size =
    let w = fromIntegral size `div` 2
        poss = concatMap List.permutations [[0,w,w],[0,-w,-w]]
    in [Position (x,y,z) | [x,y,z] <- poss]


colorAxis :: Color -> Axis
colorAxis Red    = Rx
colorAxis Orange = Rx
colorAxis White  = Uy
colorAxis Yellow = Uy
colorAxis Green  = Fz
colorAxis Blue   = Fz

orientAxis FaceR = Rx
orientAxis FaceL = Rx
orientAxis FaceU = Fz
orientAxis FaceD = Fz
orientAxis FaceF = Uy
orientAxis FaceB = Uy

-- Now we can check if a cube is solvable!
solvable :: Cube -> Bool
solvable c =
    even (totalEdgeParity c)
    && even (permutationParity c)
    && totalCornerParity c `mod` 3 == 0

-- Now to generate random moves to check solvability.
randomMoves :: IO [Move]
randomMoves = do
    numMoves <- Random.randomRIO (0,30)
    take numMoves . Random.randoms <$> Random.newStdGen

-- Finally, we can start generating scrambles. We want to evenly sample the
-- available space, which means putting cubies in random positions with random
-- orientations. 1 in 12 won't be solvable, so we filter those out.
-- Let's start by randomly twisting a cubie.
twistCubie :: Cube -> Position -> IO Cube
twistCubie (Cube size stickers idxs) pos =
    let (twistedStickers, rest) = List.partition (\(Sticker _ p _) -> p == pos) stickers
        orientations = map (\(Sticker _ _ o) -> o) twistedStickers
    in do
        startO <- Random.randomRIO (0,length orientations)
        -- Keep the same handedness
        let newOrientations = take (length orientations) $ drop startO $ cycle orientations
        let newStickers = zipWith (\(Sticker c p _) o -> Sticker c p o) twistedStickers newOrientations
        pure $ Cube size (newStickers <> rest) idxs

-- Now we can twist all edges and corners
twistCubies :: Cube -> IO Cube
twistCubies c = do
    let edgePos = edgePositions (cubeSize c)
        cornerPos = cornerPositions (cubeSize c)
    Monad.foldM twistCubie c (cornerPos <> edgePos)

-- | permuteCorners :: Cube -> IO Cube
-- Next we need to permute pieces. Corners, edges, and centers need to be
-- permuted individually.
--

displayColor Red    = "R"
displayColor Green  = "G"
displayColor Blue   = "B"
displayColor Yellow = "Y"
displayColor Orange = "O"
displayColor White  = "W"

displayPosition (Position (x,y,z)) =
    (if x > 0 then 'R' else 'L')
    : (if y > 0 then 'U' else 'D')
    : [ if z > 0 then 'F' else 'B' ]

displayOrientation (Orientation (x,y,z))
    | x > 0 = "R"
    | x < 0 = "L"
    | y > 0 = "U"
    | y < 0 = "D"
    | z > 0 = "F"
    | z < 0 = "B"
    | otherwise = error "bad displayOrientation"

displaySticker (Sticker c p o) =
    displayColor c <> "[" <> displayPosition p <> "]" <> "→" <> displayOrientation o

c'' [x,y,z] = Position
    ( if x == 'R' then 1 else -1
    , if y == 'U' then 1 else -1
    , if z == 'F' then 1 else -1
    )
c'' _ = error "bad c''"

-- First we need a function for moving a sticker to a position. We do it in two
-- steps. First rotate around the Z axis to get to the correct X and Y position.
-- Then rotate around X axis to get the correct Z position.
moveTo :: Position -> Sticker -> Sticker
moveTo p s = xRotation p (zRotation p s)

-- Remember to negate to rotate around Z clockwise.
zRotation :: Position -> Sticker -> Sticker
zRotation (Position (x2, y2, _)) s@(Sticker _ (Position (x1,y1,_)) _) =
    let phi1 = atan2 (fromIntegral y1) (fromIntegral x1)
        phi2 = atan2 (fromIntegral y2) (fromIntegral x2)
    in rotateSticker Fz (round $ -2 * (phi2 - phi1) / pi) s

xRotation (Position (_,y2, z2)) s@(Sticker _ (Position (_,y3,z3)) _) =
    let theta3 = atan2 (fromIntegral y3) (fromIntegral z3)
        theta2 = atan2 (fromIntegral y2) (fromIntegral z2)
    in rotateSticker Rx (round $ 2 * (theta2 - theta3) / pi) s
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

    printCube $ move R' $ move D' $ move B' $ move B $ move D $ move R solved3x3

    putStr "R rotation permutates WGO corner clockwise: "
    print $ cornerParity (move R solved3x3) (Position (1,1,-1)) == 1
    putStr "R rotation permutates YGO corner counterclockwise: "
    print $ cornerParity (move R solved3x3) (Position (1,1,1)) == 2
    putStr "totalCornerPerm (moves [R,U,L] solved3x3) == 9: "
    print $ totalCornerParity (moves [R,U,L] solved3x3) == 9
    putStr "twenty sets of random moves stay solvable: "
    print . all (solvable . flip moves solved3x3) =<< Monad.replicateM 20 randomMoves
    putStr "1 in 12 random scrambles is solvable: "
    print . ((/ 100) . fromIntegral) . length . filter solvable =<< Monad.replicateM 100 (twistCubies solved3x3)
