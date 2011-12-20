{-# LANGUAGE TypeSynonymInstances #-}

import Handy
--import qualified StaticData as StaticData
import qualified KernelVars as KV

import System.Random
import GHC.Word

import qualified Obsidian.GCDObsidian.Types as Types
import qualified Obsidian.GCDObsidian.Kernel as Kernel
--import qualified Obsidian.Coordination.Array as Coord
import Obsidian.GCDObsidian.Program
import Obsidian.GCDObsidian.Array

import qualified Obsidian.GCDObsidian.CodeGen.CUDA as CUDA

halfWarp = 16
doAtOnce = 6

sigma = 2.0

radius = 3

type InArray a = PullArray a
type OutArray a = PushArray a

mkInArray = mkPullArray
mkOutArray = mkPushArray

pos2D x y wi = (KV.blockIndexY * KV.blockSizeY + y) * Literal wi * KV.gridSizeX + x

pos2DS x y wi = y * wi + x

-- read2D x y arr = f $ pos2D x y n
  -- where
    -- (Array (Pull f) n) = arr

-- write2D wi val = mkOutArray (\ func -> ForAll (\ i -> func (pos2D i KV.blockSizeY wi, val)) wi) wi

-- do2D x y a@(Array _ l) = write2D l (read2D x y a)

--tidy = KV.threadIndexY

-- Read from transposed local memory and write to regular global memory.
writeGlobal (Array (Pull f) n) = mkOutArray (\ func -> ForAll (\i -> func (pos2D i tidy n, f $ pos2DS tidy i KV.blockSizeY)) n) n

writeLocal (Array (Pull f) n) = mkOutArray (\ func -> ForAll (\i -> func (pos2DS tidy i KV.blockSizeY, f $ pos2D i tidy n)) n) n

writeTemp (Array (Pull f) n) = mkOutArray (\ func -> ForAll (\i -> func (pos2DS i tidy (Literal n), f $ pos2D i tidy n)) n) n



arr :: InArray FloatE
arr = namedArray "hi" 10

type FIArr = Array Pull (Exp Float)
type FOArr = Array Push (Exp Float)

-- Filter
generateFilter :: Int -> [Float]
generateFilter s =
    map (/ sum filt) filt
  where
    sigma' = sigma * sigma * (-2.0)
    l      = fromIntegral (s `div` 2)
    arr    = [-l .. l]
    filt   = map exp' arr
    exp' j = exp (j * j / sigma')

gauss = generateFilter (radius * 2 + 1)

shifts :: [Int]
shifts = [0 - radius .. radius]

-- op arr = foldl1 (+) $ map mk' (zip shifts gauss)
  -- where
    -- len' = len arr
    -- mk' :: (Int, Float) -> Array Pull (Exp Float)
    -- mk' (s, g) = mkInArray (\ ix -> (arr ! (ix + fromInt s)) * (Literal g)) len'


-- zzz :: Array Pull (Exp Float) -> Kernel (Array Push (Exp Float))
-- zzz = pure (writeLocal . op) ->- sync ->- pure (writeGlobal . op)

cd1' func arr0 = putStrLn $ CUDA.genKernel "kernel" func arr0

--clamp func arr x y = \ idx -> 

-- There are two "KV.blockIndexX * blockWidth" in this code to counter the
-- effect of the "bidx * len" at the start of all 1D offsets.
clamp2D offx offy blockHeight trueWidth trueHeight (Array (Pull f) n) = mkOutArray (\ func -> ForAll (\ i -> func (pos2DS i (tidy * Literal 2) (Literal n), f $ (posX' i offx) + (posY' tidy offy) * Literal n * KV.gridSizeX)) n) n
  where
    clampX' x' h' = (x' >=* Literal 0) ?? ((x' <* trueWidth) ?? (h') \\ (trueWidth - Literal 1 - KV.blockIndexX * blockWidth)) \\ (Literal 0 - KV.blockIndexX * blockWidth)
    clampY' x' = (x' >=* Literal 0) ?? ((x' <* trueHeight) ?? (x') \\ (trueHeight - Literal 1)) \\ (Literal 0)
    posX' a b = clampX' ((a + b) + KV.blockIndexX * blockWidth) (a + b)
    posY' a b = clampY' ((a + b) + KV.blockIndexY * blockHeight)
    blockWidth = Literal n

-- yyy :: Array Pull (Exp Float) -> Kernel (Array Push (Exp Float))
-- yyy = pure (writeLocal . op) ->- sync ->- pure (writeGlobal . op)

firstProblem :: Array Pull (Exp Float) -> Kernel (Array Pull (Exp Float))
firstProblem = pure preLoad ->- sync

secondProblem :: Array Pull (Exp Float) -> Kernel (Array Push (Exp Float))
secondProblem = pure preLoad ->- sync ->- pure push

thirdProblem :: Array Pull (Exp Float) -> Kernel (Array Push (Exp Float))
thirdProblem = pure preLoad ->- sync ->- pure doFirstConvolution

fourthProblem :: Array Pull (Exp Float) -> Kernel (Array Pull (Exp Float))
fourthProblem = sync ->- pure doFirstConvolution ->- sync


doFirstConvolution inp = mkOutArray (\ func
      ->  f0 func
      *>* f1 func
    ) (width * 2)
  where
    width = len inp
    height = len inp
    Array (Push f0) _ = doPartial' (0) (doOne' inp)
    Array (Push f1) _ = doPartial' (height) (doOne' inp)
    doPartial' offset (Array (Pull f) n) = mkOutArray (\ func -> ForAll (\i -> func (pos2DS (tidy + Literal offset) (i + Literal 2 * Literal height) KV.blockSizeY, f $ pos2DS (i + (Literal width `div` Literal 2)) ((tidy + Literal offset) * Literal 2) (Literal n))) n) n
    doOne' arr =
      let
        len' = len arr
        mk' :: (Int, Float) -> Array Pull (Exp Float)
        mk' (s, g) = mkInArray (\ ix -> (arr ! (ix + fromInt s)) * (Literal g)) len'
      in
        foldl1 (+) $ map mk' (zip shifts gauss)




--shifted :: Array Pull (Exp Float) -> Kernel (Array Push (Exp Float))
--shifted = pure (clamp2D (Literal (0 - 5)) (Literal 512) (Literal 1024) (Literal 2048)) ->- sync ->- pure push

-- ONLY DOES SQUARES.

-- Load FOUR sets of data from the correct offsets in an array so we have more
-- than enough for the apron.  Note that the original CUDA code loads EXACTLY
-- the correct code for the EXACT apron and data sizes, but this just loads a
-- load of data that should be enough, provided that:
--  
--  (x + 2r) <= 2x
--  (y + 2r) <= 2y
--  
-- Which, of course, we can add compile-time checks for.  Currently x == y as
-- we don't have real support for real 2D arrays in Obsidian.
preLoad arr = mkOutArray (\ func
      ->  f0 func
      *>* f1 (\ (i, a) -> func (i + len'', a))
      *>* f2 (\ (i, a) -> func (i + len'' * len'' * Literal 2, a))
      *>* f3 (\ (i, a) -> func (i + len'' * len'' * Literal 2 + len'', a))
    ) (len' * 2)
  where
    len'  = len arr
    len'' = Literal len'
    minOff = Literal $ 0 - fromInt radius
    maxOff = Literal $ len' - fromInt radius
    width'  = Literal 40
    height' = Literal 40
    topLeft  = clamp2D minOff minOff len'' width' height' arr
    topRight = clamp2D maxOff minOff len'' width' height' arr
    botLeft  = clamp2D minOff maxOff len'' width' height' arr
    botRight = clamp2D maxOff maxOff len'' width' height' arr
    Array (Push f0) _ = topLeft
    Array (Push f1) _ = topRight
    Array (Push f2) _ = botLeft
    Array (Push f3) _ = botRight

--yyy' arr = writeTemp (shifted arr)


--writeTemp (Array (Pull f) n) = mkOutArray (\ func -> ForAll (\i -> func (pos2DS i tidy (Literal n), f $ pos2D i tidy n)) n) n



-- tryLoad (x - r) (y - r) arr
  -- where
    -- tryLoad x' y' h' arr = ((x' >=* Literal 0) &&* (y' >=* Literal 0)) ?? (((x' <* Literal (len arr)) &&* (y' <* Literal h')) ?? (arr ! pos2D x' y' (len arr)) \\ ()) \\ ()



-- -- Generate code to load all of (dx + 2r)(dy + 2r) data in to local shared
-- -- memory using local threads.
-- loadApron :: {- Word32 -> -} Word32 -> Word32 -> Array Pull (Exp Float) -> Array Push (Exp Float)
-- loadApron dy r arr
  -- | len' < 2 * r || dy < 2 * r  = error "Insufficient threads for load."
  -- | otherwise                   = mkOutArray (\ func ->
                                      -- ForAll (\i -> func (pos2DS i tidy len', f $ pos2D i tidy len')) len'
                                      -- *>*
                                      -- ForAll (\i -> func (pos2DS i tidy len', f $ pos2D i tidy len')) len'
                                      -- *>*
                                      -- ForAll (\i -> func (pos2DS i tidy len', f $ pos2D i tidy len')) len'
                                      -- *>*
                                      -- ForAll (\i -> func (pos2DS i tidy len', f $ pos2D i tidy len')) len'
                                    -- )
                                    -- (len' + 2 * r)
    -- where
      -- len' = len arr

-- -- (\ func -> ForAll ) n
