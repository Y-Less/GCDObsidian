{-# LANGUAGE TypeSynonymInstances #-}

import Handy
import qualified StaticData as StaticData
import qualified KernelVars as KV

import System.Random
import GHC.Word

import qualified Obsidian.GCDObsidian.Types as Types
import qualified Obsidian.GCDObsidian.Kernel as Kernel
--import qualified Obsidian.Coordination.Array as Coord
import Obsidian.GCDObsidian.Program
import Obsidian.GCDObsidian.Array

import qualified Obsidian.GCDObsidian.CodeGen.CUDA as CUDA

--import Obsidian.GCDObsidian

halfWarp = 16
doAtOnce = 6

sigma = 2.0

radius = 3

type InArray a = PullArray a
type OutArray a = PushArray a

mkInArray = mkPullArray
mkOutArray = mkPushArray

-- type InArray a = GlobalArray Pull a
-- type OutArray a = GlobalArray Push a

-- mkInArray = mkGlobalPullArray
-- mkOutArray = mkGlobalPushArray

-- How to write to a shared 2d array.

-- How to write to a global 2d array.

-- How to read from a shared 2d array.

-- How to read from a global 2d array.

-- (?=) = read2D
-- (!=) = write2D



pos2D x y wi = (blockIndexY * blockSizeY + y) * wi * KV.gridSizeX + x

read2D x y arr = f $ pos2D x y n
  where
    (Array (Pull f) n) = arr

write2D wi val = mkOutArray (\ func -> ForAll (\ i -> func (pos2D i KV.blockSizeY wi, val)))

--do2D x y arr

do2D = write2Dl x y 512 512 . read2D x y


pos2Dl x y wi = y * wi + x

read2Dl x y arr = f $ pos2D x y n
  where
    (Array (Pull f) n) = arr

write2Dl x y wi hi val = f $ pos2D x y wi
  where
    f x = mkOutArray (\ func -> ForAll (\ i -> func 



-- Make a 2D array, adjusted for the correct ABSOLUTE location in the grid.
push2D :: InArray a -> OutArray a
push2D (Array (Pull f) n) = mkOutArray (\ func -> ForAll (\i -> func (i + KV.threadsX * KV.indexY - (KV.blockIndexX * fromIntegral n), f $ i + KV.threadsX * KV.indexY)) n) n

-- Make a shared 2D array, using local addressing only.
push2DShared :: InArray a -> OutArray a
push2DShared (Array (Pull f) n) = mkOutArray (\ func -> ForAll (\i -> func (i + KV.blockSizeX * KV.threadIndexY, f $ i + KV.blockSizeX * KV.threadIndexY)) n) n

-- Make a 2D array, adjusted for the correct ABSOLUTE location in the grid.
push2DTranspose :: InArray a -> OutArray a
push2DTranspose (Array (Pull f) n) = mkOutArray (\ func -> ForAll (\i -> func (i * KV.threadsY + KV.indexY - (KV.blockIndexX * fromIntegral n), f $ i + KV.threadsX * KV.indexY)) n) n

-- Make a shared 2D array, using local addressing only.
push2DSharedTranspose :: InArray a -> OutArray a
push2DSharedTranspose (Array (Pull f) n) = mkOutArray (\ func -> ForAll (\i -> func (i * KV.blockSizeY + KV.threadIndexY, f $ i + KV.blockSizeY * KV.threadIndexY)) n) n

arr :: InArray FloatE
arr = namedArray "hi" 512

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

shiftX s arr = mkInArray func0' (len arr)
  where
    --func0' ix = ((KV.indexX + fromInt s) <=* KV.threadsX) ?? arr ! (ix + fromInt s) \\ Literal 0
    func0' ix = arr ! (ix + fromInt s)

shiftY s arr = mkInArray func0' (len arr)
  where
    --func0' ix = ((KV.indexY + fromInt s) <=* KV.threadsY) ?? arr ! (ix + fromInt s) \\ Literal 0
    func0' ix = arr ! (ix + fromInt s)

shifts = [0 - radius .. radius]

--out :: (g -> h -> i) -> InArray (FloatE) -> [Float] -> [Int] -> InArray (FloatE) -> InArray FloatE
out func arr [] [] tot             = tot
out func arr (g : gs) (s : ss) tot = out func arr gs ss $ (dm (*) (func s arr) (Literal g)) + tot
  where
    dm f a x = mkInArray (\ ix -> f (a ! ix) x) (len a)
out func arr gs ss tot             = error $ "thing: " ++ show gs ++ ", " ++ show ss

-- We really need to rely on integer wrap-around.
doOne func arr = out func arr gauss shifts (mkInArray (\_ -> Literal 0) (len arr))


doF typ func arr = pure (typ . doOne func) arr :: Kernel (OutArray FloatE)

zzz :: InArray FloatE -> Kernel (OutArray FloatE)
zzz = doF push2DSharedTranspose shiftX ->- sync ->- doF push2DTranspose shiftY

cd1' func arr0 = putStrLn $ CUDA.genKernel "kernel" func arr0

run' = cd1' zzz arr
