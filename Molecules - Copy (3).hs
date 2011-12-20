{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

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

-- type InArray a = PullArray a
-- type OutArray a = PushArray a

-- mkInArray = mkPullArray
-- mkOutArray = mkPushArray

-- class ToSize a where
	-- toSize :: Word32 -> a

-- instance ToSize (Exp Word32) where
	-- toSize = Literal

-- instance ToSize Word32 where
	-- toSize = id

type InArray_ p a = GlobalArray p a
type OutArray_ p a = GlobalArray p a

type InArray a = GlobalArray Pull a
type OutArray a = GlobalArray Push a

mkInArray = mkGlobalPullArray
mkOutArray = mkGlobalPushArray

-- How to write to a shared 2d array.

-- How to write to a global 2d array.

-- How to read from a shared 2d array.

-- How to read from a global 2d array.

-- -- Make a 2D array, adjusted for the correct ABSOLUTE location in the grid.
-- push2D :: InArray a -> OutArray a
-- push2D (GlobalArray (Pull f) n) = mkOutArray (\ func -> ForAll (\i -> func (i + KV.threadsX * KV.indexY - (KV.blockIndexX * fromIntegral n), f $ i + KV.threadsX * KV.indexY)) n) (toSize n)

-- -- Make a shared 2D array, using local addressing only.
-- push2DShared :: InArray a -> OutArray a
-- push2DShared (GlobalArray (Pull f) n) = mkOutArray (\ func -> ForAll (\i -> func (i + KV.blockSizeX * KV.threadIndexY, f $ i + KV.blockSizeX * KV.threadIndexY)) n) (toSize n)

-- Make a 2D array, adjusted for the correct ABSOLUTE location in the grid.
push2DTranspose :: InArray a -> OutArray a
push2DTranspose arr = p1 $ mkPushArray (\ func -> ForAll (\i -> func (i * KV.threadsY + KV.indexY, f $ i + KV.threadsX * KV.indexY)) n) (n)
	where
		p0 x = block 512 x
		p1 x = unblock x
		(Array (Pull f) n) = p0 arr

-- -- Make a shared 2D array, using local addressing only.
-- push2DSharedTranspose :: InArray a -> OutArray a
-- push2DSharedTranspose (GlobalArray (Pull f) n) = mkOutArray (\ func -> ForAll (\i -> func (i * KV.blockSizeX + KV.threadIndexY, f $ i + KV.blockSizeX * KV.threadIndexY)) n) (toSize n)
	-- where
		-- arr' = block 512 arr

-- Make a shared 2D array, using local addressing only.
push2DSharedTranspose :: InArray a -> OutArray a
push2DSharedTranspose arr = p1 $ mkPushArray (\ func -> ForAll (\i -> func (i * KV.blockSizeX + KV.threadIndexY, f $ i + KV.blockSizeX * KV.threadIndexY)) n) (n)
	where
		p0 x = block 512 x
		p1 x = unblock x
		(Array (Pull f) n) = p0 arr

--in2out = unblock . push . block 512

-- Based on "push".
--array2D :: (Exp Word32 -> a) -> Word32 -> OutArray a
--array2D f n = 

-- transpose :: Pushy a => a b -> OutArray b
-- transpose arr = transpose` (push arr)
	-- where
		-- transpose' arr = 

-- k

-- transpose :: (Exp Word32 -> a) -> Word32 -> OutArray a
-- transpose f n = mkOutArray (\ func -> ForAll (\i -> func (i * KV.threadsX + KV.indexY, f $ i + KV.threadsX + KV.indexY)) n) n


-- transpose2D :: InArray a -> OutArray a
-- transpose2D (Array (Pull f) n) = transpose f n

namedGlobalArray name n = mkGlobalPullArray (\ix -> index name ix) n 
--namedGlobalArray = namedArray

arr :: InArray FloatE
arr = namedGlobalArray "hi" 512

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

shiftX s arr = mkInArray func0' (globLen arr)
  where
    func0' ix = ((KV.indexX + fromInt s) <=* KV.threadsX) ?? arr ! (ix + fromInt s) \\ Literal 0

shiftY s arr = mkInArray func0' (globLen arr)
  where
    func0' ix = ((KV.indexY + fromInt s) <=* KV.threadsY) ?? arr ! (ix + fromInt s) \\ Literal 0

shifts = [0 - radius .. radius]

--out :: (g -> h -> i) -> InArray (FloatE) -> [Float] -> [Int] -> InArray (FloatE) -> InArray FloatE
out func arr [] [] tot             = tot
out func arr (g : gs) (s : ss) tot = out func arr gs ss $ (dm (*) (func s arr) (Literal g)) + tot
  where
    dm f a x = mkInArray (\ ix -> f (a ! ix) x) (globLen a)
out func arr gs ss tot             = error $ "thing: " ++ show gs ++ ", " ++ show ss

-- We really need to rely on integer wrap-around.
--doX arr = out shiftX arr gauss shifts (mkInArray (\_ -> Literal 0) (len arr))
--doY arr = out shiftY arr gauss shifts (mkInArray (\_ -> Literal 0) (len arr))
doOne func arr = out func arr gauss shifts (mkInArray (\_ -> Literal 0) (globLen arr))




--[bidx * 42 + tidx + (bidy * blockDim.y + tidy) * gridDim.x * blockDim.x]

-- This is good, write all temporary values to memory (although there's no apron
-- at this point as we're just using this whole InArray, but doing it separated.

--doneX arr = push2DSharedTranspose (doOne shiftX arr) :: Kernel (OutArray FloatE)

--doneY arr = push2DSharedTranspose (doOne shiftY arr)

doF typ func arr = pure (typ . doOne func) arr :: Kernel (OutArray FloatE)

--doneX arr = sync (push2D (doX arr)) :: Kernel (InArray FloatE)
--doneY arr = pure (transpose2D . doY) arr :: Kernel (OutArray FloatE)

zzz :: InArray FloatE -> Kernel (OutArray FloatE)
zzz = doF push2DSharedTranspose shiftX ->- sync ->- doF push2DTranspose shiftY

--zzz :: InArray FloatE -> Kernel (OutArray FloatE)
--zzz = doneX ->- doneY

cd1' func arr0 = putStrLn $ CUDA.genKernel "kernel" func arr0

{-

__global__ void kernel(float *input0,float *result0){
  unsigned int tidx = threadIdx.x;
  unsigned int bidx = blockIdx.x;
  unsigned int tidy = threadIdx.y;
  unsigned int bidy = blockIdx.y;
  extern __shared__ __attribute__ ((aligned (16))) unsigned char sbase[];
  (( float *)sbase)[(tidx+((gridDim.x*blockDim.x)*((bidy*blockDim.y)+tidy)))] = (((((((bidx*blockDim.x)+tidx)+3)<=(gridDim.x*blockDim.x)) ? input0[((bidx*42)+((tidx+((gridDim.x*blockDim.x)*((bidy*bloc
kDim.y)+tidy)))+3))] : 0.0)*7.015932e-2)+(((((((bidx*blockDim.x)+tidx)+2)<=(gridDim.x*blockDim.x)) ? input0[((bidx*42)+((tidx+((gridDim.x*blockDim.x)*((bidy*blockDim.y)+tidy)))+2))] : 0.0)*0.13107488)
+(((((((bidx*blockDim.x)+tidx)+1)<=(gridDim.x*blockDim.x)) ? input0[((bidx*42)+((tidx+((gridDim.x*blockDim.x)*((bidy*blockDim.y)+tidy)))+1))] : 0.0)*0.19071281)+((((((bidx*blockDim.x)+tidx)<=(gridDim.
x*blockDim.x)) ? input0[((bidx*42)+(tidx+((gridDim.x*blockDim.x)*((bidy*blockDim.y)+tidy))))] : 0.0)*0.21610592)+(((((((bidx*blockDim.x)+tidx)+4294967295)<=(gridDim.x*blockDim.x)) ? input0[((bidx*42)+
((tidx+((gridDim.x*blockDim.x)*((bidy*blockDim.y)+tidy)))+4294967295))] : 0.0)*0.19071281)+(((((((bidx*blockDim.x)+tidx)+4294967294)<=(gridDim.x*blockDim.x)) ? input0[((bidx*42)+((tidx+((gridDim.x*blo
ckDim.x)*((bidy*blockDim.y)+tidy)))+4294967294))] : 0.0)*0.13107488)+((((((bidx*blockDim.x)+tidx)+4294967293)<=(gridDim.x*blockDim.x)) ? input0[((bidx*42)+((tidx+((gridDim.x*blockDim.x)*((bidy*blockDi
m.y)+tidy)))+4294967293))] : 0.0)*7.015932e-2)))))));
  __syncthreads();
  result0[((bidx*42)+((tidx*(gridDim.x*blockDim.x))+((bidy*blockDim.y)+tidy)))] = (((((((bidy*blockDim.y)+tidy)+3)<=(gridDim.y*blockDim.y)) ? (( float *)sbase)[(((tidx+(gridDim.x*blockDim.x))+((bidy*b
lockDim.y)+tidy))+3)] : 0.0)*7.015932e-2)+(((((((bidy*blockDim.y)+tidy)+2)<=(gridDim.y*blockDim.y)) ? (( float *)sbase)[(((tidx+(gridDim.x*blockDim.x))+((bidy*blockDim.y)+tidy))+2)] : 0.0)*0.13107488)
+(((((((bidy*blockDim.y)+tidy)+1)<=(gridDim.y*blockDim.y)) ? (( float *)sbase)[(((tidx+(gridDim.x*blockDim.x))+((bidy*blockDim.y)+tidy))+1)] : 0.0)*0.19071281)+((((((bidy*blockDim.y)+tidy)<=(gridDim.y
*blockDim.y)) ? (( float *)sbase)[((tidx+(gridDim.x*blockDim.x))+((bidy*blockDim.y)+tidy))] : 0.0)*0.21610592)+(((((((bidy*blockDim.y)+tidy)+4294967295)<=(gridDim.y*blockDim.y)) ? (( float *)sbase)[((
(tidx+(gridDim.x*blockDim.x))+((bidy*blockDim.y)+tidy))+4294967295)] : 0.0)*0.19071281)+(((((((bidy*blockDim.y)+tidy)+4294967294)<=(gridDim.y*blockDim.y)) ? (( float *)sbase)[(((tidx+(gridDim.x*blockD
im.x))+((bidy*blockDim.y)+tidy))+4294967294)] : 0.0)*0.13107488)+((((((bidy*blockDim.y)+tidy)+4294967293)<=(gridDim.y*blockDim.y)) ? (( float *)sbase)[(((tidx+(gridDim.x*blockDim.x))+((bidy*blockDim.y
)+tidy))+4294967293)] : 0.0)*7.015932e-2)))))));

}

-}

