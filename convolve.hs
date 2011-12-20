
import Obsidian.GCDObsidian
import Obsidian.GCDObsidian.Program
import qualified KernelVars as KV

import Handy

import qualified Obsidian.GCDObsidian.CodeGen.CUDA as CUDA

dataWidth = 1024
dataHeight = 2048

sigma = 3.0

radius = 2


-- Make a shared 2D array, using local addressing only.
--transpose :: Array Pull FloatE -> Array Push FloatE
--transpose (Array (Pull f) n) = mkPushArray (\ func -> ForAll (\i -> func (i * KV.blockSizeY + tidy, f $ i)) n) n


-- Make a 2D array, adjusted for the correct ABSOLUTE location in the grid.
transpose :: Array Pull FloatE -> Array Push FloatE
transpose (Array (Pull f) n) = mkPushArray (\ func -> ForAll (\i -> func (i * KV.threadsY + KV.indexY - (KV.blockIndexX * fromIntegral n), f $ i)) n) n



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

-- Clamp the reading from an array to a given x/y range.  This is quite a simple
-- base for reading all the data we need (rather than the HORRIBLE pre-load
-- system I was trying to make before that looked NOTHING LIKE convolution!  Now
-- this code should allow us to write something that looks a LOT better...
clamp width height offset (Array (Pull f) n) = mkPullArray func' n
    where
      width' = Literal width
      height' = Literal height
      clampX' i = ifThenElse (KV.threadIndexX + offset >=* Literal 0) (ifThenElse (i + offset <* width') (i) (width' - Literal 1 - offset)) (Literal 0 - offset)
      clampY' i = i + KV.blockIndexY * KV.blockSizeY --ifThenElse (i >=* Literal 0) (ifThenElse (i <* height') (i) (height' - Literal 1)) (Literal 0)
      func' i = f $ clampX' i + clampY' (tidy) * width'

gOff arr = KV.blockIndexX * Literal (len arr)
lOff arr = Literal 0


convolve arr =
  let
    len' = len arr
    mk' :: (Int, Float) -> Array Pull (Exp Float)
    mk' (s, g) = mkPullArray (\ ix -> (arr ! (ix + (Literal . fromInteger . toInteger) s)) * (Literal g)) len'
  in
    foldl1 (+) $ map mk' (zip shifts gauss)

firstHalf :: Array Pull FloatE -> Array Push FloatE
firstHalf arr = (transpose . convolve . clamp dataWidth dataHeight (gOff arr)) arr

secondHalf :: Array Pull FloatE -> Array Push FloatE
secondHalf arr = (transpose . convolve . clamp dataHeight dataWidth (gOff arr)) arr



-- Declare basic data for running with.
cd1' func arr0 = putStrLn $ CUDA.genKernel "kernel" func (arr0)

-- An array clamped to it's bounds.
arr :: Array Pull FloatE
arr = clamp dataWidth dataHeight (gOff arr') arr'
  where
    arr' = namedArray "hithere" 32

