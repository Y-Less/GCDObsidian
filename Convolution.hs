{-# LANGUAGE TypeSynonymInstances #-}

import Handy
import qualified StaticData as StaticData

import System.Random
import GHC.Word
import qualified Obsidian.GCDObsidian.Types as Types
import qualified Obsidian.GCDObsidian.Kernel as Kernel
import qualified Obsidian.Coordination.Array as Coord

-- Generic code.
doConvolution f arr = Array idx (len arr)
  where
    idx id = arr ! f id

gridWidth = Coord.gwd * Coord.bwd
yPos = Coord.bhd * Coord.bidy + Kernel.tidy


-- First half.
transpose id = gridWidth * id + yPos

doY = doConvolution transpose

-- Second half.
linear id = gridWidth * yPos + id

doX = doConvolution linear

doAll :: Array FloatE -> Array FloatE
doAll = doY . doX

doCall = cd1 doAll (namedArray "points" 512)
