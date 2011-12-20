module KernelVars where

import qualified Obsidian.GCDObsidian.Kernel as Kernel

import Obsidian.GCDObsidian.Exp

gridSizeX = variable "gridDim.x"  --Coord.gwd
gridSizeY = variable "gridDim.y"  --Coord.ghd
gridSize = gridSizeX * gridSizeY

blockIndexX = variable "bidx" --Coord.bidx
blockIndexY = variable "bidy" --Coord.bidy
blockIndex = blockIndexY * gridSizeX + blockIndexX

blockSizeX = variable "blockDim.x" --Coord.bwd
blockSizeY = variable "blockDim.y" --Coord.bhd
blockSize = blockSizeX * blockSizeY

-- UBER HACK!!!
threadIndexX = variable "(int)threadIdx.x"--Kernel.tidx
threadIndexY = Kernel.tidy
threadIndex = threadIndexY * blockSizeX + threadIndexX -- Ever needed?

threadsX = gridSizeX * blockSizeX
threadsY = gridSizeY * blockSizeY
threads = threadsX * threadsY

indexX = blockIndexX * blockSizeX + threadIndexX
indexY = blockIndexY * blockSizeY + threadIndexY
index = indexY * threadsX + indexX

offset = indexY * threadsX


