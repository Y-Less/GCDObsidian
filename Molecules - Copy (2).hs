{-# LANGUAGE TypeSynonymInstances #-}

import Handy
import qualified StaticData as StaticData

import System.Random
import GHC.Word
import qualified Obsidian.GCDObsidian.Types as Types
import qualified Obsidian.GCDObsidian.Kernel as Kernel
import qualified Obsidian.Coordination.Array as Coord

type UsingType = Float
type UsingExpr = Exp UsingType

-- Should be predictable!  But I don't know if it's the same as the other one
-- found in the C++ implementation yet (same seed though currently).
--randomNumbers :: [Float]
randomNumbers = StaticData.fPointData --randomRs (0, 1) (mkStdGen 1066)

atomMaxX = 2000.0
atomMaxY = 2000.0
atomMaxZ = 2000.0
atomMaxC = 200.0

atomCount = 5

data Pos3D a = Pos3D {
		pos3DX :: a,
		pos3DY :: a,
		pos3DZ :: a
	}
	deriving (Show, Eq)

data Atom a = Atom {
		atomPos :: Pos3D a,
		atomC   :: a
	}
	deriving (Show, Eq)

---------------------------------------
class FromFloat a where
	fromFloat :: Float -> a

instance FromFloat Float where
	fromFloat x = x

instance FromFloat Int where
	fromFloat = round

instance FromFloat IntE where
	fromFloat = Literal . fromFloat

instance FromFloat FloatE where
	fromFloat = Literal

---------------------------------------
class FromWord32 a where
	fromWord32 :: Word32 -> a

instance FromWord32 Int where
	fromWord32 = fromInteger . toInteger

instance FromWord32 IntE where
	fromWord32 = Literal . fromWord32

instance FromWord32 Float where
	fromWord32 = fromInteger . toInteger

instance FromWord32 FloatE where
	fromWord32 = Literal . fromWord32

---------------------------------------
class FromInt a where
	fromInt :: Int -> a

instance FromInt Word32 where
	fromInt = fromInteger . toInteger

instance FromInt IntE where
	fromInt = Literal . fromInt

instance FromInt Float where
	fromInt = fromInteger . toInteger

instance FromInt FloatE where
	fromInt = Literal . fromInt

---------------------------------------
class FromExpWord32 a where
	fromExpWord32 :: UWordE -> a

instance FromExpWord32 IntE where
	fromExpWord32 = CastOp Types.Word32 Types.Int

instance FromExpWord32 FloatE where
	fromExpWord32 = CastOp Types.Word32 Types.Float

---------------------------------------
class FromExpFloat a where
	fromExpFloat :: FloatE -> a

instance FromExpFloat IntE where
	fromExpFloat = CastOp Types.Float Types.Int

instance FromExpFloat UWordE where
	fromExpFloat = CastOp Types.Float Types.Word32

---------------------------------------
class FromExpInt a where
	fromExpInt :: IntE -> a

instance FromExpInt UWordE where
	fromExpInt = CastOp Types.Int Types.Word32

instance FromExpInt FloatE where
	fromExpInt = CastOp Types.Int Types.Float

---------------------------------------
generateAtoms = generateAtoms' (cycle randomNumbers)
	where
		generateAtoms' _ 0 = []
		generateAtoms' (r0 : r1 : r2 : r3 : rs) i = Atom (Pos3D (fromFloat (r0 * atomMaxX)) (fromFloat (r1 * atomMaxY)) (fromFloat (r2 * atomMaxZ))) (fromFloat (r3 * atomMaxC)) : generateAtoms' rs (i - 1)
-- Changing the charge above to "0.0" reduces the entire expression to:
-- "result0[id] = 0.0;"

atoms :: [Atom UsingExpr]
atoms = generateAtoms atomCount

difference3D (Pos3D x0 y0 z0) (Pos3D x1 y1 z1) = (x' * x') + (y' * y') + (z' * z')
	where
		x' = x1 - x0
		y' = y1 - y0
		z' = z1 - z0

-- 1D grid (sort of).
createGrid :: Word32 -> Array UsingExpr
createGrid n = Array (\ ix -> Literal 42) n --fromWord32 ix



grid = createGrid 1024 -- 32 blocks of 1024 elements.

-- NO SQUARE ROOT YET!

-- Currently VERY wrong for lack of operators.

-- Take a reference point and an accumulation array, and return a new array
-- using the current atom.
part0 :: Array UsingExpr -> Array UsingExpr -> Atom UsingExpr -> Array UsingExpr
part0 (Array ixf len) total (Atom pos charge) =
	Array (\ idx -> (total ! idx) + (charge / sqrt (difference3D (part1 ixf idx) pos))) len

-- Get the current grid location as a "Pos3D".
part1 :: (Exp Word32 -> UsingExpr) -> Exp Word32 -> Pos3D UsingExpr
--part1 ixf idx = Pos3D (ixf idx) (fromExpWord32 $ Coord.bidy * Coord.bhi + Kernel.tidy) (fromFloat 1.0)
part1 ixf idx = Pos3D (fromExpWord32 $ Coord.bidx * Coord.bwd + Kernel.tidx) (fromExpWord32 $ Coord.bidy * Coord.bhi + Kernel.tidy) (fromFloat 1.0)

-- Fold over the function defined above.
part2 g = foldl (part0 g) (Array (\ _ -> 0) (len g)) atoms

part3 = cd1 part2 grid

-- We can mimick 2d arrays using the block index.  I just don't know what the
-- limits are at the minute...

-- Even better, I can mimick 
