import Obsidian.GCDObsidian
import qualified Obsidian.GCDObsidian.CodeGen.CUDA as CUDA

type APull a = Array Pull (Exp a)

named2D name n = mkPullArray (\ix -> Index (name, [ix, Literal 5])) n


cd1 func arr0 = putStrLn $ CUDA.genKernel "kernel" (pure func) (arr0)


arr = named2D "hi" 42 :: APull Float

