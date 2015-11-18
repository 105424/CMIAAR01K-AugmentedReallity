module Lib
    ( main
    ) where

import Foreign.Ptr
import System.Environment
import Data.Word
import Data.Array.Repa hiding ((++))
import qualified Data.Array.Repa as R hiding ((++))
import Data.Array.Repa.IO.DevIL
import Data.Array.Repa.Repr.ForeignPtr
 
--
-- Read an image, desaturate, write out with new name.
--
main :: IO ()
main = do
  [f] <- getArgs
  runIL $ do
    (RGB a) <- readImage f
    b <- (computeP $ R.traverse a id luminosity) :: IL (Array F DIM3 Word8)
    writeImage ("grey-" ++ f) (RGB b)

luminosity :: (DIM3 -> Word8) -> DIM3 -> Word8
luminosity _ (Z :. _ :. _ :. 3) = 255   -- alpha channel
luminosity f (Z :. i :. j :. _) = ceiling $ 0.21 * r + 0.71 * g + 0.07 * b
    where
        r = fromIntegral $ f (Z :. i :. j :. 0)
        g = fromIntegral $ f (Z :. i :. j :. 1)
        b = fromIntegral $ f (Z :. i :. j :. 2)
