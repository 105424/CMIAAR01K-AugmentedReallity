module Lib
    ( main
    ) where

import Foreign.Ptr
import System.Environment
import Data.Word
import Data.Array.Repa hiding ((++))
import qualified Data.Array.Repa as R hiding ((++))
import Data.Array.Repa.IO.DevIL as RD (readImage, runIL, writeImage, IL, Image( RGB )) 
import Data.Array.Repa.Repr.ForeignPtr

-- wrap the traverse to accept our own Image type 
{-data Image = RGB (Array F DIM3 Word8) | RGBA (Array F DIM3 Word8) | YUV (Array F DIM3 Word8)
traverse :: Shape sh' => Lib.Image -> (DIM3 -> sh') -> ((DIM3 -> Word8) -> sh' -> b) -> Array D sh' b
traverse (Lib.RGB i) s f = R.traverse i s f
traverse (Lib.RGBA i) s f = R.traverse i s f
traverse (Lib.YUV i) s f = R.traverse i s f-}

--
-- Read an image, desaturate, write out with new name.
--
main :: IO ()
main = do
  [f,r] <- getArgs
  bwFilterWithRGB f r

bwFilterWithYUV :: FilePath -> FilePath -> IO ()
bwFilterWithYUV f d = do
  runIL $ do
    (RGB rgbInput) <- RD.readImage f
    yuvImage <- (computeP $ R.traverse rgbInput id toYUV) :: IL (Array F DIM3 Word8)
    rgbImage <- (computeP $ R.traverse yuvImage id bwFilterYUV) :: IL (Array F DIM3 Word8)
    -- rgbImage <- (computeP $ R.traverse bwYuv id fromYUV) :: IL (Array F DIM3 Word8)
    RD.writeImage d (RD.RGB rgbImage)

bwFilterWithRGB :: FilePath -> FilePath -> IO ()
bwFilterWithRGB f d = do
  runIL $ do
    (RGB rgbInput) <- RD.readImage f
    rgbImage <- (computeP $ R.traverse rgbInput id bwFilterRGB) :: IL (Array F DIM3 Word8)
    -- rgbImage <- (computeP $ R.traverse bwYuv id fromYUV) :: IL (Array F DIM3 Word8)
    RD.writeImage d (RD.RGB rgbImage)


bwFilterRGB :: (DIM3 -> Word8) -> DIM3 -> Word8
bwFilterRGB f (Z :. i :. j :. k) =  case k of 0 -> ceiling $ (r + g + b) / 3
                                              1 -> ceiling $ (r + g + b) / 3
                                              2 -> ceiling $ (r + g + b) / 3
                                              3 -> 255  -- alpha
                                              _ -> f (Z :. i :. j :. k)
                                    where
                                       r = fromIntegral $ f (Z :. i :. j :. 0)
                                       g = fromIntegral $ f (Z :. i :. j :. 1)
                                       b = fromIntegral $ f (Z :. i :. j :. 2)
                            

bwFilterYUV :: (DIM3 -> Word8) -> DIM3 -> Word8
bwFilterYUV f (Z :. i :. j :. 0) = f (Z :. i :. j :. 0) -- y
bwFilterYUV _ (Z :. _ :. _ :. 1) = 0   -- u
bwFilterYUV _ (Z :. _ :. _ :. 2) = 0   -- v


-- we use ceiling so we can use multiply word8 with a fraction and then ciel it up to a word8 compatible instance again
toYUV :: (DIM3 -> Word8) -> DIM3 -> Word8
toYUV f (Z :. i :. j :. k) = case k of 0 -> ceiling $ wr * r + wg * g + wb * b -- y
                                       1 -> ceiling $ uMax * ( (b - y) / (1 - wb) ) -- u
                                       2 -> ceiling $ vMax * ( (r - y) / (1 - wr) ) -- v
                                       _ -> f (Z :. i :. j :. k)
                             where
                                wr = 0.299
                                wb = 0.114
                                wg = 1 - wr - wb
                                uMax = 0.436
                                vMax = 0.615
                                r = fromIntegral $ f (Z :. i :. j :. 0)
                                g = fromIntegral $ f (Z :. i :. j :. 1)
                                b = fromIntegral $ f (Z :. i :. j :. 2)
                                a = fromIntegral $ f (Z :. i :. j :. 3)
                                y = fromIntegral $ f (Z :. i :. j :. 0) -- we need y to calculate the u and v (which might bring troubelsom result given paralisme)

fromYUV :: (DIM3 -> Word8) -> DIM3 -> Word8
fromYUV f (Z :. i :. j :. k) = case k of 0 -> ceiling $ 255 * ( y + (v * ( (1 - wr) / (vMax) ) ) ) -- r
                                         1 -> ceiling $ 255 * ( y + (u * ( (1 - wb) / (uMax) ) ) ) -- g
                                         2 -> ceiling $ 255 * ( (y - (wr * r) - (wb * b)) / wg )   -- b
                                         _ -> f (Z :. i :. j :. k)
                             where
                                wr = 0.299
                                wb = 0.114
                                wg = 1 - wr - wb
                                uMax = 0.436
                                vMax = 0.615
                                -- this will be fucked in parallel execution i guess
                                r = fromIntegral $ f (Z :. i :. j :. 0)
                                g = fromIntegral $ f (Z :. i :. j :. 1)
                                b = fromIntegral $ f (Z :. i :. j :. 2) 
                                                               
                                y = fromIntegral $ f (Z :. i :. j :. 0)
                                u = fromIntegral $ f (Z :. i :. j :. 1)
                                v = fromIntegral $ f (Z :. i :. j :. 2)
