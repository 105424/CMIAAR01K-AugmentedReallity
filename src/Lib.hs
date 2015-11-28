module Lib
    ( fromRGBToYUVToRGB,
      bwFilterWithYUV,
      bwFilterWithRGB
    ) where

import Foreign.Ptr
import System.Environment
import Data.Word
import Data.Array.Repa hiding ((++))
import qualified Data.Array.Repa as R hiding ((++))
import Data.Array.Repa.IO.DevIL as RD (readImage, runIL, writeImage, IL, Image( RGB )) 
import Data.Array.Repa.IO.Matrix as RM 
import Data.Array.Repa.Repr.ForeignPtr

-- examples
fromRGBToYUVToRGB :: FilePath -> FilePath -> IO ()
fromRGBToYUVToRGB f d = do
  (RGB rgbInput) <- runIL $ RD.readImage f
  let yuvImage = R.traverse rgbInput id fromRGBToYUV
  rgbImage <- computeP $ R.traverse yuvImage id fromYUVtoRGB :: IO (Array F DIM3 Word8)
  runIL $ RD.writeImage d (RD.RGB rgbImage)

bwFilterWithYUV :: FilePath -> FilePath -> IO ()
bwFilterWithYUV f d = do
  (RGB rgbInput) <- runIL $ RD.readImage f
  let yuvImage = R.traverse rgbInput id fromRGBToYUV
  let bwYuvImage = R.traverse yuvImage id bwFilterYUV
  rgbImage <- computeP $ R.traverse bwYuvImage id fromYUVtoRGB :: IO (Array F DIM3 Word8)
  runIL $ RD.writeImage d (RD.RGB rgbImage)

bwFilterWithRGB :: FilePath -> FilePath -> IO ()
bwFilterWithRGB f d = do
  (RGB rgbInput) <- runIL $ RD.readImage f 
  bwRgbImage <- (computeP $ R.traverse rgbInput id bwFilterRGB) :: IO (Array F DIM3 Word8)
  runIL $ RD.writeImage d (RD.RGB bwRgbImage)

-- cod
bwFilterRGB :: (DIM3 -> Word8) -> DIM3 -> Word8
bwFilterRGB f (Z :. i :. j :. k) =  case k of 0 -> ceiling $ (r + g + b) / 3
                                              1 -> ceiling $ (r + g + b) / 3
                                              2 -> ceiling $ (r + g + b) / 3
                                    where
                                       r = fromIntegral $ f (Z :. i :. j :. 0)
                                       g = fromIntegral $ f (Z :. i :. j :. 1)
                                       b = fromIntegral $ f (Z :. i :. j :. 2)                            

bwFilterYUV :: Fractional a => (DIM3 -> a) -> DIM3 -> a
bwFilterYUV f (Z :. i :. j :. 0) = f (Z :. i :. j :. 0) -- y
bwFilterYUV _ (Z :. _ :. _ :. 1) = 0   -- u
bwFilterYUV _ (Z :. _ :. _ :. 2) = 0   -- v


fromRGBToYUV ::(DIM3 -> Word8) -> DIM3 -> Double
fromRGBToYUV f (Z :. i :. j :. k) = case k of 0 -> y
                                              1 -> uMax * ( (b - y) / (1 - wb) ) -- u
                                              2 -> vMax * ( (r - y) / (1 - wr) ) -- v
                                    where
                                        wr = 0.299
                                        wg = 0.587
                                        wb = 0.114                                       
                                        uMax = 0.436
                                        vMax = 0.615
                                        r = (fromIntegral $ f (Z :. i :. j :. 0)) / 255
                                        g = (fromIntegral $ f (Z :. i :. j :. 1)) / 255
                                        b = (fromIntegral $ f (Z :. i :. j :. 2)) / 255
                                        y = (wr * r) + (wg * g) + (wb * b)

fromYUVtoRGB :: (DIM3 -> Double) -> DIM3 -> Word8
fromYUVtoRGB f (Z :. i :. j :. k) = case k of 0 -> ceiling $ 255 * r
                                              1 -> ceiling $ 255 * ( (y - (wr * r) - (wb * b)) / wg )
                                              2 -> ceiling $ 255 * b
                                    where
                                        wr = 0.299
                                        wg = 0.587
                                        wb = 0.114 
                                        uMax = 0.436
                                        vMax = 0.615                                                               
                                        y = f (Z :. i :. j :. 0)
                                        u = f (Z :. i :. j :. 1)
                                        v = f (Z :. i :. j :. 2)

                                        r = ( y + (v * ( (1 - wr) / (vMax) ) ) )
                                        b = ( y + (u * ( (1 - wb) / (uMax) ) ) )