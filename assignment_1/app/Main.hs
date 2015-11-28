module Main where

import qualified Lib as L
import System.Environment

--
-- Read an image, desaturate, write out with new name.
--
main :: IO ()
main = do
  [f] <- getArgs
  L.bwFilterWithRGB f (f ++ "bwFilterWithRGB.jpg")
  L.fromRGBToYUVToRGB f (f ++ "fromRGBToYUVToRGB.jpg")
  L.bwFilterWithYUV f (f ++ "bwFilterWithYUV.jpg")
