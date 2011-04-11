{-# LANGUAGE TupleSections,ScopedTypeVariables #-}
module LoadImage (loadImage, Picture) where

import Graphics.GD hiding (Color)
import Control.Applicative
import System.IO
import Data.Char (toLower)
import System.FilePath (takeExtension)
import System.Exit
import Data.Array.Unboxed
import Data.Array.IO
import Control.Monad (forM_)
import Control.Applicative

type Picture = UArray (Int, Int) Int

loadImage f = do load <- case map toLower (takeExtension f) of
                           ".jpg"  -> return loadJpegFile
                           ".jpeg" -> return loadJpegFile
                           ".png"  -> return loadPngFile
                           ".gif"  -> return loadGifFile
                           _       -> die
                 image <- load f
                 (w, h, picture) <- imageToPicture image
                 return $ (w, h, picture)
                 
                 
imageToPicture :: Image -> IO (Int, Int, Picture)
imageToPicture img =
    do (w, h) <- imageSize img
       let maxX = w - 1
           maxY = h - 1
           bounds = ((0, 0), (maxX, maxY))
       cm <- newArray bounds 0 :: IO (IOUArray (Int, Int) Int)
       putStrLn "converting..."
       {-# SCC "forPixels" #-} forM_ (range bounds) $ \pos ->
         (fromIntegral <$>
          {-# SCC "getPixel" #-} getPixel pos img) >>=
         {-# SCC "writeArray" #-} writeArray cm pos
       putStrLn "converted"
       (w, h, ) <$> freeze cm

die :: IO a
die = do
    hPutStr stderr
         "Usage: scaleimage <x> <y> input.[png,gif,jpg] output.[png,gif,jpg]"
    exitWith (ExitFailure 1)
