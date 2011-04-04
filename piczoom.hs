{-# LANGUAGE TupleSections #-}
module Main (main, main') where

import Control.Monad.State.Lazy
import System.Random
import System.Environment
import Data.Char (toLower)
import System.FilePath (takeExtension)
import System.Exit
import Graphics.GD hiding (Color)
import Data.Array.Unboxed
import Data.Array.IO
import Control.Monad (forM_)
import Control.Applicative
import System.IO
import Data.List (transpose)
import Data.Bits

import LEDWall

data ZoomState = Zoom { zoomPic :: Picture, 
                        zoomWH :: (Int, Int), 
                        zoomStep :: Int,
                        zoomSource :: (Int, Int),
                        zoomTarget :: (Int, Int)
                      }
type ZoomAction a = StateT ZoomState IO a

type Picture = UArray (Int, Int) Int

transitionDuration = 50

getT :: ZoomAction Double
getT = (/ transitionDuration) <$>
       fromIntegral <$>
       zoomStep <$>
       get

advance :: ZoomAction ()
advance = do st <- get
             let step = zoomStep st + 1
                 st' = st { zoomStep = step }
             put $ st'
             
             t <- getT
             when (t > 1.0) $
                  do let (w, h) = zoomWH st'
                     tx <- liftIO $ randomRIO (0, w - 16)
                     ty <- liftIO $ randomRIO (0, h - 15)
                     put $ st' { zoomStep = 0,
                                 zoomSource = zoomTarget st',
                                 zoomTarget = (tx, ty)
                               }

getColor :: Int -> Int -> ZoomAction Color
getColor x y = do picture <- zoomPic <$> get
                  --liftIO $ putStrLn $ "getColor " ++ show x ++ " " ++ show y
                  return $ colorToRGB $ picture ! (x, y)
    where colorToRGB :: Int -> Color
          colorToRGB c = let r = c `shiftR` 16
                             g = c `shiftR` 8
                             b = c
                         in RGB (norm r) (norm g) (norm b)
              where norm = fromIntegral . (.&. 0xFF)


sampleColor :: Int -> Int -> ZoomAction Color
sampleColor x y = do (w, h) <- zoomWH <$> get
                     case (w, y) of
                       _ | (x >= 0 && y >= 0 &&
                            x < w && y < h) ->
                           getColor x y
                       _ ->
                           return $ RGB 0 0 0

getAlpha :: ZoomAction Double
getAlpha = do t <- getT
              return $ sin $ t * pi / 2

compose :: ZoomAction [[Color]]
compose = do a <- getAlpha
             (sx, sy) <- zoomSource <$> get
             (tx, ty) <- zoomTarget <$> get
             let (dx, dy) = (truncate $ fromIntegral sx * (1.0 - a) + fromIntegral tx * a, 
                             truncate $ fromIntegral sy * (1.0 - a) + fromIntegral ty * a)
             transpose <$> reverse <$>
                           (forM [0..14] $ \y ->
                            forM [0..15] $ \x ->
                            --liftIO (putStrLn $ show (dx + x, dy + y)) >>
                            sampleColor (dx + x) (dy + y))

zoomer = do advance
            compose

main = getArgs >>= main'
main' [f] = do
          load <- case map toLower (takeExtension f) of
                    ".jpg"  -> return loadJpegFile
                    ".jpeg" -> return loadJpegFile
                    ".png"  -> return loadPngFile
                    ".gif"  -> return loadGifFile
                    _       -> die
          image <- load f
          (w, h, picture) <- imageToPicture image
                    
          runAnimation zoomer $ Zoom picture (w, h) (truncate transitionDuration) (0, 0) (0, 0)

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
