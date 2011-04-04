{-# LANGUAGE TupleSections,ScopedTypeVariables #-}
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

transitionDuration = 60

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
                  (w, h) <- zoomWH <$> get
                  case (w, y) of
                    _ | (x >= 0 && y >= 0 &&
                         x < w && y < h) ->
                        return $ colorToRGB $ picture ! (x, y)
                    _ ->
                        return $ RGB 0 0 0
    where colorToRGB :: Int -> Color
          colorToRGB c = let r = c `shiftR` 16
                             g = c `shiftR` 8
                             b = c
                         in RGB (norm r) (norm g) (norm b)
              where norm = fromIntegral . (.&. 0xFF)


sampleColor :: Int -> Int -> Int -> ZoomAction Color
sampleColor x y zoom = do (r :: Int, g :: Int, b :: Int) <-
                              foldM (\(r, g, b) (x', y') ->
                                      do RGB r' g' b' <- getColor x' y'
                                         return (r + fromIntegral r', g + fromIntegral g', b + fromIntegral b')
                                    ) (0, 0, 0) [(x', y')
                                                 | x' <- [x..(x + zoom - 1)],
                                                   y' <- [y..(y + zoom - 1)]]
                          let l = zoom ^ 2
                              c :: Int -> Word
                              c = fromIntegral . (`div` l)
                          return $ RGB (c r) (c g) (c b)

getAlpha :: ZoomAction Double
getAlpha = do t <- getT
              return $ (sin $ (t - 0.5) * pi) / 2 + 0.5

compose :: ZoomAction [[Color]]
compose = do a <- getAlpha
             (sx, sy) <- zoomSource <$> get
             (tx, ty) <- zoomTarget <$> get
             (w, h) <- zoomWH <$> get
             let (dx, dy) = (truncate $ fromIntegral sx * (1.0 - a) + fromIntegral tx * a, 
                             truncate $ fromIntegral sy * (1.0 - a) + fromIntegral ty * a)
                 maxZoom = min 20 $ min (fromIntegral w / 16) (fromIntegral h / 16)
                 zoom = 1.0 + maxZoom * (1 - (a * 2 - 1) ** 2)
             --liftIO $ hPutStr stderr $ "zoom=" ++ show zoom ++ "\n"
             reverse <$> transpose <$> reverse <$>
                           (forM [0..14] $ \y ->
                            forM [0..15] $ \x ->
                            --liftIO (hPutStr stderr $ show (x, y, dx + truncate (fromIntegral x * zoom), dy + truncate (fromIntegral y * zoom))) >>
                            sampleColor (dx + truncate (fromIntegral (x - 7) * zoom + 7)) (dy + truncate (fromIntegral (y - 7) * zoom + 7)) (truncate zoom))

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
