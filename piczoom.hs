{-# LANGUAGE TupleSections,ScopedTypeVariables #-}
module Main (main, main') where

import Control.Monad.State.Lazy
import System.Random
import System.Environment
import Data.Char (toLower)
import System.FilePath (takeExtension)
import System.Exit
import Data.Array.Unboxed
import Data.Array.IO
import Control.Monad (forM_)
import Control.Applicative
import System.IO
import Data.List (transpose)
import Data.Bits

import LEDWall
import LoadImage

data ZoomState = Zoom { zoomPic :: Picture, 
                        zoomWH :: (Int, Int), 
                        zoomStep :: Int,
                        zoomSource :: (Int, Int),
                        zoomTarget :: (Int, Int)
                      }
type ZoomAction a = StateT ZoomState IO a


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
                     tx <- liftIO $ randomRIO (8, w - 8)
                     ty <- liftIO $ randomRIO (8, h - 8)
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


sampleColor :: [(Int, Int)] -> ZoomAction Color
sampleColor xys = do (r :: Int, g :: Int, b :: Int) <-
                       foldM (\(r, g, b) (x', y') ->
                               do RGB r' g' b' <- getColor x' y'
                                  return (r + fromIntegral r', g + fromIntegral g', b + fromIntegral b')
                             ) (0, 0, 0) xys
                     let l = max 1 $ length xys
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
             let (dx, dy) = (fromIntegral sx * (1.0 - a) + fromIntegral tx * a, 
                             fromIntegral sy * (1.0 - a) + fromIntegral ty * a)
                 maxZoom = min 15 $ min (fromIntegral w / 16) (fromIntegral h / 16)
                 zoom = 1.0 + maxZoom * (1 - (a * 2 - 1) ** 2)
             --liftIO $ hPutStr stderr $ "zoom=" ++ show zoom ++ "\n"
             reverse <$> transpose <$> reverse <$>
                           (forM [-7..7] $ \y ->
                            forM [-7..8] $ \x ->
                            let fx x = x * zoom + 7 + dx
                                fy y = y * zoom + 7 + dy
                                x1 = truncate $ fx x
                                y1 = truncate $ fy y
                                x2 = max x1 $ truncate (fx $ x + 1) - 1
                                y2 = max y1 $ truncate (fy $ y + 1) - 1
                            in sampleColor [(x', y')
                                            | x' <- [x1..x2],
                                              y' <- [y1..y2]]
                           )

zoomer = do advance
            compose

main = getArgs >>= main'
main' [f] = do
          (w, h, picture) <- loadImage f
                    
          runAnimation zoomer $ Zoom picture (w, h) (truncate transitionDuration) (0, 0) (0, 0)

