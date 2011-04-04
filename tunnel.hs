{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad.State.Lazy
import Control.Applicative
import Data.IORef
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>), ViewR((:>), EmptyR))
import System.Random

import LEDWall

data Tube = Tube { tubeColors :: [Color], tubeAngle :: Double }
type Tubes = Seq Tube
data TunnelState = Tunnel { tunnelTubes :: Tubes,
                            tunnelTubesZ :: Integer,
                            tunnelCameraZ :: Double,
                            tunnelCameraAngle :: Double
                          }

type TunnelAction a = StateT TunnelState IO a

getNewestTube :: TunnelAction Tube
getNewestTube = do tubes <- tunnelTubes <$> get
                   return $ case Seq.viewr tubes of
                            _ :> tube -> tube
                            EmptyR -> Tube { tubeColors = [RGB 0 0 0],
                                             tubeAngle = 0
                                           }

getTube :: Double -> TunnelAction Tube
getTube zOff = do cameraZ <- tunnelCameraZ <$> get
                  let z = cameraZ + zOff
                  
                  tubes <- tunnelTubes <$> get
                  tubesZ <- tunnelTubesZ <$> get
                  let offset :: Integer
                      offset = truncate z - tubesZ
                      tubesLength = Seq.length tubes
                  case fromInteger offset >= Seq.length tubes of
                    True -> getNewestTube
                    False -> return $ Seq.index tubes $ fromInteger offset

advanceCamera :: TunnelAction ()
advanceCamera = do st <- get
                   let cameraZ = tunnelCameraZ st + 0.1
                       cameraAngle = tunnelCameraAngle st + 0.05
                       (tubesZ, tubes)
                           | fromIntegral (tunnelTubesZ st) < cameraZ - 1 =
                               (tunnelTubesZ st + 1, Seq.drop 1 $ tunnelTubes st)
                           | otherwise =
                               (tunnelTubesZ st, tunnelTubes st)
                   put $ st { tunnelCameraZ = cameraZ,
                              tunnelCameraAngle = cameraAngle,
                              tunnelTubesZ = tubesZ,
                              tunnelTubes = tubes }
                   fillTubes
                     
fillTubes :: TunnelAction ()
fillTubes = do tubesLength <- Seq.length <$> tunnelTubes <$> get
               when (tubesLength < tubesRetain) $
                    do appendTube
                       fillTubes
    where tubesRetain = 8
          
          appendTube :: TunnelAction ()
          appendTube = do prevTube <- getNewestTube
                          segments :: Int <- liftIO $ randomRIO (1, 5)
                          colors <- forM [1..segments] $ const $ liftIO $
                                    do r <- randomRIO (0.5, 1.0)
                                       g <- randomRIO (0.5, 1.0)
                                       b <- randomRIO (0.5, 1.0)
                                       return $ RGBDouble r g b
                          a <- liftIO $ randomRIO (0.01, pi)
                          let tube = Tube { tubeColors = colors, tubeAngle = tubeAngle prevTube + a }
                          st <- get
                          put $ st { tunnelTubes = tunnelTubes st |> tube }

tunnel :: TunnelAction [[Color]]
tunnel = do advanceCamera
            forM (map (+ 0.01) [-7..8]) $ \y ->
                forM (map (+ 0.01) [-7..7]) $ \x ->
                do let distance = sqrt $ x ** 2 + y ** 2
                       angle = atan (y / x) + (pi / 2) + (if x > 0 then 0 else pi)
                           
                       z = 16 / distance
            
                   
                   --liftIO $ putStrLn $ show (truncate x, truncate y, z)
                   tube <- getTube z
                   cameraAngle <- tunnelCameraAngle <$> get
                   let colors = tubeColors tube
                       angle' = angle + cameraAngle + tubeAngle tube
                       color = cycle colors !! (truncate $ angle' * (fromIntegral $ length colors) / (2 * pi))
                       color' = mapColor (/ sqrt (6 / distance)) color
                   return color'
                   
mapColor :: (Double -> Double) -> Color -> Color
mapColor f (RGBDouble r g b) = RGBDouble (f r) (f g) (f b)
{-mapColor f (RGB r g b) = let c = truncate . 
                                 (255.0 *) . 
                                 f .
                                 fromIntegral .
                                 (/ 255)
                         in RGB (c r) (c g) (c b)-}

main = do stateRef <- newIORef $ Tunnel Seq.empty 0 0 0
          runAnimation $ do st <- readIORef stateRef
                            (a, st') <- runStateT tunnel st
                            writeIORef stateRef st'
                            return a
