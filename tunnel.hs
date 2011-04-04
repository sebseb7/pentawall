import Control.Monad.State.Lazy
import Control.Applicative
import Data.IORef
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>))
import System.Random

import LEDWall

data Tube = Tube { tubeColor :: Color }
type Tubes = Seq Tube
data TunnelState = Tunnel { tunnelTubes :: Tubes,
                            tunnelTubesZ :: Integer,
                            tunnelCameraZ :: Double
                          }

type TunnelAction a = StateT TunnelState IO a

getTube :: Double -> TunnelAction Tube
getTube zOff = do cameraZ <- tunnelCameraZ <$> get
                  let z = cameraZ + zOff
                  
                  tubes <- tunnelTubes <$> get
                  tubesZ <- tunnelTubesZ <$> get
                  let offset = fromIntegral $ truncate z - tubesZ
                      tubesLength = Seq.length tubes
                      tube | offset >= Seq.length tubes = Seq.index tubes (tubesLength - 1)
                           | otherwise = Seq.index tubes offset
                  return tube

advanceCamera :: TunnelAction ()
advanceCamera = do st <- get
                   let cameraZ = tunnelCameraZ st + 0.3
                       (tubesZ, tubes)
                           | fromIntegral (tunnelTubesZ st) < cameraZ - 1 =
                               (tunnelTubesZ st + 1, Seq.drop 1 $ tunnelTubes st)
                           | otherwise =
                               (tunnelTubesZ st, tunnelTubes st)
                   put $ st { tunnelCameraZ = cameraZ,
                              tunnelTubesZ = tubesZ,
                              tunnelTubes = tubes }
                   fillTubes
                     
fillTubes :: TunnelAction ()
fillTubes = do tubesLength <- Seq.length <$> tunnelTubes <$> get
               when (tubesLength < tubesRetain) $
                    do appendTube
                       fillTubes
    where tubesRetain = 100
          
          appendTube :: TunnelAction ()
          appendTube = do st <- get
                          c <- liftIO $ randomRIO (0, 255)
                          let tube = Tube { tubeColor = RGB c c c }
                          put $ st { tunnelTubes = tunnelTubes st |> tube }

tunnel :: TunnelAction [[Color]]
tunnel = do advanceCamera
            forM (map (+ 0.01) [-7..8]) $ \y ->
                forM (map (+ 0.01) [-7..7]) $ \x ->
                do let distance = sqrt $ x ** 2 + y ** 2
                       angle = atan (y / x) + (pi / 2) + (if x > 0 then 0 else pi)
                           
                       z = 8 / distance
            
                   
                   --liftIO $ putStrLn $ show (truncate x, truncate y, z)
                   tube <- getTube z
                   return $ tubeColor tube

main = do stateRef <- newIORef $ Tunnel Seq.empty 0 0
          runAnimation $ do st <- readIORef stateRef
                            (a, st') <- runStateT tunnel st
                            writeIORef stateRef st'
                            return a
