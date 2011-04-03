import Control.Monad.State.Lazy
--import Control.Applicative
import Data.IORef

import LEDWall

data TunnelState = Tunnel

tunnel :: StateT TunnelState IO [[Color]]
tunnel = forM (map (+ 0.01) [-7..8]) $ \y ->
         forM (map (+ 0.01) [-7..7]) $ \x ->
         do let distance = sqrt $ x ** 2 + y ** 2
                angle = atan (y / x) + (pi / 2) + (if x > 0 then 0 else pi)
                a = distance / 8
                b = angle / (2 * pi)
            --liftIO $ putStrLn $ show $ (x, y, angle / pi)
            return $ RGBDouble b a 0

main = do stateRef <- newIORef Tunnel
          runAnimation $ do st <- readIORef stateRef
                            (a, st') <- runStateT tunnel st
                            writeIORef stateRef st'
                            return a
