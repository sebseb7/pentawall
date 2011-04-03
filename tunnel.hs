import Control.Monad.State.Lazy
--import Control.Applicative
import Data.IORef

import LEDWall

data TunnelState = Tunnel

tunnel :: StateT TunnelState IO [[Color]]
tunnel = forM [1..16] $ \y ->
         forM [1..15] $ \x ->
         do let distance = sqrt $ (fromIntegral $ y - 8) ** 2 + (fromIntegral $ x - 8) ** 2
                a = distance / 8
            return $ RGBDouble a a a

main = do stateRef <- newIORef Tunnel
          runAnimation $ do st <- readIORef stateRef
                            (a, st') <- runStateT tunnel st
                            writeIORef stateRef st'
                            return a
