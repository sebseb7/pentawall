import Control.Monad.State.Lazy
import LEDWall

clip :: Double -> Double -> Double
clip a d | a >= d = clip (a - d) d
         | otherwise = a

radar = do angleOffset <- get
                
           let angleOffset' = (angleOffset + 0.3) `clip` (2 * pi)
           put angleOffset'
           
           forM (map (+ 0.01) [-7..8]) $ \y ->
             forM (map (+ 0.01) [-7..7]) $ \x ->
             do let distance = sqrt $ x ** 2 + y ** 2
                    angle = atan (y / x) + (pi / 2) + (if x > 0 then 0 else pi)
                    angle' = angle + angleOffset'
                    a = max 0.0 $ 1.0 - (angle' / (2 * pi)) `clip` 1.0
                return $ RGBDouble (a * 3 / 4) a 0


main = runAnimation radar 0
