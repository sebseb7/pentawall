import Control.Monad.State.Lazy
import LEDWall

clip :: Double -> Double -> Double
clip a d | a >= d = clip (a - d) d
         | otherwise = a

radar = do (angleOffset, n) <- get
                
           let angleOffset' = (angleOffset + 0.3) `clip` (2 * pi)
               n' = n + 4
               n'' = n' `mod` 360 
               n''' = fromIntegral (n'' `mod` 60) / 60
               color
                 | n'' < 60 = RGBDouble 1 n''' 0
                 | n'' < 120 = RGBDouble (1 - n''') 1 0
                 | n'' < 180 = RGBDouble 0 1 n'''
                 | n'' < 240 = RGBDouble 0 (1 - n''') 1
                 | n'' < 300 = RGBDouble n''' 0 1
                 | otherwise = RGBDouble 1 0 (1 - n''')
           put (angleOffset', n'')
           
           forM (map (+ 0.01) [-7..8]) $ \y ->
             forM (map (+ 0.01) [-7..7]) $ \x ->
             do let --distance = sqrt $ x ** 2 + y ** 2
                    angle = atan (y / x) + (pi / 2) + (if x > 0 then 0 else pi)
                    angle' = angle + angleOffset'
                    a = max 0.0 $ 1.0 - (angle' / (2 * pi)) `clip` 1.0
                return $ mapColor (* a) color

mapColor :: (Double -> Double) -> Color -> Color
mapColor f (RGBDouble r g b) = RGBDouble (f r) (f g) (f b)


main = runAnimation radar (0, 0)
