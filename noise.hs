import System.Random
import LEDWall

stars :: IO [[Color]]
stars = mapM (\y ->
               mapM (\x ->
                     do a <- randomRIO (0, 255)
                        return $ RGB a (255 - a) 0
                    ) [1..16]
             ) [1..15]
        
--genStars :: [Position]
--genStars = 

main = runAnimation $ stars
