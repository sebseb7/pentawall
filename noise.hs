import System.Random
import Control.Applicative

import LEDWall

main = do let i :: Int -> Word
              i = fromIntegral
          --xs <- map i <$> randomRs (0, 15) <$> newStdGen
          --ys <- map i <$> randomRs (0, 15) <$> newStdGen
          colors <- map (\a -> RGB a a a) <$>
                    map i <$> randomRs (0, 255) <$> newStdGen
          putStr $
                 concatMap instructionToCommand $
                 map (\(xy@(x, y), color) ->
                       Pixel xy $
                       if odd (x + y)
                       then RGB 0 0 0
                       else RGB 255 255 255
                     ) $
                 zip (cycle $ [(x, y) | y <- reverse [1..16], x <- [1..16]]) colors
