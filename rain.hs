import Control.Monad.State.Lazy
import System.Random
import Control.Applicative
import LEDWall

data Drop = Drop Word Word Color

data Rain = Rain [Drop]

type RainAction a = StateT Rain IO a

mapColor :: (Double -> Double) -> Color -> Color
mapColor f (RGBDouble r g b) = RGBDouble (f r) (f g) (f b)

addColors :: Color -> Color -> Color
addColors (RGBDouble r g b) (RGBDouble r' g' b') = RGBDouble (r + r') (g + g') (b + b')

dropColors :: Drop -> [[Color]]
dropColors (Drop dropX dropY dropColor) = 
  map (\y ->
        map (\x ->
              if dropX == x && y <= dropY
              then mapColor (* (max 0 $ 1 - fromIntegral (dropY - y) / 8)) dropColor
              else RGBDouble 0 0 0
            ) [0..14]
      ) [0..15]

composeDrops :: RainAction [[Color]]
composeDrops = do Rain drops <- get
                  let background = map (\y ->
                                         map (const $ RGBDouble 0 0 0) [0..14]
                                       ) [0..15]
                  return $ foldl (\pixels drop ->
                                   zipWith (zipWith addColors) pixels (dropColors drop)
                                 ) background drops

advance :: RainAction ()
advance = do rainDown
             addDrop (0, 0) (0, 0) (0.2, 0.8)
             addDrop (0.5, 0.8) (0.2, 0.5) (0, 0)
             dropDrops

rainDown = do Rain drops <- get
              let drops' = map (\(Drop x y color) ->
                                 Drop x (y + 1) color
                               ) drops
              put $ Rain drops'
             
addDrop rRange gRange bRange = 
  do Rain drops <- get
             
     x <- fromInteger <$> (liftIO $ randomRIO (0, 14))
     r <- liftIO $ randomRIO rRange
     g <- liftIO $ randomRIO gRange
     b <- liftIO $ randomRIO bRange
     let drop = Drop x 0 (RGBDouble r g b)
                 
     put $ Rain $ drop : drops
             
dropDrops = do Rain drops <- get
               let drops' = filter (\(Drop _ y _) ->
                                     y < 32
                                   ) drops
               put $ Rain drops'

rain :: RainAction [[Color]]
rain = do advance
          composeDrops

main = runAnimation rain $ Rain []
