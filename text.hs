import Control.Monad.State.Lazy
import Data.Char (ord)
import Control.Monad (forM_)
import Control.Applicative
import Data.List (transpose)
import Data.Array.Unboxed

import LEDWall
import LoadImage

data ScrollState = Scroll Picture Int [(String, Color)]

fontCoordinates :: Char -> (Int, Int)
fontCoordinates char 
    | byte < 128 =
        ((byte `mod` 32) * 4, (byte `div` 32) * 6)
    | otherwise =
        (0, 0)
    where byte = fromIntegral $ ord char

scroller :: StateT ScrollState IO [[Color]]
scroller = do Scroll font n texts <- get
              put $ Scroll font (n + 1) texts
              
              forM (reverse [0..15]) $ \x ->
                        forM (reverse [0..14]) $ \y ->
                        do let (text, color) = case drop (y `div` 6) texts of
                                                 line:_ -> line
                                                 _ -> ("", RGB 0 0 0)
                               x' = x + n
                               char = case drop (x' `div` 4) text of
                                        char:_ -> char
                                        _ -> ' '
                               (fx, fy) = fontCoordinates char
                               (fx', fy') = (fx + x' `mod` 4, fy + y `mod` 6)
                               fontColor = font ! (fx', fy')
                               color' :: Color
                               color' | fontColor < 1 = RGB 0 0 0
                                      | otherwise = color
                           return color'


main = do (fontW, fontH, fontPic) <- loadImage "font4x6.png"
          runAnimation scroller $ Scroll fontPic 0 [(cycle "Hello World ", RGB 255 255 0),
                                                    (cycle "Mem:   3950564k total,  3584252k used,   366312k free,       28k buffers ", RGB 0 0 255),
                                                    (cycle "°", RGB 255 0 0)]
            