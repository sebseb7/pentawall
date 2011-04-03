module LEDWall where

import Text.Printf

type Word = Int

data Color = RGB Word Word Word
           | RGBDouble Double Double Double
type Position = (Word, Word)

data Instruction = Blank Color
                 | Pixel Position Color
                 | Frame [Color]

generateOutput :: [[Color]] -> [Instruction]
generateOutput pixels = 
  concatMap (\(y, row) ->
              map (\(x, color) ->
                    Pixel (x, y) color
                  ) (zip [1..] row)
            ) (zip [1..] pixels)

command2 :: Position -> Color -> String
command2 (x, y) (RGB r g b) = 
  printf "02%02x%02x%02x%02x%02x\r\n" x y r g b

command3 colors = "03" ++ concatMap colorToHex colors ++ "\r\n"

colorToHex :: Color -> String
colorToHex (RGB r g b) = printf "%02x%02x%02x" r g b
colorToHex (RGBDouble r g b) = colorToHex $
                               RGB (c r) (c g) (c b)
    where c :: Double -> Word
          c = truncate .
              (* 255.0) .
              min 1.0 .
              max 0.0

instructionToCommand :: Instruction -> String
instructionToCommand (Blank color) = command2 (0, 0) color
instructionToCommand (Pixel (x, y) color) = command2 (x, y) color
instructionToCommand (Frame colors) = command3 colors

runAnimation :: IO [[Color]] -> IO ()
runAnimation gen = gen >>= 
                   (putStr . instructionToCommand . Frame . concat) >>
                   runAnimation gen
