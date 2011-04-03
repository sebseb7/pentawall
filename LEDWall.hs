module LEDWall where

import Text.Printf

type Word = Int

data Color = RGB Word Word Word
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

command3 colors = "03" ++ concatMap (\(RGB r g b) ->
                                      printf "%02x%02x%02x" r g b
                                    ) colors ++ "\r\n"

instructionToCommand :: Instruction -> String
instructionToCommand (Blank color) = command2 (0, 0) color
instructionToCommand (Pixel (x, y) color) = command2 (x, y) color
instructionToCommand (Frame colors) = command3 colors

runAnimation :: IO [[Color]] -> IO ()
runAnimation gen = gen >>= 
                   (putStr . instructionToCommand . Frame . concat) >>
                   runAnimation gen
