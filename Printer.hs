module Printer (display) where

import Types
import Data.List (transpose)
import Text.PrettyPrint.Boxes

blue = "\ESC[94m"
green = "\ESC[92m"
yellow = "\ESC[93m"
red = "\ESC[91m"
endc = "\ESC[0m"

wrapWithColor :: String -> String -> String
wrapWithColor color s = color ++ s ++ endc

greenify :: String -> String
greenify = wrapWithColor green

blueify :: String -> String
blueify = wrapWithColor blue

yellowify :: String -> String
yellowify = wrapWithColor yellow

redify :: String -> String
redify = wrapWithColor red

display :: (Displayable a) =>  [a] -> IO ()
display x = do
  let xs = fmap dsp x
  let rows = map (zipWith ($) [greenify, blueify, yellowify, redify]) xs
  let columns = transpose rows
  printBox $ hsep 1 left (map (vcat left . map text) columns)
