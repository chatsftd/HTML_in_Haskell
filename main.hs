{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
import HinH.Types
import HinH.Dats
import HinH.Print
import Prelude hiding(div)
main :: IO ()
main = putStrLn $ printHTML main2

main2 :: HTML ()
main2 = do
 div % do
  h1 "title1"
  p "a"
  p "b"
  p "c"
  p "d" 
 div % do
  h1 "title2"
  p "a"
  p "b"
  p "c"
  p "d"
 


