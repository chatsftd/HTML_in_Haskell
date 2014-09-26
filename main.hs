{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
import HInH.Types
import HInH.Dats
import HInH.Print
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
 


