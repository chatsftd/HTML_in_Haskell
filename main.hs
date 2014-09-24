{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
import HInH.Types
import HInH.Dats
import HInH.Print
import Prelude hiding(div)
main :: IO ()
main = putStrLn $ printHTML main2

main2 :: HTML ()
main2 = do
 __$     div $ do
  __$        h1 "title1"
  __$        p "a"
  __$        p "b"
  __$        p "c"
  __$        p "d" 
 __$     div $ do
  __$        h1 "title2"
  __$        p "a"
  __$        p "b"
  __$        p "c"
  __$        p "d" 
  