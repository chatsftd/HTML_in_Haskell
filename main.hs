{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
import HinH.Types
import HinH.Dats
import HinH.StateHTML
import HinH.Print
import Prelude hiding(div)
main :: IO ()
main = do
 putStrLn "main2:"
 putStrLn $ printHTML empF main2 
 putStrLn "\nmain3:"
 putStrLn $ printHTML empF $ evalS main3 0
 putStrLn "\nmain3 (indented):"
 putStrLn $ printHTML indF $ evalS main3 0


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
 


main3 :: StateHTML Int
main3 = do
 _PUT 1
 chapter
 chapter

chapter :: StateHTML Int
chapter = do
 num <- _GET 
 div % do
  h1 $ "title" ++ show num
  p "a"
  p "b"
  p "c"
  p "d"
 _PUT $ num + 1  

