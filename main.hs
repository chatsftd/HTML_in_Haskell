{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
import HinH.Types
import HinH.Dats
import HinH.Print
import Prelude hiding(div)
import Control.Monad.State
main :: IO ()
main = do
 putStrLn "main2:"
 putStrLn $ printHTML main2
 putStrLn "\nmain3:"
 putStrLn $ printHTML $ evalStateT main3 0

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
 

main3 :: StateT Int HTML ()
main3 = do
 put 1
 chapter
 chapter

chapter :: StateT Int HTML ()
chapter = do
 num <- get 
 lift $ div % do
  h1 $ "title" ++ show num
  p "a"
  p "b"
  p "c"
  p "d"
 put $ num + 1  
