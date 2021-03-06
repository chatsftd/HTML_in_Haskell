{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
import HinH.Public
import HinH.Dats
import Prelude hiding(div,head)
main :: IO ()
main = mapM_ putStrLn [
 "main2:",                      toStr empF main2,           "",
 "main2 (fully indented):",     toStr indF main2,           "",
 "main2 (partially indented):", toStr parF main2,           "",
 "main3:",                      toStr empF $ evalS main3 0, "",
 "main3 (fully indented):",     toStr indF $ evalS main3 0, "",
 "main3 (partially indented):", toStr parF $ evalS main3 0, "",
 "main4 (partially indented):", toStr parF main4
 ]


main2 :: HTML ()
main2 = do
 div %% [ "class" := "chapter"] % do
  h1 "title1"
  p "a"
  p "b"
  script $ R "alert(1);"


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

main4 :: HTML ()
main4 = do
 doctypeHTML5
 html %% [ "lang" := "en" ] % do
  head % do
   title "title"
  body % do
   main2  
