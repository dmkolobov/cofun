module Lib
    ( someFunc
    ) where

import UI.NCurses 
import Curses

import Data.Functor 

someFunc = runCurses 
         $ do 
             w <- window 
             updateWindow w
               $ do 
                   drawBox (Just $ Glyph '#' []) (Just $ Glyph '#' [])
                   drawString "hello, world."
             render 
             waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q') 

waitFor :: Window -> (Event -> Bool) -> Curses () 
waitFor w p = loop
  where 
    loop = do 
             ev <- getEvent w Nothing 
             case ev of 
               Nothing  -> loop 
               Just ev' -> if p ev' then return () else loop