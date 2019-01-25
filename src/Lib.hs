module Lib
    ( someFunc
    ) where

import UI.NCurses 
import Curses

import Control.Comonad
import Data.Functor 

import Tape 
import Uni

import Control.Monad.IO.Class

shouldQuit :: Event -> Bool 
shouldQuit e 
  | EventCharacter 'q' <- e = True 
  | EventCharacter 'Q' <- e = True 
  | True                    = False

shouldAdvance :: Event -> Bool 
shouldAdvance e 
  | EventCharacter 's' <- e = True 
  | True                    = False

-- Note that Curses is an instance of MonadIO. So we can use `liftIO` inside 
-- a curses `do` block to lift regular IO actions to the `Curses` monad.

data Direction = L | R 

data ScanState
  = ScanState { scanDirection :: Direction
              }

mainLoop :: Window 
         -> Window
         -> Curses ()
mainLoop buf win 
  = do 
      e <- getEvent win Nothing 
      case e of 
        Just e | shouldQuit e    -> return () 
        Just e | shouldAdvance e -> do 
                                      ( row , col ) <- getCursor win
                                      updateWindow win 
                                      -- Here we define an update to the window. 
                                        $ do
                                            drawString "@"
                                            moveCursor (row + 1) col
                                      render 
                                      mainLoop buf win
        _                        -> do 
                                      mainLoop buf win

someFunc = runCurses 
         $ do 
             w <- window 
             b <- window
             setEcho False
             mainLoop b w 
