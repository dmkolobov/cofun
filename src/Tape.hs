module Tape where 

import Control.Comonad 

-- | The class of one-dimensional comonads that can have their focus 
-- shifted left or right by one.
class Comonad w => Tape w where 
  -- | Move the focus one to the left
  moveL :: w a -> w a
  -- | Move the focus one to the right
  moveR :: w a -> w a

-- | all universes whose focus is left of the current focus
lefts :: Tape w => w a -> [w a] 
lefts = tail . iterate moveL 

-- | all universes whose focus is right of the current focus
rights :: Tape w => w a -> [w a]
rights = tail . iterate moveR

-- | move the universe `n` times to the left
leftN :: Tape w => Int -> w a -> [w a] 
leftN n = take n . lefts

-- | move the universe `n` times to the right
rightN :: Tape w => Int -> w a -> [w a]
rightN n = take n . rights 

-- | The class of tapes which support writing
class Tape w => Turing w where 
  -- | Write a value to the current location
  write :: a -> w a -> w a

writeList :: Turing w => [a] -> w a -> w a
writeList []       tape = tape 
writeList (a : as) tape = writeList as . moveR $ write a tape
