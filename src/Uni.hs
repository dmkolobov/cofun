{-# LANGUAGE DeriveFunctor #-}

module Uni where 

import Data.Functor.Compose

import Data.Distributive
import Data.List

import Control.Comonad

import Tape

-- | Instances ----------------------------------------------------------------
-------------------------------------------------------------------------------

data Uni a = Uni [a] -- ^ elements to the left
                  a  -- ^ focus
                 [a] -- ^ elements to the right
           deriving Functor

instance Tape Uni where 
  moveL ~(Uni l a r) = Uni (tail l) (head l) (a : r)
  moveR ~(Uni l a r) = Uni (a : l) (head r) (tail r)

instance Turing Uni where 
  write x ~(Uni l _ r) = Uni l x r

instance Comonad Uni where 

  extract ~(Uni _ x _) = x 

  duplicate u@(~(Uni l _ r))
    = Uni (lefts u) u (rights u)

instance ComonadApply Uni where 

  ~(Uni lf f rf) <@> ~(Uni la a ra)
    = Uni (zipWith ($) lf la) 
          (f a) 
          (zipWith ($) rf ra)

instance Distributive Uni where 
  distribute fu 
    = Uni (fmap extract <$> (tail $ iterate (fmap moveL) fu))
          (extract <$> fu)
          (fmap extract <$> (tail $ iterate (fmap moveR) fu))

instance ( Comonad f 
         , Comonad g 
         , Distributive g ) => Comonad (Compose f g) where

  extract = extract . extract . getCompose 

  duplicate = Compose 
            . fmap (fmap Compose . distribute)  
            . duplicate        
            . fmap duplicate 
            . getCompose    
            
-- | Finite spaces 
-------------------------------------------------------------------------------

data Finite w a = Finite Int (w a) Int

instance ( Show a , Tape w ) => Show (Finite w a) where 

  show (Finite n u m) = let 
                          ls = reverse . take n $ lefts u 
                          rs = (u :)   . take m $ rights u 
                        in 
                          show $ extract <$> ls <> rs

finite :: Turing w => [a] -> w a -> Finite w a  
finite as tape = Finite (length as - 1) (moveL $ writeList as tape) 0

resize :: Int -> Int -> Finite w a -> Finite w a 
resize a b (Finite n tape m) = Finite (max 0 $ n + a) tape (max 0 $ m + b)

-- | Some inductively defined universes.
-------------------------------------------------------------------------------

thedanger :: Uni a 
thedanger = kfix $ Uni (repeat $ const undefined) 
                       (const  $ undefined)
                       (repeat $ const undefined)

numbers :: Uni Int 
numbers = kfix 
        $ Uni (repeat $ pred . extract . moveR) 
              (const 0) 
              (repeat $ succ . extract . moveL)

substrate :: Uni Int 
substrate = kfix $ Uni (repeat $ const 0) (const 0) (repeat $ const 0)

-- | Some rules over common universes.
-------------------------------------------------------------------------------

sumLR :: Uni Int -> Int 
sumLR uni = extract (moveL uni) + extract (moveR uni)

neighborhood :: Tape w => w a -> ( a , a , a )
neighborhood tape = ( extract $ moveL tape
                    , extract tape 
                    , extract $ moveR tape )

type Rule3 a = ( a , a , a ) -> a

data Cell = L -- ^ live cell
          | D -- ^ dead cell 

instance Show Cell where 

  show L = "@"
  show D = " "

rule30 :: Rule3 Cell 
rule30 hood = case hood of 
                ( L , L , L ) -> D 
                ( L , L , D ) -> D 
                ( L , D , L ) -> D
                ( L , D , D ) -> L
                ( D , L , L ) -> L 
                ( D , D , L ) -> L
                ( D , L , D ) -> L 
                ( D , D , D ) -> D

petri :: Uni Cell 
petri = kfix $ Uni (repeat $ const D) (const L) (repeat $ const D)

sim :: Rule3 a -> Uni a -> Uni a 
sim rule = fmap (rule . neighborhood) . duplicate