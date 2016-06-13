
-- |
-- Module      : Data.List.Stretchable
-- Copyright   : (c) Justus Sagemüller 2016
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : requires GHC>6 extensions

{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.List.Stretchable ( Stretch
                             , (*:<), (*++), (>:*), (++*)
                             , stretchToLen
                             ) where

import Prelude hiding (foldr)

import Control.Applicative
import Control.Comonad
import Control.Arrow

import Data.Semigroup

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List as List
import Data.Foldable

import Data.String

-- | List type that behaves as something between a normal finite list
--   (WRT the 'Foldable' instance) or as an infinite list created by cycling
--   a couple of elements in the list (WRT the 'Applicative' instance, which is
--   similar to an infinite @ZipList@).
data Stretch a
     = GapStretch (NonEmpty ([a], NonEmpty a)) [a]
     | CalcStretch Int (Int -> [a])
      deriving (Functor)


infixr 5 *:<, *++

-- | Cycle a single element in front of a list.
(*:<) :: a -> [a] -> Stretch a
cyc *:< fin = GapStretch (([], cyc:|[]):|[]) fin

-- | Cycle a couple of elements in front of a list.
(*++) :: (a,[a]) -> [a] -> Stretch a
(c,yc) *++ fin = GapStretch (([], c:|yc):|[]) fin

infixl 5 >:*, ++*

-- | Cycle a single element after a list.
(>:*) :: [a] -> a -> Stretch a
ini >:* cyc = GapStretch ((ini, cyc:|[]):|[]) []

-- | Cycle a couple of elements behind a list.
(++*) :: [a] -> (a,[a]) -> Stretch a
ini ++* (c,yc) = GapStretch ((ini, c:|yc):|[]) []


minimumLen :: Stretch a -> Int
minimumLen sl = case precomputeStretch sl of (CalcStretch ml _) -> ml


stretchToLen :: Int -> Stretch a -> [a]
stretchToLen l sl = case precomputeStretch sl of CalcStretch _ str -> str l


precomputeStretch :: Stretch a -> Stretch a
precomputeStretch sl@(GapStretch ps ll) = CalcStretch lmin doStretch
 where lmin = length sl
       nGaps = NE.length ps
       doStretch lreq
          | lreq >= lmin 
              = foldr (++) ll
                  $ zipWith (\ncyc (fix,loop)
                              -> fix++take ncyc (cycle $ NE.toList loop))
                         (stretchDistrib 0 nGaps (lreq-lmin) [])
                         (NE.toList ps)
          | otherwise    = take lreq $ doStretch lmin
       stretchDistrib _ 1 nIns = (nIns:)
       stretchDistrib _ n _ | n<1     = error "No stretch to be distributed!"
       stretchDistrib bias nGaps' nIns
          = let ngl = (nGaps'+bias)`div`2
                nil = (nIns+bias)`div`2
            in stretchDistrib (1-bias) ngl nil
             . stretchDistrib (1-bias) (nGaps'-ngl) (nIns-nil)
precomputeStretch cs = cs

track :: NonEmpty a -> NonEmpty a
track (x:|xs) = NE.fromList $ xs++[x]

tracks :: NonEmpty a -> NonEmpty (NonEmpty a)
tracks xs = fmap (trimLen . NE.fromList) . trimLen . NE.tails $ NE.cycle xs
 where trimLen :: NonEmpty b -> NonEmpty b
       trimLen l = NE.zipWith const l xs

kcart :: a -> NonEmpty a -> NonEmpty a
kcart x xs = x :| NE.init xs

instance Semigroup (Stretch a) where
  GapStretch prep₁ lpf₁ <> GapStretch ((p₂f₀,p₂c₀):|p₂s) lpf₂
        = GapStretch (prep₁<>((lpf₁++p₂f₀,p₂c₀):|p₂s)) lpf₂
  CalcStretch len₁ str₁ <> CalcStretch len₂ str₂
        = CalcStretch (len₁+len₂)
                      (\l -> let ll = (l*len₁)`div`(len₁+len₂)
                             in str₁ ll ++ str₂ (l-ll) )
  sl₁ <> sl₂ = precomputeStretch sl₁ <> precomputeStretch sl₂
instance Monoid a => Monoid (Stretch a) where
  mempty = pure mempty
  mappend = (<>)

-- | Stretch at each space character, or else extend the end of the string with spaces.
instance IsString (Stretch Char) where
  fromString "" = GapStretch (("",' ':|[]):|[]) ""
  fromString (' ':s) = case fs s of
              (preps,fin) -> GapStretch ((" ", ' ':|[]):|preps) fin
   where fs "" = ([], "")
         fs (' ':s) = first ((" ", ' ':|[]):) $ fs s
         fs (c:s) = case fs s of
                 ([], fin) -> ([], c:fin)
                 ((p₀f,p₀c):ps,fin) -> ((c:p₀f,p₀c) : ps, fin)
  fromString (c:s) = case fromString s of
                 GapStretch ((p₀f,p₀c):|ps) fin
                     -> GapStretch ((c:p₀f,p₀c) :| ps) fin

-- | 'liftA2' corresponds to a 'zipWith' where the shorter list is extended
--   to match the longer one. 'pure' just repeats a single element
--   (note that @'toList' $ pure x ≡ []@).
--   
--   Due to the way this instance works, 'Data.Traversable.sequenceA' can be
--   used as a properly-aligned transpose operation.
instance Applicative Stretch where
  pure x = GapStretch (pure ([], pure x)) []
  CalcStretch len₁ str₁ <*> CalcStretch len₂ str₂
      = CalcStretch (max len₁ len₂) (\l -> zipWith ($) (str₁ l) (str₂ l))
  sl₁ <*> sl₂ = precomputeStretch sl₁ <*> precomputeStretch sl₂
  
-- | Fold over the fixed part of the list.
instance Foldable Stretch where
  foldr f i (GapStretch ((pf₀,_):|[]) fin) = foldr f (foldr f i fin) pf₀
  foldr f i (GapStretch ((pf₀,_):|((pf₁,pc₁):ps)) fin)
           = foldr f (foldr f i $ GapStretch ((pf₁,pc₁):|ps) fin) pf₀
  foldr f i (CalcStretch len str) = foldr f i $ str len
--  null (GapStretch (([],_):|[]) []) = True
--  null (CalcStretch 0 _) = True
--  null _ = False
-- 
