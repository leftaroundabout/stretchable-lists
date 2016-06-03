
-- |
-- Module      : Data.List.Stretchable
-- Copyright   : (c) Justus Sagemüller 2016
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : requires GHC>6 extensions

{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.List.Stretchable ( Stretch(..)
                             ) where

import Prelude hiding (foldr)

import Control.Applicative
import Control.Comonad
import Control.Arrow

import Data.Semigroup

import qualified Data.Vector as Arr

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
     | CalcStretch Int (Arr.Vector a) (Int -> [Int])
      deriving (Functor)

minimumLen :: Stretch a -> Int
minimumLen (CalcStretch ml _ _) = ml
minimumLen (GapStretch ps ll) = foldr ((+) . length . fst)
                                      (length ll) ps

stretchToLen :: Stretch a -> Int -> [a]
stretchToLen (CalcStretch _ str) = str
stretchToLen sl@(GapStretch ps ll) = doStretch

precomputeStretch :: Stretch a -> Stretch a
precomputeStretch sl = CalcStretch (minimumLen sl) (stretchToLen sl)

stretchToLen :: Stretch a -> Int -> [a]
stretchToLen (CalcStretch _ str) = str
stretchToLen sl@(GapStretch ps ll) = doStretch
 where lmin = minimumLen sl
       nGaps = NE.length ps
       doStretch lreq
          | lreq > lmin 
              = foldr (++) ll
                  $ zipWith (\ncyc (fix,loop)
                              -> fix++take ncyc (cycle $ NE.toList loop))
                         (stretchDistrib 0 nGaps (lreq-lmin) [])
                         (NE.toList ps)
          | otherwise    = take lreq $ doStretch lmin
       stretchDistrib _ 1 nIns = (nIns:)
       stretchDistrib bias nGaps' nIns
          = let ngl = nGaps'`div`2 + bias
                nil = nIns`div`2 + bias
            in stretchDistrib (1-bias) ngl nil
             . stretchDistrib (1-bias) (nGaps'-ngl) (nIns-nil)

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
instance Applicative Stretch where
  pure x = GapStretch (pure ([], pure x)) []
  CalcStretch len₁ str₁ <*> CalcStretch len₂ str₂
      = CalcStretch (max len₁ len₂) (liftA2 (zipWith ($)) str₁ str₂)
  sl₁ <*> sl₂ = precomputeStretch sl₁ <*> precomputeStretch sl₂
  
rshiftSnd :: b -> [(a,b)] -> ([(a,b)], b)
rshiftSnd y₀ [] = ([], y₀)
rshiftSnd y₀ ((x,y) : xys) = first ((x,y₀):) $ rshiftSnd y xys
  
-- | Fold over the fixed part of the list.
instance Foldable Stretch where
  foldr f i (GapStretch ((pf₀,_):|[]) fin)
           = foldr f (foldr f i fin) pf₀
  foldr f i (GapStretch ((pf₀,_):|((pf₁,pc₁):ps)) fin)
           = foldr f (foldr f i $ GapStretch ((pf₁,pc₁):|ps) fin) pf₀
--  null (GapStretch (([],_):|[]) []) = True
--  null (CalcStretch 0 _) = True
--  null _ = False
-- 
-- | Due to the way the 'Applicative' instance works,
--   'sequenceA' can be used as a properly-aligned 'transpose'.
instance Traversable Stretch where
  sequenceA (GapStretch f _) = 
