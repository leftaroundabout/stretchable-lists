
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
                             , pattern (:#)
                             , pattern (:*)
                             , adInfinitum
                             ) where

import Control.Applicative
import Control.Comonad

import Data.Semigroup

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List as List
import Data.Foldable

import Data.String

data Stretch a = Stretch {
        fixedPart :: [a]
      , cyclePart :: NonEmpty a
      }
      deriving (Functor)

adInfinitum :: Stretch a -> [a]
adInfinitum (Stretch f c) = f ++ cycle (NE.toList c)

infixr 6 :#, :*

-- | The usual Cons operation. Note: the semantics of this operator are a bit
--   wily; used as a pattern it /always/ matches, unrolling the cyclic part as
--   needed, whereas as a constructor it always prepends to the fixed part.
--   It is thus not a pattern in the strict sense: you can pop an element off an
--   “empty” list and push it back on, resulting in a 'length' of 1.
pattern (:#) :: a -> Stretch a -> Stretch a
pattern x :# xs <- (uncons -> (x,xs))
 where x :# Stretch f c = Stretch (x:f) c

-- | Combine an ordinary finite list of with a supply of elements
--   (which can then be unrolled cyclically as requested).
pattern (:*) :: [a] -> (a, [a]) -> Stretch a
pattern xs :* cs <- Stretch xs (nEuncons' -> cs)
 where xs :* (c,cs) = Stretch xs (c:|cs)

instance (Show a) => Show (Stretch a) where
  showsPrec p (Stretch fs (c:|cs))
       = showParen (p>5) $ shows fs
                         . (":*"++) . shows (c,cs)

-- | Increase the length by one.
stretch :: Stretch a -> Stretch a
stretch (Stretch fs (c:|cs)) = Stretch (fs++[c]) $ NE.fromList (cs++[c])

loopLastFew :: Int -> [a] -> Stretch a
loopLastFew n l
    | (rlo@(_:_), rin) <- splitAt n $ reverse l
                = Stretch (reverse rin) (NE.fromList $ reverse rlo)
loopLastFew _ _ = error "Can't loop less than one element!"

uncons :: Stretch a -> (a, Stretch a)
uncons (Stretch (x:f) c) = (x, Stretch f c)
uncons (Stretch [] c@(x:|_)) = (x, Stretch [] $ track c)

nEuncons' :: NonEmpty a -> (a, [a])
nEuncons' (c:|cs) = (c,cs)

track :: NonEmpty a -> NonEmpty a
track (x:|xs) = NE.fromList $ xs++[x]

kcart :: a -> NonEmpty a -> NonEmpty a
kcart x xs = x :| NE.init xs

instance Semigroup (Stretch a) where
  Stretch l _ <> Stretch r rc = Stretch (l++r) rc
instance Monoid a => Monoid (Stretch a) where
  mempty = pure mempty
  mappend = (<>)

instance IsString (Stretch Char) where
  fromString s = Stretch s (' ':|[])

instance Comonad Stretch where
  extract (Stretch _ (x:|_)) = x
  duplicate s@(Stretch fs (c:|cs)) = Stretch
                    [loopLastFew ncs l | l <- drop ncs . List.inits $ fs++init(c:cs)]
                  $ NE.fromList . take ncs $ iterate stretch s
   where ncs = 1 + length cs

instance Applicative Stretch where
  pure = Stretch [] . pure
  Stretch fs fcyc <*> Stretch xs xcyc
     | lf>lx      = Stretch (zipWith ($) fs (xs++xext))
                            (NE.fromList . take lcex $ zipWith ($) fcyc' xcont)
     | otherwise  = Stretch (zipWith ($) (fs++fext) xs)
                            (NE.fromList . take lcex $ zipWith ($) fcont xcyc')
   where lf = length fs; lx = length xs
         lcex = lcm (NE.length fcyc) (NE.length xcyc)
         fcyc' = cycle $ NE.toList fcyc
         xcyc' = cycle $ NE.toList xcyc
         (fext,fcont) = splitAt (lx-lf) fcyc'
         (xext,xcont) = splitAt (lf-lx) xcyc'
  
