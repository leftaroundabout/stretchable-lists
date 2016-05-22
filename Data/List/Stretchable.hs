
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

-- | List type that behaves either just like a normal finite list
--   (in the 'Foldable' instance) or as an infinite list created by cycling
--   a couple of elements at the end (in the 'Applicative' instance, which is
--   similar to an infinite @ZipList@).
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

tracks :: NonEmpty a -> NonEmpty (NonEmpty a)
tracks xs = fmap (trimLen . NE.fromList) . trimLen . NE.tails $ NE.cycle xs
 where trimLen :: NonEmpty b -> NonEmpty b
       trimLen l = NE.zipWith const l xs

kcart :: a -> NonEmpty a -> NonEmpty a
kcart x xs = x :| NE.init xs

instance Semigroup (Stretch a) where
  l <> Stretch r rc = Stretch (toList l++r) rc
instance Monoid a => Monoid (Stretch a) where
  mempty = pure mempty
  mappend = (<>)

instance IsString (Stretch Char) where
  fromString s = Stretch s (' ':|[])

-- | 'extract' and 'duplicate' essentially correspond to 'head' and 'List.tails'.
instance Comonad Stretch where
  extract (Stretch (x:_) _) = x
  extract (Stretch _ (x:|_)) = x
  duplicate (Stretch [] cs) = Stretch [] . fmap (Stretch []) $ tracks cs
  duplicate s@(Stretch (_:fs) cs) = s :# duplicate (Stretch fs cs)

-- | 'liftA2' corresponds to a 'zipWith' where the shorter list is extended
--   to match the longer one.
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
  
-- | Fold over the fixed part of the list plus the first cycle.
instance Foldable Stretch where
  foldl f i (Stretch s c) = foldl f (foldl f i s) c
  foldr f i (Stretch s c) = foldr f (foldr f i c) s
  foldl' f i (Stretch s c) = let i' = foldl' f i s in i' `seq` foldl' f i' c
  foldr' f i (Stretch s c) = let i' = foldr' f i c in i' `seq` foldr' f i' s
  foldr1 f (Stretch s c) = foldr f (foldr1 f c) s
  foldl1 f (Stretch s c) = foldl f (foldl1 f c) s
  null _ = False

-- | Due to the way the 'Applicative' instance works,
--   'sequenceA' can be used as a properly-aligned 'transpose'.
instance Traversable Stretch where
  sequenceA s@(Stretch f _) = fmap recycle . sequenceA $ toList s
   where recycle l = Stretch f' $ s'₀:|s'
          where (f',s'₀:s') = splitAt lf l
         lf = length f
