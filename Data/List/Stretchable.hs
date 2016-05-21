
-- |
-- Module      : Data.List.Stretchable
-- Copyright   : (c) Justus Sagemüller 2016
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : requires GHC>6 extensions

{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

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
import Data.Foldable

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

instance Comonad Stretch where
  extract (Stretch _ (x:|_)) = x
  duplicate s@(Stretch [] (_:|[])) = Stretch [] (s:|[])
  duplicate s@(Stretch [] (c:|c':cs)) = case duplicate . Stretch [] $ c':|cs of
                 Stretch [] csd -> Stretch [] $ s :| map (c:#) (NE.toList csd)
  duplicate s@(Stretch fs cs) = duplicate . Stretch (init fs) $ kcart (last fs) cs

instance Applicative Stretch where
  pure = Stretch [] . pure
  Stretch fs fcyc <*> Stretch xs xcyc
     | lf>lx      = Stretch (zipWith ($) fs (xs++xext))
                            (NE.zipWith ($) fcyc (NE.fromList $ take lxex xcont))
     | otherwise  = Stretch (zipWith ($) (fs++fext) xs)
                            (NE.zipWith ($) (NE.fromList $ take lfex fcont) xcyc)
   where lf = length fs; lfex = NE.length fcyc
         lx = length xs; lxex = NE.length xcyc
         (fext,fcont) = splitAt (lx-lf) . cycle $ NE.toList fcyc
         (xext,xcont) = splitAt (lf-lx) . cycle $ NE.toList xcyc
  
-- instance Monoid (ExpandaList a) => Monoid (ExpandaList (ExpandaList a)) where
--   mappend (ExpandaList l lc) (ExpandaList r rc)
--              = ExpandaList
--                   ( zipWith extAppend
--                             (l++take (h₂-h₁) (cycle lc))
--                             (r++take (h₁-h₂) (cycle rc)) )
--                   ( zipWith extAppend
--                             (take (max hlc hrc) (cycle lc))
--                             (take (max hlc hrc) (cycle rc)) )
--                   
--    where maxlenL = go $ l++lc
--           where go [] = 0
--                 go (ExpandaList ln _:l') = max (length ln+1) $ go l'
--          [hl,hlc,hr,hrc] = length<$>[l,lc,r,rc]
-- 
-- instance Applicative Stretch where
