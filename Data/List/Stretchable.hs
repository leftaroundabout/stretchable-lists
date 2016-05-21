
-- |
-- Module      : Data.List.Stretchable
-- Copyright   : (c) Justus Sagemüller 2016
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : requires GHC>6 extensions

module Data.List.Stretchable where

import Control.Applicative
import Control.Comonad

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Foldable

data Stretch a = Stretch {
        fixedPart :: [a]
      , cyclePart :: NonEmpty a
      }
