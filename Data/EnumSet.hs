{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Documentation: <http://hackage.haskell.org/packages/archive/containers/latest/doc/html/Data-IntSet.html>
module Data.EnumSet
    ( module Data.EnumSet.Base
    ) where

import Prelude
import Text.ParserCombinators.ReadPrec
import Text.Read

import Data.EnumSet.Base

instance (Enum k, Show k) => Show (EnumSet k) where
    showsPrec p s = showParen (p > 10) $
        showString "fromList " . shows (toList s)

instance (Enum k, Read k) => Read (EnumSet k) where
    readPrec = parens . prec 10 $ do
        Ident "fromList" <- lexP
        fromList `fmap` readPrec

