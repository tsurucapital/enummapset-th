{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Documentation: <http://hackage.haskell.org/packages/archive/containers/latest/doc/html/Data-IntMap-Lazy.html>
module Data.EnumMap.Lazy
    ( module Data.EnumMap.Base
    ) where

import Prelude
import Text.ParserCombinators.ReadPrec
import Text.Read

import Data.EnumMap.Base (EnumMap)
import Data.EnumMap.Base hiding (EnumMap (EnumMap))

instance (Enum k, Show k, Show a) => Show (EnumMap k a) where
    showsPrec p m = showParen (p > 10) $
        showString "fromList " . shows (toList m)

instance (Enum k, Read k, Read a) => Read (EnumMap k a) where
    readPrec = parens . prec 10 $ do
        Ident "fromList" <- lexP
        fromList `fmap` readPrec

