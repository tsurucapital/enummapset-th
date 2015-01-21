{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE KindSignatures #-}

-- | Refer to the <http://hackage.haskell.org/package/containers/docs/Data-IntSet.html documentation>
-- for "Data.IntSet".
module Data.EnumSet where

import Prelude (Eq, Ord, Enum, Functor (..), (.), ($), (>))
import Control.DeepSeq
import Data.IntSet
import Data.Typeable
import Data.Data
import Data.Monoid
import Text.ParserCombinators.ReadPrec
import Text.Read
import Text.Show

import Data.EnumMapSetWrapper

newtype EnumSet k = EnumSet { unEnumSet :: IntSet } deriving
    (Eq, Ord, Monoid, Typeable, Data, NFData)

-- * Operators
w '(\\)

-- * Query
w 'null
w 'size
w 'member
w 'notMember
w 'lookupLT
w 'lookupGT
w 'lookupLE
w 'lookupGE
w 'isSubsetOf
w 'isProperSubsetOf

-- * Construction
w 'empty
w 'singleton
w 'insert
w 'delete

-- * Combine
w 'union
w 'unions
w 'difference
w 'intersection

-- * Filter
w 'filter
w 'partition
w 'split
w 'splitMember

-- * Map
w' 'map

-- * Folds
w 'foldr
w 'foldl

-- * Strict folds
w 'foldr'
w 'foldl'

-- * Min/Max
w 'findMin
w 'findMax
w 'deleteMin
w 'deleteMax
w 'deleteFindMin
w 'deleteFindMax
w 'maxView
w 'minView

-- * Conversion: List
w 'elems
w 'toList
w 'fromList

-- * Conversion: Ordered list
w 'toAscList
w 'toDescList
w 'fromAscList
w 'fromDistinctAscList

-- * Debugging
w 'showTree
w 'showTreeWith

------------------------------------------------------------------------

instance (Enum k, Show k) => Show (EnumSet k) where
    showsPrec p s = showParen (p > 10) $
        showString "fromList " . shows (Data.EnumSet.toList s)

instance (Enum k, Read k) => Read (EnumSet k) where
    readPrec = parens . prec 10 $ do
        Ident "fromList" <- lexP
        fmap Data.EnumSet.fromList readPrec

