{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

module Data.EnumMap.STRICT where

import Prelude (Eq, Ord, Enum, Functor (..), (.), ($), (>))
import Control.DeepSeq
import Data.Data
import Data.Foldable (Foldable)
import Data.IntMap.STRICT
import Data.Monoid
import Data.Traversable
import Text.ParserCombinators.ReadPrec
import Text.Read
import Text.Show

import Data.EnumMapSetWrapper
import Data.EnumSet (EnumSet (..))

infixr 1 `EnumMap`
newtype EnumMap k v = EnumMap { unEnumMap :: IntMap v } deriving
    (Eq, Ord, Monoid, Typeable, Data, NFData)

------------------------------------------------------------------------

-- * Operators
w '(!)
w '(\\)

-- * Query
w 'null
w 'size
w 'member
w 'notMember
w 'lookup
w 'findWithDefault
w 'lookupLT
w 'lookupGT
w 'lookupLE
w 'lookupGE

-- * Construction
w 'empty
w 'singleton

-- * Insertion
w 'insert
w 'insertWith
w 'insertWithKey
w 'insertLookupWithKey

-- * Delete/Update
w 'delete
w 'adjust
w 'adjustWithKey
w 'update
w 'updateWithKey
w 'updateLookupWithKey
w 'alter

-- * Combine: Union
w 'union
w 'unionWith
w 'unionWithKey
w 'unions
w 'unionsWith

-- * Combine: Difference
w 'difference
w 'differenceWith
w 'differenceWithKey

-- * Combine: Intersection
w 'intersection
w 'intersectionWith
w 'intersectionWithKey

-- * Combine: Universal combining function
w 'mergeWithKey

-- * Traversal: Map
w 'map
w 'mapWithKey
w 'traverseWithKey
w 'mapAccum
w 'mapAccumWithKey
w 'mapAccumRWithKey
w' 'mapKeys
w' 'mapKeysWith
w' 'mapKeysMonotonic

-- * Traversal: Folds
w 'foldr
w 'foldl
w 'foldrWithKey
w 'foldlWithKey

-- * Traversal: Strict folds
w 'foldr'
w 'foldl'
w 'foldrWithKey'
w 'foldlWithKey'

-- * Conversion
w 'elems
w 'keys
w 'assocs
w 'keysSet
w 'fromSet

-- * Conversion: Lists
w 'toList
w 'fromList
w 'fromListWith
w 'fromListWithKey

-- * Conversion: Ordered lists
w 'toAscList
w 'toDescList
w 'fromAscList
w 'fromAscListWith
w 'fromAscListWithKey
w 'fromDistinctAscList

-- * Filter
w 'filter
w 'filterWithKey
w 'partition
w 'partitionWithKey
w 'mapMaybe
w 'mapMaybeWithKey
w 'mapEither
w 'mapEitherWithKey
w 'split
w 'splitLookup

-- * Submap
w 'isSubmapOf
w 'isSubmapOfBy
w 'isProperSubmapOf
w 'isProperSubmapOfBy

-- * Min/Max
w 'findMin
w 'findMax
w 'deleteMin
w 'deleteMax
w 'deleteFindMin
w 'deleteFindMax
w 'updateMin
w 'updateMax
w 'updateMinWithKey
w 'updateMaxWithKey
w 'minView
w 'maxView
w 'minViewWithKey
w 'maxViewWithKey

-- * Debugging
w 'showTree
w 'showTreeWith

------------------------------------------------------------------------

instance Functor (EnumMap k) where
    {-# INLINE fmap #-}
    fmap = Data.EnumMap.STRICT.map

deriving instance Foldable (EnumMap k)
deriving instance Traversable (EnumMap k)

instance (Enum k, Show k, Show a) => Show (EnumMap k a) where
    showsPrec p m = showParen (p > 10) $
        showString "fromList " . shows (Data.EnumMap.STRICT.toList m)

instance (Enum k, Read k, Read a) => Read (EnumMap k a) where
    readPrec = parens . prec 10 $ do
        Ident "fromList" <- lexP
        Data.EnumMap.STRICT.fromList `fmap` readPrec

-- vim: ft=haskell:

