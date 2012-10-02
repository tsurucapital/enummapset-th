{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_HADDOCK not-home #-}

module Data.EnumSet.Base
    ( EnumSet (..)
    , module Data.EnumSet.Base
    ) where

import Prelude ()
import Data.IntSet

import Data.EnumMapSetWrapper

-- Operators
w '(\\)

-- Query
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

-- Construction
w 'empty
w 'singleton
w 'insert
w 'delete

-- Combine
w 'union
w 'unions
w 'difference
w 'intersection

-- Filter
w 'filter
w 'partition
w 'split
w 'splitMember

-- Map
w' 'map

-- Folds
w 'foldr
w 'foldl

-- Strict folds
w 'foldr'
w 'foldl'

-- Min/Max
w 'findMin
w 'findMax
w 'deleteMin
w 'deleteMax
w 'deleteFindMin
w 'deleteFindMax
w 'maxView
w 'minView

-- Conversion: List
w 'elems
w 'toList
w 'fromList

-- Conversion: Ordered list
w 'toAscList
w 'toDescList
w 'fromAscList
w 'fromDistinctAscList

-- Debugging
w 'showTree
w 'showTreeWith

