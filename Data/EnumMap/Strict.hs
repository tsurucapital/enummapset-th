{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Documentation: <http://hackage.haskell.org/packages/archive/containers/latest/doc/html/Data-IntMap-Strict.html>
module Data.EnumMap.Strict
    ( EnumMap
    , module Data.EnumMap.Strict
    ) where

import Prelude ()
import Data.IntMap.Strict

import Data.EnumMapSetWrapper
import Data.EnumMap.Lazy ({-instance Show, Read-})

#include "map.inc"

