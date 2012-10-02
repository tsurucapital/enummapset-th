{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_HADDOCK not-home #-}

module Data.EnumMap.Base
    ( EnumMap (..)
    , module Data.EnumMap.Base
    ) where

import Prelude ()
import Data.IntMap.Lazy

import Data.EnumMapSetWrapper

#include "map.inc"

