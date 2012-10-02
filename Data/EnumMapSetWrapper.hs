{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK not-home, ignore-exports #-}

{- |

A type @t@ is either negative (takes) or positive (gives). If @t ≡ a → b@,
then @b@ has the same polarity as @t@, while @a@ has the opposite.

Given the reification of a type @s@ of the appropriate polarity, 'pos' (or
'neg') returns a triple comprising a wrapper (or an unwrapper) of type @s
→ t@, any additional contexts required, along with the reification of @t@.
When @s@ is 'Key', we replace it with a type @k@; the replacements for
'IntMap' @v@ and 'IntSet' are 'EnumMap' @k v@ and 'EnumSet' @k@
respectively; otherwise the type is left untouched. In each case the
appropriate wrapper (or unwrapper) is returned.

In order to generalise functions that map from one key type to another, we
heuristically consider the rightmost type in a sequence of ‘→’s to be the
output, treating everything else as input. For example:

> EnumSet.map ∷ (Enum i, Enum o) ⇒ (i → o) → EnumSet i → EnumSet o

We're screwed if someone adds a function that generalises to more than two
different key types. [Ed: You could always write it out by hand…]

Where only one key type is concerned, 'substT' unifies the input and output
key type variables.

-}

module Data.EnumMapSetWrapper (EnumMap (..), EnumSet (..), w, w') where

import Prelude
import Control.Applicative
import Control.Arrow
import Data.List (nub)
import Data.IntSet (IntSet, Key)
import Data.IntMap (IntMap)
import Language.Haskell.TH.Syntax

import Control.DeepSeq
import Data.Foldable
import Data.Traversable
import Data.Typeable
import Data.Data
import Data.Monoid

newtype EnumMap k v = EnumMap { unEnumMap :: IntMap v } deriving
    (Eq, Ord, Monoid, Functor, Foldable, Traversable, Typeable, Data, NFData)

newtype EnumSet k = EnumSet { unEnumSet :: IntSet } deriving
    (Eq, Ord, Monoid, Typeable, Data, NFData)

------------------------------------------------------------------------

-- | @o ≃ (.)@
infixr 9 `o`
o :: Exp -> Exp -> Exp
o = flip UInfixE (VarE '(.))

-- | @pre f ≃ (. f)@
pre :: Exp -> Exp
pre f = InfixE Nothing (VarE '(.)) (Just (ParensE f))

-- | @post g ≃ (g .)@
post :: Exp -> Exp
post g = InfixE (Just (ParensE g)) (VarE '(.)) Nothing

-- | Argument input and output positions.
ki, ko :: Name
ki = mkName "k"
ko = mkName "k'"

-- | Like (***), but with 50% extra free.
{-# INLINE xxx #-}
xxx :: (a -> a') -> (b -> b') -> (c -> c') -> (a, b, c) -> (a', b', c')
xxx f g h = \ (a, b, c) -> (f a, g b, h c)

------------------------------------------------------------------------

pos :: Name -> Type -> (Exp, Cxt, Type)
pos k typ = case typ of
    ForallT tvs cxt t -> (wrap, [], ForallT tvs (nub $ cxt' ++ cxt) t') where
        (wrap, cxt', t') = pos k t

    ArrowT `AppT` a `AppT` b -> (wrap, cxt, ArrowT `AppT` a' `AppT` b') where
        (a'unwrap, a'cxt, a') = neg ki a
        (b'wrap, b'cxt, b') = pos ko b
        cxt = nub (a'cxt ++ b'cxt)
        wrap = post b'wrap `o` pre a'unwrap

    ConT ((==) ''Key -> True) ->
        (VarE 'toEnum, [ClassP ''Enum [VarT k]], VarT k)
    ConT ((==) ''IntMap -> True) `AppT` v ->
        (ConE 'EnumMap, [], ConT ''EnumMap `AppT` VarT k `AppT` v)
    ConT ((==) ''IntSet -> True) ->
        (ConE 'EnumSet, [], ConT ''EnumSet `AppT` VarT k)
    ConT ((==) ''Maybe -> True) `AppT` a ->
            (VarE 'fmap `AppE` wrap, cxt, ConT ''Maybe `AppT` a') where
        (wrap, cxt, a') = pos k a

    TupleT 2 `AppT` a `AppT` b ->
            (wrap, cxt, TupleT 2 `AppT` a' `AppT` b') where
        (a'wrap, a'cxt, a') = pos k a
        (b'wrap, b'cxt, b') = pos k b
        cxt = nub (a'cxt ++ b'cxt)
        wrap = UInfixE (ParensE a'wrap) (VarE '(***)) (ParensE b'wrap)

    TupleT 3 `AppT` a `AppT` b `AppT` c ->
            (wrap, cxt, TupleT 3 `AppT` a' `AppT` b' `AppT` c') where
        (a'wrap, a'cxt, a') = pos k a
        (b'wrap, b'cxt, b') = pos k b
        (c'wrap, c'cxt, c') = pos k c
        cxt = nub (a'cxt ++ b'cxt ++ c'cxt)
        wrap = VarE 'xxx `AppE` a'wrap `AppE` b'wrap `AppE` c'wrap

    ListT `AppT` a -> (wrap, cxt, ListT `AppT` a') where
        (a'wrap, cxt, a') = pos k a
        wrap = VarE 'map `AppE` a'wrap

    VarT t `AppT` a -> (wrap, cxt, VarT t `AppT` a') where
        (a'wrap, cxt, a') = pos k a
        wrap = VarE '(<$>) `AppE` a'wrap

    _ -> (VarE 'id, [], typ)

------------------------------------------------------------------------

neg :: Name -> Type -> (Exp, Cxt, Type)
neg k typ = case typ of
    ArrowT `AppT` a `AppT` b -> (unwrap, cxt, ArrowT `AppT` a' `AppT` b') where
        (a'wrap, a'cxt, a') = pos ki a
        (b'unwrap, b'cxt, b') = neg ko b
        cxt = nub (a'cxt ++ b'cxt)
        unwrap = post b'unwrap `o` pre a'wrap

    ConT ((==) ''Key -> True) ->
        (VarE 'fromEnum, [ClassP ''Enum [VarT k]], VarT k)
    ConT ((==) ''IntMap -> True) `AppT` v ->
        (VarE 'unEnumMap, [], ConT ''EnumMap `AppT` VarT k `AppT` v)
    ConT ((==) ''IntSet -> True) ->
        (VarE 'unEnumSet, [], ConT ''EnumSet `AppT` VarT k)

    TupleT 2 `AppT` a `AppT` b ->
            (unwrap, cxt, TupleT 2 `AppT` a' `AppT` b') where
        (a'unwrap, a'cxt, a') = neg k a
        (b'unwrap, b'cxt, b') = neg k b
        cxt = nub (a'cxt ++ b'cxt)
        unwrap = UInfixE (ParensE a'unwrap) (VarE '(***)) (ParensE b'unwrap)

    ListT `AppT` a -> (unwrap, cxt, ListT `AppT` a') where
        (a'unwrap, cxt, a') = neg k a
        unwrap = VarE 'map `AppE` a'unwrap

    _ -> (VarE 'id, [], typ)

------------------------------------------------------------------------

substT :: Name -> Name -> Type -> Type
substT from to = subT where
    subT :: Type -> Type
    subT typ = case typ of
        VarT ((==) from -> True) -> VarT to
        s `AppT` t -> subT s `AppT` subT t
        ForallT tvs cxt t -> ForallT tvs' cxt' (subT t) where
            tvs' = nub (map subB tvs)
            cxt' = nub (map subP cxt)
        _ -> typ
    subB :: TyVarBndr -> TyVarBndr
    subB tv = case tv of
        PlainTV ((==) from -> True) -> PlainTV to
        KindedTV ((==) from -> True) k -> KindedTV to k
        _ -> tv
    subP :: Pred -> Pred
    subP p = case p of
        ClassP c ts -> ClassP c (map subT ts)
        EqualP s t -> EqualP (subT s) (subT t)

w, w' :: Name -> Q [Dec]
(w, w') = (wrap True, wrap False) where
    wrap :: Bool -> Name -> Q [Dec]
    wrap subst name@(mkName . nameBase -> base) = do
        VarI _name (pos ko -> (e, cxt', typ')) _dec fixity <- reify name
        let ks = map PlainTV [ki, ko]
        let t' = (if subst then substT ko ki else id) $ case typ' of
                ForallT tvs cxt t ->
                    ForallT (ks ++ tvs) (nub $ cxt' ++ cxt) t
                t -> ForallT ks cxt' t
        let body = NormalB (e `AppE` VarE name)
        return [ InfixD fixity base
            , PragmaD (InlineP base Inline FunLike AllPhases)
            , SigD base t', ValD (VarP base) body [] ]

