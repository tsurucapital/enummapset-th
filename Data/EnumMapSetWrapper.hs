{-# LANGUAGE CPP #-}
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

module Data.EnumMapSetWrapper (w, w') where

import Prelude
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Arrow
import Data.List (nub)
import Data.IntSet (IntSet)
import Data.IntMap (IntMap, Key)
#if !MIN_VERSION_containers(0,5,1)
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap
#endif
import Language.Haskell.TH.Syntax

enumMap, enumSet :: Name
enumMap = mkName "EnumMap"
enumSet = mkName "EnumSet"

enumMapT :: Name -> Type -> Type
enumMapT k v = ConT enumMap `AppT` VarT k `AppT` v
enumSetT :: Name -> Type
enumSetT k = ConT enumSet `AppT` VarT k

unEnumMapE, unEnumSetE :: Exp
unEnumMapE = VarE (mkName "unEnumMap")
unEnumSetE = VarE (mkName "unEnumSet")

------------------------------------------------------------------------

-- | @o ≃ (.)@
infixr 9 `o`
o :: Exp -> Exp -> Exp
o = flip UInfixE (VarE '(.))

arrT :: Type -> Type -> Type
arrT a b = ArrowT `AppT` a `AppT` b

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

    ArrowT `AppT` a `AppT` b -> (wrap, cxt, a' `arrT` b') where
        (a'unwrap, a'cxt, a') = neg ki a
        (b'wrap, b'cxt, b') = pos ko b
        cxt = nub (a'cxt ++ b'cxt)
        wrap = post b'wrap `o` pre a'unwrap

#if MIN_VERSION_containers(0,5,1)
    ConT ((==) ''Key -> True) ->
#else
    ConT ((||) <$> (==) ''Key <*> (==) ''Int -> True) ->
#endif
#if MIN_VERSION_template_haskell(2,10,0)
        (VarE 'toEnum, [ConT ''Enum `AppT` VarT k], VarT k)
#else
        (VarE 'toEnum, [ClassP ''Enum [VarT k]], VarT k)
#endif
    ConT ((==) ''IntMap -> True) `AppT` v ->
        (ConE enumMap, [], enumMapT k v)
    ConT ((==) ''IntSet -> True) ->
        (ConE enumSet, [], enumSetT k)
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
    ArrowT `AppT` a `AppT` b -> (unwrap, cxt, a' `arrT` b') where
        (a'wrap, a'cxt, a') = pos ki a
        (b'unwrap, b'cxt, b') = neg ko b
        cxt = nub (a'cxt ++ b'cxt)
        unwrap = post b'unwrap `o` pre a'wrap

#if MIN_VERSION_containers(0,5,1)
    ConT ((==) ''Key -> True) ->
#else
    ConT ((||) <$> (==) ''Key <*> (==) ''Int -> True) ->
#endif
#if MIN_VERSION_template_haskell(2,10,0)
        (VarE 'fromEnum, [ConT ''Enum `AppT` VarT k], VarT k)
#else
        (VarE 'fromEnum, [ClassP ''Enum [VarT k]], VarT k)
#endif
    ConT ((==) ''IntMap -> True) `AppT` v ->
        (unEnumMapE, [], enumMapT k v)
    ConT ((==) ''IntSet -> True) ->
        (unEnumSetE, [], enumSetT k)

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
#if MIN_VERSION_template_haskell(2,10,0)
    subP = subT
#else
    subP p = case p of
        ClassP c ts -> ClassP c (map subT ts)
        EqualP s t -> EqualP (subT s) (subT t)
#endif

w, w' :: Name -> Q [Dec]
(w, w') = (wrap True, wrap False) where
    wrap :: Bool -> Name -> Q [Dec]
#if !MIN_VERSION_containers(0,5,1)
    wrap _ name | name == 'IntMap.size = do
        let size = mkName "size"
        let a = mkName "a"
        let t' = ForallT [PlainTV ki, PlainTV a] [] $
                enumMapT ki (VarT a) `arrT` ConT ''Int
        let body = NormalB (VarE name `o` unEnumMapE)
        return [ inlineD size, SigD size t', ValD (VarP size) body [] ]
    wrap _ name | name == 'IntSet.size = do
        let size = mkName "size"
        let t' = ForallT [PlainTV ki] [] $
                enumSetT ki `arrT` ConT ''Int
        let body = NormalB (VarE name `o` unEnumSetE)
        return [ inlineD size, SigD size t', ValD (VarP size) body [] ]
#endif
    wrap subst name@(mkName . nameBase -> base) = do
        VarI _name (pos ko -> (e, cxt', typ')) _dec _fixity <- reify name
        let ks = map PlainTV [ki, ko]
        let t' = (if subst then substT ko ki else id) $ case typ' of
                ForallT tvs cxt t ->
                    ForallT (ks ++ tvs) (nub $ cxt' ++ cxt) t
                t -> ForallT ks cxt' t
        let body = NormalB (e `AppE` VarE name)
        return [ inlineD base, SigD base t', ValD (VarP base) body [] ]

    inlineD base = PragmaD $ InlineP base
#if MIN_VERSION_template_haskell(2,8,0)
        Inline FunLike AllPhases
#else
        (InlineSpec True False Nothing)
#endif

