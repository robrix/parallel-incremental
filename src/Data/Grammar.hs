{-# LANGUAGE DataKinds, FlexibleInstances, GADTs, RankNTypes, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Data.Grammar
( Grammar(..)
) where

import Control.Applicative
import Control.Monad (guard)
import Data.Coerce
import qualified Data.Higher.Foldable as H
import qualified Data.Higher.Functor as H
import qualified Data.Higher.Monoid as H
import Data.Higher.Functor.Classes as H
import Data.Recursive
import GHC.Generics
import GHC.TypeLits
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token

data Grammar t r a
  = Err [String]
  | Nul a
  | Sat (t -> Maybe a)
  | Alt (r a) (r a)
  | forall c b . Seq (c -> b -> a) (r c) (r b)
  | Lab (r a) String
  | End a

instance (Bounded t, Enum t, Show t) => H.Show1 (Grammar t) where
  liftShowsPrec sp d g = case g of
    Err es    -> showsUnaryWith   showsPrec              "Err" d es
    Nul a     -> showsUnaryWith   hide                   "Nul" d a
    Sat p     -> showsUnaryWith   hide                   "Sat" d p
    Alt a b   -> showsBinaryWith  sp        sp           "Alt" d a b
    Seq f a b -> showsTernaryWith hide      sp        sp "Seq" d f a b
    Lab r s   -> showsBinaryWith  sp        showsPrec    "Lab" d r s
    End a     -> showsUnaryWith   hide                   "End" d a
    where hide _ _ = showChar '_'

instance (Corecursive1 (r (Grammar t)), Cobase1 (r (Grammar t)) ~ Grammar t) => Functor (r (Grammar t)) where
  fmap = liftA

instance (Corecursive1 (r (Grammar t)), Cobase1 (r (Grammar t)) ~ Grammar t) => Applicative (r (Grammar t)) where
  pure = embed1 . Nul
  liftA2 f a b = embed1 (Seq f a b)

instance (Corecursive1 (r (Grammar t)), Cobase1 (r (Grammar t)) ~ Grammar t, Recursive (r (Grammar t))) => Alternative (r (Grammar t)) where
  empty = embed1 (Err [])
  a <|> b = embed1 (Alt a b)
  many a = mu (\ more -> (:) <$> a <*> more <|> pure [])
  some a = (:) <$> a <*> many a

instance (Corecursive1 (r (Grammar t)), Cobase1 (r (Grammar t)) ~ Grammar t, Recursive (r (Grammar t))) => Parsing (r (Grammar t)) where
  try = id
  a <?> s = embed1 (Lab a s)
  eof = embed1 (End ())
  unexpected s = embed1 (Err [s])
  notFollowedBy a = a *> empty <|> pure ()

instance (Corecursive1 (r (Grammar Char)), Cobase1 (r (Grammar Char)) ~ Grammar Char, Recursive (r (Grammar Char))) => CharParsing (r (Grammar Char)) where
  satisfy p = embed1 (Sat (\ c -> guard (p c) *> Just c))

instance (Corecursive1 (r (Grammar Char)), Cobase1 (r (Grammar Char)) ~ Grammar Char, Recursive (r (Grammar Char))) => TokenParsing (r (Grammar Char))

instance H.Foldable (Grammar t) where
  foldMap f g = case g of
    Err _     -> H.mempty
    Nul _     -> H.mempty
    Sat _     -> H.mempty
    Alt a b   -> f a H.<> f b
    Seq _ a b -> f a H.<> f b
    Lab r _   -> f r
    End _     -> H.mempty

instance H.Functor (Grammar t) where
  fmap f g = case g of
    Err es    -> Err es
    Nul a     -> Nul a
    Sat p     -> Sat p
    Alt a b   -> Alt (f a) (f b)
    Seq g a b -> Seq g (f a) (f b)
    Lab r s   -> Lab (f r) s
    End a     -> End a

instance Generic1 (Grammar t r) where
  type Rep1 (Grammar t r) = D1
    ('MetaData "Grammar" "Data.Grammar" "parallel-incremental" 'False)
    (   (   (   C1
                  ('MetaCons "Err" 'PrefixI 'False)
                  (S1
                    ('MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                    (K1 R [String]))
            :+: C1
                  ('MetaCons "Nul" 'PrefixI 'False)
                  (S1
                    ('MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                    Par1))
        :+: (   C1
                  ('MetaCons "Sat" 'PrefixI 'False)
                  (S1
                    ('MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                    ((->) t :.: Maybe))
            :+: C1
                  ('MetaCons "Alt" 'PrefixI 'False)
                  (   S1
                    ('MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                    (Rec1 r)
                  :*: S1
                    ('MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                    (Rec1 r))))
    :+: (   (   C1
                  ('MetaCons "Seq" 'PrefixI 'False)
                  (Exists1
                    (Exists2
                      (     S3
                              ('MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                              (App3 0 (->) :...: App3 1 (->))
                      :***: S3
                              ('MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                              (Rec3 0 r)
                      :***: S3
                              ('MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                              (Rec3 1 r)
                      )))
            :+: C1
                  ('MetaCons "Lab" 'PrefixI 'False)
                  (   S1
                        ('MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                        (Rec1 r)
                  :*: S1
                        ('MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                        (K1 R String)))
        :+: C1
              ('MetaCons "End" 'PrefixI 'False)
              (S1
                ('MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                Par1)))
  from1 (Err e)     = M1 (L1 (L1 (L1 (coerce e))))
  from1 (Nul a)     = M1 (L1 (L1 (R1 (coerce a))))
  from1 (Sat p)     = M1 (L1 (R1 (L1 (coerce p))))
  from1 (Alt a b)   = M1 (L1 (R1 (R1 (M1 (coerce a :*: coerce b)))))
  from1 (Seq f a b) = M1 (R1 (L1 (L1 (M1 (Exists1 (Exists2 (coerce f :***: M3 (Rec3 a) :***: M3 (Rec3 b))))))))
  from1 (Lab a s)   = M1 (R1 (L1 (R1 (M1 (coerce a :*: coerce s)))))
  from1 (End a)     = M1 (R1 (R1 (coerce a)))
  to1 (M1 (L1 (L1 (L1 e)))) = Err (coerce e)
  to1 (M1 (L1 (L1 (R1 a)))) = Nul (coerce a)
  to1 (M1 (L1 (R1 (L1 p)))) = Sat (coerce p)
  to1 (M1 (L1 (R1 (R1 (M1 (a :*: b)))))) = Alt (coerce a) (coerce b)
  to1 (M1 (R1 (L1 (L1 (M1 (Exists1 (Exists2 (f :***: (M3 (Rec3 a) :***: M3 (Rec3 b)))))))))) = Seq (coerce f) a b
  to1 (M1 (R1 (L1 (R1 (M1 (a :*: s)))))) = Lab (coerce a) (coerce s)
  to1 (M1 (R1 (R1 a))) = End (coerce a)

data Exists1 f   a = forall b . Exists1 { unExists1 :: f   b a }
data Exists2 f b a = forall c . Exists2 { unExists2 :: f c b a }

newtype (f :...: g) a b c = Comp3 { unComp3 :: f a b (g a b c) }

infixr 7 :...:

data (f :***: g) c b a = f c b a :***: g c b a
  deriving (Eq, Ord, Show)

infixr 6 :***:

newtype App3 p f a b c = App3 { unApp3 :: f (At3 p a b c) c }

deriving instance Eq   (f a c) => Eq   (App3 0 f a b c)
deriving instance Eq   (f b c) => Eq   (App3 1 f a b c)
deriving instance Eq   (f c c) => Eq   (App3 2 f a b c)
deriving instance Ord  (f a c) => Ord  (App3 0 f a b c)
deriving instance Ord  (f b c) => Ord  (App3 1 f a b c)
deriving instance Ord  (f c c) => Ord  (App3 2 f a b c)
deriving instance Show (f a c) => Show (App3 0 f a b c)
deriving instance Show (f b c) => Show (App3 1 f a b c)
deriving instance Show (f c c) => Show (App3 2 f a b c)

newtype Par3 p a b c = Par3 { unPar3 :: At3 p a b c }

deriving instance Eq   a => Eq   (Par3 0 a b c)
deriving instance Eq   b => Eq   (Par3 1 a b c)
deriving instance Eq   c => Eq   (Par3 2 a b c)
deriving instance Ord  a => Ord  (Par3 0 a b c)
deriving instance Ord  b => Ord  (Par3 1 a b c)
deriving instance Ord  c => Ord  (Par3 2 a b c)
deriving instance Show a => Show (Par3 0 a b c)
deriving instance Show b => Show (Par3 1 a b c)
deriving instance Show c => Show (Par3 2 a b c)

newtype Rec3 p f a b c = Rec3 { unRec3 :: f (At3 p a b c) }

deriving instance Eq   (f a) => Eq   (Rec3 0 f a b c)
deriving instance Eq   (f b) => Eq   (Rec3 1 f a b c)
deriving instance Eq   (f c) => Eq   (Rec3 2 f a b c)
deriving instance Ord  (f a) => Ord  (Rec3 0 f a b c)
deriving instance Ord  (f b) => Ord  (Rec3 1 f a b c)
deriving instance Ord  (f c) => Ord  (Rec3 2 f a b c)
deriving instance Show (f a) => Show (Rec3 0 f a b c)
deriving instance Show (f b) => Show (Rec3 1 f a b c)
deriving instance Show (f c) => Show (Rec3 2 f a b c)

newtype M3 i (m :: Meta) f a b c = M3 { unM3 :: f a b c }
type S3 = M3 S

type family At2 (n :: Nat) a b where
  At2 0 a _ = a
  At2 1 _ b = b

type family At3 (n :: Nat) a b c where
  At3 0 a _ _ = a
  At3 1 _ b _ = b
  At3 2 _ _ c = c
