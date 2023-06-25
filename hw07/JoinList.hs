module JoinList where

import Distribution.Simple.Utils (xargs)
import Sized

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: (Monoid m) => JoinList m a -> JoinList m a -> JoinList m a
lhs +++ rhs = Append (tag lhs <> tag rhs) lhs rhs

tag :: (Monoid m) => JoinList m a -> m
tag (Single t _) = t
tag (Append t _ _) = t
tag _ = mempty

indexJ ::
  (Sized b, Monoid b) =>
  Int ->
  JoinList b a ->
  Maybe a
indexJ 0 (Single _ a) = Just a
indexJ idx (Append q lhs rhs)
  | idx < sizeL = indexJ idx lhs
  | idx <= sizeC = indexJ (idx - sizeL) rhs
 where
  sizeL = getSize . size $ tag lhs
  sizeC = getSize . size $ q
indexJ _ _ = Nothing
