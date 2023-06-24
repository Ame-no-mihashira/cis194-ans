module JoinList where

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: (Monoid m) => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ x = x
x +++ Empty = x
lhs +++ rhs = Append (tag lhs <> tag rhs) lhs rhs

tag :: (Monoid m) => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m
