module JoinList where

import Buffer
import Scrabble
import Sized

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: (Monoid m) => JoinList m a -> JoinList m a -> JoinList m a
xs +++ Empty = xs
Empty +++ xs = xs
lhs +++ rhs = Append (tag lhs <> tag rhs) lhs rhs

tag :: (Monoid m) => JoinList m a -> m
tag (Append t _ _) = t
tag (Single t _) = t
tag _ = mempty

fromTag :: (Sized b, Monoid b) => JoinList b a -> Int
fromTag = getSize . size . tag

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x : xs) !!? 0 = Just x
(x : xs) !!? i = xs !!? (i - 1)

indexJ ::
  (Sized b, Monoid b) =>
  Int ->
  JoinList b a ->
  Maybe a
indexJ idx xs@(Append _ lhs rhs)
  | idx < sizeL = indexJ idx lhs
  | idx < sizeC = indexJ (idx - sizeL) rhs
 where
  sizeL = fromTag lhs
  sizeC = fromTag xs
indexJ 0 (Single _ a) = return a
indexJ _ _ = Nothing

dropJ ::
  (Sized b, Monoid b) =>
  Int ->
  JoinList b a ->
  JoinList b a
dropJ qty xs@(Append _ lhs rhs)
  | qty < sizeL = dropJ qty lhs +++ rhs
  | qty < sizeC = dropJ (qty - sizeL) rhs
 where
  sizeL = fromTag lhs
  sizeC = fromTag xs
dropJ qty xs | qty <= 0 = xs
dropJ _ _ = Empty

takeJ ::
  (Sized b, Monoid b) =>
  Int ->
  JoinList b a ->
  JoinList b a
takeJ qty xs@(Append _ lhs rhs)
  | qty < sizeL = takeJ qty lhs
  | qty < sizeC = lhs +++ takeJ (qty - sizeL) rhs
 where
  sizeL = fromTag lhs
  sizeC = fromTag xs
takeJ qty xs | qty <= 0 = Empty
takeJ _ xs = xs

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

sample =
  Append
    (Size 4)
    ( Append
        (Size 3)
        (Single (Size 1) 'y')
        ( Append
            (Size 2)
            (Single (Size 1) 'e')
            (Single (Size 1) 'a')
        )
    )
    (Single (Size 1) 'h')

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

splitHalf :: [a] -> ([a], [a])
splitHalf xs =
  splitAt half xs
 where
  half = flip div 2 $ length xs

instance Buffer (JoinList (Score, Size) String) where
  toString (Append _ lhs rhs) = toString lhs ++ toString rhs
  toString (Single _ a) = a
  toString _ = ""
  fromString s = case words s of
    [] -> Empty
    [x] -> Single (scoreString x, 1) x
    xs ->
      lhs +++ rhs
     where
      half = flip div 2 $ length xs
      (as, bs) = splitAt half xs
      (lhs, rhs) = (fromString $ unwords as, fromString $ unwords bs)
  line = indexJ
  replaceLine idx s xs =
    takeJ idx xs
      +++ fromString s
      +++ dropJ (idx + 1) xs
  numLines = getSize . snd . tag
  value = getScore . fst . tag
