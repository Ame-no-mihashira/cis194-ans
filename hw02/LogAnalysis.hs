{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Data.List (foldl')

import Log

parseMessage :: String -> LogMessage
parseMessage s = case words s of
  ("I" : ts : xs) -> LogMessage Info (read ts) (unwords xs)
  ("W" : ts : xs) -> LogMessage Warning (read ts) (unwords xs)
  ("E" : lv : ts : xs) -> LogMessage (Error (read lv)) (read ts) (unwords xs)
  _ -> Unknown s

parse :: String -> [LogMessage]
parse =
  map parseMessage
    . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert x Leaf = Node Leaf x Leaf
insert x@(LogMessage _ t1 _) (Node lhs (LogMessage _ t2 _) rhs)
  | t1 < t2 = insert x lhs
  | t1 > t2 = insert x rhs
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldl' (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder (Node lhs x rhs) =
  inOrder lhs
    ++ x
    : inOrder rhs
inOrder _ = []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =
  map message
    . filter wrong
 where
  wrong x = case x of
    (LogMessage (Error lv) _ _) -> lv >= 50
    _ -> False
  message x = case x of
    (LogMessage _ _ s) -> s
    _ -> ""
