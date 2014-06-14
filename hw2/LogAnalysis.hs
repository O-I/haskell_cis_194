{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Exercise 1

-- Parses an individual log message

parseMessage :: String -> LogMessage
parseMessage message
  | kind == "I"  = LogMessage  Info    (intArg 1)  (strArg 2)
  | kind == "W"  = LogMessage  Warning (intArg 1)  (strArg 2)
  | kind == "E"  = LogMessage (Error   (intArg 1)) (intArg 2) (strArg 3)
  | otherwise    = Unknown message
  where msg      = words message
        kind     = head msg
        intArg n = read (msg !! n) :: Int
        strArg n = unwords (drop n msg)

-- Parses an entire log file

parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)

-- Exercise 2

-- Inserts a LogMessage into a MessageTree

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree     = tree
insert message Leaf         = Node Leaf message Leaf
insert message (Node l m r)
  | message `after` m = Node l m (insert message r)
  | otherwise         = Node (insert message l) m r

-- Compares two LogMessages by timestamp

after :: LogMessage -> LogMessage -> Bool
after (LogMessage _ t1 _) (LogMessage _ t2 _) = t1 > t2
after _ _ = error "Missing timestamp"

-- Exercise 3

-- Builds a MessageTree given a list of LogMessages

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4

-- Produces a list of LogMessages sorted by timestamp

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf         = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r