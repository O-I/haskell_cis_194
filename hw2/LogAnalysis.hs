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
  | message > m = Node (insert l Leaf) m (insert message r)
  | otherwise   = Node (insert message l) m (insert r Leaf)

-- Compares two LogMessages by timestamp

instance Ord LogMessage where
  Unknown _ = error "Unknown has no timestamp"
  (x timestamp1 y) `compare` (x timestamp2 y) = timestamp1 `compare` timestamp2
