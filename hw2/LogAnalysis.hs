{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Exercise 1

-- Parse an individual log message
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

-- Parse an entire log file
parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)

-- Exercise 2

-- Insert a LogMessage into a MessageTree
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree     = tree
insert message Leaf         = Node Leaf message Leaf
insert message (Node l m r)
  | message `after` m = Node l m (insert message r)
  | otherwise         = Node (insert message l) m r

-- Compare two LogMessages by timestamp
after :: LogMessage -> LogMessage -> Bool
after (LogMessage _ t1 _) (LogMessage _ t2 _) = t1 > t2
after _ _ = error "Missing timestamp"

-- Exercise 3

-- Build a MessageTree given a list of LogMessages
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4

-- Produce a list of LogMessages sorted by timestamp
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf         = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

-- Exercise 5

-- Extract LogMessages with severity >= 50 from unsorted list
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs =
  [msg | m@(LogMessage _ _ msg) <- inOrder (build msgs), severityAtAndOver50 m]

severityAtAndOver50 :: LogMessage -> Bool
severityAtAndOver50 (LogMessage (Error severity) _ _) = severity >= 50
severityAtAndOver50 _ = False