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
  | otherwise    = Unknown (unwords msg)
  where msg      = words message
        kind     = head msg
        intArg n = read (msg !! n) :: Int
        strArg n = unwords (drop n msg)

-- Parses an entire log file

parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)