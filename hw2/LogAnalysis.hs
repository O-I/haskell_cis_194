{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Exercise 1

-- Parses an individual log message

parseMessage :: String -> LogMessage
parseMessage message
  | kind == "I"  = LogMessage Info
                   (intArg 1)
                   (unwords (drop 2 msg))
  | kind == "W"  = LogMessage Warning
                   (intArg 1)
                   (unwords (drop 2 msg))
  | kind == "E"  = LogMessage (Error
                   (intArg 1))
                   (intArg 2)
                   (unwords (drop 3 msg))
  | otherwise    = Unknown (unwords msg)
  where msg      = words message
        kind     = head msg
        intArg n = read (msg !! n) :: Int

-- Parses an entire log file

parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)