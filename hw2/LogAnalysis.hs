{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Exercise 1

-- Parses an individual log message

parseMessage :: String -> LogMessage
parseMessage message
  | kind == "I" = LogMessage Info
                  (read (msg !! 1) :: Int)
                  (unwords (drop 2 msg))
  | kind == "W" = LogMessage Warning
                  (read (msg !! 1) :: Int)
                  (unwords (drop 2 msg))
  | kind == "E" = LogMessage (Error
                  (read (msg !! 1) :: Int))
                  (read (msg !! 2) :: Int)
                  (unwords (drop 3 msg))
  | otherwise   = Unknown (unwords msg)
  where msg     = words message
        kind    = head msg

-- Parses an entire log file

parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)