{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Exercise 1

-- Parses an individual log message

parseMessage :: String -> LogMessage
parseMessage message
  | take 2 message == "I " = LogMessage Info
                                        (read (words message !! 1) :: Int)
                                        (unwords (drop 2 (words message)))
  | take 2 message == "W " = LogMessage Warning
                                        (read (words message !! 1) :: Int)
                                        (unwords (drop 2 (words message)))
  | take 2 message == "E " = LogMessage (Error
                                        (read (words message !! 1) :: Int))
                                        (read (words message !! 2) :: Int)
                                        (unwords (drop 3 (words message)))
  | otherwise              = Unknown message

-- Parses an entire log file

parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)