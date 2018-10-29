#!/usr/bin/env stack
-- stack --resolver lts-12.5 --install-ghc runghc --package parsec
module StopServerWithNoPlayers (main) where

import System.Exit (die)
import System.Process (readProcess, callProcess)

import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec.Char (string, digit, char, endOfLine, anyChar, oneOf)
import Text.Parsec (parse, many, many1)
import Text.Parsec.Combinator (count, between, manyTill)

import Data.Time.LocalTime (
  LocalTime(..), TimeOfDay(..), zonedTimeToLocalTime, getZonedTime,
  utcToLocalTime, utc, localTimeToUTC)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (addUTCTime)

import Data.List (isInfixOf)

data LogEntry = LogEntry {
  logEntryDate :: LocalTime,
  logEntryText :: String
  }

main :: IO ()
main = do
  n <- getNumPlayersOnline
  if n > 0
  then putStrLn "Players are online."
  else do d <- getLastActiveDateTime "test.log"
          putStrLn $ "Activity found at " ++ show d
          now <- zonedTimeToLocalTime <$> getZonedTime
          if addSeconds (60*10) d < now
          then putStrLn "Stopping the server." >> stopServer
          else putStrLn "Doing nothing."

getNumPlayersOnline :: IO Int
getNumPlayersOnline = do
  output <- readProcess "/usr/bin/docker" ["exec", "mc", "rcon-cli", "list"] ""
  case parse playerList "" output of
    Left err -> die (show err)
    Right numPlayers -> return numPlayers

playerList :: Parser Int
playerList = do
  string "There are "
  numStr <- many1 digit
  string " of a max "
  many1 digit
  string " players online:"
  return (read numStr)

getLastActiveDateTime :: String -> IO LocalTime
getLastActiveDateTime logFilename = do
  result <- parseFromFile (many logLine) logFilename
  case result of
    Left err -> die (show err)
    Right logLines ->
      return $ maximum $ map logEntryDate $ filter isActivity logLines

logLine :: Parser LogEntry
logLine = do
  dateTime <- between (char '[') (char ']') iso8601
  message <- manyTill anyChar endOfLine
  return $ LogEntry dateTime message

iso8601 :: Parser LocalTime
iso8601 = do
  year <- count 4 digit
  char '-'
  month <- count 2 digit
  char '-'
  day <- count 2 digit
  let calendarDay = fromGregorian (read year) (read month) (read day)
  oneOf "T "
  hour <- count 2 digit
  char ':'
  minute <- count 2 digit
  char ':'
  second <- count 2 digit
  let timeOfDay = TimeOfDay (read hour) (read minute) (read second)
  return (LocalTime calendarDay timeOfDay)

isActivity :: LogEntry -> Bool
isActivity (LogEntry _ text) = 
  isInfixOf "Starting Minecraft server" text || isInfixOf "Disconnected" text

stopServer :: IO ()
stopServer = callProcess "/usr/bin/docker" ["exec", "mc", "rcon-cli", "stop"]

addSeconds :: Real a => a -> LocalTime -> LocalTime
addSeconds s = utcToLocalTime utc
             . addUTCTime (realToFrac s)
             . localTimeToUTC utc
