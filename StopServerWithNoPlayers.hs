module StopServerWithNoPlayers (main) where

import Text.Parsec.String (Parser)
import Text.Parsec.Char (string)
import Text.Parsec (parse)
import Text.Parsec.Number (nat)
import System.Exit (die)
import System.Process (readProcess, callProcess)

-- Use the list command to see if anyone is currently online. If someone
-- is online, do nothing. Otherwise, check the logs.
-- $ docker exec mc rcon-cli list
-- There are 0 of a max 20 players online:

main :: IO ()
main = getNumPlayersOnline >>= print

getNumPlayersOnline :: IO Int
getNumPlayersOnline = do
  output <- readProcess "/usr/bin/docker" ["exec", "mc", "rcon-cli", "list"] ""
  case parse playerList "" output of
    Left err -> die (show err)
    Right numPlayers -> return numPlayers

playerList :: Parser Int
playerList =
  string "There are " *> nat <*
  (string " of a max " >> nat >> string " players online:")

stopServer :: IO ()
stopServer = callProcess "/usr/bin/docker" ["exec", "mc", "rcon-cli", "stop"]

-- Check the logs for most recent player logout message, which looks like this:
-- [2018-10-27T16:32:41] [Server thread/INFO]: LambdaCube[/172.17.0.1:49542] logged in with entity id 452 at (-126.5, 79.0, 216.5)
-- [2018-10-27T16:32:41] [Server thread/INFO]: LambdaCube joined the game
-- [2018-10-27T16:36:12] [Server thread/INFO]: LambdaCube lost connection: Disconnected
-- [2018-10-27T16:36:12] [Server thread/INFO]: LambdaCube left the game

-- If the timestamp on the most recent logout is more than some grace period 
-- time in the past, like say, more than 10 minutes ago (configurable), then
-- shutdown the server with this command.
-- rcon-cli stop
