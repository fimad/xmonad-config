#!/opt/ghc/7.8.4/bin/runhaskell

{-# LANGUAGE RecordWildCards #-}

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Exception (catch, SomeException)
import Control.Monad (forever)
import Data.List (isInfixOf)
import Data.Maybe (listToMaybe)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, hFlush, Handle)
import System.Process
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String


data IRCLine = IRCLine {
        ircDate    :: String
    ,   ircTime    :: String
    ,   ircNick    :: String
    ,   ircMessage :: String
    } deriving (Eq, Ord, Read, Show)

parseIRCLine :: Parser IRCLine
parseIRCLine = do
    date <- many1 $ noneOf ws
    _    <- many1 $ oneOf ws
    time <- many1 $ noneOf ws
    _    <- many1 $ oneOf ws
    nick <- many1 $ noneOf ws
    _    <- many1 $ oneOf ws
    msg  <- many1 $ noneOf "\n\r"
    _    <- many1 $ endOfLine
    return $ IRCLine date time nick msg
    where ws = " \t"

parseIRCLines :: String -> [IRCLine]
parseIRCLines = either (const []) id
              . parse (many parseIRCLine) "IRC Log"

prettyPrint :: [String] -> String -> IRCLine -> String
prettyPrint triggers channel IRCLine{..}= concat [
        "^fg(#073642)^bg(#93a1a1)"
    ,   channel, "" , sep "#93a1a1" nickBg
    ,   "^fg(", nickFg, ")", ircNick, sep nickBg "#073642"
    ,   "^fg(#839496) ", ircMessage, " ", sep "#073642" "#002b36"
    ]
    where
        sep oldBg newBg = concat ["^fg(", oldBg, ")^bg(", newBg, ")\57520"]
        (nickFg, nickBg) = if any (`isInfixOf` ircMessage) triggers
                               then ("#fdf6e3", "#6c71c4")
                               else ("#073642", "#839496")

main = do
    (channel:logFile:triggers) <- getArgs `catch` usageError
    dzenHandle <- spawnDzen2
    forever $ do
        lines <- parseIRCLines <$> readFile logFile
        let maybeLast = listToMaybe $ reverse lines
        let printToDzen = hPutStrLn dzenHandle . prettyPrint triggers channel
        maybe (return ()) printToDzen maybeLast
        hFlush dzenHandle
        threadDelay $ 1000000  -- 1 second

spawnDzen2 :: IO Handle
spawnDzen2 = do
    (Just stdin, _, _, _) <- createProcess (shell dzen2){std_in = CreatePipe}
    return stdin
    where
        font = "Inconsolata for Powerline:size=11"
        dzen2 = concat [
                "dzen2"
            ,   " -ta l"  -- Text align left.
            ,   " -y -1"  -- Bottom of the screen.
            ,   " -fn '", font, "'"
            ,   " -fg '#839496'"
            ,   " -bg '#002b36'"
            ]

usageError :: SomeException -> IO a
usageError _ = do
    putStrLn "usage: irc-status-bar [channel] [log-file] [triggers...]"
    exitFailure
