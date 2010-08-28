
module IRC.Parser (parseMessage) where

import IRC
import Text.Regex.PCRE.Light.Char8
import Control.Monad

parseMessage input = foldl mplus mzero $ map (\x -> x input) [
    parsePing,
    parseJoin,
    parsePrivMsg,
    parseQuit
    ]

parse re input =
    let re' = compile re []
    in match re' input []

parsePing input = do
    results <- parse "^PING (.*)$" input
    return $ Ping (results !! 1)

parseJoin input = do
    results <- parse "^:([^!]+)![^ ]+ JOIN :(.*)$" input
    return $ Join (results !! 1) (results !! 2)

parseQuit input = do
    results <- parse "^:([^!]+)![^ ]+ QUIT :(.*)$" input
    return $ Join (results !! 1) (results !! 2)

parsePrivMsg input = do
    results <- parse "^:([^!]+)![^ ]+ PRIVMSG ([^ ]+) :(.*)$" input
    return $ PrivMsg (results !! 1) (results !! 2) (results !! 3)

