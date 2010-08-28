-- Copyright 2010 Rev. Johnny Healey
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

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

