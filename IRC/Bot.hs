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

module IRC.Bot (Bot, handler, echoBot, privMsg, 
    multiBot, privMsgTextMatch, simpleResponse
    ) where

import IO
import IRC
import IRC.Parser
import Control.Monad.Writer
import Text.Regex.PCRE.Light.Char8
import Data.Time.Clock.POSIX
import System.Timeout

type Bot a = Message -> Integer -> OutputEvent a ()

process :: String -> Integer -> Bot a -> ChannelSet a -> 
    Maybe ([String], ChannelSet a)
process line time bot channels = case parseMessage line of
    Just (Ping msg) -> Just (["PONG "++msg], channels)
    Just msg@(Join _ channelName) -> 
        runOnChannel (bot msg time) channelName channels
    Just msg@(PrivMsg _ channelName _) ->
        runOnChannel (bot msg time) channelName channels
    Nothing -> Nothing

handler :: Handle -> Bot a -> ChannelSet a -> IO ()
handler h bot channels = do
    (line,channels') <- eventTimeout h channels (hGetLine h)
    print line
    time <- getPOSIXTime
    let time' = floor time
        (result, channels'') = case process line time' bot channels' of
            Just val -> val
            Nothing -> ([], channels')
    mapM (hPutStrLn h) result
    print (show result)
    handler h bot channels''

eventTimeout :: Handle -> ChannelSet a -> IO b -> IO (b, ChannelSet a)
eventTimeout h channels action =
    case nextEventTime channels of
        Nothing -> do 
            response <- action
            return (response,channels)
        Just (evTime, name) -> do
            time <- getPOSIXTime
            let time' = floor time
                diff = evTime - time'
                to = fromIntegral $ 1000000 * diff
            if time' >= evTime
                then do
                    channels' <- runEvent h name channels
                    eventTimeout h channels' action
                else do
                    response <- timeout to action
                    case response of
                        Nothing -> eventTimeout h channels action
                        Just val -> return (val, channels)

runEvent :: Handle -> String -> ChannelSet a -> IO (ChannelSet a)
runEvent h name channels =
    let result = do
        (event, channels') <- popNextEvent channels name
        runOnChannel event name channels'
    in case result of
        Nothing -> return channels
        Just (lines, channels'') -> do
            print "Running Event"
            mapM (hPutStrLn h) lines
            print (show lines)
            return channels''

echoBot (PrivMsg nick channel text) = return $ ["PRIVMSG "++channel++" :"++text]
echoBot _ = return []

privMsg msg = do
    channel <- getChannelName
    tell ["PRIVMSG " ++ channel ++ " :" ++ msg]

multiBot :: [(Message -> Maybe b, b -> Bot a)] -> Bot a
multiBot [] _ _ = return ()
multiBot ((test, processor):xs) msg time = 
    case test msg of
        Just x -> processor x msg time
        Nothing -> multiBot xs msg time

simpleResponse :: OutputEvent a () -> b -> Bot a
simpleResponse ev _ _ _ = ev 

privMsgTextMatch re reParams (PrivMsg _ _ text) = match re text reParams

