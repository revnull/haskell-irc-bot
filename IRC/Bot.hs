
module IRC.Bot (Bot, handler, echoBot, privMsg, 
    multiBot, privMsgTextMatch, simpleResponse
    ) where

import IO
import IRC
import IRC.Parser
import Control.Monad.Writer
import Text.Regex.PCRE.Light.Char8

type Bot a = Message -> OutputEvent a ()

process :: String -> Bot a -> ChannelSet a -> Maybe ([String], ChannelSet a)
process line bot channels = case parseMessage line of
    Just (Ping msg) -> Just (["PONG "++msg], channels)
    Just msg@(Join _ channelName) -> 
        runOnChannel (bot msg) channelName channels
    Just msg@(PrivMsg _ channelName _) ->
        runOnChannel (bot msg) channelName channels
    Nothing -> Nothing

handler :: Handle -> Bot a -> ChannelSet a -> IO ()
handler h bot channels = do
    line <- hGetLine h
    print line
    let (result, channels') = case process line bot channels of
            Just val -> val
            Nothing -> ([], channels)
    mapM (hPutStrLn h) result
    print (show result)
    handler h bot channels'

echoBot (PrivMsg nick channel text) = return $ ["PRIVMSG "++channel++" :"++text]
echoBot _ = return []

privMsg msg = do
    channel <- getChannelName
    tell ["PRIVMSG " ++ channel ++ " :" ++ msg]

multiBot :: [(Message -> Maybe b, b -> Bot a)] -> Bot a
multiBot [] _ = return ()
multiBot ((test, processor):xs) msg = 
    case test msg of
        Just x -> processor x msg
        Nothing -> multiBot xs msg

simpleResponse :: OutputEvent a () -> b -> Bot a
simpleResponse ev _ _ = ev 

privMsgTextMatch re reParams (PrivMsg _ _ text) = match re text reParams

