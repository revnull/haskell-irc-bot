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

import IRC
module IRC (Channel, ChannelSet, Event, OutputEvent, EventSet,
    Message(Ping, PrivMsg, Join, Quit),
    connectIRC, joinChannel, joinChannels, delayEvent, get, put,
    getChannelName, runOnChannel ) where

import System.IO
import Network
import Text.Printf
import Text.Regex.PCRE.Light.Char8
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Writer
import qualified Control.Monad.State as State

data Channel a = Channel String a (EventSet a)
type ChannelSet a = Map.Map String (Channel a)

type Event a = State.State (Channel a)
type OutputEvent a = WriterT [String] (Event a)
type EventSet a = Map.Map Int (OutputEvent a ())

data Message = Ping String 
    | PrivMsg String String String 
    | Join String String 
    | Quit String String
    deriving Show

connectIRC :: String -> Int -> String -> Maybe String -> IO Handle
connectIRC server port nick passwd = do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    case passwd of
        (Just pass) -> hPrintf h "PASS %s\n" pass
        Nothing -> return ()
    hPrintf h "NICK %s\n" nick
    hPrintf h "USER %s 0 * :Haskell IRC Bot\n" nick
    return $ h

joinChannel :: Handle -> String -> a -> IO (Channel a)
joinChannel h channel state = do 
    hPrintf h "JOIN %s\n" channel
    return $ Channel channel state Map.empty

joinChannels :: Handle -> [(String, a)] -> IO (ChannelSet a)
joinChannels h channels = do
    channels' <- mapM (uncurry $ joinChannel h) channels
    return $ Map.fromList $ zip [name | (name,_) <- channels] channels'

get :: OutputEvent a a
get = do
    (Channel _ state _) <- lift State.get
    return state

put :: a -> OutputEvent a ()
put new = do
    (Channel name state events) <- State.get
    lift . State.put $ (Channel name new events)
    return ()

getEventSet :: OutputEvent a (EventSet a)
getEventSet = do
    (Channel _ _ events) <- lift State.get
    return events

setEventSet :: EventSet a -> OutputEvent a ()
setEventSet events = do
    (Channel name state _) <- lift State.get
    lift . State.put $ (Channel name state events)
    return ()

delayEvent :: Int -> OutputEvent a () -> OutputEvent a ()
delayEvent delay ev = do
    (Channel name state events) <- lift State.get
    lift . State.put $ Channel name state (Map.insert delay ev events)
    return ()

getChannelName :: OutputEvent a String
getChannelName = do
    (Channel name _ _) <- lift State.get
    return name

runOnChannel :: OutputEvent a () -> String -> ChannelSet a -> 
    Maybe ([String], ChannelSet a)
runOnChannel wr channelName channels = do
    channel <- Map.lookup channelName channels
    let st = runWriterT wr
        ((_, result), channel') = State.runState st channel
    return (result, Map.insert channelName channel' channels)

