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

module IRC.Bots.Boggle (boggle, initialBoggle) where

import IRC
import IRC.Bot
import Text.Regex.PCRE.Light.Char8
import Data.Array
import qualified Data.Map as Map
import qualified Data.Set as Set
import Random
import IRC.Bots.Boggle.Board

type Scores = Map.Map String Int
type WordSet = Set.Set String

data Game = Game Board Integer Scores WordSet
data BoggleBot = BoggleBot (Maybe Game) StdGen Int

getGame = do
    (BoggleBot game _ _) <- get
    return game

putGame game = do
    (BoggleBot _ gen sb) <- get
    put (BoggleBot game gen sb)

getGen = do
    (BoggleBot _ gen _) <- get
    return gen

putGen gen = do
    (BoggleBot game _ sb) <- get
    put (BoggleBot game gen sb)

getScrollback = do
    (BoggleBot _ _ sb) <- get
    return sb

putScrollback sb = do
    (BoggleBot game gen _) <- get
    put (BoggleBot game gen sb)

makeRandomBoard :: OutputEvent BoggleBot Board
makeRandomBoard = do
    gen <- getGen
    let (board, gen') = makeBoard gen
    putGen gen'
    return board

formatLetter l = 
    if length l == 1
        then l ++ "  "
        else l ++ " "

outputBoard :: Board -> OutputEvent BoggleBot ()
outputBoard b = do
    mapM privMsg [concatMap formatLetter r | r <- rows b]
    return ()

initialBoggle init = BoggleBot Nothing (mkStdGen init) 0

finishGame = do
    privMsg "Time is up!"
    putGame Nothing

play :: Bot BoggleBot
play msg ts = return ()

startGame :: Bot BoggleBot
startGame msg ts = 
    let re = compile "boggle" [caseless]
    in case privMsgTextMatch re [] msg of
        Just _ -> do
            board <- makeRandomBoard
            putGame $ Just (Game board ts Map.empty Set.empty)
            outputBoard board
            delayEvent (ts + 180) finishGame
        Nothing -> return ()

boggle :: Bot BoggleBot
boggle msg ts = do
    (BoggleBot game _ _) <- get
    case game of
        Just g -> play msg ts
        Nothing -> startGame msg ts

