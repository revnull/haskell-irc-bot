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

module IRC.Bots.Boggle.Board (Board, makeBoard, rows) where

import Random
import qualified Data.Map as Map
import Data.Array
import Maybe

type Board = Array (Int,Int) String

letterFreqs :: Map.Map String [Int]
letterFreqs = Map.fromList [
    ("QU", [3]),
    ("A", [73, 15, 2, 1, 1]),
    ("C", [43, 6, 1]),
    ("B", [21, 2, 1, 1]),
    ("E", [100, 35, 7, 1, 1]),
    ("D", [43, 6, 1, 1]),
    ("G", [35, 4, 1, 1]),
    ("F", [16, 2, 1, 1]),
    ("I", [81, 22, 4, 1, 1, 1]),
    ("H", [26, 2, 1]),
    ("K", [11, 1, 1, 1]),
    ("J", [3, 1]),
    ("M", [29, 4, 1, 1]),
    ("L", [53, 10, 1, 1]),
    ("O", [58, 13, 2, 1]),
    ("N", [69, 16, 2, 1, 1]),
    ("P", [32, 4, 1, 1]),
    ("S", [82, 23, 4, 1, 1, 1]),
    ("R", [73, 14, 1, 1]),
    ("U", [36, 3, 1, 1]),
    ("T", [68, 15, 2, 1]),
    ("W", [11, 1, 1]),
    ("V", [13, 1]),
    ("Y", [18, 1]),
    ("X", [4, 1]),
    ("Z", [5, 1, 1, 1])
    ]

getLetter i ((l,(x:_)):xs) =
    if x >= i
        then l
        else getLetter (i - x) xs

updateFreqs l total freqs =
    let (x:xs) = fromJust $ Map.lookup l freqs
        freqs' = if xs == []
            then Map.delete l freqs
            else Map.insert l xs freqs
    in (total - x, freqs')

letterIter gen _ _ 0 = ([],gen)
letterIter gen total freqs remaining =
    let (i, gen') = randomR (0,total - 1) gen
        val = getLetter i $ Map.toList freqs
        (total', freqs') = updateFreqs val total freqs
        (next, gen'') = letterIter gen' total' freqs' (remaining - 1)
    in (val:next, gen'')

randomLetters gen count =
    let totalCount = sum $ [head l | (_,l) <- Map.toList letterFreqs]
    in letterIter gen totalCount letterFreqs count

makeBoard :: RandomGen r => r -> (Board,r)
makeBoard gen =
    let (vals, gen') = randomLetters gen 16
        board = array ((0,0),(3,3)) $ do
            (i,l) <- zip [0..] vals
            return (divMod i 4, l) 
    in (board, gen')

rows :: Board -> [[String]]
rows board = do
    row <- [0..3]
    return $ do
        col <- [0..3]
        return $ board!(row,col)

