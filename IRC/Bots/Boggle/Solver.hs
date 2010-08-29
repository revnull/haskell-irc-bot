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

module IRC.Bots.Boggle.Solver (loadDictionary, member, solve) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import IO
import IRC.Bots.Boggle.Board
import System.IO

data Dictionary = Empty | Leaf | Node Bool (Map.Map Char Dictionary)
    deriving Show

insert :: String -> Dictionary -> Dictionary
insert "" (Node _ children) = Node True children
insert "" _ = Leaf
insert (x:xs) Empty = Node False $ Map.fromList [(x,insert xs Empty)]
insert (x:xs) Leaf = Node True $ Map.fromList [(x,insert xs Empty)]
insert (x:xs) (Node isWord children) = 
    let child = case Map.lookup x children of
            Nothing -> insert xs Empty
            Just val -> insert xs val
        children' = Map.insert' x child children
    in Node isWord children' 

member :: String -> Dictionary -> Bool
member _ Empty = False
member "" (Node isWord _) = isWord
member "" Leaf = True
member _ Leaf = False
member (x:xs) (Node _ children) = 
    case Map.lookup x children of
        Nothing -> False
        Just child -> member xs child

loadDictionaryIter :: Dictionary -> Handle -> IO (Dictionary)
loadDictionaryIter dict h = do
    result <- try (hGetLine h)
    case result of
        (Right line) -> loadDictionaryIter (insert line dict) h
        (Left eofErrorType) -> return dict

loadDictionary filename = withFile filename ReadMode $ loadDictionaryIter Empty

solve :: Dictionary -> Board -> Set.Set String
solve dict board = undefined

