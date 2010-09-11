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

module IRC.Bots.Boggle.Solver (Dictionary, loadDictionary, member, solve) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Array
import IO
import IRC.Bots.Boggle.Board
import System.IO
import Data.Char

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
        children' = Map.insert x child children
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
        (Right line) -> loadDictionaryIter (insert (map toUpper line) dict) h
        (Left eofErrorType) -> return dict

loadDictionary filename = withFile filename ReadMode $ loadDictionaryIter Empty

nextChild :: String -> Dictionary -> Maybe Dictionary
nextChild "" dict = Just dict
nextChild (x:xs) n@(Node _ children) = do
    child <- Map.lookup x children
    nextChild xs child
nextChild _ _ = Nothing

solveIter :: Dictionary -> Board -> (Int,Int) -> String -> 
    Set.Set (Int, Int) -> [String]
solveIter Empty _ _ _ _ = []
solveIter Leaf _ _ word _ = return word
solveIter dict@(Node isWord children) board point@(row,col) word visited = 
    let visited' = Set.insert point visited
        nextDict = nextChild (board!point) dict
        value = board!point
        nextWord = word ++ value
        validPoint p@(r,c) =
            r >= 0 && r < 4 && c >= 0 && c < 4 && not (Set.member p visited')
    in case nextChild value dict of
        Nothing -> if isWord
            then [word]
            else []
        Just nextDict -> do
            rowD <- [-1,0,1]
            colD <- [-1,0,1]
            let nextPoint = (row+rowD,col+colD)
                next = solveIter nextDict board nextPoint nextWord visited'
            if validPoint nextPoint
                then if isWord
                    then word:next
                    else next
                else if isWord
                    then [word]
                    else []

solve :: Dictionary -> Board -> Set.Set String
solve dict board = Set.fromList $ do
    i <- [0..15]
    solveIter dict board (divMod i 4) "" Set.empty

