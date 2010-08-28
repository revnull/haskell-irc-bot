-- Advanced Towel Simulator
--
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

module IRC.Bots.Towelie (towelie) where

import IRC
import IRC.Bot
import Text.Regex.PCRE.Light.Char8

towelRE = compile "towel(?!ie)" [caseless]
waterRE = compile "water|wet|pool|lake|swim|swam" [caseless]
weedRE = compile "weed|pot|marijuana|joint|blunt|bong|high" [caseless]

towelie = multiBot [
    (privMsgTextMatch towelRE [], 
        simpleResponse $ privMsg "You're a towel!"),
    (privMsgTextMatch waterRE [], 
        simpleResponse $ privMsg "Don't forget to bring a towel!"),
    (privMsgTextMatch weedRE [], 
        simpleResponse $ privMsg "You wanna get high?")
    ]

