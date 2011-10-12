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
import IRC.Bot
import IRC.Bots.Towelie

server = "192.168.1.20"
port = 6667
nick = "AmIBotOrNot"
user = towelie
passwd = Nothing

main = do
    connection <- connectIRC server port nick user passwd
    channels <- joinChannels connection [("#bots",[])]
    handler connection towelie channels

