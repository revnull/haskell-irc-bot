-- Advanced Towel Simulator
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

