
import IRC
import IRC.Bot
import IRC.Bots.Towelie

server = "192.168.1.20"
port = 6667
nick = "AmIBotOrNot"
passwd = Nothing

main = do
    connection <- connectIRC server port nick passwd
    channels <- joinChannels connection [("#bots",[])]
    handler connection towelie channels

