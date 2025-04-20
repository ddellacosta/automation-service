module Service.MQTT.Class
 ( MQTTClient (..)
 )
where

import Control.Monad (void)
import Data.ByteString.Lazy (ByteString)
import qualified Network.MQTT.Client as MQTT
import Network.MQTT.Topic (Topic, toFilter)

class MQTTClient a where
  publishMQTT :: a -> Topic -> ByteString -> IO ()
  subscribeMQTT :: a -> Topic -> IO ()
  unsubscribeMQTT :: a -> Topic -> IO ()

instance MQTTClient (MQTT.MQTTClient) where
  publishMQTT mc topic msg = MQTT.publish mc topic msg False
  subscribeMQTT mc topic =
    -- handle response properly!
    void $ MQTT.subscribe mc [(toFilter topic, MQTT.subOptions)] []
  unsubscribeMQTT mc topic =
    -- ditto
    -- https://hackage.haskell.org/package/net-mqtt-0.8.6.2/docs/Network-MQTT-Client.html#g:3
    -- In MQTT 3.1.1, there is no body to an unsubscribe response, so it can be ignored. If this returns, you were unsubscribed. In MQTT 5, you'll get a list of unsub status values corresponding to your request filters, and whatever properties the server thought you should know about.
    void $ MQTT.unsubscribe mc [toFilter topic] []
