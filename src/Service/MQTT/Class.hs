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

instance MQTTClient (MQTT.MQTTClient) where
  publishMQTT mc topic msg = MQTT.publish mc topic msg False
  subscribeMQTT mc topic =
    -- handle response properly!
    void $ MQTT.subscribe mc [(toFilter topic, MQTT.subOptions)] []
