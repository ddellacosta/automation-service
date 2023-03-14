# Using automation-service

There are two main components in using automation-service, which are automations (usually written in Lua, but also possible with Haskell), and messages. Messages are MQTT JSON-formatted messages, and are used to communicate to the automation-service daemon in order to start, stop, and schedule automations. Both are covered below.


## Writing Automations

### Lua

Writing automations in Lua is the easiest and recommended way to create automations.


#### automation-service Lua API

TODO define including parameters and return values, if any...can I get this out of DocumentedFunction somehow?

* **register**

* **publish**

* **subscribe**

* **sendMessage**

* **logDebugMsg**

* **microSleep**

* **sleep**


#### Program Structure

* **setup**

Use this for initializing or loading state into global variables. If your script is intended as a one-off automation rather than something that runs indefinitely, you can keep all your program logic in this function and skip writing a `loop` function.

* **loop**

Implement this function if you intend for your automation to run endlessly, checking a piece of state and acting on it repeatedly, or publishing messages at regular intervals. You can even implement your own scheduling algorithm and trigger messages internally using the `sendMessage` API function. Implementing this function will cause it to iterate over and over endlessly, and resource usage is entirely in the hands of the author, so be aware of what this is going to generate, use, set, etc., and see the note also about blocking below.

* **cleanup**

Any state cleanup, device resetting, or other final resource management should be added here.


Note that using Lua scripts comes with the condition that you don't use any construct that blocks endlessly _in Lua_. An example of this is `while (true)` to loop endlessly--this prevents automation-service from shutting the script down. Use the built in `loop` for repeating a process indefinitely. 


TODO provide example scripts with explanations, more
