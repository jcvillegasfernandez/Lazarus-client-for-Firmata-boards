# Lazarus client for Firmata boards
Components working with Configurable Firmata boards on Lazarus (free Pascal)

I have built some pieces to work with Lazarus (I think it will be easy to port to Delphi).

To install the components, install FirmataBoard.lpk package.
There are simple docs, Spanish and English (I hope you understand them) in order to look how they work.
The core component is TBoard where you can link different modules TPin, TI2C, TTask, TOneWire, TAccelStepper, TServo, TPS2Mouse, TNeoPixel, etc. You can manage some boards (with their own modules) at the same time.

PS2Mouse and NeoPixel are not ConfigurableFirmata standard modules. Some chenges in ConfigurableFirmata code to make it work on an esp8266.

Take a look to the examples. I made all my tests on Arduino Uno board and NodeMCU ver 1.0 (ESP-12E module).

At this moment it is a beta version 0.10, I made a few examples, if there is not an example for a module it means not tested.
