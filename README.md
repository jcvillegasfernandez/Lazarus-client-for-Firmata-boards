# Lazarus client for Firmata boards
Components working with Configurable Firmata boards on Lazarus (free Pascal)

I am not a master in programming but with hard work I have built some pieces to work with Lazarus (I think it will be easy to port to Delphi).

After installing FirmataBoard.lpk package you need to know the basic lines to work with them.
There are simple docs, Spanish and English (I hope you understand them).
The core component is TBoard with some modules to link TPin, TI2C, TTask, TOneWire, TAccelStepper, TServo, TPS2Mouse, TNeoPixel,  etc.

PS2Mouse and NeoPixel are not ConfigurableFirmata standard modules.

Take a look to the examples. I made all my tests on Arduino Uno boards.

At this moment it is a beta version 0.6, I made a few examples, if there is not an example for a module it means not tested.
