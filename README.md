# Lazarus client for Firmata boards
Components working with Firmata boards on Lazarus (free Pascal)

I am not a master in programming but with hard work I have built some pieces to work with Lazarus (I think it will be easy to port to Delphi).

After installing FirmataBoard.lpk package you need to know the basic lines to work with them.
The core component is TBoard with some modules to link TPin, TI2C, TTask, TOneWire, TAccelStepper, TPS2Mouse,  etc.

When you enable a TBoard instance it triggers a TOnBoardReady, that means TBoard is enabled, the board knows all board capabilities and it is ready to get and send data.

Take a look to the examples.

At this moment it is a beta version 0.5, I made a few examples, if there is not an example for a module it means not tested.
