# Lazarus client for Firmata boards
Components for working with Firmata boards on Lazarus

I am not a master in programming but with hard work I have built some pieces to work with Lazarus (I think it will be easy to port to Delphi).

After installing FirmataBoard.lpk package you need to know the basic lines to work with them.
The core component is TBoard with some modules to link TPin, TI2C, TTask, TOneWire, TAccelStepper, etc.
Because of the heterogeneous ways of communication, I chose a way to get and send data. When TBoard wants to know if there is any data available calls FOnDeviceDataAvailable event, calls FOnGetDataFromDevice for getting a data byte or calls FOnSendDataToDevice to send data.

TBoard uses TThreads (like LazSerial does) but be careful because TThread only runs when application is in idle state that means out of an event.

When you enable a TBoard instance it triggers a TOnBoardReady, that means TBoard is enabled, knows all board capabilities and it is ready to get and send data.
TBoard has two events TOnBeforeOpen is triggered before Enabled TBoard and TOnAfterClose triggered after disable TBoard.

At this moment it is a beta version 0.1, I made a few examples, I haven’t tested some modules yet because I don’t have devices to try (I ordered some but still waiting for them), if there is not an example for a module it means not tested.
