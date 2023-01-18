A series of packaged modules for Lazarus (free Pascal) on devices with Firmata, ConfigurableFirmata, etc (probably easy to port to Delphi).

First of all, is to install the FirmataBoard.lpk package, it is in the FirmataBoard directory. Once installed, it will create a tab in the component palette called 'Firmata'.
At the moment the version is 2.6 firmware 2.12, but it could work with another, especially with other ConfigurableFirmata especially with 2.6 protocol.

The main component is TBoard and it is independent of the data source. Different TPin, TTasks (Scheduler), TOneWire, TI2C (currently only one), TAccelStepper, TAccelStepperGroup, TSerial, TServo, TEncoder, TDht, TFrequency modules can be linked to TBoard, if there are enough free pins.
There are also two more modules TPS2Mouse and TNeoPixel, they are not standard ConfigurableFirmata modules so you have to make small changes in the ConfigurableFirmata code to make them work.

More important is to modify the code of ConfiguableFirmata so that Scheduler and Onewire work correctly on an ESP8266, ESP32 and other types of devices, since as they are they only work with Arduino.

Some usage examples are shown. The examples of use by serial port were done on Arduino Uno, ESP8266 and ESP32. An example of use through TCP is also attached. This test was carried out with ESP32.

At this moment it is a beta version of the 2.6 protocol, the modules that do not have an example are that they have not been tested but should work.