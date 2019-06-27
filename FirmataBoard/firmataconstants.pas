unit FirmataConstants;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  {TPinValue = 0..1;

  TPortPins = bitpacked record
    Pin0, Pin1, Pin2, Pin3, Pin4, Pin5, Pin6, Pin7: TPinValue;
  end;

  TPortValue = packed record
     case Integer of
       0: (Byte: Byte);
       1: (Pins: TPortPins);
     end; }

  TErrorRec = record
    Number: integer;
    Text: string;
  end;

const
// if error > 1000 + ErrorArray index then irrecuperable error close application
ErrorsArray: array [1..53] of String = (
  'No way to send data, check OnSendDataToDevice event',       // 1
  'Firmata board not enabled',                                 // 2
  'Too much time waiting for firmata, close firmata',          // 3
  'Unknown SysEx command, ',                                   // 4
  'Task ID number too big',                                    // 5
  'Length of task too big',                                    // 6
  'Pin is not supported',                                      // 7
  'Port is not supported',                                     // 8
  'Analog pin is not supported',                               // 9
  'Analog pin is not in analog mode',                          // 10
  'Mode is not supported',                                     // 11
  'Pin belongs to a module',                                   // 12
  'Pin is not in right mode',                                  // 13
  'Is not a valid Port',                                       // 14
  'Got bad serial message',                                    // 15
  'Device number too big',                                     // 16
  'Wrong value setup interface',                               // 17
  'Group number too big',                                      // 18
  'Member device too big',                                     // 19
  'Number is out of range',                                    // 20
  'Data too long, max for sysex is 64',                        // 21
  'Task does not exists',                                      // 22
  'There is not any task',                                     // 23
  'Data too long, max for sysex is 64',                        // 24
  'Bad length of task',                                        // 25
  'No way to check available data, check OnDeviceDataAvailable', // 26
  'Too much time waiting for data,  device error',             // 27
  'No way to get external data, check OnGetDataFromDevice',    // 28
  'Many bytes to read',                                        // 29
  'Data message too long',                                     // 30
  'Too much waiting for data, close firmata',                  // 31
  'Module is not enabled',                                     // 32
  'Module is enabled, disable it first',                       // 33
  'No resolution found for mode',                              // 34
  'No soported firmata version',                               // 35
  'Pulses per step',                                           // 36
  'Motor pin is busy',                                         // 37
  'Too many member in group',                                  // 38
  'Member already exists',                                     // 39
  'Can not delete member does not exist',                      // 40
  'There are not any members in group',                        // 41
  'Number out of range',                                       // 42
  'AccelStepper module already exists',                        // 43
  'No more room for',                                          // 44
  'There is already a module',                                 // 45
  'No valid device',                                           // 46
  'Sys Ex command error',                                      // 47
  'Not valid type',                                            // 48
  'Min number of pixels must be > 1',                          // 49
  'Pixel number out of range',                                 // 50
  'Wrong led color support',                                   // 51
  'Only positive values for gamma',                            // 52
  'Fade is running'                                            // 53
  );

   FIRMWARE_MAJOR_VERSION =  2;
   FIRMWARE_MINOR_VERSION =  5;

   {
    Version numbers for the protocol.  The protocol is still changing, so these
    version numbers are important.
    Query using the REPORT_VERSION message.
   }
   PROTOCOL_MAJOR_VERSION =  2; // for non-compatible changes
   PROTOCOL_MINOR_VERSION =  5; // for backwards compatible changes

   MAX_DATA_BYTES =          64; // max number of data bytes in incoming messages

  // message command bytes (128-255/$80-$FF)
   FIRMATA_ERROR =           $50; // error using firmata
   DIGITAL_MESSAGE =         $90; // send data for a digital port (collection of 8 pins)
   ANALOG_MESSAGE =          $E0; // send data for an analog pin (or PWM)
   REPORT_ANALOG =           $C0; // enable analog input by pin #
   REPORT_DIGITAL =          $D0; // enable digital input by port pair

   SET_PIN_MODE =            $F4; // set a pin to INPUT/OUTPUT/PWM/etc
   SET_DIGITAL_PIN_VALUE =   $F5; // set value of an individual digital pin

   REPORT_VERSION =          $F9; // report protocol version
   SYSTEM_RESET =            $FF; // reset from MIDI

   START_SYSEX =             $F0; // start a MIDI Sysex message
   END_SYSEX =               $F7; // end a MIDI Sysex message
   EXTENDED_SYSEX =          $00; // extended SysEx commands, netx to bytes are extended command
   // extended command set using sysex (0-127/$00-$7F)
  { $00-$0F reserv for user-defined commands }
   PS2MOUSE_DATA =           $50; // user defined command PS2 mouse
   NEOPIXELS_DATA =          $51; // user defined command Neopixels
   SERIAL_DATA =             $60; // SERIAL DATA
   ENCODER_DATA =            $61; // reply with encoders current positions
   ACCELSTEPPER_DATA =       $62; // Accelerated stepper data
   SERVO_CONFIG =            $70; // set max angle, minPulse, maxPulse, freq
   STRING_DATA =             $71; // a string message with 14-bits per char
   STEPPER_DATA =            $72; // control a stepper motor, old system
   ONEWIRE_DATA =            $73; // send an OneWire read/write/reset/select/skip/search request
   SHIFT_DATA =              $75; // a bitstream to/from a shift register
   I2C_REQUEST =             $76; // send an I2C read/write request
   I2C_REPLY =               $77; // a reply to an I2C read request
   I2C_CONFIG =              $78; // config I2C settings such as delay times and power pins
   REPORT_FIRMWARE =         $79; // report name and version of the firmware
   EXTENDED_ANALOG =         $6F; // analog write (PWM, devvo, etc) to any pin
   PIN_STATE_QUERY =         $6D; // ask for a pin's current mode and value
   PIN_STATE_RESPONSE =      $6E; // reply with pin's current mode and value
   CAPABILITY_QUERY =        $6B; // ask for supported modes and resolution of all pins
   CAPABILITY_RESPONSE =     $6C; // reply with supported modes and resolution
   ANALOG_MAPPING_QUERY =    $69; // ask for mapping of analog to pin numbers
   ANALOG_MAPPING_RESPONSE = $6A; // reply with mapping info
   SAMPLING_INTERVAL =       $7A; // set the poll rate of the main loop
   SCHEDULER_DATA =          $7B; // send a createtask/deletetask/addtotask/schedule/querytasks/querytask request to the scheduler
   SYSEX_NON_REALTIME =      $7E; // MIDI Redevved for non-realtime messages
   SYSEX_REALTIME =          $7F; // MIDI Redevved for realtime messages

  // pin modes
   PIN_MODE_INPUT =          $00; // same as INPUT defined in Arduino.h
   PIN_MODE_OUTPUT =         $01; // same as OUTPUT defined in Arduino.h
   PIN_MODE_ANALOG =         $02; // analog pin in analogInput mode
   PIN_MODE_PWM =            $03; // digital pin in PWM output mode
   PIN_MODE_SERVO =          $04; // digital pin in SERVO output mode
   PIN_MODE_SHIFT =          $05; // shiftIn/shiftOut mode
   PIN_MODE_I2C =            $06; // pin included in I2C setup
   PIN_MODE_ONEWIRE =        $07; // pin configured for 1-wire
   PIN_MODE_STEPPER =        $08; // pin configured for stepper motor
   PIN_MODE_ENCODER =        $09; // pin configured for rotary encoders
   PIN_MODE_SERIAL =         $0A; // pin configured for serial communication
   PIN_MODE_PULLUP =         $0B; // enable internal pull-up resistor for pin
   PIN_MODE_PS2MOUSE =       $0C; // mouse mode pin
   PIN_MODE_NEOPIXELS =      $0D; // mode for neopixels leds
   PIN_MODE_IGNORE =         $7F; // pin configured to be ignored by digitalWrite and capabilityResponse

   HIGH = 1;
   LOW = 0;

   // serial constants: communicate with sevial devices, including other boards
   MAX_SERIAL_PORTS =          8;
   HW_SERIAL0 =              $00; //(for using Serial when another transport is used for the Firmata Stream)
   HW_SERIAL1 =              $01;
   HW_SERIAL2 =              $02;
   HW_SERIAL3 =              $03;

   SW_SERIAL0 =              $08;
   SW_SERIAL1 =              $09;
   SW_SERIAL2 =              $0A;
   SW_SERIAL3 =              $0B;

   SERIAL_CONFIG =           $10;
   SERIAL_WRITE =            $20;
   SERIAL_READ =             $30;
   SERIAL_REPLY =            $40;
   SERIAL_CLOSE =            $50;
   SERIAL_FLUSH =            $60;
   SERIAL_LISTEN =           $70;

   // Where hardware serial the pin mode = "Serial" and the pin resolution = one of the following:
   RES_RX0 =                 $00;
   RES_TX0 =                 $01;
   RES_RX1 =                 $02;
   RES_TX1 =                 $03;
   RES_RX2 =                 $04;
   RES_TX2 =                 $05;
   RES_RX3 =                 $06;
   RES_TX3 =                 $07;

   //read modes
   SERIAL_READ_MODE_CONT =   $00;
   SERIAL_READ_MODE_STOP =   $01;

   // ConfigurableFirmata
   I2C_WRITE                = $00;
   I2C_READ                 = $08;
   I2C_READ_CONTINUOUSLY    = $10;
   I2C_STOP_READING         = $18;
   I2C_READ_WRITE_MODE_MASK = $18;
   I2C_10BIT_ADDRESS_MODE_MASK = $20;
   I2C_AUTORESTART_RESTART     = $40;

   //onewire ConfigurabelFirmata:
   ONEWIRE_SEARCH_REQUEST = $40;
   ONEWIRE_CONFIG_REQUEST = $41;
   ONEWIRE_SEARCH_REPLY = $42;
   ONEWIRE_READ_REPLY = $43;
   ONEWIRE_SEARCH_ALARMS_REQUEST = $44;
   ONEWIRE_SEARCH_ALARMS_REPLY = $45;

   ONEWIRE_RESET_REQUEST_BIT = $01;
   ONEWIRE_SKIP_REQUEST_BIT = $02;
   ONEWIRE_SELECT_REQUEST_BIT = $04;
   ONEWIRE_READ_REQUEST_BIT = $08;
   ONEWIRE_DELAY_REQUEST_BIT = $10;
   ONEWIRE_WRITE_REQUEST_BIT = $20;
   ONEWIRE_WITHDATA_REQUEST_BITS = $3C;

   // Scheduler
   CREATE_TASK =             $00; // Create_task command  (0x00)
   DELETE_TASK =             $01; // delete_task command  (0x01)
   ADD_TO_TASK =             $02; // add_to_task command  (0x02)
   DELAY_TASK =              $03; // delay_task command   (0x03)
   SCHEDULE_TASK =           $04; // schedule_task command    (0x04)
   QUERY_ALL_TASKS =         $05; // query_all_tasks command  (0x05)
   QUERY_TASK =              $06; // query_task command       (0x06)
   SCHEDULER_RESET =         $07; // scheduler reset command  (0x07)
   ERROR_FIRMATA_TASK =      $08; // error_task Reply Command (0x08)
   QUERY_ALL_TASKS_REPLY =   $09; // query_all_tasks Reply Command (0x09)
   QUERY_TASK_REPLY =        $0A; // query_task Reply Commandc (0x0A)

   // Accelerated stepper
   ACCELSTEPPER_CONFIG =     $00; // config stepper
   ACCELSTEPPER_ZERO =       $01; // set zero stepper position
   ACCELSTEPPER_STEP =       $02;  // relative steps to move
   ACCELSTEPPER_TO =         $03;  // absolute steps to move from zero
   ACCELSTEPPER_ENABLE =     $04;  // enable steppers with enable pin
   ACCELSTEPPER_STOP =       $05;  // stepper move completed
   ACCELSTEPPER_REPORT_POSITION = $06;  // stepper report position
   ACCELSTEPPER_STOP_LIMIT =      $07;  // stop limit command not yet implemented
   ACCELSTEPPER_SET_ACCELERATION = $08;  // set acceleration command
   ACCELSTEPPER_SET_SPEED        = $09;  // set speed command
   ACCELSTEPPER_MOVE_COMPLETED =   $0A;  // move completed report
   ACCELSTEPPER_MULTI_CONFIG =     $20;  // multiConfig command
   ACCELSTEPPER_MULTI_TO =         $21;  // multi to command
   ACCELSTEPPER_MULTI_STOP =       $23;  // multi stop
   ACCELSTEPPER_MULTI_MOVE_COMPLETED =   $24;  // multi move completed report

   MAX_ACCELSTEPPER_DEVICES =      10;   // max 10 motor devices
   MAX_ACCELSTEPPER_MULTI =         5;   // max 5 groups of 10 devices
   MAX_ACCELERATION =              4000;
   MAX_SPEED =                     4000;



   // Accelstepper interface   (upper 3 bits = wire count:
   ACCEL_INTERFACE_DRIVER =   $10; //  001XXXX = driver
   ACCEL_INTERFACE_2_WIRE =   $20; //  010XXXX = two wire
   ACCEL_INTERFACE_3_WIRE =   $30; //  011XXXX = three wire
   ACCEL_INTERFACE_4_WIRE =   $40; //  100XXXX = four wire)
   // Accelstepper step type   (4th - 6th bits, step size = 1/2^0bXXX
   WHOLE_STEP =               $00; //  Examples:  XXX000X = whole step
   HALF_STEP =                $01; // XXX001X = half step
   //                                 XXX010X = quarter step
   //                                           etc...)
   //
   // Accelstepper has enable pin   (lower 1 bit = has enable pin:
   //                                 XXXXXX0 = no enable pin
   //                                 XXXXXX1 = has enable pin)
   //
   MAX_SERVOS =               12; // max servos devices
   //
   MAX_ENCODERS =             5; // max encoders devices
   ENCODER_ATTACH =           $00;  // config
   ENCODER_REPORT_POSITION =  $01;  // query report position
   ENCODER_REPORT_POSITIONS = $02;  // query report position for all encoders
   ENCODER_RESET_POSITION =   $03;  // Reset encoder position
   ENCODER_REPORT_AUTO =      $04;  // Enable/disable reporting
   ENCODER_DETACH =           $05;  // detach encoder
   DIRECTION_MASK =           $40;  // B01000000
   ENCODER_MASK =             $3F;  // B00111111, ENCODER
   // PS2 mouse
   MAX_MICE =                  $03;
   PS2MOUSE_RESET      =       $00;
   PS2MOUSE_STATUS     =       $01;
   PS2MOUSE_CONFIG     =       $02;
   PS2MOUSE_DEVICEID   =       $03;
   PS2MOUSE_SET_RESOLUTION  =  $04;
   PS2MOUSE_SET_SAMPLE_RATE =  $05;
   PS2MOUSE_REPORTING  =       $06;
   PS2MOUSE_SET_FIVE_BUTTONS = $07;
   PS2MOUSE_SET_STREAM_MODE =  $08;
   PS2MOUSE_SET_REMOTE_MODE =  $09;
   PS2MOUSE_REMOTE     =       $01;
   PS2MOUSE_STREAM     =       $00;
   PS2MOUSE_1_COUNT_MM =       0;
   PS2MOUSE_2_COUNT_MM =       1;
   PS2MOUSE_4_COUNT_MM =       2;
   PS2MOUSE_8_COUNT_MM =       3;
   PS2MOUSE_SCALING_1_TO_1  =  $E6;
   PS2MOUSE_SCALING_2_TO_1  =  $E7;
   // NeoPixel NEOPIXELS_DATA = $51          hybridgroup/FirmataNeopixels
   MAX_NEOPIXELS        =	4;

   NEOPIXELS_OFF        =      $00; // set strip to be off
   NEOPIXELS_CONFIG     =      $01; // configure the strip
   NEOPIXELS_SHOW       =      $02; // latch the pixels and show them
   NEOPIXELS_SET_PIXEL  =      $03; // set the color value of pixel n
   NEOPIXELS_FADE_RUN_PAUSE =  $04; // run and pause fade
   NEOPIXELS_SET_BRIGHTNESS =  $05; // set the brightness of pixel n using 8bit value
   NEOPIXELS_SHIFT_CONFIG   =  $06; // shift pixels places along the strip
   NEOPIXELS_FILL_SEGMENT   =  $07; // Fills all or a given start+length of strip.
   NEOPIXELS_FADE_CONFIG    =  $08; // progressive pixels color change
   NEOPIXELS_SHIFT_RUN      =  $09; // run shift step
   NEOPIXELS_FADE_ONE_STEP  =  $0A; // progressive pixels color change, 1 step
   NEOPIXELS_COPY_PIXELS    =  $0B; // move pixels from src to dest



implementation

end.

