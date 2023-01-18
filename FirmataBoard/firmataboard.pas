{==============================================================================|
| Project : ConfigurableFirmata Board Client                                   |
|==============================================================================|
| Content: Board and modules components                                        |
|==============================================================================|
| Copyright (c)2018-2019, Juan carlos Villegas Fernández                       |
| All rights reserved.                                                         |
|                                                                              |
| Redistribution and use in source and binary forms, with or without           |
| modification, are permitted provided that the following conditions are met:  |
|                                                                              |
| Redistributions of source code must retain the above copyright notice, this  |
| list of conditions and the following disclaimer.                             |
|                                                                              |
| Redistributions in binary form must reproduce the above copyright notice,    |
| this list of conditions and the following disclaimer in the documentation    |
| and/or other materials provided with the distribution.                       |
|                                                                              |
| Neither the name of Juan carlos Villegas Fernández nor the names of its      |
| contributors may be used to endorse or promote products derived from this    |
| software without specific prior written permission.                          |
|                                                                              |
| THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  |
| AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    |
| IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   |
| ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR  |
| ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL       |
| DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR   |
| SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER   |
| CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT           |
| LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    |
| OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  |
| DAMAGE.                                                                      |
|==============================================================================|
| The Initial Developer of the Original Code is Juan Carlos Villegas Fernández.|
| (Palencia)-Spain                                                             |
|                                                                              |
|==============================================================================|
| Contributor(s):                                                              |
|  (c)                                                                         |
|==============================================================================}

unit firmataboard;
//{$mode objfpc}{$H+}
{$mode delphi}{$H+}

interface

uses
{$IFDEF LINUX}
{$IFDEF UseCThreads}
  cthreads,
{$ENDIF}
{$ENDIF}
  Classes, LclType, ExtCtrls, sysUtils, forms, firmataconstants,
  StdCtrls, dialogs,  LResources, LclIntf, math, graphics, typinfo;

 type
  // Digital Pin register type

  {:variable type to store capability and current mode of pin}
  TCapability = record
    Mode: Byte;         // pin mode
    Resolution: Byte;   // Bits of resolution
  end;

  {:Possible pin modes}
  TPinModes = (PIN_MODE_INPUT, PIN_MODE_OUTPUT, PIN_MODE_ANALOG, PIN_MODE_PWM, PIN_MODE_SERVO,
           PIN_MODE_SHIFT, PIN_MODE_I2C, PIN_MODE_ONEWIRE, PIN_MODE_STEPPER, PIN_MODE_ENCODER,
           PIN_MODE_SERIAL, PIN_MODE_PULLUP, PIN_MODE_SPI, PIN_MODE_SONAR, PIN_MODE_TONE,
           PIN_MODE_DHT, PIN_MODE_FREQUENCY, PIN_MODE_PS2MOUSE, PIN_MODE_NEOPIXEL,
           PIN_MODE_IGNORE);

  TBitOrder = (LSB_FIRST = 0, MSB_FIRST = 1);
  TDHTSensorType = (DHT_11, DHT_12, DHT_21, DHT_22, AM2301);

 type

  {:Possible serial port values}
  TSerialPorts = (HW_SERIAL0, HW_SERIAL1, HW_SERIAL2, HW_SERIAL3,
                  SW_SERIAL0=$08, SW_SERIAL1, SW_SERIAL2, SW_SERIAL3);
  TSerialPinType = (RX, TX);

  {:Array of pin capabilities}
  TCapabilities = array of TCapability;

  {:Board pin variables}
  TBoardPin = record
       ActualMode: Byte;
       Busy: Boolean;      // if busy then is assigned to a module
       AnalogMap: Byte;    // analog pin mapping
       Capabilities: TCapabilities; // supported modes and resolutions
  end;

  {:Array of board pins}
  TBoardPins = Array of TBoardPin;   // array of pins


  {:Mouse postition record}
  TPosition = record
    x: Byte;
    y: Byte;
  end;

  {:Mouse data record}
  TPS2MouseData = record
    StatusMove: Byte;
    Position: TPosition;
    Wheel: Byte; // is z move for Intelli mouse or TWheelBits for five-buttons mouse
  end;

  {: Mouse status record}
  TPS2MouseStatus = record
    StatusMouse: Byte;
    Resolution: Byte;
    SampleRate: Byte;
  end;

  {:Possible mouse types}
  TMouseType = (STANDARD, INTELLI_MOUSE=3, FIVEBUTTONS, STILL_UNKNOWN);

  TFadeType= (STRIP, PIXEL);

  {:Event thread timer}
  TOnTimer = procedure(sender: TObject) of Object;
  {:Event run just before enable the object}
  TOnBeforeOpen = procedure(sender: TObject) of Object;
  {:Event run just after close disable the object}
  TOnAfterClose = procedure(sender: TObject) of Object;
  {:Procedure to send data to the board}
  TOnSendDataToDevice = procedure(sender: TObject; str: string) of Object;
  {:Event run when an error occur}
  TOnError = procedure(sender: TObject; Error: integer; TextError: string) of Object;
  {:Event run when board is initialized and got their cpabilities}
  TOnBoardReady = procedure(sender: TObject) of Object;
  {:Event run when the board send a message data, etc not command data}
  TOnBoardData = procedure(sender: TObject; Command: Byte; Data: string) of object;
  {:Event run when received pin value data from board}
  TOnPinValue = procedure(sender: TObject; Value: integer) of object;
  {:Event run when received pin value state from board, pin mode and last written value}
  TOnPinState = procedure(sender: TObject; Mode: TPinModes; State: integer) of object;
  {:Procedure to get an extended sysex command}
  TOnExtendedSysex = procedure(sender: TObject; Data: String) of Object;
  {:Event run when a module is enabled}
  TOnEnabled = procedure(sender: TObject) of Object;
  {:Event run when a module is disabled}
  TOnDisabled = procedure(sender: TObject) of Object;
  {:Event run in a task error}
  TOnTaskError = procedure(sender: TObject; Time: integer; Length: integer; Place: integer; TaskData: String) of object;
  {:Receiving IDs from all board task}
  TOnQueryAllTask = procedure(sender: TObject; TaskIDs: array of byte) of object;
  {:Receiving detailed information from a task}
  TOnQueryTask = procedure(sender: TObject; Time: integer; Length: integer; Place: integer; TaskData: String) of object;
  {:Information about all One Wire devices on the bus}
  TOnSearch = procedure(sender: TObject; DeviceIDs: array of string) of Object;
  {:Information about all One Wire devices in alarm state}
  TOnOneWireAlarm = procedure(sender: TObject; AlarmIDs: array of string) of Object;
  {:Data event from One Wire device}
  TOnOneWireData = procedure(sender: TObject; Data: string) of Object;
  {:Data event from I2C device}
  TOnI2CData = procedure(sender: TObject; Slave: Word; Reg_Number: Byte; Sequence: Byte; Data: string) of Object;
  {:Data event from SPI device}
  TOnSPIData = procedure(sender: TObject; DeviceID: Byte; Channel: byte; RequestID: byte; Data: string; NumBytes: integer) of Object;
  {:Event reporting information about a stepper position or move completed}
  TOnStepperPosition = procedure(sender: TObject; Device: byte; Position: integer) of Object;
  {:Event reporting information about a stepper group move completed}
  TOnAccelStepperMultiMoveCompleted = procedure(sender: TObject; Group: byte) of Object;
  {:Event run when serial data is available}
  TOnSerialMessage = procedure(sender: TObject; Data: string) of Object;
  {:Event reporting information about a encode position}
  TOnEncoderPosition = procedure(sender: TObject; Direction: integer; Position: integer) of Object;
  {:Event reporting information about a Frequency data}
  TOnFrequencyData = procedure(sender: TObject; Pin: Byte; Time: integer; Ticks: integer) of Object;
  {:Event reporting Temperature and Humidity}
  TOnDHTData = procedure(sender: TObject; Pin: Byte; Temperature: single; Humidity: single) of Object;
  {:Event reporting position and buttons data of a mouse}
  TOnMouseData = procedure(sender: TObject; MouseData: TPS2MouseData) of Object;
  {:Event reporting data status of a mouse}
  TOnMouseStatus = procedure(sender: TObject; MouseStatus: TPS2MouseStatus) of Object;
  {:Information about a mouse type}
  TOnMouseDeviceID = procedure(sender: TObject; MouseType: TMouseType) of Object;
  {:Event reporting fade end}
  TOnFadeEnd = procedure(sender: TObject; Device: Byte) of Object;


  TBoard = class;

  {:Thread to get data from a board}
  TBoardThread = class(TThread)
  private
    FOwner: TBoard;
    FInitTime: QWord;
    FTime: QWord;
    FMaxTime: QWord;
    FByteTimeOut: QWord;
    FStarting: Boolean;
    FFirstTry: Boolean;
    FInterval: Cardinal;
    FCommandData: string;
    FCommandBuffer: string;
    FOnTimer: TOnTimer;
    FEnabled: Boolean;
    FEndLastCommand: Boolean;
    procedure DoOnTimer;
    procedure askProtocol;
    procedure TimeOut;
    procedure TimeOutByte;
    procedure UpdateVars;
    procedure ParseSysExCommand;
    procedure ParseCommand;
    procedure GetCommand;

    protected
      procedure Execute; override;
    public
      constructor Create(CreateSuspended: Boolean);
      destructor Destroy; override;
      property OnTimer: TOnTimer read FOnTimer write FOnTimer;
      property MaxTime: QWord read FMaxTime write FMaxTime;
      property Interval: Cardinal read FInterval write FInterval;
      procedure StopTimer;
      procedure StartTimer;
      Property Owner: TBoard read FOwner write FOwner;

  end;


  TPin = class;
  TTask = class;
  TOneWire = class;
  TI2C = class;
  TAccelStepper = class;
  TAccelStepperGroup = class;
  TSerial = class;
  TServo = class;
  TEncoder = class;
  TSPI = Class;
  TFrequency = Class;
  TDHT = Class;
  TPS2Mouse = class;
  TNeoPixel = class;


  {:Array of pin modules, max number is board pins}
  TPins = array of TPin;
  {:Array of task modules, max is 127}
  TTasks = array [0..127] of TTask;
  {:Array of AccelStepper modules}
  TAccelSteppers = array [0..MAX_ACCELSTEPPER_DEVICES - 1] of TAccelStepper;
  {:Array of OneWire modules}
  TOneWires = array of TOneWire;
  {:Array of SPIs modules}
  TSPIs = array [0..MAX_SPI_DEVICES - 1] of TSPI;
  {:Array of Serial modules}
  TSerials = array [TSerialPorts] of TSerial; // max 8 serial ports
  {:Array of Servos modules}
  TServos = array [0..MAX_SERVOS - 1] of TServo;
  {:Array of Encoders modules}
  TEncoders = array [0..MAX_ENCODERS - 1] of TEncoder;
  {:Array of Frequencies modules}
  TFrequencies = array [0..MAX_FREQUENCIES - 1] of TFrequency;
  TDHTs = array of TDHT;
  {:Array of Mouse modules}
  TMice = array [0..MAX_MICE - 1] of TPS2Mouse;
  {:Array of Neopixels modules}
  TNeoPixels = array [0..MAX_NEOPIXELS - 1] of TNeoPixel;

  TBoard = class (TComponent)
    private
      FBoardThread: TBoardThread;
      FCommandBuffer: string;
      FEnabled: Boolean;
      FStarting: Boolean;
      FGotCapabilities: Boolean;
      FGotFirmware: Boolean;
      FGotProtocol: Boolean;
      FSamplingInterval: integer;
      FOnBoardData: TOnBoardData; // only when there are firmata data and no other event
      FOnPinValue: TOnPinValue;
      FOnPinState:   TOnPinState;
      FOnBeforeOpen: TOnBeforeOpen;
      FOnAfterClose: TOnAfterClose;
      FOnError: TOnError;
      FOnTimer: TOnTimer;
      FTimerInterval: Integer;
      FOnSendDataToDevice: TOnSendDataToDevice;
      FOnExtendedSysex: TOnExtendedSysex;

    private
      {:array of board pins capbilities, values, mode, etc}
      FBoardPins: TBoardPins;
      {:Total board pins}
      FBoardPinsNumber: integer;
      {:Total board analog pins}
      FAnalogPinsNumber: integer;
      {: Type of board, standard firmata, configurable firmata ...}
      FBoardType: string;
      {:String showing board firmware version}
      FBoardStringProtocol: string;
      {:String showing board protocol version}
      FBoardStringFirmware: string;

      FOnBoardReady: TOnBoardReady;
      FOnQueryAllTask: TOnQueryAllTask;

      {:Max time to wait for board to start}
      FMaxTime: QWord;
      FInitTime: QWord;
      FByteTimeOut: QWord;
      {:Time taken to initialize the board}
      FStartingTime: QWord;
      // modules
      FPins: TPins;
      FTasks: TTasks;
      FOneWires: TOneWires;
      FI2C: TI2C;
      FSPIs: TSPIs;
      FAccelSteppers: TAccelSteppers;
      FAccelStepperGroup: TAccelStepperGroup;
      FSerials: TSerials;
      FServos: TServos;
      FEncoders: TEncoders;
      FFrequencies: TFrequencies;
      FDHTs: TDHTs;
      FMice: TMice;
      FNeoPixels: TNeoPixels;

      procedure setEnabled(State: Boolean);
      {:procedure for initialize board variables}
      procedure initBoardVariables;
      {:When board is ready initialize module variables}
      procedure InternalBoardReady;
      {:procedure to disable all modules of the board prior disable board itself}
      procedure DisableModules;

      procedure setMaxTime(TimeToWait: Qword);  // milisec to wait for firmata to be ready
      {:get board pin record, not pin module}
      function GetBoardPin(Index: Integer): TBoardPin;
      {:set board pin record, not pin module}
      procedure SetBoardPin(Index: integer; Pin: TBoardPin);
      {:set board sampling interval, min value is 19 milisecs}
      procedure SetSamplingInterval(Interval: integer);
    public
      {:Store last error number.}
      FLastError: integer;
      {:procedure error, need more work}
      procedure RaiseError(Error: integer; FunctionError: string);
      {:get command or sysex command from board}
      procedure parseCommand(Data: string);
      {:get command from data}
      procedure ParseData(Data: string);
      procedure parseSysExCommand(Data:string); // string includes END_SYSEX byte

      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      procedure CloseApp;

      // Firmata comands
      // All firmata functions for commands return the string command sent, they have a write parameter, if write is True the data is sent to the external device
      // if write is false then is not sent to device

      {:ask board for version}
      function askProtocol(write: Boolean=true): string;
      {:ask board for firmware}
      function askFirmware(write: Boolean=true): string;
      {:ask board capabilities}
      function askBoardCapabilities(write: Boolean=true): string;
      {:set sampling interval but return data command for tasks}
      function SendSamplingInterval(write: Boolean=true): string;
      {:perhaps this function should be on TTask module}
      function QueryAllTasks(write: Boolean=true): string;
      {:Reset all tasks, perhaps this function should be on TTask module}
      function SchedulerReset(write: Boolean=true): string;
      {:send command to board}
      function SendCommand(Data: string; write: Boolean=True): string;
      {:send sysex command to board}
      function SendSysEx(data: string; write: Boolean=true): string;

      {:reset board}
      procedure Reset;
      {:enable board}
      procedure Open;
      {:disable board}
      procedure Close;
      {:get board pin number from analog pin number}
      function GetPinFromAnalogPin(AnalogPin: byte): Byte;
      {:Check if pin supports this mode}
      function CheckCapability(Pin: byte; Mode: TPinModes): Boolean;
      {:Check if pin is reporting}
      function CheckReportPort(Pin: Byte): Boolean;
      {:get resolution for this pin mode}
      function GetPinResolution(Pin: Byte; Mode: TPinModes): Integer;
      {:fill a TStrings with pins capability}
      procedure printPinInfo(Info: TStrings);
      {: array of Board Pins}
      property BoardPins[Index:Integer]: TBoardPin read GetBoardPin write SetBoardPin;
      property Enabled: Boolean read FEnabled write SetEnabled;
      property BoardType: string read FBoardType;
      property LastError: integer read FLastError write FLastError;
      {: Firmware version of firmata board}
      property FirmataFirmware: string read FBoardStringFirmware;
      {: Firmware version of protocol}
      property FirmataProtocol: string read FBoardStringProtocol;
      {: number of pins in board}
      property PinsNumber: integer read FBoardPinsNumber;
      {: number of analog pins in board}
      property AnalogPinsNumber: integer read FAnalogPinsNumber;
      {: time spend starting firmata}
      property StartingTime: Qword read FStartingTime;
    published
      {: Sampling interval for board default 19 milisec}
      property SamplingInterval: integer read FSamplingInterval write SetSamplingInterval;
      Property TimerInterval: integer read FTimerInterval write FTimerInterval;
      {: max time to wait for starting board}
      property MaxTime: Qword read FMaxTime write setMaxTime;
      property OnBeforeOpen: TOnBeforeOpen read FOnBeforeOpen write FOnBeforeOpen;
      property OnAfterClose: TOnAfterClose read FOnAfterClose write FOnAfterClose;
      {: when firmata board became ready}
      property OnBoardReady: TOnBoardReady read FOnBoardReady write FOnBoardReady;
      property OnSendDataToDevice: TOnSendDataToDevice read FOnSendDataToDevice write FOnSendDataToDevice;
      property OnError: TOnError read FOnError write FOnError;
      property OnTimer: TOnTimer read FOnTimer write FOnTimer;
      {: when the board send data, but is not a command}
      property OnBoardData: TOnBoardData read FOnBoardData write FOnBoardData;
      {: Response to QueryAllTask command}
      property OnQueryAllTask: TOnQueryAllTask read FOnQueryAllTask write FOnQueryAllTask;
  end;

  TPin = class (TComponent)
    private
      FBoard: TBoard;
      FEnabled: Boolean;
      FPin: Byte;
      FValue: integer;   // last value read on pin
      FMode: TPinModes;
      FState: integer;         // last value written on pin
      FReporting: Boolean;  // if report must enabled when set mode to input or analoginput
      FBoardPinsNumber: integer;  // Number of total pins
      FAnalogPinsNumber: integer;  //number of analog pins
      FOnEnabled: TOnEnabled;
      FOnDisabled: TOnDisabled;


      FOnPinValue: TOnPinValue;
      FOnPinState: TOnPinState;

      procedure setEnabled(State: Boolean);
      procedure setMode(Mode: TPinModes);
      procedure setPin(pin: Byte);
      procedure setBoard(Board: TBoard);
      function AnalogWrite(Value: word; write: Boolean=true): string;
      function AnalogWriteExtended(Value: uint32; write: Boolean=true): string;
    public
      {: write digital pin value}
      function SetDigitalPinValue(Value: Byte; write: Boolean=true): string;
      {: same as setPinDigitalPinValue}
      function DigitalWrite(Value: Byte; write: Boolean=true): string;
      {: write digital values in all pins of port}
      function DigitalWritePort(Port: byte; Value: integer; write: Boolean=true): string;
      // pins general
      {: ask pin state command}
      function askPinState(write: Boolean=true): string;
      {: set pin mode command of pin definided in TPIn}
      function SetPinMode(write: Boolean=true): string;
      {: Enable/disable digital reporting, be carefull with digital pins}
      function DigitalReport(enabled: boolean; write: Boolean=true): string;
      // digital pins
      // Analog pins
      {: Enable/disable analog reporting}
      function AnalogReport(enabled: boolean; write: Boolean=true): string;
      // get command
      {: get analog value}
      procedure GetAnalogMessage(Value: integer);
      // send commands
      {: send a command, not sysex}
      function SendCommand(Data: string; write: Boolean=True): string;
      {: send a sysex command}
      function SendSysEx(data: string; write: Boolean=true): string;

      {: enable/disable report for digital pin or analog pin}
      function ReportPin(Enabled: boolean; write: Boolean=true): string;
      {: write analog or digital pin value}
      function WriteValue(Value: uint32; write: Boolean=True): string;

      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      property Enabled: Boolean read FEnabled write SetEnabled;
    published
      property Name;
      {: board owned pin}
      property Board: TBoard read FBoard write setBoard;
      {: pin number in board}
      property Pin: byte read FPin write setPin;
      {: mode of pin}
      property Mode: TPinModes read FMode write setMode;
      {: last value read on pin}
      property Value: integer read FValue;
      {: true when pin is reporting data}
      Property Reporting: Boolean read FReporting;
      {: last value written on pin}
      property State: integer read FState;
      {: event reporting pin value}
      property OnPinValue: TOnPinValue read FOnPinValue write FOnPinValue;
      {: event reportint pin state}
      property OnPinState: TOnPinState read FOnPinState write FOnPinState;
      property OnEnabled: TOnEnabled read FOnEnabled write FOnEnabled;
      property OnDisabled: TOnDisabled read FOnDisabled write FOnDisabled;
  end;

  TTask = class (TComponent)
    private
      FBoard: TBoard;
      FEnabled: Boolean;
      FTaskID: Byte;  // Task ID

      FDataTask: String;
      FDataTaskRunOnce: string;
      FRunOnce: Boolean;
      FTimeDelay: Integer;
      FRunDelay: Integer;
      FOnEnabled: TOnEnabled;
      FOnDisabled: TOnDisabled;

      FOnTaskError: TOnTaskError;
      FOnQueryTask : TOnQueryTask;

      procedure setBoard(Board: TBoard);
      procedure setEnabled(State: Boolean);
      procedure setTask(Task: Byte);
      procedure setTimeDelay(Delay: integer);
      procedure setRunDelay(Delay: integer);

      function SendSysEx(data: string; write: Boolean=true): string;
      {: internal run task}
      function ScheduleTask(write: Boolean=true): string;
    public
      procedure parsefirmatacommand(Sender: TObject; CommandData: String);
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      {: create task}
      function CreateTask(write: Boolean=true): string;
      {: stop and delte task}
      function DeleteTask(write: Boolean=true): string;
      {: add data to task, max 51 bytes every call}
      function AddToTask(Data: String; write: Boolean=true): string;
      {: ask query data task}
      function QueryTask(write: Boolean=true): string;
      {: delay a running task}
      function DelayTask(write: Boolean=true): string;
      {: If enabled, then task is run}
      property Enabled: Boolean read FEnabled write SetEnabled;
    published
      {: ID of task, 0..127}
      property TaskID: Byte read FTaskID write setTask;
      {: Task data string}
      property DataTask: string read FDataTaskRunOnce write FDataTaskRunOnce;
      {: if false and automatic end delay is assigned, to run it again}
      property RunOnce: Boolean read FRunOnce write FRunOnce;  // task is only run once
      {: time delay after task end, used for automatic run again}
      property TimeDelay: integer read FTimeDelay write setTimeDelay;  // delay for task
      {: time delay for first run in automatic run again}
      property RunDelay: integer read FRunDelay write setRunDelay;  // delay for firts run
      property Board: TBoard read FBoard write setBoard;
      property OnEnabled: TOnEnabled read FOnEnabled write FOnEnabled;
      property OnDisabled: TOnDisabled read FOnDisabled write FOnDisabled;
      property OnTaskError: TOnTaskError read FOnTaskError write FOnTaskError;
      {: response for query task data}
      property OnQueryTask: TOnQueryTask read FOnQueryTask write FOnQueryTask;
  end;

  TOneWire = class (TComponent)
    private
      FBoard: TBoard;
      FEnabled: Boolean;
      FPin: Byte;
      FDevice: string;
      FParasitisticPower: Boolean;

      FOnEnabled: TOnEnabled;
      FOnDisabled: TOnDisabled;
      FOnOneWireData:  TOnOneWireData;
      FOnSearch: TOnSearch;
      FOnOneWireAlarm: TOnOneWireAlarm;

      procedure setPin(Pin: Byte);
      procedure setBoard(Board: TBoard);
      procedure setEnabled(State: Boolean);

      function SendSysEx(data: string; write: Boolean=true): string;
    public
      procedure parsefirmatacommand(Sender: TObject; CommandData: String);
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      // Firmata comands
      // All firmata functions for commands return the string command sent, they have a write parameter, if write is True the data is sent to the external device
      // if write is false then is not sent to device
      // Onewire functions
      {: config one wire pin}
      function config(write: Boolean=true): string;
      {: search for one wire devices on bus}
      function Search(write: Boolean=true): string;
      {: search devices in alarm state on bus}
      function AlarmSearch(write: Boolean=true): string;
      {: reset one wire bus}
      function Reset(write: Boolean=true): string;
      {: send a command to all onewire devices}
      function Skip(write: Boolean=true): string;
      {: select device on bus}
      function Select(write: Boolean=true): string;
      function ResetAndSelect(write: Boolean=true): string;
      function ResetAndSelectAndWrite(DataOut: string; write: Boolean=true): string;
      function ResetAndSelectAndWrite(Delay: integer; DataOut: string; write: Boolean=true): string; overload;
      function ResetAndSelectAndWriteAndRead(BytestoRead: uint16; Correlation: uint16;
                                       Delay: integer; DataOut: string; write: Boolean=true): string;
      function Read(BytestoRead: uint16; Correlation: uint16; write: Boolean=true): string;
      function Write(Data: string; write: Boolean=true): string;
      function Write(Delay: integer; Data: string; write: Boolean=true): string; overload;
      function WriteAndRead(BytestoRead: uint16; Correlation: uint16; Data: string; write: Boolean=true): string;
      function WriteAndRead(BytestoRead: uint16; Correlation: uint16;
                                       Delay: integer; Data: string; write: Boolean=true): string; overload;
      function SendCommands(Command: Byte; numBytesToRead: uint16; correlationId: uint16;
                                       delay: integer; dataToWrite: String; write: Boolean=true): string;

      property Enabled: Boolean read FEnabled write SetEnabled;
    published
      {: one wire bus pin}
      property Pin: Byte read FPin write setPin;
      property Board: TBoard read FBoard write setBoard;
      property OnEnabled: TOnEnabled read FOnEnabled write FOnEnabled;
      Property Device: string read FDevice write FDevice;
      property OnDisabled: TOnDisabled read FOnDisabled write FOnDisabled;
      {: get data response event}
      property OnOneWireData: TOnOneWireData read FOnOneWireData write FOnOneWireData;
      {: reponse for query device IDs on bus}
      property OnSearch: TOnSearch read FOnSearch write FOnSearch;
      {: reponse for query alarm device IDs on bus}
      property OnOneWireAlarm: TOnOneWireAlarm read FOnOneWireAlarm write FOnOneWireAlarm;
      {: Enable/disable parasitistic power on bus}
      property ParasitisticPower: Boolean read FParasitisticPower write FParasitisticPower;
  end;

  TI2C = class (TComponent)
    private
      FBoard: TBoard;
      FEnabled: Boolean;

      FDelay: byte;
      FI2CQueries: Byte;
      FSDApin: Byte;
      FSCLpin: Byte;
      FSequence: byte;  // 0-7
      F10bits: Boolean;
      FContinuously: array of byte; // store slaves in read continuosly

      FOnEnabled: TOnEnabled;
      FOnDisabled: TOnDisabled;
      FOnI2CData: TOnI2CData;

      procedure setSDAPin(pin: Byte); // set SDA pin
      procedure setSCLPin(pin: Byte); // set SCL pin
      procedure setBoard(Board: TBoard);
      procedure setEnabled(State: Boolean);
      procedure setSequence(Value: Byte);

      function SendSysEx(data: string; write: Boolean=true): string;

    public
      procedure parsefirmatacommand(Sender: TObject; CommandData: String);
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      // Firmata comands
      // All firmata functions for commands return the string command sent, they have a write parameter, if write is True the data is sent to the external device
      // if write is false then is not sent to device
      // I2c commands
      function Config(write: Boolean=true): string;   // delay
      function Config(Delay: byte; write: Boolean=True): string; overload;  // delay
      function Request(Slave: word; command: byte; data: string; restart: Boolean=false; write: Boolean=True): string;
      function WriteData(Slave: word; Address: integer; AddressSize: Byte; Data: String; restart: Boolean=false; write: Boolean=True): string;
      function Read(Slave: word; regID: integer; BytesToRead: byte; restart: Boolean=false; write: Boolean=true): string;
      function ReadContinuously(Slave: word; RegID: integer; BytesToRead: byte; restart: Boolean=false; write: Boolean=true): string;
      function StopReading(Slave: word; write: Boolean=true): string;
      // not published
      property Enabled: Boolean read FEnabled write SetEnabled;
      property Type10Bits: Boolean read F10bits write F10bits;   // not supported yet
    published
      property Sequence: byte read FSequence write setSequence;
      property SDApin: Byte read FSDApin write setSDApin; // depends on board chip
      property SCLpin: Byte read FSCLpin write setSCLpin; // depends on board chip
      property Delay: Byte read FDelay write FDelay;
      property Board: TBoard read FBoard write setBoard;
      property OnEnabled: TOnEnabled read FOnEnabled write FOnEnabled;
      property OnDisabled: TOnDisabled read FOnDisabled write FOnDisabled;
      property OnI2CData: TOnI2CData read FOnI2CData write FOnI2Cdata;
  end;

  TMotorInterface = (ACCEL_INTERFACE_DRIVER, ACCEL_INTERFACE_2_WIRE, ACCEL_INTERFACE_3_WIRE, ACCEL_INTERFACE_4_WIRE);
  TStepSize = (WHOLE_STEP, HALF_STEP);

  TAccelStepper = class (TComponent)
     private
       FBoard: TBoard;
       FEnabled: Boolean;

       FDevice: Byte;  // max 10 devices
       FSteps: integer;
       FMotorPin1: byte;
       FMotorPin2: byte;
       FMotorPin3: byte;
       FMotorPin4: byte;
       FMotorEnablePin: byte;
       FInvertPin1: Boolean;
       FInvertPin2: Boolean;
       FInvertPin3: Boolean;
       FInvertPin4: Boolean;
       FInvertEnablePin: Boolean;
       FInterfaceType: TMotorInterface;  // interface  7th, 6th and 5th bits bbbXXXX = driver
       FStepSize: TStepSize;   // WHOLE_STEP =               $00; //  Examples:  XXX000X = whole step
                               //HALF_STEP =                $01; // XXX001X = half step
       FSpeed: single;
       FAcceleration: single;
       FRunning: Boolean;
       FFastStop: Boolean;  // if it stops fast

       FOnEnabled: TOnEnabled;
       FOnDisabled: TOnDisabled;
       FOnStepperPosition: TOnStepperPosition;
       FOnStepperMoveCompleted: TOnStepperPosition;

       procedure setBoard(Board: TBoard);
       procedure setEnabled(State: Boolean);

       function setMotorPin(MotorPin: Byte): Byte;
       procedure setDevice(Device: Byte);
       procedure setMotorPin1(MotorPin: Byte);
       procedure setMotorPin2(MotorPin: Byte);
       procedure setMotorPin3(MotorPin: Byte);
       procedure setMotorPin4(MotorPin: Byte);
       procedure setMotorEnablePin(MotorPin: Byte);
       procedure setSpeed(Speed: single);
       procedure setAcceleration(Value: single);

       function SendSysEx(data: string; write: Boolean=true): string;
     public
       procedure parsefirmatacommand(Sender: TObject; CommandData: String);
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;

       // Firmata comands
       // All firmata functions for commands return the string command sent, they have a write parameter, if write is True the data is sent to the external device
       // if write is false then is not sent to device
       // AccelStepperGroup functions
       function Config(write: Boolean=True): string;
       function SetZero(write: Boolean=True): string;
       function Move(write: Boolean=True): string;
       function MoveTo(write: Boolean=True): string;
       function MotorEnable(State: Boolean; write: Boolean=True): string;
       function Stop(write: Boolean=True): string;
       function FastStop(write: Boolean=True): string;
       function ReportPosition(write: Boolean=True): string;
       function MotorAcceleration(write: Boolean=True): string;
       function MotorAcceleration(Acceleration: single; write: Boolean=True): string; overload;
       function MotorSpeed(write: Boolean=True): string;
       // not published
       property Enabled: Boolean read FEnabled write SetEnabled;
     published
       property Board: TBoard read FBoard write setBoard;
       property Device: byte read FDevice write setDevice;
       property MotorPin1_or_DriverStep: Byte read FMotorPin1 write setMotorPin1;
       property MotorPin2_or_DriverDirection: Byte read FMotorPin2 write setMotorPin2;
       property MotorPin3: Byte read FMotorPin3 write setMotorPin3;
       property MotorPin4: Byte read FMotorPin4 write setMotorPin4;
       property MotorEnablePin: byte read FMotorEnablePin write setMotorEnablePin;
       property InvertMotorPin1: Boolean read FInvertPin1 write FInvertPin1;
       property InvertMotorPin2: Boolean read FInvertPin2 write FInvertPin2;
       property InvertMotorPin3: Boolean read FInvertPin3 write FInvertPin3;
       property InvertMotorPin4: Boolean read FInvertPin4 write FInvertPin4;
       property InvertEnablePin: Boolean read FInvertEnablePin write FInvertEnablePin;
       property InterfaceType: TMotorInterface read FInterfaceType write FInterfaceType;
       property StepSize: TStepSize read FStepSize write FStepSize;
       property Speed: single read FSpeed write setSpeed;
       property Steps: integer read FSteps write FSteps;
       property Acceleration: single read FAcceleration write setAcceleration;
       property Running: Boolean read FRunning;
       property OnEnabled: TOnEnabled read FOnEnabled write FOnEnabled;
       property OnDisabled: TOnDisabled read FOnDisabled write FOnDisabled;
       property OnStepperPosition: TOnStepperPosition read FOnStepperPosition write FOnStepperPosition;
       property OnStepperMoveCompleted: TOnStepperPosition read FOnStepperMoveCompleted write FOnStepperMoveCompleted;
   end;

  TMember = record
    Device: Byte;
    Member: TAccelStepper;
  end;

  TAccelStepperGroup = class (TComponent)
    private
      FBoard: TBoard;
      FEnabled: Boolean;
      FGroup: Byte;  // group ID
      FMembers: array of TMember;
      FOnEnabled: TOnEnabled;
      FOnDisabled: TOnDisabled;
      FOnAccelStepperMultiMoveCompleted: TOnAccelStepperMultiMoveCompleted;

      procedure setBoard(Board: TBoard);
      procedure setEnabled(State: Boolean);
      function getMember(Index: integer): TMember;
      procedure setGroup(Group: Byte);

      function SendSysEx(data: string; write: Boolean=true): string;

    public
      procedure parsefirmatacommand(Sender: TObject; commandData: String);
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      // Firmata comands
      // All firmata functions for commands return the string command sent, they have a write parameter, if write is True the data is sent to the external device
      // if write is false then is not sent to device
      // AccelStepperGroup functions
      procedure AddMember(FirmataAccelStepper: TAccelStepper);
      procedure DeleteMember(FirmataAccelStepper: TAccelStepper);
      function StepperMultiConfig(write: Boolean=True): string;
      function StepperMultiTo(Position: integer; write: Boolean=True): string;
      function StepperMultiStop(write: Boolean=True): string;
      // util functions
      // not published
      property Members[Index: integer]: TMember read getMember;
      property Enabled: Boolean read FEnabled write SetEnabled;
    published
      property Board: TBoard read FBoard write setBoard;
      property Group: byte read FGroup write setGroup;
      property OnEnabled: TOnEnabled read FOnEnabled write FOnEnabled;
      property OnDisabled: TOnDisabled read FOnDisabled write FOnDisabled;
      property OnAccelStepperMultiMoveCompleted : TOnAccelStepperMultiMoveCompleted read FOnAccelStepperMultiMoveCompleted write FOnAccelStepperMultiMoveCompleted;
    end;

  TSerial = class (TComponent)
      private
        FBoard: TBoard;
        FEnabled: Boolean;
        FBaudRate: integer;
        FPort: TSerialPorts;

        FRxPin: Byte;
        FTxPin: Byte;

        FOnEnabled: TOnEnabled;
        FOnDisabled: TOnDisabled;
        FOnSerialMessage: TOnSerialMessage;

        procedure setPort(Port: TSerialPorts);
        procedure setBoard(Board: TBoard);
        procedure setEnabled(State: Boolean);
        procedure setRxPin(Pin: Byte);
        procedure setTxPin(Pin: Byte);
        procedure setBaudRate(BaudRate: integer);

        function SendSysEx(data: string; write: Boolean=true): string;
      public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        // Firmata comands
        // All firmata functions for commands return the string command sent, they have a write parameter, if write is True the data is sent to the external device
        // if write is false then is not sent to device
        // Serial
        function Config(write: Boolean=true): String;
        function Write(Data: string; write: Boolean=true): String;
        function Read(ReadMode: byte; BytesToRead: uint16; write: Boolean=true): String;
        function Close(write: Boolean=True): string;
        function Flush(write: Boolean=True): string;
        function Listen(write: Boolean=True): string;

        property Enabled: Boolean read FEnabled write SetEnabled;
      published
        property Port: TSerialPorts read FPort write setPort;
        property Board: TBoard read FBoard write setBoard;
        property BaudRate: integer read FBaudRate write setBaudRate;  // MAX about 50000
        property RxPin: Byte read FRxPin write setRxPin;
        property TxPin: Byte read FTxPin write setTxPin;
        property OnEnabled: TOnEnabled read FOnEnabled write FOnEnabled;
        property OnDisabled: TOnDisabled read FOnDisabled write FOnDisabled;
        property OnSerialMessage: TOnSerialMessage read FOnSerialMessage write FOnSerialMessage;
    end;

  TServo = class (TComponent)
    private
      FBoard: TBoard;
      FEnabled: Boolean;
      FDevice: Byte;    // to make different servos and pins, from 0 to MAX_SERVOS - 1
      FPin: Byte;
      FMinPulse: integer;
      FMaxPulse: integer;
      FValue: integer;    // value to send servo

      FOnEnabled: TOnEnabled;
      FOnDisabled: TOnDisabled;

      function AnalogWrite(Value: word; write: Boolean=true): string;
      function AnalogWriteExtended(Value: uint32; write: Boolean=true): string;
      procedure setEnabled(State: Boolean);
      procedure setPin(pin: Byte);
      procedure setMinPulse(Pulse: integer);
      procedure setMaxPulse(Pulse: integer);
      procedure setBoard(Board: TBoard);
      procedure setValue(Value: integer);
    public
      function WriteValue(write: Boolean=true): string;

      function SendCommand(Data: string; write: Boolean=True): string;
      function SendSysEx(data: string; write: Boolean=true): string;

      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function config(write: Boolean=true): string;

      property Enabled: Boolean read FEnabled write SetEnabled;
    published
      Property Device: Byte read FDevice;
      property Pin: byte read FPin write setPin;
      property Board: TBoard read FBoard write setBoard;
      property MinPulse: integer read FMinPulse write setMinPulse;
      Property MaxPulse: integer read FMaxPulse write setMaxPulse;
      property Value: integer read FValue write setValue;
      property OnEnabled: TOnEnabled read FOnEnabled write FOnEnabled;
      property OnDisabled: TOnDisabled read FOnDisabled write FOnDisabled;
  end;

  TEncoder = class (TComponent)
    private
      FBoard: TBoard;
      FEnabled: Boolean;
      FDevice: Byte;

      FPinA: Byte;
      FPinB: Byte;

      FOnEnabled: TOnEnabled;
      FOnDisabled: TOnDisabled;
      FOnEncoderPosition: TOnEncoderPosition;

      procedure setPinA(pin: Byte);
      procedure setPinB(pin: Byte);
      procedure setBoard(Board: TBoard);
      procedure setEnabled(State: Boolean);

      function SendSysEx(data: string; write: Boolean=true): string;

    public
      procedure parsefirmatacommand(Sender: TObject; CommandData: String);
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      // Firmata comands
      // All firmata functions for commands return the string command sent, they have a write parameter, if write is True the data is sent to the external device
      // if write is false then is not sent to device
      function Attach(write: Boolean=true): string;
      function QueryPosition(write: Boolean=true): string;
      function AllEncoderPositions(write: Boolean=true): string;
      function ResetPosition(write: Boolean=true): string;
      function Reporting(Enabled: Boolean; write: Boolean=true): string;
      function Detach(write: Boolean=true): string;
      // not published
      property Enabled: Boolean read FEnabled write SetEnabled;
    published
      property Device: Byte read FDevice;
      property PinA: Byte read FPinA write setPinA;
      property PinB: Byte read FPinB write setPinB;
      property Board: TBoard read FBoard write setBoard;
      property OnEnabled: TOnEnabled read FOnEnabled write FOnEnabled;
      property OnDisabled: TOnDisabled read FOnDisabled write FOnDisabled;
      property OnEncoderPosition: TOnEncoderPosition read FOnEncoderPosition write FOnEncoderPosition;
  end;

  TInterruptModes = (INTERRUPT_MODE_DISABLE,INTERRUPT_MODE_LOW, INTERRUPT_MODE_HIGH,
                     INTERRUPT_MODE_RISING, INTERRUPT_MODE_FALLING, INTERRUPT_MODE_CHANGE);

  TFrequency = class (TComponent)
    private
      FBoard: TBoard;
      FEnabled: Boolean;
      FPin: Byte;
      FDevice: Byte;        //only 2 pins are available on AVR based boards (2 and 3)
      FInterruptMode: TInterruptModes;
      FSamplingInterval: integer;

      FOnEnabled: TOnEnabled;
      FOnDisabled: TOnDisabled;
      FOnFrequencyData:  TOnFrequencyData;

      procedure setBoard(Board: TBoard);
      procedure setEnabled(State: Boolean);
      procedure setPin(Value: Byte);
      procedure setSamplingInterval(Interval: integer);
      procedure setInterruptMode(Mode: TInterruptModes);

      function DisableReporting(write: Boolean=true): string;
      function Attach(write: Boolean=true): string;
      function SendSysEx(data: string; write: Boolean=true): string;
    public
      procedure parsefirmatacommand(Sender: TObject; CommandData: String);
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      // Firmata comands
      // All firmata functions for commands return the string command sent, they have a write parameter, if write is True the data is sent to the external device
      // if write is false then is not sent to device
      // Frequency functions

      property Enabled: Boolean read FEnabled write SetEnabled;
    published
      {: one wire bus pin}
      property SamplingInterval: integer read FSamplingInterval write setSamplingInterval;
      property InterruptMode: TInterruptModes read FInterruptMode write setInterruptMode;
      property Pin: Byte read FPin write setPin;
      property Board: TBoard read FBoard write setBoard;
      property OnEnabled: TOnEnabled read FOnEnabled write FOnEnabled;
      Property Device: byte read FDevice;
      property OnDisabled: TOnDisabled read FOnDisabled write FOnDisabled;
      {: get data response event}
      property OnFrequencyData: TOnFrequencyData read FOnFrequencyData write FOnFrequencyData;
  end;

//
//
//
{ TSPI }
//
//
//
TSPI = class (TComponent)
    private
      FBoard: TBoard;
      FEnabled: Boolean;
      FDeviceID: byte;
      FMOSIpin: byte;
      FMISOpin: byte;
      FCLKpin: byte;
      FCSpin: Byte;
      FSpeed: integer;
      FWordSize: byte;   // Only word size of 8 bits in this moment
      FMode: byte;
      FCSControl: boolean;
      FCSActiveLow: Boolean;
      FBitOrder: TBitOrder;
      FChannel: byte;   // only channel 0 in this moment
      FPacketData7Bit: Boolean;

      FOnEnabled: TOnEnabled;
      FOnDisabled: TOnDisabled;
      FOnSPIData: TOnSPIData;

      procedure setMOSIPin(pin: Byte); // set MOSI pin
      procedure setMISOPin(pin: Byte); // set MISO pin
      procedure setCLKPin(pin: Byte); // set CLK pin
      procedure setCSPin(pin: Byte); // set CS pin, select pin
      procedure setMode(Mode: byte);
      procedure setChannel(Value: Byte);   // HW SPIs
      procedure setBoard(Board: TBoard);
      procedure setEnabled(State: Boolean);
      procedure setDeviceID(Value: byte);
      procedure setWordSize(Value: byte);
      procedure setSpeed(Value: integer);
      procedure setCSControl(Value: Boolean);
      procedure setBitOrder(value: TBitOrder);
      procedure setPacketData7Bit(Value: boolean);

      function SendSysEx(data: string; write: Boolean=true): string;
    public
      procedure parsefirmatacommand(Sender: TObject; CommandData: String);
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      // Firmata comands
      // All firmata functions for commands return the string command sent, they have a write parameter, if write is True the data is sent to the external device
      // if write is false then is not sent to device
      // SPI commands
      function SPIBegin(write: Boolean=true): string;
      function Config(write: Boolean=true): string;
      function Transfer(RequestID: byte; Data: string; DeselectCSPin: Boolean=True; write: Boolean=true): string;
      function Write(RequestID: byte; Data: string; DeselectCSPin: Boolean=True; write: Boolean=true): string;
      function WriteAck(RequestID: byte; Data: string; DeselectCSPin: Boolean=True; write: Boolean=true): string;
      function Read(RequestID: byte; NumData: byte; DeselectCSPin: Boolean=True; write: Boolean=true): string;
      function SPIEnd(write: Boolean=true): string;
      function setCSPinValue(Value: Byte; write: Boolean=true): string;

      // not published
      property Enabled: Boolean read FEnabled write SetEnabled;
      property DeviceID: Byte read FDeviceID write setDeviceID;
      property WordSize: byte read FWordSize write setWordSize;
      property Channel: byte read FChannel write setChannel;

    published
      property PacketData7Bit: Boolean read FPacketData7Bit write setPacketData7Bit;
      property MISOPin: Byte read FMISOpin write setMISOpin;
      property MOSIPin: Byte read FMOSIpin write setMOSIpin;
      property CLKPin: Byte read FCLKpin write setCLKpin;
      property CSPin: Byte read FCSpin write setCSpin;
      property Speed: integer read FSpeed write setSpeed;
      property Mode: byte read FMode write setMode;
      property BitOrder: TBitOrder read FBitOrder write setBitOrder;
      property CSControl: Boolean read FCSControl write setCSControl;
      property CSActiveLow: Boolean read FCSActiveLow write FCSActiveLow;
      property Board: TBoard read FBoard write setBoard;
      property OnEnabled: TOnEnabled read FOnEnabled write FOnEnabled;
      property OnDisabled: TOnDisabled read FOnDisabled write FOnDisabled;
      property OnSPIData: TOnSPIData read FOnSPIData write FOnSPIdata;
  end;
//
//
// TDHT
//
//

TDHT = class (TComponent)
private
  FBoard: TBoard;
  FEnabled: Boolean;
  FPin: Byte;
  FBlockingReads: Boolean;
  FSamplingInterval: integer;  // default and minimun is 500ms
  FSensorType: TDHTSensorType;  // type of sensor DHT11 or DHT12

  FOnEnabled: TOnEnabled;
  FOnDisabled: TOnDisabled;
  FOnDHTData: TOnDHTData;

  procedure setBoard(Board: TBoard);
  procedure setEnabled(State: Boolean);
  procedure setPin(pin: Byte); // set pin
  procedure setSensorType(Value: TDHTSensorType);
  procedure setBlockingReads(value: Boolean);
  procedure setSamplingInterval(Value: integer);

  function SendSysEx(data: string; write: Boolean=true): string;
public
  procedure parsefirmatacommand(Sender: TObject; CommandData: String);
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;

  // Firmata comands
  // All firmata functions for commands return the string command sent, they have a write parameter, if write is True the data is sent to the external device
  // if write is false then is not sent to device
  // DHT commands
  function Attach(write: Boolean=true): string;
  function Detach(write: Boolean=true): string;

  // not published
  property Enabled: Boolean read FEnabled write SetEnabled;
  // not use at the momment
  property BlockingReads: Boolean read FBlockingReads write setBlockingReads;   // not used
  property SamplingInterval: integer read FSamplingInterval write setSamplingInterval;  // not used
published
  property Pin: Byte read FPin write setPin;
  property SensorType: TDHTSensorType read FSensorType write FSensorType;
  property Board: TBoard read FBoard write setBoard;
  property OnEnabled: TOnEnabled read FOnEnabled write FOnEnabled;
  property OnDisabled: TOnDisabled read FOnDisabled write FOnDisabled;
  property OnDHTData: TOnDHTData read FOnDHTData write FOnDHTData;
end;

 { TWheelBits = bitpacked record
    two_bits: 0..3;
    5th_button: Boolean;
    4th_buton: Boolean;
    Move: 0..15;
  end; }

  { TStatusMoveBits = bitpacked record
    y_of: Boolean;   // y overflow
    x_of: Boolean;     // x overflow
    y_sign: Boolean;
    x_sign: Boolean;
    always_1: 1;
    mid_button: Boolean;
    right_button: Boolean;
    lef_button: Boolean;
  end;       }

 { TStatusMouseBits = bitpacked record
      always_0: 0;
      Mode: 0..1; // 1 remote, 0 stream
      Reporting: Boolean;
      Scaling: TMouseScaling;
      always_0_0: 0:
      mid_button: Boolean;
      right_button: Boolean;
      lef_button: Boolean;
  end;}

  TMouseResolution = (PS2MOUSE_1_COUNT_MM, PS2MOUSE_2_COUNT_MM, PS2MOUSE_4_COUNT_MM, PS2MOUSE_8_COUNT_MM);
  TMouseScaling = (PS2MOUSE_SCALING_1_TO_1, PS2MOUSE_SCALING_2_TO_1);
  TMouseSampleRate = (R_10, R_20, R_40, R_60, R_80, R_100, R_200);
  TMouseMode = (PS2MOUSE_STREAM, PS2MOUSE_REMOTE);

  TPS2Mouse = class (TComponent)
      private
        FBoard: TBoard;
        FStatus: TPS2MouseStatus;
        FEnabled: Boolean;
        FClockPin: Byte;
        FDataPin: Byte;
        FDevice: Byte;
        FMode: TMouseMode;
        FMouseType: TMouseType;
        FReporting: Boolean;
        FResolution: TMouseResolution;
        FSampleRate: TMouseSampleRate;
        FScaling: TMouseScaling;
        FMouseData: TPS2MouseData;
        FMouseStatus: TPS2MouseStatus;
        FOnEnabled: TOnEnabled;
        FOnDisabled: TOnDisabled;
        FOnMouseData:  TOnMouseData;
        FOnMouseStatus:  TOnMouseStatus;
        FOnMouseDeviceID: TOnMouseDeviceID;

        procedure setClockPin(Pin: Byte);
        procedure setDataPin(Pin: Byte);
        procedure setBoard(Board: TBoard);
        procedure setEnabled(State: Boolean);
        procedure setReporting(Enable: Boolean);
        procedure setSampleRate(SampleRate: TMouseSampleRate);
        procedure setResolution(Resolution: TMouseResolution);

        function SendSysEx(data: string; write: Boolean=true): string;
      public
        procedure parsefirmatacommand(Sender: TObject; CommandData: String);
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        // Firmata comands
        // All firmata functions for commands return the string command sent, they have a write parameter, if write is True the data is sent to the external device
        // After mouse config is set to SampleRate:=R_60, Mode:=PS2MOUSE_REMOTE, MouseType:=STILL_UNKNOWN,
        //              Resolution:=PS2MOUSE_8_COUNT_MM, SampleRate:=R_60, Scaling:=SCALING_1_TO_1
        // if write is false then is not sent to device
        // ps2mouse functions
        function config(write: Boolean=true): string;
        function QueryData(write: Boolean=true): string;
        function QueryStatus(write: Boolean=true): string;
        function QueryDeviceID(write: Boolean=true): string;
        function Reset(write: Boolean=true): string;
        function SendReporting(write: Boolean=true): string;
        function SendResolution(write: Boolean=true): string;
        function SendSampleRate(write: Boolean=true): string;
        function SendFiveButtonsMode(write: Boolean=true): string;
        function SendRemoteMode(write: Boolean=true): string;

        property Enabled: Boolean read FEnabled write SetEnabled;
        property MouseData: TPS2MouseData read FMouseData;
        property Status: TPS2MouseStatus read FStatus;
        property Device: Byte read FDevice;
        property MouseType: TMouseType read FMouseType;
        property Scaling: TMouseScaling read FScaling;
        property Mode: TMouseMode read FMode;
      published
        property Resolution: TMouseResolution read FResolution write setResolution;
        property SampleRate: TMouseSampleRate read FSampleRate write setSampleRate;
        property ClockPin: Byte read FClockPin write setClockPin;
        property DataPin: Byte read FDataPin write setDataPin;
        property Board: TBoard read FBoard write setBoard;
        property Reporting: Boolean read FReporting write setReporting;
        property OnEnabled: TOnEnabled read FOnEnabled write FOnEnabled;
        property OnDisabled: TOnDisabled read FOnDisabled write FOnDisabled;
        property OnMouseData: TOnMouseData read FOnMouseData write FOnMouseData;
        property OnMouseStatus: TOnMouseStatus read FOnMouseStatus write FOnMouseStatus;
        property OnMouseDeviceID: TOnMouseDeviceID read FOnMouseDeviceID write FOnMouseDeviceID;
    end;

  TPixel = record
    PixelColor: longword; // 8 bits color, RGBW = 32 bits
  end;

  TPixels= array of TPixel;

  TNeoPixel = class (TComponent)
    private
       FBoard: TBoard;
       FEnabled: Boolean;
       FPin: Byte;
       FDevice: Byte;
       FLedColors: String;
       FKHZ400: Boolean;
       FPixelsNumber: smallint;
       FPixels: TPixels;
       FBrightness: Byte;
       FGamma: single;
       FFadeRunning: Boolean;
       FFadeColor: longword;
       FFadeFirst: smallint;
       FFadeLast: smallint;
       FFadeLoopsWait: byte;
       FShiftFirst: smallint;
       FShiftLast: smallint;
       FShiftType: byte;
       FOnFadeEnd: TOnFadeEnd;
       FOnEnabled: TOnEnabled;
       FOnDisabled: TOnDisabled;

       procedure setPin(Pin: Byte);
       procedure setBoard(Board: TBoard);
       procedure setEnabled(State: Boolean);
       procedure setLedColors(rgbwType: string); // 0bRRRRGGBB for RGB + NEO_KHZ800,  0bWWRRGGBB for RGBW devices
       procedure setPixelsNumber(Number: smallint);
       procedure setBrightness(value: Byte);
       function getNeoPixel(Index: integer): TPixel;
       procedure setFadeRunning(Enabled: Boolean);
       procedure setFadeLoopsWait(Loops: byte);
       procedure setGamma(Value: single);
       procedure parsefirmatacommand(Sender: TObject; Data: String);
       function SendSysEx(data: string; write: Boolean=true): string;
     public
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;

       // Firmata comands
       // All firmata functions for commands return the string command sent, they have a write parameter, if write is True the data is sent to the external device
       // if write is false then is not sent to device
       // neopixel functions

       function ColorHSV(hue: uint16; sat: byte; val: byte): uint32;
       procedure RainBow(First_Pixel: uint16; Last_Pixel: uint16; First_hue: uint16 = 0;
             saturation: byte = 255; brightness: byte = 255; gammify: boolean = false; Delay: smallint=0; do_show: Boolean=true);
       function gamma32(x: uint32; gamma: single): uint32;


       function config(write: Boolean=true): string;
       function Off(write: Boolean=true): string;
       function Show(write: Boolean=true): string;

       {: Set pixel color, pixelnumber, color, show color after that}
       function PixelColor(Pixel: smallint; Color: longword; do_show: boolean=true; write: Boolean=true): string;
       {: Set pixel color, pixelnumber, Red, Green, Blue and White, show color after that}
       function PixelColor(Pixel: smallint; Red: byte; Green: byte; Blue: Byte; White: byte; do_show: boolean=true; write: Boolean=true): string; overload;
       {: Fill segment of pixels with color from First to Last pixel with color and Filldelay every pixel, do_show  after every pixel}
       function FillSegment(Color: longword; First: smallint; Last: smallint; do_show: Boolean=true; write: Boolean=true): string;
       {: Fill segment of pixels with color from First to Last pixel with color and Filldelay every pixel, do_show  after every pixel}
       function FillSegment(Red: byte; Green: byte; Blue: Byte; White: byte; first: smallint; last: smallint; do_show: Boolean=true; write: Boolean=true): string; overload;
       {: Fill complete strip with color from First to Last pixel with color and Filldelay every pixel, do_show  after every pixel}
       function FillStrip(Color: longword; do_show: Boolean=true; write: Boolean=true): string;
       {: Fill complete strip with color from First to Last pixel with color and Filldelay in ms every pixel, do_show  after every pixel}
       function FillStrip(Red: byte; Green: byte; Blue: Byte; White: byte; do_show: Boolean=true; write: Boolean=true): string; overload;
       {: Config shift segment of pixels}
       function ShiftSegmentConfig(First: smallint; last: smallint; Direction: char; Wrap: Boolean; do_show: Boolean=true; write: Boolean=true): string;
       {: Config shift strip of pixels}
       function ShiftStripConfig(Direction: char; Wrap: Boolean; do_show: Boolean=true; write: Boolean=true): string; overload;
       {: Run a step of shift segment or strema of pixels with Shitdelay}
       function ShiftRun(write: Boolean=True): string;
       {: Run a step of shift segment or strema of pixels with Shiftdelay}
       function ShiftRun(ShiftType: Byte; write: Boolean=True): string; overload;
       {: Config shade segment final color}
       function FadeSegmentConfig(color: longword; first: smallint; last: smallint; write: Boolean=True): string;
       {: Config shade segment final color}
       function FadeSegmentConfig(Red: byte; Green: byte; Blue: Byte; White: byte; first: smallint; last: smallint; write: Boolean=True): string; overload;
       {: Config shade strip final color}
       function FadeStripConfig(color: longword; write: Boolean=True): string;
       {: Config shade strip final color}
       function FadeStripConfig(Red: byte; Green: byte; Blue: Byte; White: byte; write: Boolean=True): string;  overload;
       {: Run fade segment or strip with Delay = (FadeWait + 1) * Board.SamplingInterval in ms, if FadeWait = 0 then is about 50 Hz}
       function FadeRunPause(write: Boolean=True): string;
       {: Run a fade segment or strip step}
       function FadeOneStep(do_show: boolean=true; write: Boolean=True): string;
       {: Copy count pixels from src to dest}
       function CopyPixels(src: smallint; dest: smallint; count: smallint; do_show: Boolean=true; write: Boolean=True): string;
       function sendBrightness(do_show: Boolean=true; write: Boolean=true): string;

       property Pixels[Index: integer]: TPixel read getNeoPixel;
       property Enabled: Boolean read FEnabled write SetEnabled;
       property Device: Byte read FDevice;
       property ShiftType: byte read FShiftType;
     published
       property FadeWait: Byte read FFadeLoopsWait write setFadeLoopsWait;
       property FadeRunning: Boolean read FFadeRunning write setFadeRunning;
       property Gamma: single read FGamma write setGamma;
       property Brightness: Byte read FBrightness write setBrightness;
       property LedColors: string read FLedColors write setLedColors;
       property KHZ400: Boolean read FKHZ400 write FKHZ400;
       property PixelsNumber: smallint read FPixelsNumber write setPixelsNumber;
       property Pin: Byte read FPin write setPin;
       property Board: TBoard read FBoard write setBoard;
       property OnFadeEnd: TOnFadeEnd read FOnFadeEnd write FOnFadeEnd;
       property OnEnabled: TOnEnabled read FOnEnabled write FOnEnabled;
       property OnDisabled: TOnDisabled read FOnDisabled write FOnDisabled;
   end;

// Utils functions
{: encode 1 8-bit-byte char into 2 7-bit-char}
function Encode1ByteCharTo2(Data1Byte: string): string;
{: decode 2 bye 7-bit-char into 1  8-bit-byte char}
function Decode2BytesCharTo1(Data2bytes: string): string;
{: returns an hex string from a byte char}
function StrToHex(const Buffer: string): string;
{: returns an hex separated byte string from a byte char}
function StrToHexSep(const Value: string): string;
{: decode a 7-bit-char string into a 8-bit-char string}
function Decode7To8bit(Data7bit: string): String;
{: encode a 8-bit-char string into a 7-bit-char string}
function Encode8To7Bit(Data8bit:string): string;
{: converts a N bytes string, low byte first, into a positive value}
function decodeNbytestoInt(DataString: String): QWord;
{: converts a 64-bit unsigned int value into a byte string low byte first}
function encode64BitUnsignedInt(Value: QWord): string;
{: converts a 32-bit unsigned int value into a byte string low byte first}
function encode32BitUnSignedInt(Value: Integer): String;
{: converts a 32-bit int value into a byte string low byte first}
function encode32BitSignedInt(Value: Integer): String;
{: converts a string of bytes, low byte first, into a 32-bit int value}
function decode32BitSignedInt(StrEncoded: string): integer;
{: converts a string of bytes, low byte first, into a 64-bit unsigned value}
function decode64BitUnsignedInt(StrEncoded: string): Qword;
{: check crc8 of buffer being last byte CRC8}
function CheckCRC8(Buffer: String): Boolean;
{: check crc16 of buffer being 2 last bytes CRC16}
function CheckCRC16(Buffer: String): Boolean;
{: calculate crc8 of buffer}
function CalculateCRC8(Buffer: String): Byte;
{: calculate crc16 of buffer}
function CalculateCRC16(Buffer: String): uint16;
{: encode single into accelstepper custom float (4 bytes of 7-bit char, only 28 bits are used)}
function EncodeAccelFloat(Value: Single): String;
{: decode 4 bytes custom float (7-bit-char) into a single}
function DecodeAccelFloat(Data7Bit: string): single;
{: function returning pin mode value}
function ByteToPinModes(Value: Byte): TPinModes;
{: function retuning pin mode from value}
function PinModesToByte(PinMode: TPinModes): Byte;
{: function returning Index of String Modes }
function GetIndexModeOfString(StringMode: string): integer;
{: function returning serial port type value}
function ByteToSerialPorts(Value: Byte): TSerialPorts;
{: function retuning serial port type from value}
function SerialPortsToByte(SerialPort: TSerialPorts): Byte;


//
procedure Register;


implementation

uses
  LazarusPackageIntf;

procedure Register;
begin
  {$I firmataboard.lrs}
  RegisterComponents('Firmata', [TBoard, TPin, TTask, TOneWire, TI2C, TSPI,
                   TAccelStepper, TAccelStepperGroup, TSerial, TServo, TEncoder, TFrequency, TDHT, TPS2Mouse, TNeoPixel]);
end;

//
// utils functions
//
//

function ByteToPinModes(Value: Byte): TPinModes;
begin
  if Value < ord(PIN_MODE_IGNORE) then
    Result:=TPinModes(value)
  else
    Result:=PIN_MODE_IGNORE;
end;
// PinModes to Byte
function PinModesToByte(PinMode: TPinModes): Byte;
begin
  if PinMode <> PIN_MODE_IGNORE then
    Result:=ord(PinMode)
  else
    Result:=$7F;
end;
// Get pin mode byte of string
function GetIndexModeOfString(StringMode: string): integer;
begin
  Result:= GetEnumValue(TypeInfo(TPinModes), StringMode);
end;
{function GetIndexModeOfString(StringMode: string): integer;
var
  i: Integer;
begin
  Result:=-1;  //Not found
  for i:=0 to Length(PinModesString)-1 do
    if StringMode = PinModesString[ByteToPinModes(i)] then
    begin
      Result:=i;
      break;
    end;
end;}

function ByteToSerialPorts(Value: Byte): TSerialPorts;
begin
  if Value > ord(SW_SERIAL3) then
    Result:=SW_SERIAL3
  else
    Result:=TSerialPorts(value);
end;

function SerialPortsToByte(SerialPort: TSerialPorts): Byte;
begin
  Result:=ord(SerialPort);
end;
// converts Byte string values into hex string
function StrToHex(const Buffer: string): string;
begin
  SetLength(result, 2*Length(Buffer));
  BinToHex(@Buffer[1], @result[1], Length(Buffer));
end;

// converts Byte string values into a separated hex string
function StrToHexSep(const Value: String): string;
var
  n: Integer;
begin
  Result := '';
  for n := 1 to Length(Value) do
    Result:=Result+IntToHex(ord(Value[n]), 2)+' ';
end;

function encode32BitUnSignedInt(Value: Integer): String;
var
  i: integer;
begin
  Result:='';
  for i:=0 to 4 do
    Result:=Result+chr((Value >> (7 * i)) and $7F);
end;

function encode32BitSignedInt(Value: integer): string;
var
  TmpValue: Integer;
  i: integer;
begin
  TmpValue:=Abs(Value);

  Result:='';
  for i:=0 to 4 do
    Result:=Result+chr((TmpValue >> (7 * i)) and $7F);

  if Value < 0 then
    Result[5]:=chr(ord(Result[5]) or $08);
end;

function encode64BitUnsignedInt(Value: QWord): string;
var
  i: integer;
begin
  Result:='';
  for i:=0 to 9 do
    Result:=Result+chr((Value >> (7 * i)) and $7F);
  Result:=Result+chr(Value >> 63);
end;

function decode32BitSignedInt(StrEncoded: string): integer;
begin
  Result:=ord(StrEncoded[1]) or (ord(StrEncoded[2]) << 7)  or (ord(StrEncoded[3]) << 14)
          or (ord(StrEncoded[4]) << 21) or ((ord(StrEncoded[5]) and $7) << 28);
  if (ord(StrEncoded[5]) >> 3) = 1 then // negative
    Result:=-Result;
end;

function decode64BitUnsignedInt(StrEncoded: string): QWord;
var
  i: integer;
begin
  Result:=0;
  for i:=0 to 9 do
    Result:=Result or (ord(StrEncoded[i+1]) << (i*7));
  Result:=Result or ((ord(StrEncoded[10]) << 63) and 1);
end;

// converts a n bytes string, low byte first, into a positive int value
function decodeNbytestoInt(DataString: String): QWord;
var
  i: integer;
begin
  Result:=0;

  for i:=length(DataString) downto 1 do
  begin
    Result:=(Result << 8);
    Result:=Result+ord(DataString[i]);
  end;
end;

// Get string data 7 bit encoded and decode to 8 bit data string
function Decode7To8bit(Data7bit: string): String;
var
   rotate: integer;
   i: integer;
begin
    rotate:=0;
    Result:='';
    i:=1;
    While i < Length(Data7bit) do
    begin
       Result:=Result+chr((ord(Data7bit[i]) >> rotate) or (ord(Data7bit[i+1]) << (7-rotate)));
       inc(i);
       inc(rotate);
       if rotate = 7 then
       begin
           inc(i);
           rotate:=0;
       end;
    end;
end;

// Get string Data 8 bit and encode to 7 bit Data string
function Encode8To7Bit(Data8bit:string): string;
var
   rotate: integer;
   i: integer;
begin
    Result:='';
    if Data8Bit = '' then
      exit;

    i:=0;
    rotate:=8;
    while i < length(Data8Bit) do
    begin
      inc(i);
      if rotate=8 then
      begin
        Result:=Result+chr(ord(Data8bit[i]) and $7F);
        rotate:=1;
      end;
      if i<length(Data8Bit) then
         Result:=Result+chr((ord(Data8bit[i]) >> (8-rotate)) or ((ord(Data8bit[i+1]) << rotate) and $7F))
      else
         Result:=Result+chr(ord(Data8bit[i]) >> (8-rotate));
      inc(rotate);
    end;
end;

// converts a 2 bytes char into a 1 byte char
function Decode2BytesCharTo1(Data2bytes: string): string;
var
  i: integer;
begin
  Result:='';
  i:=1;
  while i < Length(Data2bytes) do
  begin
    Result:=Result+chr(ord(Data2bytes[i]) or (ord(Data2bytes[i+1]) << 7));
    inc(i, 2);  //i:=i+2;
  end;
end;
// converts a 1 byte char into a 2 bytes char
function Encode1ByteCharTo2(Data1Byte: string): string;
var
  i: integer;
begin
  Result:='';
  for i:=1 to Length(Data1Byte) do
  begin
    Result:=Result+chr(ord(Data1Byte[i]) and $7F)+chr(ord(Data1Byte[i]) >> 7);
  end;
end;
// Returns CRC8 of a string
function CalculateCRC8(Buffer: String): Byte;
var
  i_Byte, j_Bit: Byte;
begin
  Result:=0;

  for i_Byte:=1 to Length(Buffer) do
  begin
    Result:=Result xor ord(Buffer[i_Byte]);
    for j_Bit:=1 to 8 do
    begin
      if (Result and 1 ) = 1 then
        Result:=(Result shr 1) xor $8C
      else
        Result:=Result shr 1;
    end;
  end;
end;
// Returns CRC16 of a string
function CalculateCRC16(Buffer: String): uint16;
var
  i_Byte, j_Bit: Byte;
begin
  Result:=0;

  for i_Byte:=1 to Length(Buffer) do
  begin
    Result:=Result xor ord(Buffer[i_Byte]);
    for j_Bit:=1 to 8 do
    begin
      if (Result and 1 ) = 1 then
        Result:=(Result shr 1) xor $A001
      else
        Result:=Result shr 1;
    end;
  end;
end;
// Ckeck CRC8, of a string last byte is CRC, returns true o false
function CheckCRC8(Buffer: string): Boolean;
begin
  if (Length(Buffer) < 2) then  // two bytes min, a byte and CRC
  begin
    Result:=False;
    exit;
  end;

  Result:=CalculateCRC8(LeftStr(Buffer,Length(Buffer)-1)) = ord(Buffer[Length(Buffer)]);
end;
// Ckeck CRC16, of a string last 2 bytes is CRC, returns true o false
function CheckCRC16(Buffer: String): Boolean;
begin
  if (Length(Buffer) < 3) then  // three bytes min, a byte and 2 bytes CRC
  begin
    Result:=False;
    exit;
  end;

  Result:=CalculateCRC16(LeftStr(Buffer,Length(Buffer)-2)) = Puint16(@Buffer[Length(Buffer)-1])^;
end;
{ AccelStepperFirmata's Custom Float Format

Floats sent and received by accelStepperFirmata are composed of a 23-bit significand (mantissa) and a 4-bit, base 10 exponent (biased -11 with an explicit 1's bit) and a sign bit.
0-20 	21 	22-25 	26-27
least significant bits 	sign 	exponent 	most significant bits
21 bits 	1 bit 	4 bits 	2 bits

Byte 1 	Least most significant bits
Byte 2 	Next most significant bits
Byte 3 	Next most significant bits
Byte 4 	Sign, Exponent and 2 most significant bits}
{AccelStepperFirmata's Custom Float Format

Floats sent and received by accelStepperFirmata are composed of a 23-bit significand (mantissa) and a 4-bit, base 10 exponent (biased -11 with an explicit 1's bit) and a sign bit.
0-20 	21 	22-25 	26-27
least significant bits 	sign 	exponent 	most significant bits
21 bits 	1 bit 	4 bits 	2 bits

These values allow a range from 8.388608*10^-11 to 83886.08. Small enough to represent one step per year and large enough to exceed our max achievable stepper speed.

Example 1: 1 step per hour

1 step per hour = 1 step / 60 minutes / 60 seconds = 0.000277... steps per second

The largest integer that can be represented in 23 bits is 8388608 so the significand will be limited to 6 or 7 digits. In this case 2777777 (note the value truncates).

The exponent is 4 bits which limits the range to 0-15, but we subtract 11 from that value on the receiving end to give us a range from -11 to 4. In this example we are passing 1 to give us a -10 value in the exponent.
	Decimal 	Binary
Significand 	2777777 	01 0101001 1000101 0110001
Exponent 	1 	0001    0001
Sign 	0 	0               0         0101001 1000101 0110001 0 0001 01

Byte 1 	Least most significant bits 	0110001
Byte 2 	Next most significant bits 	1000101
Byte 3 	Next most significant bits 	0101001
Byte 4 	Sign, Exponent and 2 most significant bits 	0000101
Values in firmata are passed in the 7 least significant bits of each message byte so we will be passing in 4 bytes in this order:}
function EncodeAccelFloat(Value: single): String;   // return a 4 bytes string encoded 7 bit
var
  NumberS: string;
  EPos: integer;
  Exponent: integer;
  Mantissa: Integer;
  Sign: Boolean;
begin    // range 8.388608*10^-11 to 8388608
   if (Value <> 0) and ((Abs(Value) > 8388608) or (Abs(Value) < 8.388608E-11)) then
     NumberS:=UpperCase(Format('%e', [0])) // set value to 0 if no valid
   else
     NumberS:=UpperCase(Format('%e', [Value]));
   // E Position
   EPos:=Pos('E', NumberS);
   Sign:=(NumberS[1] = '-'); // True if negative
   // Exponent
   Exponent:=StrToInt(Copy(NumberS, EPos + 1, 4));
   // adjust exponent
   Exponent:=Exponent + 5;
   // Number without sign and without exponent
   if Sign then
      NumberS:=Copy(NumberS, 2, EPos - 3);
   // Number without dot
   NumberS:=NumberS[1] + Copy(NumberS+'000000', 3, 6); // set length to 7 significant digits
   Mantissa:=StrToInt(NumberS);
   // now put in order the 28 bits: byte1 (7 LSB) + byte 2 (next 7 LSB)+ byte3 (next 7 LSB)+ byte 4 (1 sign bit + 4 bits exponent + first 2 + MSB)
   Result:=chr(Mantissa and $7F)+chr((Mantissa >> 7) and $7F)+chr((Mantissa >> 14) and $7F)+chr((ord(sign) << 6) or (Exponent << 2) or ((Mantissa >> 21) and $03));
end;

{
Byte 1 	Least most significant bits 	0110001
Byte 2 	Next most significant bits 	1000101
Byte 3 	Next most significant bits 	0101001
Byte 4 	Sign, Exponent and 2 most significant bits 	0000101
}
function DecodeAccelFloat(Data7Bit: string): single;  // Data7Bit 4 bytes encoded to 7 bits, total 28 bits
var
  Mantissa: integer;
  Exponent: integer;
  sign: Boolean;
begin
  Mantissa:=ord(Data7Bit[1]) or (ord(Data7Bit[2]) << 7) or (ord(Data7Bit[3]) << 14) or ((ord(Data7Bit[4]) and $03) << 21);
  Exponent:=((ord(Data7Bit[4]) >> 2) and $0F) - 11;
  Sign:=Boolean((ord(Data7Bit[4]) >> 6) and $01);
  Result:=Mantissa * power(10, Exponent);
  if Sign then
    Result:=-Result;
end;
//
//
//

{ TBoardThread }
procedure TBoardThread.DoOnTimer;
begin
  if Assigned(FOnTimer) then
    FOnTimer(Self);
end;

procedure TBoardThread.TimeOut;
begin
  if Assigned(FOwner) then
    FOwner.RaiseError(1003, '');
end;

procedure TBoardThread.TimeOutByte;
begin
  if Assigned(FOwner) then
    FOwner.RaiseError(1027, '');
end;

procedure TBoardThread.askProtocol;
begin
  if Assigned(FOwner) then
    FOwner.askProtocol;   // ask for board protocol
end;

procedure TBoardThread.Execute;
begin
  while not Terminated do
  begin
    //Sleep(1);
    if FEnabled and (TThread.GetTickCount64 - FTime > FInterval) then
    begin
      FTime:=TThread.GetTickCount64;
      Synchronize({$IFDEF OBJFPC}@{$ENDIF}DoOnTimer);
    end;
    GetCommand;
    if FStarting then
    begin
      if (TThread.GetTickCount64 - FInitTime) > FMaxTime then
      begin
        Synchronize({$IFDEF OBJFPC}@{$ENDIF}TimeOut);
        Terminate;
      end
      else if ((TThread.GetTickCount64 - FInitTime) > 1000) and FFirstTry then
      begin
        FFirstTry:=False;
        Synchronize({$IFDEF OBJFPC}@{$ENDIF}askProtocol);
      end;
    end
    else if not FEndLastCommand and ((TThread.GetTickCount64 - FByteTimeOut) > 2000) then
    begin
      Synchronize({$IFDEF OBJFPC}@{$ENDIF}TimeOutByte);
      Terminate;
    end;
  end;
end;

procedure TBoardThread.ParseSysExCommand;
begin
  FOwner.ParseSysExCommand(FCommandData);
  FStarting:=Owner.FStarting;
end;

procedure TBoardThread.ParseCommand;
begin
  FOwner.ParseCommand(FCommandData);
end;

procedure TBoardThread.GetCommand;
var
  i: integer;
begin
  Synchronize({$IFDEF OBJFPC}@{$ENDIF}UpdateVars);
  while FCommandBuffer <> '' do
  begin
    if FEndLastCommand then
      FByteTimeOut:=TThread.GetTickCount64;  // for data inside a command

    FCommandData:='';   // new command data
    FEndLastCommand:=false;

    case ord(FCommandBuffer[1]) of
      START_SYSEX: begin  // 0xF0
        i:=Pos(chr(END_SYSEX), FCommandBuffer);
        if i > 0 then   // found END_SYSEX, so complete sysex command received
        begin
          FCommandData:=Copy(FCommandBuffer, 2, i-2); // not include START_SYSEX nor END_SYEX
          Delete(FCommandBuffer, 1, i); // delete sysex command from buffer
          FEndLastCommand:=true;
          if not FStarting or (ord(FCommandData[1]) in [REPORT_FIRMWARE, CAPABILITY_RESPONSE, STRING_DATA]) then
            Synchronize({$IFDEF OBJFPC}@{$ENDIF}ParseSysExCommand); // not send START_SYSEX nor END_SYEX     // only can process these commands in starting mode
        end
        else  // no complete sysex command received, so roll back and wait for more data
          exit;
     end;
     $90 .. $9F, $E0 .. $EF :
       if length(FCommandBuffer) > 2 then
       begin
         FCommandData:=Copy(FCommandBuffer, 1, 3);
         //FCommandBuffer:=Copy(FCommandBuffer, 4, Length(FCommandBuffer) - 3);
         Delete(FCommandBuffer, 1, 3); // delete command from buffer
         FEndLastCommand:=true;
         if FStarting then
           exit; // can't proccess this command in starting mode
         Synchronize({$IFDEF OBJFPC}@{$ENDIF}ParseCommand);
       end
       else // no complete command received, so roll back and wait for more data
         exit;
     REPORT_PROTOCOL :  // same as REPORT_VERSION
       if length(FCommandBuffer) > 2 then  // complete command received
       begin
         FCommandData:=Copy(FCommandBuffer, 1, 3);
         //FCommandBuffer:=Copy(FCommandBuffer, 4, Length(FCommandBuffer) - 3);
         Delete(FCommandBuffer, 1, 3);  // delete command from buffer
         FEndLastCommand:=true;
         Synchronize({$IFDEF OBJFPC}@{$ENDIF}ParseCommand);
       end
       else  // no complete command received, so roll back and wait for more data
         exit;
     else  // bad data, remove it
       Delete(FCommandBuffer, 1, 1);
       FEndLastCommand:=true;
    end;
  end;
end;

procedure TBoardThread.UpdateVars;   // Update vars on Thread from Board
begin
  FCommandBuffer:=FCommandBuffer + Owner.FCommandBuffer;
  Owner.FCommandBuffer:='';
  FStarting:=Owner.FStarting;
end;

procedure TBoardThread.StopTimer;
begin
  FEnabled:=False;
end;

procedure TBoardThread.StartTimer;
begin
  FTime:=TThread.GetTickCount64;
  FEnabled:=True;
  if Self.Suspended then
    Start;
end;

constructor TBoardThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FInterval:=1000;
  FreeOnTerminate:=True;
  FEnabled:=False;
  FOwner:=nil;
  FMaxTime:=6000;
  FStarting:=True;
  FFirstTry:=true;
  FEndLastCommand:=true;
end;

destructor TBoardThread.Destroy;
begin
  inherited Destroy;
end;


{ TBoard }

constructor TBoard.Create(AOwner: TComponent);
begin
  inherited;

  FBoardThread:=nil;
  FStarting:=True;
  FEnabled:=False;
  FOnBeforeOpen:=nil;
  FOnAfterClose:=nil;
  FOnBoardReady:=nil;
  FOnSendDataToDevice:=nil;
  FOnQueryAllTask:=nil;
  FOnError:=nil;
  FonTimer:=nil;
  FTimerInterval:=0;
  FOnBoardData:=nil;
  FOnPinValue:=nil;
  FOnPinState:=nil;
  FSamplingInterval:=19;  // 19 milisec default arduino sampling interval
  FBoardPinsNumber:=0;
  FAnalogPinsNumber:=0;
  FGotCapabilities:=False;
  FGotFirmware:=False;
  FGotProtocol:=False;
  FMaxTime:=6000; // default max time to wait for firmata to start
  FStartingTime:=0;
  FBoardStringProtocol:='';
  FBoardStringFirmware:='';
end;

destructor TBoard.Destroy();
begin
  inherited Destroy;
end;

procedure TBoard.CloseApp;
begin
  Close;
  TForm(Owner).Close;
end;

function TBoard.GetBoardPin(Index: Integer): TBoardPin;
begin
  if (Index < FBoardPinsNumber) and (Index >= 0) then
     Result:=FBoardPins[Index]
  else
     Result:=default(TBoardPin);
end;

procedure TBoard.SetBoardPin(Index: integer; Pin: TBoardPin);
begin
  if (Index < FBoardPinsNumber) and (Index >= 0) then
     FBoardPins[Index]:=Pin;
end;

procedure TBoard.SetSamplingInterval(Interval: integer);
begin
  if Interval=FSamplingInterval then
    exit;
  if (Interval < 1) or (Interval > $3FFF) then
    exit;
  FSamplingInterval:=Interval;
  if FEnabled then
    SendSamplingInterval(true);
end;

function TBoard.SendSamplingInterval(write: Boolean=true): string;
begin
  Result:='';
  if FEnabled then
    Result:=SendSysEx(chr(SAMPLING_INTERVAL)+chr(FSamplingInterval and $7F)+chr((FSamplingInterval >> 7) and $7F), write);
end;

procedure TBoard.setMaxTime(TimeToWait: Qword);  // milisec to wait for firmata
begin
  if TimeToWait < 1800 then  // min time to wait is about 1800 milisec
    exit
  else
    FMaxTime:=TimeToWait;
end;

procedure TBoard.setEnabled(State: Boolean);
begin
  if FEnabled = State then
     exit;

  if State then
     Open
  else
     Close;
end;

procedure TBoard.Open;
begin
  if FEnabled then   //already open
     exit;

  initBoardVariables;

  FEnabled:=true;

  if Assigned(FOnBeforeOpen) then
    FOnBeforeOpen(self);

  if not FEnabled then
    exit;

  if not Assigned(FOnSendDataToDevice) then
    RaiseError(1001, 'Board:Open');

  // Launch Thread
  FBoardThread:=TBoardThread.Create(true);
  FBoardThread.Owner:=Self;
  FBoardThread.FMaxTime:=FMaxTime;
  FBoardThread.FByteTimeOut:=0;
  FBoardThread.FStarting:=True;

  FBoardThread.FCommandData:='';
  FBoardThread.FCommandBuffer:='';
  FBoardThread.FEndLastCommand:=true;
  FBoardThread.FOnTimer:={$IFDEF OBJFPC}@{$ENDIF}FOnTimer;
  FBoardThread.FInterval:=FTimerInterval;
  FBoardThread.FInitTime:=TThread.GetTickCount64;

  FInitTime:=FBoardThread.FInitTime;

  FBoardThread.Start;  //launch thread
  //FBoardThread.StartTimer;  //launch thread timer, so Start Thread if necessary
end;

procedure TBoard.InternalBoardReady;
var
  i: integer;
begin
 // initialize module variables
  SetLength(FPins, FBoardPinsNumber);

  for i:=0 to 127 do  // Max tasks 127
    FTasks[i]:=nil;

  FI2C:=nil;     // in this moment only one module

  SetLength(FOneWires, FBoardPinsNumber);  // all pins can be used by a onewire module
  for i:=0 to FBoardPinsNumber - 1 do
    FOneWires[i]:=nil;

  for i:=0 to MAX_SPI_DEVICES - 1 do
    FSPIs[i]:=nil;

  for i:=0 to MAX_ACCELSTEPPER_DEVICES - 1 do
    FAccelSteppers[i]:=nil;

  FAccelStepperGroup:=nil;

  for i:=0 to MAX_SERIAL_PORTS - 1 do
    FSerials[ByteToSerialPorts(i)]:=nil;

  for i:=0 to MAX_SERVOS - 1 do
    FServos[i]:=nil;

  for i:=0 to MAX_ENCODERS - 1 do
    FEncoders[i]:=nil;

  for i:=0 to MAX_FREQUENCIES - 1 do
    FFrequencies[i]:=nil;

  // all pins can be used for DHT
  SetLength(FDHTs, FBoardPinsNumber);  // all pins can be used by a DHT module
  for i:=0 to FBoardPinsNumber - 1 do
    FDHTs[i]:=nil;

  for i:=0 to MAX_MICE - 1 do
    FMice[i]:=nil;

  for i:=0 to MAX_NEOPIXELS - 1 do
    FNeoPixels[i]:=nil;
end;

procedure TBoard.DisableModules;
var
  i: integer;
begin
   // disable neopixels
   for i:=0 to MAX_NEOPIXELS - 1 do
     if Assigned(FNeoPixels[i]) and  FNeoPixels[i].FEnabled then
        FNeoPixels[i]:=nil;

   // disable mice
   for i:=0 to MAX_MICE - 1 do
     if Assigned(FMice[i]) and  FMice[i].FEnabled then
        FMice[i].setEnabled(false);

   // disable DHTs
    // disable Onewire modules
   for i:=0 to FBoardPinsNumber - 1 do
     if Assigned(FDHTs[i]) and FDHTs[i].FEnabled then
       FDHTs[i].setEnabled(false);

   // disable Frecuencies
   for i:=0 to MAX_FREQUENCIES - 1 do
     if Assigned(FFrequencies[i]) and FFrequencies[i].FEnabled then
       FFrequencies[i].setEnabled(false);

   // disable servos
   for i:=0 to MAX_SERVOS - 1 do
     if Assigned(FServos[i]) and FServos[i].FEnabled then
       FServos[i].setEnabled(false);

   // disable encoders
   for i:=0 to MAX_ENCODERS - 1 do
     if Assigned(FEncoders[i]) and FEncoders[i].FEnabled then
       FEncoders[i].setEnabled(false);

   // disable AccelStepperGroup
   if Assigned(FAccelStepperGroup) and FAccelStepperGroup.FEnabled then
     FAccelStepperGroup.setEnabled(False);

   // disable AccelSteppers
   for i:=0 to MAX_ACCELSTEPPER_DEVICES - 1 do
     if Assigned(FAccelSteppers[i]) and FAccelSteppers[i].FEnabled then
       FAccelSteppers[i].setEnabled(False);

   // disable I2C
   if Assigned(FI2C) and FI2C.FEnabled then
     FI2C.setEnabled(False);

   // disable SPI
   for i:=0 to MAX_SPI_DEVICES - 1 do
     if Assigned(FSPIs[i]) and FSPIs[i].FEnabled then
       FSPIs[i].setEnabled(False);

   // disable Onewire modules
   for i:=0 to FBoardPinsNumber - 1 do
     if Assigned(FOneWires[i]) and FOneWires[i].FEnabled then
       FOneWires[i].setEnabled(false);

   // disable tasks
   for i:=0 to 127 do
     if Assigned(FTasks[i]) and FTasks[i].FEnabled then
       FTasks[i].setEnabled(False);

   // disable serial ports
   for i:=0 to MAX_SERIAL_PORTS - 1 do
     if Assigned(FSerials[ByteToSerialPorts(i)]) and FSerials[ByteToSerialPorts(i)].FEnabled then
       FSerials[ByteToSerialPorts(i)].setEnabled(False);

   // disable pins
   for i:=0 to FBoardPinsNumber - 1 do
     if Assigned(FPins[i]) and FPins[i].FEnabled then
       FPins[i].setEnabled(False);

end;

procedure TBoard.Close;
begin
  FEnabled:=False;

  if Assigned(FOnAfterClose) then
    FOnAfterClose(self);

  DisableModules;

  // stop and terminate thread
  if FBoardThread <> nil then
  begin
    FBoardThread.FreeOnTerminate:=True;
    FBoardThread.Terminate;
    FBoardThread:=nil;
  end;
end;

function TBoard.askProtocol(write: Boolean=true): string;
begin
  Result:=SendCommand(chr(REPORT_VERSION), write);
end;

function TBoard.askFirmware(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(REPORT_FIRMWARE), write);
end;

function TBoard.askBoardCapabilities(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(CAPABILITY_QUERY), write);
end;

procedure TBoard.initBoardVariables;
begin
  FLastError:=0;
  FBoardPinsNumber:=0;
  FAnalogPinsNumber:=0;
  Setlength(FBoardPins,0);
  FCommandBuffer:='';
  FBoardStringProtocol:='';
  FBoardStringFirmware:='';
  FGotProtocol:=False;
  FGotFirmware:=False;
  FGotCapabilities:=False;
  FStarting:=True;
  FTimerInterval:=0;
end;

procedure TBoard.RaiseError(Error: integer; FunctionError: string);
var
  ErrorString: string;
begin
  FLastError:=Error;

  ErrorString:=Format('Error (%d): %s',[Error, ErrorsArray[Error mod 1000]]);

  if FunctionError <> '' then
    ErrorString:=ErrorString+' at '+FunctionError;

  if Assigned(FOnError) then
    FOnError(self, Error, ErrorString);

  if Error > 1000 then   // irrecuperable error
    CloseApp;
end;

procedure TBoard.Reset;
begin
   Close;
   Open;
end;

procedure TBoard.ParseData(Data: string);
begin
  FCommandBuffer:=FCommandBuffer+Data;
end;

procedure TBoard.parseCommand(Data: string);
var
  Pin: Byte;
  Value: integer;
  BitValue: integer;
  mask: Byte;
  Port: Byte;
begin
  case ord(Data[1]) of
      {0  digital data, 0x90-0x9F, second nibble of byte 0 gives the port number (e.g. 0x92 is the third port, port 2)
      1  digital pins 0-6 bitmask
      2  digital pin 7-13 bitmask}
    $90 .. $9F: begin  // digital I/O message
      {port = Command; port_data = data[self.LSB] + (data[self.MSB] << 7)
      set all the pins for this reporting port get the first pin number for this report, pin = port * PorTBoardPins}
      mask:=1;
      Port:= ord(Data[1]) and $0F; // port number
      Value:=(ord(Data[2]) and $7F) or ((ord(Data[3]) << 7) and $7F);   // get digital values

      for Pin:=Port * 8 to Port * 8 + 7 do
      begin
        if FEnabled and (Pin < Length(FPins)) and Assigned(FPins[Pin]) and FPins[Pin].Enabled and FPins[Pin].FReporting then  // report on pin is enabled
        begin
          BitValue:=ord((Value and mask) > 0);
          FPins[Pin].FValue:=BitValue;
          if Assigned(FPins[Pin].FOnPinValue) then  // Report has to be true
            FPins[Pin].FOnPinValue(self, BitValue);
        end;
        mask:=mask << 1; // next pin mask
      end;
    end;
    {0  analog pin, 0xE0-0xEF,
    1  analog least significant 7 bits
    2  analog most significant 7 bits}
    $E0 .. $EF: begin  // analog I/O message ,    AnalogPin:= ReadByte and $0F; // Channel number
      Pin:=GetPinFromAnalogPin(ord(Data[1]) and $0F);  // get pin from analogpin
      Value:=ord(Data[2]) or (ord(Data[3]) << 7);
      if FEnabled and Assigned(FPins[Pin]) and FPins[Pin].FEnabled then
        FPins[Pin].GetAnalogMessage(Value);
    end;
    {0  version report header (0xF9)
    1  major protocol version (0-127)
    2  minor protocol version (0-127)}
    REPORT_PROTOCOL: begin // Protocol version
      // data[2] = major version
      // data[3] = mainor version
      FGotProtocol:=true;

      FBoardstringProtocol:=IntToStr(ord(Data[2]))+'.'+IntToStr(ord(Data[3]));
      if FStarting then  // to complete init procedure
        askFirmware;     // Don't know the reason why board does not send this
    end;
  end;
end;

procedure TBoard.ParseSysExCommand(Data: string);
var
  Pin: byte;
  AnalogPin: byte;
  Value: integer;
  Mode: Byte;
  DataString: string;
  Device: byte; // accelstepper or SPI device
  Port: Byte;
  i: integer;
  TaskID: Byte;
  TaskIDs: array of Byte;
  EncoderData: string;
begin
   DataString:='';
   case ord(Data[1]) of
        { 0  START_SYSEX       (0xF0)
        1 EXTENDED_SYSEX       (0x00)
        2 EXTENTED_ID_0         byte0
        3 EXTENDED_ID_1         byte1
        ... PAYLOAD             nbytes
        N  END_SYSEX           (0xF7)}
        EXTENDED_SYSEX: begin // 0x00  not yet supported
           // TODO more research
           DataString:=Copy(Data, 2, Length(Data) - 1); // excludes EXTENDED_SYEX
           if Assigned(FOnExtendedSysex) then
             FOnExtendedSysex(self, DataString);
           if Assigned(FOnExtendedSysex) then
             FOnExtendedSysex(self, DataString);
        end;
        {0  START_SYSEX       (0xF0)
        1  REPORT_VERSION     (0x79)
        2  major version     (0-127)
        3  minor version     (0-127)
        4  first char of firmware name (LSB)
        5  first char of firmware name (MSB)
        6  second char of firmware name (LSB)
        7  second char of firmware name (MSB)
        ... for as many bytes as it needs
        N  END_SYSEX         (0xF7)}
        REPORT_FIRMWARE: begin // 0x79
          // data[2] = major version
          // data[3] = mainor version
           DataString:=Copy(Data, 4, Length(Data) - 3); // excludes 3 first bytes
           DataString:=Decode2BytesCharTo1(DataString);  // converts 2 bytes char to 1 byte char
           FBoardType:=DataString;
           FBoardStringFirmware:=IntToStr(ord(Data[2]))+'.'+IntToStr(ord(Data[3]));

           if not FGotcapabilities and not FGotFirmware then
             askBoardCapabilities;
           FGotFirmware:=True;
        end;
        {0  START_SYSEX              (0xF0)
        1  CAPABILITY_RESPONSE       (0x6C)
        2  1st supported mode of pin 0
        3  1st mode's resolution of pin 0
        4  2nd supported mode of pin 0
        5  2nd mode's resolution of pin 0
        ... additional modes/resolutions, followed by `0x7F`,
        to mark the end of the pin's modes. Subsequently, each pin
        follows with its modes/resolutions and `0x7F`,
        until all pins are defined.
        N  END_SYSEX                (0xF7)}
        CAPABILITY_RESPONSE: begin // 0x6C;
           Pin:=0;
           AnalogPin:=0;
           i:=2;   //  i:=1   exclude CAPABILITY_RESPONSE       (0x6C)
           SetLength(FBoardPins,0); // clear FBoardPins
           while i <= Length(Data) do
           begin
              // New pin element
              Setlength(FBoardPins,Pin+1);  // increment size of array of pins
              FBoardPins[pin].Busy:=false;  // Pin no assigned to any module
              FBoardPins[pin].AnalogMap:=PinModesToByte(PIN_MODE_IGNORE); // default no analog pin map
              FBoardPins[pin].ActualMode:=PinModesToByte(PIN_MODE_OUTPUT);

              if ord(Data[i]) = PinModesToByte(PIN_MODE_IGNORE) then
                FBoardPins[pin].ActualMode:=ord(Data[i]) // if first mode is PIN_MODE_IGNORE then pin must be ignored
              else
                while ord(Data[i]) <> PinModesToByte(PIN_MODE_IGNORE) do
                begin
                   // New pin capability
                   SetLength(FBoardPins[pin].Capabilities, Length(FBoardPins[Pin].Capabilities) + 1);
                   FBoardPins[Pin].Capabilities[Length(FBoardPins[Pin].Capabilities) - 1].mode:=ord(Data[i]);
                   if ord(Data[i]) = PinModesToByte(PIN_MODE_ANALOG) then  // new analog pin discovered
                   begin
                     FBoardPins[pin].AnalogMap:=AnalogPin;  // map analog Pin
                     inc(AnalogPin);
                     FBoardPins[pin].ActualMode:=ord(Data[i]); // default for anlog pin is analog mode
                   end;
                   inc(i);  // read resolution for other than analogic mode
                   FBoardPins[Pin].Capabilities[Length(FBoardPins[Pin].Capabilities) - 1].Resolution:=ord(Data[i]); // store Resolution
                   inc(i);    // read next capability
                end; // end pin
              inc(Pin);    // next pin number
              inc(i);
           end; // end all Pins
           FBoardPinsNumber:=Pin;  // discovered pins
           FAnalogPinsNumber:=AnalogPin;  // discovered analog pins
           FGotCapabilities:=true;
           if FGotProtocol and FGotFirmware then //is from start or reset
           begin
             FStarting:=False;
             InternalBoardReady; // Initializa module variables
             SendSamplingInterval;  // SamplingInterval default to 19 ms
             FStartingTime:=TThread.GetTickCount64 - FInitTime;
             if Assigned(FOnBoardReady) then
               FOnBoardReady(self);
           end;
        end;
        {0  START_SYSEX              (0xF0)
        1  ANALOG_MAPPING_RESPONSE   (0x6A)
        2  analog channel corresponding to pin 0, or 127 if pin 0 does not support analog
        3  analog channel corresponding to pin 1, or 127 if pin 1 does not support analog
        4  analog channel corresponding to pin 2, or 127 if pin 2 does not support analog
        ... etc, one byte for each pin
        N  END_SYSEX                (0xF7)}
        ANALOG_MAPPING_RESPONSE: begin // $6A
          if FStarting then // avoid problems with some firmatas
            exit
          else // Firmata has already started, so normal behavior
          begin
            i:=2;
            Pin:=0;
            while i <= Length(Data) do
            begin
              FBoardPins[Pin].AnalogMap:=ord(Data[i]);  // Pin map
              Inc(Pin);  // next pin
              inc(i);
            end;
          end;
        end;
        {0  START_SYSEX              (0xF0)
         1  PIN_STATE_RESPONSE       (0x6E)
         2  pin                      (0-127)
         3  pin mode (the currently configured mode)
         4  pin state, bits 0-6
         5  (optional) pin state, bits 7-13
         6  (optional) pin state, bits 14-20
         ... additional optional bytes, as many as needed
         N  END_SYSEX                (0xF7)}
        PIN_STATE_RESPONSE: begin // $6E  max 64 bits
          Pin:=ord(Data[2]);
          Mode:=ord(Data[3]);
          DataString:=Copy(Data, 4, Length(Data) - 3);

          Value:=Decode32BitSignedInt(DataString);  // converts to int

          if Assigned(FPins[Pin]) and FPins[Pin].Enabled then
          begin
            FPins[Pin].FMode:=ByteToPinModes(Mode);
            FBoardPins[Pin].ActualMode:=Mode;
            if Assigned(FPins[Pin].FOnPinState) then
              FPins[Pin].FOnPinState(self, FPins[Pin].FMode, Value);
          end;
        end;
        {0  START_SYSEX              (0xF0)
        1  EXTENDED_ANALOG           (0x6F)
        2  pin                      (0-127)
        3  bits 0-6                 (least significant byte)
        4  bits 7-13                (most significant byte)
        ... additional bytes may be sent if more bits are needed
        N  END_SYSEX                (0xF7)}
        EXTENDED_ANALOG: begin //  $6F
          Pin:=ord(Data[2]);
          DataString:=Copy(Data, 3, Length(Data) - 2);
          Value:=Decode32BitSignedInt(DataString);  // converts to int

          if Assigned(FPins[Pin]) and FPins[Pin].Enabled then
          begin
            FPins[Pin].FValue:=Value;
            if Assigned(FPins[Pin].FOnPinValue) then
              FPins[Pin].FOnPinValue(self, Value);
          end;
        end;
        {0  START_SYSEX        (0xF0)
        1  STRING_DATA        (0x71)
        2  first char LSB
        3  first char MSB
        4  second char LSB
        5  second char MSB
        ... additional bytes up to half the buffer size - 3 (START_SYSEX, STRING_DATA, END_SYSEX)
        N  END_SYSEX          (0xF7)}
        STRING_DATA: begin //  $71
          DataString:=Copy(Data, 2, Length(Data) - 1);
          DataString:=Decode2BytesCharTo1(DataString);
          if Assigned(FOnBoardData) then
            FOnBoardData(self, STRING_DATA, DataString);
        end;
        {0  START_SYSEX        (0xF0)
        1  SERIAL_DATA      (0x60)
        2  SERIAL_REPLY       (0x40) // OR with port (0x41 = SERIAL_REPLY | HW_SERIAL1)
        3  data 0             (LSB)
        4  data 0             (MSB)
        3  data 1             (LSB)
        4  data 1             (MSB)
        ...                   //up to max buffer - 5
        n  END_SYSEX          (0xF7)}
        SERIAL_DATA: begin // $60
          Port:=ord(Data[2]);  // get port
          DataString:=Copy(Data, 3, Length(Data) - 2);
          DataString:=Decode2BytesCharTo1(DataString);

          if (Port and $FC) <> SERIAL_REPLY then // error
            RaiseError(15, 'Board:ParseSysexCommand, Serial Message Response')
          else
            Port:=Port and $0F;  // get real port

          if Assigned(FSerials[ByteToSerialPorts(Port)]) and FSerials[ByteToSerialPorts(Port)].Enabled then
          begin
             if Assigned(FSerials[ByteToSerialPorts(Port)].FOnSerialMessage) then
               FSerials[ByteToSerialPorts(Port)].FOnSerialMessage(self, DataString);
          end;
        end;
        {0  START_SYSEX              (0xF0)
         1  Scheduler Command        (0x7B)
         2  error_task Reply Command (0x08) or query_task Reply Command (0x0A)
         3  task id                  (0-127)
         4  time_ms bit 0-6
         5  time_ms bit 7-13
         6  time_ms bit 14-20
         7  time_ms bit 21-27
         8  time_ms bit 28-31 | (length bit 0-2) << 4
         9  length bit 3-9
         10 length bit 10-15 | (position bit 0) << 7
         11 position bit 1-7
         12 position bit 8-14
         13 position bit 15 | taskdata bit 0-5 << 1 [taskdata is optional]
         14 taskdata bit 6-12  [optional]
         15 taskdata bit 13-19 [optional]
         n  ... as many bytes as needed (don't exceed MAX_DATA_BYTES though)
         n+1  END_SYSEX              (0xF7) }
        SCHEDULER_DATA: begin // scheduler $7B
          case ord(Data[2]) of  // first byte is Error_task or query_task
            ERROR_FIRMATA_TASK, QUERY_TASK_REPLY: begin
              DataString:=Copy(Data, 2, Length(Data) - 1); // include Data[2], excludes SCHEDULER_DATA
              // get TaskID
              TaskID:=ord(Data[3]);
              if Assigned(FTasks[TaskID]) and FTasks[TaskID].Enabled then
                FTasks[TaskID].parsefirmatacommand(Self, DataString);
            end;
            {0  START_SYSEX          (0xF0)
             1  Scheduler Command    (0x7B)
             2  query_all_tasks Reply Command (0x09)
             3  taskid_1             (0-127) [optional]
             4  taskid_2             (0-127) [optional]
             n  ... as many bytes as needed (don't exceed MAX_DATA_BYTES though)
             n+1  END_SYSEX (0xF7)}
            QUERY_ALL_TASKS_REPLY: begin // query_all_tasks Reply Command (0x09)
              SetLength(TaskIDs,0);
              i:=2;
              while i <= Length(Data) do
              begin
                Setlength(TaskIDs, length(TaskIDs)+1);   // new task
                TaskIDs[length(TaskIDs)-1]:=ord(Data[i]);
                inc(i);
              end;
              if (Length(FTasks) > 0) and Assigned(FOnQueryAllTask) then  // al least One task exists
                FOnQueryAllTask(self, TaskIDs);
            end;
            else
            begin  // unknown Scheduler command, discard command
              RaiseError(4, 'Board:ParseSysexCommand, GetSchedulerCommands');
            end;
          end;
        end;
        I2C_REPLY: begin // $77
          DataString:=Copy(Data, 2, Length(Data) - 1);
          if Assigned(FI2C) and FI2C.Enabled then
            FI2C.parsefirmatacommand(Self, DataString);
        end;
        SPI_DATA: begin // $68
          DataString:=Copy(Data, 2, Length(Data) - 1);
          // first byte is subcommand
          // second byte is DeviceId and Channel
          Device:=(ord(DataString[2]) and $78) >> 3; // extract Device ID
          if Assigned(FSPIs[Device]) and FSPIs[Device].Enabled then
            FSPIs[Device].parsefirmatacommand(Self, DataString);
        end;
        ONEWIRE_DATA: begin //  $73;
          DataString:=Copy(Data, 2, Length(Data) - 1);
          Pin:=ord(DataString[2]);  // second byte
          if Assigned(FOneWires[Pin]) and FOneWires[Pin].Enabled then
            FOneWires[Pin].parsefirmatacommand(Self, DataString);
        end;
        ACCELSTEPPER_DATA: begin  // $62
          DataString:=Copy(Data, 2, Length(Data) - 1);
          case ord(Data[2]) of
            {0  START_SYSEX                             (0xF0)
             1  ACCELSTEPPER_DATA                       (0x62)
             2  ACCELSTEPPER_REPORT_POSITION and ACCELSTEPPER_MOVE_COMPLETED   (0x06) and (0x0b)
             3  device number                           (0-9)
             4  position, bits 0-6
             5  position, bits 7-13
             6  position, bits 14-20
             7  position, bits 21-27
             8  position, bits 28-31
             9  END_SYSEX                               (0xF7)}
             ACCELSTEPPER_REPORT_POSITION, ACCELSTEPPER_MOVE_COMPLETED: begin  // only one accelstepper
               // Device is second byte
               Device:=ord(DataString[2]);
               if Assigned(FAccelSteppers[Device]) and FAccelSteppers[Device].Enabled then
                  FAccelSteppers[Device].parsefirmatacommand(Self, DataString);
             end;
             {0  START_SYSEX                             (0xF0)
             1  ACCELSTEPPER_DATA                       (0x62)
             2  ACCELSTEPPER_MULTI_MOVE_COMPLETED      (0x24)
             3  group  number                           (0-4)
             4  END_SYSEX(0xF7)}
             ACCELSTEPPER_MULTI_MOVE_COMPLETED: begin  // group of accelstepper
               if Assigned(FAccelStepperGroup) and FAccelStepperGroup.Enabled then
                 FAccelStepperGroup.parsefirmatacommand(Self, DataString);
            end
            else  // subcommand unknown
            begin
              RaiseError(4, 'Board:ParseSysexCommand, AccelStepper Unknown Command');
            end;
          end;
        end;
        {0 START_SYSEX                (0xF0)
        * 1 ENCODER_DATA               (0x61)
        * 2 first encoder #  &  DIRECTION    [= (direction << 6) | (#)]
        * 3 first encoder position, bits 0-6
        * 4 first encoder position, bits 7-13
        * 5 first encoder position, bits 14-20
        * 6 first encoder position, bits 21-27
        * 7  END_SYSEX or second encoder #  &  DIRECTION    [= (direction << 6) | (#)]
        * 8 second encoder position, bits 0-6
        * ...
        * N END_SYSEX                  (0xF7) }
        ENCODER_DATA: begin   // $061
          DataString:=Copy(Data, 2, Length(Data) - 1);
          for i:=1 to (Length(DataString) div 5) do // length data div encoders number
          begin
            EncoderData:=Copy(DataString, (i - 1) * 5, 5);
            Device:=ord(EncoderData[1]) and ENCODER_MASK;  // first byte

            if Assigned(FEncoders[Device]) and FEncoders[Device].Enabled then
              FEncoders[Device].parsefirmatacommand(Self, EncoderData);
          end;
        end;
        {0  START_SYSEX      (0xF0)
        1 FREQUENCY_COMMANDD     Command (0x7D)
        2 FREQUENCY_SUBCOMMAND_REPORT (0x02)
        3 pin                        (0-127)
        4 Time of measurement        (32 bits as 5 bytes) in milliseconds
        5 Current number of ticks    (32 bits as 5 bytes)
        N END_SYSEX                  (0xF7)}
        FREQUENCY_COMMAND: begin // $7D
          if ord(Data[2]) <> FREQUENCY_SUBCOMMAND_REPORT then
          begin
            RaiseError(4, 'Board:ParseSysexCommand, Frequency Unknown Command Data');
            exit;
          end;
          Device:=ord(Data[3]);
          DataString:=Copy(Data, 3, Length(Data) - 2);
          if Assigned(FFrequencies[Device]) and FFrequencies[Device].Enabled then
            FFrequencies[Device].parsefirmatacommand(Self, DataString);
        end;
        {0 START_SYSEX                (0xF0)
        1 DHTSENSOR_DATA             (0x74)
        2 DHTSENSOR_RESPONSE         (0x00)
        3 pin                        (0-127)
        4 temperature, bits 0-6      (lsb - temperature in Celsius * 10)
        5 temperature, bits 7-13     (msb - temperature in Celsius * 10)
        6 humidity, bits 0-6         (lsb - relative humidity in % * 10)
        7 humidity, bits 7-13        (msb - relative humidity in % * 10)
        N END_SYSEX   (0xF7)}
        DHTSENSOR_DATA: begin
          DataString:=Copy(Data, 3, Length(Data) - 2);
          Pin:=ord(Data[3]);
          if Assigned(FDHTs[Pin]) and FDHTs[Pin].Enabled then
            FDHTs[Pin].parsefirmatacommand(Self, DataString);
        end;
        PS2MOUSE_DATA: begin // 0x50
          // first byte subcommand
          // second byte device
          DataString:=Copy(Data, 2, Length(Data) - 1);
          Device:=ord(DataString[2]);
          if Assigned(FMice[Device]) and FMice[Device].Enabled then
              FMice[Device].parsefirmatacommand(Self, DataString);
        end;
         {0  START_SYSEX      (0xF0)
         1  NEOPIXEL_DATA    (0X51)
         2  NEOPIXEL_RUN_PAUSE  (0X04)
         3  deviceNum (0-3)
         4  END_SYSEX        (0xF7) }
        NEOPIXEL_DATA: begin // 0x51
          // first byte subcommand
          // second byte device
          if ord(Data[2]) <> NEOPIXEL_FADE_RUN_PAUSE then
          begin
            RaiseError(4, 'Board:ParseSysexCommand, Neopixel Unknown Command Data');
            exit;
          end;
          Device:=ord(Data[3]);
          if Assigned(FNeoPixels[Device]) and FNeoPixels[Device].Enabled then
            FNeoPixels[Device].parsefirmatacommand(Self, Data[3]);
        end;
   end;
end;

function TBoard.GetPinFromAnalogPin(AnalogPin: byte): Byte;
var
  i: integer;
begin
  Result:=$FF;
  if AnalogPin > FAnalogPinsNumber then
    exit;
  for i:=0 to Length(FBoardPins) - 1 do
  begin
    if FBoardPins[i].AnalogMap = AnalogPin then
    begin
      Result:=i;
      Break;
    end;
  end;
end;

// check if supported capability
function TBoard.CheckCapability(Pin: byte; Mode: TPinModes): Boolean;
var
  i: integer;
  ModeValue: Byte;
begin
  Result:=False;
  ModeValue:=PinModesToByte(Mode);

  if Pin > FBoardPinsNumber then
    exit
  else
    for i:=0 to length(FBoardPins[Pin].Capabilities)-1 do
    begin
      if FBoardPins[Pin].Capabilities[i].Mode = ModeValue then
      begin
        Result:=True;
        break;
      end;
    end;
end;
// return true if there is another digital pin in the port reporting
function TBoard.CheckReportPort(Pin: Byte): Boolean;
var
  i: integer;
  Port: Byte;
begin
  Result:=False;

  Port:=Pin div 8;

  for i:=Port * 8 to Port * 8 + 7 do // 8 pins in port
    if (i < Length(FPins)) and (i <> Pin) and Assigned(FPins[i]) and FPins[i].FEnabled and FPins[i].FReporting then
    begin
      Result:=True;
      break;
    end;
end;
// get resolution of pin
function TBoard.GetPinResolution(Pin: Byte; Mode: TPinModes): Integer;
var
  i: integer;
  NotFound: Boolean;
  ModeValue: Byte;
begin
  Result:=0;
  NotFound:=true;

  ModeValue:=PinModesToByte(Mode);

  for i:=0 to length(FBoardPins[Pin].Capabilities)-1 do
  begin
    if FBoardPins[Pin].Capabilities[i].Mode = ModeValue then
    begin
      Result:=(1 << FBoardPins[Pin].Capabilities[i].Resolution) - 1; // get resolution
      NotFound:=false;
      break;
    end;
  end;
  if NotFound then
    RaiseError(34, 'Board:GetPinResolution, GetPinResolution');
end;

{0  START_SYSEX              (0xF0)
1  Scheduler Command        (0x7B)
2  query_all_tasks command  (0x05)
3  END_SYSEX                (0xF7) }
function TBoard.QueryAllTasks(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(SCHEDULER_DATA)+chr(QUERY_ALL_TASKS), write);
end;
{0  START_SYSEX              (0xF0)
 1  Scheduler Command        (0x7B)
 2  scheduler reset command  (0x07)
 3  END_SYSEX                (0xF7)}
function TBoard.SchedulerReset(write: Boolean=true): string;
var
  i: integer;
begin
  if write then
    for i:=0 to 127 do
      if Assigned(FTasks[i]) then
        FTasks[i].setEnabled(False); // disable FTasks[i]

  Result:=SendSysEx(chr(SCHEDULER_DATA)+chr(SCHEDULER_RESET), write);
end;

{ Send START_SYSEX + data + END_SYSEX total <= MAX_DATA_BYTES (64 bytes)}
function TBoard.SendSysEx(data: string; write: Boolean=true): string;
begin
  Result:='';
  if (not FEnabled) and (not FStarting) then
  begin
    RaiseError(2, 'Board:SendSysEx');
    exit;
  end;
  if Length(data) > MAX_DATA_BYTES  then
  begin
    RaiseError(21, 'Board:SendSysEx');
    exit;
  end;
  Result:=chr(START_SYSEX)+Data+chr(END_SYSEX);
  if write then
  begin
    if Assigned(OnSendDataToDevice) then
    begin
      FOnSendDataToDevice(self, Result);
    end
    else
      RaiseError(1001, 'Board:SendSysEx');
    Result:='';
  end;
end;

// This method is used to transmit a non-sysex command.
function TBoard.SendCommand(Data: string; write: Boolean=True): string;
begin
  Result:=Data;
  if (not FEnabled) and (not FStarting) then
  begin
    RaiseError(2, 'Board:SendCommand');
    exit;
  end;
  if write then
  begin
    if Assigned(OnSendDataToDevice) then
    begin
      FOnSendDataToDevice(self, Data);
    end
    else
      RaiseError(1001, 'Board:SendCommand');
    Result:='';
  end;
end;

procedure TBoard.printPinInfo(Info: TStrings);
var
  pin : byte;
  Line: string;
  i: integer;
begin
  for pin:=0 to FBoardPinsNumber - 1 do
  begin
       Line:='Pin '+pin.ToString+': Mode=';
       if FBoardPins[pin].ActualMode = PinModesToByte(PIN_MODE_IGNORE) then begin
              Line:=Line+'No operational pin';
       end
       else
       begin
         Line:=Line + Copy(GetEnumName(TypeInfo(TPinModes), FBoardPins[pin].ActualMode), 10, 10) +', Capabilities=';
         for i:=0 to Length(FBoardPins[Pin].Capabilities) - 1 do
         begin
           Line:=Line + Copy(GetEnumName(TypeInfo(TPinModes), FBoardPins[pin].Capabilities[i].mode), 10, 10);
           Line:=Line+'('+IntToStr(FBoardPins[pin].Capabilities[i].Resolution)+' bit), ';
         end;
       end;
       if RightStr(Line,2) = ', ' then
         Line:=LeftStr(Line, Length(Line)-2);

       Info.Add(Line);
  end;
end;
//
//
//
{ TPin  }
//
//
//
constructor TPin.Create(AOwner: TComponent);
begin
  inherited;
  FBoard:= nil;
  FEnabled:=false;
  FPin:=PinModesToByte(PIN_MODE_IGNORE);
  FValue:=0;
  FMode:=PIN_MODE_IGNORE;
  FState:=0;
  FReporting:=False;
  FBoardPinsNumber:=0;
  FAnalogPinsNumber:=0;
  FOnEnabled:=nil;
  FOnDisabled:=nil;

  FOnPinValue:=nil;
  FOnPinState:=nil;
end;

destructor TPin.Destroy();
begin
  inherited Destroy;
end;

procedure TPin.setBoard(Board: TBoard);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'Pin:setBoard')
  else if Assigned(Board) then
    FBoard:=Board;
end;

procedure TPin.setEnabled(State: Boolean);
begin
  if not Assigned(FBoard) then
    exit;

  if FEnabled = State then
     exit;

  if State then
  begin
    if FBoard.Enabled then
    begin
      FBoardPinsNumber:=FBoard.FBoardPinsNumber;
      FAnalogPinsNumber:=FBoard.FBoardPinsNumber;
      FEnabled:=True;
      if FBoard.CheckCapability(FPin, FMode) then
      begin
        if FBoard.FBoardPins[FPin].Busy then
        begin
          // Pin is busy
          FEnabled:=False;
          FBoard.RaiseError(12, 'Pin:setEnabled');
          exit;
        end
        else // pin is free
        begin
          FBoard.FBoardPins[FPin].Busy:=True;
          FBoard.FPins[FPin]:=self;
          SetPinMode; // set new pin mode
          if Assigned(FOnEnabled) then
            FOnEnabled(self);
        end;
      end
      else  // not pin capability
      begin
        FEnabled:=False;
        // Not a valid mode
        FBoard.RaiseError(11, 'Pin:setEnabled');
      end;
    end
    else  // Board not asigned
    begin
      FBoardPinsNumber:=0;
      FAnalogPinsNumber:=0;
    end;
  end
  else  // disable
  begin
    if Assigned(FOnDisabled) then
      FOnDisabled(self);
    if Assigned(FBoard) then
    begin
      if FReporting then
        ReportPin(False);
      FBoard.FBoardPins[FPin].Busy:=False;
      FBoard.FPins[FPin]:=nil;
    end;
    FEnabled:=False;
  end;
end;

procedure TPin.setMode(Mode: TPinModes);
begin
  if Mode = FMode then
    exit;
  if Enabled then
    FBoard.RaiseError(33, 'Pin:setMode')
  else if Mode in [PIN_MODE_INPUT, PIN_MODE_OUTPUT, PIN_MODE_ANALOG, PIN_MODE_PWM,
           PIN_MODE_PULLUP] then
    FMode:=Mode;
end;

procedure TPin.setPin(Pin: Byte);
begin
  if FEnabled then // cannot do that
    FBoard.RaiseError(33, 'Pin:setPin')
  else if Pin < PinModesToByte(PIN_MODE_IGNORE) then  // disabled
    FPin:=Pin;
end;

function TPin.SendCommand(Data: string; write: Boolean=True): string;
begin
  Result:='';
  if not FEnabled and write then
    exit;
  Result:=FBoard.SendCommand(Data, write);
end;

function TPin.SendSysEx(data: string; write: Boolean=true): string;
begin
  Result:='';
  if not FEnabled and write then
    exit;
  Result:=FBoard.SendSysEx(Data, write);
end;

procedure TPin.GetAnalogMessage(Value: integer);
begin
   FValue:=Value;

   if Assigned(FOnPinValue) then
     FOnPinValue(self, Value);
end;
{0  START_SYSEX              (0xF0)
1  pin state query          (0x6D)
2  pin                      (0-127)
3  END_SYSEX                (0xF7)}
function TPin.askPinState(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(PIN_STATE_QUERY)+chr(FPin), write);
end;
{0  set digital pin mode (0xF4) (MIDI Undefined)
1  set pin number (0-127)
2  mode (INPUT/OUTPUT/ANALOG/PWM/SERVO/I2C/ONEWIRE/STEPPER/ENCODER/SERIAL/PULLUP/etc)}
function TPin.SetPinMode(write: Boolean=true): string; // if write true then send bytes to device
var
  ModeValue: Byte;
begin
  Result:='';
  ModeValue:=PinModesToByte(FMode);

  if write then
  begin
    FBoard.FBoardPins[FPin].ActualMode:=ModeValue;
    if FMode in [PIN_MODE_INPUT, PIN_MODE_PULLUP, PIN_MODE_ANALOG] then
    begin
      FReporting:=True;
      if FMode=PIN_MODE_PULLUP then
        FState:=HIGH;
    end;
    FValue:=0;  // after change pin mode firmata set value to 0
  end;
  Result:=SendCommand(chr(SET_PIN_MODE)+chr(FPin)+chr(ModeValue), write); // $F4   );
end;

// enable/disable report for digital pin or analog pin
function TPin.ReportPin(Enabled: boolean; write: Boolean=true): string;
begin
  if FMode in [PIN_MODE_INPUT, PIN_MODE_PULLUP] then // digital pin
    Result:=DigitalReport(Enabled, write)
  else if FMode = PIN_MODE_ANALOG then
    Result:=AnalogReport(Enabled, write)
  else
    Result:='';
end;
{report digital pin, actually report digital port 	0xD0 	port 	disable/enable(0/1) 	- n/a -}
function TPin.DigitalReport(enabled: boolean; write: Boolean=true): string;
var
  Port: Byte;
begin
  if write then
  begin
    Port:=FPin div 8;
    FReporting:=Enabled;
  end;
  if not enabled then
  begin
    if FBoard.CheckReportPort(FPin) then // no disable report pin, there is a digital pin in the same port reporting
      Result:=SendCommand(chr(REPORT_DIGITAL or port)+chr(0), false) // do not need to disable, this is for tasks
    else
      Result:=SendCommand(chr(REPORT_DIGITAL or port)+chr(0), true) // need to disable
  end
  else  // enable report on pin
    Result:=SendCommand(chr(REPORT_DIGITAL or port)+chr(1), write);
end;
{report analog pin 	0xC0 	pin # 	disable/enable(0/1) 	- n/a - }
function TPin.AnalogReport(enabled: boolean; write: Boolean=true): string;
begin
  Result:='';

  // check first
  if FMode <> PIN_MODE_ANALOG then
  begin
    FBoard.RaiseError(10, 'Pin:AnalogReport');
    exit;
  end;

  if write then
      FReporting:=Enabled;

  Result:=SendCommand(chr(REPORT_ANALOG or FBoard.FBoardPins[FPin].AnalogMap)+chr(ord(enabled)), write);
end;

{0  digital data, 0x90-0x9F, (MIDI NoteOn, bud different data format)
1  digital pins 0-6 bitmask
2  digital pin 7-13 bitmask}
function TPin.DigitalWritePort(Port: byte; Value: integer; write: Boolean=true): string;
var
  mask: integer;
  Pin: byte;
begin
  Result:='';

  if (Port * 8 + 8) > (FBoardPinsNumber - 1) then
  begin
    FBoard.RaiseError(8, 'Pin:DigitalWritePort');
    exit;
  end;

  if write then
  begin
    mask:=1;
    for Pin:=Port * 8 to Port * 8 + 7 do // default 8 pins in port
    begin
      if (Pin < Length(FBoard.FPins)) and FEnabled and Assigned(FBoard.FPins[Pin]) and Fboard.FPins[FPin].Enabled and (FMode in [PIN_MODE_OUTPUT, PIN_MODE_INPUT, PIN_MODE_PULLUP]) then
      begin
        if (Value and mask)>0 then  // write a 1
          FBoard.FPins[Pin].FState:=HIGH
        else      // write a 0
          FBoard.FPins[Pin].FState:=LOW;
        FBoard.FPins[Pin].FOnPinState(self, FMode, FState);
      end;
      mask:=mask << 1; // next pin mask
    end;
  end;

  Result:=SendCommand(chr(DIGITAL_MESSAGE or Port)+chr(Value and $7F)+chr((value >> 7) and $7F), write);
end;
// Write pin value, for digital or analog pin
function TPin.WriteValue(Value: uint32; write: Boolean=True): string;
var
  ValueTmp: uint32;
begin
  if FMode in [PIN_MODE_OUTPUT, PIN_MODE_INPUT, PIN_MODE_PULLUP] then
    Result:=SetDigitalPinValue(Value, write)
  else if FMode = PIN_MODE_PWM then
  begin
    // search for resolution
    ValueTmp:=min(Value, FBoard.GetPinResolution(FPin, FMode));

    if (ValueTmp > $3FFF) or (FPin > 15) then
      Result:=AnalogWriteExtended(ValueTmp, write)
    else
      Result:=AnalogWrite(ValueTmp, write);      // value for analogwrite is 14 bits $3FFF
  end;
end;

function TPin.DigitalWrite(Value: Byte; write: Boolean=true): string;
begin
   Result:=SetDigitalPinValue(Value, write);
end;
{set digital pin value}
{0  set digital pin value (0xF5) (MIDI Undefined)
1  set pin number (0-127)
2  value (LOW/HIGH, 0/1)}
function TPin.SetDigitalPinValue(Value: Byte; write: Boolean=true): string;
var
  TmpValue: Byte;
begin
  Result:='';

  if not (FMode in [PIN_MODE_OUTPUT, PIN_MODE_INPUT, PIN_MODE_PULLUP]) then
  begin
    FBoard.RaiseError(7, 'Pin:DigitalWrite');
    exit;
  end;
  if Value <> 0 then
    TmpValue:=HIGH
  else
    TmpValue:=LOW;

  if write then   // avoid change Fstate when it is not a real write
    FState:=TmpValue;

  Result:=SendCommand(chr(SET_DIGITAL_PIN_VALUE)+chr(FPin)+chr(TmpValue), write); // $F5
end;
{0  analog pin, 0xE0-0xEF, (MIDI Pitch Wheel)  max 15 pins
1  analog least significant 7 bits
2  analog most significant 7 bits }
function TPin.AnalogWrite(Value: word; write: Boolean=true): string;
var
   ValueTmp: uint32;
begin
  if FPin > 15 then
  begin
    Result:=AnalogWriteExtended(Value, write);
    exit;
  end;

  ValueTmp:=min(Value, FBoard.GetPinResolution(FPin, FMode));

  Result:=SendCommand(chr(ANALOG_MESSAGE or FPin)+chr(ValueTmp and $7F)+chr((ValueTmp >> 7) and $7F),write);
  if write then
    FState:=ValueTmp;  // store last value written
end;
{0  START_SYSEX              (0xF0)
1  extended analog message  (0x6F)
2  pin                      (0-127)
3  bits 0-6                 (least significant byte)
4  bits 7-13                (most significant byte)
... additional bytes may be sent if more bits are needed
N  END_SYSEX                (0xF7)}
function TPin.AnalogWriteExtended(Value: uint32; write: Boolean=True): string; // analog write (PWM, Servo, etc) to any pin
var
  i: integer;
  Valuetmp: integer;
  Data: String;
begin
  ValueTmp:=min(Value, FBoard.GetPinResolution(FPin, FMode));

  Data:=chr(Valuetmp and $FF);  // byte 1
  Valuetmp:=Valuetmp>>8; // prepare value

  for i:=2 to 4 do    // max 4 bytes 32 bits
  begin
    data:=data+chr(Valuetmp and $FF);  // LSB byte ,MSB byte
    Valuetmp:=Valuetmp>>8; // prepare value
    if Valuetmp = 0 then  // no more data to send
      break;
  end;

  Result:=SendSysEx(chr(EXTENDED_ANALOG)+chr(FPin)+Encode8To7Bit(data), write);
  if write then
    FState:=min(Value, FBoard.GetPinResolution(FPin, FMode));   //store value written
end;

//
//
//
{ TTask }
//
//
//
constructor TTask.Create(AOwner: TComponent);
begin
  inherited;

  FBoard:= nil;
  FEnabled:=false;
  FTaskID:=0;  // Task ID
  FDataTask:='';
  FDataTaskRunOnce:='';
  FRunOnce:=True;
  FOnEnabled:=nil;
  FOnDisabled:=nil;
  FRunDelay:=0;
  FTimeDelay:=0;
  FOnTaskError:=nil;
  FOnQueryTask:=nil;
end;

destructor TTask.Destroy();
begin
  inherited Destroy;
end;

procedure TTask.setTask(Task: Byte);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'Task:setTask')
  else if Task > 127 then
    FTaskID:=0
  else
    FTaskID:=Task;
end;

procedure TTask.setTimeDelay(Delay: integer);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'Task:setTimeDelay')
  else if Delay < 0 then
    FTimeDelay:=0
  else
    FTimeDelay:=Delay;
end;

procedure TTask.setRunDelay(Delay: integer);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'Task:setRunDelay')
  else if Delay < 0 then
    FRunDelay:=0
  else
    FRunDelay:=Delay;
end;

procedure TTask.setBoard(Board: TBoard);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'Task:setBoard')
  else if Assigned(Board) then
    FBoard:=Board;
end;

procedure TTask.setEnabled(State: Boolean);
begin
  if not Assigned(FBoard) then
    exit;

  if FEnabled = State then
     exit;

  if State = True then
  begin
    if Assigned(FBoard) and FBoard.Enabled then
    begin
      if length(FDataTaskRunOnce) < 3 then // min command length
      begin
        FBoard.RaiseError(25, 'Task:setEnabled');
        exit;
      end;
      if Assigned(FBoard.FTasks[FTaskID]) then // TaskID already exists
      begin
        FBoard.RaiseError(45, 'Task:setEnabled');
        exit;
      end;
      // task does not exist

      if FRunOnce then  // task run again after finished, so needs a end delay
        FDataTask:=FDataTaskRunOnce
      else
        FDataTask:=FDataTaskRunOnce+DelayTask(false);  // only get command, for continuous running

      FBoard.LastError:=0;
      FBoard.FTasks[FTaskID]:=self;
      FEnabled:=True;
      CreateTask; // length of task is Length of FDataTask
      if FBoard.LastError > 0 then
      begin
        FEnabled:=False;
        FBoard.FTasks[FTaskID]:=nil;
        exit;
      end;
      AddToTask(FDataTask);
      ScheduleTask;  // Run task
      if Assigned(FOnEnabled) then
        FOnEnabled(self);
    end
    else // FBoard is not enabled
    begin
      FEnabled:=false;
      FBoard.RaiseError(2, 'Task:setEnabled');
    end;
  end
  else  // disabled
  begin
    if Assigned(FOnDisabled) then
      FOnDisabled(self);
    DeleteTask;
    FBoard.FTasks[TaskID]:=nil;
    FEnabled:=False;
  end;
end;

procedure TTask.parsefirmatacommand(Sender: TObject; CommandData: String);
var
  TaskID: byte;
  DataString: string;
  Time_ms: integer;
  Len: integer;     // data length
  Position: integer;
  Command: Byte;
begin
  Time_ms:=0;
  Len:=0;
  Position:=0;
  Command:=ord(CommandData[1]);   // subcommand is first byte
    {0  START_SYSEX              (0xF0)
    1  Scheduler Command        (0x7B)
    2  error_task Reply subCommand (0x08) or query_task Reply Command (0x0A)
    3  task id                  (0-127)
    4  time_ms bit 0-6
    5  time_ms bit 7-13
    6  time_ms bit 14-20
    7  time_ms bit 21-27
    8  time_ms bit 28-31 | (length bit 0-2) << 4
    9  length bit 3-9
    10 length bit 10-15 | (position bit 0) << 7
    11 position bit 1-7
    12 position bit 8-14
    13 position bit 15 | taskdata bit 0-5 << 1 [taskdata is optional]
    14 taskdata bit 6-12  [optional]
    15 taskdata bit 13-19 [optional]
    n  ... as many bytes as needed (don't exceed MAX_DATA_BYTES though)
    n+1  END_SYSEX              (0xF7) }
  TaskID:=ord(CommandData[2]);  // get task ID
  DataString:=Copy(CommandData, 3, Length(CommandData) - 2);
  if DataString <> '' then
  begin
    DataString:=Decode7To8bit(DataString);  // decode 7 bits string
    Time_ms:=decodeNbytestoInt(Copy(DataString,1,4)); // first 32 bits, 4 bytes
    Len:=decodeNbytestoInt(Copy(DataString,5,2));  // next 16 bits, 2 bytes
    Position:=decodeNbytestoInt(Copy(DataString,7,2));  // next 16 bits, 2 bytes
    DataString:=Copy(DataString,9,Length(DataString)-8);  // Task Data
  end;

  if Command = QUERY_TASK_REPLY then
  begin
    if Assigned(FOnQueryTask) then
      FOnQueryTask(self, Time_ms, Len, Position, DataString);
  end
  else if Assigned(FOnTaskError) then    // ERROR_TASK_REPLY
    FOnTaskError(self, Time_ms, Len, Position, DataString);
end;
{ Send START_SYSEX + data + END_SYSEX total <= MAX_DATA_BYTES (64 bytes)}
function TTask.SendSysEx(data: string; write: Boolean=true): string;
begin
  Result:='';
  if not FEnabled and write then
    exit;
  Result:=FBoard.SendSysEx(Data, write);
end;

//
// SCHEDULER COMMANDS
//
{0  START_SYSEX          (0xF0)
 1  Scheduler Command    (0x7B)
 2  create_task command  (0x00)
 3  task id              (0-127)
 4  length LSB           (bit 0-6)
 5  length MSB           (bit 7-13)
 6  Config byte (optional)  (bit 0 = 1 task is paused at then end not deleted
 6,7  END_SYSEX            (0xF7) }
function TTask.CreateTask(write: Boolean=true): string;
var
  Flength: integer;
begin
  Result:='';

  Flength:=Length(FDataTask); // Task stores data as 7 bit

  if FLength > $3FFF then // 14 bits
  begin
    FBoard.RaiseError(6, 'Task:CreateTask');
    exit;
  end;
  Result:=SendSysEx(chr(SCHEDULER_DATA)+chr(CREATE_TASK)+chr(FTaskID and $7F)+chr(FLength and $7F)+chr((FLength >> 7) and $7F), write);
end;
{0  START_SYSEX          (0xF0)
 1  Scheduler Command    (0x7B)
 2  delete_task command  (0x01)
 3  task id              (0-127)
 4  END_SYSEX            (0xF7)}
function TTask.DeleteTask(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(SCHEDULER_DATA)+chr(DELETE_TASK)+chr(FTaskID), write);
end;
{0  START_SYSEX          (0xF0)
1  Scheduler Command    (0x7B)
2  add_to_task command  (0x02)
3  task id              (0-127)
4  taskdata bit 0-6     [optional] task bytes encoded using 8 times 7 bit
                         for 7 bytes of 8 bit
5  taskdata bit 7-13    [optional]
6  taskdata bit 14-20   [optional]
n  ... as many bytes as needed (don't exceed MAX_DATA_BYTES though)
n+1  END_SYSEX          (0xF7)}
function TTask.AddToTask(Data: String; write: Boolean=true): string;
var
  i: integer;
begin
  Result:='';
  i:=0;
  while i < Length(Data) do
  begin
    // max size for send is 52 8_bits bytes = 59 bytes 7_Bits, should be 53 but one error on ConfigurableFirmata.cpp, 64 bytes message
    Result:=Result+SendSysEx(chr(SCHEDULER_DATA)+chr(ADD_TO_TASK)+chr(FTaskID)+Encode8To7Bit(Copy(Data, i+1, 52)), write);
    inc(i, 52);
  end;
end;
{0  START_SYSEX          (0xF0)
 1  Scheduler Command    (0x7B)
 2  delay_task command   (0x03)
 3  time_ms bit 0-6      time_ms is of type long, requires 32 bit.
 4  time_ms bit 7-13
 5  time_ms bit 14-20
 6  time_ms bit 21-27
 7  time_ms bit 28-31
 8  END_SYSEX            (0xF7)}
function TTask.DelayTask(write: Boolean=true): string;    // use for internal task
begin
  Result:=SendSysEx(chr(SCHEDULER_DATA)+chr(DELAY_TASK)+Encode32BitUnSignedInt(FTimeDelay), write);
end;
{0  START_SYSEX              (0xF0)
 1  Scheduler Command        (0x7B)
 2  schedule_task command    (0x04)
 3  task id                  (0-127)
 4  time_ms bit 0-6          time_ms is of type long, requires 32 bit.
 5  time_ms bit 7-13
 6  time_ms bit 14-20
 7  time_ms bit 21-27
 8  time_ms bit 28-31
 9  END_SYSEX                (0xF7)}
function TTask.ScheduleTask(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(SCHEDULER_DATA)+chr(SCHEDULE_TASK)+chr(FTaskID)+Encode32BitUnSignedInt(FRunDelay), write);
end;

{0  START_SYSEX              (0xF0)
 1  Scheduler Command        (0x7B)
 2  query_task command       (0x06)
 3  task id                  (0-127)
 4  END_SYSEX                (0xF7) }
function TTask.QueryTask(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(SCHEDULER_DATA)+chr(QUERY_TASK)+chr(FTaskID), write);
end;


//
//
//
{ TOneWire }
//
//
//
constructor TOneWire.Create(AOwner: TComponent);
begin
  inherited;

  FBoard:= nil;
  FEnabled:=false;

  FOnEnabled:=nil;
  FOnDisabled:=nil;

  FOnOneWireData:=nil;
  FOnSearch:=nil;
  FOnOneWireAlarm:=nil;
  FPin:=PinModesToByte(PIN_MODE_IGNORE);
  FParasitisticPower:=false;
end;

destructor TOneWire.Destroy();
begin
  inherited Destroy;
end;

procedure TOneWire.setBoard(Board: TBoard);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'OneWire:setBoard')
  else if Assigned(Board) then
    FBoard:=Board;
end;

procedure TOneWire.setPin(Pin: Byte);
begin
  if FEnabled then
  begin
    FBoard.RaiseError(33, 'OneWire:setPin');
    exit;
  end
  else if Pin > 127 then
  begin
    FBoard.RaiseError(31, 'OneWire:setPin');
    FPin:=127;
  end
  else
    FPin:=Pin;
end;

procedure TOneWire.setEnabled(State: Boolean);
begin
  if not Assigned(FBoard) then
    exit;

  if FEnabled = State then
     exit;

  if State then
  begin
    FEnabled:=True;
    if Assigned(FBoard) and FBoard.Enabled then
    begin
      // Check if supported pin
      if not FBoard.CheckCapability(FPin, PIN_MODE_ONEWIRE) then // not supported
      begin
        FEnabled:=false;
        FBoard.RaiseError(7, 'OneWire:setEnabled');
      end
      else if FBoard.FBoardPins[FPin].Busy then  // check if pin is assigned
      begin
        FEnabled:=false;
        FBoard.RaiseError(12, 'OneWire:setEnabled');
      end;
    end
    else
    begin
      FEnabled:=false; // firmataboard not enabled
      FBoard.RaiseError(2, 'OneWire:setEnabled');
    end;
    if FEnabled then
    begin
      if Assigned(FBoard.FOneWires[FPin]) then
      begin
        FEnabled:=False;
        FBoard.RaiseError(54, 'OneWire:setEnabled');
      end
      else // new onewire module
      begin
        FBoard.FOneWires[FPin]:=self;
        FBoard.FBoardPins[FPin].Busy:=true;  //  pin is assigned to this module
        config;  // configure onewire
        if Assigned(FOnEnabled) then
          FOnEnabled(self);
      end;
    end;
  end
  else  // disable
  begin
    if Assigned(FOnDisabled) then
      FOnDisabled(self);
    if Assigned(FBoard) then
    begin
      FBoard.FBoardPins[FPin].Busy:=false; // free onewire pin
      FBoard.FOneWires[FPin]:=nil;
    end;
    FEnabled:=False;
  end;
end;

procedure TOneWire.parsefirmatacommand(Sender: TObject; CommandData: String);
var
  Pin: byte;
  DataString: string;
  OneWireIDs: array of string;
  i: integer;
begin
  // first byte is ONEWIRE_SEARCH_REPLY or ONEWIRE_SEARCH_ALARMS_REPLY
  case ord(CommandData[1]) of   // Onewire command
       {0  START_SYSEX      (0xF0)
       1  OneWire Command  (0x73)
       2  search reply command (0x42|0x45) 0x42 normal search reply
                                           0x45 reply to a SEARCH_ALARMS request
       3  pin              (0-127)
       4  bit 0-6   [optional] address bytes encoded using 8 times 7 bit for 7 bytes of 8 bit
       5  bit 7-13  [optional] 1.address[0] = byte[0]    + byte[1]<<7 & 0x7F
       6  bit 14-20 [optional] 1.address[1] = byte[1]>>1 + byte[2]<<6 & 0x7F
       7  ....                 ...
       11 bit 49-55            1.address[6] = byte[6]>>6 + byte[7]<<1 & 0x7F
       12 bit 56-63            1.address[7] = byte[8]    + byte[9]<<7 & 0x7F
       13 bit 64-69            2.address[0] = byte[9]>>1 + byte[10]<<6 &0x7F
       n  ... as many bytes as needed (don't exceed MAX_DATA_BYTES though)
       n+1  END_SYSEX      (0xF7)}
       ONEWIRE_SEARCH_REPLY, ONEWIRE_SEARCH_ALARMS_REPLY: begin  // $42 $45
          Pin:=ord(CommandData[2]);
          DataString:=Copy(CommandData, 3, Length(CommandData)-2);  // excludes ONEWIRE_SEARCH_REPLY or ONEWIRE_SEARCH_ALARMS_REPLY and Pin
          DataString:=Decode7To8bit(DataString);

          SetLength(OneWireIDs,Length(DataString) div 8);  // Calc size of discovered IDs array
          for i:=0 to length(OneWireIDs)-1 do // fill the array
            OneWireIDs[i]:=Copy(DataString,i,8);  // Store next ID
          if (Length(OneWireIDs) > 0) and (Length(OneWireIDs[0]) > 0) then
            FDevice:=OneWireIDs[0];   // deafult device to first OneWireID found

          if ord(CommandData[1]) = ONEWIRE_SEARCH_REPLY then
          begin
            if Assigned(FOnSearch) then
              FOnSearch(self,OneWireIDs);
          end
          else  // ONEWIRE_SEARCH_ALARMS_REPLY
             if Assigned(FOnOneWireAlarm) then
               FOnOneWireAlarm(self,OneWireIDs);
       end;
       {0  START_SYSEX          (0xF0)
       1  OneWire Command      (0x73)
       2  read reply command   (0x43)
       3  pin                  (0-127)
       4  bit 0-6   [optional] data bytes encoded using 8 times 7 bit for 7 bytes of 8 bit
       5  bit 7-13  [optional] correlationid[0] = byte[0]   + byte[1]<<7 & 0x7F
       6  bit 14-20 [optional] correlationid[1] = byte[1]>1 + byte[2]<<6 & 0x7F
       7  bit 21-27 [optional] data[0] = byte[2]>2 + byte[3]<<5 & 0x7F
       8  ....                 data[1] = byte[3]>3 + byte[4]<<4 & 0x7F
       n  ... as many bytes as needed (don't exceed MAX_DATA_BYTES though)
       n+1  END_SYSEX          (0xF7)}
       ONEWIRE_READ_REPLY: begin //  $43;
           Pin:=ord(CommandData[2]);
           DataString:=Copy(CommandData, 3, Length(CommandData)-2);  // excludes ONEWIRE_READ_REPLY and Pin
           DataString:=Decode7To8bit(DataString); // Data received from Onewire
           if Assigned(FOnOneWireData) then
             // object, data (correlation and scratchpad)
             FOnOneWireData(self,DataString);
       end;
   end;
end;
{ Send START_SYSEX + data + END_SYSEX total <= MAX_DATA_BYTES (64 bytes)}
function TOneWire.SendSysEx(data: string; write: Boolean=true): string;
begin
  Result:='';
  if not FEnabled and write then
    exit;

  Result:=FBoard.SendSysEx(Data, write);
end;
//
// ONEWIRE commands
//

{0  START_SYSEX      (0xF0)
1  OneWire Command  (0x73)
2  config command   (0x41)
3  pin              (0-127)
4  power            (0x00|0x01) 0x00 = leave pin on state high after write to support
                                parasitic power
                                0x01 = don't leave pin on high after write
5  END_SYSEX (0xF7)}
function TOneWire.config(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(ONEWIRE_DATA)+chr(ONEWIRE_CONFIG_REQUEST)+chr(FPin)+chr(ord(FParasitisticPower)), write);
  if write then
    FBoard.FBoardPins[FPin].ActualMode:=PinModesToByte(PIN_MODE_ONEWIRE);
end;

{0  START_SYSEX      (0xF0)
 1  OneWire Command  (0x73)
 2  search command   (0x40|0x44) 0x40 normal search for all devices on the bus
                        0x44 SEARCH_ALARMS request to find only those
                        devices that are in alarmed state.
 3  pin              (0-127)
 4  END_SYSEX        (0xF7)}
function TOneWire.Search(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(ONEWIRE_DATA)+chr(ONEWIRE_SEARCH_REQUEST)+chr(FPin), write);
end;
{0  START_SYSEX      (0xF0)
1  OneWire Command  (0x73)
2  search command   (0x40|0x44) 0x40 normal search for all devices on the bus
                                0x44 SEARCH_ALARMS request to find only those
                                devices that are in alarmed state.
3  pin              (0-127)
4  END_SYSEX        (0xF7) }
function TOneWire.AlarmSearch(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(ONEWIRE_DATA)+chr(ONEWIRE_SEARCH_ALARMS_REQUEST)+chr(FPin), write);
end;

{0  START_SYSEX      (0xF0)
1  OneWire Command  (0x73)
2  command bits     (0x00-0x2F) bit 0 = reset, bit 1 = skip, bit 2 = select,
                                bit 3 = read, bit 4 = delay, bit 5 = write
3  pin              (0-127)
4  bit 0-6   [optional] data bytes encoded using 8 times 7 bit for 7 bytes of 8 bit
5  bit 7-13  [optional] data[0] = byte[0]   + byte[1]<<7 & 0x7F
6  bit 14-20 [optional] data[1] = byte[1]>1 + byte[2]<<6 & 0x7F
7  ....                 data[2] = byte = byte[2]>2 + byte[3]<<5 & 0x7F ...
n  ... as many bytes as needed (don't exceed MAX_DATA_BYTES though)
n+1  END_SYSEX      (0xF7)

// data bytes within OneWire Request Command message
0  address[0]                    [optional, if bit 2 set]
1  address[1]                              "
2  address[2]                              "
3  address[3]                              "
4  address[4]                              "
5  address[5]                              "
6  address[6]                              "
7  address[7]                              "
8  number of bytes to read (LSB) [optional, if bit 3 set]
9  number of bytes to read (MSB)           "
10 request correlationid byte 0            "
11 request correlationid byte 1            "
10 delay in ms      (bits 0-7)   [optional, if bit 4 set]
11 delay in ms      (bits 8-15)            "
12 delay in ms      (bits 16-23)           "
13 delay in ms      (bits 24-31)           "
14 data to write    (bits 0-7)   [optional, if bit 5 set]
15 data to write    (bits 8-15)            "
16 data to write    (bits 16-23)           "
n  ... as many bytes as needed (don't exceed MAX_DATA_BYTES though)}
function TOneWire.SendCommands(Command: Byte; numBytesToRead: uint16;
                   correlationId: uint16; delay: integer; dataToWrite: String; write: Boolean=true): string;
var
   Data: string;
   SubCommand: byte;
begin
  Result:='';

  Data:='';
  Subcommand:=Command and $3F;  // bits 0 to 5

  if (Subcommand and ONEWIRE_SELECT_REQUEST_BIT) > 0 then
  begin
    // Device
    Data:=leftStr(FDevice+'#0#0#0#0#0#0#0#0',8);  // set length to 8 and fill with #0
  end;
  if ((Subcommand and ONEWIRE_READ_REQUEST_BIT) > 0) or (numBytesToRead > 0) then
  begin
    SubCommand:=Subcommand or ONEWIRE_READ_REQUEST_BIT;
    Data:=Data+chr(numBytesToRead and $FF)+chr((numBytesToRead >> 8) and $FF);
    Data:=Data+chr(correlationId and $FF)+chr((correlationId >> 8) and $FF);
  end;
  if ((Subcommand and ONEWIRE_DELAY_REQUEST_BIT) > 0 ) or (delay > 0) then
  begin
    SubCommand:=Subcommand or ONEWIRE_DELAY_REQUEST_BIT;
    Data:=Data+chr(delay and $FF)+chr((delay >> 8) and $FF)+chr((delay >> 16) and $FF)+chr((delay >> 24) and $FF);
  end;
  if ((Subcommand and ONEWIRE_WRITE_REQUEST_BIT) > 0 ) or (dataToWrite <> '') then
  begin
    SubCommand:=Subcommand or ONEWIRE_WRITE_REQUEST_BIT;
    Data:=Data+dataToWrite;
  end;

  Result:=SendSysEx(chr(ONEWIRE_DATA)+chr(Subcommand)+chr(FPin)+Encode8To7Bit(Data), write);
end;

function TOneWire.Reset(write: Boolean=true): string;
begin
  //Command, numBytesToRead, correlationId, delay, dataToWrite, write
  Result:=SendCommands(ONEWIRE_RESET_REQUEST_BIT, 0, 0, 0, '', write);
end;

function TOneWire.Skip(write: Boolean=true): string;
begin
  //Command, numBytesToRead, correlationId, delay, dataToWrite, write
  Result:=SendCommands(ONEWIRE_SKIP_REQUEST_BIT, 0, 0, 0, '', write);
end;

function TOneWire.Select(write: Boolean=true): string;
begin
  //Command, numBytesToRead, correlationId, delay, dataToWrite, write
  Result:=SendCommands(ONEWIRE_SELECT_REQUEST_BIT, 0, 0, 0, '', write);
end;

function TOneWire.ResetAndSelect(write: Boolean=true): string;
begin
  //Command, numBytesToRead, correlationId, delay, dataToWrite, write
  Result:=SendCommands(ONEWIRE_RESET_REQUEST_BIT or ONEWIRE_SELECT_REQUEST_BIT, 0, 0, 0, '', write);
end;

function TOneWire.ResetAndSelectAndWrite(DataOut: string; write: Boolean=true): string;
begin
   //Command, numBytesToRead, correlationId, delay, dataToWrite, write
  Result:=SendCommands(ONEWIRE_RESET_REQUEST_BIT or ONEWIRE_SELECT_REQUEST_BIT or ONEWIRE_WRITE_REQUEST_BIT, 0, 0, 0, DataOut, write);
end;

function TOneWire.ResetAndSelectAndWrite(Delay: integer; DataOut: string; write: Boolean=true): string; overload;
begin
   //Command, numBytesToRead, correlationId, delay, dataToWrite, write
  Result:=SendCommands(ONEWIRE_RESET_REQUEST_BIT or ONEWIRE_SELECT_REQUEST_BIT or ONEWIRE_WRITE_REQUEST_BIT, 0, 0, Delay, DataOut, write);
end;

function TOneWire.ResetAndSelectAndWriteAndRead(BytestoRead: uint16; Correlation: uint16; Delay: integer; DataOut: string; write: Boolean=true): string;
begin
  //Command, numBytesToRead, correlationId, delay, dataToWrite, write
  Result:=SendCommands(ONEWIRE_RESET_REQUEST_BIT or ONEWIRE_SELECT_REQUEST_BIT or ONEWIRE_WRITE_REQUEST_BIT or ONEWIRE_READ_REQUEST_BIT,
                         BytesToRead, Correlation, Delay, DataOut, write);
end;

function TOneWire.Read(BytestoRead: uint16; Correlation: uint16; write: Boolean=true): string;
begin
  //Command, numBytesToRead, correlationId, delay, dataToWrite, write
  Result:=SendCommands(ONEWIRE_READ_REQUEST_BIT, BytestoRead, correlation, 0, '', write);
end;

function TOneWire.Write(Data: string; write: Boolean=true): string;
begin
  //Command, numBytesToRead, correlationId, delay, dataToWrite, write
  Result:=SendCommands(ONEWIRE_WRITE_REQUEST_BIT, 0, 0, 0, Data, write);
end;

function TOneWire.Write(Delay: integer; Data: string; write: Boolean=true): string;  overload;
begin
  Result:=SendCommands(ONEWIRE_WRITE_REQUEST_BIT, 0, 0, Delay, Data, write);
end;

function TOneWire.WriteAndRead(BytestoRead: uint16; Correlation: uint16; Data: string; write: Boolean=true): string;
begin
  //Command, numBytesToRead, correlationId, delay, dataToWrite, write
  Result:=SendCommands(ONEWIRE_WRITE_REQUEST_BIT or ONEWIRE_READ_REQUEST_BIT, BytestoRead, Correlation, 0, Data, write);
end;

function TOneWire.WriteAndRead(BytestoRead: uint16; Correlation: uint16; Delay: integer; Data: string; write: Boolean=true): string; overload;
begin
  //Command, numBytesToRead, correlationId, delay, dataToWrite, write
  Result:=SendCommands(ONEWIRE_WRITE_REQUEST_BIT or ONEWIRE_READ_REQUEST_BIT, BytestoRead, Correlation, Delay, Data, write);
end;


//
//
//
{ TI2C }
//
//
//
constructor TI2C.Create(AOwner: TComponent);
begin
  inherited;

  FBoard:= nil;
  FEnabled:=false;

  FOnEnabled:=nil;
  FOnDisabled:=nil;

  FContinuously:=nil;

  F10bits:=False;   // not supported yet
  FSequence:=0;

  FOnI2CData:=nil;
  FI2CQueries:=0;
  FDelay:=0;  // default delay is 0
  FSDAPin:=18;  // default SDA pin is 18, A4
  FSCLPin:=19;     // deafult SCL pin is 19, A5
end;

destructor TI2C.Destroy();
begin
  inherited Destroy;
end;

procedure TI2C.setBoard(Board: TBoard);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'I2C:setBoard')
  else if Assigned(Board) then
    FBoard:=Board;
end;

procedure TI2C.setSDAPin(pin: Byte);   // for future use
begin
  if FEnabled then
  begin
    FBoard.RaiseError(33, 'I2C:setSDAPin');
    exit;
  end
  else if Pin > 127 then
  begin
    FBoard.RaiseError(31, 'I2C:setSDAPin');
    FSDAPin:=18;   // default to arduino
  end
  else
    FSDAPin:=Pin;
end;

procedure TI2C.setSCLPin(pin: Byte);  // for future use
begin
  if FEnabled then
  begin
    FBoard.RaiseError(33, 'I2C:setSCLPin');
    exit;
  end
  else if Pin > 127 then
  begin
    FBoard.RaiseError(31, 'I2C:setSCLPin');
    FSCLPin:=19;   // default to arduino
  end
  else
    FSCLPin:=Pin;
end;

procedure TI2C.setSequence(Value: Byte);
begin
  if Value > 7 then
  begin
    FSequence:=0;
    FBoard.RaiseError(26, 'I2C:setSequence');
  end
  else
    FSequence:=Value;
end;

procedure TI2C.setEnabled(State: Boolean);
var
   i: integer;
begin
  if not Assigned(FBoard) then
    exit;

  if FEnabled = State then
     exit;

  if State then
  begin
    FEnabled:=False;
    if Assigned(FBoard) and FBoard.Enabled then
    begin
      if FBoard.CheckCapability(FSDAPin, PIN_MODE_I2C) and FBoard.CheckCapability(FSCLPin, PIN_MODE_I2C) then
      begin
        if FBoard.FBoardPins[FSDAPin].Busy or FBoard.FBoardPins[FSCLPin].Busy then
        begin
          FBoard.RaiseError(12, 'I2C:setEnabled');
          exit;
        end
        else if not Assigned(FBoard.FI2C) then // Both pis are free, check if module is free
        begin // Configure I2C
          FBoard.FI2C:=self;
          FEnabled:=true;
          FSequence:=0;
          config(FDelay);
          FBoard.FBoardPins[FSDAPin].Busy:=True;
          FBoard.FBoardPins[FSCLPin].Busy:=True;
          if Assigned(FOnEnabled) then
            FOnEnabled(self);
        end
        else  // Module already in use
          FBoard.RaiseError(45, 'I2C:setEnabled');
      end
      else  // No valid pins
        FBoard.RaiseError(7, 'I2C:setEnabled');
    end
    else  // firmata is not enabled
      FBoard.RaiseError(36, 'I2C:setEnabled');
  end
  else  // state is false disable
  begin
    if Assigned(FOnDisabled) then
      FOnDisabled(self);
    // check if there is any ReadContinuously activated and stop it
    for i:=0 to Length(FContinuously) - 1 do
      StopReading(FContinuously[i]);
    FContinuously:=nil;
    FBoard.FBoardPins[FSDAPin].Busy:=false;   // free pins
    FBoard.FBoardPins[FSCLPin].Busy:=false;
    if Assigned(FBoard) then
      FBoard.FI2C:=nil;
    FEnabled:=False;
  end;
end;

procedure TI2C.parsefirmatacommand(Sender: TObject; CommandData: String);
var
  DataString: string;
  Slave_Address : word;
  I2C_Register: byte;
  Sequence: byte;
begin
  {0  START_SYSEX (0xF0)
   1  I2C_REPLY (0x77)
   2  slave address (LSB)
   3  slave address (MSB). Bits 0-2 return the transaction sequence from the request in 7 bit mode.
   4  register (LSB)
   5  register (MSB)
   6  data 0 (LSB)
   7  data 0 (MSB)
   ...
   n  END_SYSEX (0XF7)}

   // CommandData excludes I2C_REPLAY and END_SYSEX

   Slave_Address:=ord(CommandData[1]);

   if F10bits then
   begin
     Slave_Address:=Slave_Address or (ord(CommandData[2]) << 7);   // 10 bits mode
     Sequence:=0;
   end
   else   // 7 Bits mode
     Sequence:=ord(CommandData[2]) and $7;



   I2C_Register:=ord(CommandData[3]) or (ord(CommandData[4]) << 7);
   DataString:=Copy(CommandData, 5, Length(CommandData)-4);
   DataString:=Decode2BytesCharTo1(DataString);
   if Assigned(FOnI2CData) then
      FOnI2CData(self, Slave_Address, I2C_Register, Sequence, DataString);
end;

{ Send START_SYSEX + data + END_SYSEX total <= MAX_DATA_BYTES (64 bytes)}
function TI2C.SendSysEx(data: string; write: Boolean=true): string;
begin
  Result:='';
  if not FEnabled and write then
    exit;
  Result:=FBoard.SendSysEx(Data, write);
end;
//
// I2C commands
//
{0  START_SYSEX (0xF0)
1  I2C_CONFIG (0x78)
2  Delay in microseconds (LSB) [optional]
3  Delay in microseconds (MSB) [optional]
... user defined for special cases, etc
n  END_SYSEX (0xF7)}
function TI2C.Config(Delay: byte; write: Boolean=True): string; overload;
var
  I2CData: string;
begin
  Result:='';
  I2CData:='';

  if Delay > 0 then
    I2CData:=chr(Delay and $7F)+chr((Delay >> 7) and $7F) ;

  // Firmata defaults pins, set pin 18 y 19 (analog 4 and 5) on arduino to I2C Mode  SDA, SCL
  // Default pins for esp8266 are SDApin=4 and SCLpin=5
  if write then
  begin
    FBoard.FBoardPins[FSDAPin].ActualMode:=PinModesToByte(PIN_MODE_I2C);
    FBoard.FBoardPins[FSCLPin].ActualMode:=PinModesToByte(PIN_MODE_I2C);
  end;
  Result:=SendSysEx(chr(I2C_CONFIG)+I2CData, write);
end;

function TI2C.Config(write: Boolean=true): string;
begin
  Result:=Config(0, write);
end;
{0  START_SYSEX (0xF0)
1  I2C_REQUEST (0x76)
2  slave address (LSB)
3  slave address (MSB) + read/write and address mode bits
          bit 7: always 0
          bit 6: auto restart transmission, 0 = stop (default), 1 = restart
          bit 5: address mode, 0 = 7bits mode, 1 = 10-bit mode
          bits 4-3: read/write, 00 = write, 01 = read once, 10 = read continuously, 11 = stop reading
          bits 2-0: slave address MSB in 10-bit mode, transaction sequence number in 7-bit mode
4  data 0 (LSB)
5  data 0 (MSB)
6  data 1 (LSB)
7  data 1 (MSB)
...
n  END_SYSEX (0xF7) }
function TI2C.Request(Slave: word; command: byte; data: string; restart: Boolean=false; write: Boolean=True): string;
var
  I2CData: string;
  Mode: Byte;   // is slave address (MSB) + bits
begin
  Mode:=command and I2C_READ_WRITE_MODE_MASK; // only keeps bits 4 and 3

  if F10bits then
  begin
     Mode:=Mode or ((Slave >> 7) and $7); // takes bits 7,8 and 9 from slave ¿?
     Mode:=Mode or I2C_10BIT_ADDRESS_MODE_MASK;  // set 10 bit mode, bit 5 on
  end
  else if Mode in [I2C_READ, I2C_READ_CONTINUOUSLY] then
    Mode:=Mode or (FSequence and  $7);  // bits 0-2

  if restart then
    Mode:=Mode or I2C_AUTORESTART_RESTART;   // bit 6 on

  {bits 4-3: read/write, 00 = write, 01 = read once, 10 = read continuously, 11 = stop reading}
  {case (command and I2C_READ_WRITE_MODE_MASK) of
     I2C_WRITE:begin
        ;
     end;
     I2C_READ: begin
         // a slave register is specified
         // slaveRegister = data[1] + (data[2] << 7);
         // data[3] or (data[4] << 7) = bytes to read
         // else
         // data[1] or (data[2] << 7) = bytes to read
     end;
     I2C_READ_CONTINUOUSLY: begin
         // if a slave register is specified
         // slaveRegister = data[1] + (data[2] << 7);
         // data[3] or (data[4] << 7) = bytes to read
         // else
         // data[1] or (data[2] << 7) = bytes to read
     end;
     I2C_STOP_READING: begin

     end;
  end; }

  I2CData:=Encode1ByteCharTo2(Data);

  Result:=SendSysEx(chr(I2C_REQUEST)+chr(Slave and $7F)+chr(Mode)+I2CData, write);
end;

function TI2C.WriteData(Slave: word; Address: integer; AddressSize: Byte; Data: String; restart: Boolean=false; write: Boolean=True): string;
  //Request(Slave, command, data, restart, write)
var
  I2CData: string;
begin
  Result:='';

  I2CData:=chr(Address and $FF);
  if AddressSize = 2 then
    I2CData:=chr((Address >> 8) and $FF)+I2CData;   // first is MSB byte address

  I2CData:=I2CData+Data;

  if (Length(I2CData) * 2) > (MAX_DATA_BYTES - 3) then               // b) 64 - 3 = 61 max Message Data so 60 ( Data = 30) because each data is 2 bytes
  begin                                                              // Data size max is 28 for addressSize 2 or 29 for AddressSize 1
    FBoard.RaiseError(30, 'I2C:i2cWrite');
    exit;
  end;

  Result:=Request(Slave, I2C_WRITE, I2CData, restart, write);
end;

function TI2C.Read(Slave: word; regID: integer; BytesToRead: byte;
                 restart: Boolean=false; write: Boolean=True): string;
//Request(Slave, command, data, restart, write)
var
  I2CData: string;
begin
  Result:='';

  if RegID <> -1 then
    I2CData:=chr(RegID and $7F)+chr(BytesToRead)
  else
    I2CData:=chr(BytesToRead);

  if ((BytesToRead + ord(RegID <> -1)) * 2) > (MAX_DATA_BYTES - 5) then   // 64 - 5 = 59 max Message Data so 29 bytesToRead (look at TI2C.parsefirmatacommand)
  begin
    FBoard.RaiseError(29, 'I2C:i2cRead');
    exit;
  end;
  Result:=Request(Slave, I2C_READ, I2CData, restart, write);
end;

function TI2C.ReadContinuously(Slave: word; RegID: integer; BytesToRead: byte; restart: Boolean=false; write: Boolean=True): string;
//Request(Slave, command, data, restart, write)
var
  I2CData: string;
begin
  Result:='';

  if RegID <> -1 then
    I2CData:=chr(RegID and $7F)+chr(BytesToRead)
  else
    I2CData:=chr(BytesToRead);

  if (BytesToRead * 2) > (MAX_DATA_BYTES - 5) then   // 64 - 5 = 59 max Message Data so 29 bytesToRead (look at TI2C.parsefirmatacommand)
  begin
    FBoard.RaiseError(29, 'I2C:i2cReadContinuously');
    exit;
  end;

  if write then   // store for stop it when disabled if we forget to do it
  begin
    SetLength(FContinuously, Length(FContinuously) + 1);
    FContinuously[Length(FContinuously)]:=Slave;
  end;
  Result:=Request(Slave, I2C_READ_CONTINUOUSLY, I2CData, restart, write);
end;

function TI2C.StopReading(Slave: word; write: Boolean=True): string;
var
  i: integer;
//Request(Slave, command, data, restart, write)
begin
  if write then
    for i:=0 to Length(FContinuously) - 1 do
      if FContinuously[i] = Slave then
      begin
        if i < (Length(FContinuously) - 1) then
          FContinuously[i]:=FContinuously[Length(FContinuously) - 1];
        setLength(FContinuously, Length(FContinuously) - 1);
        break;
      end;
  Result:=Request(Slave, I2C_STOP_READING, '', false, write);
end;
//
//
//
{ TAccelStepper }
//
//
//
constructor TAccelStepper.Create(AOwner: TComponent);
begin
  inherited;

  FBoard:= nil;
  FEnabled:=false;

  FOnEnabled:=nil;
  FOnDisabled:=nil;

  FDevice:=0;
  FMotorPin1:=PinModesToByte(PIN_MODE_IGNORE);
  FMotorPin2:=PinModesToByte(PIN_MODE_IGNORE);
  FMotorPin3:=PinModesToByte(PIN_MODE_IGNORE);
  FMotorPin4:=PinModesToByte(PIN_MODE_IGNORE);
  FMotorEnablePin:=PinModesToByte(PIN_MODE_IGNORE);
  FInvertPin1:=false;
  FInvertPin2:=False;
  FInvertPin3:=false;
  FInvertPin4:=false;
  FInvertEnablePin:=false;
  FStepSize:=WHOLE_STEP; // default whole step
  FSpeed:=1000;
  FSteps:=0;
  FAcceleration:=40;
  FRunning:=False;
  FFastStop:=False;
  FInterfaceType:=ACCEL_INTERFACE_4_WIRE;  // default to 4 wire
  FOnStepperPosition:=nil;
  FOnStepperMoveCompleted:=nil;
end;

destructor TAccelStepper.Destroy();
begin
  inherited Destroy;
end;

procedure TAccelStepper.setBoard(Board: TBoard);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'AccelStepper:setBoard')
  else if Assigned(Board) then
    FBoard:=Board;
end;

procedure TAccelStepper.setDevice(Device: Byte);
begin
  if Device > (MAX_ACCELSTEPPER_DEVICES - 1) then
  begin
    FBoard.RaiseError(16, 'AccelStepper:setDevice');
    exit;
  end;
  if FEnabled then
  begin
    FBoard.RaiseError(33, 'AccelStepper:setDevice');
    exit;
  end;
  FDevice:=Device;
end;

function TAccelStepper.setMotorPin(MotorPin: Byte): Byte;
begin
  Result:=PinModesToByte(PIN_MODE_IGNORE);
  if MotorPin > PinModesToByte(PIN_MODE_IGNORE) then
  begin
    FBoard.RaiseError(16, 'AccelStepper:setMotorPin');
    exit;
  end;
  if FEnabled then
  begin
    FBoard.RaiseError(33, 'AccelStepper:setMotorPin');
    exit;
  end;
  Result:=MotorPin;
end;

procedure TAccelStepper.setMotorPin1(MotorPin: Byte);
begin
    FMotorPin1:=setMotorPin(MotorPin);
end;

procedure TAccelStepper.setMotorPin2(MotorPin: Byte);
begin
  FMotorPin2:=setMotorPin(MotorPin);
end;

procedure TAccelStepper.setMotorPin3(MotorPin: Byte);
begin
  FMotorPin3:=setMotorPin(MotorPin);
end;

procedure TAccelStepper.setMotorPin4(MotorPin: Byte);
begin
  FMotorPin4:=setMotorPin(MotorPin);
end;

procedure TAccelStepper.setMotorEnablePin(MotorPin: Byte);
begin
  FMotorEnablePin:=setMotorPin(MotorPin);
end;

procedure TAccelStepper.setSpeed(Speed: single);
begin
  if Speed > MAX_SPEED then
    FSpeed:=MAX_SPEED
  else
    FSpeed:=Speed;
  if FEnabled then
    MotorSpeed;  // change speed
end;

procedure TAccelStepper.setAcceleration(Value: single);
begin
  if Value > MAX_ACCELERATION then
    FAcceleration:=0.0 // Hack in AccelStepper
  else
    FAcceleration:=Value;
  if FEnabled then
    MotorAcceleration;  // change acceleration
end;

procedure TAccelStepper.setEnabled(State: Boolean);
begin
  if not Assigned(FBoard) then
    exit;

  if FEnabled = State then
     exit;

  if State then
  begin
    FRunning:=false;
    FEnabled:=True;
    if Assigned(FBoard) and FBoard.Enabled then
    begin
      // check Motorpin1 and motorpin2 capability
      if FBoard.CheckCapability(FMotorPin1, PIN_MODE_STEPPER) and FBoard.CheckCapability(FMotorPin2, PIN_MODE_STEPPER) then
      begin
        if FBoard.FBoardPins[FMotorPin1].Busy or FBoard.FBoardPins[FMotorPin2].Busy then
        begin
           FEnabled:=False;
           FBoard.RaiseError(12, 'AccelStepper:setEnabled; MotorPin1 and/or MotorPin2');
        end
        else  // motorpin1 and motorpin2 pins are free
        begin
          FBoard.FBoardPins[FMotorPin1].Busy:=True;
          FBoard.FBoardPins[FMotorPin2].Busy:=True;
          if FInterfaceType = ACCEL_INTERFACE_3_WIRE then
          begin
            if FBoard.CheckCapability(FMotorPin3, PIN_MODE_STEPPER) then
            begin
              if FBoard.FBoardPins[FMotorPin3].Busy then
              begin
                FEnabled:=False;
                FBoard.RaiseError(12, 'AccelStepper:setEnabled; MotorPin3');
              end
              else // motorpin3 is free
                FBoard.FBoardPins[FMotorPin3].Busy:=True;
            end
            else  // motorpin3 not supported
            begin
              FEnabled:=False;
              FBoard.RaiseError(11, 'AccelStepper:setEnabled; MotorPin3');
            end;
          end
          else if FInterfaceType = ACCEL_INTERFACE_4_WIRE then
          begin
            if FBoard.CheckCapability(FMotorPin3, PIN_MODE_STEPPER) and FBoard.CheckCapability(FMotorPin4, PIN_MODE_STEPPER) then
            begin
              if FBoard.FBoardPins[FMotorPin3].Busy or FBoard.FBoardPins[FMotorPin4].Busy then
              begin
                FEnabled:=False;
                FBoard.RaiseError(12, 'AccelStepper:setEnabled; MotorPin3 and/or MotorPin4');
              end
              else // pins are free
              begin
                FBoard.FBoardPins[FMotorPin3].Busy:=True;
                FBoard.FBoardPins[FMotorPin4].Busy:=True;
              end;
            end
            else  // motorpin3 and/or motorpin4 not supported
            begin
              FEnabled:=False;
              FBoard.RaiseError(11, 'AccelStepper:setEnabled; MotorPin3 and/or MotorPin4');
            end;
          end;
        end;
      end
      else // motorpin1 and/or Motorpin2 not supported
      begin
        FEnabled:=False;
        FBoard.RaiseError(11, 'AccelStepper:setEnabled; MotorPin1 and/or MotorPin2');
      end;
      if FMotorEnablePin <> PinModesToByte(PIN_MODE_IGNORE) then  // pin assigned
      begin
        if FBoard.CheckCapability(FMotorEnablePin, PIN_MODE_STEPPER) then
        begin
          if FBoard.FBoardPins[FMotorEnablePin].Busy then   // pin is not free
          begin
            FEnabled:=False;
            FBoard.RaiseError(12, 'AccelStepper:setEnabled; MotorEnablePin');
          end
          else    // pin is free
            FBoard.FBoardPins[FMotorEnablePin].Busy:=True;
        end
        else   // pin is not compatible
        begin
          FEnabled:=False;
          FBoard.RaiseError(11, 'AccelStepper:setEnabled; MotorEnablePin');
        end
      end;
      if FEnabled then
      begin
        if Assigned(FBoard.FAccelSteppers[FDevice]) then
        begin
          FEnabled:=False;
          FBoard.RaiseError(43, 'AccelStepper:setEnabled');
        end
        else // new stepper, device not found
        begin
          FBoard.FAccelSteppers[FDevice]:=self;
          Config;
          MotorSpeed; // set speed
          MotorAcceleration;  // set acceleration
        end;
      end
      else
      begin
        FBoard.FBoardPins[FMotorPin1].Busy:=false;   // free pins
        FBoard.FBoardPins[FMotorPin2].Busy:=false;
        FBoard.FBoardPins[FMotorPin3].Busy:=false;
        FBoard.FBoardPins[FMotorPin4].Busy:=false;
        FBoard.FBoardPins[FMotorEnablePin].Busy:=false;
      end;
    end
    else  // Board disabled
    begin
      FEnabled:=False;
      FBoard.RaiseError(2, 'AccelStepper:setEnabled');
    end;
  end
  else  // state is false disable
  begin
    if Assigned(FOnDisabled) then
      FOnDisabled(self);
    if FRunning then
      Stop; // send stop
    FBoard.FBoardPins[FMotorPin1].Busy:=false;   // free pins
    FBoard.FBoardPins[FMotorPin2].Busy:=false;
    FBoard.FBoardPins[FMotorPin3].Busy:=false;
    FBoard.FBoardPins[FMotorPin4].Busy:=false;
    FBoard.FBoardPins[FMotorEnablePin].Busy:=false;
    FBoard.FAccelSteppers[FDevice]:=nil;
    FEnabled:=False;
  end;
end;

// get data from firmata
procedure TAccelStepper.parsefirmatacommand(Sender: TObject; CommandData: String);
var
  DataString: string;
  Position: integer;
  Device: Byte;
begin
  {0  START_SYSEX                             (0xF0)
  1  ACCELSTEPPER_DATA                       (0x62)
  2  ACCELSTEPPER_REPORT_POSITION and ACCELSTEPPER_MOVE_COMPLETED   (0x06) and (0x0a)
  3  device number                           (0-9)
  4  position, bits 0-6
  5  position, bits 7-13
  6  position, bits 14-20
  7  position, bits 21-27
  8  position, bits 28-31
  9  END_SYSEX        (0xF7)}
  DataString:=Copy(CommandData, 3, Length(CommandData)-2);
  Position:=Decode32BitSignedInt(DataString);

  // command is first byte, Device is second byte
  Device:=ord(CommandData[2]);
  if ord(CommandData[1]) = ACCELSTEPPER_REPORT_POSITION then
  begin
    if Assigned(FBoard.FAccelSteppers[Device].FOnStepperPosition) then
        FBoard.FAccelSteppers[Device].FOnStepperPosition(self, Device, Position);
  end
  else    // ACCELSTEPPER_MOVE_COMPLETED
  begin
    FRunning:=False; // stepper has stopped
    {//if FMotorEnablePin <> PinModesToByte(PIN_MODE_IGNORE) then  // to do it automatically
        Result:=MotorEnable(False, write); }
    if Assigned(FBoard.FAccelSteppers[Device].FOnStepperMoveCompleted) then
        FBoard.FAccelSteppers[Device].FOnStepperMoveCompleted(self, Device, Position);
  end;
end;
{ Send START_SYSEX + data + END_SYSEX total <= MAX_DATA_BYTES (64 bytes)}
function TAccelStepper.SendSysEx(data: string; write: Boolean=true): string;
begin
  Result:='';
  if not FEnabled and write then
    exit;
  Result:=FBoard.SendSysEx(Data, write);
end;

//
// AccelStepper commands
//
{0  START_SYSEX                                (0xF0)
1  ACCELSTEPPER_DATA                          (0x62)
2  ACCELSTEPPER_CONFIG                        (0x00 = config)
3  device number                              (0-9) (Supports up to 10 motors)

4  interface                                  (upper 3 bits = wire count:
                                                001XXXX = driver
                                                010XXXX = two wire
                                                011XXXX = three wire
                                                100XXXX = four wire)

                                              (4th - 6th bits = step type
                                                step size = 1/2^0bXXX
                                                Examples:
                                                XXX000X = whole step
                                                XXX001X = half step
                                                XXX010X = quarter step
                                                etc...)

                                              (lower 1 bit = has enable pin:
                                                XXXXXX0 = no enable pin
                                                XXXXXX1 = has enable pin)

5  motorPin1 or stepPin number                (0-127)
6  motorPin2 or directionPin number           (0-127)
7  [when interface >= 0x011] motorPin3        (0-127)
8  [when interface >= 0x100] motorPin4        (0-127)
9  [when interface && 0x0000001] enablePin    (0-127)
10 [optional] pins to invert                  (lower 5 bits = pins:
                                                XXXXXX1 = invert motorPin1
                                                XXXXX1X = invert motorPin2
                                                XXXX1XX = invert motorPin3
                                                XXX1XXX = invert motorPin4
                                                XX1XXXX = invert enablePin)
11 END_SYSEX                                  (0xF7)}
function TAccelStepper.Config(write: Boolean=True): string;
var
  Interfacing: byte;
  InvertedPins: Byte;
  Motors: string;
begin
  Interfacing:=((ord(FInterfaceType) + 1) << 4) or (ord(FStepSize) << 1);
  InvertedPins:=byte(FInvertPin1) or (byte(FInvertPin2) << 1) or (byte(FInvertPin3) << 2) or (byte(FInvertPin4) << 3);

  Motors:=chr(FMotorPin1)+chr(FMotorPin2);

  if FInterfaceType = ACCEL_INTERFACE_3_WIRE then
    Motors:=Motors+chr(FMotorPin3)
  else if FInterfaceType = ACCEL_INTERFACE_4_WIRE then
    Motors:=Motors+chr(FMotorPin3)+chr(FMotorPin4);

  if FMotorEnablePin <> PinModesToByte(PIN_MODE_IGNORE) then
  begin
    Interfacing:=Interfacing or 1;
    InvertedPins:=InvertedPins or (Byte(FInvertEnablePin) << 4);
    Motors:=Motors+chr(FMotorEnablePin);
  end;

  if InvertedPins <> 0 then
    Result:=chr(InvertedPins)
  else
    Result:='';

  Result:=SendSysEx(chr(ACCELSTEPPER_DATA)+chr(ACCELSTEPPER_CONFIG)+chr(FDevice)+chr(Interfacing)+Motors+Result, write);

  if write then
  begin
    // update FBoardPins
    FBoard.FBoardPins[FMotorPin1].ActualMode:=PinModesToByte(PIN_MODE_STEPPER);
    FBoard.FBoardPins[FMotorPin2].ActualMode:=PinModesToByte(PIN_MODE_STEPPER);
    if FInterfaceType = ACCEL_INTERFACE_3_WIRE then
      FBoard.FBoardPins[FMotorPin3].ActualMode:=PinModesToByte(PIN_MODE_STEPPER)
    else if FInterfaceType = ACCEL_INTERFACE_4_WIRE then
    begin
      FBoard.FBoardPins[FMotorPin3].ActualMode:=PinModesToByte(PIN_MODE_STEPPER);
      FBoard.FBoardPins[FMotorPin4].ActualMode:=PinModesToByte(PIN_MODE_STEPPER);
    end;
    if FMotorEnablePin <> PinModesToByte(PIN_MODE_IGNORE) then
      FBoard.FBoardPins[FMotorEnablePin].ActualMode:=PinModesToByte(PIN_MODE_STEPPER);
      // Arduino sets Pin FMotorEnablePin as OUTPUT HIGH if it has to be LOW then invert this pin
  end;
end;
{0  START_SYSEX                            (0xF0)
1  ACCELSTEPPER_DATA                       (0x62)
2  ACCELSTEPPER_ZERO                       (0x01)
3  device number                           (0-9)
4  END_SYSEX                               (0xF7)}
function TAccelStepper.SetZero(write: Boolean=True): string;
begin
  Result:=SendSysEx(chr(ACCELSTEPPER_DATA)+chr(ACCELSTEPPER_ZERO)+chr(FDevice), write);
end;
{  START_SYSEX                             (0xF0)
1  ACCELSTEPPER_DATA                       (0x62)
2  ACCELSTEPPER_MOVE_RELATIVE             (0x02)
3  device number                           (0-9)
4  num steps, bits 0-6
5  num steps, bits 7-13
6  num steps, bits 14-20
7  num steps, bits 21-27
8  num steps, bits 28-32
9  END_SYSEX                               (0xF7)}
function TAccelStepper.Move(write: Boolean=True): string;
begin
  Result:='';
  if write then
    FRunning:=True;

  {if FMotorEnablePin <> PinModesToByte(PIN_MODE_IGNORE) then  // to do it automatically
    Result:=MotorEnable(True, write); }
  Result:=Result+SendSysEx(chr(ACCELSTEPPER_DATA)+chr(ACCELSTEPPER_MOVE_RELATIVE)+chr(FDevice)+Encode32BitSignedInt(FSteps), write);
end;
{0  START_SYSEX                            (0xF0)
1  ACCELSTEPPER_DATA                       (0x62)
2  ACCELSTEPPER_MOVE_ABSOLUTE              (0x03)
3  device number                           (0-9)
4  position, bits 0-6
5  position, bits 7-13
6  position, bits 14-20
7  position, bits 21-27
8  position, bits 28-32
9 END_SYSEX                               (0xF7)}
function TAccelStepper.MoveTo(write: Boolean=True): string;
begin
  Result:='';
  if write then
    FRunning:=True;

  {if FMotorEnablePin <> PinModesToByte(PIN_MODE_IGNORE) then  // to do it automatically
    Result:=MotorEnable(True, write); }
  Result:=Result+SendSysEx(chr(ACCELSTEPPER_DATA)+chr(ACCELSTEPPER_MOVE_ABSOLUTE)+chr(FDevice)+Encode32BitSignedInt(FSteps), write);
end;
{0  START_SYSEX                             (0xF0)
1  ACCELSTEPPER_DATA                       (0x62)
2  ACCELSTEPPER_ENABLE                     (0x04)
3  device number                           (0-9)
4  device state                            (HIGH : enabled | LOW : disabled)
5  END_SYSEX                               (0xF7)}
function TAccelStepper.MotorEnable(State: Boolean; write: Boolean=True): string;
begin
  if FMotorEnablePin <> PinModesToByte(PIN_MODE_IGNORE) then
    Result:=SendSysEx(chr(ACCELSTEPPER_DATA)+chr(ACCELSTEPPER_ENABLE)+chr(FDevice)+chr(ord(State)), write);
end;
{0  START_SYSEX                             (0xF0)
1  ACCELSTEPPER_DATA                       (0x62)
2  ACCELSTEPPER_STOP                       (0x05)
3  device number                           (0-9)
4  END_SYSEX                             (0xF7)}
function TAccelStepper.Stop(write: Boolean=True): string;
begin
  Result:=SendSysEx(chr(ACCELSTEPPER_DATA)+chr(ACCELSTEPPER_STOP)+chr(FDevice), write);
end;
// faststop implemented
function TAccelStepper.FastStop(write: Boolean=True): string;
begin
  if write then // prepare result value for task
    FFastStop:=true;
  Result:=MotorAcceleration(0.0, write)+Stop(write)+MotorAcceleration(FAcceleration, write);  // stops with max decceleration and recover acceleration
end;

{0  START_SYSEX                             (0xF0)
1  ACCELSTEPPER_DATA                       (0x62)
2  ACCELSTEPPER_REPORT_POSITION            (0x06)
3  device number                           (0-9)
4  END_SYSEX                               (0xF7)}
function TAccelStepper.ReportPosition(write: Boolean=True): string;
begin
  Result:=SendSysEx(chr(ACCELSTEPPER_DATA)+chr(ACCELSTEPPER_REPORT_POSITION)+chr(FDevice), write);
end;
//
//TODO
// Stepper limit, not yet implemented in ConfigurableFirmata
//
{0  START_SYSEX                             (0xF0)
1  ACCELSTEPPER_DATA                       (0x62)
2  ACCELSTEPPER_STOP_LIMIT                 (0x07)
3  device number                           (0-9)
4  lower limit pin number                  (0-127)
5  lower limit state                       (0x00 | 0x01)
6  upper limit pin number                  (0-127)
7  upper limit state                       (0x00 | 0x01)
8  END_SYSEX                               (0xF7)}
// not yet implemented

//
{0  START_SYSEX                             (0xF0)
1  ACCELSTEPPER_DATA                       (0x62)
2  ACCELSTEPPER_SET_ACCELERATION           (0x08)
3  device number                           (0-9) (Supports up to 10 motors)
4  accel, bits 0-6                         (acceleration in steps/sec^2)
5  accel, bits 7-13
6  accel, bits 14-20
7  accel, bits 21-27
8  END_SYSEX                               (0xF7)}

function TAccelStepper.MotorAcceleration(write: Boolean=True): string;   // uses FAcceleration
begin
  Result:=SendSysEx(chr(ACCELSTEPPER_DATA)+chr(ACCELSTEPPER_SET_ACCELERATION)+chr(FDevice)+EncodeAccelFloat(FAcceleration), write);
end;

function TAccelStepper.MotorAcceleration(Acceleration: single; write: Boolean=True): string; overload;  // uses Acceleration
begin
  Result:=SendSysEx(chr(ACCELSTEPPER_DATA)+chr(ACCELSTEPPER_SET_ACCELERATION)+chr(FDevice)+EncodeAccelFloat(Acceleration), write);
end;
{0  START_SYSEX                             (0xF0)
1  ACCELSTEPPER_DATA                       (0x62)
2  ACCELSTEPPER_SET_SPEED                  (0x09)
3  device number                           (0-9) (Supports up to 10 motors)
4  maxSpeed, bits 0-6                      (maxSpeed in steps per sec)
5  maxSpeed, bits 7-13
6  maxSpeed, bits 14-20
7  maxSpeed, bits 21-27
8  END_SYSEX                               (0xF7)}
function TAccelStepper.MotorSpeed(write: Boolean=True): string;
begin
  Result:=SendSysEx(chr(ACCELSTEPPER_DATA)+chr(ACCELSTEPPER_SET_SPEED)+chr(FDevice)+EncodeAccelFloat(FSpeed), write);
end;
//
//
//
{ TAccelStepperGroup }
//
//
//
constructor TAccelStepperGroup.Create(AOwner: TComponent);
begin
  inherited;

  FBoard:= nil;
  FEnabled:=false;

  FOnEnabled:=nil;
  FOnDisabled:=nil;

  FGroup:=0;
  FMembers:=nil;
  FOnAccelStepperMultiMoveCompleted:=nil;
end;

destructor TAccelStepperGroup.Destroy();
begin
  inherited Destroy;
end;

function TAccelStepperGroup.getMember(Index: integer): TMember;
begin
  if (Index < Length(FMembers)) and (Index >= 0) then
    Result:=FMembers[Index]
  else
    Result:=default(TMember);
end;

procedure TAccelStepperGroup.AddMember(FirmataAccelStepper: TAccelStepper);
var
  i: integer;
  exists: Boolean;
begin
  if FEnabled then
  begin
    FBoard.RaiseError(33, 'AccelStepperGroup:AddMember');
    exit;
  end;
  if Length(FMembers) = MAX_ACCELSTEPPER_DEVICES then // no more room for members
  begin
    FBoard.RaiseError(38, 'AccelStepperGroup:AddMember');
    exit;
  end;

  exists:=False;
  for i:=0 to Length(FMembers) - 1 do  // check for duplicate one
  begin
    if FMembers[Length(FMembers)].Device = FirmataAccelStepper.FDevice then
    begin
      exists:=true;
      FBoard.RaiseError(39, 'AccelStepperGroup:AddMember');
      break;
    end;
  end;
  if not exists then // new member
  begin
    SetLength(FMembers, Length(FMembers) + 1);
    FMembers[Length(FMembers)].Device:=FirmataAccelStepper.FDevice;
    FMembers[Length(FMembers)].Member:=FirmataAccelStepper;
  end;
end;

procedure TAccelStepperGroup.DeleteMember(FirmataAccelStepper: TAccelStepper);
var
  i, j: integer;
  exists: Boolean;
begin
  if FEnabled then
  begin
    FBoard.RaiseError(33, 'AccelStepperGroup:DeleteMember');
    exit;
  end;
  exists:=False;
  j:=Length(FMembers) - 1;
  for i:=0 to j do
    if FMembers[i].Device = FirmataAccelStepper.FDevice then // exists
    begin
      if i < j then
      begin
        FMembers[i].Device:=FMembers[j].Device;  // copy last element in this position
        FMembers[i].Member:=FMembers[j].Member;
      end;
      SetLength(FMembers, j);                // new array size
      exists:=true;
      break;
    end;

  if not exists then // not exist
    FBoard.RaiseError(40, 'AccelStepperGroup:DeleteMember');
end;

procedure TAccelStepperGroup.setBoard(Board: TBoard);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'AccelStepperGroup:setBoard')
  else if Assigned(Board) then
    FBoard:=Board;
end;

procedure TAccelStepperGroup.setGroup(Group: Byte);
begin
  if FEnabled then
  begin
    FBoard.RaiseError(33, 'AccelStepperGroup:setGroup');
    exit;
  end;
  if Group > (MAX_ACCELSTEPPER_MULTI - 1) then
  begin
    FBoard.RaiseError(16, 'AccelStepperGroup:setGroup');
    exit;
  end;
  FGroup:=Group;
end;

procedure TAccelStepperGroup.setEnabled(State: Boolean);
begin
    if not Assigned(FBoard) then
      exit;

    if FEnabled = State then
       exit;

    if State then
    begin
      FEnabled:=True;
      if Assigned(FBoard) and FBoard.Enabled then
      begin
        if Length(FMembers) > 0 then  // there is a member
        begin
          FBoard.FAccelStepperGroup:=self;
          if Assigned(FOnEnabled) then
            FOnEnabled(self);
        end
        else  // no members
        begin
          FEnabled:=False;
          FBoard.RaiseError(41, 'AccelStepperGroup:setEnabled');
          exit;
        end;
      end
      else  // firmata is not enabled
      begin
        FEnabled:=False;
        FBoard.RaiseError(36, 'AccelStepperGroup:setEnabled');
      end;
    end
    else  // state is false disable
    begin
      if Assigned(FOnDisabled) then
        FOnDisabled(self);
      if Assigned(FBoard) then
      begin
        StepperMultiStop;
        FBoard.FAccelStepperGroup:=nil;
      end;
      FEnabled:=False;
    end;
end;
{ Send START_SYSEX + data + END_SYSEX total <= MAX_DATA_BYTES (64 bytes)}
function TAccelStepperGroup.SendSysEx(data: string; write: Boolean=true): string;
begin
  Result:='';
  if not FEnabled and write then
    exit;
  Result:=FBoard.SendSysEx(Data, write);
end;

// get data from firmata
procedure TAccelStepperGroup.parsefirmatacommand(Sender: TObject; CommandData: String);
var
  group: Byte;
begin
    {0  START_SYSEX                             (0xF0)
     1  ACCELSTEPPER_DATA                       (0x62)
     2  multi stepper move complete command     (0x24)
     3  group  number                           (0-4)
     4  END_SYSEX(0xF7)}

     // first byte is subcommand and second byte is Group number
    Group:=ord(CommandData[2]);  // drop command first byte
    if Assigned(FOnAccelStepperMultiMoveCompleted) then
           FOnAccelStepperMultiMoveCompleted(self, Group);
end;
{0  START_SYSEX                              (0xF0)
1  ACCELSTEPPER_DATA                        (0x62)
2  ACCELSTEPPER_MULTI_CONFIG                (0x20)
3  group number                             (0-4)
4  member 0x00 device number                (0-9)
5  member 0x01 device number                (0-9)
6  [optional] member 0x02 device number     (0-9)
7  [optional] member 0x03 device number     (0-9)
8  [optional] member 0x04 device number     (0-9)
9  [optional] member 0x05 device number     (0-9)
10 [optional] member 0x06 device number     (0-9)
11 [optional] member 0x07 device number     (0-9)
12 [optional] member 0x08 device number     (0-9)
13 [optional] member 0x09 device number     (0-9)
14 END_SYSEX                                (0xF7)}
function TAccelStepperGroup.StepperMultiConfig(write: Boolean=True): string;
var
  i: integer;
begin
  Result:='';
  if Length(FMembers) < 1 then
  begin
    FBoard.RaiseError(41, 'AccelStepperGroup:StepperMultiConfig');
    exit;
  end;
  for i:=0 to Length(FMembers)-1 do
    Result:=chr(FMembers[i].Device)+Result;

  Result:=SendSysEx(chr(ACCELSTEPPER_DATA)+chr(ACCELSTEPPER_MULTI_CONFIG)+chr(FGroup)+Result, write);
end;

{0  START_SYSEX                              (0xF0)
1  ACCELSTEPPER_DATA                        (0x62)
2  ACCELSTEPPER_MULTI_TO                    (0x21)
3  group number                             (0-4)
4  position, bits 0-6
5  position, bits 7-13
6  position, bits 14-20
7  position, bits 21-27
8  position, bits 28-31

*Repeat 4 through 8 for each device in group*

53 END_SYSEX                                (0xF7)}
function TAccelStepperGroup.StepperMultiTo(Position: integer; write: Boolean=True): string;
begin
  Result:=SendSysEx(chr(ACCELSTEPPER_DATA)+chr(ACCELSTEPPER_MULTI_TO)+chr(FGroup)+Encode32BitSignedInt(Position), write);
end;
{0  START_SYSEX                             (0xF0)
1  ACCELSTEPPER_DATA                       (0x62)
2  ACCELSTEPPER_MULTI_STOP                 (0x23)
3  group number                            (0-4)
4  END_SYSEX                               (0xF7)}
function TAccelStepperGroup.StepperMultiStop(write: Boolean=True): string;
begin
  Result:='';
  if Group > 4 then
  begin
    FBoard.RaiseError(16, 'AccelStepperGroup:StepperMultiStop');
    exit;
  end;

  Result:=SendSysEx(chr(ACCELSTEPPER_DATA)+chr(ACCELSTEPPER_STOP)+chr(FGroup), write);
end;

{$include 'firmataboard1.inc'};

end.


