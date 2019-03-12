unit firmataboard;
//{$mode objfpc}{$H+}
{$mode delphi}{$H+}

interface

uses
{$IFDEF LINUX}
  Classes,
{$IFDEF UseCThreads}
  cthreads,
{$ENDIF}
{$ELSE}
  Windows, Classes, //registry,
{$ENDIF}
 ExtCtrls, sysUtils, forms, firmataconstants,
  StdCtrls, dialogs,  LResources, LclIntf, math;

 type
  // Digital Pin register type

  TCapability = record
    Mode: Byte;         // pin mode
    Resolution: Byte;   // Bits of resolution
  end;

  TPinModes = (PIN_MODE_INPUT, PIN_MODE_OUTPUT, PIN_MODE_ANALOG, PIN_MODE_PWM, PIN_MODE_SERVO,
           PIN_MODE_SHIFT, PIN_MODE_I2C, PIN_MODE_ONEWIRE, PIN_MODE_STEPPER, PIN_MODE_ENCODER,
           PIN_MODE_SERIAL, PIN_MODE_PULLUP, PIN_MODE_IGNORE);

  TSerialPorts = (HW_SERIAL0, HW_SERIAL1, HW_SERIAL2, HW_SERIAL3,
                  SW_SERIAL0, SW_SERIAL1, SW_SERIAL2, SW_SERIAL3);
  TSerialPinType = (RX, TX);

  TCapabilities = array of TCapability;

  TBoardPin = record
       ActualMode: Byte;
       Busy: Boolean;      // if busy then is assigned to a module
       AnalogMap: Byte;     // analog pin mapping
       Capabilities: TCapabilities; // supported modes and resolutions
  end;

  TBoardPins = Array of TBoardPin;   // array of pins

  TOnBeforeOpen = procedure(sender: TObject) of Object;
  TOnAfterClose = procedure(sender: TObject) of Object;
  TOnSendDataToDevice = procedure(sender: TObject; str: string) of Object;
  TOnGetDataFromDevice = function(sender: TObject): integer of object;
  TOnDeviceDataAvailable = function(sender: TObject): Boolean of object;
  TOnError = procedure(sender: TObject; Error: integer; TextError: string) of Object;
  TOnBoardReady = procedure(sender: TObject) of Object;
  TOnBoardData = procedure(sender: TObject; Command: Byte; Data: string) of object;
  TOnPinValue = procedure(sender: TObject; Value: integer) of object;
  TOnPinState = procedure(sender: TObject; Mode: TPinModes; State: integer) of object;
  TOnExtendedSysex = procedure(sender: TObject; ID0: byte; ID1: byte; Data: String) of Object;

  TOnEnabled = procedure(sender: TObject) of Object;
  TOnDisabled = procedure(sender: TObject) of Object;

  TOnTaskError = procedure(sender: TObject; Time: integer; Length: integer; Place: integer; TaskData: String) of object;
  TOnQueryAllTask = procedure(sender: TObject; TaskIDs: array of byte) of object;
  TOnQueryTask = procedure(sender: TObject; Time: integer; Length: integer; Place: integer; TaskData: String) of object;
  TOnSearch = procedure(sender: TObject; Pin: Byte; DeviceIDs: array of string) of Object;
  TOnOneWireAlarm = procedure(sender: TObject; Pin: Byte; AlarmIDs: array of string) of Object;
  TOnOneWireData = procedure(sender: TObject; Pin: Byte; Data: string) of Object;
  TOnI2CData = procedure(sender: TObject; Slave: Byte; Reg_Number: Byte; Data: string) of Object;
  TOnStepperPosition = procedure(sender: TObject; Device: byte; Position: integer) of Object;
  TOnAccelStepperMultiMoveCompleted = procedure(sender: TObject; Group: byte) of Object;
  TOnSerialMessage = procedure(sender: TObject; Data: string) of Object;
  TOnEncoderPosition = procedure(sender: TObject; Direction: integer; Position: integer) of Object;

  TBoard = class;

  TBoardThread = class(TThread)
   public
     MustDie: boolean;
     Owner: TBoard;
   protected
     procedure CallEvent;
     procedure Execute; override;
   published
     property Terminated;
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

  TPins = array of TPin;
  TTasks = array [0..128] of TTask;
  TAccelSteppers = array [0..MAX_ACCELSTEPPER_DEVICES - 1] of TAccelStepper;
  TOneWires = array of TOneWire;
  TSerials = array [TSerialPorts] of TSerial; // max 8 serial ports
  TServos = array [0..MAX_SERVOS - 1] of TServo;
  TEncoders = array [0..MAX_ENCODERS - 1] of TEncoder;

  TBoard = class (TComponent)
    private
      FBoardThread: TBoardThread;
      FEnabled: Boolean;
      FStarting: Boolean;
      FGotCapabilities: Boolean;
      FGotFirmware: Boolean;
      FGotVersion: Boolean;
      FSamplingInterval: integer;

      FOnBoardData: TOnBoardData; // only when there are firmata data and no other event
      FOnPinValue: TOnPinValue;
      FOnPinState:   TOnPinState;
      FOnBeforeOpen: TOnBeforeOpen;
      FOnAfterClose: TOnAfterClose;
      FOnError: TOnError;
      FOnSendDataToDevice: TOnSendDataToDevice;
      FOnGetDataFromDevice: TOnGetDataFromDevice;
      FOnDeviceDataAvailable: TOnDeviceDataAvailable;
      FOnExtendedSysex: TOnExtendedSysex;

    private
      FLastError: integer;
      FBoardPins: TBoardPins;  // array of Pins capabilities, values, etc.
      FBoardPinsNumber: integer;  // Number of total pins
      FAnalogPinsNumber: integer;  //number of analog pins
      FBoardVersion: array [0..1] of byte;
      FBoardStringFirmware: string;
      FBoardFirmware: array [0..1] of byte;

      FOnQueryAllTask: TOnQueryAllTask;

      FEndCommand: Boolean;
      FOnBoardReady: TOnBoardReady;
      FInitTime: QWord;
      FTimeEnd: QWord;
      FStartedTime: QWord;
      // Firmata Modules
      FPins: TPins;
      FTasks: TTasks;
      FOneWires: TOneWires;
      FI2C: TI2C;
      FAccelSteppers: TAccelSteppers;
      FAccelStepperGroup: TAccelStepperGroup;
      FSerials: TSerials;
      FServos: TServos;
      FEncoders: TEncoders;

      procedure setEnabled(State: Boolean);
      procedure initBoardVariables;
      procedure InternalBoardReady;
      procedure setTimeEnd(TimeToWait: Qword);  // milisec to wait for firmata

      function GetBoardPin(Index: Integer): TBoardPin;
      procedure SetBoardPin(Index: integer; Pin: TBoardPin);
      procedure SetSamplingInterval(Interval: integer);

      procedure RaiseError(Error: integer; FunctionError: string; TextError: string='');
    public
      function GetNextByte: Byte;
      procedure GetFirmataCommand(Sender: TObject);

      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      // Firmata comands
      // All firmata functions for commands return the string command sent, they have a write parameter, if write is True the data is sent to the external device
      // if write is false then is not sent to device
      //
      function BoardReset(write: Boolean=true): string;
      function askVersion(write: Boolean=true): string;
      function askFirmware(write: Boolean=true): string;
      function askBoardCapabilities(write: Boolean=true): string;
      function SendSamplingInterval(Interval: integer; write: Boolean=true): string;
      function QueryAllTasks(write: Boolean=true): string;
      function SchedulerReset(write: Boolean=true): string;
      // send commands
      function SendCommand(Data: string; write: Boolean=True): string;
      function SendSysEx(data7bit: string; write: Boolean=true): string;

      // Getting firmata data
      procedure GetSysExCommandData(Command: Byte);

      procedure Reset;  // Reset CustomFirmata
      procedure Open;
      procedure Close;

      function GetPinFromAnalogPin(AnalogPin: byte): Byte;
      function CheckCapability(Pin: byte; Mode: TPinModes): Boolean;
      function CheckReportPort(Pin: Byte): Boolean; // return true if there is another digital pin in the port reporting
      function GetPinResolution(Pin: Byte; Mode: TPinModes): Integer;  // get pin resolution in a mode
      procedure printPinInfo(Memo: TMemo);
      property BoardPins[Index:Integer]: TBoardPin read GetBoardPin write SetBoardPin;
      property Enabled: Boolean read FEnabled write SetEnabled;
    published
      property SamplingInterval: integer read FSamplingInterval write SetSamplingInterval;
      property LastError: integer read FLastError write FLastError;
      property PinsNumber: integer read FBoardPinsNumber;
      property AnalogPinsNumber: integer read FAnalogPinsNumber;
      property TimeStarting: Qword read FTimeEnd write setTimeEnd;  // max time to wait starting
      property TimeToStart: Qword read FStartedTime;  // time spend starting firmata
      property FirmataFirmware: string read FBoardStringFirmware;
      property OnBeforeOpen: TOnBeforeOpen read FOnBeforeOpen write FOnBeforeOpen;
      property OnAfterClose: TOnAfterClose read FOnAfterClose write FOnAfterClose;
      property OnBoardReady: TOnBoardReady read FOnBoardReady write FOnBoardReady;
      property OnSendDataToDevice: TOnSendDataToDevice read FOnSendDataToDevice write FOnSendDataToDevice;
      property OnGetDataFromDevice: TOnGetDataFromDevice read FOnGetDataFromDevice write FOnGetDataFromDevice;
      property OnDeviceDataAvailable: TOnDeviceDataAvailable read FOnDeviceDataAvailable write FOnDeviceDataAvailable;
      property OnError: TOnError read FOnError write FOnError;
      property OnBoardData: TOnBoardData read FOnBoardData write FOnBoardData;
      property OnQueryAllTask: TOnQueryAllTask read FOnQueryAllTask write FOnQueryAllTask;
  end;

  TPin = class (TComponent)
    private
      FBoard: TBoard;
      FEnabled: Boolean;
      FPin: Byte;
      FValue: integer;
      FMode: TPinModes;
      FState: integer;
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
    public
      // digital ports
      function DigitalWritePort(Port: byte; Value: integer; write: Boolean=true): string;
      // pins general
      function askPinState(write: Boolean=true): string;
      function SetPinMode(write: Boolean=true): string; // if write true then send bytes to device
      function DigitalReport(enabled: boolean; write: Boolean=true): string;
      // digital pins
      function DigitalWrite(Value: Byte; write: Boolean=true): string;  //Same as SetDigitalPinValue
      function SetDigitalPinValue(Value: Byte; write: Boolean=true): string;   // only set value for one digital pin
      // Analog pins
      function AnalogWrite(Value: integer; write: Boolean=true): string;
      function AnalogWriteExtended(Value: integer; write: Boolean=true): string;  // analog write (PWM, Servo, etc) to any pin
      function AnalogReport(enabled: boolean; write: Boolean=true): string;
      // get command
      procedure GetAnalogMessage(Value: integer);
      // send commands
      function SendCommand(Data: string; write: Boolean=True): string;
      function SendSysEx(data7bit: string; write: Boolean=true): string;

      // enable report for digital pin or analog pin
      function ReportPin(Enabled: boolean; write: Boolean=true): string;
      function WriteValue(Value: integer; write: Boolean=True): string;

      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      property Enabled: Boolean read FEnabled write SetEnabled;
    published
      property Board: TBoard read FBoard write setBoard;
      property Pin: byte read FPin write setPin;
      property Mode: TPinModes read FMode write setMode;
      property Value: integer read FValue;
      Property Reporting: Boolean read FReporting;
      property State: integer read FState;
      property OnPinValue: TOnPinValue read FOnPinValue write FOnPinValue;
      property OnPinState: TOnPinState read FOnPinState write FOnPinState;
  end;

  TTask = class (TComponent)
    private
      FBoard: TBoard;
      FEnabled: Boolean;
      FTaskID: Byte;  // Task ID

      FDataTask: String;
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

      function SendSysEx(data7bit: string; write: Boolean=true): string;

      function ScheduleTask(write: Boolean=true): string;
    public
      procedure GetFirmataCommand(Sender: TObject; CommandData: String);
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function CreateTask(write: Boolean=true): string;
      function DeleteTask(write: Boolean=true): string;
      function AddToTask(Data7Bit: String; write: Boolean=true): string;
      function QueryTask(write: Boolean=true): string;
      function DelayTask(write: Boolean=true): string;

      property Enabled: Boolean read FEnabled write SetEnabled;
    published
      property TaskID: Byte read FTaskID write setTask;
      property DataTask: string read FDataTask write FDataTask;
      property RunOnce: Boolean read FRunOnce write FRunOnce;  // task is only run once
      property TimeDelay: integer read FTimeDelay write setTimeDelay;  // delay for task
      property RunDelay: integer read FRunDelay write setRunDelay;  // delay for firts run
      property Board: TBoard read FBoard write setBoard;
      property OnEnabled: TOnEnabled read FOnEnabled write FOnEnabled;
      property OnDisabled: TOnDisabled read FOnDisabled write FOnDisabled;
      property OnTaskError: TOnTaskError read FOnTaskError write FOnTaskError;
      property OnQueryTask: TOnQueryTask read FOnQueryTask write FOnQueryTask;
  end;

  TOneWire = class (TComponent)
    private
      FBoard: TBoard;
      FEnabled: Boolean;
      FOneWirePin: Byte;
      FDevice: string;
      FParasitisticPower: Boolean;

      FOnEnabled: TOnEnabled;
      FOnDisabled: TOnDisabled;
      FOnOneWireData:  TOnOneWireData;
      FOnSearch: TOnSearch;
      FOnOneWireAlarm: TOnOneWireAlarm;

      procedure setOneWirePin(Pin: Byte);
      procedure setBoard(Board: TBoard);
      procedure setEnabled(State: Boolean);

      function SendSysEx(data7bit: string; write: Boolean=true): string;
    public
      procedure GetFirmataCommand(Sender: TObject; CommandData: String);
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      // Firmata comands
      // All firmata functions for commands return the string command sent, they have a write parameter, if write is True the data is sent to the external device
      // if write is false then is not sent to device
      // Onewire functions
      function config(write: Boolean=true): string;
      function Search(write: Boolean=true): string;
      function AlarmSearch(write: Boolean=true): string;
      function Reset(write: Boolean=true): string;
      function Skip(write: Boolean=true): string;
      function Select(Device: string; write: Boolean=true): string;
      function ResetAndSelect(Device: string; write: Boolean=true): string;
      function Read(BytestoRead: uint16; Correlation: uint16; write: Boolean=true): string;
      function Delay(Delay: integer; write: Boolean=true): string;
      function Write(Data: string; write: Boolean=true): string;
      function Write(Delay: integer; Data: string; write: Boolean=true): string; overload;
      function WriteAndRead(BytestoRead: uint16; Correlation: uint16; Data: string; write: Boolean=true): string;
      function WriteAndRead(BytestoRead: uint16; Correlation: uint16;
                                       Delay: integer; Data: string; write: Boolean=true): string; overload;
      function SendCommands(Command: Byte; Device: String; numBytesToRead: uint16; correlationId: uint16;
                                       delay: integer; dataToWrite: String; write: Boolean=true): string;

      property Enabled: Boolean read FEnabled write SetEnabled;
    published
      property OneWirePin: Byte read FOneWirePin write setOneWirePin;
      property Board: TBoard read FBoard write setBoard;
      property OnEnabled: TOnEnabled read FOnEnabled write FOnEnabled;
      Property Device: string read FDevice write FDevice;
      property OnDisabled: TOnDisabled read FOnDisabled write FOnDisabled;
      property OnOneWireData: TOnOneWireData read FOnOneWireData write FOnOneWireData;
      property OnSearch: TOnSearch read FOnSearch write FOnSearch;
      property OnOneWireAlarm: TOnOneWireAlarm read FOnOneWireAlarm write FOnOneWireAlarm;
      property ParasitisticPower: Boolean read FParasitisticPower write FParasitisticPower;
  end;

  TI2C = class (TComponent)
    private
      FBoard: TBoard;
      FEnabled: Boolean;

      FI2Cconfig: string;
      FDelay: byte;
      FI2CQueries: Byte;
      FSDApin: Byte;
      FSCLpin: Byte;

      FContinuously: array of byte; // store slaves in read continuosly
      FContinuously10Bits: array of byte; // store slaves in read continuosly

      FOnEnabled: TOnEnabled;
      FOnDisabled: TOnDisabled;
      FOnI2CData: TOnI2CData;

      procedure setSDAPin(SDApin: Byte); // set SDA pin
      procedure setSCLPin(SCLpin: Byte); // set SCL pin
      procedure setBoard(Board: TBoard);
      procedure setEnabled(State: Boolean);

      function SendSysEx(data7bit: string; write: Boolean=true): string;

    public
      procedure GetFirmataCommand(Sender: TObject; CommandData: String);
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      // Firmata comands
      // All firmata functions for commands return the string command sent, they have a write parameter, if write is True the data is sent to the external device
      // if write is false then is not sent to device
      // I2c commands
      function Config(write: Boolean=true): string; overload;  // delay
      function Config(Delay: byte; write: Boolean=True): string; overload;  // delay
      function Config(delay: byte; data: string; write: Boolean=true): string; overload;
      function Config(data: string; write: Boolean=true): string; overload;
      function Request(Slave: word; command: byte; data: string; restart: Boolean=false; mode10bit: boolean=false; write: Boolean=True): string;
      function WriteData(Slave: Byte; Address: integer; AddressSize: Byte; Data: String; restart: Boolean=false; write: Boolean=True): string;
      function Read(Slave: Byte; regID: integer; BytesToRead: byte; restart: Boolean=false; write: Boolean=true): string;
      function RandomRead(Slave: Byte; Address: word; AddressSize: Byte; BytesToRead: byte; restart: Boolean=false; write: Boolean=True): string;
      function ReadContinuously(Slave: Byte; RegID: integer; BytesToRead: byte; restart: Boolean=false; write: Boolean=true): string;
      function StopReading(Slave: Byte; write: Boolean=true): string;
      // i2c 10 bits functions, not implemented yet on Firmata
      function WriteData10Bits(Slave: uint16; Address: integer; AddressSize: Byte; Data: String; restart: Boolean=false; write: Boolean=True): string;
      function Read10bits(Slave: uint16; regID: integer; BytesToRead: byte; restart: Boolean=false; write: Boolean=true): string;
      function RandomRead10bits(Slave: Byte; Address: word; AddressSize: Byte; BytesToRead: byte; restart: Boolean=false; write: Boolean=True): string;
      function Read10bitsContinuously(Slave: uint16; RegID: integer; BytesToRead: byte; restart: Boolean=false; write: Boolean=true): string;
      function StopReading10Bits(Slave: uint16; write: Boolean=True): string;
      // not published
      property Enabled: Boolean read FEnabled write SetEnabled;
      property SDApin: Byte read FSDApin; // for now is read only,  write setSDAPin;  // fixed pin in arduino
      property SCLpin: Byte read FSCLpin; // for now is read only,  write setSCLPin;  // fixed pin in arduino
    published
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

       function SendSysEx(data7bit: string; write: Boolean=true): string;
     public
       procedure GetFirmataCommand(Sender: TObject; CommandData: String);
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

      function SendSysEx(data7bit: string; write: Boolean=true): string;

    public
      procedure GetFirmataCommand(Sender: TObject; commandData: String);
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

        function SendSysEx(data7bit: string; write: Boolean=true): string;
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
      FValue: integer;

      FOnEnabled: TOnEnabled;
      FOnDisabled: TOnDisabled;

      function AnalogWrite(Value: integer; write: Boolean=true): string;
      function AnalogWriteExtended(Value: integer; write: Boolean=true): string;
      procedure setEnabled(State: Boolean);
      procedure setPin(pin: Byte);
      procedure setMinPulse(Pulse: integer);
      procedure setMaxPulse(Pulse: integer);
      procedure setBoard(Board: TBoard);
      procedure setValue(Value: integer);
    public
      function WriteValue(write: Boolean=true): string;

      function SendCommand(Data: string; write: Boolean=True): string;
      function SendSysEx(data7bit: string; write: Boolean=true): string;

      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function config(write: Boolean=true): string;

      property Enabled: Boolean read FEnabled write SetEnabled;
    published
      Property Device: Byte read FDevice;
      property Board: TBoard read FBoard write setBoard;
      property Pin: byte read FPin write setPin;
      property MinPulse: integer read FMinPulse write setMinPulse;
      Property MaxPulse: integer read FMaxPulse write setMaxPulse;
      property Value: integer read FValue write setValue;
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

      function SendSysEx(data7bit: string; write: Boolean=true): string;

    public
      procedure GetFirmataCommand(Sender: TObject; CommandData: String);
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

// Utils functions
function Encode1ByteCharTo2(Data1Byte: string): string;
function Decode2BytesCharTo1(Data2bytes: string): string;
function StrToHex(const Buffer: string): string;   // Return a hex string from a byte string
function StrToHexSep(const Value: string): string;  // Return a separated hex string from a byte string
function Decode7To8bit(Data7bit: string): String;
function Encode8To7Bit(Data8bit:string): string;
function DecodeNBytesToInt(DataString: String): integer;        // converts a N bytes string, low byte first, into a positive integer
function encode32BitSignedInt(Value: Integer): String;             // converts a int value into a byte string low byte first
function decode32BitSignedInt(StrEncoded: string): integer;        // converts a string of bytes, low byte first, into a int value
function CheckCRC8(Buffer: String): Boolean; // check crc last byte is CRC8 to check
function CheckCRC16(Buffer: String): Boolean;  // check crc last 2 bytes are CRC16 to check
function CalculateCRC8(Buffer: String): Byte;
function CalculateCRC16(Buffer: String): uint16;
function EncodeAccelFloat(Value: Double): String;   // encode double into accelstepper float (4 bytes, only 28 first bits are used)
function DecodeAccelFloat(Data7Bit: string): Double;    // decode 4 bytes float (only 28 first bits are used) into a double;
procedure CloseApplication;  // terminate application
function ByteToPinModes(Value: Byte): TPinModes;
function PinModesToByte(PinMode: TPinModes): Byte;
function ByteToSerialPorts(Value: Byte): TSerialPorts;
function SerialPortsToByte(SerialPort: TSerialPorts): Byte;

const
  PinModesString: array[TPinModes] of string = ('INPUT', 'OUTPUT', 'ANALOG', 'PWM', 'SERVO',
                                      'SHIFT', 'I2C', 'ONEWIRE', 'STEPPER', 'ENCODER',
                                      'SERIAL', 'PULLUP', 'IGNORE PIN');

procedure Register;


implementation

uses
  LazarusPackageIntf;

procedure Register;
begin
  {$I firmataboard.lrs}
  RegisterComponents('Firmata', [TBoard, TPin, TTask, TOneWire, TI2C,
                   TAccelStepper, TAccelStepperGroup, TSerial, TServo, TEncoder]);
end;

//
//
// utils functions
//
//
procedure CloseApplication;
begin
  Application.Terminate; // cierra programa
  while not Application.Terminated do   // comprueba que realmente termina
    Application.ProcessMessages;
end;
// byte value to PinModes
function ByteToPinModes(Value: Byte): TPinModes;
var
  i: TPinModes;
begin
  Result:=PIN_MODE_IGNORE;
  for i:=system.Low(TPinModes) to system.High(TPinModes) do
  begin
    if Value = PinModesToByte(i) then
    begin
      Result:=i;
      Break;
    end;
  end;
end;
// PinModes to Byte
function PinModesToByte(PinMode: TPinModes): Byte;
begin
  if PinMode <> PIN_MODE_IGNORE then
    Result:=ord(PinMode)
  else
    Result:=$7F;
end;
// byte value to SerialPorts
function ByteToSerialPorts(Value: Byte): TSerialPorts;
var
  i: TSerialPorts;
begin
  for i:=system.Low(TSerialPorts) to system.High(TSerialPorts) do
  begin
    if Value = SerialPortsToByte(i) then
    begin
      Result:=i;
      Break;
    end;
  end;
end;
// SerialPorts to Byte
function SerialPortsToByte(SerialPort: TSerialPorts): Byte;
begin
  if SerialPort < SW_SERIAL0 then
    Result:=ord(SerialPort)
  else
    Result:=ord(SW_SERIAL0)+ord(SerialPort);
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
    Result := Result + Format('%3.2x', [ord(Value[n])]);
  Result := upperCase(Result);
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

function decode32BitSignedInt(StrEncoded: string): integer;
var
  i: integer;
begin
  Result:=0;
  for i:=0 to 3 do
    Result:=Result or (ord(StrEncoded[i+1]) << (i*7));
  Result:=Result or ((ord(StrEncoded[5]) << 28) and 7);

  if (ord(StrEncoded[5]) >> 3) = 1 then // negative
    Result:=-Result;
end;
// converts a n bytes string, low byte first, into a positive int value
function decodeNbytestoInt(DataString: String): integer;
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
function EncodeAccelFloat(Value: double): String;   // return a 4 bytes string encoded 7 bit
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
function DecodeAccelFloat(Data7Bit: string): double;  // Data7Bit 4 bytes encoded to 7 bits, total 28 bits
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
{ TComPortReadThread }
procedure TBoardThread.CallEvent;
begin
  if Assigned(owner.FOnDeviceDataAvailable) then
  begin
     if Owner.FOnDeviceDataAvailable(self) then  // must return true if dataavailable
       Owner.GetFirmataCommand(self)  // must be a new command
  end
  else
  begin
    Owner.RaiseError(1026, 'CallEvent');  // now way to check available data
  end;
end;

procedure TBoardThread.Execute;
begin
  try
    while not MustDie do
    begin
      Synchronize(
      {$IFDEF OBJFPC}@{$ENDIF}CallEvent);
    end;
  finally
    Terminate;
  end;

end;

{ TBoard }
constructor TBoard.Create(AOwner: TComponent);
begin
  inherited;

  FBoardThread:=nil;
  FStarting:=False;
  FEndCommand:=true;
  FEnabled:=False;
  FOnBeforeOpen:=nil;
  FOnAfterClose:=nil;
  FOnBoardReady:=nil;
  FOnSendDataToDevice:=nil;
  FOnGetDataFromDevice:=nil;
  FOnDeviceDataAvailable:=nil;
  FOnQueryAllTask:=nil;
  FOnError:=nil;
  FOnBoardData:=nil;
  FOnPinValue:=nil;
  FOnPinState:=nil;
  FSamplingInterval:=19;  // 19 milisec default arduino sampling interval
  FBoardPinsNumber:=0;
  FAnalogPinsNumber:=0;
  FGotCapabilities:=False;
  FGotFirmware:=False;
  FGotVersion:=False;
  FTimeEnd:=6000; // default max time to wait for firmata to start
  FStartedTime:=0;
end;

destructor TBoard.Destroy();
begin
  inherited Destroy;
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
  SendSamplingInterval(Interval);
end;

function TBoard.SendSamplingInterval(Interval: integer; write: Boolean=true): string;
begin
  if interval=FSamplingInterval then
    exit;
  FSamplingInterval:=Interval;
  // dont know if both bytes LSB and MSB are 7 or 8 bits
  Result:=SendSysEx(chr(SAMPLING_INTERVAL)+chr(Interval and $7F)+chr((Interval >> 7) and $7F), write);
end;

procedure TBoard.setTimeEnd(TimeToWait: Qword);  // milisec to wait for firmata
begin
  if Time < 1700 then  // min time to wait is about 1700 milisec
    exit
  else
     FTimeEnd:=TimeToWait;
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

  FEndCommand:=true;
  FStarting:=True;
  FGotCapabilities:=False;
  FGotFirmware:=False;
  FGotVersion:=False;

  if Assigned(FOnBeforeOpen) then
     FOnBeforeOpen(self);

  if not Assigned(FOnSendDataToDevice) then
  begin
    RaiseError(1001, 'Open');
    close;
  end;
  if not Assigned(OnGetDataFromDevice) then
  begin
     RaiseError(1028, 'Open');
     close;
  end;
 if not Assigned(OnDeviceDataAvailable) then
  begin
     RaiseError(1026, 'Open');
     close
  end;

  // Launch Thread
  FBoardThread := TBoardThread.Create(true);
  FBoardThread.Owner := Self;
  FBoardThread.MustDie := false;
  FBoardThread.Start;

  // init time measure for starting firmata
  FInitTime:=TThread.GetTickCount64;  // milisec
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

  for i:=0 to MAX_ACCELSTEPPER_DEVICES - 1 do
    FAccelSteppers[i]:=nil;

  FAccelStepperGroup:=nil;

  for i:=0 to MAX_SERIAL_PORTS - 1 do
    FSerials[ByteToSerialPorts(i)]:=nil;

  for i:=0 to MAX_SERVOS - 1 do
    FServos[i]:=nil;

  for i:=0 to MAX_ENCODERS - 1 do
    FEncoders[i]:=nil;
end;

procedure TBoard.Close;
begin
  FEnabled:=False;

  // stop runnung thread
  if FBoardThread <> nil then begin
    FBoardThread.FreeOnTerminate:=false;
    FBoardThread.MustDie:= true;

    while not FBoardThread.Terminated do begin
      Application.ProcessMessages;
    end;

    FBoardThread.Free;
    FBoardThread:=nil;
  end;

  if Assigned(FOnAfterClose) then
    FOnAfterClose(self);
end;

function TBoard.askVersion(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(REPORT_VERSION), write);
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
  FBoardPinsNumber:=0;
  FAnalogPinsNumber:=0;
  Setlength(FBoardPins,0);

  FEndCommand:=True;
  FBoardVersion[0]:=0;
  FBoardVersion[1]:=0;
  FBoardStringFirmware:='';
  FBoardFirmware[0]:=0;
  FBoardFirmware[1]:=0;
  FGotVersion:=False;
  FGotFirmware:=False;
  FGotCapabilities:=False;
end;

procedure TBoard.RaiseError(Error: integer; FunctionError: string; TextError: string='');
var
  ErrorString: string;
begin
  FLastError:=Error;
  if Error > 1000 then
    ErrorString:=Format('Error (%d); %s in %s %s',[Error, ErrorsArray[Error - 1000], FunctionError, TextError])
  else
    ErrorString:=Format('Error (%d); %s in %s %s',[Error, ErrorsArray[Error], FunctionError, TextError]);

  if Assigned(FOnError) then
     FOnError(self, Error, ErrorString);

  if Error > 1000 then  // close application
  begin
    raise Exception.Create(ErrorString);
    CloseApplication;
  end;
end;

procedure TBoard.Reset;
begin
   BoardReset;
   Close;
   Open;
end;

//
// This is a weird function because of the problem with TThread, threads only run when the program is in idle state, out of a Event
//
function TBoard.GetNextByte: Byte;
var
  value: integer;
  TimeOut: QWord;
begin
  Value:=-1;
  TimeOut:=TThread.GetTickCount64;

  while true do
  begin
    if Assigned(FOnGetDataFromDevice) then
      Value:=FOnGetDataFromDevice(self);
    if Value <> -1 then
      break;
    if not FStarting and ((TThread.GetTickCount64 - TimeOut) > 2000) then  // time out reading a byte
    begin
      RaiseError(1031, 'GetNextByte');
      Close;
    end;
  end;
  Result:=Value;
   // Check if firmata has started to stop time out
   if FStarting then
   begin
     FStartedTime:=TThread.GetTickCount64-FInitTime;   // Get milisec ticks

     if FStartedTime > FTimeEnd then  // time out for firmata, default to 6 second
     begin
       RaiseError(1003, 'GetNextByte');
       FStarting:=False;
       Close;
    end;
  end;
end;

procedure TBoard.GetFirmataCommand(Sender: TObject);
var
  ReadByte: byte;
  Pin: Byte;
  Value: integer;
  BitValue: integer;
  mask: Byte;
  Port: Byte;
begin
  if FEndCommand then    // last command has finished
  begin
    FEndCommand:=False;  // Start a new command
    ReadByte:=GetNextByte;

    case ReadByte of
      START_SYSEX: begin  // 0xF0
        GetSysExCommandData(GetNextByte);
      end;
        {0  digital data, 0x90-0x9F, second nibble of byte 0 gives the port number (e.g. 0x92 is the third port, port 2)
        1  digital pins 0-6 bitmask
        2  digital pin 7-13 bitmask}
      $90 .. $9F: begin  // digital I/O message
        {port = Command; port_data = data[self.LSB] + (data[self.MSB] << 7)
        set all the pins for this reporting port get the first pin number for this report, pin = port * PorTBoardPins}
        mask:=1;
        Port:= ReadByte and $0F; // port number
        Value:=(GetNextByte and $7F) or ((GetNextByte << 7) and $7F);   // get digital values

        for Pin:=Port * 8 to Port * 8 + 7 do
        begin
          if FEnabled and Assigned(FPins[Pin]) and FPins[Pin].Enabled and FPins[Pin].FReporting then  // report on pin is enabled
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
        Pin:=GetPinFromAnalogPin(ReadByte and $0F);  // get pin from analogpin
        Value:=GetNextByte or ((GetNextByte << 7) and $7F);
        if FEnabled and Assigned(FPins[Pin]) and FPins[Pin].FEnabled then
          FPins[Pin].GetAnalogMessage(Value);
      end;
      {0  version report header (0xF9)
      1  major version (0-127)
      2  minor version (0-127)}
      REPORT_VERSION: begin //
        FBoardVersion[0]:=GetNextByte;
        FBoardVersion[1]:=GetNextByte;
        FGotVersion:=true;
      end;
    end;
    FEndCommand:=True;
  end;
end;

procedure TBoard.GetSysExCommandData(Command: byte);
var
  ReadByte: Byte;
  Pin: byte;
  AnalogPin: byte;
  Value: integer;
  Mode: Byte;
  DataString: string;
  Device: byte; // accelstepper device
  // exended sysex
  ID0: byte;
  ID1: byte;
  Port: Byte;
  i: integer;
  TaskID: Byte;
  TaskIDs: array of Byte;
  EncoderData: string;
begin
     DataString:='';
     case Command of
          { 0  START_SYSEX       (0xF0)
          1 EXTENDED_SYSEX       (0x00)
          2 EXTENTED_ID_0         byte0
          3 EXTENDED_ID_1         byte1
          ... PAYLOAD             nbytes
          N  END_SYSEX           (0xF7)}
          EXTENDED_SYSEX: begin // 0x00  not yet supported
             // TODO more research
             ID0:=GetNextByte; // ID_0
             ID1:=GetNextByte; // ID_1
             // now read until end command
             ReadByte:=GetNextByte;
             while ReadByte <> END_SYSEX do
             begin
               DataString:=DataString+chr(ReadByte);
               ReadByte:=GetNextByte;
             end;
             if Assigned(FOnExtendedSysex) then
               FOnExtendedSysex(self, ID0, ID1, DataString);
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
             FBoardFirmware[0]:=GetNextByte;
             FBoardFirmware[1]:=GetNextByte;
             ReadByte:=GetNextByte;
             while ReadByte <> END_SYSEX do  // Read Firmware string each char are 2 bytes (low byte, high byte)
             begin
               DataString:=DataString+Chr(ReadByte);
               ReadByte:=GetNextByte;
             end;
             DataString:=Decode2BytesCharTo1(DataString);  // converts 2 bytes char to 1 byte char
             FBoardStringFirmware:=DataString+' firmware('+IntToStr(FBoardFirmware[0])+'.'+IntToStr(FBoardFirmware[1])+
                          ') version('+IntToStr(FBoardVersion[0])+'.'+IntToStr(FBoardVersion[1])+')';
             FGotFirmware:=True;
             if (FBoardFirmware[0] > FIRMWARE_MAJOR_VERSION) or (FBoardFirmware[1] < FIRMWARE_MINOR_VERSION) then
                RaiseError(1035, 'GetSysExCommand');
             if FStarting and not FGotCapabilities then
                askBoardCapabilities; // because firmata does not send capabilities by default
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
             ReadByte:=GetNextByte;
             Pin:=0;
             AnalogPin:=0;
             SetLength(FBoardPins,0); // clear FBoardPins
             while ReadByte <> END_SYSEX do  // for all pin modes
             begin
                // New pin element
                Setlength(FBoardPins,Pin+1);  // increment size of array of pins
                FBoardPins[pin].Busy:=false;  // Pin no assigned to any module
                FBoardPins[pin].AnalogMap:=PinModesToByte(PIN_MODE_IGNORE); // default no analog pin map
                FBoardPins[pin].ActualMode:=PinModesToByte(PIN_MODE_OUTPUT);

                if ReadByte = PinModesToByte(PIN_MODE_IGNORE) then
                  FBoardPins[pin].ActualMode:=ReadByte // if first mode is PIN_MODE_IGNORE then pin must be ignored
                else
                  while ReadByte <> PinModesToByte(PIN_MODE_IGNORE) do
                  begin
                     // New pin capability
                     SetLength(FBoardPins[pin].Capabilities, Length(FBoardPins[Pin].Capabilities) + 1);
                     FBoardPins[Pin].Capabilities[Length(FBoardPins[Pin].Capabilities) - 1].mode:=ReadByte;
                     if ReadByte = PinModesToByte(PIN_MODE_ANALOG) then  // new analog pin discovered
                     begin
                       FBoardPins[pin].AnalogMap:=AnalogPin;  // map analog Pin
                       inc(AnalogPin);
                       FBoardPins[pin].ActualMode:=ReadByte; // default for anlog pin is analog mode
                     end;
                     ReadByte:=GetNextByte;  // read resolution for other than analogic mode
                     FBoardPins[Pin].Capabilities[Length(FBoardPins[Pin].Capabilities) - 1].Resolution:=ReadByte; // store Resolution
                     ReadByte:=GetNextByte; // read next capability
                  end; // end pin
                inc(Pin);    // next pin number
                ReadByte:=GetNextByte;
             end; // end all Pins
             FBoardPinsNumber:=Pin;  // discovered pins
             FAnalogPinsNumber:=AnalogPin;  // discovered analog pins
             FGotCapabilities:=true;
             if FGotVersion and FGotFirmware then //is from start or reset
             begin
               FStarting:=False;
               FEnabled:=true;
               InternalBoardReady; // Initializa module variables
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
            begin
              Repeat
              until GetNextByte = END_SYSEX;
            end
            else // Firmata has already started, so normal behavior
            begin
              ReadByte:=GetNextByte;
              Pin:=0;
              while ReadByte <> END_SYSEX do
              begin
                FBoardPins[Pin].AnalogMap:=ReadByte;  // Pin map
                Inc(Pin);  // next pin
                ReadByte:=GetNextByte;
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
                Pin:=GetNextByte;
                Mode:=GetNextByte;
                ReadByte:=GetNextByte;
                while ReadByte <> END_SYSEX do
                begin
                  DataString:=DataString+chr(ReadByte);   // First Char is LSB
                  ReadByte:=GetNextByte;
                end;

                Value:=Decode32BitSignedInt(DataString);  // converts to int

                if Assigned(FPins[Pin]) and FPins[Pin].Enabled then
                begin
                  FPins[Pin].FMode:=ByteToPinModes(Mode);
                  FBoardPins[Pin].ActualMode:=Mode;
                  if Assigned(FPins[Pin].FOnPinState) then
                    FPins[Pin].FOnPinState(self, FPins[Pin].FMode, Value);
                end
                else   // Module is not asigned or enabled
                  RaiseError(32,'Pin State Response');
          end;
          {0  START_SYSEX              (0xF0)
          1  EXTENDED_ANALOG           (0x6F)
          2  pin                      (0-127)
          3  bits 0-6                 (least significant byte)
          4  bits 7-13                (most significant byte)
          ... additional bytes may be sent if more bits are needed
          N  END_SYSEX                (0xF7)}
          EXTENDED_ANALOG: begin //  $6F
              Pin:=GetNextByte;
              ReadByte:=GetNextByte;
              while ReadByte <> END_SYSEX do
              begin
                DataString:=DataString+chr(ReadByte);   // First Char is LSB
                ReadByte:=GetNextByte;
              end;
              Value:=Decode32BitSignedInt(DataString);  // converts to int

              if Assigned(FPins[Pin]) and FPins[Pin].Enabled then
              begin
                FPins[Pin].FValue:=Value;
                if Assigned(FPins[Pin].FOnPinValue) then
                  FPins[Pin].FOnPinValue(self, Value);
              end
              else   // Module is not asigned or enabled
                RaiseError(32,'Extended Analog Reponse');;
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
             ReadByte:=GetNextByte;
             while ReadByte <> END_SYSEX do
             begin
                DataString:=DataString+Char(ReadByte);
                ReadByte:=GetNextByte;
             end;
             DataString:=Decode2BytesCharTo1(DataString);
             if Assigned(FOnBoardData) then
                  FOnBoardData(self, STRING_DATA, DataString);
          end;
          {0  START_SYSEX        (0xF0)
          1  SERIAL_DATA        (0x60)
          2  SERIAL_REPLY       (0x40) // OR with port (0x41 = SERIAL_REPLY | HW_SERIAL1)
          3  data 0             (LSB)
          4  data 0             (MSB)
          3  data 1             (LSB)
          4  data 1             (MSB)
          ...                   //up to max buffer - 5
          n  END_SYSEX          (0xF7)}
          SERIAL_MESSAGE: begin // $60
            ReadByte:=GetNextByte;
            Port:=ReadByte;  // get port
            ReadByte:=GetNextByte;
            while ReadByte <> END_SYSEX do
            begin
              DataString:=DataString+Char(ReadByte);
              ReadByte:=GetNextByte;
            end;
            DataString:=Decode2BytesCharTo1(DataString);

            if (Port and $FC) <> SERIAL_REPLY then // error
              RaiseError(15, 'Serial Message Response')
            else
              Port:=Port and $0F;  // get real port

            if Assigned(FSerials[ByteToSerialPorts(Port)]) and FSerials[ByteToSerialPorts(Port)].Enabled then
            begin
               if Assigned(FSerials[ByteToSerialPorts(Port)].FOnSerialMessage) then
                 FSerials[ByteToSerialPorts(Port)].FOnSerialMessage(self, DataString);
            end
            else   // Module is not asigned or enabled
              RaiseError(32,'Serial Message response');

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
            ReadByte:=GetNextByte;
            case ord(ReadByte) of  // first byte is Error_task or query_task
              ERROR_FIRMATA_TASK, QUERY_TASK_REPLY: begin
                DataString:=chr(ReadByte); // save command
                ReadByte:=GetNextByte;
                while ReadByte <> END_SYSEX do
                begin
                  DataString:=DataString+chr(ReadByte);
                  ReadByte:=GetNextByte;
                end;
                // get TaskID
                TaskID:=ord(DataString[2]);
                if Assigned(FTasks[TaskID]) and FTasks[TaskID].Enabled then
                  FTasks[TaskID].GetFirmataCommand(Self, DataString)
                else   // Module is not asigned or enabled
                  RaiseError(32,'Scheduler Data Response');
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
                ReadByte:=GetNextByte;
                while ReadByte <> END_SYSEX do
                begin
                  Setlength(TaskIDs,system.length(TaskIDs)+1);
                  TaskIDs[system.length(TaskIDs)-1]:=ReadByte;
                  ReadByte:=GetNextByte;
                end;
                if Assigned(FOnQueryAllTask) then
                  FOnQueryAllTask(self, TaskIDs);
              end;
              else
              begin  // unknown Scheduler command, now read until end command
                ReadByte:=GetNextByte;
                while ReadByte <> END_SYSEX do
                begin
                  DataString:=DataString+chr(ReadByte);
                  ReadByte:=GetNextByte;
                end;
                RaiseError(4, 'GetSchedulerCommands');
              end;
            end;
          end;
          I2C_REPLY: begin // $77
            ReadByte:=GetNextByte;
            while ReadByte <> END_SYSEX do
            begin
              DataString:=DataString+chr(ReadByte);
              ReadByte:=GetNextByte;
            end;
            if Assigned(FI2C) and FI2C.Enabled then
              FI2C.GetFirmataCommand(Self, DataString)
            else   // Module is not asigned or enabled
              RaiseError(32,'I2C Replay');
          end;
          ONEWIRE_DATA: begin //  $73;
            ReadByte:=GetNextByte;
            while ReadByte <> END_SYSEX do
            begin
              DataString:=DataString+chr(ReadByte);
              ReadByte:=GetNextByte;
            end;
            // first byte is subcommand
            Pin:=ord(DataString[2]);  // second byte
            if Assigned(FOneWires[Pin]) and FOneWires[Pin].Enabled then
              FOneWires[Pin].GetFirmataCommand(Self, DataString)
            else   // Module is not asigned or enabled
              RaiseError(32,'OneWire Data Response');
          end;
          ACCELSTEPPER_DATA: begin  // $62
            ReadByte:=GetNextByte;
            while ReadByte <> END_SYSEX do
            begin
              DataString:=DataString+chr(ReadByte);
              ReadByte:=GetNextByte;
            end;
            // subcommand is first byte
            case ord(DataString[1]) of
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
                    FAccelSteppers[Device].GetFirmataCommand(Self, DataString)
                 else   // Module is not asigned or enabled
                   RaiseError(32,'AccelStepper Data Response');
               end;
               {0  START_SYSEX                             (0xF0)
               1  ACCELSTEPPER_DATA                       (0x62)
               2  ACCELSTEPPER_MULTI_MOVE_COMPLETED      (0x24)
               3  group  number                           (0-4)
               4  END_SYSEX(0xF7)}
               ACCELSTEPPER_MULTI_MOVE_COMPLETED: begin  // group of accelstepper
                 if Assigned(FAccelStepperGroup) and FAccelStepperGroup.Enabled then
                   FAccelStepperGroup.GetFirmataCommand(Self, DataString)
                 else   // Module is not asigned or enabled
                   RaiseError(32,'AccelStepperGroup Multi Move Completed');
              end
              else  // subcommand unknown
              begin
                Repeat
                until GetNextByte = END_SYSEX; // discart command
                RaiseError(4, 'AccelStepper Unknown Command');
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
            ReadByte:=GetNextByte;
            while ReadByte <> END_SYSEX do
            begin
              DataString:=DataString+chr(ReadByte);
              ReadByte:=GetNextByte;
            end;
            for i:=1 to (Length(DataString) - 1) div 5 do // length data less END SYSEX byte
            begin
              EncoderData:=Copy(DataString, (i - 1) * 5, 5);
              Device:=ord(EncoderData[1]) and ENCODER_MASK;  // first byte

              if Assigned(FEncoders[Device]) and FEncoders[Device].Enabled then
                FEncoders[Device].GetFirmataCommand(Self, EncoderData);
            end;
            if ord(DataString[Length(DataString)]) <> END_SYSEX then
              RaiseError(47, 'ENCODER_DATA'); // last Byte must be END_SYSEX
          end;
          else   // Sysex command unknown
          begin
            // now read until end command
            Repeat
            until GetNextByte = END_SYSEX; // discart command
            RaiseError(4, 'Got an unknown SysEx command data');
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
    if (i <> Pin) and Assigned(FPins[i]) and FPins[i].FEnabled and FPins[i].FReporting then
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
    RaiseError(34, 'GetPinResolution');
end;

{system reset 	0xFF}
function TBoard.BoardReset(write: Boolean=true): string;
begin
     //FLazSerial.SynSer.Purge; // clear all data in serial port
     Result:=SendCommand(chr(SYSTEM_RESET), write);
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
function TBoard.SendSysEx(data7bit: string; write: Boolean=true): string;
begin
  Result:='';
  if (not FEnabled) and (not FStarting) then
  begin
    RaiseError(2, 'SendSysEx');
    exit;
  end;
  if Length(data7bit) > (MAX_DATA_BYTES - 2) then
  begin
    RaiseError(21, 'SendSysEx');
    exit;
  end;
  Result:=chr(START_SYSEX)+Data7bit+chr(END_SYSEX);
  if write then
  begin
    if Assigned(OnSendDataToDevice) then
    begin
      FOnSendDataToDevice(self, Result);
      sleep(FSamplingInterval+length(Result));  // gives some time to Firmata
    end
    else
      RaiseError(1001, 'SendSysEx');
    Result:='';
  end;
end;

// This method is used to transmit a non-sysex command.
function TBoard.SendCommand(Data: string; write: Boolean=True): string;
begin
  Result:=Data;
  if write then
  begin
    if Assigned(OnSendDataToDevice) then
    begin
      FOnSendDataToDevice(self, Data);
      sleep(FSamplingInterval+length(Result));  // gives some time to Firmata
    end
    else
      RaiseError(1001, 'SendCommand');
    Result:='';
  end;
end;

procedure TBoard.printPinInfo(Memo: TMemo);
var
  pin : integer;
  Line: string;
  i: integer;
begin
  //memo.Append('');
  //Memo.Append('Firmata capabilities');
  memo.Append('');
  for pin:=0 to FBoardPinsNumber - 1 do
  begin
       Line:='Pin '+pin.ToString+': Mode=';
       if FBoardPins[pin].ActualMode = PinModesToByte(PIN_MODE_IGNORE) then begin
              Line:=Line+'No operational pin';
       end
       else
       begin
         Line:=Line+PinModesString[ByteToPinModes(FBoardPins[pin].ActualMode)]+', Capabilities=';
         for i:=0 to Length(FBoardPins[Pin].Capabilities) - 1 do
         begin
           Line:=Line+PinModesString[ByteToPinModes(FBoardPins[pin].Capabilities[i].mode)];
           Line:=Line+'('+IntToStr(FBoardPins[pin].Capabilities[i].Resolution)+' bit), ';
         end;
       end;
       if RightStr(Line,2) = ', ' then
         Line:=LeftStr(Line, Length(Line)-2);

       Memo.Append(Line);
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
    FBoard.RaiseError(33, 'setBoard')
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
          FBoard.RaiseError(12, 'setEnabled');
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
        FBoard.RaiseError(11, 'setEnabled');
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
    FBoard.RaiseError(33, 'setMode')
  else if Mode in [PIN_MODE_INPUT, PIN_MODE_OUTPUT, PIN_MODE_ANALOG, PIN_MODE_PWM,
           PIN_MODE_SERVO, PIN_MODE_SHIFT, PIN_MODE_ENCODER, PIN_MODE_PULLUP] then
    FMode:=Mode;
end;

procedure TPin.setPin(Pin: Byte);
begin
  if FEnabled then // cannot do that
    FBoard.RaiseError(33, 'setPin')
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

function TPin.SendSysEx(data7bit: string; write: Boolean=true): string;
begin
  Result:='';
  if not FEnabled and write then
    exit;
  Result:=FBoard.SendSysEx(Data7bit, write);
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
2  mode (INPUT/OUTPUT/ANALOG/PWM/SERVO/I2C/ONEWIRE/STEPPER/ENCODER/SERIAL/PULLUP)}
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
    FBoard.RaiseError(10, 'AnalogReport');
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
  Pin: integer;
begin
  Result:='';

  if (Port * 8 + 8) > (FBoardPinsNumber - 1) then
  begin
      FBoard.RaiseError(8, 'DigitalWritePort');
      exit;
  end;

  if write then
  begin
    mask:=1;
    for Pin:=Port * 8 to Port * 8 + 7 do // default 8 pins in port
    begin
      if Assigned(FBoard.FPins[Pin]) and Fboard.FPins[FPin].Enabled and (FMode in [PIN_MODE_OUTPUT, PIN_MODE_INPUT, PIN_MODE_PULLUP]) then
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
function TPin.WriteValue(Value: integer; write: Boolean=True): string;
var
  ValueTmp: integer;
begin
  if FMode = PIN_MODE_OUTPUT then
    Result:=SetDigitalPinValue(Value, write)
  else if FMode in [PIN_MODE_PWM, PIN_MODE_SERVO, PIN_MODE_SHIFT] then
  begin
    // search for resolution
    ValueTmp:=Value and FBoard.GetPinResolution(FPin, FMode);

    if (ValueTmp > $3FFF) or (FPin > 15) then
      Result:=AnalogWriteExtended(Value, write)
    else
      Result:=Analogwrite(Value, write);
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
    FBoard.RaiseError(7, 'DigitalWrite');
    exit;
  end;
  if Value <> 0 then
    TmpValue:=HIGH
  else
    TmpValue:=LOW;

  if write then   // avoid change Fstate when it is not a real write
    FState:=TmpValue;

  Result:=SendCommand(chr(SET_DIGITAL_PIN_VALUE)+chr(Pin)+chr(TmpValue), write); // $F5
end;
{0  analog pin, 0xE0-0xEF, (MIDI Pitch Wheel)
1  analog least significant 7 bits
2  analog most significant 7 bits }
function TPin.AnalogWrite(Value: integer; write: Boolean=true): string;
var
   ValueTmp: integer;
begin
  Result:='';

  if not (FMode in [PIN_MODE_PWM, PIN_MODE_SERVO, PIN_MODE_SHIFT]) then
  begin
    FBoard.RaiseError(11, 'AnalogWrite');
    exit;
  end;
  // search for resolution
  ValueTmp:=Value and FBoard.GetPinResolution(FPin, FMode);

  if write then
    FState:=Valuetmp;  // store last value written

  Result:=SendCommand(chr(ANALOG_MESSAGE or FPin)+chr(Valuetmp and $7F)+chr(Valuetmp >> 7 and $7F),write);
end;
{0  START_SYSEX              (0xF0)
1  extended analog message  (0x6F)
2  pin                      (0-127)
3  bits 0-6                 (least significant byte)
4  bits 7-13                (most significant byte)
... additional bytes may be sent if more bits are needed
N  END_SYSEX                (0xF7)}
function TPin.AnalogWriteExtended(Value: integer; write: Boolean=True): string; // analog write (PWM, Servo, etc) to any pin
var
  i: integer;
  Valuetmp: integer;
  data: String;
begin
  data:='';

  // search for resolution
  ValueTmp:=Value and FBoard.GetPinResolution(FPin, FMode);

  for i:=0 to sizeof(Valuetmp) do
  begin
    data:=chr(Valuetmp and $FF)+data;  // LSB byte ,MSB byte
    Valuetmp:=Valuetmp>>8; // get new byte
    if Valuetmp = 0 then  // no more data to send
      break;
  end;

  if write then
    FState:=ValueTmp;  //store value written

  Result:=SendSysEx(chr(EXTENDED_ANALOG)+chr(FPin)+Encode8To7Bit(data), write);
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
  FRunOnce:=True;

  FOnEnabled:=nil;
  FOnDisabled:=nil;

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
    FBoard.RaiseError(33, 'setTask')
  else if (Task < 0) or (Task > 127) then
    FTaskID:=0
  else
    FTaskID:=Task;
end;

procedure TTask.setTimeDelay(Delay: integer);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'setTimeDelay')
  else if Delay < 1 then
    FTimeDelay:=1
  else
    FTimeDelay:=Delay;
end;

procedure TTask.setRunDelay(Delay: integer);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'setRunDelay')
  else if Delay < 1 then
    FRunDelay:=1
  else
    FRunDelay:=Delay;
end;

procedure TTask.setBoard(Board: TBoard);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'setBoard')
  else if Assigned(Board) then
    FBoard:=Board;
end;

procedure TTask.setEnabled(State: Boolean);
var
  i: integer;
begin
  if not Assigned(FBoard) then
    exit;

  if FEnabled = State then
     exit;

  if State then
  begin
    if Assigned(FBoard) and FBoard.Enabled then
    begin
      if length(FDataTask) < 3 then // min command length
      begin
        FBoard.RaiseError(25, 'setEnabled');
        exit;
      end;
      if Assigned(FBoard.FTasks[FTaskID]) then // TaskID already exists
      begin
        FBoard.RaiseError(45, 'setEnabled');
        exit;
      end;
      // task does not exist
      if not FRunOnce then  // task run again after finished, so needs a end delay
        FDataTask:=FDataTask+DelayTask(false);  // only get command, for continuous running
      FBoard.FLastError:=0;
      FBoard.FTasks[FTaskID]:=self;
      FEnabled:=True;
      CreateTask; // length of task is Length of FDataTask
      if FBoard.FLastError > 0 then
      begin
        FEnabled:=False;
        FBoard.FTasks[FTaskID]:=nil;
        exit;
      end;
      AddToTask(Encode8To7Bit(FDataTask));  // Data Task has to be sent encoded
      ScheduleTask;  // Run task
      if Assigned(FOnEnabled) then
        FOnEnabled(self);
    end
    else // FBoard is not enabled
    begin
      FEnabled:=false;
      FBoard.RaiseError(2, 'setEnabled');
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

procedure TTask.GetFirmataCommand(Sender: TObject; CommandData: String);
var
  TaskID: byte;
  DataString: string;
  Time_ms: integer;
  Length: integer;
  Position: integer;
  Command: Byte;
begin
  DataString:='';
  TaskID:=0;
  Time_ms:=0;
  Length:=0;
  Position:=0;
  Command:=ord(CommandData[1]);   // subcommand is first byte
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
  TaskID:=ord(Commanddata[2]);  // get task ID
  DataString:=Copy(CommandData, 3, system.Length(CommandData) - 2);
  if DataString <> '' then
  begin
    DataString:=Decode7To8bit(DataString);  // decode 7 bits string
    Time_ms:=decodeNbytestoInt(Copy(DataString,1,4)); // first 32 bits, 4 bytes
    Length:=decodeNbytestoInt(Copy(DataString,5,2));  // next 16 bits, 2 bytes
    Position:=decodeNbytestoInt(Copy(DataString,7,2));  // next 16 bits, 2 bytes
    DataString:=Copy(DataString,9,system.Length(DataString)-8);  // Task Data
  end;

  if Command = QUERY_TASK_REPLY then
  begin
    if Assigned(FOnQueryTask) then
      FOnQueryTask(self, Time_ms, Length, Position, DataString);
  end
  else if Assigned(FOnTaskError) then    // ERROR_TASK_REPLY
    FOnTaskError(self, Time_ms, Length, Position, DataString);
end;
{ Send START_SYSEX + data + END_SYSEX total <= MAX_DATA_BYTES (64 bytes)}
function TTask.SendSysEx(data7bit: string; write: Boolean=true): string;
begin
  Result:='';
  if not FEnabled and write then
    exit;
  Result:=FBoard.SendSysEx(Data7bit, write);
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
 6  END_SYSEX            (0xF7) }
function TTask.CreateTask(write: Boolean=true): string;
var
  Flength: integer;
begin
  Result:='';

  Flength:=Length(FDataTask); // Task stores data as 7 bit

  if FLength > $3FFF then // 14 bits
  begin
    FBoard.RaiseError(6, 'CreateTask');
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
function TTask.AddToTask(Data7Bit: String; write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(SCHEDULER_DATA)+chr(ADD_TO_TASK)+chr(FTaskID)+Data7Bit, write);
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
  Result:=SendSysEx(chr(SCHEDULER_DATA)+chr(DELAY_TASK)+Encode32BitSignedInt(FTimeDelay), write);
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
  Result:=SendSysEx(chr(SCHEDULER_DATA)+chr(SCHEDULE_TASK)+chr(FTaskID)+Encode32BitSignedInt(FRunDelay), write);
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
  FOneWirePin:=PinModesToByte(PIN_MODE_IGNORE);
  FParasitisticPower:=false;
end;

destructor TOneWire.Destroy();
begin
  inherited Destroy;
end;

procedure TOneWire.setBoard(Board: TBoard);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'setBoard')
  else if Assigned(Board) then
    FBoard:=Board;
end;

procedure TOneWire.setOneWirePin(Pin: Byte);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'setOneWirePin')
  else
    FOneWirePin:=Pin;
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
      if not FBoard.CheckCapability(FOneWirePin, PIN_MODE_ONEWIRE) then // not supported
      begin
        FEnabled:=false;
        FBoard.RaiseError(7, 'setOneWirePin');
      end
      else if FBoard.FBoardPins[FOneWirePin].Busy then  // check if pin is assigned
      begin
        FEnabled:=false;
        FBoard.RaiseError(12, 'setOneWirePin');
      end;
    end
    else
    begin
      FEnabled:=false; // firmataboard not enabled
      FBoard.RaiseError(36, 'setOneWirePin');
    end;
    if FEnabled then
    begin
      if Assigned(FBoard.FOneWires[FOneWirePin]) then
      begin
        FEnabled:=False;
        FBoard.RaiseError(43, 'setEnabled');
      end
      else // new onewire module
      begin
        FBoard.FOneWires[FOneWirePin]:=self;
        FBoard.FBoardPins[FOneWirePin].Busy:=true;  //  pin is assigned to this module
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
      FBoard.FBoardPins[FOneWirePin].Busy:=false; // free onewire pin
      FBoard.FOneWires[FOneWirePin]:=nil;
    end;
    FEnabled:=False;
  end;
end;

procedure TOneWire.GetFirmataCommand(Sender: TObject; CommandData: String);
var
  Pin: byte;
  DataString: string;
  OneWireIDs: array of string;
  i: integer;
  Command: Byte;
  Index: integer;

  function GetNextByte: Byte;
  begin
    if Index > system.Length(CommandData) then
      Result:=END_SYSEX
    else
     Result:=ord(CommandData[Index]);
    Inc(Index);
  end;
begin
  Index:=1;
  // first byte is subcommand
  Command:=GetNextByte;
  case Command of
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
          Pin:=GetNextByte;
          DataString:=Copy(CommandData, Index, system.Length(CommandData));
          DataString:=Decode7To8bit(DataString);

          SetLength(OneWireIDs,Length(DataString) div 8);  // Calc size of discovered IDs array
          for i:=0 to length(OneWireIDs)-1 do // fill the array
            OneWireIDs[i]:=Copy(DataString,i,8);  // Store next ID
          if (Length(OneWireIDs) > 0) and (Length(OneWireIDs[0]) > 0) then
            FDevice:=OneWireIDs[0];   // deafult device to first OneWireID found

          if Command = ONEWIRE_SEARCH_REPLY then
          begin
            if Assigned(FOnSearch) then
              FOnSearch(self,pin,OneWireIDs);
          end
          else  // ONEWIRE_SEARCH_ALARMS_REPLY
             if Assigned(FOnOneWireAlarm) then
               FOnOneWireAlarm(self,pin,OneWireIDs);
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
           Pin:=GetNextByte;
           DataString:=Copy(CommandData, Index, system.Length(CommandData));
           DataString:=Decode7To8bit(DataString); // Data received from Onewire
           if Assigned(FOnOneWireData) then
             // object, onewirepin, data (correlation and scratchpad)
             FOnOneWireData(self,pin,DataString);
       end
       else   // Onwrire command unknown
       begin   // unknown OneWire command
         DataString:=Copy(CommandData, Index, system.Length(CommandData));
         FBoard.RaiseError(4, 'Got an unknown OneWire commands');
       end;
   end;
end;
{ Send START_SYSEX + data + END_SYSEX total <= MAX_DATA_BYTES (64 bytes)}
function TOneWire.SendSysEx(data7bit: string; write: Boolean=true): string;
begin
  Result:='';
  if not FEnabled and write then
    exit;

  Result:=FBoard.SendSysEx(Data7bit, write);
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
  Result:=SendSysEx(chr(ONEWIRE_DATA)+chr(ONEWIRE_CONFIG_REQUEST)+chr(FOneWirePin)+chr(ord(FParasitisticPower)), write);
  if write then
    FBoard.FBoardPins[FOneWirePin].ActualMode:=PinModesToByte(PIN_MODE_ONEWIRE);
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
  Result:=SendSysEx(chr(ONEWIRE_DATA)+chr(ONEWIRE_SEARCH_REQUEST)+chr(FOneWirePin), write);
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
  Result:=SendSysEx(chr(ONEWIRE_DATA)+chr(ONEWIRE_SEARCH_ALARMS_REQUEST)+chr(FOneWirePin), write);
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
function TOneWire.SendCommands(Command: Byte; Device: String; numBytesToRead: uint16;
                   correlationId: uint16; delay: integer; dataToWrite: String; write: Boolean=true): string;
var
   Data8bit: string;
   SubCommand: byte;
begin
  Result:='';

  Data8bit:='';
  Subcommand:=Command;

  {if Device = '' then
  begin
    FBoard.RaiseError(46, 'SendCommand');
    exit;
  end; }
  if Device <> '' then
  begin
    SubCommand:=Subcommand or ONEWIRE_SELECT_REQUEST_BIT;
    // Device
    Data8bit:=Data8bit+leftStr(Device+'#0#0#0#0#0#0#0#0',8);  // set length to 8 and fill with #0
  end;
  if (numBytesToRead <> 0) or (CorrelationId <> 0) then
  begin
    SubCommand:=Subcommand or ONEWIRE_READ_REQUEST_BIT;
    Data8bit:=Data8bit+chr(numBytesToRead and $FF)+chr((numBytesToRead >> 8) and $FF);
    Data8bit:=Data8bit+chr(correlationId and $FF)+chr((correlationId >> 8) and $FF);
  end;
  if delay <> 0 then
  begin
    SubCommand:=Subcommand or ONEWIRE_DELAY_REQUEST_BIT;
    Data8bit:=Data8bit+chr(delay and $FF)+chr((delay >> 8) and $FF)+chr((delay >> 16) and $FF)+chr((delay >> 24) and $FF);
  end;

  Result:=SendSysEx(chr(ONEWIRE_DATA)+chr(Subcommand)+chr(FOneWirePin)+Encode8To7Bit(Data8Bit+dataToWrite), write);
end;

function TOneWire.Reset(write: Boolean=true): string;
begin
  //Command,Device, numBytesToRead, correlationId, delay, dataToWrite, write
  Result:=SendCommands(ONEWIRE_RESET_REQUEST_BIT, '', 0, 0, 0, '', write);
end;

function TOneWire.Skip(write: Boolean=true): string;
begin
  //Command, Device, numBytesToRead, correlationId, delay, dataToWrite, write
  Result:=SendCommands(ONEWIRE_SKIP_REQUEST_BIT, '', 0, 0, 0, '', write);
end;

function TOneWire.Select(Device: string; write: Boolean=true): string;
begin
  //Command, Device, numBytesToRead, correlationId, delay, dataToWrite, write
  Result:=SendCommands(ONEWIRE_SELECT_REQUEST_BIT, Device, 0, 0, 0, '', write);
end;

function TOneWire.ResetAndSelect(Device: string; write: Boolean=true): string;
begin
  //Command, Device, numBytesToRead, correlationId, delay, dataToWrite, write
  Result:=SendCommands(ONEWIRE_RESET_REQUEST_BIT or ONEWIRE_SELECT_REQUEST_BIT, Device, 0, 0, 0, '', write);
end;

function TOneWire.Read(BytestoRead: uint16; Correlation: uint16; write: Boolean=true): string;
begin
  //Command, Device, numBytesToRead, correlationId, delay, dataToWrite, write
  Result:=SendCommands(ONEWIRE_READ_REQUEST_BIT, '', BytestoRead, correlation, 0, '', write);
end;

function TOneWire.Delay(Delay: integer; write: Boolean=true): string;
begin
  //Command, Device, numBytesToRead, correlationId, delay, dataToWrite, write
  Result:=SendCommands(ONEWIRE_DELAY_REQUEST_BIT, '', 0, 0, Delay, '', write);
end;

function TOneWire.Write(Data: string; write: Boolean=true): string;
begin
  //Command, Device, numBytesToRead, correlationId, delay, dataToWrite, write
  Result:=SendCommands(ONEWIRE_WRITE_REQUEST_BIT, '', 0, 0, 0, Data, write);
end;

function TOneWire.Write(Delay: integer; Data: string; write: Boolean=true): string;  overload;
begin
  Result:=SendCommands(ONEWIRE_WRITE_REQUEST_BIT, '', 0, 0, Delay, Data, write);
end;

function TOneWire.WriteAndRead(BytestoRead: uint16; Correlation: uint16; Data: string; write: Boolean=true): string;
begin
  //Command, Device, numBytesToRead, correlationId, delay, dataToWrite, write
  Result:=SendCommands(ONEWIRE_WRITE_REQUEST_BIT or ONEWIRE_READ_REQUEST_BIT, '', BytestoRead, Correlation, 0, Data, write);
end;

function TOneWire.WriteAndRead(BytestoRead: uint16; Correlation: uint16; Delay: integer; Data: string; write: Boolean=true): string; overload;
begin
  //Command, Device, numBytesToRead, correlationId, delay, dataToWrite, write
  Result:=SendCommands(ONEWIRE_WRITE_REQUEST_BIT or ONEWIRE_READ_REQUEST_BIT, '', BytestoRead, Correlation, Delay, Data, write);
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
  FContinuously10Bits:=nil;

  FOnI2CData:=nil;
  FI2CQueries:=0;
  FI2Cconfig:='';  // initial configuration string
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
    FBoard.RaiseError(33, 'setBoard')
  else if Assigned(Board) then
    FBoard:=Board;
end;

procedure TI2C.setSDAPin(SDApin: Byte);   // for future use
begin
  if FEnabled then
  begin
    FBoard.RaiseError(33, 'setSDAPin');
    exit;
  end;
  FSDAPin:=SDAPin
end;

procedure TI2C.setSCLPin(SCLpin: Byte);  // for future use
begin
  if FEnabled then
  begin
    FBoard.RaiseError(33, 'setSCLPin');
    exit;
  end;
  FSCLPin:=SCLPin
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
          FBoard.RaiseError(12, 'setEnabled');
          exit;
        end
        else if not Assigned(FBoard.FI2C) then // Both pis are free, check if there is another module
        begin // Configure I2C
          FBoard.FI2C:=self;
          FEnabled:=true;
          config(FDelay, FI2Cconfig);
          if Assigned(FOnEnabled) then
            FOnEnabled(self);
        end
        else  // Module already in use
          FBoard.RaiseError(45, 'setEnabled');
      end
      else  // No valid pins
        FBoard.RaiseError(7, 'setEnabled');
    end
    else  // firmata is not enabled
      FBoard.RaiseError(36, 'setEnabled');
  end
  else  // state is false disable
  begin
    if Assigned(FOnDisabled) then
      FOnDisabled(self);
    // check if there is any ReadContinuously activated and stop it
    for i:=0 to Length(FContinuously) - 1 do
      StopReading(FContinuously[i]);
    for i:=0 to Length(FContinuously10Bits) - 1 do    // for future use
      StopReading10Bits(FContinuously10Bits[i]);
    FContinuously:=nil;
    FContinuously10Bits:=nil;    // for future use
    FBoard.FBoardPins[FSDAPin].Busy:=false;   // free pins
    FBoard.FBoardPins[FSCLPin].Busy:=false;
    if Assigned(FBoard) then
      FBoard.FI2C:=nil;
    FEnabled:=False;
  end;
end;

procedure TI2C.GetFirmataCommand(Sender: TObject; CommandData: String);
var
  DataString: string;
  Slave_Address : byte;
  I2C_Register: byte;
  Index: integer;

  function GetNextByte: Byte;
  begin
    if Index > system.Length(CommandData) then
      Result:=END_SYSEX
    else
     Result:=ord(CommandData[Index]);
    Inc(Index);
  end;
begin
  Index:=1;
  {0  START_SYSEX (0xF0)
   1  I2C_REPLY (0x77)
   2  slave address (LSB)
   3  slave address (MSB)
   4  register (LSB)
   5  register (MSB)
   6  data 0 (LSB)
   7  data 0 (MSB)
   ...
   n  END_SYSEX (0XF7)}
   Slave_Address:=GetNextByte or (GetNextByte << 7);
   I2C_Register:=GetNextByte or (GetNextByte << 7);
   DataString:=Copy(Commanddata, Index, system.Length(CommandData));
   DataString:=Decode2BytesCharTo1(DataString);
   if Assigned(FOnI2CData) then
      FOnI2CData(self, Slave_Address, I2C_Register, DataString);
end;
{ Send START_SYSEX + data + END_SYSEX total <= MAX_DATA_BYTES (64 bytes)}
function TI2C.SendSysEx(data7bit: string; write: Boolean=true): string;
begin
  Result:='';
  if not FEnabled and write then
    exit;
  Result:=FBoard.SendSysEx(Data7bit, write);
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
function TI2C.Config(Delay: byte; Data: String; write: Boolean=True): string; overload;
var
  I2CData: string;
begin
  Result:='';
  I2CData:='';
  if Delay > 0 then
    I2CData:=chr(Delay and $7F)+chr(Delay >> 7);

  // Not sure what to do with 'Data', in normal use is always null
  if Data <> '' then
    I2CData:=I2CData+Encode8To7Bit(Data); // not sure if this is the right conversion, perhaps Encode1ByteCharTo2 function

  // Firmata defaults pins, set pin 18 y 19 (analog 4 and 5) to I2c Mode  SDA, SCL
  if write then
  begin
    FBoard.FBoardPins[FSDAPin].ActualMode:=PinModesToByte(PIN_MODE_I2C);
    FBoard.FBoardPins[FSCLPin].ActualMode:=PinModesToByte(PIN_MODE_I2C);
  end;
  Result:=SendSysEx(chr(I2C_CONFIG)+I2CData, write);
end;

function TI2C.Config(write: Boolean=true): string; overload;
begin
  Result:=Config(0, '', write);
end;

function TI2C.Config(Data: string; write: Boolean=True): string; overload;  // No delay
begin
  Result:=Config(0, Data, write);
end;

function TI2C.Config(Delay: byte; write: Boolean=True): string; overload;  // delay
begin
  Result:=Config(Delay, '', write);
end;

{0  START_SYSEX (0xF0)
1  I2C_REQUEST (0x76)
2  slave address (LSB)
3  slave address (MSB) + read/write and address mode bits
          bit 7: always 0
          bit 6: auto restart transmission, 0 = stop (default), 1 = restart
          bit 5: address mode, 0 = 7bits mode, 1 = 10-bit mode
          bits 4-3: read/write, 00 = write, 01 = read once, 10 = read continuously, 11 = stop reading
          bits 2-0: slave address MSB in 10-bit mode, not used in 7-bit mode
4  data 0 (LSB)
5  data 0 (MSB)
6  data 1 (LSB)
7  data 1 (MSB)
...
n  END_SYSEX (0xF7) }
function TI2C.Request(Slave: word; command: byte; data: string; restart: Boolean=false; mode10bit: boolean=false; write: Boolean=True): string;
var
  I2CData: string;
  Mode: Byte;   // is slave address (MSB) + bits
begin
  Mode:=command and I2C_READ_WRITE_MODE_MASK; // only keeps bits 4 and 3
  if mode10bit then
  begin
     Mode:=Mode or ((Slave >> 7) and 7); // takes bits 7,8 and 9 from slave ¿?
     Mode:=Mode or I2C_10BIT_ADDRESS_MODE_MASK;  // set 10 bit mode, bit 5 on
  end;

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

function TI2C.WriteData(Slave: Byte; Address: integer; AddressSize: Byte; Data: String; restart: Boolean=false; write: Boolean=True): string;
  //Request(Slave, command, data, restart, mode10bit, write)
var
  I2CData: string;
begin
  Result:='';

  I2CData:=chr(Address and $FF);
  if AddressSize = 2 then
    I2CData:=chr((Address >> 8) and $FF)+I2CData;   // first is MSB byte address

  if ((Length(Data)+Length(I2CData)) * 2) > (MAX_DATA_BYTES - 5) then // because each byte needs two bytes in messsage
  begin
    FBoard.RaiseError(30, 'i2cWrite');
    exit;
  end;

  Result:=Request(Slave, I2C_WRITE, I2CData+Data, restart, false, write);
end;

function TI2C.Read(Slave: Byte; regID: integer; BytesToRead: byte;
                 restart: Boolean=false; write: Boolean=True): string;
//Request(Slave, command, data, restart, mode10bit, write)
var
  I2CData: string;
begin
  Result:='';

  if RegID <> -1 then
    I2CData:=chr(RegID and $7F)+chr(BytesToRead)
  else
    I2CData:=chr(BytesToRead);

  if ((Length(I2CData)+BytesToRead) * 2) > (MAX_DATA_BYTES - 5) then // because each byte needs two bytes in messsage
  begin
    FBoard.RaiseError(29, 'i2cRead');
    exit;
  end;
  Result:=Request(Slave, I2C_READ, I2CData, restart, false, write);
end;

function TI2C.RandomRead(Slave: Byte; Address: word; AddressSize: Byte; BytesToRead: byte;
                 restart: Boolean=false; write: Boolean=True): string;
//Request(Slave, command, data, restart, mode10bit, write)
begin
  Result:='';
  if ((BytesToRead + 1) * 2) > (MAX_DATA_BYTES - 5) then // because each byte needs two bytes in messsage
  begin
    FBoard.RaiseError(29, 'i2cRandomRead');
    exit;
  end;
  // Dummy write, select address
  Result:=WriteData(Slave, Address, AddressSize, '', restart, write);
  // Real read
  Result:=Result+Request(Slave, I2C_READ, chr(BytesToRead), restart, false, write);
end;

function TI2C.ReadContinuously(Slave: Byte; RegID: integer; BytesToRead: byte; restart: Boolean=false; write: Boolean=True): string;
//Request(Slave, command, data, restart, mode10bit, write)
var
  I2CData: string;
begin
  Result:='';

  if RegID <> -1 then
    I2CData:=chr(RegID and $7F)+chr(BytesToRead)
  else
    I2CData:=chr(BytesToRead);

  if ((Length(I2CData)+BytesToRead) * 2) > (MAX_DATA_BYTES - 5) then // because each byte needs two bytes in messsage
  begin
    FBoard.RaiseError(29, 'i2cReadContinuously');
    exit;
  end;
  if write then   // store for stop it when disabled if we forget to do it
  begin
    SetLength(FContinuously, Length(FContinuously) + 1);
    FContinuously[Length(FContinuously)]:=Slave;
  end;
  Result:=Request(Slave, I2C_READ_CONTINUOUSLY, I2CData, restart, false, write);
end;

function TI2C.StopReading(Slave: Byte; write: Boolean=True): string;
var
  i: integer;
//Request(Slave, command, data, restart, mode10bit, write)
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
  Result:=Request(Slave, I2C_STOP_READING, '', false, false, write);
end;

// for future use
function TI2C.WriteData10Bits(Slave: uint16; Address: integer; AddressSize: Byte; Data: String; restart: Boolean=false; write: Boolean=True): string;
  //Request(Slave, command, data, restart, mode10bit, write)
var
  I2CData: string;
begin
  Result:='';

  I2CData:=chr(Address and $FF);
  if AddressSize = 2 then
    I2CData:=chr((Address >> 8) and $FF)+I2CData;   // first is MSB byte address

  if ((Length(Data)+Length(I2CData)) * 2) > (MAX_DATA_BYTES - 5) then // because each byte needs two bytes in messsage
  begin
    FBoard.RaiseError(30, 'i2cWrite10Bits');
    exit;
  end;
  Result:=Request(Slave, I2C_WRITE, I2CData+Data, restart, true, write);
end;
// for future use
function TI2C.Read10bits(Slave: uint16; regID: integer; BytesToRead: byte; restart: Boolean=false; write: Boolean=True): string;
var
  I2CData: string;
begin
  Result:='';

  if RegID <> -1 then
    I2CData:=chr(RegID and $7F)+chr(BytesToRead)
  else
    I2CData:=chr(BytesToRead);

  if ((Length(I2CData)+BytesToRead) * 2) > (MAX_DATA_BYTES - 5) then // because each byte needs two bytes in messsage
  begin
    FBoard.RaiseError(29, 'i2cRead10Bits');
    exit;
  end;
  Result:=Request(Slave, I2C_READ, I2CData, restart, true, write);
end;
// for future use
function TI2C.RandomRead10bits(Slave: Byte; Address: word; AddressSize: Byte; BytesToRead: byte;
                 restart: Boolean=false; write: Boolean=True): string;
begin
  Result:='';

  if ((BytesToRead + 1) * 2) > (MAX_DATA_BYTES - 5) then // because each byte needs two bytes in messsage
  begin
    FBoard.RaiseError(29, 'i2cRandomRead10Bits');
    exit;
  end;
  // Dummy write, select address
  Result:=WriteData10Bits(Slave, Address, AddressSize, '', restart, write);
  // Real read
  Result:=Result+Request(Slave, I2C_READ, chr(BytesToRead), restart, True, write);
end;
// for future use
function TI2C.Read10bitsContinuously(Slave: uint16; RegID: integer; BytesToRead: byte;
                 restart: Boolean=false; write: Boolean=True): string;
//Request(Slave, command, data, restart, mode10bit, write)
var
  I2CData: string;
begin
  Result:='';

  if RegID <> -1 then
    I2CData:=chr(RegID and $7F)+chr(BytesToRead)
  else
    I2CData:=chr(BytesToRead);

  if ((Length(I2CData)+BytesToRead) * 2) > (MAX_DATA_BYTES - 5) then // because each byte needs two bytes in messsage
  begin
    FBoard.RaiseError(29, 'i2cRead10BitsContinuously');
    exit;
  end;
  if write then
  begin
    SetLength(FContinuously10Bits, Length(FContinuously10Bits) + 1);
    FContinuously10Bits[Length(FContinuously10Bits)]:=Slave;
  end;
  Result:=Request(Slave, I2C_READ_CONTINUOUSLY, I2CData, restart, True, write);
end;
// for future use
function TI2C.StopReading10bits(Slave: uint16; write: Boolean=True): string;
var
  I: integer;
//Request(Slave, command, data, restart, mode10bit, write)
begin
  if write then
    for i:=0 to Length(FContinuously10Bits) - 1 do
      if FContinuously10Bits[i] = Slave then
      begin
        if i < (Length(FContinuously10Bits) - 1) then
          FContinuously10Bits[i]:=FContinuously10Bits[Length(FContinuously10Bits) - 1];
        setLength(FContinuously10Bits, Length(FContinuously10Bits) - 1);
        break;
      end;
  Result:=Request(Slave, I2C_STOP_READING, '', false, True, write);
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
    FBoard.RaiseError(33, 'setBoard')
  else if Assigned(Board) then
    FBoard:=Board;
end;

procedure TAccelStepper.setDevice(Device: Byte);
begin
  if Device > (MAX_ACCELSTEPPER_DEVICES - 1) then
  begin
    FBoard.RaiseError(16, 'setDevice');
    exit;
  end;
  if FEnabled then
  begin
    FBoard.RaiseError(33, 'setDevice');
    exit;
  end;
  FDevice:=Device;
end;

function TAccelStepper.setMotorPin(MotorPin: Byte): Byte;
begin
  Result:=PinModesToByte(PIN_MODE_IGNORE);
  if MotorPin > PinModesToByte(PIN_MODE_IGNORE) then
  begin
    FBoard.RaiseError(16, 'setMotorPin');
    exit;
  end;
  if FEnabled then
  begin
    FBoard.RaiseError(33, 'setMotorPin');
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
    FEnabled:=True;
    if Assigned(FBoard) and FBoard.Enabled then
    begin
      // check Motorpin1 and motorpin2 capability
      if FBoard.CheckCapability(FMotorPin1, PIN_MODE_STEPPER) and FBoard.CheckCapability(FMotorPin2, PIN_MODE_STEPPER) then
      begin
        if FBoard.FBoardPins[FMotorPin1].Busy or FBoard.FBoardPins[FMotorPin2].Busy then
        begin
           FEnabled:=False;
           FBoard.RaiseError(12, 'setEnabled', '; MotorPin1 and/or MotorPin2');
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
                FBoard.RaiseError(12, 'setEnabled', '; MotorPin3');
              end
              else // motorpin3 is free
                FBoard.FBoardPins[FMotorPin3].Busy:=True;
            end
            else  // motorpin3 not supported
            begin
              FEnabled:=False;
              FBoard.RaiseError(11, 'setEnabled', '; MotorPin3');
            end;
          end
          else if FInterfaceType = ACCEL_INTERFACE_4_WIRE then
          begin
            if FBoard.CheckCapability(FMotorPin3, PIN_MODE_STEPPER) and FBoard.CheckCapability(FMotorPin4, PIN_MODE_STEPPER) then
            begin
              if FBoard.FBoardPins[FMotorPin3].Busy or FBoard.FBoardPins[FMotorPin4].Busy then
              begin
                FEnabled:=False;
                FBoard.RaiseError(12, 'setEnabled','; MotorPin3 and/or MotorPin4');
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
              FBoard.RaiseError(11, 'setEnabled', '; MotorPin3 and/or MotorPin4');
            end;
          end;
        end;
      end
      else // motorpin1 and/or Motorpin2 not supported
      begin
        FEnabled:=False;
        FBoard.RaiseError(11, 'setEnabled','; MotorPin1 and/or MotorPin2');
      end;
      if FEnabled then
      begin
        if Assigned(FBoard.FAccelSteppers[FDevice]) then
        begin
          FEnabled:=False;
          FBoard.RaiseError(43, 'setEnabled');
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
      end;
    end
    else  // Board disabled
    begin
      FEnabled:=False;
      FBoard.RaiseError(2, 'setEnabled');
    end;
  end
  else  // state is false disable
  begin
    if Assigned(FOnDisabled) then
      FOnDisabled(self);
    FBoard.FBoardPins[FMotorPin1].Busy:=false;   // free pins
    FBoard.FBoardPins[FMotorPin2].Busy:=false;
    FBoard.FBoardPins[FMotorPin3].Busy:=false;
    FBoard.FBoardPins[FMotorPin4].Busy:=false;
    FBoard.FAccelSteppers[FDevice]:=nil;
    FEnabled:=False;
  end;
end;

// get data from firmata
procedure TAccelStepper.GetFirmataCommand(Sender: TObject; CommandData: String);
var
  DataString: string;
  Position: integer;
  Command: Byte;
  Device: Byte;
  Index: integer;

  function GetNextByte: Byte;
  begin
    if Index > system.Length(CommandData) then
      Result:=END_SYSEX
    else
     Result:=ord(CommandData[Index]);
    Inc(Index);
  end;
begin
  Index:=1;
  // command is first byte, Device is second byte
  Command:=GetNextByte;
  Device:=GetNextByte;
  DataString:=Copy(Commanddata, Index, system.Length(CommandData));
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
  Position:=Decode32BitSignedInt(DataString);
  if Command = ACCELSTEPPER_REPORT_POSITION then
    begin
      if Assigned(FBoard.FAccelSteppers[Device].FOnStepperPosition) then
          FBoard.FAccelSteppers[Device].FOnStepperPosition(self, Device, Position);
    end
    else if Assigned(FBoard.FAccelSteppers[Device].FOnStepperMoveCompleted) then
          FBoard.FAccelSteppers[Device].FOnStepperMoveCompleted(self, Device, Position);
end;
{ Send START_SYSEX + data + END_SYSEX total <= MAX_DATA_BYTES (64 bytes)}
function TAccelStepper.SendSysEx(data7bit: string; write: Boolean=true): string;
begin
  Result:='';
  if not FEnabled and write then
    exit;
  Result:=FBoard.SendSysEx(Data7bit, write);
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
    Interfacing:=Interfacing or FMotorEnablePin;
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
2  ACCELSTEPPER_TO              (0x02)
3  device number                           (0-9)
4  num steps, bits 0-6
5  num steps, bits 7-13
6  num steps, bits 14-20
7  num steps, bits 21-27
8  num steps, bits 28-32
9  END_SYSEX                               (0xF7)}
function TAccelStepper.Move(write: Boolean=True): string;
begin
  Result:=SendSysEx(chr(ACCELSTEPPER_DATA)+chr(ACCELSTEPPER_STEP)+chr(FDevice)+Encode32BitSignedInt(FSteps), write);
end;
{0  START_SYSEX                            (0xF0)
1  ACCELSTEPPER_DATA                       (0x62)
2  ACCELSTEPPER_ABSOLUTE_STEP              (0x03)
3  device number                           (0-9)
4  position, bits 0-6
5  position, bits 7-13
6  position, bits 14-20
7  position, bits 21-27
8  position, bits 28-32
9 END_SYSEX                               (0xF7)}
function TAccelStepper.MoveTo(write: Boolean=True): string;
begin
  Result:=SendSysEx(chr(ACCELSTEPPER_DATA)+chr(ACCELSTEPPER_TO)+chr(FDevice)+Encode32BitSignedInt(FSteps), write);
end;
{0  START_SYSEX                             (0xF0)
1  ACCELSTEPPER_DATA                       (0x62)
2  ACCELSTEPPER_ENABLE                     (0x04)
3  device number                           (0-9)
4  device state                            (HIGH : enabled | LOW : disabled)
5  END_SYSEX                               (0xF7)}
function TAccelStepper.MotorEnable(State: Boolean; write: Boolean=True): string;
begin
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
function TAccelStepper.FastStop(write: Boolean=True): string;
begin
  // set Acceleration posible
  Result:=SendSysEx(chr(ACCELSTEPPER_DATA)+chr(ACCELSTEPPER_SET_ACCELERATION)+chr(FDevice)+EncodeAccelFloat(1000000), write);
  Result:=Result+Stop(write);   // stop with max acceleration
  // recover Acceleration
  Result:=SendSysEx(chr(ACCELSTEPPER_DATA)+chr(ACCELSTEPPER_SET_ACCELERATION)+chr(FDevice)+EncodeAccelFloat(FAcceleration), write);
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

function TAccelStepper.MotorAcceleration(write: Boolean=True): string;
begin
  Result:=SendSysEx(chr(ACCELSTEPPER_DATA)+chr(ACCELSTEPPER_SET_ACCELERATION)+chr(FDevice)+EncodeAccelFloat(FAcceleration), write);
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
    FBoard.RaiseError(33, 'AddMember');
    exit;
  end;
  if Length(FMembers) = MAX_ACCELSTEPPER_DEVICES then // no more room for members
  begin
    FBoard.RaiseError(38, 'AddMember');
    exit;
  end;

  exists:=False;
  for i:=0 to Length(FMembers) - 1 do  // check for duplicate one
  begin
    if FMembers[Length(FMembers)].Device = FirmataAccelStepper.FDevice then
    begin
      exists:=true;
      FBoard.RaiseError(39, 'AddMember');
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
    FBoard.RaiseError(33, 'DeleteMember');
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
    FBoard.RaiseError(40, 'DeleteMember');
end;

procedure TAccelStepperGroup.setBoard(Board: TBoard);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'setBoard')
  else if Assigned(Board) then
    FBoard:=Board;
end;

procedure TAccelStepperGroup.setGroup(Group: Byte);
begin
  if FEnabled then
  begin
    FBoard.RaiseError(33, 'setGroup');
    exit;
  end;
  if Group > (MAX_ACCELSTEPPER_MULTI - 1) then
  begin
    FBoard.RaiseError(16, 'setGroup');
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
          FBoard.RaiseError(41, 'setEnabled');
          exit;
        end;
      end
      else  // firmata is not enabled
      begin
        FEnabled:=False;
        FBoard.RaiseError(36, 'setEnabled');
      end;
    end
    else  // state is false disable
    begin
      if Assigned(FOnDisabled) then
        FOnDisabled(self);
      if Assigned(FBoard) then
        FBoard.FAccelStepperGroup:=nil;
      FEnabled:=False;
    end;
end;
{ Send START_SYSEX + data + END_SYSEX total <= MAX_DATA_BYTES (64 bytes)}
function TAccelStepperGroup.SendSysEx(data7bit: string; write: Boolean=true): string;
begin
  Result:='';
  if not FEnabled and write then
    exit;
  Result:=FBoard.SendSysEx(Data7bit, write);
end;

// get data from firmata
procedure TAccelStepperGroup.GetFirmataCommand(Sender: TObject; CommandData: String);
var
  Index: integer;
  group: Byte;

  function GetNextByte: Byte;
  begin
    if Index > system.Length(CommandData) then
      Result:=END_SYSEX
    else
      Result:=ord(CommandData[Index]);
    Inc(Index);
  end;
begin
    Index:=1;
    {0  START_SYSEX                             (0xF0)
     1  ACCELSTEPPER_DATA                       (0x62)
     2  multi stepper move complete command     (0x24)
     3  group  number                           (0-4)
     4  END_SYSEX(0xF7)}

     // first byte is subcommand and second byte is Group number
    GetNextByte; // drop first byte
    Group:=GetNextByte;
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
    FBoard.RaiseError(41, 'StepperMultiConfig');
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
    FBoard.RaiseError(16, 'StepperMultiStop');
    exit;
  end;

  Result:=SendSysEx(chr(ACCELSTEPPER_DATA)+chr(ACCELSTEPPER_STOP)+chr(FGroup), write);
end;
//
//
//
// TSerial
//
//
//
constructor TSerial.Create(AOwner: TComponent);
begin
  inherited;

  FBoard:= nil;
  FEnabled:=false;
  FPort:=HW_SERIAL1;
  FBaudRate:=0;   // TODO
  FRxPin:=RES_RX1;
  FTxPin:=RES_TX1;
  FOnEnabled:=nil;
  FOnDisabled:=nil;

  FOnSerialMessage:=nil;
end;

destructor TSerial.Destroy();
begin
  inherited Destroy;
end;

procedure TSerial.setBoard(Board: TBoard);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'setBoard')
  else if Assigned(Board) then
    FBoard:=Board;
end;

procedure TSerial.setPort(Port: TSerialPorts);
begin
  if FEnabled then
  begin
    FBoard.RaiseError(33, 'setPort');
    exit;
  end;

  FPort:=Port;
end;

procedure TSerial.setRxPin(Pin: Byte);
begin
  if FEnabled then
  begin
    FBoard.RaiseError(33, 'setRxPin');
    exit;
  end;
  FRxPin:=Pin;
end;

procedure TSerial.setTxPin(Pin: Byte);
begin
  if FEnabled then
  begin
    FBoard.RaiseError(33, 'setTxPin');
    exit;
  end;
  FTxPin:=Pin;
end;

procedure TSerial.setBaudRate(BaudRate: integer);
begin
  begin
    FBoard.RaiseError(33, 'setBaudRate');
    exit;
  end;
  if BaudRate > $1FFFFF then
    FBaudRate:=$1FFFFF
  else if BaudRate < 1 then
    FBaudRate:=1
  else
    FBaudRate:=BaudRate;
end;

procedure TSerial.setEnabled(State: Boolean);
  function getPinFromPort(Port: TSerialPorts; PinType: TSerialPinType): Byte;
  var
    i: integer;
  begin
    Result:=PinModesToByte(PIN_MODE_IGNORE);
    for i:=0 to FBoard.FBoardPinsNumber - 1 do
      if (ord(Port)+ord(PinType)) = FBoard.GetPinResolution(i, PIN_MODE_SERIAL) then
      begin
        Result:=i;
        break;
      end;
  end;
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
      if FPort < SW_SERIAL0 then // hardware serial
      begin
        FRxPin:=getPinFromPort(FPort, RX);
        FTxPin:=getPinFromPort(FPort, TX);
        if (FRxPin = PinModesToByte(PIN_MODE_IGNORE)) or (FTxPin = PinModesToByte(PIN_MODE_IGNORE)) then // check capability pins
        begin
          FEnabled:=false;
          FBoard.RaiseError(11, 'setEnabled');
        end
        else if FBoard.FBoardPins[FRxPin].Busy or FBoard.FBoardPins[FRxPin].Busy then // now check if pins are busy
        begin
          FEnabled:=false;
          FBoard.RaiseError(12, 'setEnabled');
        end;
      end
      else // software serial
      begin
        // Check if supported pins are supported
        if not FBoard.CheckCapability(FRxPin, PIN_MODE_SERIAL) or not FBoard.CheckCapability(FTxPin, PIN_MODE_SERIAL) then // not supported
        begin
          FEnabled:=false;
          FBoard.RaiseError(7, 'setEnabled');
        end
        else if FBoard.FBoardPins[FRxPin].Busy or FBoard.FBoardPins[FTxPin].Busy then  // check if pins are assigned
        begin
          FEnabled:=false;
          FBoard.RaiseError(12, 'setEnabled');
        end;
      end;
    end
    else // firmataboard not enabled
    begin
      FEnabled:=false;
      FBoard.RaiseError(36, 'setenabled');
    end;
    if FEnabled then
    begin
      if Assigned(FBoard.FSerials[FPort]) then
      begin
        FEnabled:=False;
        FBoard.RaiseError(43, 'setEnabled');
      end
      else // new Tserial module
      begin
        FBoard.FSerials[FPort]:=self;
        FBoard.FBoardPins[FRxPin].Busy:=true;  //  pin is assigned to this module
        FBoard.FBoardPins[FTxPin].Busy:=true;  //  pin is assigned to this module
        config;  // configure Serial module
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
      FBoard.FBoardPins[FRxPin].Busy:=false; // free Rxpin
      FBoard.FBoardPins[FTxPin].Busy:=false; // free Txpin
      FBoard.FSerials[FPort]:=nil;
    end;
    FEnabled:=False;
  end;
end;

{ Send START_SYSEX + data + END_SYSEX total <= MAX_DATA_BYTES (64 bytes)}
function TSerial.SendSysEx(data7bit: string; write: Boolean=true): string;
begin
  Result:='';
  if not FEnabled and write then
    exit;

  Result:=FBoard.SendSysEx(Data7bit, write);
end;
//
// SERIAL COMMANDS
//
{0  START_SYSEX      (0xF0)
1  SERIAL_DATA      (0x60)  // command byte
2  SERIAL_CONFIG    (0x10)  // OR with port (0x11 = SERIAL_CONFIG | HW_SERIAL1)
3  baud             (bits 0 - 6)
4  baud             (bits 7 - 13)
5  baud             (bits 14 - 20) // need to send 3 bytes for baud even if value is < 14 bits
6  rxPin            (0-127) [optional] // only set if platform requires RX pin number
7  txPin            (0-127) [optional] // only set if platform requires TX pin number
6|8 END_SYSEX      (0xF7)}
function TSerial.Config(write: Boolean=true): String;
var
  StrBaud: string;
  RXTXPins: string;
begin
  Result:='';
  RXTXPins:='';

  StrBaud:=chr(FBaudRate and $7F)+chr((FBaudRate >> 7) and $7F)+ chr((FBaudRate >> 14) and $7F);

  if FPort >= SW_SERIAL0 then // soft port, pins have been sent
    RXTXPins:=chr(FRxPin)+chr(FTxPin);

  Result:=SendSysEx(chr(SERIAL_DATA)+chr(SERIAL_CONFIG or SerialPortsToByte(FPort))+StrBaud+RXTXPins, write);

  if write then
  begin
   FBoard.FBoardPins[FRxPin].ActualMode:=PinModesToByte(PIN_MODE_SERIAL);
   FBoard.FBoardPins[FTxPin].ActualMode:=PinModesToByte(PIN_MODE_SERIAL);
  end;
end;
{0  START_SYSEX      (0xF0)
1  SERIAL_DATA      (0x60)
2  SERIAL_WRITE     (0x20) // OR with port (0x21 = SERIAL_WRITE | HW_SERIAL1)
3  data 0           (LSB)
4  data 0           (MSB)
5  data 1           (LSB)
6  data 1           (MSB)
...                 // up to max buffer - 5
n  END_SYSEX        (0xF7)}
function TSerial.Write(Data: string; write: Boolean=true): String;
var
  TwoBytesChar: String;
begin
  TwoBytesChar:=Encode1ByteCharTo2(Data);
  Result:=SendSysEx(chr(SERIAL_DATA)+chr(SERIAL_WRITE or SerialPortsToByte(FPort))+TwoBytesChar, write);
end;
{0  START_SYSEX        (0xF0)
1  SERIAL_DATA        (0x60)
2  SERIAL_READ        (0x30) // OR with port (0x31 = SERIAL_READ | HW_SERIAL1)
3  SERIAL_READ_MODE   (0x00) // 0x00 => read continuously, 0x01 => stop reading
4  maxBytesToRead     (lsb) [optional]
5  maxBytesToRead     (msb) [optional]
4|6 END_SYSEX         (0xF7)}
function TSerial.Read(ReadMode: byte; BytesToRead: uint16; write: Boolean=true): String;
var
  ReadsNumber: string;
begin
  ReadsNumber:='';
  if BytesToRead > 0 then
    ReadsNumber:=chr(BytesToRead and $7F)+chr((BytesToRead >> 7) and $7F);
  Result:=SendSysEx(chr(SERIAL_DATA)+chr(SERIAL_READ or SerialPortsToByte(FPort))+chr(ReadMode and $01)+ReadsNumber, write);
end;
{0  START_SYSEX        (0xF0)
1  SERIAL_DATA        (0x60)
2  SERIAL_CLOSE       (0x50) // OR with port (0x51 = SERIAL_CLOSE | HW_SERIAL1)
3  END_SYSEX          (0xF7)}
function TSerial.Close(write: Boolean=True): string;
begin
  Result:=SendSysEx(chr(SERIAL_DATA)+chr(SERIAL_CLOSE or SerialPortsToByte(FPort)), write);
end;
{0  START_SYSEX        (0xF0)
1  SERIAL_DATA        (0x60)
2  SERIAL_FLUSH       (0x60) // OR with port (0x61 = SERIAL_FLUSH | HW_SERIAL1)
3  END_SYSEX          (0xF7)}
function TSerial.Flush(write: Boolean=True): string;
begin
  Result:=SendSysEx(chr(SERIAL_DATA)+chr(SERIAL_FLUSH or SerialPortsToByte(FPort)), write);
end;
{0  START_SYSEX        (0xF0)
1  SERIAL_DATA        (0x60)
2  SERIAL_LISTEN      (0x70) // OR with port to switch to (0x79 = switch to SW_SERIAL1)
3  END_SYSEX          (0xF7)}
function TSerial.Listen(write: Boolean=True): string;
begin
  Result:=SendSysEx(chr(SERIAL_DATA)+chr(SERIAL_LISTEN or SerialPortsToByte(FPort)), write);
end;
//
//
//
// TServo
//
//
//
constructor TServo.Create(AOwner: TComponent);
begin
  inherited;
  FBoard:= nil;
  FEnabled:=false;
  FPin:=PinModesToByte(PIN_MODE_IGNORE);
  FDevice:=PinModesToByte(PIN_MODE_IGNORE);

  FMinPulse:=544;
  FMaxPulse:=2400;
  FValue:=0;

  FOnEnabled:=nil;
  FOnDisabled:=nil;
end;

destructor TServo.Destroy();
begin
  inherited Destroy;
end;

procedure TServo.setBoard(Board: TBoard);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'setBoard')
  else if Assigned(Board) then
    FBoard:=Board;
end;

procedure TServo.setEnabled(State: Boolean);
var
  i: integer;
begin
  if not Assigned(FBoard) then
    exit;

  if FEnabled = State then
     exit;

  if State then
  begin
    if FBoard.Enabled then
    begin
      FEnabled:=True;
      if FBoard.CheckCapability(FPin, PIN_MODE_SERVO) then
      begin
        if FBoard.FBoardPins[FPin].Busy then
        begin
          // Pin is busy
          FEnabled:=False;
          FBoard.RaiseError(12, 'setEnabled');
          exit;
        end
        else // pin is free
        begin
          for i:=0 to MAX_SERVOS - 1 do  // looking for a free servo room
          begin
            if not Assigned(FBoard.FServos[i]) then // found one free
            begin
              FDevice:=i;
              break;
            end;
          end;
          if FDevice <> PinModesToByte(PIN_MODE_IGNORE) then
          begin
            FBoard.FBoardPins[FPin].Busy:=True;
            FBoard.FServos[FDevice]:=self;
            Config;  // set pin mode to servo and config min and max pulses
            if Assigned(FOnEnabled) then
                 FOnEnabled(self);
          end
          else  // // there is not left servo
          begin
            FEnabled:=False;
            FBoard.RaiseError(44, 'setEnabled');
          end;
        end;
      end
      else  // not pin capability
      begin
        FEnabled:=False;
        // Not a valid mode
        FBoard.RaiseError(11, 'setEnabled');
      end;
    end;
  end
  else  // disable
  begin
    if Assigned(FOnDisabled) then
      FOnDisabled(self);
    if Assigned(FBoard) then
    begin
      FBoard.FBoardPins[FPin].Busy:=False;  // free pin
      FBoard.FServos[FDevice]:=nil;    // free servo
      FDevice:=PinModesToByte(PIN_MODE_IGNORE);
    end;
    FEnabled:=False;
  end;
end;

procedure TServo.setMinPulse(Pulse: integer);
begin
  if Pulse > $3FFF then  // out of range
    FMinPulse:=$3FFF
  else if Pulse < 1 then
    FMinPulse:=1
  else if Pulse > FMaxPulse then
    FMinPulse:=FMaxPulse
  else
    FMinPulse:=Pulse;
end;

procedure TServo.setMaxPulse(Pulse: integer);
begin
  if Pulse > $3FFF then  // out of range
    FMaxPulse:=$3FFF
  else if Pulse < 1 then
    FMaxPulse:=1
  else if Pulse < FMinPulse then
    FMaxPulse:=FMinPulse
  else
    FMaxPulse:=Pulse;
end;

procedure TServo.setPin(Pin: Byte);
begin
  if FEnabled then // cannot do that
    FBoard.RaiseError(33, 'setPin')
  else if Pin < PinModesToByte(PIN_MODE_IGNORE) then  // disabled
    FPin:=Pin;
end;

procedure TServo.setValue(Value: integer);
begin
  FValue:=Value;
  if FEnabled then
     WriteValue;
end;

function TServo.SendCommand(Data: string; write: Boolean=True): string;
begin
  Result:='';
  if not FEnabled and write then
    exit;
  Result:=FBoard.SendCommand(Data, write);
end;

function TServo.SendSysEx(data7bit: string; write: Boolean=true): string;
begin
  Result:='';
  if not FEnabled and write then
    exit;
  Result:=FBoard.SendSysEx(Data7bit, write);
end;

// minPulse and maxPulse are 14-bit unsigned integers
{0  START_SYSEX          (0xF0)
1  SERVO_CONFIG         (0x70)
2  pin number           (0-127)
3  minPulse LSB         (0-6)
4  minPulse MSB         (7-13)
5  maxPulse LSB         (0-6)
6  maxPulse MSB         (7-13)
7  END_SYSEX            (0xF7) }
function TServo.config(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(SERVO_CONFIG)+chr(FMinPulse and $7F)+chr((FMinPulse >> 7) and $7F)+chr(FMaxPulse and $7F)+chr((FMaxPulse << 7) and $7F), write);
  if write then
    FBoard.FBoardPins[FPin].ActualMode:=PinModesToByte(PIN_MODE_SERVO);
end;
{
0  analog pin, 0xE0-0xEF, (MIDI Pitch Wheel)
1  analog least significant 7 bits
2  analog most significant 7 bits }
function TServo.AnalogWrite(Value: integer; write: Boolean=true): string;
begin
  if write then
    FValue:=Value;
  Result:=SendCommand(chr(ANALOG_MESSAGE or FPin)+chr(Value and $7F)+chr(Value >> 7 and $7F), write);
end;
{0  START_SYSEX              (0xF0)
1  extended analog message  (0x6F)
2  pin                      (0-127)
3  bits 0-6                 (least significant byte)
4  bits 7-13                (most significant byte)
... additionaly bytes may be sent if more bits are needed
N  END_SYSEX     (0xF7)}
function TServo.AnalogWriteExtended(Value: integer; write: Boolean=true): string;
var
  i: integer;
  Valuetmp: integer;
  data: String;
begin
  data:='';

  // search for resolution
  ValueTmp:=Value and FBoard.GetPinResolution(FPin, PIN_MODE_SERVO);

  for i:=0 to sizeof(Valuetmp) do
  begin
    data:=chr(Valuetmp and $FF)+data;  // LSB byte ,MSB byte
    Valuetmp:=Valuetmp>>8; // get new byte
    if Valuetmp = 0 then  // no more data to send
      break;
  end;
  if write then
    FValue:=ValueTmp;

  Result:=SendSysEx(chr(EXTENDED_ANALOG)+chr(FPin)+Encode8To7Bit(data), write);
end;
{Write to servo, servo write is performed if the pin mode is SERVO
if value > $3FFF (14 bits) or Pin > 15 then extended analog write else Analog Write }
function TServo.WriteValue(write: Boolean=true): string;
begin
  if (FValue > $3FFF) or (FPin > 15) then
    Result:=AnalogWriteExtended(FValue, write)
  else
    Result:=AnalogWrite(FValue, write);
end;
//
//
//
// TEncoder
//
//
//
constructor TEncoder.Create(AOwner: TComponent);
begin
  inherited;
  FBoard:= nil;
  FEnabled:=false;
  FPinA:=PinModesToByte(PIN_MODE_IGNORE);
  FPinB:=PinModesToByte(PIN_MODE_IGNORE);
  FDevice:=PinModesToByte(PIN_MODE_IGNORE);  // not assigned to board yet

  FOnEnabled:=nil;
  FOnDisabled:=nil;
end;

destructor TEncoder.Destroy();
begin
  inherited Destroy;
end;

procedure TEncoder.setBoard(Board: TBoard);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'setBoard')
  else if Assigned(Board) then
    FBoard:=Board;
end;

procedure TEncoder.setEnabled(State: Boolean);
var
  i: integer;
begin
  if not Assigned(FBoard) then
    exit;

  if FEnabled = State then
     exit;

  if State then
  begin
    if FBoard.Enabled then
    begin
      FEnabled:=True;
      if FBoard.CheckCapability(FPinA, PIN_MODE_ENCODER)
         and FBoard.CheckCapability(FPinB, PIN_MODE_ENCODER) then  // I think all pins are capable, not information in capability
      begin
        if FBoard.FBoardPins[FPinA].Busy or FBoard.FBoardPins[FPinA].Busy then
        begin
          // Pin is busy
          FEnabled:=False;
          FBoard.RaiseError(12, 'setEnabled');
          exit;
        end
        else // pins are free
        begin
          for i:=0 to MAX_ENCODERS - 1 do
          begin
            if not Assigned(FBoard.FEncoders[i]) then  // found a free encoder
            begin
              FDevice:=i;
              break;
            end;
          end;
          if FDevice <> PinModesToByte(PIN_MODE_IGNORE) then
          begin
            FBoard.FBoardPins[FPinA].Busy:=True;
            FBoard.FBoardPins[FPinB].Busy:=True;
            FBoard.FEncoders[FDevice]:=self;
            Attach;  // set pins to encoders
            if Assigned(FOnEnabled) then
              FOnEnabled(self);
          end
          else  // there is not left encoder
          begin
            FEnabled:=False;
            FBoard.RaiseError(44, 'setEnabled');
          end;
        end;
      end
      else  // not pin capability
      begin
        FEnabled:=False;
        // Not a valid mode
        FBoard.RaiseError(11, 'setEnabled');
      end;
    end;
  end
  else  // disable
  begin
    if Assigned(FOnDisabled) then
      FOnDisabled(self);
    if Assigned(FBoard) then
    begin
      FBoard.FBoardPins[FPinA].Busy:=False;
      FBoard.FBoardPins[FPinB].Busy:=False;
      FBoard.FEncoders[FDevice]:=nil;
      FDevice:=PinModesToByte(PIN_MODE_IGNORE);
    end;
    FEnabled:=False;
  end;
end;

procedure TEncoder.setPinA(Pin: Byte);
begin
  if FEnabled then // cannot do that
    FBoard.RaiseError(33, 'setPinA')
  else if Pin < PinModesToByte(PIN_MODE_IGNORE) then  // disabled
    FPinA:=Pin;
end;

procedure TEncoder.setPinB(Pin: Byte);
begin
  if FEnabled then // cannot do that
    FBoard.RaiseError(33, 'setPinB')
  else if Pin < PinModesToByte(PIN_MODE_IGNORE) then  // disabled
    FPinB:=Pin;
end;

function TEncoder.SendSysEx(data7bit: string; write: Boolean=true): string;
begin
  Result:='';
  if not FEnabled and write then
    exit;
  Result:=FBoard.SendSysEx(Data7bit, write);
end;
{/* -----------------------------------------------------
* 0 START_SYSEX                (0xF0)
* 1 ENCODER_DATA               (0x61)
* 2 ENCODER_ATTACH             (0x00)
* 3 encoder #                  ([0 - MAX_ENCODERS-1])
* 4 pin A #                    (first pin)
* 5 pin B #                    (second pin)
* 6 END_SYSEX                  (0xF7)
* -----------------------------------------------------
*/ }
function TEncoder.Attach(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(ENCODER_DATA)+chr(ENCODER_ATTACH)+chr(FDevice)+chr(FPinA)+chr(FPinB), write);
  if write then
  begin
    FBoard.FBoardPins[FPinA].ActualMode:=PinModesToByte(PIN_MODE_ENCODER);
    FBoard.FBoardPins[FPinB].ActualMode:=PinModesToByte(PIN_MODE_ENCODER);
  end;
end;
{
Report encoder's position  Query

 /* -----------------------------------------------------
 * 0 START_SYSEX                (0xF0)
 * 1 ENCODER_DATA               (0x61)
 * 2 ENCODER_REPORT_POSITION    (0x01)
 * 3 Encoder #                  ([0 - MAX_ENCODERS-1])
 * 4 END_SYSEX                  (0xF7)
 * -----------------------------------------------------
 */}
function TEncoder.QueryPosition(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(ENCODER_DATA)+chr(ENCODER_REPORT_POSITION)+chr(FDevice), write);
end;
{Report all encoders positions  Query

 /* -----------------------------------------------------
 * 0 START_SYSEX                (0xF0)
 * 1 ENCODER_DATA               (0x61)
 * 2 ENCODER_REPORT_POSITIONS   (0x02)
 * 3 END_SYSEX                  (0xF7)
 * -----------------------------------------------------
 */}
 function TEncoder.AllEncoderPositions(write: Boolean=true): string;
 begin
   Result:=SendSysEx(chr(ENCODER_DATA)+chr(ENCODER_REPORT_POSITIONS), write);
 end;
{Reset encoder position to zero  Query

  /* -----------------------------------------------------
  * 0 START_SYSEX                (0xF0)
  * 1 ENCODER_DATA               (0x61)
  * 2 ENCODER_RESET_POSITION     (0x03)
  * 3 encoder #                  ([0 - MAX_ENCODERS-1])
  * 4 END_SYSEX                  (0xF7)
  * -----------------------------------------------------
  */ }
function TEncoder.ResetPosition(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(ENCODER_DATA)+chr(ENCODER_RESET_POSITION)+chr(FDevice), write);
end;
{Enable/disable reporting Query

 /* -----------------------------------------------------
 * 0 START_SYSEX                (0xF0)
 * 1 ENCODER_DATA               (0x61)
 * 2 ENCODER_REPORT_AUTO        (0x04)
 * 3 enable                     (0x00 => false, true otherwise)
 * 4 END_SYSEX                  (0xF7)
 * -----------------------------------------------------
 */
}
function TEncoder.Reporting(Enabled: Boolean; write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(ENCODER_DATA)+chr(ENCODER_REPORT_AUTO)+chr(ord(Enabled)), write);
end;
{Detach encoder Query

 /* -----------------------------------------------------
 * 0 START_SYSEX                (0xF0)
 * 1 ENCODER_DATA               (0x61)
 * 2 ENCODER_DETACH             (0x05)
 * 3 encoder #                  ([0 - MAX_ENCODERS-1])
 * 4 END_SYSEX                  (0xF7)
 * -----------------------------------------------------
 */}
function TEncoder.Detach(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(ENCODER_DATA)+chr(ENCODER_DETACH)+chr(FDevice), write);
  if write then
  begin
    FBoard.FBoardPins[FPinA].ActualMode:=PinModesToByte(PIN_MODE_OUTPUT);
    FBoard.FBoardPins[FPinB].ActualMode:=PinModesToByte(PIN_MODE_OUTPUT);
    setEnabled(False);  // disable encoder and pins
  end;
end;
{Response to query encoder's position

 /* -----------------------------------------------------
 * 0 START_SYSEX                (0xF0)
 * 1 ENCODER_DATA               (0x61)
 * 2 Encoder #  &  DIRECTION    [= (direction << 6) | (#)]
 * 3 current position, bits 0-6
 * 4 current position, bits 7-13
 * 5 current position, bits 14-20
 * 6 current position, bits 21-27
 * 7 END_SYSEX                  (0xF7)
 * -----------------------------------------------------
 */
 Note : Byte #2 contains both encoder's number (i.e. channel) and encoder's direction.
 Direction is stored on the seventh bit, 0 (LOW) for positive and 1 (HIGH) for negative.

directionMask = 0x40; // B01000000
channelMask   = 0x3F; // B00111111

//ex direction is negative and encoder is on index 2
direction = 1;
encoder = 2;
bytes[2] =  (direction << 6) | (encoder);
}
procedure TEncoder.GetFirmataCommand(Sender: TObject; CommandData: String);
var
  Position: integer;
begin
  // first byte is encoder and direction
  Position:=ord(CommandData[2]) or (ord(CommandData[3]) << 7) or (ord(CommandData[4]) << 14) or (ord(CommandData[5]) << 21);
  if Assigned(FOnEncoderPosition) then
    FOnEncoderPosition(self, ord((ord(CommandData[1]) and DIRECTION_MASK) <> 0), Position);
end;

end.


