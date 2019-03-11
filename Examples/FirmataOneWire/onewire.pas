unit onewire;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, ExtCtrls, LazSerial, firmataconstants, firmataboard,
  lazsynautil;

type

  { TForm1 }

  TForm1 = class(TForm)
    CreateTask: TButton;
    DeleteTask: TButton;
    Label5: TLabel;
    OneWire1: TOneWire;
    Task1: TTask;
    Pins: TComboBox;
    LazSerial1: TLazSerial;
    ShowTemp: TEdit;
    Readtemp: TButton;
    Search: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Board1: TBoard;
    configure: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Device: TEdit;
    Puerto: TEdit;
    OpenPort: TButton;
    ClosePort: TButton;
    TaskExe: TButton;

    procedure CreateTaskClick(Sender: TObject);
    procedure DeleteTaskClick(Sender: TObject);
    procedure Board1AfterClose(sender: TObject);
    procedure Board1BeforeOpen(sender: TObject);
    function Board1DeviceDataAvailable(sender: TObject): Boolean;
    procedure Board1Error(sender: TObject; Error: integer; TextError: string; Afected: integer);
    procedure Board1FirmataData(sender: TObject; Command: Byte; Data: string);
    procedure Board1FirmataReady(sender: TObject);
    function Board1GetDataFromDevice(sender: TObject): integer;
    procedure OneWire1OneWireAlarm(sender: TObject; Pin: Byte; AlarmIDs: array of string);
    procedure OneWire1Search(sender: TObject; Pin: Byte; DeviceIDs: array of string);
    procedure OneWire1WireData(sender: TObject; Pin: Byte; Data: string);
    procedure Task1QueryTask(sender: TObject; Time: integer; Length: integer; Place: integer; TaskData: String);
    procedure Board1SendDataToDevice(sender: TObject; str: string);
    procedure Task1TaskError(sender: TObject; Time: integer; Length: integer; Place: integer; TaskData: String);
    procedure FormCreate(Sender: TObject);
    procedure configureClick(Sender: TObject);
    procedure Memo1Click(Sender: TObject);
    procedure OpenPortClick(Sender: TObject);
    procedure ClosePortClick(Sender: TObject);
    procedure PinOnewireChange(Sender: TObject);
    procedure PinsChange(Sender: TObject);
    procedure ReadtempClick(Sender: TObject);
    procedure SearchClick(Sender: TObject);
    procedure TaskExeClick(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  OneWireIDs: array of string;
  OneWireAlarmIDs: array of string;
  Correlation: integer;
  Scratchpad: string;
  TaskCreated: Boolean;
  Resolution: integer;
  FTimeOut: QWord;
  FSearching: Boolean;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.OpenPortClick(Sender: TObject);
begin
  memo1.Clear;
  TaskCreated:=False;
  // Enable Firmata

  Board1.Enabled:=true;

  Puerto.Enabled:=false;
  closeport.Enabled:=True;
  configure.Enabled:=false;
  Openport.Enabled:=False;
  search.enabled:=True;
  Pins.Enabled:=true;
  TaskCreated:=False;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    memo1.Enabled:=true;
    Memo1.Clear;
{$IFDEF LINUX}
    Puerto.Text:='/dev/ttyUSB0';
{$ELSE}
    Puerto.Text:='COM1';
{$ENDIF}
  LazSerial1.Device:=Puerto.Text;
  LazSerial1.BaudRate:=br_57600;
  LazSerial1.FlowControl:=fcNone;
  LazSerial1.StopBits:=sbOne;
  LazSerial1.DataBits:=db8bits;
  LazSerial1.Parity:=pNone;

end;

procedure TForm1.DeleteTaskClick(Sender: TObject);
begin
  TaskCreated:=false;
  Task1.Enabled:=False;  // Stop task1
  TaskExe.Enabled:=false;
  DeleteTask.Enabled:=false;
  CreateTask.Enabled:=True;
end;

procedure TForm1.CreateTaskClick(Sender: TObject);
var
  PartialTask: array [1..11] of String;   // array of commands
  i: integer;
  size: integer;
  Pin: TPin;  // I use it to get pin on off data task in a easier way
begin
  // The last "false" value in functions means not write in board, only get command string
  Pin:=TPin.Create(self);
  Pin.Pin:=13;  // board led
  Pin.Mode:=PIN_MODE_OUTPUT;    // it is not necessary to enable Pin to use it in tasks

  PartialTask[1]:=OneWire1.ResetAndSelect(OneWireIDs[0], false); // Reset and select device[0]
  Task1.TimeDelay:=100; // set Delay Task time
  PartialTask[2]:=Task1.DelayTask(false);
  // Resolution has already been set to 9
  PartialTask[3]:=OneWire1.Write(chr($44), false);  // convert temperature command and delay 100ms
  PartialTask[4]:=Pin.SetDigitalPinValue(1, false); // set pin 13 on, LED ON
  Task1.TimeDelay:=200; // set new Delay Task time
  PartialTask[5]:=Task1.DelayTask(false);    // delay task
  // only 100ms for resolution=9
  PartialTask[6]:=OneWire1.ResetAndSelect(OneWireIDs[0], false); // Reset and select device[0]
  Task1.TimeDelay:=100; // set new Delay Task time
  PartialTask[7]:=Task1.DelayTask(false);
  // Corelation=2 means read temp;
  PartialTask[8]:=OneWire1.WriteAndRead(9, 2, chr($BE), false); // Delay 100ms, $BE Send command to read Scratchpad 8 bytes + CRC
  Task1.TimeDelay:=1000; // set new Delay Task time
  PartialTask[9]:=Task1.DelayTask(false);    // delay task 1
  PartialTask[10]:=Pin.SetDigitalPinValue(0, false); // set pin 13 off, LED OFF
  Pin.Destroy;  // Pin is not necessary yet
  Task1.TimeDelay:=1000; // set new Delay Task time
  PartialTask[11]:=Task1.DelayTask(false);    // delay task 1, this delay keeps task 1 running

  size:=0;
  Task1.DataTask:='';
  for i:=1 to Length(PartialTask) do  // calculate length of task 1 and data task
  begin
    size:=size+Length(PartialTask[i]);
    Task1.DataTask:=Task1.DataTask+PartialTask[i];
  end;
  Task1.RunOnce:=False;  // has been done in PartialTask[11]
  TaskCreated:=True;
  TaskExe.Enabled:=True;
  DeleteTask.Enabled:=True;
  CreateTask.Enabled:=False;
end;

procedure TForm1.TaskExeClick(Sender: TObject);
begin
  Task1.Enabled:=true;      // Create and run task1 command data

  Task1.QueryTask; // show task information
end;

procedure TForm1.configureClick(Sender: TObject);
begin
  LazSerial1.ShowSetupDialog;
  Puerto.text:=LazSerial1.Device;
end;

procedure TForm1.Memo1Click(Sender: TObject);
begin
   If Board1.Enabled then
   begin
     Memo1.Clear;
     Board1.printPinInfo(Memo1);
   end;
end;

procedure TForm1.Board1FirmataReady(sender: TObject);
var
  i, j: integer;
begin
  memo1.clear;
  memo1.lines.add('Firmata started in, '+inttostr(Board1.TimeToStart)+' milisec');
  memo1.lines.add('Firmata Firmare:' + Board1.FirmataFirmware);

  Board1.printPinInfo(Memo1);

  // Fill combobox with compatible pins
  for i:=0 to Board1.PinsNumber - 1 do
  begin
    for j:=0 to Length(Board1.BoardPins[i].Capabilities) - 1 do
    begin
      if Board1.BoardPins[i].Capabilities[j].Mode = PinModesToByte(PIN_MODE_ONEWIRE) then
      begin
        Pins.AddItem(IntTostr(i),nil);
        break;
      end;
    end;
  end;

  Pins.Enabled:=True;
  Pins.ItemIndex:=0;  // First pin
  OneWire1.OneWirePin:=strtoint(Pins.Text);
  OneWire1.Enabled:=True;
  search.enabled:=true;
  Readtemp.enabled:=true;
  CreateTask.Enabled:=True;
  sleep(100);
  SearchClick(Self);  // search one wire
end;

procedure TForm1.ClosePortClick(Sender: TObject);
begin
    puerto.Enabled:=true;
    closeport.Enabled:=False;
    Openport.Enabled:=True;
    configure.Enabled:=True;
    Pins.Enabled:=false;
    Pins.Clear;
    ReadTemp.enabled:=False;
    search.enabled:=false;
    memo1.Clear;

    OneWire1.Enabled:=false;
    Task1.Enabled:=false;

    Board1.enabled:=false;
    TaskCreated:=False;
    TaskExe.Enabled:=false;
    DeleteTask.Enabled:=False;
    CreateTask.Enabled:=False;
end;

procedure TForm1.PinOnewireChange(Sender: TObject);
begin
   ReadTemp.enabled:=False;
end;

procedure TForm1.PinsChange(Sender: TObject);
begin
     if OneWire1.OneWirePin = strtoint(Pins.Text) then   // same pin
       exit;
     // Pin has changed
     // be careful with this change if a task is running
     if TaskCreated then
       Task1.Enabled:=False;
     OneWire1.Enabled:=false; // in order to be able to change onewire pin
     OneWire1.OneWirePin:=strtoint(Pins.Text);   // new pin
     OneWire1.Enabled:=True; // enable onewire again
     Memo1.Lines.Add('OneWire pin set to '+Pins.Text);
end;

procedure TForm1.ReadtempClick(Sender: TObject);
var
  i: integer;
begin
  OneWire1.ResetAndSelect(OneWireIDs[0]);
  OneWire1.Write(chr($44));  // convert temperature command
  Correlation:=2;
  sleep(100);  // need a delay, 800 ms for high resolution
  OneWire1.ResetAndSelect(OneWireIDs[0]);
  OneWire1.WriteAndRead(9, correlation, chr($BE)); // $BE Send command to read Scratchpad 8 bytes + CRC
  //sleep(100);  // need a delay
end;

// sender: TObject; PinOnewire: Byte; Array of string of devices found;
procedure TForm1.OneWire1Search(sender: TObject; Pin: Byte; DeviceIDs: array of string);
var
  i: integer;
begin
  Memo1.Lines.Add('Onewire Pin='+IntToStr(OneWire1.OneWirePin));
  SetLength(OneWireIDs,Length(DeviceIDs));
  if length(DeviceIDs)>0 then
  begin
    for i:=0 to length(DeviceIDs)-1 do
    begin
      OneWireIDs[i] := DeviceIDs[i];
      Memo1.Lines.Add('Device='+StrToHex(DeviceIDs[i]));
    end;
    Device.Text:=StrToHex(DeviceIDs[0]); // select first device
    Correlation:=1; // read scratchpad
    OneWire1.ResetAndSelect(OneWireIDs[0]);
    OneWire1.WriteAndRead(9, correlation, chr($BE)); // $BE Send command to read Scratchpad
  end
  else // no devices found
  begin
    ReadTemp.enabled:=False;
    Memo1.Lines.Add('Device=onewire device not found');
  end;
end;

// sender: TObject; PinOnewire: Byte; Array of string of alarms found;
procedure TForm1.OneWire1OneWireAlarm(sender: TObject; Pin: Byte; AlarmIDs: array of string);
var
  i: integer;
begin
  SetLength(OneWireAlarmIDs,Length(AlarmIDs));
  if length(AlarmIDs)>0 then
  begin
    for i:=0 to length(AlarmIDs)-1 do
    begin
      OneWireAlarmIDs[i] := AlarmIDs[i];
      Memo1.Lines.Add('Device Alarm='+StrToHex(AlarmIDs[i]));
    end;
  end;
end;

// sender: TObject; PinOnewire: Byte; Data: string) of Object;
procedure TForm1.OneWire1WireData(Sender: TObject; Pin: Byte; Data: string);
begin
  Correlation:=ord(Data[1]) or (ord(Data[2])<<8); // get correlation
  Scratchpad:=Copy(Data,3,9);  // get scratchpad + CRC
  
  if not CheckCRC8(Scratchpad) then
  begin
    ShowTemp.Text:='CRC error';
    exit;
  end;

  case Correlation of
    1: begin // read scratchpad, resolution are bit6 and bit5 from config register byte 4, starting from byte 0
      Resolution:=((ord(Scratchpad[4]) >> 5) and 3) + 9;  // Calc resolution
      if Resolution <> 9 then  // set low resolution 9 bits
      begin
        // How to set resolution for configuration register, can be 9, 10, 11 and 12
        // Scratchpad[4]:=((Resolution-9) << 5) or $1F;
        Scratchpad[4]:=chr($9F);  // set bit6 and bit5 to 0, low resolution
        OneWire1.ResetAndSelect(OneWireIDs[0]);
        // send new sratchpad with low resolution, correlation = 3, register 2 and 3 do not change
        OneWire1.WriteAndRead(0, 3, chr($4E)+Copy(Scratchpad,2,3)); // $4E Send command to write Scratchpad
      end;
      ReadTemp.enabled:=True;
    end;
    2: begin // read temp
      ShowTemp.Text:=floattostr(0.0625*(ord(Scratchpad[1]) or ord(Scratchpad[2])<<8))+'ºC';
    end;  //set reolution nothing to read
    3: begin // not necessary in this case, nothing to read
      ;
    end;
    else // others
    begin
      showMessage(Format('Error; Correlation %d does not exist', [Correlation]));
    end;
  end;
end;

procedure TForm1.Board1SendDataToDevice(sender: TObject; str: string);
begin
  LazSerial1.WriteData(str);
end;

function TForm1.Board1GetDataFromDevice(sender: TObject): integer;
begin
  if LazSerial1.DataAvailable then
    Result:=LazSerial1.SynSer.RecvByte(100)
  else
    Result:=-1;
end;

function TForm1.Board1DeviceDataAvailable(sender: TObject): Boolean;
begin
  Result:=LazSerial1.DataAvailable;
end;

procedure TForm1.Board1BeforeOpen(sender: TObject);
begin
  // Open way of comunication
  LazSerial1.Device:=Puerto.Text;
  LazSerial1.Open;
  if LazSerial1.active=false then
  begin
     Board1.Enabled:=False;
  end;
  Memo1.Clear;
  memo1.Append('Wait !!!, Firmata starting....');
end;

procedure TForm1.Board1Error(sender: TObject; Error: integer;
  TextError: string; Afected: integer);
begin
  ShowMessage(TextError);
end;

procedure TForm1.Board1FirmataData(sender: TObject; Command: Byte;
  Data: string);
begin
  memo1.lines.add('Firmata String:' + Data);
end;

procedure TForm1.Board1AfterClose(sender: TObject);
begin
  if LazSerial1.Active then
    LazSerial1.Close;
end;

procedure TForm1.SearchClick(Sender: TObject);
begin
   //OneWire1.Reset;
   OneWire1.Search;
end;

procedure TForm1.Task1QueryTask(sender: TObject; Time: integer; Length: integer; Place: integer; TaskData: String);
begin
  Memo1.Lines.add('TaskID='+inttostr(Task1.Taskid)+' time='+inttostr(time)+' longitud='+inttostr(length)+' Data='+StrToHexSep(TaskData));
  if Length <> system.Length(TaskData) then
    Memo1.Lines.Add('Longitud de tarea errónea');
end;

procedure TForm1.Task1TaskError(sender: TObject; Time: integer; Length: integer; Place: integer; TaskData: String);
begin
  Memo1.Lines.add('Error TaskID='+inttostr(Task1.Taskid)+' time='+inttostr(time)+' longitud='+inttostr(length)+'Lugar='+inttostr(Place)+' Data='+StrToHex(TaskData));
end;


end.

