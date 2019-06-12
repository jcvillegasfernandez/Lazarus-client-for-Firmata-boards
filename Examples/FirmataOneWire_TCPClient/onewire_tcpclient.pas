unit onewire_tcpclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, ExtCtrls, firmataconstants, firmataboard, blcksock, synsock;

type

  { TForm1 }

  TForm1 = class(TForm)
    CreateTask: TButton;
    DeleteTask: TButton;
    Label5: TLabel;
    Label6: TLabel;
    OneWire1: TOneWire;
    Server: TEdit;
    Task1: TTask;
    Pins: TComboBox;
    ShowTemp: TEdit;
    Readtemp: TButton;
    Search: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Board1: TBoard;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Device: TEdit;
    Puerto: TEdit;
    OpenPort: TButton;
    ClosePort: TButton;
    TaskExe: TButton;

    procedure Board1BoardData(sender: TObject; Command: Byte; Data: string);
    procedure CreateTaskClick(Sender: TObject);
    procedure DeleteTaskClick(Sender: TObject);
    procedure Board1AfterClose(sender: TObject);
    procedure Board1BeforeOpen(sender: TObject);
    procedure Board1Error(sender: TObject; Error: integer; TextError: string; Afected: integer);
    procedure Board1FirmataReady(sender: TObject);
    function Board1DeviceDataAvailable(sender: TObject): Boolean;
    function Board1GetDataFromDevice(sender: TObject): string;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure OneWire1OneWireAlarm(sender: TObject; AlarmIDs: array of string);
    procedure OneWire1Search(sender: TObject; DeviceIDs: array of string);
    procedure OneWire1WireData(sender: TObject; Data: string);
    procedure Task1QueryTask(sender: TObject; Time: integer; Length: integer; Place: integer; TaskData: String);
    procedure Board1SendDataToDevice(sender: TObject; str: string);
    procedure Task1TaskError(sender: TObject; Time: integer; Length: integer; Place: integer; TaskData: String);
    procedure FormCreate(Sender: TObject);
    procedure Memo1Click(Sender: TObject);
    procedure OpenPortClick(Sender: TObject);
    procedure ClosePortClick(Sender: TObject);
    procedure PinOnewireChange(Sender: TObject);
    procedure PinsChange(Sender: TObject);
    procedure ReadtempClick(Sender: TObject);
    procedure SearchClick(Sender: TObject);
    procedure TaskExeClick(Sender: TObject);
    procedure SocketStatusHandler(Sender: TObject; Reason: THookSocketReason;
                                             const Value: AnsiString);
    procedure MonitorSocket(Sender: TObject; Writing: Boolean;
            const Buffer: TMemory; Len: Integer);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  client: TTCPBlockSocket;
  OneWireIDs: array of string;
  OneWireAlarmIDs: array of string;
  Correlation: integer;
  Scratchpad: string;
  TaskCreated: Boolean;
  Resolution: integer;

implementation

{$R *.lfm}

{ TForm1 }
procedure TForm1.SocketStatusHandler(Sender: TObject; Reason: THookSocketReason;
                                         const Value: AnsiString);
var
  Status: String;
begin
  case Reason of
    HR_ResolvingBegin:
      Status := 'ResolvingBegin';
    HR_ResolvingEnd:
      Status := 'ResolvingEnd';
    HR_SocketCreate:
      Status := 'SocketCreate';
    HR_SocketClose:
      Status := 'SocketClose';
    HR_Connect:
      Status := 'Connect';
    HR_CanRead:
      Status := 'CanRead';
    HR_CanWrite:
      Status := 'CanWrite';
    HR_ReadCount:
      Status := 'ReadCount';
    HR_WriteCount:
      Status := 'WriteCount';
    HR_Wait:
      Status := 'Wait';
    HR_Error: begin
      Status := 'Error';
      client.OnStatus:=nil;
    end;

  end;
  Memo1.Lines.add(Status+ ', ' + Value);
end;

procedure TForm1.MonitorSocket(Sender: TObject; Writing: Boolean;
        const Buffer: TMemory; Len: Integer);
var
  i: integer;
  s: string;
begin
  setstring(s, Buffer, Len);

  if writing then // writing to device
  begin
    i:=memo1.lines.Add('Written: ');
  end
  else
  begin
    i:=memo1.lines.Add('Read: ');
  end;
  Memo1.Lines[i]:=Memo1.Lines[i]+StrToHexSep(s);
end;

procedure TForm1.OpenPortClick(Sender: TObject);
begin
  memo1.Clear;

  client:=TTCPBlockSocket.Create;
  //client.OnStatus:=@SocketStatusHandler;
  //client.OnMonitor:=@MonitorSocket;

  // Enable Firmata
  Board1.Enabled:=true;

  Puerto.Enabled:=false;
  Server.Enabled:=false;
  closeport.Enabled:=True;
  Openport.Enabled:=False;

  search.enabled:=True;
  Pins.Enabled:=true;
  TaskCreated:=False;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    memo1.Enabled:=true;
    Memo1.Clear;
    memo1.Enabled:=true;
    Memo1.Clear;

    Puerto.Text:='3030';
    Server.Text:='192.168.10.11';
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ClosePortClick(self);
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
  PartialTask: array [1..7] of String;   // array of commands
  i: integer;
  size: integer;
  Pin: TPin;  // I use it to get pin on off data task in a easier way
begin
  // The last "false" value in functions means not write in board, only get command string
  Pin:=TPin.Create(self);
  Pin.Board:=Board1;
  Pin.Pin:=2;  // board led
  Pin.Mode:=PIN_MODE_OUTPUT;    // it is not necessary to enable Pin to use it in tasks
  Pin.Enabled:=True; // set pin mode to output
  Pin.Enabled:=false;

  PartialTask[1]:=OneWire1.ResetAndSelect(false); // Reset and select device[0]
  // Resolution has already been set to 9
  PartialTask[2]:=OneWire1.Write(chr($44), false);  // convert temperature command and delay 100ms
  PartialTask[3]:=Pin.SetDigitalPinValue(0, false); // set pin 2 on, LED ON
  Task1.TimeDelay:=1000; // set new Delay Task time
  PartialTask[4]:=Task1.DelayTask(false);    // delay task
  PartialTask[5]:=OneWire1.ResetAndSelect(false); // Reset and select device[0]
  PartialTask[6]:=OneWire1.WriteAndRead(9, 2, chr($BE), false); // Delay 200ms, $BE Send command to read Scratchpad 8 bytes + CRC
  PartialTask[7]:=Pin.SetDigitalPinValue(1, false); // set pin 2 off, LED OFF

  Pin.Destroy;  // Pin is not necessary yet
  Task1.TimeDelay:=1000; // set new Delay Task time
 // PartialTask[8]:=Task1.DelayTask(false);    // delay task 1, this delay keeps task 1 running

  size:=0;
  Task1.RunDelay:=1; // delay before run task
  Task1.DataTask:='';
  for i:=1 to Length(PartialTask) do  // calculate length of task 1 and data task
  begin
    size:=size+Length(PartialTask[i]);
    Task1.DataTask:=Task1.DataTask+PartialTask[i];
  end;
  Task1.RunOnce:=False;  // continuous run
  TaskCreated:=True;
  TaskExe.Enabled:=True;
  DeleteTask.Enabled:=True;
  CreateTask.Enabled:=False;
  //Memo1.Lines.add('TaskID='+inttostr(Task1.Taskid)+' longitud='+inttostr(length(Task1.DataTask))+' Data='+StrToHexSep(Task1.DataTask));
end;

procedure TForm1.TaskExeClick(Sender: TObject);
begin
  Task1.Enabled:=true;      // Create and run task1 command data
  Task1.QueryTask; // show task information
  TaskExe.Enabled:=False;
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
  i: integer;
begin
  memo1.clear;
  //client.OnStatus:=nil;
  //client.OnMonitor:=nil;
  memo1.lines.add('Firmata started in, '+inttostr(Board1.StartingTime)+' milisec');
  memo1.lines.add('Firmata Firmare:' + Board1.FirmataFirmware);

  Board1.printPinInfo(Memo1);

  // Fill combobox with compatible pins
  for i:=0 to Board1.PinsNumber - 1 do
  begin
    if Board1.CheckCapability(i, PIN_MODE_ONEWIRE) then
      Pins.AddItem(IntTostr(i),nil);
  end;

  if Pins.Items.Count = 0 then // onewire module is not installed in ConfigurableFirmata
  begin
    memo1.Lines.add('');
    memo1.Lines.add('Onewire module is not installed, or there isn''t a free supported pin in ConfigurableFirmata');
    exit;
  end;

  Pins.Enabled:=True;
  if Pins.Items.IndexOf(inttostr(OneWire1.OneWirePin)) <> -1 then
    Pins.ItemIndex:=Pins.Items.IndexOf(inttostr(OneWire1.OneWirePin))
  else
  begin
    Pins.ItemIndex:=0;  // First pin
    //OneWire1.OneWirePin:=StrToInt(Pins.Items[Pins.ItemIndex]);
    OneWire1.OneWirePin:=strtoint(Pins.Text);
  end;
  OneWire1.Enabled:=True;

  search.enabled:=true;
  openPort.Enabled:=false;
  Server.Enabled:=false;
  Puerto.Enabled:=false;
  Closeport.Enabled:=true;
end;

procedure TForm1.ClosePortClick(Sender: TObject);
begin
    puerto.Enabled:=true;
    closeport.Enabled:=False;
    Openport.Enabled:=True;
    Server.Enabled:=true;
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
     begin
       Task1.Enabled:=False;
       DeleteTask.Enabled:=False;
       CreateTask.Enabled:=True;
     end;
     OneWire1.Enabled:=false; // in order to be able to change onewire pin
     OneWire1.OneWirePin:=strtoint(Pins.Text);   // new pin
     OneWire1.Enabled:=True; // enable onewire again
     Memo1.Lines.Add('OneWire pin set to '+Pins.Text);
end;

procedure TForm1.ReadtempClick(Sender: TObject);
begin
  OneWire1.ResetAndSelect;
  OneWire1.Write(chr($44));  // convert temperature command
  Correlation:=2;
  sleep(100 << (Resolution - 9));  // need a delay, 800 ms for high resolution
  OneWire1.ResetAndSelect;
  OneWire1.WriteAndRead(9, correlation, chr($BE)); // $BE Send command to read Scratchpad 8 bytes + CRC
end;

// sender: TObject; PinOnewire: Byte; Array of string of devices found;
procedure TForm1.OneWire1Search(sender: TObject; DeviceIDs: array of string);
var
  i: integer;
begin
  // Onewire1-device is already set to DeviceIDs[0]
  Memo1.Lines.Add('Onewire Pin='+IntToStr(OneWire1.OneWirePin));
  SetLength(OneWireIDs,Length(DeviceIDs));
  if length(DeviceIDs)>0 then
  begin
    for i:=0 to length(DeviceIDs)-1 do
    begin
      OneWireIDs[i] := DeviceIDs[i];
      Memo1.Lines.Add('Device='+StrToHex(DeviceIDs[i]));
    end;
    Readtemp.enabled:=true;
    Device.Text:=StrToHex(DeviceIDs[0]); // select first device
    CreateTask.Enabled:=True;
    Correlation:=1; // read scratchpad
    OneWire1.ResetAndSelect;
    OneWire1.WriteAndRead(9, correlation, chr($BE)); // $BE Send command to read Scratchpad
  end
  else // no devices found
  begin
    CreateTask.Enabled:=False;
    ReadTemp.enabled:=False;
    Memo1.Lines.Add('Device=onewire device not found');
  end;
end;

// sender: TObject; PinOnewire: Byte; Array of string of alarms found;
procedure TForm1.OneWire1OneWireAlarm(sender: TObject; AlarmIDs: array of string);
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
procedure TForm1.OneWire1WireData(Sender: TObject; Data: string);
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
      Resolution:=((ord(Scratchpad[5]) >> 5) and 3) + 9;  // Calc resolution
      if Resolution <> 9 then  // set low resolution 9 bits
      begin
        Resolution:=9;
        // How to set resolution for configuration register, can be 9, 10, 11 and 12
        Scratchpad[5]:=chr(((Resolution-9) << 5) or $1F);
        //Scratchpad[5]:=chr($1F);  // set bit6 and bit5 to 0, low resolution
        OneWire1.ResetAndSelect;
        // send new sratchpad with resolution, register 2 and 3 do not change
        OneWire1.Write(chr($4E)+Copy(Scratchpad,3,3)); // $4E Send command to write Scratchpad
      end;
      ReadTemp.enabled:=True;
    end;
    2: begin // read temp resolution
      {9 bit	0.5 degrees C	93.75 mSec
      10 bit	0.25 degrees C	187.5 mSec
      11 bit	0.125 degrees C	375 mSec
      12 bit	0.0625 degrees C	750 mSec }
      ShowTemp.Text:=floattostr(0.0625*(((ord(Scratchpad[1]) >> (12-Resolution)) << (12-Resolution)) or ord(Scratchpad[2])<<8))+'ºC';
    end;
    else // others
    begin
      showMessage(Format('Error; Correlation %d does not exist', [Correlation]));
    end;
  end;
end;

function TForm1.Board1DeviceDataAvailable(sender: TObject): Boolean;
begin
  Result:=client.CanReadEx(10);
end;

procedure TForm1.Board1SendDataToDevice(sender: TObject; str: string);
begin
  client.SendString(str);
end;

function TForm1.Board1GetDataFromDevice(sender: TObject): string;
begin
  Result:=client.RecvPacket(0);
end;

procedure TForm1.Board1BeforeOpen(sender: TObject);
begin
  // Open way of comunication
  Client.Connect(server.text, puerto.text);

  if client.LastError <> 0 then
  begin
    Board1.Enabled:=False;
    board1.RaiseError(1002,'Could not open connection');
  end;
  Memo1.Clear;
  memo1.Append('Wait !!!, Firmata starting....');
end;

procedure TForm1.Board1Error(sender: TObject; Error: integer;
  TextError: string; Afected: integer);
begin
  ShowMessage(TextError);
end;
 
procedure TForm1.Board1BoardData(sender: TObject; Command: Byte; Data: string);
begin
  memo1.lines.add('Firmata String:' + Data);
end;

procedure TForm1.Board1AfterClose(sender: TObject);
begin
  client.CloseSocket;
end;

procedure TForm1.SearchClick(Sender: TObject);
begin
   //OneWire1.Reset;
   OneWire1.Search;
end;

procedure TForm1.Task1QueryTask(sender: TObject; Time: integer; Length: integer; Place: integer; TaskData: String);
begin
  Memo1.Lines.add('TaskID='+inttostr(Task1.Taskid)+' time='+inttostr(time)+' length='+inttostr(length)+' Data='+StrToHexSep(TaskData));
  if Length <> system.Length(TaskData) then
    Memo1.Lines.Add('Bad task length');
end;

procedure TForm1.Task1TaskError(sender: TObject; Time: integer; Length: integer; Place: integer; TaskData: String);
begin
  Memo1.Lines.add('Error TaskID='+inttostr(Task1.Taskid)+' time='+inttostr(time)+' longitud='+inttostr(length)+'Lugar='+inttostr(Place)+' Data='+StrToHex(TaskData));
end;


end.

