unit firmatapin_tcpclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, ExtCtrls, firmataconstants, firmata, firmataboard, blcksock,
  synsock;


type

  { TForm1 }

  TForm1 = class(TForm)
    CreateTask: TButton;
    DeleteTask: TButton;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Pin2: TPin;
    Pinx: TPin;
    Server: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    Board1: TBoard;
    Task1: TTask;
    TaskExe: TButton;
    ToggleReport: TToggleBox;
    Valuewrite: TEdit;
    SetValue: TButton;
    Label4: TLabel;
    Pins: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    Modes: TComboBox;
    Puerto: TEdit;
    LedOn: TButton;
    LedOff: TButton;
    OpenPort: TButton;
    ClosePort: TButton;
    ValueRead: TEdit;

    procedure Board1AfterClose(sender: TObject);
    procedure Board1BeforeOpen(sender: TObject);
    function Board1DeviceDataAvailable(sender: TObject): Boolean;
    procedure Board1Error(sender: TObject; Error: integer; TextError: string; Afected: integer);
    procedure Board1FirmataData(sender: TObject; Command: Byte; Data: string);
    procedure Board1FirmataReady(sender: TObject);
    function Board1GetDataFromDevice(sender: TObject): string;
    procedure CreateTaskClick(Sender: TObject);
    procedure DeleteTaskClick(Sender: TObject);
    procedure PinxPinValue(sender: TObject; Value: integer);
    procedure PuertoEditingDone(Sender: TObject);
    procedure Board1SendDataToDevice(sender: TObject; str: string);
    procedure FormCreate(Sender: TObject);
    procedure SetValueClick(Sender: TObject);
    procedure LedOffClick(Sender: TObject);
    procedure LedOnClick(Sender: TObject);
    procedure Memo1Click(Sender: TObject);
    procedure ModesChange(Sender: TObject);
    procedure OpenPortClick(Sender: TObject);
    procedure ClosePortClick(Sender: TObject);
    procedure PinsChange(Sender: TObject);
    procedure Task1QueryTask(sender: TObject; Time: integer; Length: integer;
      Place: integer; TaskData: String);
    procedure Task1TaskError(sender: TObject; Time: integer; Length: integer;
      Place: integer; TaskData: String);
    procedure TaskExeClick(Sender: TObject);
    procedure ToggleReportChange(Sender: TObject);
    procedure TogglereportClick(Sender: TObject);
    procedure ValuewriteEditingDone(Sender: TObject);
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
  PinMode: Byte;
  client: TTCPBlockSocket;

implementation

{$R *.lfm}


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
    HR_Error:
      Status := 'Error';
  end;
  Memo1.Lines.add(Status+ ', ' + Value);
end;

{ TForm1 }
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
  PinMode:=$7F;
  memo1.Clear;

  client:=TTCPBlockSocket.Create;
  //client.OnStatus:=@SocketStatusHandler;
//  client.OnMonitor:=@MonitorSocket;

  // Enable Firmata
  Board1.Enabled:=true;

  Puerto.Enabled:=false;
  Server.Enabled:=false;
  closeport.Enabled:=True;
  Openport.Enabled:=False;
end;

procedure TForm1.Board1AfterClose(sender: TObject);
begin
  client.CloseSocket;
end;

procedure TForm1.Board1BeforeOpen(sender: TObject);
begin
  // Open way of comunication
  //Memo1.Clear;
  memo1.Append('Wait !!!, Firmata starting....');

  Client.Connect(server.text, puerto.text);

  if client.LastError <> 0 then
  begin
    Board1.Enabled:=False;
    board1.RaiseError(1002,'Could not open connection');
  end;
end;

procedure TForm1.Board1Error(sender: TObject; Error: integer; TextError: string; Afected: integer);
begin
   ShowMessage(TextError);
end;

procedure TForm1.Board1FirmataData(sender: TObject; Command: Byte;
  Data: string);
begin
   memo1.lines.add('Firmata String:' + Data);
end;

procedure TForm1.Board1FirmataReady(sender: TObject);
var
  i: Integer;
begin
  //client.OnStatus:=nil;
//  client.OnMonitor:=nil;
  memo1.lines.add('Firmata started in, '+inttostr(Board1.StartingTime)+' milisec');
  memo1.lines.add('Firmata Firmare:' + Board1.FirmataFirmware);
  ledon.Enabled:=True;
  ledoff.Enabled:=True;
  Board1.printPinInfo(Memo1);

  for i:=0 to Board1.PinsNumber - 1 do
  begin
    if Board1.BoardPins[i].ActualMode <> PinModesToByte(PIN_MODE_IGNORE) then
      Pins.AddItem(IntTostr(i),nil);
  end;
    //INPUT/OUTPUT/ANALOG/PWM/SERVO/SHIFT/I2C/ONEWIRE/STEPPER/ENCODER/SERIAL/PULLUP/ IGNORE
    //0    /     1/     2/  3/    4/    5/  6/      7/      8/      9/    10/    11/    127
    for i:=0 to 14 do
    begin
      Modes.AddItem(PinModesString[ByteToPinModes(i)], nil);
    end;
    Valuewrite.Text:='0';
    Pins.ItemIndex:=0;
    Pins.Visible:=True;
    Modes.Visible:=True;
    Pinx.Pin:=StrToInt(Pins.Items[Pins.ItemIndex]);
    Modes.ItemIndex:=1;   // default to OUTPUT
    if not Board1.CheckCapability(Pinx.Pin, ByteToPinModes(Modes.ItemIndex)) then
      for i:=0 to 14 do
      begin
        if Board1.CheckCapability(Pinx.Pin, ByteToPinModes(Modes.ItemIndex)) then
        begin
          Modes.ItemIndex:=i; //
          break;
        end;
      end;
    Pinx.Enabled:=true;
    Pin2.Mode:=PIN_MODE_OUTPUT;
    Pin2.Enabled:=true;

    SetValue.Visible:=True;
    Valuewrite.Visible:=True;;
    ToggleReport.Visible:=false;
    Label3.Visible:=True;
    Label4.Visible:=True;
    Label6.Visible:=True;
    Createtask.Enabled:=True;
end;

procedure TForm1.Memo1Click(Sender: TObject);
begin
  Memo1.Clear;
  Board1.printPinInfo(Memo1);
end;

procedure TForm1.LedOnClick(Sender: TObject);
begin
  Pin2.WriteValue(LOW);
end;

procedure TForm1.LedOffClick(Sender: TObject);
begin
  Pin2.WriteValue(HIGH);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  memo1.Enabled:=true;
  Memo1.Clear;

  //Puerto.Text:='3030';
  //  Server.Text:='192.168.10.11';
end;

procedure TForm1.ClosePortClick(Sender: TObject);
begin
    client.OnStatus:=@SocketStatusHandler;
    //client.OnMonitor:=@MonitorSocket;

    puerto.Enabled:=true;
    server.Enabled:=true;
    closeport.Enabled:=False;
    Openport.Enabled:=True;
    ledon.Enabled:=false;
    ledoff.Enabled:=false;
    Pins.Enabled:=false;
    Modes.Enabled:=false;
    Pins.Visible:=False;
    Modes.Visible:=False;
    SetValue.Visible:=false;
    Valuewrite.Visible:=false;
    Valueread.Visible:=false;
    ToggleReport.Enabled:=False;
    ToggleReport.Visible:=false;
    Pinx.Enabled:=False;
    Pin2.Enabled:=false;
    Board1.Enabled:=false;
    Label3.Visible:=false;
    Label4.Visible:=false;
    Label6.Visible:=false;
    Label7.Visible:=false;
    Pins.Clear;
    Modes.Clear;
end;

procedure TForm1.ModesChange(Sender: TObject);
begin
   // Pin Mode has changed check report visibility
   if ByteToPinModes(Modes.ItemIndex) in [PIN_MODE_INPUT, PIN_MODE_PULLUP, PIN_MODE_ANALOG] then
   begin
     ToggleReport.Visible:=True;
     ValueWrite.Visible:=false;
     SetValue.Visible:=False;
     Label6.Visible:=false;
     Valueread.Visible:=true;
     Label7.Visible:=true;
   end
   else
   begin
     ToggleReport.Visible:=False;
     ValueWrite.Visible:=True;
     SetValue.Visible:=true;
     Label6.Visible:=true;
     Valueread.Visible:=false;
     Label7.Visible:=false
   end;

   if Pinx.Mode <> ByteToPinModes(Modes.ItemIndex) then
   begin
     if Pinx.Enabled then
     begin
       Pinx.Enabled:=false;
       ToggleReport.Checked:=false;
     end;
     Pinx.Mode:=ByteToPinModes(Modes.ItemIndex);  // new pin mode
     Valuewrite.Text:='0';
     Pinx.Enabled:=True;   // enable set new pin mode
     ToggleReport.Checked:=Pinx.Reporting;
   end;
end;

procedure TForm1.PinsChange(Sender: TObject);
var
  i: integer;
begin
  if Pinx.Pin = StrToInt(Pins.Items[Pins.ItemIndex]) then // pin does not change
     exit;

  if Pinx.Enabled then
  begin
    Pinx.Enabled:=false;  // disable Pinx to be able to change pin
    ToggleReport.Checked:=false;
  end;

  Pinx.Pin:=StrToInt(Pins.Items[Pins.ItemIndex]); // new pin number
  if not Board1.CheckCapability(Pinx.Pin, ByteToPinModes(Modes.ItemIndex)) then
  begin
    for i:=0 to Modes.Items.Count -1 do
    begin
      if Board1.CheckCapability(Pinx.Pin, ByteToPinModes(i)) then
      begin
        Modes.ItemIndex:=i; //first compatible mode
        break;
      end;
    end;
  end;

  Pinx.Mode:=ByteToPinModes(Modes.ItemIndex);

  Pinx.Enabled:=True;
  ToggleReport.Checked:=Pinx.Reporting;
  ModesChange(self);
end;

procedure TForm1.Task1QueryTask(sender: TObject; Time: integer;
  Length: integer; Place: integer; TaskData: String);
begin
    Memo1.Lines.add('TaskID='+inttostr(Task1.TaskID)+' time='+inttostr(time)+' length='+inttostr(length)+' Data='+StrToHexSep(TaskData));
  if Length <> system.Length(TaskData) then
    Memo1.Lines.Add('Bad task length');
end;


procedure TForm1.Task1TaskError(sender: TObject; Time: integer; Length: integer; Place: integer; TaskData: String);
begin
  Memo1.Lines.add('Error TaskID='+inttostr(Task1.TaskID)+' time='+inttostr(time)+' length='+inttostr(length)+'Lugar='+inttostr(Place)+' Data='+StrToHexSep(TaskData));
end;

procedure TForm1.ToggleReportChange(Sender: TObject);
begin
  if toggleReport.Checked then
    ToggleReport.Caption:='Report enabled'
  else
    ToggleReport.caption:='Report disabled';
end;

procedure TForm1.TogglereportClick(Sender: TObject);
begin
   Pinx.ReportPin(ToggleReport.Checked);
end;

procedure TForm1.ValuewriteEditingDone(Sender: TObject);
var
  Value: integer;
  Valid: Boolean;
begin
  Valid:=False;
  Value:=0;
  if Pinx.Mode in [PIN_MODE_OUTPUT, PIN_MODE_PWM, PIN_MODE_SERVO,
                PIN_MODE_SHIFT, PIN_MODE_STEPPER, PIN_MODE_ENCODER] then
  begin
     Try
       Value:=StrToInt(TEdit(Sender).Text);
       Valid:=True;
     finally
       if not Valid then
       begin
         Showmessage(Tedit(Sender).Text+' is not a valid value');
         Value:=0;
       end;
     end;
  end;
  TEdit(Sender).Text:=intToStr(Value);
end;

procedure TForm1.SetValueClick(Sender: TObject);
begin
  if Pinx.Enabled then
    Pinx.WriteValue(StrToInt(Valuewrite.Text));
end;

procedure TForm1.PinxPinValue(sender: TObject; Value: integer);  // analogpin
begin
  ValueRead.Text:=IntTostr(Value);
end;

procedure TForm1.PuertoEditingDone(Sender: TObject);
var
  Value: integer;
  Valid: Boolean;
begin
  Valid:=False;
  Try
    Value:=StrToInt(TEdit(Sender).Text);
    Valid:=True;
  finally
    if not Valid then
    begin
      Showmessage(Tedit(Sender).Text+' is not a valid value');
      Value:=3030;
    end;
  end;
  TEdit(Sender).Text:=intToStr(Value);
end;

function TForm1.Board1DeviceDataAvailable(sender: TObject): Boolean;
begin
  Result:=client.CanReadEx(5);
end;

function TForm1.Board1GetDataFromDevice(sender: TObject): string;
begin
  Result:=client.RecvPacket(0);
end;

procedure TForm1.CreateTaskClick(Sender: TObject);
var
  TaskString: string;
begin
  Task1.TaskID:=1;
  Task1.TimeDelay:=1500; // delay in delaytask
  Task1.RunDelay:=1; // delay before run task

  // The last "false" value in functions means not write in board, only get command string
  TaskString:=Pin2.SetDigitalPinValue(0, false); // set pin 2 on
  TaskString:=TaskString+Task1.DelayTask(false); // delay 1500 ms
  TaskString:=TaskString+Pin2.SetDigitalPinValue(1, false); // set pin 2 off
  Task1.RunOnce:=False;  // set a final delay in task to re-run task
  Task1.DataTask:=TaskString;
  TaskExe.Enabled:=True;
  DeleteTask.Enabled:=True;
  Createtask.Enabled:=False;
end;

procedure TForm1.TaskExeClick(Sender: TObject);
begin
  Task1.Enabled:=true;  // create and execute task
  Task1.QueryTask;
  TaskExe.Enabled:=False;
end;

procedure TForm1.DeleteTaskClick(Sender: TObject);
begin
  Task1.Enabled:=false;
  TaskExe.Enabled:=false;
  DeleteTask.Enabled:=false;
  Createtask.Enabled:=True;
end;

procedure TForm1.Board1SendDataToDevice(sender: TObject; str: string);
begin
  client.SendString(str);
end;


end.

