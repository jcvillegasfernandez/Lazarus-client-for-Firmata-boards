unit Firmatapin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, ExtCtrls, firmataconstants, firmata, firmataboard, LazSynaser,
  LazSerial;

type
  TPinValue = 0..1;
  TPortPins = bitpacked record
    Pin0, Pin1, Pin2, Pin3, Pin4, Pin5, Pin6, Pin7: TPinValue;
  end;

  TPortValue = packed record
     case Integer of
       0: (Byte: Byte);
       1: (Pins: TPortPins);
     end;

  { TForm1 }

  TForm1 = class(TForm)
    Board1: TBoard;
    CreateTask: TButton;
    DeleteTask: TButton;
    Label7: TLabel;
    Pin13: TPin;
    Pin2: TPin;
    Label5: TLabel;
    Label6: TLabel;
    Task1: TTask;
    Task2: TTask;
    TaskExe: TButton;
    LazSerial1: TLazSerial;
    ToggleReport: TToggleBox;
    Valuewrite: TEdit;
    SetValue: TButton;
    Label4: TLabel;
    Pins: TComboBox;
    configure: TButton;
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
    procedure Board1Error(sender: TObject; Error: integer; TextError: string; Afected: integer);
    procedure Board1FirmataData(sender: TObject; Command: Byte; Data: string);
    procedure Board1FirmataReady(sender: TObject);
    function Board1DeviceDataAvailable(sender: TObject): Boolean;
    function Board1GetDataFromDevice(sender: TObject): string;
    procedure Pin2PinValue(sender: TObject; Value: integer);
    procedure Task1QueryTask(sender: TObject; Time: integer; Length: integer; Place: integer; TaskData: String);
    procedure Board1SendDataToDevice(sender: TObject; str: string);
    procedure Task1TaskError(sender: TObject; Time: integer; Length: integer; Place: integer; TaskData: String);
    procedure FormCreate(Sender: TObject);
    procedure configureClick(Sender: TObject);
    procedure SetValueClick(Sender: TObject);
    procedure LedOffClick(Sender: TObject);
    procedure LedOnClick(Sender: TObject);
    procedure Memo1Click(Sender: TObject);
    procedure ModesChange(Sender: TObject);
    procedure OpenPortClick(Sender: TObject);
    procedure ClosePortClick(Sender: TObject);
    procedure PinsChange(Sender: TObject);
    procedure DeleteTaskClick(Sender: TObject);
    procedure CreateTaskClick(Sender: TObject);
    procedure TaskExeClick(Sender: TObject);
    procedure ToggleReportChange(Sender: TObject);
    procedure TogglereportClick(Sender: TObject);
    procedure ValuewriteEditingDone(Sender: TObject);
    procedure SerialStatusHandler(Sender: TObject; Reason: THookSerialReason; const Value: string);

  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  PinMode: Byte;
  PortValue: TPortValue;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.OpenPortClick(Sender: TObject);
begin
  PinMode:=$7F;
  PortValue.byte:=0;   // set all pins to 0
  memo1.Clear;

  LazSerial1.Device:=Puerto.Text;
  LazSerial1.BaudRate:=br_57600;
  LazSerial1.FlowControl:=fcNone;
  LazSerial1.StopBits:=sbOne;
  LazSerial1.DataBits:=db8bits;
  LazSerial1.OnStatus:=nil;
 // LazSerial1.OnStatus:=@SerialStatusHandler;

  // Enable Firmata
  Board1.Enabled:=true;

  Puerto.Enabled:=false;
  closeport.Enabled:=True;
  configure.Enabled:=false;
  Openport.Enabled:=False;
end;

procedure TForm1.SerialStatusHandler(Sender: TObject; Reason: THookSerialReason; const Value: string);
var
  Status: String;
begin
  case Reason of
    HR_SerialClose : Status:='Port ' + Value + ' closed';
    HR_Connect :   Status:='Port ' + Value + ' connected';
    HR_CanRead :   Status:= 'CanRead : ' + Value ;
    HR_CanWrite :  Status:= 'CanWrite : ' + Value ;
    HR_ReadCount : Status:= 'ReadCount : ' + Value ;
    HR_WriteCount : Status:= 'WriteCount : ' + Value ;
    HR_Wait :  Status:= 'Wait : ' + Value ;
  end ;
  Memo1.Lines.add(Status);
end;

procedure TForm1.Board1AfterClose(sender: TObject);
begin
  if LazSerial1.Active then
    LazSerial1.Close;
end;

procedure TForm1.Board1BeforeOpen(sender: TObject);
begin
  // Open way of comunication
  LazSerial1.Open;
  if LazSerial1.active = false then
  begin
     Board1.Enabled:=False;
     exit;
  end;

  memo1.Append('Wait !!!, Firmata starting....');
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
  memo1.lines.add('Firmata started in, '+inttostr(Board1.StartingTime)+' milisec');
  memo1.lines.add('Firmata Firmware:' + Board1.FirmataFirmware);
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
    for i:=0 to 12 do
    begin
      Modes.AddItem(PinModesString[ByteToPinModes(i)], nil);
    end;
    Valuewrite.Text:='0';
    Pins.ItemIndex:=0;
    Pins.Visible:=True;
    Modes.Visible:=True;
    Pin2.Pin:=StrToInt(Pins.Items[Pins.ItemIndex]);
    Modes.ItemIndex:=1;
    Pin2.Mode:=ByteToPinModes(Modes.ItemIndex);
    Pin2.Enabled:=true;
    Pin13.Mode:=PIN_MODE_OUTPUT;
    Pin13.Enabled:=true;
    //  SetValue.Enabled:=True;
    SetValue.Visible:=True;
      //Valuewrite.Enabled:=True;
    Valuewrite.Visible:=True;
    CreateTask.Enabled:=True;
    ToggleReport.Visible:=false;
    Label3.Visible:=True;
    Label4.Visible:=True;
    Label6.Visible:=True;
end;

procedure TForm1.Memo1Click(Sender: TObject);
begin
  Memo1.Clear;
  Board1.printPinInfo(Memo1);
end;

procedure TForm1.LedOnClick(Sender: TObject);
begin
  Pin13.WriteValue(HIGH);
end;

procedure TForm1.LedOffClick(Sender: TObject);
begin
  Pin13.WriteValue(LOW);
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

end;

procedure TForm1.configureClick(Sender: TObject);
begin
  LazSerial1.ShowSetupDialog;
  Puerto.text:=LazSerial1.Device;
end;

procedure TForm1.ClosePortClick(Sender: TObject);
begin
    puerto.Enabled:=true;
    closeport.Enabled:=False;
    Openport.Enabled:=True;
    configure.Enabled:=True;
    ledon.Enabled:=false;
    ledoff.Enabled:=false;
    Pins.Enabled:=false;
    Modes.Enabled:=false;
    memo1.Clear;
    Pins.Visible:=False;
    Modes.Visible:=False;
    SetValue.Visible:=false;
    Valuewrite.Visible:=false;
    Valueread.Visible:=false;
    ToggleReport.Enabled:=False;
    TaskExe.Enabled:=false;
    DeleteTask.Enabled:=False;
    CreateTask.Enabled:=False;
    ToggleReport.Visible:=false;
    Task1.Enabled:=false;
    Pin2.Enabled:=False;
    Pin13.Enabled:=false;
    Board1.Enabled:=false;
    Label3.Visible:=false;
    Label4.Visible:=false;
    Label6.Visible:=false;
    Label7.Visible:=false;
    Pins.Clear;
    Modes.Clear;
end;

procedure TForm1.PinsChange(Sender: TObject);
begin
  if Pin2.Pin = StrToInt(Pins.Items[Pins.ItemIndex]) then // pin does not change
     exit;
  if Pin2.Enabled then
  begin
    if Pin2.Reporting then
    begin
      Pin2.ReportPin(False);
      ToggleReport.Checked:=false;
    end;
    Pin2.Enabled:=false;  // disable pin2 to be able to change pin
  end;
  Pin2.Pin:=StrToInt(Pins.Items[Pins.ItemIndex]); // new pin number
  if not Board1.CheckCapability(Pin2.Pin, ByteToPinModes(Modes.ItemIndex)) then
    Modes.ItemIndex:=PinModesToByte(PIN_MODE_OUTPUT);
  Pin2.Mode:=ByteToPinModes(Modes.ItemIndex);
//  if Pin2.Mode in [PIN_MODE_INPUT, PIN_MODE_PULLUP, PIN_MODE_ANALOG] then
//    ToggleReport.Visible:=True
//  else
//    ToggleReport.Visible:=false;
  Pin2.Enabled:=True;
  ToggleReport.Checked:=Pin2.Reporting;
end;

procedure TForm1.DeleteTaskClick(Sender: TObject);
begin
  Task1.Enabled:=false;
  TaskExe.Enabled:=false;
  DeleteTask.Enabled:=false;
end;

procedure TForm1.CreateTaskClick(Sender: TObject);
var
  TaskString: string;
begin
  Task1.TaskID:=1;
  Task1.TimeDelay:=1500; // delay in delaytask
  Task1.RunDelay:=1; // delay before run task

  // The last "false" value in functions means not write in board, only get command string
  TaskString:=Pin13.SetDigitalPinValue(1, false); // set pin 13 on
  TaskString:=TaskString+Task1.DelayTask(false); // delay 1500 ms
  TaskString:=TaskString+Pin13.SetDigitalPinValue(0, false); // set pin 13 off
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

procedure TForm1.ToggleReportChange(Sender: TObject);
begin
  if toggleReport.Checked then
    ToggleReport.Caption:='Report enabled'
  else
    ToggleReport.caption:='Report disabled';
end;

procedure TForm1.TogglereportClick(Sender: TObject);
begin
   Pin2.ReportPin(ToggleReport.Checked);
end;

procedure TForm1.ModesChange(Sender: TObject);
begin
   if Pin2.Mode=ByteToPinModes(Modes.ItemIndex) then
     exit;
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

   if Pin2.Mode <> ByteToPinModes(Modes.ItemIndex) then
      begin
        if Pin2.Enabled then
        begin
          Pin2.Enabled:=false;
          ToggleReport.Checked:=false;
        end;
        Pin2.Mode:=ByteToPinModes(Modes.ItemIndex);  // new pin mode
        Valuewrite.Text:='0';
        Pin2.Enabled:=True;   // enable set new pin mode
        ToggleReport.Checked:=Pin2.Reporting;
      end;
end;

procedure TForm1.ValuewriteEditingDone(Sender: TObject);
var
  Value: integer;
  Valid: Boolean;
begin
  Valid:=False;
  Value:=0;
  if Pin2.Mode in [PIN_MODE_OUTPUT, PIN_MODE_PWM, PIN_MODE_SERVO,
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
  if Pin2.Enabled then
    Pin2.WriteValue(StrToInt(Valuewrite.Text));
end;

procedure TForm1.Pin2PinValue(sender: TObject; Value: integer);  // analogpin
begin
  ValueRead.Text:=IntTostr(Value);
end;

procedure TForm1.Task1QueryTask(sender: TObject; Time: integer; Length: integer; Place: integer; TaskData: String);
begin
  Memo1.Lines.add('TaskID='+inttostr(Task1.TaskID)+' time='+inttostr(time)+' length='+inttostr(length)+' Data='+StrToHexSep(TaskData));
  if Length <> system.Length(TaskData) then
    Memo1.Lines.Add('Bad task length');
end;

function TForm1.Board1DeviceDataAvailable(sender: TObject): Boolean;
begin
  Result:=LazSerial1.SynSer.CanReadEx(100);
end;

function TForm1.Board1GetDataFromDevice(sender: TObject): string;
begin
   Result:=LazSerial1.ReadData;
end;

procedure TForm1.Board1SendDataToDevice(sender: TObject; str: string);
begin
  LazSerial1.WriteData(str);
end;

procedure TForm1.Task1TaskError(sender: TObject; Time: integer; Length: integer; Place: integer; TaskData: String);
begin
  Memo1.Lines.add('Error TaskID='+inttostr(Task1.TaskID)+' time='+inttostr(time)+' length='+inttostr(length)+'Lugar='+inttostr(Place)+' Data='+StrToHexSep(TaskData));
end;




end.

