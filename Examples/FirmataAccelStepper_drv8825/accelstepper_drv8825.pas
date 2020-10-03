unit accelstepper_drv8825;

//{$mode objfpc}{$H+}
{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, ExtCtrls, LazSerial, firmataconstants, FirmataBoard,
  lazsynautil;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label7: TLabel;
    Display: TLabel;
    FastStop: TButton;
    Steps: TEdit;
    Move: TButton;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    MotorDevice: TEdit;
    AccelStepper1: TAccelStepper;
    LazSerial1: TLazSerial;
    Label4: TLabel;
    Board1: TBoard;
    configure: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Puerto: TEdit;
    OpenPort: TButton;
    ClosePort: TButton;
    ToggleBox1: TToggleBox;

    procedure AccelStepper1StepperMoveCompleted(sender: TObject; Device: byte; Position: integer);
    procedure AccelStepper1StepperPosition(sender: TObject; Device: byte; Position: integer);
    procedure Board1AfterClose(sender: TObject);
    procedure Board1BeforeOpen(sender: TObject);
    function Board1DeviceDataAvailable(sender: TObject): Boolean;
    procedure Board1Error(sender: TObject; Error: integer; TextError: string; Afected: integer);
    procedure Board1FirmataData(sender: TObject; Command: Byte; Data: string);
    procedure Board1FirmataReady(sender: TObject);
    function Board1GetDataFromDevice(sender: TObject): integer;
    procedure Board1SendDataToDevice(sender: TObject; str: string);
    procedure FastStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure configureClick(Sender: TObject);
    procedure MoveClick(Sender: TObject);
    procedure Memo1Click(Sender: TObject);
    procedure OpenPortClick(Sender: TObject);
    procedure ClosePortClick(Sender: TObject);
    procedure StepsEditingDone(Sender: TObject);
    procedure stopClick(Sender: TObject);
    procedure ToggleBox1Click(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.OpenPortClick(Sender: TObject);
begin
  memo1.Clear;

  Board1.Enabled:=true;

  Puerto.Enabled:=false;
  closeport.Enabled:=True;
  configure.Enabled:=false;
  Openport.Enabled:=False;
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

procedure TForm1.configureClick(Sender: TObject);
begin
  LazSerial1.ShowSetupDialog;
  Puerto.text:=LazSerial1.Device;
end;

procedure TForm1.MoveClick(Sender: TObject);
begin
  AccelStepper1.MotorEnable(True);  // enable motor
  if Copy(Move.Caption, 1, 4) = 'Move' then
  begin
    AccelStepper1.Steps:=strtoint(steps.text);

    if ToggleBox1.Checked then
      AccelStepper1.MoveTo
    else
      AccelStepper1.Move;
    Move.Caption:='Stop';
    FastStop.Visible:=true;
  end
  else if Move.Caption = 'Stop' then  // Stop
  begin
    Move.Caption:='Wait!!!, stopping';
    AccelStepper1.Stop;  // smooth stop
  end;
end;
 
procedure TForm1.FastStopClick(Sender: TObject);
begin
  AccelStepper1.FastStop;  // Fast stop
end;

procedure TForm1.AccelStepper1StepperMoveCompleted(sender: TObject; Device: byte; Position: integer);
begin
  AccelStepper1.MotorEnable(False);  // disable motor
  if Copy(Move.Caption, 1, 4) <> 'Move' then
  begin
    if FastStop.Visible or (Move.Caption <> 'Stop') then // motor has been stopped
      Memo1.Lines.Add('Motor has been stopped');
    if ToggleBox1.Checked then
      Move.Caption:='Move to'
    else
      Move.Caption:='Move';
  end;
  FastStop.Visible:=False;
  Memo1.Lines.Add('New position: '+inttostr(Position));
end;

procedure TForm1.AccelStepper1StepperPosition(sender: TObject;
  Device: byte; Position: integer);
begin
    Memo1.Lines.Add('Position: '+inttostr(Position))
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
begin
  Display.Caption:='Acceleration: '+floattoStr(AccelStepper1.Acceleration)+LineEnding+'Speed: '+floattoStr(AccelStepper1.Speed);
  memo1.clear;
  memo1.lines.add('Firmata started in, '+inttostr(Board1.StartingTime)+' milisec');
  memo1.lines.add('Firmata Firmare:' + Board1.FirmataFirmware);
  Board1.printPinInfo(Memo1);

  AccelStepper1.Enabled:=True; // enable accelStepper

  if not AccelStepper1.Enabled then
  begin
    memo1.Lines.add('');
    memo1.Lines.add('AccelStepper module is not installed or there aren''t any free supported pins in ConfigurableFirmata');
    exit;
  end;
  move.Enabled:=True;
  AccelStepper1.MotorEnable(False);  // disable motor
end;

procedure TForm1.ClosePortClick(Sender: TObject);
begin
    if AccelStepper1.Running then
    begin
      AccelStepper1.Enabled:=false;
      //sleep(round(AccelStepper1.Speed/AccelStepper1.Acceleration*1000)); // max wait for smooth stop
    end;
    if ToggleBox1.Checked then
      Move.Caption:='Move to'
    else
      Move.Caption:='Move';
    move.Enabled:=false;
    memo1.Clear;
    puerto.Enabled:=true;
    closeport.Enabled:=False;
    Openport.Enabled:=True;
    configure.Enabled:=True;
    FastStop.Visible:=False;

    Board1.enabled:=false;
end;

procedure TForm1.StepsEditingDone(Sender: TObject);
var
  value: integer;
  Test: Boolean;
begin
  try
    Test:=False;
    value:=strtoint(Steps.Text);
    Test:=True;
  finally
    if Test then // error
      AccelStepper1.Steps:=Value
    else
      Steps.Text:='0';
  end;
end;

procedure TForm1.stopClick(Sender: TObject);
begin
  AccelStepper1.Stop(true);
end;

procedure TForm1.ToggleBox1Click(Sender: TObject);
begin
  if ToggleBox1.Checked then
  begin
    ToggleBox1.Caption:='Absolute';
    Move.Caption:='Move to';
  end
  else
  begin
    ToggleBox1.Caption:='Relative';
    Move.Caption:='Move';
  end;
end;

procedure TForm1.Board1SendDataToDevice(sender: TObject; str: string);
begin
  LazSerial1.WriteData(str);
end;



function TForm1.Board1GetDataFromDevice(sender: TObject): integer;
begin
  if LazSerial1.SynSer.CanReadEx(0) then
    Result:=LazSerial1.SynSer.RecvByte(100)
  else
    Result:=-1;
end;

procedure TForm1.Board1BeforeOpen(sender: TObject);
begin
  // Open way of comunication
  LazSerial1.Device:=Puerto.Text;
  LazSerial1.Open;
  if LazSerial1.active=false then
  begin
     Board1.Enabled:=False;
     memo1.Append('Could not open port');
     exit;
  end;
  Memo1.Clear;
  memo1.Append('Wait !!!, Firmata starting....');

end;

function TForm1.Board1DeviceDataAvailable(sender: TObject): Boolean;
begin
  Result:=LazSerial1.Synser.CanReadEx(0);
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

end.

