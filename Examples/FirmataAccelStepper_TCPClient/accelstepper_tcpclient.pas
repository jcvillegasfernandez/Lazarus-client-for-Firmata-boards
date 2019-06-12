unit accelstepper_tcpclient;

//{$mode objfpc}{$H+}
{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, ExtCtrls, firmataconstants, FirmataBoard, blcksock, synsock;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label7: TLabel;
    Display: TLabel;
    FastStop: TButton;
    Label8: TLabel;
    Server: TEdit;
    Steps: TEdit;
    Move: TButton;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    MotorDevice: TEdit;
    AccelStepper1: TAccelStepper;
    Label4: TLabel;
    Board1: TBoard;
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
    procedure Board1Error(sender: TObject; Error: integer; TextError: string; Afected: integer);
    procedure Board1FirmataData(sender: TObject; Command: Byte; Data: string);
    procedure Board1FirmataReady(sender: TObject);
    function Board1GetDataFromDevice(sender: TObject): string;
    function Board1DeviceDataAvailable(sender: TObject): Boolean;
    procedure Board1SendDataToDevice(sender: TObject; str: string);
    procedure FastStopClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MoveClick(Sender: TObject);
    procedure Memo1Click(Sender: TObject);
    procedure OpenPortClick(Sender: TObject);
    procedure ClosePortClick(Sender: TObject);
    procedure StepsEditingDone(Sender: TObject);
    procedure stopClick(Sender: TObject);
    procedure ToggleBox1Click(Sender: TObject);
    procedure PuertoEditingDone(Sender: TObject);
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

procedure TForm1.FormCreate(Sender: TObject);
begin
    memo1.Enabled:=true;
    Memo1.Clear;
    //Puerto.Text:='3030';
    //  Server.Text:='192.168.10.11';

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

procedure TForm1.Board1AfterClose(sender: TObject);
begin
  client.CloseSocket;
end;

procedure TForm1.MoveClick(Sender: TObject);
begin
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

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ClosePortClick(self);
end;

procedure TForm1.AccelStepper1StepperMoveCompleted(sender: TObject; Device: byte; Position: integer);
begin
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

    //client.OnStatus:=nil;
//  client.OnMonitor:=nil;
  memo1.lines.add('Firmata started in, '+inttostr(Board1.StartingTime)+' milisec');
  memo1.lines.add('Firmata Firmare:' + Board1.FirmataFirmware);
  {AccelStepper1.MotorPin1_or_DriverStep:=8;
  AccelStepper1.MotorPin2_or_DriverDirection:=10;
  AccelStepper1.MotorPin3:=9;
  AccelStepper1.MotorPin4:=11;}
  AccelStepper1.StepSize:=HALF_STEP;
  Board1.printPinInfo(Memo1);

  AccelStepper1.Enabled:=True; // enable accelStepper
  if not AccelStepper1.Enabled then
  begin
    memo1.Lines.add('');
    memo1.Lines.add('AccelStepper module is not installed or there aren''t any free supported pins in ConfigurableFirmata');
   exit;
  end;
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
    memo1.Clear;
    //client.OnStatus:=@SocketStatusHandler;
    //client.OnMonitor:=@MonitorSocket;
    puerto.Enabled:=true;
    closeport.Enabled:=False;
    Openport.Enabled:=True;

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
  client.SendString(str);
end;

function TForm1.Board1GetDataFromDevice(sender: TObject): string;
begin
  Result:=client.RecvPacket(0);
end;

function TForm1.Board1DeviceDataAvailable(sender: TObject): Boolean;
begin
  Result:=client.CanReadEx(5);
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
end.

