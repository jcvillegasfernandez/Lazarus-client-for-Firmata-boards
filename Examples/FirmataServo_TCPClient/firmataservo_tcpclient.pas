unit firmataservo_tcpclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, ExtCtrls, firmataconstants, firmata, firmataboard,
  blcksock, synsock;

type
   { TForm1 }

  TForm1 = class(TForm)
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    loop: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Board1: TBoard;
    Pins: TComboBox;
    Port: TEdit;
    Servo1: TServo;
    Value_write: TEdit;
    SetValue: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Server: TEdit;
    OpenPort: TButton;
    ClosePort: TButton;

    procedure Board1AfterClose(sender: TObject);
    procedure Board1BeforeOpen(sender: TObject);
    function Board1DeviceDataAvailable(sender: TObject): Boolean;
    procedure Board1Error(sender: TObject; Error: integer; TextError: string; Afected: integer);
    procedure Board1FirmataData(sender: TObject; Command: Byte; Data: string);
    procedure Board1FirmataReady(sender: TObject);
    function Board1GetDataFromDevice(sender: TObject): string;
    procedure Board1SendDataToDevice(sender: TObject; str: string);
    procedure FormCreate(Sender: TObject);
    procedure loopClick(Sender: TObject);
    procedure PinsChange(Sender: TObject);
    procedure SetValueClick(Sender: TObject);
    procedure Memo1Click(Sender: TObject);
    procedure OpenPortClick(Sender: TObject);
    procedure ClosePortClick(Sender: TObject);
    procedure Value_writeEditingDone(Sender: TObject);
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
  client:=TTCPBlockSocket.Create;
  //client.OnStatus:=@SocketStatusHandler;
 //  client.OnMonitor:=@MonitorSocket;

   // Enable Firmata
   Board1.Enabled:=true;

   Port.Enabled:=false;
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
  Memo1.Clear;
  memo1.Append('Wait !!!, Firmata starting....');

  // Open way of comunication
  Client.Connect(server.text, port.text);

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
  //client.OnMonitor:=nil;
  memo1.lines.add('Firmata started in, '+inttostr(Board1.StartingTime)+' milisec');
  memo1.lines.add('Firmata Firmare:' + Board1.FirmataFirmware);

  Board1.printPinInfo(Memo1);

  // Fill combobox with compatible pins
  for i:=0 to Board1.PinsNumber - 1 do
  begin
    if Board1.CheckCapability(i, PIN_MODE_SERVO) then
      Pins.AddItem(IntTostr(i),nil);
  end;

  if Pins.Items.Count = 0 then // SERVO module is not installed in ConfigurableFirmata
  begin
    memo1.Lines.add('');
    memo1.Lines.add('Servo module is not installed, or there isn''t a free supported pin in ConfigurableFirmata');
    exit;
  end;

  Pins.Enabled:=True;
  if Pins.Items.IndexOf(inttostr(Servo1.Pin)) <> -1 then
    Pins.ItemIndex:=Pins.Items.IndexOf(inttostr(Servo1.Pin))
  else
  begin
    Pins.ItemIndex:=0;  // First pin
    Servo1.Pin:=strtoint(Pins.Text);
  end;
  Servo1.Enabled:=True;

  SetValue.Enabled:=True;
  Value_write.Enabled:=True;

  Value_write.Text:='0';
end;

procedure TForm1.Memo1Click(Sender: TObject);
begin
  Memo1.Clear;
  Board1.printPinInfo(Memo1);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    memo1.Enabled:=true;
    Memo1.Clear;

    //Puerto.Text:='3030';
    //Server.Text:='192.168.10.11';
end;

procedure TForm1.loopClick(Sender: TObject);
var
  i: integer;
begin
  i := 0;
  while i < 181 do
  begin
    inc(i, 5);
    Servo1.Value:=i;
    sleep(200);  // can't do faster because of TCP Buffer
  end;
  while i > 0 do
  begin
    dec(i, 5);
    Servo1.Value:=i;
    sleep(200);   // can't do faster because of TCP Buffer
  end;
end;

procedure TForm1.PinsChange(Sender: TObject);
begin
     if Servo1.Pin = strtoint(Pins.Text) then   // same pin
       exit;
     Servo1.Enabled:=false; // in order to be able to change onewire pin
     Servo1.Pin:=strtoint(Pins.Text);   // new pin
     Servo1.Enabled:=True; // enable onewire again
     Memo1.Lines.Add('Servo pin set to '+Pins.Text);
end;

procedure TForm1.ClosePortClick(Sender: TObject);
begin
    //client.OnStatus:=@SocketStatusHandler;
    //client.OnMonitor:=@MonitorSocket;

    Server.Enabled:=true;
    closeport.Enabled:=False;
    Openport.Enabled:=True;
    port.Enabled:=True;

    memo1.Clear;
    Pins.Enabled:=false;
    Pins.Clear;

    Servo1.Enabled:=false;

    SetValue.Enabled:=false;
    Value_write.Enabled:=false;

    Board1.Enabled:=false;
end;

procedure TForm1.Value_writeEditingDone(Sender: TObject);
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
      TEdit(Sender).Text:='0';
    end;
  end;
end;

procedure TForm1.SetValueClick(Sender: TObject);
begin
   Servo1.Value:=strtoInt(Value_write.Text); //TODO
end;

function TForm1.Board1DeviceDataAvailable(sender: TObject): Boolean;
begin
  Result:=client.CanReadEx(5);
end;

function TForm1.Board1GetDataFromDevice(sender: TObject): string;
begin
  Result:=client.RecvPacket(0);
end;

procedure TForm1.Board1SendDataToDevice(sender: TObject; str: string);
begin
  client.SendString(str);
end;


end.

