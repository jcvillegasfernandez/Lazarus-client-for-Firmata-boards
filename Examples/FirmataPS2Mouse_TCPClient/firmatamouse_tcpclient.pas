unit firmatamouse_tcpclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, ExtCtrls, firmataconstants, firmata, firmataboard, blcksock,
  synsock;

type
   { TForm1 }

  TForm1 = class(TForm)
    Board1: TBoard;
    ButtonFive: TCheckBox;
    ButtonLeft: TCheckBox;
    ButtonFour: TCheckBox;
    ButtonRight: TCheckBox;
    ButtonMiddle: TCheckBox;
    GetMouseData: TButton;
    Edit1: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Server: TEdit;
    Reporting: TToggleBox;
    x_Edit: TEdit;
    Label3: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    PS2Mouse1: TPS2Mouse;
    Puerto: TEdit;
    OpenPort: TButton;
    ClosePort: TButton;
    y_Edit: TEdit;
    z_Edit: TEdit;
    procedure Board1AfterClose(sender: TObject);
    procedure Board1BeforeOpen(sender: TObject);
    function Board1DeviceDataAvailable(sender: TObject): Boolean;
    procedure Board1Error(sender: TObject; Error: integer; TextError: string; Afected: integer);
    procedure Board1FirmataData(sender: TObject; Command: Byte; Data: string);
    procedure Board1FirmataReady(sender: TObject);
    function Board1GetDataFromDevice(sender: TObject): string;
    procedure Board1SendDataToDevice(sender: TObject; str: string);
    procedure FormCreate(Sender: TObject);
    procedure GetMouseDataClick(Sender: TObject);
    procedure Memo1Click(Sender: TObject);
    procedure OpenPortClick(Sender: TObject);
    procedure ClosePortClick(Sender: TObject);
    procedure PS2Mouse1MouseData(sender: TObject; MouseData: TPS2MouseData);
    procedure PS2Mouse1MouseDeviceID(sender: TObject; MouseType: TMouseType);
    procedure PS2Mouse1MouseStatus(sender: TObject; MouseStatus: TPS2MouseStatus);
    procedure ReportingChange(Sender: TObject);
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
  x : integer;
  y : integer;
  z : integer;
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

procedure TForm1.Board1AfterClose(sender: TObject);
begin
  client.CloseSocket;
end;

procedure TForm1.Board1BeforeOpen(sender: TObject);
begin
  // Open way of comunication
  Memo1.Clear;
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
begin
  //client.OnStatus:=nil;
 //  client.OnMonitor:=nil;
  memo1.lines.add('Firmata started in, '+inttostr(Board1.StartingTime)+' milisec');
  memo1.lines.add('Firmata Firmare:' + Board1.FirmataFirmware);
  Board1.printPinInfo(Memo1);

  PS2Mouse1.Enabled:=True;
  if not PS2Mouse1.Enabled then
  begin
    memo1.Lines.add('');
    memo1.Lines.add('PS2Mouse module is not installed or there aren''t any free supported pins in ConfigurableFirmata');
   exit;
  end;
  PS2Mouse1.QueryDeviceID;
  Reporting.Enabled:=True;
  GetMouseData.Enabled:=True;
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

  Edit1.Text:='STILL UNKNOWN';
  //Puerto.Text:='3030';
  //  Server.Text:='192.168.10.11';
end;

procedure TForm1.ClosePortClick(Sender: TObject);
begin
    client.OnStatus:=@SocketStatusHandler;
    //client.OnMonitor:=@MonitorSocket;
    Reporting.Enabled:=False;
    Reporting.Checked:=False;
    GetMouseData.Enabled:=False;
    ButtonFour.Visible:=false;
    ButtonFive.Visible:=false;
    Edit1.Text:='STILL UNKNOWN';

    puerto.Enabled:=true;
    closeport.Enabled:=False;
    Openport.Enabled:=True;

    memo1.Clear;
    PS2Mouse1.Enabled:=false;
    Board1.Enabled:=false;
end;

procedure TForm1.GetMouseDataClick(Sender: TObject);
begin
  PS2Mouse1.QueryData;
end;

procedure TForm1.PS2Mouse1MouseData(sender: TObject; MouseData: TPS2MouseData);
var
  rel_x: smallint;
  rel_y: smallint;
  rel_z: shortint;
begin
  rel_x := MouseData.Position.x - ((MouseData.StatusMove << 4) and $100);
  rel_y := MouseData.Position.y - ((MouseData.StatusMove << 3) and $100);

  x:=x+rel_x;
  y:=y+rel_y;
  x_Edit.Text:=IntToStr(x);
  y_Edit.Text:=intToStr(y);
  { TPS2MouseData = record
    StatusMove: Byte;
    Position: TPosition;
    Wheel: Byte; // is z move for Intelli mouse or TWheelBits for five-buttons mouse
  end;}

      { Wheel = z on INTELLI_MOUSE}

      { TWheelBits = bitpacked record  for FIVEBUTTONS
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
        leff_button: Boolean;
        end;       }
  ButtonLeft.Checked:=Boolean(MouseData.StatusMove and 1);
  ButtonRight.Checked:=Boolean(MouseData.StatusMove and 2);
  ButtonMiddle.Checked:=Boolean(MouseData.StatusMove and 4);
  if PS2Mouse1.MouseType = INTELLI_MOUSE then
    rel_z:=MouseData.Wheel
  else if PS2Mouse1.MouseType = FIVEBUTTONS then
  begin
    rel_z:=(MouseData.Wheel xor $FF) + 1;
    ButtonFour.Checked:=Boolean(MouseData.Wheel and $10);
    ButtonFive.Checked:=Boolean(MouseData.Wheel and $20);
  end
  else
  begin
    rel_z:=0;
    z:=0;
  end;
  z:=z+rel_z;
  z_Edit.Text:=intToStr(z);
end;

procedure TForm1.PS2Mouse1MouseDeviceID(sender: TObject; MouseType: TMouseType);
begin
   case MouseType of
     INTELLI_MOUSE: begin
       ButtonFour.Visible:=False;
       BUttonFive.Visible:=False;
       Edit1.Text:='INTELLI MOUSE';
     end;
     FIVEBUTTONS: begin
       ButtonFour.Visible:=True;
       ButtonFive.Visible:=true;
       Edit1.Text:='FIVE BUTTONS';
     end;
     STANDARD: begin
       ButtonFour.Visible:=false;
       ButtonFive.Visible:=false;
       Edit1.Text:='STANDARD';
     end;
   end;
end;

procedure TForm1.PS2Mouse1MouseStatus(sender: TObject; MouseStatus: TPS2MouseStatus);
begin
  ;
end;

procedure TForm1.ReportingChange(Sender: TObject);
begin
  if Reporting.Checked then
  begin
    Reporting.Caption:='Reporting is On';
    PS2Mouse1.Reporting:=True;
    GetMouseData.Enabled:=False;
  end
  else
  begin
    Reporting.Caption:='Reporting is Off';
    PS2Mouse1.Reporting:=False;
    GetMouseData.Enabled:=True;
  end;
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

