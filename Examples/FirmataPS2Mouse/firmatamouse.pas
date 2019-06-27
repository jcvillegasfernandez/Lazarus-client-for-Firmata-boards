unit firmatamouse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, ExtCtrls, firmataconstants, firmata, firmataboard,
  LazSerial;

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
    Reporting: TToggleBox;
    x_Edit: TEdit;
    Label3: TLabel;
    LazSerial1: TLazSerial;
    configure: TButton;
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
    procedure configureClick(Sender: TObject);
    procedure GetMouseDataClick(Sender: TObject);
    procedure Memo1Click(Sender: TObject);
    procedure OpenPortClick(Sender: TObject);
    procedure ClosePortClick(Sender: TObject);
    procedure PS2Mouse1MouseData(sender: TObject; MouseData: TPS2MouseData);
    procedure PS2Mouse1MouseDeviceID(sender: TObject; MouseType: TMouseType);
    procedure ReportingChange(Sender: TObject);
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

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.OpenPortClick(Sender: TObject);
begin
  memo1.Clear;

  // Enable Firmata
  Board1.Enabled:=true;

  Puerto.Enabled:=false;
  closeport.Enabled:=True;
  configure.Enabled:=false;
  Openport.Enabled:=False;
end;

procedure TForm1.Board1AfterClose(sender: TObject);
begin
  if LazSerial1.Active then
    LazSerial1.Close;
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
  memo1.clear;
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
  Edit1.Text:='STILL UNKNOWN';
end;

procedure TForm1.configureClick(Sender: TObject);
begin
  LazSerial1.ShowSetupDialog;
  Puerto.text:=LazSerial1.Device;
end;

procedure TForm1.ClosePortClick(Sender: TObject);
begin
    Reporting.Enabled:=False;
    GetMouseData.Enabled:=False;
    ButtonFour.Visible:=false;
    ButtonFive.Visible:=false;
    Edit1.Text:='STILL UNKNOWN';

    puerto.Enabled:=true;
    closeport.Enabled:=False;
    Openport.Enabled:=True;
    configure.Enabled:=True;

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

procedure TForm1.ReportingChange(Sender: TObject);
begin
  if Reporting.Checked then
  begin
    Reporting.Caption:='Reporting On';
    PS2Mouse1.Reporting:=True;
    GetMouseData.Enabled:=False;
  end
  else
  begin
    Reporting.Caption:='Reporting Off';
    PS2Mouse1.Reporting:=False;
    GetMouseData.Enabled:=True;
  end;
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


end.

