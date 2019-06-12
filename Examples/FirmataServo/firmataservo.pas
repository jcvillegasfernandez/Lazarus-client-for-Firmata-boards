unit firmataservo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, ExtCtrls, firmataconstants, firmata, firmataboard,
  LazSerial;

type
   { TForm1 }

  TForm1 = class(TForm)
    Label5: TLabel;
    loop: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Board1: TBoard;
    LazSerial1: TLazSerial;
    Pins: TComboBox;
    Servo1: TServo;
    Valuewrite: TEdit;
    SetValue: TButton;
    configure: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Puerto: TEdit;
    OpenPort: TButton;
    ClosePort: TButton;

    procedure Board1AfterClose(sender: TObject);
    procedure Board1BeforeOpen(sender: TObject);
    function Board1DeviceDataAvailable(sender: TObject): Boolean;
    procedure Board1Error(sender: TObject; Error: integer; TextError: string; Afected: integer);
    procedure Board1FirmataData(sender: TObject; Command: Byte; Data: string);
    procedure Board1FirmataReady(sender: TObject);
    function Board1GetDataFromDevice(sender: TObject): integer;
    procedure Board1SendDataToDevice(sender: TObject; str: string);
    procedure configureClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure loopClick(Sender: TObject);
    procedure PinsChange(Sender: TObject);
    procedure SetValueClick(Sender: TObject);
    procedure Memo1Click(Sender: TObject);
    procedure OpenPortClick(Sender: TObject);
    procedure ClosePortClick(Sender: TObject);
    procedure ValuewriteEditingDone(Sender: TObject);

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
var
  i: Integer;
begin
  memo1.clear;
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
  Valuewrite.Enabled:=True;

  Valuewrite.Text:='0';
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
    sleep(10);
  end;
  while i > 0 do
  begin
    dec(i, 5);
    Servo1.Value:=i;
    sleep(10);
  end;
end;

procedure TForm1.configureClick(Sender: TObject);
begin
  LazSerial1.ShowSetupDialog;
  Puerto.text:=LazSerial1.Device;
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
    puerto.Enabled:=true;
    closeport.Enabled:=False;
    Openport.Enabled:=True;
    configure.Enabled:=True;

    memo1.Clear;
    Pins.Enabled:=false;
    Pins.Clear;

    Servo1.Enabled:=false;

    SetValue.Enabled:=false;
    Valuewrite.Enabled:=false;

    Board1.Enabled:=false;
end;

procedure TForm1.ValuewriteEditingDone(Sender: TObject);
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
   Servo1.Value:=strtoInt(valuewrite.Text); //TODO
end;

function TForm1.Board1DeviceDataAvailable(sender: TObject): Boolean;
begin
  Result:=LazSerial1.DataAvailable;
end;

function TForm1.Board1GetDataFromDevice(sender: TObject): integer;
begin
  if LazSerial1.DataAvailable then
    Result:=LazSerial1.SynSer.RecvByte(100)
  else
    Result:=-1; // error
end;

procedure TForm1.Board1SendDataToDevice(sender: TObject; str: string);
begin
  LazSerial1.WriteData(str);
end;


end.

