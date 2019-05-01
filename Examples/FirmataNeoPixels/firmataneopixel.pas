unit firmataneopixel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, ExtCtrls, firmataconstants, firmata, firmataboard, LCLIntf,
  LazSerial, fill_with_color, fade_config, pixel_color, shift_config, copy_pixels;

type
   { TForm1 }

  TForm1 = class(TForm)
    copypixels: TButton;
    shiftconfig: TButton;
    gamma: TEdit;
    Label13: TLabel;
    Label16: TLabel;
    Label4: TLabel;
    runfade: TButton;
    fadeconfig: TButton;
    onestepfade: TButton;
    ShowPixel: TCheckBox;
    Show_pixels: TButton;
    shift_left: TButton;
    fill_color: TButton;
    setPixeln: TButton;
    shift_right: TButton;
    Label5: TLabel;
    Label7: TLabel;
    Label3: TLabel;
    Board1: TBoard;
    LazSerial1: TLazSerial;
    NeoPixel1: TNeoPixel;
    Pins: TComboBox;
    configure: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    PortName: TEdit;
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
    procedure fadeconfigClick(Sender: TObject);
    procedure fill_colorClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure gammaEditingDone(Sender: TObject);
    procedure Memo1Click(Sender: TObject);
    procedure copypixelsClick(Sender: TObject);
    procedure NeoPixel1FadeEnd(sender: TObject);
    procedure onestepfadeClick(Sender: TObject);
    procedure OpenPortClick(Sender: TObject);
    procedure ClosePortClick(Sender: TObject);
    procedure runfadeClick(Sender: TObject);
    procedure setPixelnClick(Sender: TObject);
    procedure shiftconfigClick(Sender: TObject);
    procedure shift_leftClick(Sender: TObject);
    procedure shift_rightClick(Sender: TObject);
    procedure ShowPixelChange(Sender: TObject);
    procedure Show_pixelsClick(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
  end;

procedure ShowForm(Form: TForm);

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

  PortName.Enabled:=false;
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
  LazSerial1.Device:=PortName.Text;
  LazSerial1.Open;
  if LazSerial1.active=false then
  begin
     Board1.Enabled:=False;
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

  // fill_color combobox with compatible pins
  for i:=0 to Board1.PinsNumber - 1 do
  begin
    if Board1.CheckCapability(i, PIN_MODE_NEOPIXELS) then
      Pins.AddItem(IntTostr(i),nil);
  end;

  if Pins.Items.Count = 0 then // NEOPIXELS module is not installed in ConfigurableFirmata
  begin
    memo1.Lines.add('');
    memo1.Lines.add('Servo module is not installed, or there isn''t a free supported pin in ConfigurableFirmata');
    exit;
  end;

  Pins.Enabled:=True;
  if Pins.Items.IndexOf(inttostr(NeoPixel1.Pin)) <> -1 then
    Pins.ItemIndex:=Pins.Items.IndexOf(inttostr(NeoPixel1.Pin))
  else
  begin
    Pins.ItemIndex:=0;  // First pin
    NeoPixel1.Pin:=strtoint(Pins.Text);
  end;
  NeoPixel1.Enabled:=True;

  gamma.Enabled:=True;
  gamma.Text:=floattostr(neopixel1.Gamma);
  setpixeln.Enabled:=True;
  fill_color.Enabled:=true;
  shiftconfig.enabled:=true;
  fadeconfig.enabled:=true;
  copypixels.enabled:=true;

  Shift_right.Enabled:=true;
  Shift_left.enabled:=true;

  onestepfade.enabled:=true;
  runfade.enabled:=true;

  openPort.Enabled:=false;
  configure.enabled:=false;
  PortName.Enabled:=false;
  Closeport.Enabled:=true;

end;

procedure TForm1.Memo1Click(Sender: TObject);
begin
  Memo1.Clear;
  Board1.printPinInfo(Memo1);
end;

procedure TForm1.NeoPixel1FadeEnd(sender: TObject);
begin
  memo1.Lines.Add('Fade end');
end;

procedure TForm1.onestepfadeClick(Sender: TObject);
begin
  NeoPixel1.FadeOneStep(showpixel.checked);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    memo1.Enabled:=true;
    Memo1.Clear;
{$IFDEF LINUX}
    PortName.Text:='/dev/ttyUSB0';
{$ELSE}
    PortName.Text:='COM1';
{$ENDIF}
  LazSerial1.Device:=PortName.Text;
  LazSerial1.BaudRate:=br_57600;
  LazSerial1.FlowControl:=fcNone;
  LazSerial1.StopBits:=sbOne;
  LazSerial1.DataBits:=db8bits;
end;

procedure TForm1.gammaEditingDone(Sender: TObject);
begin
  NeoPixel1.Gamma:=strtofloat(gamma.text);
end;

procedure TForm1.configureClick(Sender: TObject);
begin
  LazSerial1.ShowSetupDialog;
  PortName.text:=LazSerial1.Device;
end;

procedure TForm1.setPixelnClick(Sender: TObject);
begin
  ShowForm(Pixel_color1);
end;

procedure TForm1.fill_colorClick(Sender: TObject);
begin
  ShowForm(Fill_with_color1);
end;

procedure TForm1.shiftconfigClick(Sender: TObject);
begin
  ShowForm(Shift_config1);
end;

procedure TForm1.fadeconfigClick(Sender: TObject);
begin
  ShowForm(Fade_config1);
end;

procedure TForm1.copypixelsClick(Sender: TObject);
begin
  ShowForm(Copy_Pixels1);
end;

procedure TForm1.ClosePortClick(Sender: TObject);
begin
    neopixel1.Off;
    PortName.Enabled:=true;
    closeport.Enabled:=False;
    Openport.Enabled:=True;
    configure.Enabled:=True;

    memo1.Clear;
    Pins.Enabled:=false;
    Pins.Clear;
    gamma.Enabled:=false;

    setpixeln.Enabled:=false;
    fill_color.Enabled:=false;
    shiftconfig.enabled:=false;
    fadeconfig.enabled:=false;
    copypixels.enabled:=false;

    Shift_right.Enabled:=false;
    Shift_left.enabled:=false;

    onestepfade.enabled:=false;
    runfade.enabled:=false;

    Board1.Enabled:=false;
end;

procedure TForm1.runfadeClick(Sender: TObject);
begin
  if Neopixel1.FadeRunning then
  begin
    NeoPixel1.FadeRunPause; // Stop fade()
  end
  else
    NeoPixel1.FadeRunning:=True;  // run fade
end;

procedure TForm1.shift_rightClick(Sender: TObject);
begin
  //  shif_type    bit0 = 1 right 0=left, bit1 = 1 wrap, bit2 = 1 do_show
  NeoPixel1.ShiftRun((NeoPixel1.ShiftType and $FE) or 1);  // shift right
end;

procedure TForm1.ShowPixelChange(Sender: TObject);
begin
  if ShowPixel.checked then
    ShowPixel.caption:='Show pixels'
  else
    ShowPixel.caption:='Not show';
end;

procedure TForm1.Show_pixelsClick(Sender: TObject);
begin
  neopixel1.Show;
end;

procedure TForm1.shift_leftClick(Sender: TObject);
begin
  //  shif_type    bit0 = 1 right 0=left, bit1 = 1 wrap, bit2 = 1 do_show
  NeoPixel1.ShiftRun(NeoPixel1.ShiftType and $FE);  // shift left
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

procedure ShowForm(Form: TForm);
begin
  Application.Minimize;
  //Formprincipal.hide;
  //Form.Top:=Top+30;
  //Form.Left:=Left;
  Form.Position:=poMainFormCenter;
  Form.ShowModal;
  //Formprincipal.Show;
  Application.Restore;
end;

end.

