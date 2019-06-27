unit firmataneopixel_tcpclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, ExtCtrls, firmataconstants, firmata, firmataboard, blcksock, synsock,
  LCLIntf,
  fill_with_color, fade_config, pixel_color, shift_config, copy_pixels;

type
   { TFormNeo1 }

  TFormNeo1 = class(TForm)
    copypixels: TButton;
    Label6: TLabel;
    Port: TEdit;
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
    NeoPixel1: TNeoPixel;
    Pins: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Server: TEdit;
    OpenPort: TButton;
    ClosePort: TButton;

    procedure Board1AfterClose(sender: TObject);
    procedure Board1BeforeOpen(sender: TObject);
    procedure Board1Error(sender: TObject; Error: integer; TextError: string; Afected: integer);
    procedure Board1FirmataData(sender: TObject; Command: Byte; Data: string);
    procedure Board1FirmataReady(sender: TObject);
    function Board1GetDataFromDevice(sender: TObject): string;
    function Board1DeviceDataAvailable(sender: TObject): Boolean;
    procedure Board1SendDataToDevice(sender: TObject; str: string);
    procedure fadeconfigClick(Sender: TObject);
    procedure fill_colorClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
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
    procedure SocketStatusHandler(Sender: TObject; Reason: THookSocketReason;
                                             const Value: AnsiString);
    procedure MonitorSocket(Sender: TObject; Writing: Boolean;
        const Buffer: TMemory; Len: Integer);
  private
    { private declarations }
  public
    { public declarations }
  end;

procedure ShowForm(Form: TForm);

var
  FormNeo1: TFormNeo1;
  client: TTCPBlockSocket;

implementation

{$R *.lfm}

{ TFormNeo1 }
procedure TFormNeo1.SocketStatusHandler(Sender: TObject; Reason: THookSocketReason;
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

procedure TFormNeo1.MonitorSocket(Sender: TObject; Writing: Boolean;
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

procedure TFormNeo1.OpenPortClick(Sender: TObject);
begin
  memo1.Clear;

  client:=TTCPBlockSocket.Create;
  //client.OnStatus:=@SocketStatusHandler;
  //client.OnMonitor:=@MonitorSocket;

  // Enable Firmata
  Board1.Enabled:=true;

  Port.Enabled:=false;
  Server.Enabled:=false;
  closeport.Enabled:=True;
  Openport.Enabled:=False;
end;

procedure TFormNeo1.Board1AfterClose(sender: TObject);
begin
  client.CloseSocket;
end;

procedure TFormNeo1.Board1BeforeOpen(sender: TObject);
begin
  // Open way of comunication
    memo1.Append('Wait !!!, Firmata starting....');
    Client.Connect(server.text, Port.text);

    if client.LastError <> 0 then
    begin
      Board1.Enabled:=False;
      board1.RaiseError(1002,'Could not open connection');
    end;
end;

procedure TFormNeo1.Board1Error(sender: TObject; Error: integer; TextError: string; Afected: integer);
begin
   ShowMessage(TextError);
end;

procedure TFormNeo1.Board1FirmataData(sender: TObject; Command: Byte;
  Data: string);
begin
   memo1.lines.add('Firmata String:' + Data);
end;

procedure TFormNeo1.Board1FirmataReady(sender: TObject);
var
  i: Integer;
begin
  //client.OnStatus:=nil;
  //client.OnMonitor:=nil;
  memo1.lines.add('Firmata started in, '+inttostr(Board1.StartingTime)+' milisec');
  memo1.lines.add('Firmata Firmare:' + Board1.FirmataFirmware);

  Board1.printPinInfo(Memo1);

  // fill_color combobox with compatible pins
  for i:=0 to Board1.PinsNumber - 1 do
  begin
    if Board1.CheckCapability(i, PIN_MODE_NEOPIXELS) then
      Pins.AddItem(IntTostr(i),nil);
  end;

  if Pins.Items.Count = 0 then // NEOPIXEL module is not installed in ConfigurableFirmata
  begin
    memo1.Lines.add('');
    memo1.Lines.add('NeoPixel module is not installed, or there isn''t a free supported pin in ConfigurableFirmata');
    exit;
  end;

  Pins.Enabled:=True;
  if Pins.Items.IndexOf(inttostr(NeoPixel1.Pin)) <> -1 then
    Pins.ItemIndex:=NeoPixel1.Pin
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
  Show_pixels.enabled:=true;
  Shift_right.Enabled:=true;
  Shift_left.enabled:=true;
  ShowPixel.Enabled:=true;
  onestepfade.enabled:=true;
  runfade.enabled:=true;

  openPort.Enabled:=false;
  Server.Enabled:=false;
  Closeport.Enabled:=true;

end;

procedure TFormNeo1.Memo1Click(Sender: TObject);
begin
  Memo1.Clear;
  Board1.printPinInfo(Memo1);
end;

procedure TFormNeo1.NeoPixel1FadeEnd(sender: TObject);
begin
  memo1.Lines.Add('Fade end');
end;

procedure TFormNeo1.onestepfadeClick(Sender: TObject);
begin
  NeoPixel1.FadeOneStep(showpixel.checked);
end;

procedure TFormNeo1.FormCreate(Sender: TObject);
begin
  memo1.Enabled:=true;
  Memo1.Clear;

  Port.Text:='3030';
  Server.Text:='192.168.10.11';
end;

procedure TFormNeo1.gammaEditingDone(Sender: TObject);
begin
  NeoPixel1.Gamma:=strtofloat(gamma.text);
end;

procedure TFormNeo1.setPixelnClick(Sender: TObject);
begin
  ShowForm(Pixel_color1);
end;

procedure TFormNeo1.fill_colorClick(Sender: TObject);
begin
  ShowForm(Fill_with_color1);
end;

procedure TFormNeo1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ClosePortClick(self);
end;

procedure TFormNeo1.shiftconfigClick(Sender: TObject);
begin
  ShowForm(Shift_config1);
end;

procedure TFormNeo1.fadeconfigClick(Sender: TObject);
begin
  ShowForm(Fade_config1);
end;

procedure TFormNeo1.copypixelsClick(Sender: TObject);
begin
  ShowForm(Copy_Pixels1);
end;

procedure TFormNeo1.ClosePortClick(Sender: TObject);
begin
    neopixel1.Off;
    Server.Enabled:=true;
    Port.Enabled:=True;
    closeport.Enabled:=False;
    Openport.Enabled:=True;

    memo1.Clear;
    Pins.Enabled:=false;
    Pins.Clear;
    gamma.Enabled:=false;

    setpixeln.Enabled:=false;
    fill_color.Enabled:=false;
    shiftconfig.enabled:=false;
    fadeconfig.enabled:=false;
    copypixels.enabled:=false;
    Show_pixels.enabled:=false;
    Shift_right.Enabled:=false;
    Shift_left.enabled:=false;

    ShowPixel.Enabled:=false;
    onestepfade.enabled:=false;
    runfade.enabled:=false;
    //client.OnStatus:=@SocketStatusHandler;
    NeoPixel1.Enabled:=False;
    Board1.Enabled:=false;
end;

procedure TFormNeo1.runfadeClick(Sender: TObject);
begin
  if Neopixel1.FadeRunning then
  begin
    NeoPixel1.FadeRunPause; // Stop fade()
  end
  else
    NeoPixel1.FadeRunning:=True;  // run fade
end;

procedure TFormNeo1.shift_rightClick(Sender: TObject);
begin
  //  shif_type    bit0 = 1 right 0=left, bit1 = 1 wrap, bit2 = 1 do_show
  NeoPixel1.ShiftRun((NeoPixel1.ShiftType and $FE) or 1);  // shift right
end;

procedure TFormNeo1.ShowPixelChange(Sender: TObject);
begin
  if ShowPixel.checked then
    ShowPixel.caption:='Show pixels'
  else
    ShowPixel.caption:='Not show';
end;

procedure TFormNeo1.Show_pixelsClick(Sender: TObject);
begin
  neopixel1.Show;
end;

procedure TFormNeo1.shift_leftClick(Sender: TObject);
begin
  //  shif_type    bit0 = 1 right 0=left, bit1 = 1 wrap, bit2 = 1 do_show
  NeoPixel1.ShiftRun(NeoPixel1.ShiftType and $FE);  // shift left
end;

function TFormNeo1.Board1DeviceDataAvailable(sender: TObject): Boolean;
begin
  Result:=client.CanReadEx(5);
end;

function TFormNeo1.Board1GetDataFromDevice(sender: TObject): string;
begin
  Result:=client.RecvPacket(0);
end;

procedure TFormNeo1.Board1SendDataToDevice(sender: TObject; str: string);
begin
  client.SendString(str);
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

