//
//
//
{ TPS2Mouse }
//
//
//
constructor TPS2Mouse.Create(AOwner: TComponent);
begin
  inherited;

  FBoard:= nil;
  FEnabled:=false;

  FOnEnabled:=nil;
  FOnDisabled:=nil;

  FOnMouseData:=nil;
  FOnMouseStatus:=nil;
  FOnMouseDeviceID:=nil;

  FClockPin:=PinModesToByte(PIN_MODE_IGNORE);
  FDataPin:=FClockPin;
  FDevice:=FClockPin; // PIN_MODE_IGNORE means no device
  FMode:=PS2MOUSE_REMOTE;
  FMouseType:=STILL_UNKNOWN;
  FResolution:=PS2MOUSE_8_COUNT_MM;
  FSampleRate:=R_60;
  FScaling:=PS2MOUSE_SCALING_1_TO_1;
  FillChar(FStatus, SizeOf(FStatus), 0);  // initilize record
  FillChar(FMouseData, SizeOf(FMouseData), 0);  // initilize record
end;

destructor TPS2Mouse.Destroy();
begin
  inherited Destroy;
end;

procedure TPS2Mouse.setBoard(Board: TBoard);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'setBoard')
  else if Assigned(Board) then
    FBoard:=Board;
end;

procedure TPS2Mouse.setClockPin(Pin: Byte);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'setClockPin')
  else if Pin < PinModesToByte(PIN_MODE_IGNORE) then
    FClockPin:=Pin;
end;

procedure TPS2Mouse.setDataPin(Pin: Byte);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'setDataPin')
  else if Pin < PinModesToByte(PIN_MODE_IGNORE) then
    FDataPin:=Pin;
end;

procedure TPS2Mouse.setReporting(Enable: Boolean);
begin
   if FReporting = Enable then
     exit;
   FReporting:=Enable;
   if FEnabled then
     SendReporting;
end;

procedure TPS2Mouse.setResolution(Resolution: TMouseResolution);
begin
   if FResolution = Resolution then
     exit;
   FResolution:=Resolution;
   if FEnabled then
     SendResolution;
end;

procedure TPS2Mouse.setSampleRate(SampleRate: TMouseSampleRate);
begin
   if FSampleRate = SampleRate then
     exit;
   FSampleRate:=SampleRate;
   if FEnabled then
     SendSampleRate;
end;

procedure TPS2Mouse.setEnabled(State: Boolean);
var
  i: integer;
begin
  if not Assigned(FBoard) then
    exit;

  if FEnabled = State then
     exit;

  if State then
  begin
    FEnabled:=True;
    if Assigned(FBoard) and FBoard.Enabled then
    begin
      // Check if supported pin
      if FBoard.CheckCapability(FClockPin, PIN_MODE_PS2MOUSE) and
         FBoard.CheckCapability(FDataPin, PIN_MODE_PS2MOUSE) then
      begin
        if FBoard.FBoardPins[FClockPin].Busy or FBoard.FBoardPins[FDataPin].Busy then
        begin
          FEnabled:=false;
          FBoard.RaiseError(12, 'setEnabled');
        end
        else  // pin is free
        begin
          for i:=0 to MAX_MICE - 1 do  // looking for a free mice
          begin
            if not Assigned(FBoard.FMice[i]) then // found one free
            begin
              FDevice:=i;
              break;
            end;
          end;
        end;
      end
      else // No supported pin
      begin
        FEnabled:=false;
        FBoard.RaiseError(11, 'setEnabled');
      end;
    end
    else
    begin
      FEnabled:=false; // firmataboard not enabled
      FBoard.RaiseError(36, 'setEnabled');
    end;
    if FDevice = PinModesToByte(PIN_MODE_IGNORE) then // There isn't a free mouse
    begin
      FEnabled:=False;
      FBoard.RaiseError(44, 'setEnabled');
    end;
    if FEnabled then
    begin
      FMouseType:=STILL_UNKNOWN;
      FBoard.FMice[FDevice]:=self;
      FBoard.FBoardPins[FClockPin].Busy:=true;  //  pin is assigned to this module
      FBoard.FBoardPins[FDataPin].Busy:=true;  //  pin is assigned to this module
      config;  // configure mouse
      // After mouse config, mouse is set to SampleRate:=R_60, Mode:=PS2MOUSE_REMOTE, MouseType:=STILL_UNKNOWN,
      //                     Resolution:=PS2MOUSE_8_COUNT_MM, SampleRate:=R_60, Scaling:=SCALING_1_TO_1
      if Assigned(FOnEnabled) then
        FOnEnabled(self);
    end;
  end
  else  // disable
  begin
    if Assigned(FOnDisabled) then
      FOnDisabled(self);
    if Assigned(FBoard) then
    begin
      FBoard.FBoardPins[FClockPin].Busy:=false; // free clock pin
      FBoard.FBoardPins[FDataPin].Busy:=false; // free data pin
      FBoard.FMice[FDevice]:=nil;
    end;
    FEnabled:=False;
  end;
end;

procedure TPS2Mouse.GetFirmataCommand(Sender: TObject; CommandData: String);
begin
  // first byte is subcommand
  // second byte is Mouse number
  case ord(CommandData[1]) of
       {0  START_SYSEX      (0xF0)
        1  PS2MOUSE_DATA     Command (0x50)
        2  PS2MOUSE_STATUS   (0x01)
        3  Mouse number      (0-2)
        4  status            (lsb)
        5  status            (msb)
        6 Resolution        (lsb)
        7 Resolution        (msb)
        8 SampleRate        (lsb)
        9 SampleRate        (msb)
        10 END_SYSEX        (0xF7)}
       { TStatusMouseBits = bitpacked record
            always_0: 0;
            Mode: 0..1; // 1 remote, 0 stream
            Reporting: Boolean;
            Scaling: TMouseScaling;
            always_0_0: 0:
            mid_button: Boolean;
            right_button: Boolean;
            lef_button: Boolean;
        end;}
    PS2MOUSE_STATUS: begin
      FMouseStatus.StatusMouse:=ord(CommandData[3]) or (ord(CommandData[4]) << 7);
      FMouseStatus.Resolution:=ord(CommandData[5]) or (ord(CommandData[6]) << 7);
      FMouseStatus.SampleRate:=ord(CommandData[7]) or (ord(CommandData[8]) << 7);
      if Assigned(FOnMouseStatus) then
        FOnMouseStatus(self, FMouseStatus);
    end;
    {0  START_SYSEX      (0xF0)
     1  PS2MOUSE_DATA     Command (0x50)
     2  PS2MOUSE_DATA   (0x50)
     3  Mouse number      (0-2)
     4  Mouse Type       (0-127)    3 = intelli_mouse
     5 END_SYSEX        (0xF7)}
    PS2MOUSE_DEVICEID: begin
      // first byte subcommand
      // second byte device
      // third byte type
      FMouseType:=TMouseType(ord(CommandData[3]));
      if Assigned(FOnMouseDeviceID) then
        FOnMouseDeviceID(self, FMouseType);
    end;
    {0  START_SYSEX      (0xF0)
     1  PS2MOUSE_DATA     Command (0x50)
     2  PS2MOUSE_DATA   (0x50)
     3  Mouse number      (0-2)
     4  status            (lsb)
     5  status            (msb)
     6  x position        (lsb)
     7  x position        (msb)
     8  y position        (lsb)
     9  y position        (msb)
     10 wheel             (lsb)
     11 wheel             (msb)
     12 END_SYSEX        (0xF7)}
     { TWheelBits = bitpacked record
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
       lef_button: Boolean;
       end;       }
    PS2MOUSE_DATA: begin
      // first byte subcommand
      // second byte device
      FMouseData.StatusMove:=ord(CommandData[3]) or (ord(CommandData[4]) << 7);
      FMouseData.Position.x:=ord(CommandData[5]) or (ord(CommandData[6]) << 7);
      FMouseData.Position.y:=ord(CommandData[7]) or (ord(CommandData[8]) << 7);
      FMouseData.Wheel:=ord(CommandData[9]) or (ord(CommandData[10]) << 7);
      if Assigned(FOnMouseData) then
        FOnMouseData(self, MouseData);
    end;
  end;
end;
{ Send START_SYSEX + data + END_SYSEX total <= MAX_DATA_BYTES (64 bytes)}
function TPS2Mouse.SendSysEx(data7bit: string; write: Boolean=true): string;
begin
  Result:='';
  if not FEnabled and write then
    exit;

  Result:=FBoard.SendSysEx(Data7bit, write);
end;
//
// MOUSE commands
//

{0  START_SYSEX      (0xF0)
1  PS2MOUSE_DATA     Command (0x50)
2  PS2MOUSE_CONFIG   (0x02)
3  mouse number      (0-3)
3  ClockPin          (0-127)
4  DataPin           (0-127)
5  Mode              (optional) REMOTE 1, STREAM 0
5/6  END_SYSEX (0xF7)}
function TPS2Mouse.config(write: Boolean=true): string;
begin
  // FMode default to remote mode
  Result:=SendSysEx(chr(PS2MOUSE_DATA)+chr(PS2MOUSE_CONFIG)+chr(FDevice)+chr(FClockPin)+chr(FDataPin)+chr(ord(FMode)), write);
  if write then
  begin
    FBoard.FBoardPins[FClockPin].ActualMode:=PinModesToByte(PIN_MODE_PS2MOUSE);
    FBoard.FBoardPins[FDataPin].ActualMode:=PinModesToByte(PIN_MODE_PS2MOUSE);
  end;
end;

{0  START_SYSEX      (0xF0)
 1  PS2MOUSE_DATA     Command (0x50)
 2  PS2MOUSE_STATUS   (0x01)
 3  Mouse number      (0-2)
 4  END_SYSEX        (0xF7)}
function TPS2Mouse.QueryStatus(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(PS2MOUSE_DATA)+chr(PS2MOUSE_STATUS)+chr(FDevice), write);
end;

{0  START_SYSEX      (0xF0)
 1  PS2MOUSE_DATA     Command (0x50)
 2  PS2MOUSE_DEVICEID   (0x03)
 3  Mouse number      (0-2)
 4  END_SYSEX        (0xF7)}
function TPS2Mouse.QueryDeviceID(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(PS2MOUSE_DATA)+chr(PS2MOUSE_DEVICEID)+chr(FDevice), write);
end;
{0  START_SYSEX      (0xF0)
 1  PS2MOUSE_DATA     Command (0x50)
 2  PS2MOUSE_DATA    Command (0x50)
 3  Mouse number      (0-2)
 4  END_SYSEX        (0xF7)}
function TPS2Mouse.QueryData(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(PS2MOUSE_DATA)+chr(PS2MOUSE_DATA)+chr(FDevice), write);
end;
{0  START_SYSEX      (0xF0)
 1  PS2MOUSE_DATA     Command (0x50)
 2  PS2MOUSE_RESET    Command (0x00)
 3  Mouse number      (0-2)
 4  END_SYSEX        (0xF7)}
function TPS2Mouse.Reset(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(PS2MOUSE_DATA)+chr(PS2MOUSE_RESET)+chr(FDevice), write);
end;
{0  START_SYSEX      (0xF0)
 1  PS2MOUSE_DATA     Command (0x50)
 2  PS2MOUSE_SET_SAMPLE_RATE    Command (0x05)
 3  Mouse number      (0-2)
 4  sample rate        lsb
 5  sample rate        msb
 6  END_SYSEX        (0xF7)}
function TPS2Mouse.SendSampleRate(write: Boolean=true): string;
var
  SampleRate: Byte;
begin
  case FSampleRate of
    R_10, R_20: SampleRate:=(ord(FSampleRate)+1)*10;
    R_40, R_60, R_80, R_100: SampleRate:=(ord(FSampleRate))*20;
    R_200: SampleRate:=200;
  end;
  Result:=SendSysEx(chr(PS2MOUSE_DATA)+chr(PS2MOUSE_SET_SAMPLE_RATE)+chr(FDevice)+chr(SampleRate and $7F)+chr((SampleRate >> 7) and $7F), write);
end;
{0  START_SYSEX      (0xF0)
 1  PS2MOUSE_DATA     Command (0x50)
 2  PS2MOUSE_SET_RESOLUTION     Command (0x04)
 3  Mouse number      (0-2)
 4  resolution        (0-3)
 5  END_SYSEX        (0xF7)}
function TPS2Mouse.SendResolution(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(PS2MOUSE_DATA)+chr(PS2MOUSE_SET_RESOLUTION)+chr(FDevice)+chr(ord(FResolution)), write);
end;
{0  START_SYSEX      (0xF0)
 1  PS2MOUSE_DATA     Command (0x50)
 2  PS2MOUSE_REPORTING  Command (0x06)
 3  Mouse number      (0-2)
 4  Reporting         (0-1) 1 enable, 0 disable, it is firmata reporting not mouse reporting
 5  END_SYSEX        (0xF7)}
function TPS2Mouse.SendReporting(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(PS2MOUSE_DATA)+chr(PS2MOUSE_REPORTING)+chr(FDevice)+chr(ord(FReporting)), write);
end;
{0  START_SYSEX      (0xF0)
 1  PS2MOUSE_DATA     Command (0x50)
 2  PS2MOUSE_SET_FIVE_BUTTONS  Command (0x07)
 3  Mouse number      (0-2)
 5  END_SYSEX        (0xF7)}
function TPS2Mouse.SendFiveButtonsMode(write: Boolean=true): string;
begin
  Result:='';
  if write and (FMouseType <> INTELLI_MOUSE) then
    exit;
  Result:=SendSysEx(chr(PS2MOUSE_DATA)+chr(PS2MOUSE_SET_FIVE_BUTTONS)+chr(FDevice), write);
end;
{0  START_SYSEX      (0xF0)
 1  PS2MOUSE_DATA     Command (0x50)
 2  PS2MOUSE_SET_REMOTE_MODE   Command (0x09)
 3  Mouse number      (0-2)
 5  END_SYSEX        (0xF7)}
function TPS2Mouse.SendRemoteMode(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(PS2MOUSE_DATA)+chr(PS2MOUSE_SET_REMOTE_MODE)+chr(FDevice), write);
  if write then
    FMode:=PS2MOUSE_REMOTE;
end;
//
//
//
{ TNeoPixel }
//
//
//
constructor TNeoPixel.Create(AOwner: TComponent);
begin
  inherited;

  FBoard:= nil;
  FEnabled:=false;

  FOnFadeEnd:=nil;
  FOnEnabled:=nil;
  FOnDisabled:=nil;

  FPin:=PinModesToByte(PIN_MODE_IGNORE);
  FDevice:=FPin; // PIN_MODE_IGNORE means no device
  FLedColors:='GRB';
  FPixels:=nil;
  FPixelsNumber:=1;
  FGamma:=1;
  FFadeRunning:=false;
  FFadeColor:=0;
  FFadeFirst:=0;
  FFadeLast:=0;
  FShiftFirst:=0;
  FShiftLast:=0;
  FShiftType:=7; // bit0 = 1 right = 0 left, bit1 = 1 wrap,
                  //  bit2 = 1 do_show
  FFadeLoopsWait:=1;  // if SamplingRate = 19, 1 is about 25 Hz eye speed
  FBrightness:=0;  // 0=mAX, 1=Min, 255= 1 below max
  FKHZ400:=false;
end;

destructor TNeoPixel.Destroy();
begin
  inherited Destroy;
end;

procedure TNeoPixel.setBoard(Board: TBoard);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'setBoard')
  else if Assigned(Board) then
    FBoard:=Board;
end;

procedure TNeoPixel.setPin(Pin: Byte);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'setPin')
  else if Pin < PinModesToByte(PIN_MODE_IGNORE) then
    FPin:=Pin;
end;

function TNeoPixel.getPixel(Index: integer): TPixel;
begin
  if (Index < Length(FPixels)) and (Index >= 0) then
    Result:=FPixels[Index]
  else
    Result:=default(TPixel);
end;

procedure TNeoPixel.setLedColors(rgbwType: string);  // 0bRRRRGGBB for RGB + NEO_KHZ800,  0bWWRRGGBB for RGBW devices
var
  ConR, ConG, ConB, ConW: byte;
  function CountsChar(const S: string; const C: char): integer;
  var
    i: Integer;
  begin
    result := 0;
    for i := 1 to Length(S) do
      if S[i] = C then
        inc(result);
  end;
begin
  ConR:=CountsChar(UpperCase(rgbwType), 'R');
  ConG:=CountsChar(UpperCase(rgbwType), 'G');
  ConB:=CountsChar(UpperCase(rgbwType), 'B');
  ConW:=CountsChar(UpperCase(rgbwType), 'W');

  if (ConR <> 1) or (ConG <> 1) or (ConB <> 1) or (ConW > 1) or (Length(rgbwType) > 4) then
  begin
    FBoard.RaiseError(48, 'setLedType');
    FLedColors:='';
    exit;
  end;
  FLedColors:=UpperCase(rgbwType);
end;

procedure TNeoPixel.setGamma(Value: single);
begin
  if Value < 0 then
  begin
    FBoard.RaiseError(52, 'setGamma');
    FGamma:=1;
    exit;
  end;
  FGamma:=Value;
end;

procedure TNeoPixel.setPixelsNumber(Number: smallint);
begin
  if (Number < 0) or (Number > 511) then
  begin
    FPixelsNumber:=0;
    FBoard.RaiseError(20, 'setPixelsNumber');
    exit;
  end;
  FPixelsNumber:=Number;
end;

procedure TNeoPixel.setBrightness(value: Byte);
begin
  if Value = FBrightness then
    exit;
  FBrightness:=Value;
  if FEnabled then
    sendBrightness(True);
end;

procedure TNeoPixel.setFadeRunning(Enabled: Boolean);
begin
  if Enabled = FFadeRunning then // fade is not changed
    exit;
  if FEnabled then
  begin
    FFadeRunning:=Enabled;
    FadeRunPause;
  end;
end;

procedure TNeoPixel.setFadeLoopsWait(Loops: byte);
begin
  if Loops = FFadeLoopsWait then
    exit;
  if Loops > 15 then
  begin
    FBoard.RaiseError(42, 'setFadeLoopsWait');
    exit;
  end;
  FFadeLoopsWait:=Loops;
end;

procedure TNeoPixel.setEnabled(State: Boolean);
var
  i: integer;
begin
  if not Assigned(FBoard) then
    exit;

  if FEnabled = State then
     exit;

  if State then
  begin
    FEnabled:=True;
    if Assigned(FBoard) and FBoard.Enabled then
    begin
      // Check if supported pin
      if FBoard.CheckCapability(FPin, PIN_MODE_NEOPIXELS) then
      begin
        if FBoard.FBoardPins[FPin].Busy then
        begin
          FEnabled:=false;
          FBoard.RaiseError(12, 'setEnabled');
        end
        else  // pin is free
        begin
          if FPixelsNumber < 1 then
          begin
           FEnabled:=false;
           FBoard.RaiseError(49, 'setEnabled');
          end
          else if Length(FLedColors) < 1 then
          begin
            FEnabled:=false;
            FBoard.RaiseError(51, 'setEnabled');
          end
          else
          begin
            for i:=0 to MAX_NEOPIXELS - 1 do  // looking for a free neopixel
            begin
              if not Assigned(FBoard.FNeoPixels[i]) then // found one free
              begin
                FDevice:=i;
                break;
              end;
            end;
          end;
        end;
      end
      else // No supported pin
      begin
        FEnabled:=false;
        FBoard.RaiseError(11, 'setEnabled');
      end;
    end
    else  // firmata board not enabled
    begin
      FEnabled:=false; // firmataboard not enabled
      FBoard.RaiseError(36, 'setEnabled');
    end;
    if FDevice = PinModesToByte(PIN_MODE_IGNORE) then // There isn't a free mouse
    begin
      FEnabled:=False;
      FBoard.RaiseError(44, 'setEnabled');
    end;
    if FEnabled then
    begin
      // Fill array of pixels
      setLength(FPixels, FPixelsNumber);
      for i:=0 to FPixelsNumber - 1 do
        FPixels[i].PixelColor:=0;
      FBoard.FNeoPixels[FDevice]:=self;
      FBoard.FBoardPins[FPin].Busy:=true;  //  pin is assigned to this module
      config;  // configure
      if FFadeLoopsWait <> 1 then
        FadeSegmentConfig(FFadeColor, FFadeFirst, FFadeLast);
      if Assigned(FOnEnabled) then
        FOnEnabled(self);
    end;
  end
  else  // disable
  begin
    if Assigned(FOnDisabled) then
      FOnDisabled(self);
    if Assigned(FBoard) then
    begin
      FBoard.FBoardPins[FPin].Busy:=false; // free pin
      FBoard.FNeoPixels[FDevice]:=nil;
    end;
    FEnabled:=False;
  end;
end;

procedure TNeoPixel.GetFirmataCommand(Sender: TObject);
var
  i: smallint;
begin
  FFadeRunning:=False;  // update fade status

  for i:=FFadeFirst to FFadeLast do
    FPixels[i].PixelColor:=FFadeColor;   // update pixels

  if Assigned(FOnFadeEnd) then
    FOnFadeEnd(self);
end;

{ Send START_SYSEX + data + END_SYSEX total <= MAX_DATA_BYTES (64 bytes)}
function TNeoPixel.SendSysEx(data7bit: string; write: Boolean=true): string;
begin
  Result:='';
  if not FEnabled and write then
    exit;

  Result:=FBoard.SendSysEx(Data7bit, write);
end;
//
// NEOPIXELS commands
//

{0  START_SYSEX      (0xF0)
1  NEOPIXELS_DATA    (0X51)
2  NEOPIXELS_CONFIG  (0x01)
3  device            (0-3)
4  Pin               (0-127)
5  NumberLeds        lsb(14 bits max)
6                    msb
7  LedType           lsb (9 bits) //bit 8 = KHZ400, bits 7-0 'RGBW' position;
8                    msb
9  END_SYSEX (0xF7)}
function TNeoPixel.config(write: Boolean=true): string;
var
  TmpLedColors: smallint;
  PosR, PosG, PosB, PosW: byte;
begin
  Result:='';
  if length(FLedColors) < 1 then
    exit;

  PosR:=Pos('R', FLedColors)-1;
  PosG:=Pos('G', FLedColors)-1;
  PosB:=Pos('B', FLedColors)-1;
  if Length(FLedColors) = 3 then  // is 400 KHZ
    PosW:=PosR
  else
    PosW:=Pos('W', FLedColors)-1;

  TmpLedColors:=(PosW << 6) or (PosR << 4) or (PosG << 2) or PosB;
  if FKHZ400 then
    TmpLedColors:=$100 or TmpLedColors;

  Result:=SendSysEx(chr(NEOPIXELS_DATA)+chr(NEOPIXELS_CONFIG)+chr(FDevice)+chr(FPin)+chr(FPixelsNumber and $7F)
                  +chr((FPixelsNumber >> 7) and $7F)+chr(TmpLedColors and $7F)+chr(TmpLedColors >> 7), write);

  if write then
  begin
    if FBrightness <> 0 then // update brighness
      sendBrightness(false); // do not show
    // defaul not fade, not wrap, right, not show, loops to wait = 1
    FFadeRunning:=false;
    FShiftType:=$07;  // defaul not fade, wrap, right, show

    FBoard.FBoardPins[FPin].ActualMode:=PinModesToByte(PIN_MODE_NEOPIXELS);
    FFadeFirst:=0;
    FFadeLast:=FPixelsNumber -1;
    FFadeColor:=0;
    FShiftfirst:=0;
    FShiftlast:=FPixelsNumber -1;
  end;
end;

{0  START_SYSEX      (0xF0)
 1  NEOPIXELS_DATA    (0X51)
 2  NEOPIXELS_OFF     (0x00)
 3  device            (0-3)
 4  END_SYSEX        (0xF7)}
function TNeoPixel.Off(write: Boolean=true): string;
begin
  Result:='';

  if FFadeRunning then // fade is running
  begin
    FBoard.RaiseError(53, 'Off');
    exit;
  end;
  Result:=SendSysEx(chr(NEOPIXELS_DATA)+chr(NEOPIXELS_OFF)+chr(FDevice), write);
end;

{0  START_SYSEX      (0xF0)
 1  NEOPIXELS_DATA    (0X51)
 2  NEOPIXELS_SHOW    (0x02)
 3  device            (0-3)
 4  END_SYSEX        (0xF7)}
function TNeoPixel.Show(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(NEOPIXELS_DATA)+chr(NEOPIXELS_SHOW)+chr(FDevice), write);
end;
{0  START_SYSEX      (0xF0)
 1  NEOPIXELS_DATA    (0X51)
 2  NEOPIXELS_SET_PIXEL  (0x03)
 3  device            (0-3)
 4  Pixel             lsb
 5                    msb
 6  colorRGBW         b = bits 7-0
 7   encoded 7 bits   g = bits 15-8
 8   5 bytes          r = bits 23-16
 9                    w = bits 31-24 of color, send white even not used
 10                   do_show bit32, =1 show, bit32 is bit4 of 5th byte
 11  END_SYSEX        (0xF7)}
function TNeoPixel.PixelColor(Pixel: smallint; Color: longword; do_show: boolean; write: Boolean=true): string;
var
  tmpColor: longword;
  Color7Bits: string;
begin
  Result:='';

  if FFadeRunning then // fade is running
  begin
    FBoard.RaiseError(53, 'ColorRGBW');
    exit;
  end;
  if FGamma <> 1 then
    tmpColor:=gammaRGB(Color, FGamma)
  else
    tmpColor:=Color;

  Color7Bits:=encode32BitUnSignedInt(tmpColor);
  if do_show then
    Color7Bits[5]:=chr(ord(Color7Bits[5]) or $10); // bit4
  Result:=SendSysEx(chr(NEOPIXELS_DATA)+chr(NEOPIXELS_SET_PIXEL)+chr(FDevice)+
                   chr(Pixel and $7F)+chr((Pixel >> 7) and $7F)+Color7Bits, write);
  if write then
    FPixels[Pixel].PixelColor:=tmpColor;
end;
function TNeoPixel.PixelColor(Pixel: smallint; Red: byte; Green: byte; Blue: Byte; White: byte; do_show: boolean; write: Boolean=true): string; overload;
begin
  Result:=PixelColor(Pixel, Blue or (integer(Green) << 8) or (integer(Red) << 16) or (integer(White) << 24), do_show, write);
end;
function TNeoPixel.PixelColor(Pixel: smallint; Color: longword; write: Boolean=true): string; overload;
begin
  Result:=PixelColor(Pixel, Color, false, write);
end;
function TNeoPixel.PixelColor(Pixel: smallint; Red: byte; Green: byte; Blue: Byte; White: byte; write: Boolean=true): string; overload;
begin
  Result:=PixelColor(Pixel, Blue or (integer(Green) << 8) or (integer(Red) << 16) or (integer(White) << 24), false, write);
end;

{0  START_SYSEX      (0xF0)
 1  NEOPIXELS_DATA    (0X51)
 2  NEOPIXELS_FILL_SEGMENT    (0x07)
 3  device            (0-3)
 4  colorRGBW         b = bits 7-0
 5   encoded 7 bits   g = bits 15-8
 6  5 bytes           r = bits 23-16
 7                    w = bits 31-24 of color, send white even not used
 8                    do_show bit32, =1 show, bit32 is bit4 of 5th byte
 9  First pixel       lsb   // optional, default set to 0
 10                   msb
 11  last pixel       lsb   // optional, set to last pixel strip
 12                   msb
 10..13  END_SYSEX     (0xF7)}
function TNeoPixel.FillSegment(Color: longword; First: smallint; Last: smallint; do_show: Boolean; write: Boolean=true): string;
var
   i: integer;
   tmpColor: longword;
   Color7Bits: string;
begin
  Result:='';

  if FFadeRunning then // fade is running
  begin
    FBoard.RaiseError(53, 'Fill');
    exit;
  end;
  if First >= FPixelsNumber then
  begin
    FBoard.RaiseError(50, 'Fill');
    exit;
  end;
  if Last >= FPixelsNumber then
  begin
    FBoard.RaiseError(50, 'Fill');
    exit;
  end;

  if FGamma <> 1 then
    tmpColor:=gammaRGB(Color, FGamma)
  else
    tmpColor:=Color;

  Color7Bits:=encode32BitUnSignedInt(tmpColor);
  if do_show then
    Color7Bits[5]:=chr(ord(Color7Bits[5]) or $10); // bit4

  Result:=SendSysEx(chr(NEOPIXELS_DATA)+chr(NEOPIXELS_FILL_SEGMENT)+chr(FDevice)+
                    Color7Bits+chr(First and $7F)+chr((First >> 7) and $7F)+
                    chr(Last and $7F)+chr((Last >> 7) and $7F), write);

  if write then
  begin
    for i:=First to Last do
      FPixels[i].PixelColor:=tmpColor;
  end;
end;
function TNeoPixel.FillSegment(Red: byte; Green: byte; Blue: Byte; White: byte; first: smallint; last: smallint; do_show: Boolean; write: Boolean=true): string; overload;
begin
  Result:=FillSegment(Blue or (integer(Green) << 8) or (integer(Red) << 16) or (integer(White) << 24), first, last, do_show, write);
end;
{0  START_SYSEX      (0xF0)
 1  NEOPIXELS_DATA    (0X51)
 2  NEOPIXELS_FILL_SEGMENT   (0x07)
 3  device            (0-3)
 4  colorRGBW         b = bits 7-0
 5   encoded 7 bits   g = bits 15-8
 6  5 bytes           r = bits 23-16
 7                    w = bits 31-24 of color, send white even not used
 8                    do_show bit32, =1 show, bit32 is bit4 of 5th byte
 9  END_SYSEX     (0xF7)}
function TNeoPixel.FillStrip(Color: longword; do_show: Boolean; write: Boolean=true): string;
var
   i: smallint;
   tmpColor: longword;
   Color7Bits: string;
begin
  if FGamma <> 1 then
    tmpColor:=gammaRGB(Color, FGamma)
  else
    tmpColor:=Color;

  Color7Bits:=encode32BitUnSignedInt(tmpColor);
  if do_show then
    Color7Bits[5]:=chr(ord(Color7Bits[5]) or $10); // bit4

  Result:=SendSysEx(chr(NEOPIXELS_DATA)+chr(NEOPIXELS_FILL_SEGMENT)+chr(FDevice)+Color7Bits, write);

  if write then
    for i:=0 to FPixelsNumber - 1 do
      FPixels[i].PixelColor:=tmpColor;
end;
function TNeoPixel.FillStrip(Red: byte; Green: byte; Blue: Byte; White: byte; do_show: Boolean; write: Boolean=true): string; overload;
begin
  Result:=FillStrip(Blue or (integer(Green) << 8) or (integer(Red) << 16) or (integer(White) << 24), do_show, write);
end;

{0  START_SYSEX      (0xF0)
1  NEOPIXELS_DATA    (0X51)
2  NEOPIXELS_SHIFT_CONFIG   (0x06)
3  device            (0-3)
4  First pixel       lsb  Optional
5                    msb
6  Last pixel        lsb  optional
7                    msb
8  shif_type         optional bit0 = 1 right, bit1 = 1 wrap, = 0 left
                     bit2 = 1 do_show
4/9  END_SYSEX        (0xF7)}
function TNeoPixel.ShiftSegmentConfig(First: smallint; Last: smallint; Direction: char; Wrap: Boolean; do_show: Boolean; write: Boolean=true): string;
begin
  Result:='';

  if First >= FPixelsNumber then
  begin
    FBoard.RaiseError(50, 'Shift');
    exit;
  end;
  if Last >= FPixelsNumber then
  begin
    FBoard.RaiseError(50, 'Shift');
    exit;
  end;
    // right = bit0 1=right, 0=left
    // wrap =  bit1 1=wrap
    // do_show = bit2 1=show
  FShiftType:=ord('R' = UpperCase(Direction)) or (ord(Wrap) << 1) or (ord(do_show) << 2);

  Result:=SendSysEx(chr(NEOPIXELS_DATA)+chr(NEOPIXELS_SHIFT_CONFIG)+chr(FDevice)+
                    chr(First and $7F)+chr((First >> 7) and $7F)+
                    chr(Last and $7F)+chr((Last >> 7) and $7F)+chr(FShiftType), write);

  if write then
  begin
    FShiftFirst:=First;
    FShiftLast:=Last;
  end;
end;
{0  START_SYSEX      (0xF0)
1  NEOPIXELS_DATA    (0X51)
2  NEOPIXELS_SHIFT_CONFIG  (0x06)
3  device            (0-3)
4  shif_type         bit0 = 1 right 0=left, bit1 = 1 wrap,
                     bit2 = 1 do_show
5  END_SYSEX        (0xF7)}
// shift strip
function TNeoPixel.ShiftStripConfig(Direction: char; Wrap: Boolean; do_show: Boolean; write: Boolean=true): string; overload;
begin
  Result:='';

  // direction = bit0 1=right, 0=left
  // wrap =  bit1 1=wrap
  // do_show = bit2 1=show
  FShiftType:=ord('R' = UpperCase(Direction)) or (ord(Wrap) << 1) or (ord(do_show) << 2);

  Result:=SendSysEx(chr(NEOPIXELS_DATA)+chr(NEOPIXELS_SHIFT_CONFIG)+chr(FDevice)+chr(FShiftType), write);

  if write then
  begin
    FShiftFirst:=0;
    FShiftLast:=FPixelsNumber - 1;
  end;
end;

{0  START_SYSEX      (0xF0)
1  NEOPIXELS_DATA    (0X51)
2  NEOPIXELS_SHIFT_RUN (0x09)
3  device            (0-3)
4  END_SYSEX        (0xF7)}
function TNeoPixel.ShiftRun(write: Boolean=True): string;
var
  savedColor: longword;
  i: smallint;
begin
  Result:=SendSysEx(chr(NEOPIXELS_DATA)+chr(NEOPIXELS_SHIFT_RUN)+chr(FDevice), write);

  if write then
  begin
    if boolean(FShiftType and 1) then  // shift right
    begin   // shift right
      savedColor:=FPixels[FShiftLast].PixelColor;
      for i:=FShiftLast downto 1 do
        FPixels[i].PixelColor:=FPixels[i-1].PixelColor;
      if boolean(FShiftType and 2) then  // wrap on
        FPixels[0].PixelColor:=savedColor;
    end
    else  // shift left
    begin
      savedColor:=FPixels[0].PixelColor;
      for i:=0 to FShiftLast - 1 do
        FPixels[i].PixelColor:=FPixels[i+1].PixelColor;
      if boolean(FShiftType and 2) then  // wrap on
        FPixels[FShiftLast].PixelColor:=savedColor;
    end;
  end;
end;

{0  START_SYSEX      (0xF0)
1  NEOPIXELS_DATA    (0X51)
2  NEOPIXELS_SHIFT_RUN (0x09)
3  device            (0-3)
4  shif_type         bit0 = 1 right 0=left, bit1 = 1 wrap,
                     bit2 = 1 do_show
4/5  END_SYSEX        (0xF7)}
function TNeoPixel.ShiftRun(ShiftType: Byte; write: Boolean=True): string; overload;
var
  savedColor: longword;
  i: smallint;
begin
  Result:=SendSysEx(chr(NEOPIXELS_DATA)+chr(NEOPIXELS_SHIFT_RUN)+chr(FDevice)+chr(ShiftType), write);

  if write then
  begin
    FShiftType:=ShiftType;
    if boolean(FShiftType and 1) then
    begin   // shift right
      savedColor:=FPixels[FShiftLast].PixelColor;
      for i:=FShiftLast downto FShiftLast do
        FPixels[i].PixelColor:=FPixels[i-1].PixelColor;
      if boolean(FShiftType and 2) then  // wrap on
        FPixels[0].PixelColor:=savedColor;
    end
    else  // shift left
    begin
      savedColor:=FPixels[FShiftfirst].PixelColor;
      for i:=FShiftFirst to FShiftLast - 1 do
        FPixels[i].PixelColor:=FPixels[i+1].PixelColor;
      if boolean(FShiftType and 2) then // wrap on
        FPixels[FShiftLast].PixelColor:=savedColor;
    end;
  end;
end;
{0  START_SYSEX      (0xF0)
1  NEOPIXELS_DATA    (0X51)
2  NEOPIXELS_FADE_CONFIG (0x08)
3  device            (0-3)
4  colorRGBW         b = bits 7-0
5   encoded 7 bits   g = bits 15-8
6   5 bytes          r = bits 23-16
7                    w = bits 31-24 of color, send white even not used
8
9  First pixel       lsb
10                   msb
11 Last pixel        lsb
12                   msb
13 loops wait        optional (0-15) // default to 1, if SamplingRate is 19, 1 is about 25 hz eye speed
13/14  END_SYSEX        (0xF7)}
function TNeoPixel.FadeSegmentConfig(color: longword; first: smallint; Last: smallint; write: Boolean=True): string;
var
  tmpColor: longword;
  Loopsdata: string;
begin
  Result:='';
  if FFadeRunning then // fade is running
  begin
    FBoard.RaiseError(53, 'FadeSegmentConfig');
    exit;
  end;
  if FGamma <> 1 then
    tmpColor:=gammaRGB(Color, FGamma)
  else
    tmpColor:=Color;

  if First >= FPixelsNumber then
  begin
    FBoard.RaiseError(50, 'FadeSegmentConfig');
    exit;
  end;
  if Last >= FPixelsNumber then
  begin
    FBoard.RaiseError(50, 'FadeSegmentConfig');
    exit;
  end;

  if FFadeLoopsWait <> 1 then
    LoopsData:=chr(FFadeLoopsWait)
  else
    LoopsData:='';

  Result:=SendSysEx(chr(NEOPIXELS_DATA)+chr(NEOPIXELS_FADE_CONFIG)+chr(FDevice)+
                    encode32BitUnSignedInt(tmpColor)+
                    chr(First and $7F)+chr((First >> 7) and $7F)+
                    chr(Last and $7F)+chr((Last >> 7) and $7F)+LoopsData, write);
  if write then
  begin
    FFadeColor:=tmpColor;
    FFadeFirst:=First;
    FFadeLast:=Last;
  end;
end;
function TNeoPixel.FadeSegmentConfig(Red: byte; Green: byte; Blue: Byte; White: byte; first: smallint; Last: smallint; write: Boolean=True): string; overload;
begin
  Result:= FadeSegmentConfig(Blue or (integer(Green) << 8) or (integer(Red) << 16) or (integer(White) << 24), first, Last, write);
end;
{0  START_SYSEX      (0xF0)
1  NEOPIXELS_DATA    (0X51)
2  NEOPIXELS_FADE_CONFIG (0x08)
3  device            (0-3)
4  colorRGBW         b = bits 7-0
5   encoded 7 bits   g = bits 15-8
6   5 bytes          r = bits 23-16
7                    w = bits 31-24 of color, send white even not used
8
9 loops wait        optional (0-15) // default to 1, if SamplingRate is 19, 1 is about 25 hz eye speed
9/10  END_SYSEX        (0xF7)}
function TNeoPixel.FadeStripConfig(color: longword; write: Boolean=True): string;
var
  tmpColor: longword;
  LoopsData: string;
begin
  Result:='';
  if FFadeRunning then // fade is running
  begin
    FBoard.RaiseError(53, 'FadeStripConfig');
    exit;
  end;

  if FGamma <> 1 then
    tmpColor:=gammaRGB(Color, FGamma)
  else
    tmpColor:=Color;

  if FfadeLoopsWait <> 1 then
    LoopsData:=chr(FFadeLoopsWait)
  else
    LoopsData:='';

  Result:=SendSysEx(chr(NEOPIXELS_DATA)+chr(NEOPIXELS_FADE_CONFIG)+chr(FDevice)+
                    encode32BitUnSignedInt(tmpColor)+LoopsData, write);

  if write then
  begin
    FFadeColor:=tmpColor;
    FFadeFirst:=0;
    FFadeLast:=FPixelsNumber -1;
  end;
end;
function TNeoPixel.FadeStripConfig(Red: byte; Green: byte; Blue: Byte; White: byte; write: Boolean=True): string;  overload;
begin
  Result:= FadeStripConfig(Blue or (integer(Green) << 8) or (integer(Red) << 16) or (integer(White) << 24), write);
end;
{0  START_SYSEX      (0xF0)
1  NEOPIXELS_DATA    (0X51)
2  NEOPIXELS_FADE_ONE_STEP (0x0A)
3  device            (0-3)
4  do_show           optional (0-1) 1=show
4/5  END_SYSEX        (0xF7)}
function TNeoPixel.FadeOneStep(do_show: boolean; write: Boolean=True): string;
var
  i, j: smallint;
  mask: longword;
  currentColor, targetColor, calcColor: longword;
begin
  Result:='';
  if FFadeRunning then // fade is running
  begin
    FBoard.RaiseError(53, 'FadeStripConfig');
    exit;
  end;
  if do_show then
    Result:=SendSysEx(chr(NEOPIXELS_DATA)+chr(NEOPIXELS_FADE_ONE_STEP)+chr(FDevice)+chr(ord(do_show)), write)
  else
    Result:=SendSysEx(chr(NEOPIXELS_DATA)+chr(NEOPIXELS_FADE_ONE_STEP)+chr(FDevice), write);

  if write then
  begin
    for i:=FFadeFirst to FFadeLast do
    begin
      mask:=$FF;
      calcColor:=0;
      for j:=0 to 3 do // for blue, green, red, white
      begin
        currentColor:=FPixels[i].PixelColor and mask;
        targetColor:=FFadeColor and mask;
        if currentColor > TargetColor then
          dec(currentColor)
        else if currentColor < TargetColor then
          inc(currentColor);
        calcColor:=calcColor or currentColor;
        mask:=mask << 8;
      end;
      FPixels[i].PixelColor:=calcColor;
    end;
  end;
end;
{0  START_SYSEX      (0xF0)
1  NEOPIXELS_DATA    (0X51)
2  NEOPIXELS_FADE_RUN_PAUSE  (0x04)
3  device            (0-3)
4  run/pause        (0-1) 1 fade run, 0 fade pause
5  END_SYSEX        (0xF7)}
function TNeoPixel.FadeRunPause(write: Boolean=True): string;
begin
  Result:=SendSysEx(chr(NEOPIXELS_DATA)+chr(NEOPIXELS_FADE_RUN_PAUSE)+chr(FDevice)+chr(ord(FFadeRunning)), write);
end;

{0  START_SYSEX      (0xF0)
1  NEOPIXELS_DATA    (0X51)
2  NEOPIXELS_SET_BRIGHTNESS  (0x05)
3  device            (0-3)
4  brighness bits0-6       lsb brightness value 0-255
5  brightness bit7, bit8   msb do_show
6  END_SYSEX        (0xF7)}
function TNeoPixel.sendBrightness(do_show: Boolean; write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(NEOPIXELS_DATA)+chr(NEOPIXELS_SET_BRIGHTNESS)+chr(FDevice)+
                    chr(FBrightness and $7F)+chr(((FBrightness >> 7) and $01) or (ord(do_show) << 1)), write);
end;

procedure TNeoPixel.RecalculateBrightness;   // only recalculate not show
var
  i: smallint;
begin
  for i:=0 to FPixelsNumber -1 do   // resend all pixels
      PixelColor(i, FPixels[i].PixelColor, false, true);  // Fpixels colors are gamma corrected
end;
{0  START_SYSEX      (0xF0)
1  NEOPIXELS_DATA    (0X51)
2  NEOPIXELS_MOVE_PIXELS (0x0B)
3  device            (0-3)
4  src pixel         lsb
5                    msb
6  dest pixel        lsb
7                    msb
8  count             lsb (optional) // default to 1
9                    msb
10 do_show           optional (0-1) 1=show
8/11  END_SYSEX        (0xF7)}
function TNeoPixel.Movepixels(src: smallint; dest: smallint; count: smallint; do_show: Boolean; write: Boolean=True): string;
var
  swap: smallint;
  Data: string;
begin
  Result:='';
  if src >= FPixelsNumber  then
  begin
    FBoard.RaiseError(50, 'MovePixels');
    exit;
  end;
  if dest >= FPixelsNumber  then
  begin
    FBoard.RaiseError(50, 'MovePixels');
    exit;
  end;
  if dest > src then
  begin
    swap:=src;
    src:=dest;
    dest:=swap;
  end;
  if ((src + count) >= FPixelsNumber) or ((dest + count) >= FPixelsNumber) then
  begin
    FBoard.RaiseError(50, 'MovePixels');
    exit;
  end;
  if count <> 1 then
    Data:=chr(count and $7F)+chr((count >> 7) and $7F)
  else
    Data:='';
  if do_show then
    data:=Data+chr(ord(do_show));

  Result:=SendSysEx(chr(NEOPIXELS_DATA)+chr(NEOPIXELS_MOVE_PIXELS)+chr(FDevice)+
                     chr(src and $7F)+chr((src >> 7) and $7F)+
                     chr(dest and $7F)+chr((dest >> 7) and $7F)+Data, write);
end;

end.
