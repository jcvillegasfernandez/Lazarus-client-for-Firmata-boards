//
//
//
// TSerial
//
//
//
constructor TSerial.Create(AOwner: TComponent);
begin
  inherited;

  FBoard:= nil;
  FEnabled:=false;
  FPort:=HW_SERIAL1;
  FBaudRate:=0;   // TODO
  FRxPin:=RES_RX1;
  FTxPin:=RES_TX1;
  FOnEnabled:=nil;
  FOnDisabled:=nil;

  FOnSerialMessage:=nil;
end;

destructor TSerial.Destroy();
begin
  inherited Destroy;
end;

procedure TSerial.setBoard(Board: TBoard);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'Serial:setBoard')
  else if Assigned(Board) then
    FBoard:=Board;
end;

procedure TSerial.setPort(Port: TSerialPorts);
begin
  if FEnabled then
  begin
    FBoard.RaiseError(33, 'Serial:setPort');
    exit;
  end;

  FPort:=Port;
end;

procedure TSerial.setRxPin(Pin: Byte);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'Serial:setRxPin')
  else if Pin > 127 then
  begin
    FBoard.RaiseError(31, 'Serial:setRxPin');
    FRxPin:=$7F;
  end
  else
    FRxPin:=Pin;
end;

procedure TSerial.setTxPin(Pin: Byte);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'Serial:setTxPin')
  else if Pin > 127 then
  begin
    FBoard.RaiseError(31, 'Serial:setTxPin');
    FTxPin:=$7F;
  end
  else
    FTxPin:=Pin;
end;

procedure TSerial.setBaudRate(BaudRate: integer);
begin
  begin
    FBoard.RaiseError(33, 'Serial:setBaudRate');
    exit;
  end;
  if BaudRate > $1FFFFF then
    FBaudRate:=$1FFFFF
  else if BaudRate < 1 then
    FBaudRate:=1
  else
    FBaudRate:=BaudRate;
end;

procedure TSerial.setEnabled(State: Boolean);
  function getPinFromPort(Port: TSerialPorts; PinType: TSerialPinType): Byte;
  var
    i: integer;
  begin
    Result:=PinModesToByte(PIN_MODE_IGNORE);
    for i:=0 to FBoard.FBoardPinsNumber - 1 do
      if (ord(Port)+ord(PinType)) = FBoard.GetPinResolution(i, PIN_MODE_SERIAL) then
      begin
        Result:=i;
        break;
      end;
  end;
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
      if FPort < SW_SERIAL0 then // hardware serial
      begin
        FRxPin:=getPinFromPort(FPort, RX);
        FTxPin:=getPinFromPort(FPort, TX);
        if (FRxPin = PinModesToByte(PIN_MODE_IGNORE)) or (FTxPin = PinModesToByte(PIN_MODE_IGNORE)) then // check capability pins
        begin
          FEnabled:=false;
          FBoard.RaiseError(11, 'Serial:setEnabled');
        end
        else if FBoard.FBoardPins[FRxPin].Busy or FBoard.FBoardPins[FRxPin].Busy then // now check if pins are busy
        begin
          FEnabled:=false;
          FBoard.RaiseError(12, 'Serial:setEnabled');
        end;
      end
      else // software serial
      begin
        // Check if supported pins are supported
        if not FBoard.CheckCapability(FRxPin, PIN_MODE_SERIAL) or not FBoard.CheckCapability(FTxPin, PIN_MODE_SERIAL) then // not supported
        begin
          FEnabled:=false;
          FBoard.RaiseError(7, 'Serial:setEnabled');
        end
        else if FBoard.FBoardPins[FRxPin].Busy or FBoard.FBoardPins[FTxPin].Busy then  // check if pins are assigned
        begin
          FEnabled:=false;
          FBoard.RaiseError(12, 'Serial:setEnabled');
        end;
      end;
    end
    else // firmataboard not enabled
    begin
      FEnabled:=false;
      FBoard.RaiseError(36, 'Serial:setenabled');
    end;
    if FEnabled then
    begin
      if Assigned(FBoard.FSerials[FPort]) then
      begin
        FEnabled:=False;
        FBoard.RaiseError(43, 'Serial:setEnabled');
      end
      else // new Tserial module
      begin
        FBoard.FSerials[FPort]:=self;
        FBoard.FBoardPins[FRxPin].Busy:=true;  //  pin is assigned to this module
        FBoard.FBoardPins[FTxPin].Busy:=true;  //  pin is assigned to this module
        config;  // configure Serial module
        if Assigned(FOnEnabled) then
          FOnEnabled(self);
      end;
    end;
  end
  else  // disable
  begin
    if Assigned(FOnDisabled) then
      FOnDisabled(self);
    if Assigned(FBoard) then
    begin
      FBoard.FBoardPins[FRxPin].Busy:=false; // free Rxpin
      FBoard.FBoardPins[FTxPin].Busy:=false; // free Txpin
      FBoard.FSerials[FPort]:=nil;
    end;
    FEnabled:=False;
  end;
end;

{ Send START_SYSEX + data + END_SYSEX total <= MAX_DATA_BYTES (64 bytes)}
function TSerial.SendSysEx(data: string; write: Boolean=true): string;
begin
  Result:='';
  if not FEnabled and write then
    exit;

  Result:=FBoard.SendSysEx(Data, write);
end;
//
// SERIAL COMMANDS
//
{0  START_SYSEX      (0xF0)
1  SERIAL_DATA      (0x60)  // command byte
2  SERIAL_CONFIG    (0x10)  // OR with port (0x11 = SERIAL_CONFIG | HW_SERIAL1)
3  baud             (bits 0 - 6)
4  baud             (bits 7 - 13)
5  baud             (bits 14 - 20) // need to send 3 bytes for baud even if value is < 14 bits
6  rxPin            (0-127) [optional] // only set if platform requires RX pin number
7  txPin            (0-127) [optional] // only set if platform requires TX pin number
6|8 END_SYSEX      (0xF7)}
function TSerial.Config(write: Boolean=true): String;
var
  StrBaud: string;
  RXTXPins: string;
begin
  Result:='';
  RXTXPins:='';

  StrBaud:=chr(FBaudRate and $7F)+chr((FBaudRate >> 7) and $7F)+ chr((FBaudRate >> 14) and $7F);

  if FPort >= SW_SERIAL0 then // soft port, pins have been sent
    RXTXPins:=chr(FRxPin)+chr(FTxPin);

  Result:=SendSysEx(chr(SERIAL_DATA)+chr(SERIAL_CONFIG or SerialPortsToByte(FPort))+StrBaud+RXTXPins, write);

  if write then
  begin
   FBoard.FBoardPins[FRxPin].ActualMode:=PinModesToByte(PIN_MODE_SERIAL);
   FBoard.FBoardPins[FTxPin].ActualMode:=PinModesToByte(PIN_MODE_SERIAL);
  end;
end;
{0  START_SYSEX      (0xF0)
1  SERIAL_DATA      (0x60)
2  SERIAL_WRITE     (0x20) // OR with port (0x21 = SERIAL_WRITE | HW_SERIAL1)
3  data 0           (LSB)
4  data 0           (MSB)
5  data 1           (LSB)
6  data 1           (MSB)
...                 // up to max buffer - 5
n  END_SYSEX        (0xF7)}
function TSerial.Write(Data: string; write: Boolean=true): String;
var
  TwoBytesChar: String;
begin
  TwoBytesChar:=Encode1ByteCharTo2(Data);
  Result:=SendSysEx(chr(SERIAL_DATA)+chr(SERIAL_WRITE or SerialPortsToByte(FPort))+TwoBytesChar, write);
end;
{0  START_SYSEX        (0xF0)
1  SERIAL_DATA        (0x60)
2  SERIAL_READ        (0x30) // OR with port (0x31 = SERIAL_READ | HW_SERIAL1)
3  SERIAL_READ_MODE   (0x00) // 0x00 => read continuously, 0x01 => stop reading
4  maxBytesToRead     (lsb) [optional]
5  maxBytesToRead     (msb) [optional]
4|6 END_SYSEX         (0xF7)}
function TSerial.Read(ReadMode: byte; BytesToRead: uint16; write: Boolean=true): String;
var
  ReadsNumber: string;
begin
  ReadsNumber:='';
  if BytesToRead > 0 then
    ReadsNumber:=chr(BytesToRead and $7F)+chr((BytesToRead >> 7) and $7F);
  Result:=SendSysEx(chr(SERIAL_DATA)+chr(SERIAL_READ or SerialPortsToByte(FPort))+chr(ReadMode and $01)+ReadsNumber, write);
end;
{0  START_SYSEX        (0xF0)
1  SERIAL_DATA        (0x60)
2  SERIAL_CLOSE       (0x50) // OR with port (0x51 = SERIAL_CLOSE | HW_SERIAL1)
3  END_SYSEX          (0xF7)}
function TSerial.Close(write: Boolean=True): string;
begin
  Result:=SendSysEx(chr(SERIAL_DATA)+chr(SERIAL_CLOSE or SerialPortsToByte(FPort)), write);
end;
{0  START_SYSEX        (0xF0)
1  SERIAL_DATA        (0x60)
2  SERIAL_FLUSH       (0x60) // OR with port (0x61 = SERIAL_FLUSH | HW_SERIAL1)
3  END_SYSEX          (0xF7)}
function TSerial.Flush(write: Boolean=True): string;
begin
  Result:=SendSysEx(chr(SERIAL_DATA)+chr(SERIAL_FLUSH or SerialPortsToByte(FPort)), write);
end;
{0  START_SYSEX        (0xF0)
1  SERIAL_DATA        (0x60)
2  SERIAL_LISTEN      (0x70) // OR with port to switch to (0x79 = switch to SW_SERIAL1)
3  END_SYSEX          (0xF7)}
function TSerial.Listen(write: Boolean=True): string;
begin
  Result:=SendSysEx(chr(SERIAL_DATA)+chr(SERIAL_LISTEN or SerialPortsToByte(FPort)), write);
end;

//
//
//
// TServo
//
//
//
constructor TServo.Create(AOwner: TComponent);
begin
  inherited;
  FBoard:= nil;
  FEnabled:=false;
  FPin:=PinModesToByte(PIN_MODE_IGNORE);
  FDevice:=PinModesToByte(PIN_MODE_IGNORE);

  FMinPulse:=544;
  FMaxPulse:=2400;
  FValue:=0;

  FOnEnabled:=nil;
  FOnDisabled:=nil;
end;

destructor TServo.Destroy();
begin
  inherited Destroy;
end;

procedure TServo.setBoard(Board: TBoard);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'Servo:setBoard')
  else if Assigned(Board) then
    FBoard:=Board;
end;

procedure TServo.setMinPulse(Pulse: integer);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'Servo:setMinPulse')
  else if Pulse > $3FFF then  // out of range
    FMinPulse:=544  // normal default
  else if Pulse < 1 then
    FMinPulse:=1
  else if Pulse > FMaxPulse then
    FMinPulse:=FMaxPulse
  else
    FMinPulse:=Pulse;
end;

procedure TServo.setMaxPulse(Pulse: integer);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'Servo:setMaxPulse')
  else if Pulse > $3FFF then  // out of range
    FMaxPulse:=2400   // Normal default
  else if Pulse < 1 then
    FMaxPulse:=1
  else if Pulse < FMinPulse then
    FMaxPulse:=FMinPulse
  else
    FMaxPulse:=Pulse;
end;

procedure TServo.setPin(Pin: Byte);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'Servo:setPin')
  else if Pin > 127 then
  begin
    FBoard.RaiseError(31, 'Servo:setPin');
    FPin:=$7F;
  end
  else
    FPin:=Pin;
end;

procedure TServo.setValue(Value: integer);
begin
  FValue:=Value;  // if value < FMinPulse means degrees 0º to 180º
  if FEnabled then
     WriteValue;
end;

procedure TServo.setEnabled(State: Boolean);
var
  i: integer;
begin
  if not Assigned(FBoard) then
    exit;

  if FEnabled = State then
     exit;

  if State then
  begin
    if FBoard.Enabled then
    begin
      FEnabled:=True;
      if FBoard.CheckCapability(FPin, PIN_MODE_SERVO) then
      begin
        if FBoard.FBoardPins[FPin].Busy then
        begin
          // Pin is busy
          FEnabled:=False;
          FBoard.RaiseError(12, 'Servo:setEnabled');
          exit;
        end
        else // pin is free
        begin
          for i:=0 to MAX_SERVOS - 1 do  // looking for a free servo room
          begin
            if not Assigned(FBoard.FServos[i]) then // found one free
            begin
              FDevice:=i;
              break;
            end;
          end;
          if FDevice <> PinModesToByte(PIN_MODE_IGNORE) then
          begin
            FBoard.FBoardPins[FPin].Busy:=True;
            FBoard.FServos[FDevice]:=self;
            Config;  // set pin mode to servo and config min and max pulses
            WriteValue;
            if Assigned(FOnEnabled) then
                 FOnEnabled(self);
          end
          else  // // there is not left servo
          begin
            FEnabled:=False;
            FBoard.RaiseError(44, 'Servo:setEnabled');
          end;
        end;
      end
      else  // not pin capability
      begin
        FEnabled:=False;
        // Not a valid mode
        FBoard.RaiseError(11, 'Servo:setEnabled');
      end;
    end;
  end
  else  // disable
  begin
    if Assigned(FOnDisabled) then
      FOnDisabled(self);
    if Assigned(FBoard) then
    begin
      FBoard.FBoardPins[FPin].Busy:=False;  // free pin
      FBoard.FServos[FDevice]:=nil;    // free servo
      FDevice:=PinModesToByte(PIN_MODE_IGNORE);
    end;
    FEnabled:=False;
  end;
end;

function TServo.SendCommand(Data: string; write: Boolean=True): string;
begin
  Result:='';
  if not FEnabled and write then
    exit;
  Result:=FBoard.SendCommand(Data, write);
end;

function TServo.SendSysEx(data: string; write: Boolean=true): string;
begin
  Result:='';
  if not FEnabled and write then
    exit;
  Result:=FBoard.SendSysEx(Data, write);
end;

// minPulse and maxPulse are 14-bit unsigned integers
{0  START_SYSEX          (0xF0)
1  SERVO_CONFIG         (0x70)
2  pin number           (0-127)
3  minPulse LSB         (0-6)
4  minPulse MSB         (7-13)
5  maxPulse LSB         (0-6)
6  maxPulse MSB         (7-13)
7  END_SYSEX            (0xF7) }
function TServo.config(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(SERVO_CONFIG)+chr(FPin)+chr(FMinPulse and $7F)+chr((FMinPulse >> 7) and $7F)+chr(FMaxPulse and $7F)+chr((FMaxPulse << 7) and $7F), write);
  if write then
    FBoard.FBoardPins[FPin].ActualMode:=PinModesToByte(PIN_MODE_SERVO);
end;
{
0  analog pin, 0xE0-0xEF, (MIDI Pitch Wheel)
1  analog least significant 7 bits
2  analog most significant 7 bits }
function TServo.AnalogWrite(Value: word; write: Boolean=true): string;
var
  ValueTmp: word;
begin
  // search for resolution
  ValueTmp:=min(Value, FBoard.GetPinResolution(FPin, PIN_MODE_SERVO));

  Result:=SendCommand(chr(ANALOG_MESSAGE or FPin)+chr(Value and $7F)+chr(Value >> 7 and $7F), write);
  if write then
    FValue:=ValueTmp;
end;
{0  START_SYSEX              (0xF0)
1  extended analog message  (0x6F)
2  pin                      (0-127)
3  bits 0-6                 (least significant byte)
4  bits 7-13                (most significant byte)
... additionaly bytes may be sent if more bits are needed
N  END_SYSEX     (0xF7)}
function TServo.AnalogWriteExtended(Value: uint32; write: Boolean=true): string;
var
  i: Byte;
  Valuetmp: uint32;
  Data: String;
begin
  Data:='';

  // search for resolution
  ValueTmp:=min(Value, FBoard.GetPinResolution(FPin, PIN_MODE_SERVO));;

  Data:=chr(Valuetmp and $FF);  // byte 1
  Valuetmp:=Valuetmp >> 8; // prepare value

  for i:=2 to 4 do    // max 4 bytes 32 bits, perhaps max 3 bytes 24 bits
  begin
    data:=data+chr(Valuetmp and $FF);  // LSB byte ,MSB byte
    Valuetmp:=Valuetmp >> 8; // prepare value
    if Valuetmp = 0 then  // no more data to send
      break;
  end;

  Result:=SendSysEx(chr(EXTENDED_ANALOG)+chr(FPin)+Encode8To7Bit(Data), write);
  if write then
    FValue:=min(Value, FBoard.GetPinResolution(FPin, PIN_MODE_SERVO));
end;
{Write to servo, servo write is performed if the pin mode is SERVO
if value > $3FFF (14 bits) or Pin > 15 then extended analog write else Analog Write }
function TServo.WriteValue(write: Boolean=true): string;
begin
  if (FValue > $3FFF) or (FPin > 15) then
    Result:=AnalogWriteExtended(FValue, write)
  else
    Result:=AnalogWrite(FValue, write);
end;
//
//
//
// TEncoder
//
//
//
constructor TEncoder.Create(AOwner: TComponent);
begin
  inherited;
  FBoard:= nil;
  FEnabled:=false;
  FPinA:=PinModesToByte(PIN_MODE_IGNORE);
  FPinB:=PinModesToByte(PIN_MODE_IGNORE);
  FDevice:=PinModesToByte(PIN_MODE_IGNORE);  // not assigned to board yet

  FOnEnabled:=nil;
  FOnDisabled:=nil;
end;

destructor TEncoder.Destroy();
begin
  inherited Destroy;
end;

procedure TEncoder.setBoard(Board: TBoard);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'Encoder:setBoard')
  else if Assigned(Board) then
    FBoard:=Board;
end;

procedure TEncoder.setEnabled(State: Boolean);
var
  i: integer;
begin
  if not Assigned(FBoard) then
    exit;

  if FEnabled = State then
     exit;

  if State then
  begin
    if FBoard.Enabled then
    begin
      FEnabled:=True;
      if FBoard.CheckCapability(FPinA, PIN_MODE_ENCODER)
         and FBoard.CheckCapability(FPinB, PIN_MODE_ENCODER) then  // I think all pins are capable, not information in capability
      begin
        if FBoard.FBoardPins[FPinA].Busy or FBoard.FBoardPins[FPinA].Busy then
        begin
          // Pin is busy
          FEnabled:=False;
          FBoard.RaiseError(12, 'Encoder:setEnabled');
          exit;
        end
        else // pins are free
        begin
          for i:=0 to MAX_ENCODERS - 1 do
          begin
            if not Assigned(FBoard.FEncoders[i]) then  // found a free encoder
            begin
              FDevice:=i;
              break;
            end;
          end;
          if FDevice <> PinModesToByte(PIN_MODE_IGNORE) then
          begin
            FBoard.FBoardPins[FPinA].Busy:=True;
            FBoard.FBoardPins[FPinB].Busy:=True;
            FBoard.FEncoders[FDevice]:=self;
            Attach;  // set pins to encoders
            if Assigned(FOnEnabled) then
              FOnEnabled(self);
          end
          else  // there is not left encoder
          begin
            FEnabled:=False;
            FBoard.RaiseError(44, 'Encoder:setEnabled');
          end;
        end;
      end
      else  // not pin capability
      begin
        FEnabled:=False;
        // Not a valid mode
        FBoard.RaiseError(11, 'Encoder:setEnabled');
      end;
    end;
  end
  else  // disable
  begin
    if Assigned(FOnDisabled) then
      FOnDisabled(self);
    if Assigned(FBoard) then
    begin
      FBoard.FBoardPins[FPinA].Busy:=False;
      FBoard.FBoardPins[FPinB].Busy:=False;
      FBoard.FEncoders[FDevice]:=nil;
      FDevice:=PinModesToByte(PIN_MODE_IGNORE);
    end;
    FEnabled:=False;
  end;
end;

procedure TEncoder.setPinA(Pin: Byte);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'Encoder:setDataPin')
  else if Pin > 127 then
  begin
    FBoard.RaiseError(31, 'Encoder:setDataPin');
    FPinA:=$7F;
  end
  else
    FPinA:=Pin;
end;

procedure TEncoder.setPinB(Pin: Byte);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'Encoder:setPinB')
  else if Pin > 127 then
  begin
    FBoard.RaiseError(31, 'Encoder:setPinB');
    FPinB:=$7F;
  end
  else
    FPinB:=Pin;
end;

function TEncoder.SendSysEx(data: string; write: Boolean=true): string;
begin
  Result:='';
  if not FEnabled and write then
    exit;
  Result:=FBoard.SendSysEx(Data, write);
end;
{/* -----------------------------------------------------
* 0 START_SYSEX                (0xF0)
* 1 ENCODER_DATA               (0x61)
* 2 ENCODER_ATTACH             (0x00)
* 3 encoder #                  ([0 - MAX_ENCODERS-1])
* 4 pin A #                    (first pin)
* 5 pin B #                    (second pin)
* 6 END_SYSEX                  (0xF7)
* -----------------------------------------------------
*/ }
function TEncoder.Attach(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(ENCODER_DATA)+chr(ENCODER_ATTACH)+chr(FDevice)+chr(FPinA)+chr(FPinB), write);
  if write then
  begin
    FBoard.FBoardPins[FPinA].ActualMode:=PinModesToByte(PIN_MODE_ENCODER);
    FBoard.FBoardPins[FPinB].ActualMode:=PinModesToByte(PIN_MODE_ENCODER);
  end;
end;
{
Report encoder's position  Query

 /* -----------------------------------------------------
 * 0 START_SYSEX                (0xF0)
 * 1 ENCODER_DATA               (0x61)
 * 2 ENCODER_REPORT_POSITION    (0x01)
 * 3 Encoder #                  ([0 - MAX_ENCODERS-1])
 * 4 END_SYSEX                  (0xF7)
 * -----------------------------------------------------
 */}
function TEncoder.QueryPosition(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(ENCODER_DATA)+chr(ENCODER_REPORT_POSITION)+chr(FDevice), write);
end;
{Report all encoders positions  Query

 /* -----------------------------------------------------
 * 0 START_SYSEX                (0xF0)
 * 1 ENCODER_DATA               (0x61)
 * 2 ENCODER_REPORT_POSITIONS   (0x02)
 * 3 END_SYSEX                  (0xF7)
 * -----------------------------------------------------
 */}
 function TEncoder.AllEncoderPositions(write: Boolean=true): string;
 begin
   Result:=SendSysEx(chr(ENCODER_DATA)+chr(ENCODER_REPORT_POSITIONS), write);
 end;
{Reset encoder position to zero  Query

  /* -----------------------------------------------------
  * 0 START_SYSEX                (0xF0)
  * 1 ENCODER_DATA               (0x61)
  * 2 ENCODER_RESET_POSITION     (0x03)
  * 3 encoder #                  ([0 - MAX_ENCODERS-1])
  * 4 END_SYSEX                  (0xF7)
  * -----------------------------------------------------
  */ }
function TEncoder.ResetPosition(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(ENCODER_DATA)+chr(ENCODER_RESET_POSITION)+chr(FDevice), write);
end;
{Enable/disable reporting Query

 /* -----------------------------------------------------
 * 0 START_SYSEX                (0xF0)
 * 1 ENCODER_DATA               (0x61)
 * 2 ENCODER_REPORT_AUTO        (0x04)
 * 3 enable                     (0x00 => false, true otherwise)
 * 4 END_SYSEX                  (0xF7)
 * -----------------------------------------------------
 */
}
function TEncoder.Reporting(Enabled: Boolean; write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(ENCODER_DATA)+chr(ENCODER_REPORT_AUTO)+chr(ord(Enabled)), write);
end;
{Detach encoder Query

 /* -----------------------------------------------------
 * 0 START_SYSEX                (0xF0)
 * 1 ENCODER_DATA               (0x61)
 * 2 ENCODER_DETACH             (0x05)
 * 3 encoder #                  ([0 - MAX_ENCODERS-1])
 * 4 END_SYSEX                  (0xF7)
 * -----------------------------------------------------
 */}
function TEncoder.Detach(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(ENCODER_DATA)+chr(ENCODER_DETACH)+chr(FDevice), write);
  if write then
  begin
    FBoard.FBoardPins[FPinA].ActualMode:=PinModesToByte(PIN_MODE_OUTPUT);
    FBoard.FBoardPins[FPinB].ActualMode:=PinModesToByte(PIN_MODE_OUTPUT);
    setEnabled(False);  // disable encoder and pins
  end;
end;
{Response to query encoder's position

 /* -----------------------------------------------------
 * 0 START_SYSEX                (0xF0)
 * 1 ENCODER_DATA               (0x61)
 * 2 Encoder #  &  DIRECTION    [= (direction << 6) | (#)]
 * 3 current position, bits 0-6
 * 4 current position, bits 7-13
 * 5 current position, bits 14-20
 * 6 current position, bits 21-27
 * 7 END_SYSEX                  (0xF7)
 * -----------------------------------------------------
 */
 Note : Byte #2 contains both encoder's number (i.e. channel) and encoder's direction.
 Direction is stored on the seventh bit, 0 (LOW) for positive and 1 (HIGH) for negative.

directionMask = 0x40; // B01000000
channelMask   = 0x3F; // B00111111

//ex direction is negative and encoder is on index 2
direction = 1;
encoder = 2;
bytes[2] =  (direction << 6) | (encoder);
}
procedure TEncoder.parsefirmatacommand(Sender: TObject; CommandData: String);
var
  Position: integer;
begin
  // first byte is encoder and direction
  Position:=ord(CommandData[2]) or (ord(CommandData[3]) << 7) or (ord(CommandData[4]) << 14) or (ord(CommandData[5]) << 21);
  if Assigned(FOnEncoderPosition) then
    FOnEncoderPosition(self, ord((ord(CommandData[1]) and DIRECTION_MASK) <> 0), Position);
end;
//
//
// TSPI
//
//
constructor TSPI.Create(AOwner: TComponent);
begin
  inherited;

  FDeviceID:=0;   // max in this moment is 8
  FBoard:= nil;
  FEnabled:=false;
  FMOSIpin:=11;  // arduino default
  FMISOpin:=12;  // arduino default
  FCLKpin:=13;  // arduino defualt
  FCSpin:=10;   // default pin for Chip Select
  FSpeed:= 1000000;    // default speed 1000000 hertz, max on arduino is 8000000
  FWordSize:=8;
  FChannel:=0;
  FCSControl:=true;
  FCSActiveLow:=True;
  FMode:=0;  // SPI modes 0, 1, 2, 3
  FBitOrder:=MSB_FIRST;
  FPacketData7Bit:=True;

  FOnEnabled:=nil;
  FOnDisabled:=nil;

  FOnSPIData:=nil;
end;

destructor TSPI.Destroy();
begin
  inherited Destroy;
end;

procedure TSPI.setBoard(Board: TBoard);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'SPI:setBoard')
  else if Assigned(Board) then
    FBoard:=Board;
end;

procedure TSPI.setMISOPin(Pin: Byte);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'SPI:setMISOPin')
  else if Pin > $7F then
  begin
    FBoard.RaiseError(31, 'SPI:setMISOPin');
    FMISOPin:=12;
  end
  else
    FMISOPin:=Pin;
end;

procedure TSPI.setMOSIPin(Pin: Byte);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'SPI:setMOSIPin')
  else if Pin > $7F then
  begin
    FBoard.RaiseError(31, 'SPI:setMOSIPin');
    FMOSIPin:=11;
  end
  else
    FMOSIPin:=Pin;
end;

procedure TSPI.setCLKPin(Pin: Byte);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'SPI:setCLKPin')
  else if Pin > $7F then
  begin
    FBoard.RaiseError(31, 'SPI:setCLKPin');
    FCLKPin:=13;
  end
  else
    FCLKPin:=Pin;
end;

procedure TSPI.setCSPin(Pin: Byte);
begin
  if FEnabled and FCSControl then
    FBoard.RaiseError(33, 'SPI:setCSPin')
  else if Pin > $7F then
  begin
    FBoard.RaiseError(31, 'SPI:setCSPin');
    FCSPin:=10;
  end
  else
    FCSPin:=Pin;
end;

function TSPI.setCSPinValue(Value: Byte; write: Boolean=true): string;
begin
  Result:='';
  if not FEnabled and write then
    exit;
  Result:=Board.SendCommand(chr(SET_DIGITAL_PIN_VALUE)+chr(FCSPin)+chr(Value), true); // $F5
end;

procedure TSPI.setChannel(Value: Byte);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'SPI:setChannel')
  else if Value > 7 then
  begin
    FChannel:=0;
    FBoard.RaiseError(58, 'SPI:setChannel');
  end
  else
    FChannel:=Value;
end;

procedure TSPI.setDeviceID(Value: Byte);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'SPI:setDeviceID')
  else if Value > 15 then
  begin
    FDeviceID:=0;
    FBoard.RaiseError(42, 'SPI:setDeviceID');
  end
  else
    FDeviceID:=Value;
end;

procedure TSPI.setMode(Mode: byte);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'SPI:setMode')
  else if Mode > 3 then
  begin
    FBoard.RaiseError(35, 'SPI:setMode');
    FMode:=0;
  end
  else
    FMode:=Mode;
end;

procedure TSPI.setSpeed(Value: integer);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'SPI:setSpeed')
  else if Value < 0 then
    FSpeed:=1000000     // default speed
  else
    FSpeed:=Value;
end;

procedure TSPI.setWordSize(Value: byte);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'SPI:setWordSize')
  else if (Value <> 0) or (Value <> 8) then
  begin
    FBoard.RaiseError(59, 'SPI:setWordSize');
    FWordSize:=8;
  end
  else
    FWordSize:=Value;
end;

procedure TSPI.setCSControl(Value: Boolean);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'SPI:setCSControl')
  else
    FCSControl:=Value;
end;

procedure TSPI.setBitOrder(value: TBitOrder);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'SPI:setWordSize')
  else
    FBitOrder:=Value;
end;

procedure TSPI.setPacketData7Bit(Value: boolean);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'SPI:setWPacketData7Bit')
  else
    FPacketData7Bit:=Value;
end;

procedure TSPI.setEnabled(State: Boolean);
var i: integer;
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
      if not FBoard.CheckCapability(FMISOPin, PIN_MODE_SPI) then // not supported
      begin
        FEnabled:=false;
        FBoard.RaiseError(7, 'SPI:setEnabled, setMISOPin');
      end
      else if FBoard.FBoardPins[FMISOPin].Busy and (FBoard.FBoardPins[FMISOPin].ActualMode <> PinModesToByte(PIN_MODE_SPI))  then  // check if pin is assigned
      begin
        FEnabled:=false;
        FBoard.RaiseError(12, 'SPI:setEnabled, setMISOPin');
      end
      else if not FBoard.CheckCapability(FMOSIPin, PIN_MODE_SPI) then // not supported
      begin
        FEnabled:=false;
        FBoard.RaiseError(7, 'SPI:setEnabled, setMOSIPin');
      end
      else if FBoard.FBoardPins[FMOSIPin].Busy and (FBoard.FBoardPins[FMOSIPin].ActualMode <> PinModesToByte(PIN_MODE_SPI))then  // check if pin is assigned
      begin
        FEnabled:=false;
        FBoard.RaiseError(12, 'SPI:setEnabled, setMOSIPin');
      end
      else if not FBoard.CheckCapability(FCLKPin, PIN_MODE_SPI) then // not supported
      begin
        FEnabled:=false;
        FBoard.RaiseError(7, 'SPI:setEnabled, setCLKPin');
      end
      else if FBoard.FBoardPins[FCLKPin].Busy and (FBoard.FBoardPins[FCLKPin].ActualMode <> PinModesToByte(PIN_MODE_SPI))then  // check if pin is assigned
      begin
        FEnabled:=false;
        FBoard.RaiseError(12, 'SPI:setEnabled, setCLKPin');
      end
      else if FCSControl then
      begin
        if not FBoard.CheckCapability(FCSPin, PIN_MODE_OUTPUT) then // not supported
        begin
          FEnabled:=false;
          FBoard.RaiseError(7, 'SPI:setEnabled, setCSPin');
        end
        else if FBoard.FBoardPins[FCSPin].Busy then  // check if pin is assigned
        begin
          FEnabled:=false;
          FBoard.RaiseError(12, 'SPI:setEnabled, setCSPin');
        end;
      end
      else if (FWordSize <> 8) and (FWordSize <> 0) then
        FPacketData7Bit:=False;
    end
    else
    begin
      FEnabled:=false; // firmataboard not enabled
      FBoard.RaiseError(2, 'SPI:setEnabled');
    end;
    if FEnabled then  // can be enabled
    begin
      for i:=0 to MAX_SPI_DEVICES - 1 do   // check if there is room for device
      begin
        if not Assigned(FBoard.FSPIs[i]) then
        begin
          FDeviceID:=i;
          FBoard.FSPIs[i]:=self;
          FBoard.FBoardPins[FMOSIPin].Busy:=true;  //  pin is assigned to this module
          FBoard.FBoardPins[FMISOPin].Busy:=true;  //  pin is assigned to this module
          FBoard.FBoardPins[FCLKPin].Busy:=true;  //  pin is assigned to this module
          if FCSControl then
          begin
            FBoard.FBoardPins[FCSPin].ActualMode:=PinModesToByte(PIN_MODE_OUTPUT);
            FBoard.FBoardPins[FCSPin].Busy:=true;  //  pin is assigned to this module
          end;
          SPIBegin;  // initialize SPI
          Config; // configure work mode
          if Assigned(FOnEnabled) then
            FOnEnabled(self);
          break;
        end;
        FEnabled:=False;
        FBoard.RaiseError(56, 'SPI:setEnabled');
      end;
    end;
  end
  else  // disable
  begin
    if Assigned(FOnDisabled) then
      FOnDisabled(self);
    if Assigned(FBoard) then
    begin
      SPIEnd;  // disable this SPI
      FBoard.FSPIs[FDeviceID]:=nil;
      FBoard.FBoardPins[FMOSIPin].Busy:=false;  //  pin is assigned to this module
      FBoard.FBoardPins[FMISOPin].Busy:=false;  //  pin is assigned to this module
      FBoard.FBoardPins[FCLKPin].Busy:=false;  //  pin is assigned to this module
      if FCSControl then
        FBoard.FBoardPins[FCSPin].Busy:=false;  //  pin is assigned to this module
    end;
    FEnabled:=False;
  end;
end;

procedure TSPI.parsefirmatacommand(Sender: TObject; CommandData: String);
var
  DataString: string;
  TmpDeviceIDChannel: byte;
  RequestID: byte;
  NumData: byte;
begin
  // first byte is subcommand
  case ord(CommandData[1]) of
    {0:  START_SYSEX
    1:  SPI_DATA              (0x68)
    2:  SPI_REPLY             (0x05)
    3:  deviceId | channel    (bits 3-6: deviceId, bits 0-2: channel)
    4:  requestId             (0-127) // must match the ID from the request
    5:  numWords              (0-127: number of words in the reply)
    6:  data 0                (bits 0-6)
    7:  data 0                (bits 7-14 if word size if word size > 7 && < 15)
    8:  data 0                (if word size > 14)
    ...                       up to numWords * (wordSize / 7)
    N:  END_SYSEX}
    SPI_REPLY: begin  // $05
      TmpDeviceIdChannel:=ord(CommandData[2]);
      RequestID:=ord(CommandData[3]);
      NumData:=ord(CommandData[4]);
      DataString:=Copy(CommandData, 5, Length(CommandData)-4);

      if FPacketData7Bit then
        DataString:=Decode7To8Bit(DataString)
      else
        DataString:=Decode2BytesCharTo1(DataString);

      if Assigned(FOnSPIData) then
        FOnSPIData(self, (TmpDeviceIdChannel and $78) >> 3, TmpDeviceIdChannel and $7, RequestID, DataString, NumData);
     end;
   end;
end;
{ Send START_SYSEX + data + END_SYSEX total <= MAX_DATA_BYTES (64 bytes)}
function TSPI.SendSysEx(data: string; write: Boolean=true): string;
begin
  Result:='';
  if not FEnabled and write then
    exit;

  Result:=FBoard.SendSysEx(Data, write);
end;

//
// SPI commands
//

{0  START_SYSEX      (0xF0)
1  SPI     Command  (0x68)
2  begin  command   (0x00)
3  channel  (0-7)   default 0, hardware SPIs, most microcontrollers only have one
3  END_SYSEX (0xF7)}
function TSPI.SPIBegin(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(SPI_DATA)+chr(SPI_BEGIN)+chr(FChannel), write);   // send begin
  if write then
  begin
    FBoard.FBoardPins[FMOSIPin].ActualMode:=PinModesToByte(PIN_MODE_SPI);
    FBoard.FBoardPins[FMISOPin].ActualMode:=PinModesToByte(PIN_MODE_SPI);
    FBoard.FBoardPins[FCLKPin].ActualMode:=PinModesToByte(PIN_MODE_SPI);
    if FCSControl then
      FBoard.FBoardPins[FCSPin].ActualMode:=PinModesToByte(PIN_MODE_OUTPUT);  //  pin is assigned to this module
  end;
end;

{0:  START_SYSEX
1:  SPI_DATA              (0x68)
2:  SPI_DEVICE_CONFIG     (0x01)
3:  deviceId | channel    (bits 3-6: deviceId, bits 0-2: channel)
4:  dataMode | bitOrder   (bit 3: 7 bit encoding, bits 1-2: dataMode (0-3), bit 0: bitOrder
                           bit order =0 LSB, =1 MSB )
5:  maxSpeed              (bits 0 - 6)
6:  maxSpeed              (bits 7 - 14)
7:  maxSpeed              (bits 15 - 21)
8:  maxSpeed              (bits 22 - 28)
9:  maxSpeed              (bits 29 - 32)
10: wordSize              (0 = DEFAULT = 8-bits, 1 = 1-bit, 2 = 2 bits, etc)
11: csPinOptions          bit 0: CS_PIN_CONTROL (0 = disable
                                                 1 = enable (default))
                          bit 1: CS_ACTIVE_STATE (0 = Active LOW (default)
                                                  1 = Active HIGH)
                          bits 2-6: reserved for future options
12: csPin                 (0-127) The chip select pin number (ignored if
                          CS_PIN_CONTROL set to 0)
13: END_SYSEX }
function TSPI.config(write: Boolean=true): string;
var
  TmpDeviceIDChannel: byte;
  TmpModeBitOrder: byte;
  TmpCSOptions: byte;
begin
  TmpDeviceIDChannel:=((FDeviceID and $F) << 3) or (FChannel and $7);
  TmpModeBitOrder:=(ord(FPacketData7Bit) << 3 ) or (FMode << 1) or ord(FBitOrder);
  TmpCSOptions:=(ord(not FCSActiveLow) << 1) or ord(FCSControl);
  Result:=SendSysEx(chr(SPI_DATA)+chr(SPI_DEVICE_CONFIG)+chr(TmpDeviceIDChannel)+
      chr(TmpModeBitOrder)+encode32BitUnSignedInt(FSpeed)+chr(FWordSize)+
      chr(TmpCSOptions)+chr(FCSpin and $7F), write);
end;
{0:  START_SYSEX
1:  SPI_DATA              (0x68)
2:  SPI_TRANSFER          (0x02)
3:  deviceId | channel    (bits 3-6: deviceId, bits 0-2: channel)
4:  requestId             (0-127) // increment for each call
5:  deselectCsPin         (0 = don't deselect csPin
                          1 = deselect csPin (default))
6.  numWords              (0-127: number of words to transfer)
7:  data 0                (bits 0-6)
8:  data 0                (bits 7-14 if word size if word size > 7 && < 15)
9:  data 0                (if word size > 14)
...                       up to numWords * (wordSize / 7)
N:  END_SYSEX
requestId is used in the request messages SPI_TRANSFER, SPI_WRITE and SPI_READ and in the reply message SPI_REPLY}
function TSPI.Transfer(RequestID: byte; Data: string; DeselectCSPin: Boolean=True; write: Boolean=true): string;
var
  TmpDeviceIDChannel: byte;
  SPIData: string;
begin
  Result:='';
  TmpDeviceIDChannel:=((FDeviceID and $F) << 3) or (FChannel);

  if FPacketData7Bit then
    SPIData:=Encode8To7Bit(Data)   // Data size 51 max
  else
    SPIData:=Encode1ByteCharTo2(Data);  // data size 29 max

  if Length(SPIData) > 58 then  // 64-6=58
  begin
    FBoard.RaiseError(30, 'SPI:Transfer');
    exit;
  end;

  if not FCSControl then   // Manual handling of CS Pin
    Result:=setCSPinValue(ord(not FCSActiveLow), write); // chip select

  Result:=Result+SendSysEx(chr(SPI_DATA)+chr(SPI_TRANSFER)+chr(TmpDeviceIDChannel)+
      chr(RequestID and $7F)+chr(ord(DeselectCSPin))+chr(Length(Data) and $7F)+Result, write);

  if not FCSControl and DeselectCSPin then   // Manual handling of CS Pin
    Result:=Result+setCSPinValue(ord(FCSActiveLow), write); // chip deselect chip
end;
{0:  START_SYSEX
1:  SPI_DATA              (0x68)
2:  SPI_WRITE             (0x03)
3:  deviceId | channel    (bits 3-6: deviceId, bits 0-2: channel)
4:  requestId             (0-127) // increment for each call
5:  deselectCsPin         (0 = don't deselect csPin
                          1 = deselect csPin (default))
6.  numWords              (0-127: number of words to write)
7:  data 0                (bits 0-6)
8:  data 0                (bits 7-14 if word size if word size > 7 && < 15)
9:  data 0                (if word size > 14)
...                       up to numWords * (wordSize / 7)
N:  END_SYSEX
A SPI_WRITE command should return a SPI_REPLY with a value of 1 if the write was successful or a value of 0 if the write failed.}
function TSPI.Write(RequestID: byte; Data: string; DeselectCSPin: Boolean=True; write: Boolean=true): string;
var
  TmpDeviceIDChannel: byte;
  SPIData: string;
begin
  Result:='';
  TmpDeviceIDChannel:=((FDeviceID and $7) << 3) or (FChannel);

  if FPacketData7Bit then
    SPIData:=Encode8To7Bit(Data)   // Data size 51 max
  else
    SPIData:=Encode1ByteCharTo2(Data);  // data size 29 max

  if Length(SPIData) > 58 then  // 64-6=58
  begin
    FBoard.RaiseError(30, 'SPI:Write');
    exit;
  end;

  if not FCSControl then   // Manual handling of CS Pin
    Result:=setCSPinValue(ord(not FCSActiveLow), write); // chip select

  Result:=Result+SendSysEx(chr(SPI_DATA)+chr(SPI_WRITE)+chr(TmpDeviceIDChannel)+
      chr(RequestID and $7F)+chr(ord(DeselectCSPin))+chr(Length(SPIData) and $7F)+SPIData, write);

  if not FCSControl and DeselectCSPin then   // Manual handling of CS Pin
    Result:=Result+setCSPinValue(ord(FCSActiveLow), write); // chip deselect chip
end;
{0:  START_SYSEX
1:  SPI_DATA              (0x68)
2:  SPI_WRITE_ACK         (0x07)
3:  deviceId | channel    (bits 3-6: deviceId, bits 0-2: channel)
4:  requestId             (0-127) // increment for each call
5:  deselectCsPin         (0 = don't deselect csPin
                          1 = deselect csPin (default))
6.  numWords              (0-127: number of words to write)
7:  data 0                (bits 0-6)
8:  data 0                (bits 7-14 if word size if word size > 7 && < 15)
9:  data 0                (if word size > 14)
...                       up to numWords * (wordSize / 7)
N:  END_SYSEX}
function TSPI.WriteAck(RequestID: byte; Data: string; DeselectCSPin: Boolean=True; write: Boolean=true): string;
var
  TmpDeviceIDChannel: byte;
  SPIData: string;
begin
  Result:='';
  TmpDeviceIDChannel:=((FDeviceID and $7) << 3) or (FChannel);

  if FPacketData7Bit then
    SPIData:=Encode8To7Bit(Data)   // Data size 51 max
  else
    SPIData:=Encode1ByteCharTo2(Data);  // data size 29 max

  if Length(SPIData) > 58 then  // 64-6=58
  begin
    FBoard.RaiseError(30, 'SPI:Write');
    exit;
  end;

  if not FCSControl then   // Manual handling of CS Pin
    Result:=setCSPinValue(ord(not FCSActiveLow), write); // chip select

  Result:=Result+SendSysEx(chr(SPI_DATA)+chr(SPI_WRITE_ACK)+chr(TmpDeviceIDChannel)+
      chr(RequestID and $7F)+chr(ord(DeselectCSPin))+chr(Length(SPIData) and $7F)+SPIData, write);

  if not FCSControl and DeselectCSPin then   // Manual handling of CS Pin
    Result:=Result+setCSPinValue(ord(FCSActiveLow), write); // chip deselect chip

end;
{0:  START_SYSEX
1:  SPI_DATA              (0x68)
2:  SPI_READ              (0x04)
3:  deviceId | channel    (bits 3-6: deviceId, bits 0-2: channel)
4:  requestId             (0-127)  // increment for each call
5:  deselectCsPin         (0 = don't deselect csPin
                          1 = deselect csPin (default))
6.  numWords              (0-127: number of words to read)
7:  END_SYSEX}
function TSPI.Read(RequestID: byte; NumData: byte; DeselectCSPin: Boolean=True; write: Boolean=true): string;
var
  TmpDeviceIDChannel: byte;
begin
  Result:='';
  TmpDeviceIDChannel:=((FDeviceID and $7) << 3) or (FChannel);

  if (FPacketData7Bit and (NumData > 51)) or (not FPacketData7Bit and (NumData > 29)) then  // 64-5=59 so 29 non packet bytes
  begin                                                                                  // or 52 bytes encoded 7 bit packet, but 51 same as similar functions
    FBoard.RaiseError(30, 'SPI:Read');
    exit;
  end;

  if not FCSControl then   // Manual handling of CS Pin
    Result:=setCSPinValue(ord(not FCSActiveLow), write); // chip select

  Result:=Result+SendSysEx(chr(SPI_DATA)+chr(SPI_READ)+chr(TmpDeviceIDChannel)+
      chr(RequestID and $7F)+chr(ord(DeselectCSPin))+chr(NumData and $7F), write);

  if not FCSControl and DeselectCSPin then   // Manual handling of CS Pin
    Result:=Result+setCSPinValue(ord(FCSActiveLow), write); // chip deselect chip

end;
{0:  START_SYSEX
1:  SPI_DATA              (0x68)
2:  SPI_END               (0x06)
3:  channel               (HW supports multiple SPI ports. range = 0-7, default = 0)
4:  END_SYSEX }
function TSPI.SPIEnd(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(SPI_DATA)+chr(SPI_END)+chr(FChannel), write);
end;
//
//
//
// TFrequency
//
//
//
constructor TFrequency.Create(AOwner: TComponent);
begin
  inherited;
  FBoard:= nil;
  FEnabled:=false;
  FPin:=PinModesToByte(PIN_MODE_IGNORE);
  FDevice:=PinModesToByte(PIN_MODE_IGNORE);  // not assigned to board yet
  FSamplingInterval:=100;       // 100 milisecs
  FInterruptMode:=INTERRUPT_MODE_DISABLE;            // disable


  FOnEnabled:=nil;
  FOnDisabled:=nil;
end;

destructor TFrequency.Destroy();
begin
  inherited Destroy;
end;

procedure TFrequency.setPin(Value: Byte);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'Frequency:setPin')
  else if Value > 127 then
  begin
    FBoard.RaiseError(31, 'Frequency:setPin');
    FPin:=$7F;
  end
  else
    FPin:=Value;
end;

procedure TFrequency.setBoard(Board: TBoard);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'Frequency:setBoard')
  else if Assigned(Board) then
    FBoard:=Board;
end;

procedure TFrequency.setSamplingInterval(Interval: integer);
begin
  if FEnabled then
  begin
    FBoard.RaiseError(33, 'Frequency:setSamplingInterval');
    exit;
  end;
  if (Interval < 0) or (Interval > $3FFF) then   // max value 14 bits
  begin
    FBoard.RaiseError(42, 'Frequency:setSamplingInterval');
    exit;
  end;
  FSamplingInterval:=Interval;
end;

procedure TFrequency.setInterruptMode(Mode: TInterruptModes);
begin
  if Mode = FInterruptMode then
    exit;
  if Enabled then
    FBoard.RaiseError(33, 'Frequency:setInterruptMode')
  else //if Mode in [INTERRUPT_MODE_DISABLE,INTERRUPT_MODE_LOW, INTERRUPT_MODE_HIGH,
       //            INTERRUPT_MODE_RISING, INTERRUPT_MODE_FALLING, INTERRUPT_MODE_CHANGE] then
    FInterruptMode:=Mode;
end;

procedure TFrequency.setEnabled(State: Boolean);
var
  i: integer;
begin
  if not Assigned(FBoard) then
    exit;

  if FEnabled = State then
     exit;

  if State then
  begin
    if FBoard.Enabled then
    begin
      FEnabled:=True;
      if FBoard.CheckCapability(FPin, PIN_MODE_FREQUENCY) then  // only pins 2 and 3 on avr boards
      begin
        if FBoard.FBoardPins[FPin].Busy then
        begin
          // Pin is busy
          FEnabled:=False;
          FBoard.RaiseError(12, 'Frequency:setEnabled');
          exit;
        end
        else // pin is free
        begin
          for i:=0 to MAX_FREQUENCIES - 1 do
          begin
            if not Assigned(FBoard.FFrequencies[i]) then  // found a free module
            begin
              FDevice:=i;
              break;
            end;
          end;
          if FDevice <> PinModesToByte(PIN_MODE_IGNORE) then
          begin
            FBoard.FBoardPins[FPin].Busy:=True;
            FBoard.FFrequencies[FDevice]:=self;
            Attach;  // set pin to Frequency
            if Assigned(FOnEnabled) then
              FOnEnabled(self);
          end
          else  // there is not left pin
          begin
            FEnabled:=False;
            FBoard.RaiseError(44, 'Frequency:setEnabled');
          end;
        end;
      end
      else  // not pin capability
      begin
        FEnabled:=False;
        // Not a valid mode
        FBoard.RaiseError(11, 'Frequency:setEnabled');
      end;
    end;
  end
  else  // disable
  begin
    if Assigned(FOnDisabled) then
      FOnDisabled(self);
    if Assigned(FBoard) then
    begin
      DisableReporting;
      FBoard.FBoardPins[FPin].Busy:=False;
      FBoard.FFrequencies[FDevice]:=nil;
      FDevice:=PinModesToByte(PIN_MODE_IGNORE);
    end;

    FEnabled:=False;
  end;
end;

procedure TFrequency.parsefirmatacommand(Sender: TObject; CommandData: String);
var
  Pin: byte;
  Time: integer;
  Ticks: integer;

begin
  // first byte is pin
  Pin:=ord(CommandData[1]);
  Time:=decode32BitSignedInt(Copy(CommandData, 2, 5));  // time
  Ticks:=decode32BitSignedInt(Copy(CommandData, 7, 5)); // ticks
  if Assigned(FOnFrequencyData) then
    FOnFrequencyData(self,Pin,Time,Ticks);
end;

function TFrequency.SendSysEx(data: string; write: Boolean=true): string;
begin
  Result:='';
  if not FEnabled and write then
    exit;
  Result:=FBoard.SendSysEx(Data, write);
end;
{/* -----------------------------------------------------
  * 0 START_SYSEX                (0xF0)
  * 1 FREQUENCY_COMMAND          (0x7D)
  * 2 FREQUENCY_SUBCOMMAND_QUERY (0x01)
  * 3 pin                        (0-127)
  * 4 interrupt mode             See below
  * 5 sampling interval          (lsb, bits 0-6)
  * 6 sampling interval          (msb, bits 7-13) lsb and msb together give a 14 bit sampling interval, given in milliseconds.
  * N END_SYSEX                  (0xF7)
  * -----------------------------------------------------
Interrup mode
  INTERRUPT_MODE_DISABLE 0
  INTERRUPT_MODE_LOW 1
  INTERRUPT_MODE_HIGH 2
  INTERRUPT_MODE_RISING 3
  INTERRUPT_MODE_FALLING 4
  INTERRUPT_MODE_CHANGE 5
*/}
function TFrequency.Attach(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(FREQUENCY_COMMAND)+chr(FREQUENCY_SUBCOMMAND_QUERY)+chr(FPin)+chr(ord(FInterruptMode))+
          chr(FSamplingInterval and $7F)+chr((FSamplingInterval >> 7) and $7F), write);
  if write then
    FBoard.FBoardPins[FPin].ActualMode:=PinModesToByte(PIN_MODE_FREQUENCY);
end;
{
Frequency disable reporting

 /* -----------------------------------------------------
 0 START_SYSEX                (0xF0)
 1 FREQUENCY_COMMAND          (0x7D)
 2 FREQUENCY_SUBCOMMAND_CLEAR (0x00)
 3 pin                        (0-127) Use 0x7F to clear all pin interrupts.
 N END_SYSEX                  (0xF7)
 * -----------------------------------------------------
 */}
function TFrequency.DisableReporting(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(FREQUENCY_COMMAND)+chr(FREQUENCY_SUBCOMMAND_CLEAR)+chr(FPin), write);
  if write then
    FBoard.FBoardPins[FPin].ActualMode:=PinModesToByte(PIN_MODE_OUTPUT);
end;

//
//
// TDHT
//
//
constructor TDHT.Create(AOwner: TComponent);
begin
  inherited;

  FBoard:=nil;
  FEnabled:=False;
  FPin:=PinModesToByte(PIN_MODE_IGNORE);
  FBlockingReads:=False;
  FSamplingInterval:=500;  // default and minimun is 500ms
  FSensorType:=DHT_11;

  FOnEnabled:=nil;
  FOnDisabled:=nil;

  FOnDHTData:=nil;
end;

destructor TDHT.Destroy();
begin
  inherited Destroy;
end;

procedure TDHT.setBoard(Board: TBoard);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'DHT:setBoard')
  else if Assigned(Board) then
    FBoard:=Board;
end;

procedure TDHT.setPin(Pin: Byte);
begin
  if FEnabled then
     FBoard.RaiseError(33, 'DHT:setPin')
  else if Pin > 127 then
  begin
    FBoard.RaiseError(31, 'DHT:setPin');
    FPin:=$7F;
  end
  else
    FPin:=Pin;
end;

procedure TDHT.setSensorType(Value: TDHTSensorType);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'DHT:setSensorType')
  else
    FSensorType:=Value;
end;

procedure TDHT.setSamplingInterval(Value: integer);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'DHT:setSamplingInterval')
  else if Value < 500 then
    FSamplingInterval:=500    // default and minimun speed 500ms
  else
    FSamplingInterval:=Value;
end;

procedure TDHT.setBlockingReads(Value: Boolean);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'DHT:setBlockingReads')
  else
    FBlockingReads:=Value;
end;

procedure TDHT.setEnabled(State: Boolean);
var i: integer;
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
      if not FBoard.CheckCapability(FPin, PIN_MODE_DHT) then // not supported
      begin
        FEnabled:=false;
        FBoard.RaiseError(7, 'DHT:setEnabled, Pin');
      end
      else if FBoard.FBoardPins[FPin].Busy then  // check if pin is assigned
      begin
        FEnabled:=false;
        FBoard.RaiseError(12, 'DHT:setEnabled, Pin');
      end;
    end
    else
    begin
      FEnabled:=false; // firmataboard not enabled
      FBoard.RaiseError(2, 'DHT:setEnabled');
    end;
    if FEnabled then  // can be enabled
    begin
      if Assigned(FBoard.FDHTs[FPin]) then
      begin
        FEnabled:=False;
        FBoard.RaiseError(54, 'DHT:setEnabled');
      end
      else // new DHT module
      begin
        FBoard.FDHTs[FPin]:=self;
        FBoard.FBoardPins[FPin].Busy:=true;  //  pin is assigned to this module
        Attach;  // attach sensor
        if Assigned(FOnEnabled) then
          FOnEnabled(self);
      end;
    end;
  end
  else  // disable
  begin
    if Assigned(FOnDisabled) then
      FOnDisabled(self);
    if Assigned(FBoard) then
    begin
      Detach;  // detach sensor
      FBoard.FDHTs[FPin]:=nil;
      FBoard.FBoardPins[FPin].Busy:=false;
    end;
    FEnabled:=False;
  end;
end;

procedure TDHT.parsefirmatacommand(Sender: TObject; CommandData: String);
var
  Temperature: single;
  Humidity: single;
  Pin: Byte;
begin
  // first byte is subcommand

    {0 START_SYSEX                (0xF0)
    1 DHTSENSOR_DATA             (0x74)
    2 DHTSENSOR_RESPONSE         (0x00)
    3 pin                        (0-127)
    4 temperature, bits 0-6      (lsb - temperature in Celsius * 10)
    5 temperature, bits 7-13     (msb - temperature in Celsius * 10)
    6 humidity, bits 0-6         (lsb - relative humidity in % * 10)
    7 humidity, bits 7-13        (msb - relative humidity in % * 10)
    N END_SYSEX   (0xF7)}
    if ord(CommandData[1]) = DHTSENSOR_RESPONSE then
    begin  // $00
      Pin:=ord(CommandData[2]);
      Temperature:=(ord(CommandData[3]) or (ord(CommandData[4]) << 7)) / 10;
      Humidity:=(ord(CommandData[5]) or (ord(CommandData[6]) << 7)) / 10;

      if Assigned(FOnDHTData) then
        FOnDHTData(self, Pin, Temperature, Humidity);
   end;
end;
{ Send START_SYSEX + data + END_SYSEX total <= MAX_DATA_BYTES (64 bytes)}
function TDHT.SendSysEx(data: string; write: Boolean=true): string;
begin
  Result:='';
  if not FEnabled and write then
    exit;

  Result:=FBoard.SendSysEx(Data, write);
end;

//
// DHT commands
//

{0 START_SYSEX                (0xF0)
1 DHTSENSOR_DATA             (0x74)
2 DHTSENSOR_ATTACH_DHT22     (0x02) [for DHT_11 sensors use DHTSENSOR_ATTACH_DHT11 (0x01)]
3 pin                        (0-127)
4 blocking reads             (0-1) [optional - enables an 18ms blocking call when starting the read **use with caution**. default: 0 (disabled)]
5 sampling interval          (lsb) [optional - sampling interval to report sensor data (in ms). minimum: 500ms. default: 500ms.]
... additional bytes may be sent if more bits are needed
N END_SYSEX                  (0xF7)   }
{  Sensor Type
01  DHT_11
02  DHT_22
11  DHT_11
12  DHT_12
21  DHT_21
22  DHT_22
 //AM2301 = DHT_21}
function TDHT.Attach(write: Boolean=true): string;
var
  ValueSensorType: Byte;
begin
  // No blockings reads and sampling interval a the moment
  case FSensorType of
    DHT_11: ValueSensorType:=11;
    DHT_12: ValueSensorType:=12;
    DHT_21, AM2301: ValueSensorType:=21;
    DHT_22: ValueSensorType:=22;
  end;

  Result:=SendSysEx(chr(DHTSENSOR_DATA)+chr(ValueSensorType)+chr(FPin), write);
  if write then
    FBoard.FBoardPins[FPin].ActualMode:=PinModesToByte(PIN_MODE_DHT);
end;

{0 START_SYSEX                (0xF0)
1 DHTSENSOR_DATA             (0x74)
2 DHTSENSOR_DETACH           (0x03)
3 pin                        (0-127)
4 END_SYSEX                  (0xF7)}
function TDHT.Detach(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(DHTSENSOR_DATA)+chr(DHTSENSOR_DETACH)+chr(FPin), write);
  if write then
    FBoard.FBoardPins[FPin].ActualMode:=PinModesToByte(PIN_MODE_OUTPUT);
end;
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
  FDataPin:=PinModesToByte(PIN_MODE_IGNORE);
  FDevice:=PinModesToByte(PIN_MODE_IGNORE);  // PIN_MODE_IGNORE means no device
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
    FBoard.RaiseError(33, 'PS2Mouse:setBoard')
  else if Assigned(Board) then
    FBoard:=Board;
end;

procedure TPS2Mouse.setClockPin(Pin: Byte);
begin
  if FEnabled then
     FBoard.RaiseError(33, 'PS2Mouse:setClockPin')
  else if Pin > 127 then
  begin
    FBoard.RaiseError(31, 'PS2Mouse:setClockPin');
    FClockPin:=$7F;
  end
  else
    FClockPin:=Pin;
end;

procedure TPS2Mouse.setDataPin(Pin: Byte);
begin
  if FEnabled then
     FBoard.RaiseError(33, 'PS2Mouse:setDataPin')
  else if Pin > 127 then
  begin
    FBoard.RaiseError(31, 'PS2Mouse:setDataPin');
    FDataPin:=$7F;
  end
  else
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
          FBoard.RaiseError(12, 'PS2Mouse:setEnabled');
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
        FBoard.RaiseError(11, 'PS2Mouse:setEnabled');
      end;
    end
    else
    begin
      FEnabled:=false; // firmataboard not enabled
      FBoard.RaiseError(2, 'PS2Mouse:setEnabled');
    end;
    if FEnabled and (FDevice = PinModesToByte(PIN_MODE_IGNORE)) then // There isn't a free mouse
    begin
      FEnabled:=False;
      FBoard.RaiseError(44, 'PS2Mouse:setEnabled');
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

procedure TPS2Mouse.parsefirmatacommand(Sender: TObject; CommandData: String);
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
function TPS2Mouse.SendSysEx(data: string; write: Boolean=true): string;
begin
  Result:='';
  if not FEnabled and write then
    exit;

  Result:=FBoard.SendSysEx(Data, write);
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
  FLedColors:='BGRW';
  FPixels:=nil;
  FPixelsNumber:=1;
  FGamma:=1;
  FFadeRunning:=false;
  FFadeColor:=0;
  FFadeFirst:=0;
  FFadeLast:=0;
  FShiftFirst:=0;
  FShiftLast:=0;
  FShiftType:=7; // bit0 = 1 right  = 0 left, bit1 = 1 wrap,
                  //  bit2 = 1 do_show
  FFadeLoopsWait:=1;  // if SamplingRate = 19, 1 is about 25 Hz eye speed
  FBrightness:=0;  // 0=Max, 1=Min, 255= 1 below max
end;

destructor TNeoPixel.Destroy();
begin
  inherited Destroy;
end;

procedure TNeoPixel.setBoard(Board: TBoard);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'NeoPixel:setBoard')
  else if Assigned(Board) then
    FBoard:=Board;
end;

procedure TNeoPixel.setPin(Pin: Byte);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'NeoPixel:setPin')
  else if Pin > 127 then
  begin
    FBoard.RaiseError(20, 'NeoPixel:setPin');
    FPin:=$7F;
  end
  else
    FPin:=Pin;
end;

function TNeoPixel.getNeoPixel(Index: integer): TPixel;
begin
  if (Index < Length(FPixels)) and (Index >= 0) then
    Result:=FPixels[Index]
  else
    Result:=default(TPixel);
end;

procedure TNeoPixel.setLedColors(rgbwType: string);  // 0bRRRRGGBB for RGB,  0bWWRRGGBB for RGBW devices
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
    FBoard.RaiseError(48, 'NeoPixel:setLedColors');
    FLedColors:='RGBW';
    exit;
  end;
  FLedColors:=UpperCase(rgbwType);
end;

procedure TNeoPixel.setGamma(Value: single);
begin
  if Value < 0 then
  begin
    FBoard.RaiseError(52, 'NeoPixel:setGamma');
    FGamma:=1;
    exit;
  end;
  FGamma:=Value;
end;

procedure TNeoPixel.setPixelsNumber(Number: smallint);
begin
  if FEnabled then
    FBoard.RaiseError(33, 'NeoPixel:setPixelsNumber')
  else if (Number < 0) or (Number > $3FFF) then
  begin
    FPixelsNumber:=0;
    FBoard.RaiseError(20, 'NeoPixel:setPixelsNumber');
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
    FBoard.RaiseError(20, 'NeoPixel:setFadeLoopsWait');
    exit;
  end;
  FFadeLoopsWait:=Loops;
end;

procedure TNeoPixel.RainBow(First_Pixel: uint16; Last_Pixel: uint16; First_hue: uint16 = 0;
             saturation: byte = 255; brightness: byte = 255; gammify: boolean = false; Delay: smallint=0; do_show: Boolean=true);
var
  i: integer;
  hue: integer;
  color: uint32;
begin
  for i:=First_Pixel to Last_Pixel do
  begin
    hue:=floor(first_hue + (i * 65536) / (Last_Pixel - First_Pixel + 1));
    color:= ColorHSV(hue, saturation, brightness);
    if gammify then
      color:= gamma32(color, FGamma);
    PixelColor(i, color, do_show);
    if do_show then
      Sleep(Delay + 1); // 1 ms min necessary delay with do_show to give firmata some time
  end;
end;

function TNeoPixel.gamma32(x: uint32; gamma: single): uint32;
var
  i: integer;

  function gamma8(x: byte; gamma: single): longword;
  begin
    Result:=trunc(power(x / 255.0, gamma) * 255 + 0.5);
  end;
begin
  Result:=0;
  for i:= 0 to 3 do
    Result:=Result or (gamma8(x >> (8 * i) and $FF, gamma) << (8 * i));
end;

function TNeoPixel.ColorHSV(hue: uint16; sat: byte; val: byte): uint32;
var
  r, g, b: byte;
  Tmphue: integer;
  v1: uint32;
  s1: uint16;
  s2: byte;
begin
  Tmphue:= trunc((hue * 1530 + 32768) / 65536);
  if Tmphue < 510 then // Red to Green-1
  begin
    b:= 0;
    if Tmphue < 255 then //   Red to Yellow-1
    begin
      r:= 255;
      g:= Tmphue;       //     g = 0 to 254
    end
    else         //   Yellow to Green-1
    begin
      r:= 510 - Tmphue; //     r = 255 to 1
      g:= 255;
    end;
  end
  else if Tmphue < 1020 then // Green to Blue-1
  begin
    r:= 0;
    if Tmphue < 765 then //   Green to Cyan-1
    begin
        g:= 255;
        b:= Tmphue - 510;  //     b = 0 to 254
    end
    else           //   Cyan to Blue-1
    begin
        g:= 1020 - Tmphue; //     g = 255 to 1
        b:= 255;
    end
  end
  else if Tmphue < 1530 then // Blue to Red-1
  begin
    g:= 0;
    if Tmphue < 1275 then //   Blue to Magenta-1
    begin
      r:= Tmphue - 1020; //     r = 0 to 254
      b:= 255;
    end
    else  //   Magenta to Red-1
    begin
      r:= 255;
      b:= 1530 - Tmphue; //     b = 255 to 1
    end;
  end
  else  // Last 0.5 Red (quicker than % operator)
  begin
      r:= 255;
      g:= 0;
      b:= 0;
  end;
  v1:= 1 + val;  // 1 to 256; allows >>8 instead of /255
  s1:= 1 + sat;  // 1 to 256; same reason
  s2:= 255 - sat; // 255 to 0

  Result:=((((((r * s1) >> 8) + s2) * v1) and $FF00) << 8) or
           (((((g * s1) >> 8) + s2) * v1) and $FF00) or
           (((((b * s1) >> 8) + s2) * v1) >> 8);
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
      if FBoard.CheckCapability(FPin, PIN_MODE_NEOPIXEL) then
      begin
        if FBoard.FBoardPins[FPin].Busy then
        begin
          FEnabled:=false;
          FBoard.RaiseError(12, 'NeoPixel:setEnabled');
        end
        else  // pin is free
        begin
          if FPixelsNumber < 1 then
          begin
           FEnabled:=false;
           FBoard.RaiseError(49, 'NeoPixel:setEnabled');
          end
          else if Length(FLedColors) < 1 then
          begin
            FEnabled:=false;
            FBoard.RaiseError(51, 'NeoPixel:setEnabled');
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
        FBoard.RaiseError(11, 'NeoPixel:setEnabled');
      end;
    end
    else  // firmata board not enabled
    begin
      FEnabled:=false; // firmataboard not enabled
      FBoard.RaiseError(2, 'NeoPixel:setEnabled');
    end;
    if FDevice = PinModesToByte(PIN_MODE_IGNORE) then // There isn't any free
    begin
      FEnabled:=False;
      FBoard.RaiseError(44, 'NeoPixel:setEnabled');
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

procedure TNeoPixel.parsefirmatacommand(Sender: TObject; Data: string);
var
  i: smallint;
begin
  FFadeRunning:=False;  // update fade status

  for i:=FFadeFirst to FFadeLast do
    FPixels[i].PixelColor:=FFadeColor;   // update pixels

  if Assigned(FOnFadeEnd) then
    FOnFadeEnd(self, ord(Data[1]));    // ord(data[1]) = Device
end;

{ Send START_SYSEX + data + END_SYSEX total <= MAX_DATA_BYTES (64 bytes)}
function TNeoPixel.SendSysEx(data: string; write: Boolean=true): string;
begin
  Result:='';
  if not FEnabled and write then
    exit;

  Result:=FBoard.SendSysEx(Data, write);
end;
//
// NEOPIXEL commands
//

{0  START_SYSEX      (0xF0)
1  NEOPIXEL_DATA    (0X51)
2  NEOPIXEL_CONFIG  (0x01)
3  device            (0-3)
4  Pin               (0-127)
5  NumberLeds        lsb(14 bits max)
6                    msb
7  LedType           lsb (9 bits) //bit 8 = KHZ400, bits 7-0 'RGBW' position;
8                    msb
9  END_SYSEX (0xF7)}

{
// Bits 5,4 of this value are the offset (0-3) from the first byte of a
// pixel to the location of the red color byte.  Bits 3,2 are the green
// offset and 1,0 are the blue offset.  If it is an RGBW-type device
// (supporting a white primary in addition to R,G,B), bits 7,6 are the
// offset to the white byte...otherwise, bits 7,6 are set to the same value
// as 5,4 (red) to indicate an RGB (not RGBW) device.
// i.e. binary representation:
  ledType bits 0-7 0bWWRRGGBB for RGBW devices, R offset <> W offset
  ledType bits 0-7 0bRRRRGGBB for RGB, R offset = W offset
  ledType bit 8 = 1 old  NEO_KHZ400, bit 8 = 0 new NEO_KHZ800
// Offset:         W          R          G          B
#define NEO_WRGB ((0 << 6) | (1 << 4) | (2 << 2) | (3)) ///< Transmit as W,R,G,B
#define NEO_WRBG ((0 << 6) | (1 << 4) | (3 << 2) | (2)) ///< Transmit as W,R,B,G
#define NEO_WGRB ((0 << 6) | (2 << 4) | (1 << 2) | (3)) ///< Transmit as W,G,R,B
#define NEO_WGBR ((0 << 6) | (3 << 4) | (1 << 2) | (2)) ///< Transmit as W,G,B,R
#define NEO_WBRG ((0 << 6) | (2 << 4) | (3 << 2) | (1)) ///< Transmit as W,B,R,G
#define NEO_WBGR ((0 << 6) | (3 << 4) | (2 << 2) | (1)) ///< Transmit as W,B,G,R

#define NEO_RWGB ((1 << 6) | (0 << 4) | (2 << 2) | (3)) ///< Transmit as R,W,G,B
#define NEO_RWBG ((1 << 6) | (0 << 4) | (3 << 2) | (2)) ///< Transmit as R,W,B,G
#define NEO_RGWB ((2 << 6) | (0 << 4) | (1 << 2) | (3)) ///< Transmit as R,G,W,B
#define NEO_RGBW ((3 << 6) | (0 << 4) | (1 << 2) | (2)) ///< Transmit as R,G,B,W
#define NEO_RBWG ((2 << 6) | (0 << 4) | (3 << 2) | (1)) ///< Transmit as R,B,W,G
#define NEO_RBGW ((3 << 6) | (0 << 4) | (2 << 2) | (1)) ///< Transmit as R,B,G,W

#define NEO_GWRB ((1 << 6) | (2 << 4) | (0 << 2) | (3)) ///< Transmit as G,W,R,B
#define NEO_GWBR ((1 << 6) | (3 << 4) | (0 << 2) | (2)) ///< Transmit as G,W,B,R
#define NEO_GRWB ((2 << 6) | (1 << 4) | (0 << 2) | (3)) ///< Transmit as G,R,W,B
#define NEO_GRBW ((3 << 6) | (1 << 4) | (0 << 2) | (2)) ///< Transmit as G,R,B,W
#define NEO_GBWR ((2 << 6) | (3 << 4) | (0 << 2) | (1)) ///< Transmit as G,B,W,R
#define NEO_GBRW ((3 << 6) | (2 << 4) | (0 << 2) | (1)) ///< Transmit as G,B,R,W

#define NEO_BWRG ((1 << 6) | (2 << 4) | (3 << 2) | (0)) ///< Transmit as B,W,R,G
#define NEO_BWGR ((1 << 6) | (3 << 4) | (2 << 2) | (0)) ///< Transmit as B,W,G,R
#define NEO_BRWG ((2 << 6) | (1 << 4) | (3 << 2) | (0)) ///< Transmit as B,R,W,G
#define NEO_BRGW ((3 << 6) | (1 << 4) | (2 << 2) | (0)) ///< Transmit as B,R,G,W
#define NEO_BGWR ((2 << 6) | (3 << 4) | (1 << 2) | (0)) ///< Transmit as B,G,W,R
#define NEO_BGRW ((3 << 6) | (2 << 4) | (1 << 2) | (0)) ///< Transmit as B,G,R,W
}
function TNeoPixel.config(write: Boolean=true): string;
var
  TmpLedColors: smallint;
  PosR, PosG, PosB, PosW: integer;
begin
  Result:='';

  PosR:=Pos('R', FLedColors)-1;
  PosG:=Pos('G', FLedColors)-1;
  PosB:=Pos('B', FLedColors)-1;
  if Length(FLedColors) = 4 then  // ledType bits 0-7 0bWWRRGGBB for WRGB, R offset <> W offset
    PosW:=Pos('W', FLedColors)-1
  else  // Length(FLedColors) = 3, ledType bits 0-7 0bRRRRGGBB for RGB devices, R offset = W offset
    PosW:=PosR;

  TmpLedColors:=(PosW << 6) or (PosR << 4) or (PosG << 2) or PosB;
  if FKHZ400 then
    TmpLedColors:=$100 or TmpLedColors;  // set bit 8 if KHZ400

  Result:=SendSysEx(chr(NEOPIXEL_DATA)+chr(NEOPIXEL_CONFIG)+chr(FDevice)+chr(FPin)+chr(FPixelsNumber and $7F)
                  +chr((FPixelsNumber >> 7) and $7F)+chr(TmpLedColors and $7F)+chr((TmpLedColors >> 7) and $7F), write);

  if write then
  begin
    FFadeRunning:=false;
    FShiftType:=$07;   // default right, wrap and show
    FBoard.FBoardPins[FPin].ActualMode:=PinModesToByte(PIN_MODE_NEOPIXEL);
  end;
end;

{0  START_SYSEX      (0xF0)
 1  NEOPIXEL_DATA    (0X51)
 2  NEOPIXEL_OFF     (0x00)
 3  device            (0-3)
 4  END_SYSEX        (0xF7)}
function TNeoPixel.Off(write: Boolean=true): string;
begin
  Result:='';

  if FFadeRunning then // fade is running
    FFadeRunning:=False;
  Result:=SendSysEx(chr(NEOPIXEL_DATA)+chr(NEOPIXEL_OFF)+chr(FDevice), write);
end;

{0  START_SYSEX      (0xF0)
 1  NEOPIXEL_DATA    (0X51)
 2  NEOPIXEL_SHOW    (0x02)
 3  device            (0-3)
 4  END_SYSEX        (0xF7)}
function TNeoPixel.Show(write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(NEOPIXEL_DATA)+chr(NEOPIXEL_SHOW)+chr(FDevice), write);
end;
{0  START_SYSEX      (0xF0)
 1  NEOPIXEL_DATA    (0X51)
 2  NEOPIXEL_SET_PIXEL  (0x03)
 3  device            (0-3)
 4  Pixel             lsb
 5                    msb
 6  colorRGBW         b = bits 7-0
 7   encoded 7 bits   g = bits 15-8
 8   5 bytes          r = bits 23-16
 9                    w = bits 31-24 of color, send white even not used
 10                   do_show bit32, =1 show, bit32 is bit4 of 5th byte
 11  END_SYSEX        (0xF7)}
function TNeoPixel.PixelColor(Pixel: smallint; Color: longword; do_show: boolean=true; write: Boolean=true): string;
var
  tmpColor: longword;
  Color7Bits: string;
begin
  Result:='';

  if FFadeRunning then // fade is running
  begin
    FBoard.RaiseError(53, 'NeoPixel:PixelColor');
    exit;
  end;
  if FGamma <> 1 then
    tmpColor:=gamma32(Color, FGamma)
  else
    tmpColor:=Color;

  Color7Bits:=encode32BitUnSignedInt(tmpColor);
  if do_show then
    Color7Bits[5]:=chr(ord(Color7Bits[5]) or $10); // bit4 %10000 = $10
  Result:=SendSysEx(chr(NEOPIXEL_DATA)+chr(NEOPIXEL_SET_PIXEL)+chr(FDevice)+
                   chr(Pixel and $7F)+chr((Pixel >> 7) and $7F)+Color7Bits, write);
  if write then
    FPixels[Pixel].PixelColor:=tmpColor;
end;
function TNeoPixel.PixelColor(Pixel: smallint; Red: byte; Green: byte; Blue: Byte; White: byte; do_show: boolean=true; write: Boolean=true): string; overload;
begin
  Result:=PixelColor(Pixel, Blue or (integer(Green) << 8) or (integer(Red) << 16) or (integer(White) << 24), do_show, write);
end;

{0  START_SYSEX      (0xF0)
 1  NEOPIXEL_DATA    (0X51)
 2  NEOPIXEL_FILL_SEGMENT    (0x07)
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
function TNeoPixel.FillSegment(Color: longword; First: smallint; Last: smallint; do_show: Boolean=true; write: Boolean=true): string;
var
   i: integer;
   tmpColor: longword;
   Color7Bits: string;
begin
  Result:='';

  if FFadeRunning then // fade is running
  begin
    FBoard.RaiseError(53, 'NeoPixel:FillSegment');
    exit;
  end;
  if First >= FPixelsNumber then
  begin
    FBoard.RaiseError(50, 'NeoPixel:FillSegment');
    exit;
  end;
  if Last >= FPixelsNumber then
  begin
    FBoard.RaiseError(50, 'NeoPixel:FillSegment');
    exit;
  end;

  if FGamma <> 1 then
    tmpColor:=gamma32(Color, FGamma)
  else
    tmpColor:=Color;

  Color7Bits:=encode32BitUnSignedInt(tmpColor);
  if do_show then
    Color7Bits[5]:=chr(ord(Color7Bits[5]) or $10); // bit4

  Result:=SendSysEx(chr(NEOPIXEL_DATA)+chr(NEOPIXEL_FILL_SEGMENT)+chr(FDevice)+
                    Color7Bits+chr(First and $7F)+chr((First >> 7) and $7F)+
                    chr(Last and $7F)+chr((Last >> 7) and $7F), write);

 if write then
   for i:=First to Last do
     FPixels[i].PixelColor:=tmpColor;

end;
function TNeoPixel.FillSegment(Red: byte; Green: byte; Blue: Byte; White: byte; first: smallint; last: smallint; do_show: Boolean=true; write: Boolean=true): string; overload;
begin
  Result:=FillSegment(Blue or (integer(Green) << 8) or (integer(Red) << 16) or (integer(White) << 24), first, last, do_show, write);
end;
{0  START_SYSEX      (0xF0)
 1  NEOPIXEL_DATA    (0X51)
 2  NEOPIXEL_FILL_SEGMENT   (0x07)
 3  device            (0-3)
 4  colorRGBW         b = bits 7-0
 5   encoded 7 bits   g = bits 15-8
 6  5 bytes           r = bits 23-16
 7                    w = bits 31-24 of color, send white even not used
 8                    do_show bit32, =1 show, bit32 is bit4 of 5th byte
 9  END_SYSEX     (0xF7)}
function TNeoPixel.FillStrip(Color: longword; do_show: Boolean=true; write: Boolean=true): string;
var
   i: smallint;
   tmpColor: longword;
   Color7Bits: string;
begin
  if FGamma <> 1 then
    tmpColor:=gamma32(Color, FGamma)
  else
    tmpColor:=Color;

  Color7Bits:=encode32BitUnSignedInt(tmpColor);
  if do_show then
    Color7Bits[5]:=chr(ord(Color7Bits[5]) or $10); // bit4

  Result:=SendSysEx(chr(NEOPIXEL_DATA)+chr(NEOPIXEL_FILL_SEGMENT)+chr(FDevice)+Color7Bits, write);

  if write then
    for i:=0 to FPixelsNumber - 1 do
      FPixels[i].PixelColor:=tmpColor;
end;
function TNeoPixel.FillStrip(Red: byte; Green: byte; Blue: Byte; White: byte; do_show: Boolean=true; write: Boolean=true): string; overload;
begin
  Result:=FillStrip(Blue or (integer(Green) << 8) or (integer(Red) << 16) or (integer(White) << 24), do_show, write);
end;

{0  START_SYSEX      (0xF0)
1  NEOPIXEL_DATA    (0X51)
2  NEOPIXEL_SHIFT_CONFIG   (0x06)
3  device            (0-3)
4  shif_type         bit0 = 1 right, bit1 = 1 wrap, = 0 left
                     bit2 = 1 do_show
5  First pixel       lsb
6                    msb
7  Last pixel        lsb
8                    msb
9  END_SYSEX        (0xF7)}
function TNeoPixel.ShiftSegmentConfig(First: smallint; Last: smallint; Direction: char; Wrap: Boolean; do_show: Boolean=true; write: Boolean=true): string;
begin
  Result:='';

  if First >= FPixelsNumber then
  begin
    FBoard.RaiseError(50, 'NeoPixel:ShiftSegmentConfig');
    exit;
  end;
  if Last >= FPixelsNumber then
  begin
    FBoard.RaiseError(50, 'NeoPixel:ShiftSegmentConfig');
    exit;
  end;
    // right = bit0 1=right, 0=left
    // wrap =  bit1 1=wrap
    // do_show = bit2 1=show
  FShiftType:=ord('R' = UpperCase(Direction)) or (ord(Wrap) << 1) or (ord(do_show) << 2);

  Result:=SendSysEx(chr(NEOPIXEL_DATA)+chr(NEOPIXEL_SHIFT_CONFIG)+chr(FDevice)+
                    chr(FShiftType)+chr(First and $7F)+chr((First >> 7) and $7F)+
                    chr(Last and $7F)+chr((Last >> 7) and $7F), write);

  if write then
  begin
    FShiftFirst:=First;
    FShiftLast:=Last;
  end;
end;
{0  START_SYSEX      (0xF0)
1  NEOPIXEL_DATA    (0X51)
2  NEOPIXEL_SHIFT_CONFIG  (0x06)
3  device            (0-3)
4  shif_type         bit0 = 1 right 0=left, bit1 = 1 wrap,
                     bit2 = 1 do_show
5  END_SYSEX        (0xF7)}
// shift strip
function TNeoPixel.ShiftStripConfig(Direction: char; Wrap: Boolean; do_show: Boolean=true; write: Boolean=true): string; overload;
begin
  Result:='';

  // right = bit0 1=right, 0=left
  // wrap =  bit1 1=wrap
  // do_show = bit2 1=show
  FShiftType:=ord('R' = UpperCase(Direction)) or (ord(Wrap) << 1) or (ord(do_show) << 2);

  Result:=SendSysEx(chr(NEOPIXEL_DATA)+chr(NEOPIXEL_SHIFT_CONFIG)+chr(FDevice)+chr(FShiftType), write);

  if write then
  begin
    FShiftFirst:=0;
    FShiftLast:=FPixelsNumber - 1;
  end;
end;

{0  START_SYSEX      (0xF0)
1  NEOPIXEL_DATA    (0X51)
2  NEOPIXEL_SHIFT_RUN (0x09)
3  device            (0-3)
4  END_SYSEX        (0xF7)}
function TNeoPixel.ShiftRun(write: Boolean=True): string;
var
  savedColor: longword;
  i: smallint;
begin
  Result:=SendSysEx(chr(NEOPIXEL_DATA)+chr(NEOPIXEL_SHIFT_RUN)+chr(FDevice), write);

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
1  NEOPIXEL_DATA    (0X51)
2  NEOPIXEL_SHIFT_RUN (0x09)
3  device            (0-3)
4  shif_type         bit0 = 1 right 0=left, bit1 = 1 wrap,
                     bit2 = 1 do_show
4/5  END_SYSEX        (0xF7)}
function TNeoPixel.ShiftRun(ShiftType: Byte; write: Boolean=True): string; overload;
var
  savedColor: longword;
  i: smallint;
begin
  Result:=SendSysEx(chr(NEOPIXEL_DATA)+chr(NEOPIXEL_SHIFT_RUN)+chr(FDevice)+chr(ShiftType), write);

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
1  NEOPIXEL_DATA    (0X51)
2  NEOPIXEL_FADE_CONFIG (0x08)
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
    FBoard.RaiseError(53, 'NeoPixel:FadeSegmentConfig');
    exit;
  end;
  if FGamma <> 1 then
    tmpColor:=gamma32(Color, FGamma)
  else
    tmpColor:=Color;

  if First >= FPixelsNumber then
  begin
    FBoard.RaiseError(50, 'NeoPixel:FadeSegmentConfig');
    exit;
  end;
  if Last >= FPixelsNumber then
  begin
    FBoard.RaiseError(50, 'NeoPixel:FadeSegmentConfig');
    exit;
  end;

  if FFadeLoopsWait <> 1 then
    LoopsData:=chr(FFadeLoopsWait)
  else
    LoopsData:='';

  Result:=SendSysEx(chr(NEOPIXEL_DATA)+chr(NEOPIXEL_FADE_CONFIG)+chr(FDevice)+
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
1  NEOPIXEL_DATA    (0X51)
2  NEOPIXEL_FADE_CONFIG (0x08)
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
    FBoard.RaiseError(53, 'NeoPixel:FadeStripConfig');
    exit;
  end;

  if FGamma <> 1 then
    tmpColor:=gamma32(Color, FGamma)
  else
    tmpColor:=Color;

  if FfadeLoopsWait <> 1 then
    LoopsData:=chr(FFadeLoopsWait)
  else
    LoopsData:='';

  Result:=SendSysEx(chr(NEOPIXEL_DATA)+chr(NEOPIXEL_FADE_CONFIG)+chr(FDevice)+
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
1  NEOPIXEL_DATA    (0X51)
2  NEOPIXEL_FADE_ONE_STEP (0x0A)
3  device            (0-3)
4  do_show           optional (0-1) 1=show
4/5  END_SYSEX        (0xF7)}
function TNeoPixel.FadeOneStep(do_show: boolean=true; write: Boolean=True): string;
var
  i, j: smallint;
  mask: longword;
  currentColor, targetColor, calcColor: longword;
begin
  Result:='';
  if FFadeRunning then // fade is running
  begin
    FBoard.RaiseError(53, 'NeoPixel:FadeOneStep');
    exit;
  end;
  if do_show then
    Result:=SendSysEx(chr(NEOPIXEL_DATA)+chr(NEOPIXEL_FADE_ONE_STEP)+chr(FDevice)+chr(ord(do_show)), write)
  else
    Result:=SendSysEx(chr(NEOPIXEL_DATA)+chr(NEOPIXEL_FADE_ONE_STEP)+chr(FDevice), write);

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
1  NEOPIXEL_DATA    (0X51)
2  NEOPIXEL_FADE_RUN_PAUSE  (0x04)
3  device            (0-3)
4  run/pause        (0-1) 1 fade run, 0 fade pause
5  END_SYSEX        (0xF7)}
function TNeoPixel.FadeRunPause(write: Boolean=True): string;
begin
  Result:=SendSysEx(chr(NEOPIXEL_DATA)+chr(NEOPIXEL_FADE_RUN_PAUSE)+chr(FDevice)+chr(ord(FFadeRunning)), write);
end;

{0  START_SYSEX      (0xF0)
1  NEOPIXEL_DATA    (0X51)
2  NEOPIXEL_SET_BRIGHTNESS  (0x05)
3  device            (0-3)
4  brighness bits0-6       lsb brightness value 0-255
5  brightness bit7, bit8   msb do_show
6  END_SYSEX        (0xF7)}
function TNeoPixel.sendBrightness(do_show: Boolean; write: Boolean=true): string;
begin
  Result:=SendSysEx(chr(NEOPIXEL_DATA)+chr(NEOPIXEL_SET_BRIGHTNESS)+chr(FDevice)+
                    chr(FBrightness and $7F)+chr(((FBrightness >> 7) and $1) or (ord(do_show) << 1)), write);
end;

{0  START_SYSEX      (0xF0)
1  NEOPIXEL_DATA    (0X51)
2  NEOPIXEL_MOVE_PIXELS (0x0B)
3  device            (0-3)
4  src pixel         lsb
5                    msb
6  dest pixel        lsb
7                    msb
8  count             lsb (optional) // default to 1
9                    msb
10 do_show           optional (0-1) 1=show
8/11  END_SYSEX        (0xF7)}
function TNeoPixel.CopyPixels(src: smallint; dest: smallint; count: smallint; do_show: Boolean=true; write: Boolean=True): string;
var
  swap: smallint;
  Data: string;
begin
  Result:='';
  if src >= FPixelsNumber  then
  begin
    FBoard.RaiseError(50, 'NeoPixel:CopyPixels');
    exit;
  end;
  if dest >= FPixelsNumber  then
  begin
    FBoard.RaiseError(50, 'NeoPixel:CopyPixels');
    exit;
  end;
  if dest > src then
  begin
    swap:=src;
    src:=dest;
    dest:=swap;
  end;
  if ((src + count) > FPixelsNumber) or ((dest + count) > FPixelsNumber) then
  begin
    FBoard.RaiseError(50, 'CNeoPixel:opyPixels');
    exit;
  end;
  if count <> 1 then
    Data:=chr(count and $7F)+chr((count >> 7) and $7F)
  else
    Data:='';
  if do_show then
    data:=Data+chr(ord(do_show));

  Result:=SendSysEx(chr(NEOPIXEL_DATA)+chr(NEOPIXEL_MOVE_PIXELS)+chr(FDevice)+
                     chr(src and $7F)+chr((src >> 7) and $7F)+
                     chr(dest and $7F)+chr((dest >> 7) and $7F)+Data, write);
end;

end.
