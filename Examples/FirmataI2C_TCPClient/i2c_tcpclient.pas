unit i2c_TCPClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, ExtCtrls, firmataconstants, FirmataBoard, blcksock, synsock,
  LazSerial;

type
  TChip = record
    TypeID: Byte;
    DeviceID: Byte;
    Address_Size: Byte;
    Name: String;
    As_Sel: Byte;
    Size: integer;
    PageSize: Byte;
    Actual_Address: Integer;
  end;

  TBlockValues = record
    BlockNumber: integer;
    AddressInBlock: integer;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    I2C1: TI2C;
    Label5: TLabel;
    Label9: TLabel;
    Puerto: TEdit;
    WriteString: TButton;
    ComboBox1: TComboBox;
    EditA2A1A0: TEdit;
    Label7: TLabel;
    Label8: TLabel;
    Label_A2A1A0: TLabel;
    Address: TEdit;
    Label6: TLabel;
    NumBytes: TEdit;
    StringToWrite: TEdit;
    ReadData: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Board1: TBoard;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Server: TEdit;
    OpenPort: TButton;
    ClosePort: TButton;

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure WriteStringClick(Sender: TObject);
    procedure ComboBox1CloseUp(Sender: TObject);
    procedure EditA2A1A0EditingDone(Sender: TObject);
    procedure Board1AfterClose(sender: TObject);
    procedure Board1BeforeOpen(sender: TObject);
    function Board1DeviceDataAvailable(sender: TObject): Boolean;
    procedure Board1Error(sender: TObject; Error: integer; TextError: string; Afected: integer);
    procedure Board1FirmataData(sender: TObject; Command: Byte; Data: string);
    procedure Board1FirmataReady(sender: TObject);
    function Board1GetDataFromDevice(sender: TObject): integer;
    procedure I2C1I2CData(sender: TObject; Slave: Byte; Reg_Number: Byte; Data: string);
    procedure Board1SendDataToDevice(sender: TObject; str: string);
    procedure FormCreate(Sender: TObject);
    procedure Memo1Click(Sender: TObject);
    procedure NumBytesEditingDone(Sender: TObject);
    procedure OpenPortClick(Sender: TObject);
    procedure ClosePortClick(Sender: TObject);
    procedure ReadDataClick(Sender: TObject);
    procedure AsignValuesToChip(var Chip: TChip; TypeID: Byte; As_Sel: Byte);
    procedure AddressEditingDone(Sender: TObject);
    function SetAddress(Chip: TChip): TBlocKValues; // integer 0 is BlockNumber, integer 1 is address in Block
    procedure WriteBytesToDevice(Chip: TChip; Data: String; Display: Boolean);
    procedure ReadBytesFromDevice(Chip: TChip; BytesToRead: integer);
    function BinToInt(Value: string): Integer;
    function StringToMemo(Data: string): string;
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
  Chip: TChip;
  BlockValue: TBlockValues;
  DataString: string;
  FBytesLeft: integer;

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
  //client.OnMonitor:=@MonitorSocket;

  // Enable Firmata
  Board1.Enabled:=true;

  Server.Enabled:=false;
  closeport.Enabled:=True;
  puerto.Enabled:=false;
  Openport.Enabled:=False;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  memo1.Enabled:=true;
  Memo1.Clear;

  Puerto.Text:='3030';
  Server.Text:='192.168.10.11';

  ComboBox1.ItemIndex:=0;
  ComboBox1CloseUp(ComboBox1);
  //AsignValuesToChip(Chip, ComboBox1.ItemIndex, BinToInt(EditA2A1A0.Text);

  FBytesLeft:=0;
end;

procedure TForm1.Board1AfterClose(sender: TObject);
begin
  client.CloseSocket;
end;

procedure TForm1.AsignValuesToChip(var Chip: TChip; TypeID: Byte; As_Sel: Byte);
begin
     Chip.TypeID:=TypeID;
     Chip.Actual_Address:=0;
     Chip.DeviceID:=%01010000; // 7 bits, no read write bit

     case TypeID of
        0: begin
             Chip.Name:='2401';
             Chip.Size:=128;
             Chip.PageSize:=8;
             Chip.As_Sel:=As_Sel and %111;  // values of A0, A1, A2
             Chip.Address_Size:=1;
        end;
        1: begin
             Chip.Name:='2402';
             Chip.Size:=256;
             Chip.PageSize:=8;
             Chip.As_Sel:=As_Sel and %111;  // values of A0, A1, A2
             Chip.Address_Size:=1;
        end;
        2: begin
             Chip.Name:='2404';
             Chip.Size:=512;
             Chip.PageSize:=16;
             Chip.As_Sel:=(As_Sel and %11) << 1;  // values of A1, A2
             Chip.Address_Size:=1;
        end;
        3: begin
             Chip.Name:='2408';
             Chip.Size:=1024;
             Chip.PageSize:=16;
             Chip.As_Sel:=(As_Sel and %1) << 2;  // value of A2
             Chip.Address_Size:=1;
        end;
        4: begin
             Chip.Name:='2416';
             Chip.Size:=2048;
             Chip.PageSize:=16;
             Chip.As_Sel:=0;  // Not exist
             Chip.Address_Size:=1;
        end;
        5: begin
             Chip.Name:='2432';
             Chip.Size:=4096;
             Chip.PageSize:=32;
             Chip.As_Sel:=As_Sel and %111;  // values of A0, A1, A2
             Chip.Address_Size:=2;
        end;
        6: begin
             Chip.Name:='2464';
             Chip.Size:=8192;
             Chip.PageSize:=32;
             Chip.As_Sel:=As_Sel and %111;  // values of A0, A1, A2
             Chip.Address_Size:=2;
        end;
        7: begin
             Chip.Name:='24128';
             Chip.Size:=16384;
             Chip.PageSize:=64;
             Chip.As_Sel:=As_Sel and %011;  // values of A0, A1
             Chip.Address_Size:=2;
        end;
        8: begin
             Chip.Name:='24256';
             Chip.Size:=32768;
             Chip.PageSize:=64;
             Chip.As_Sel:=As_Sel and %011;  // values of A0, A1
             Chip.Address_Size:=2;
        end;
     end;
     Chip.DeviceID:=Chip.DeviceID or Chip.As_Sel;
end;

procedure TForm1.Board1SendDataToDevice(sender: TObject; str: string);
begin
  client.SendString(str);
end;

function TForm1.Board1GetDataFromDevice(sender: TObject): integer;
begin
  if client.CanReadEx(100) then
    Result:=client.RecvByte(0)
  else
    Result:=-1;
end;

function TForm1.Board1DeviceDataAvailable(sender: TObject): Boolean;
begin
  Result:=client.CanReadEx(0);
end;

procedure TForm1.ClosePortClick(Sender: TObject);
begin
  Server.Enabled:=true;
  closeport.Enabled:=False;
  Openport.Enabled:=True;
  puerto.Enabled:=True;

  I2C1.Enabled:=False;
  Board1.enabled:=false;

  ReadData.enabled:=False;
  memo1.Enabled:=false;
  memo1.Clear;
  Address.Enabled:=False;
  NumBytes.Enabled:=False;
  WriteString.Enabled:=False;
  StringToWrite.Enabled:=False;
end;
 
procedure TForm1.WriteStringClick(Sender: TObject);
begin
  if StringToWrite.Text = '' then
    exit;
  // slow method write max of 8 bytes, start addres of writting is Chip.Actual_Address
  Chip.Actual_Address:=strtoint(Address.Text);
  WriteBytesToDevice(Chip, StringToWrite.Text, True);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    ClosePortClick(self);
end;

procedure TForm1.ReadDataClick(Sender: TObject);
begin
  FBytesLeft:=StrToInt(NumBytes.Text);
  if FBytesLeft <= 0 then
    exit;

  DataString:='';  // global variable needed because we read by an event;

  // start address of reading is Chip.Actual_Address
  Chip.Actual_Address:=strtoint(Address.Text);
  ReadBytesFromDevice(Chip, FBytesLeft);
end;

procedure TForm1.I2C1I2CData(sender: TObject; Slave: Byte; Reg_Number: Byte; Data: string);
var
  StrData: string;
  i: integer;
begin
  DataString:=DataString+Data;
  FBytesLeft:=FBytesLeft - Length(Data);
  if FBytesLeft > 0 then
    exit;  // haven't been read all bytes yet

  StrData:='';
  Memo1.Lines.Add('I2C, Slave ('+inttoStr(Slave)+') Data('+IntToStr(Length(DataString) - FBytesLeft)+')='+StrToHexSep(DataString));
  For i:=1 to Length(DataString) do
  begin
    if (DataString[i] >= ' ') and (DataString[i] <= '~') then
      StrData:=StrData+DataString[i]
    else
      StrData:=StrData+' ';
  end;
  Memo1.Lines.Add('I2C, String('+inttoStr(i)+')='+StrData);

end;

// slow method write max of 8 bytes, start addres of writting is Chip.Actual_Address
procedure TForm1.WriteBytesToDevice(Chip: TChip; Data: String; Display: Boolean);
var
  Full_Pages: integer;  // number of full 8, 16 or 64 bytes pages
  First_Page: integer;  // Bytes to write in first incomplete page 1-PageSize (1-63 bytes, 1-15 bytes, 1-8 bytes)
  Last_Page: integer;  // Bytes to write in last incomplete page 1-PageSize (1-63 bytes, 1-15 bytes, 1-8 bytes)
  i: integer;
  DataIndex: integer;
  BytesToWrite: integer;
  PageSize: integer;
begin
  if Chip.PageSize > 16 then
    PageSize:=16
  else
    PageSize:=Chip.PageSize;
  // set page size to 16 bytes max instead Chip.pagesize to avoid to run out of firmata
  // max bytes to write, from actual address until chipsize or Data length
  if (Chip.Actual_Address+Length(Data)) > Chip.Size then  // Start with address 0 to ChipSize-1
    BytesToWrite:=Chip.Size-Chip.Actual_Address-Length(Data)
  else
    BytesToWrite:=Length(Data);

  // Continuous writing is much faster than writting a byte only, max is page 8, 16 or 64 bytes,
  // Carefull, if the end of page is reached, the next byte to write is the first byte in this page
  // not the first byte in the next page

  // So compute new start page of PageSize
  First_Page:=Chip.Actual_Address mod PageSize; // position for byte to write in first incomplete page
  // so we have to write (PageSize - position) bytes in the first incomplete page
  First_Page:=PageSize-First_Page;  // First_Page are now bytes to write in first incomplete page

  if First_Page >= BytesToWrite then
  begin
    First_Page:=BytesToWrite;
    Full_Pages:=0;
    Last_Page:=0;
  end
  else  // more bytes than first incomplete page
  begin
    //Full_Pages:=(BytesToWrite-First_Page) div Chip.PageSize;  // number of full pages to write
    Full_Pages:=(BytesToWrite-First_Page) div PageSize;
    //Last_Page:=(BytesToWrite-First_Page) mod Chip.PageSize;   // bytes to write on last incomplete page if any
    Last_Page:=(BytesToWrite-First_Page) mod PageSize;
  end;

  // Calculate BlockNumber and address in block
  BlockValue:=SetAddress(Chip);
  DataIndex:=1;  // Position for first next byte to write in Data

  //write first page
  I2C1.WriteData(Chip.DeviceID or BlockValue.BlockNumber, BlockValue.AddressInBlock, Chip.Address_Size, Copy(Data, DataIndex, First_Page));   // blocknumber is 0  if chipsize >2048
  sleep(15);   // wait 15 miliseconds to write a page


  // Write first incomplete page, First_Page bytes from Start
  if Display then
      Memo1.Lines.Add('Written on first page: '+Copy(Data, DataIndex, First_Page));

  Inc(Chip.Actual_Address, First_Page); // new actual address

  // new data index
  Inc(DataIndex, First_Page);

  // write full pages
  for i:=1 to Full_Pages do
  begin
    // Calculate BlockNumber and address in block of this page
    BlockValue:=SetAddress(Chip);

    // write page of PageSize bytes
    I2C1.WriteData(Chip.DeviceID or BlockValue.BlockNumber, BlockValue.AddressInBlock, Chip.Address_Size, Copy(Data, DataIndex, Pagesize));   // write page
    sleep(15);

    if Display then
        Memo1.Lines.Add('Written on page '+IntToStr(i)+' : '+Copy(Data, DataIndex, PageSize));

    inc(Chip.Actual_Address, PageSize); // new actual address
    // new data index

    Inc(DataIndex, PageSize);
  end;

  // write last incomplete page
  if Last_Page > 0 then
  begin
    // Calculate BlockNumber and address in block of this page
    BlockValue:=SetAddress(Chip);

    I2C1.WriteData(Chip.DeviceID or BlockValue.BlockNumber, BlockValue.AddressInBlock, Chip.Address_Size, Copy(Data, DataIndex, Last_Page));   // write page

    if Display then
        Memo1.Lines.Add('Written on last page: '+Copy(Data, DataIndex, Last_Page));

    Chip.Actual_Address:=Chip.Actual_Address+Last_Page; // new actual address
  end;
end;

function TForm1.SetAddress(Chip: TChip): TBlocKValues; // integer 0 is BlockNumber, integer 1 is address in Block
begin
  // 2401    128 bytes
  //      Pin3=A2, Pin2=A1, Pin1=A0
  // bit 7 not used
  // BlockNumber=0

  // 2402    256 bytes
  //      Pin3=A2, Pin2=A1, Pin1=A0
  // BlockNumber=0

  // 2404    512 bytes
  //      Pin3=A2, Pin2=A1
  // BlockNumber=0 or 1

  // 2408   1024 bytes
  //      Pin3=A2
  // BlockNumber=0 to 3

  // 2416   2048 bytes
  // BlockNumber=0 to 8

  // 24128   16384 bytes
  // BlockNumber=0

  // 24256   32768 bytes
  // BlockNumber=0

  //BlockNumber:=MemAddress div 256;  // 256 bytes block number where to write
  //AddressInBlock:=MemAddress mod 256; // AddressInBlock (0-255) to write inside the block

  // 24XX
  // Bits 7, 6, 5, 4 Device ID 1010, Device=160 (1010 000 0=$A0=80)
  // Bits 3, 2, 1 = BlockNumber or A2 A1 A0 ID Select

  // 2432 2464
  // Bits 7, 6, 5, 4 Device ID 1010, Device=160 (1010 000 0=$A0=160)
  // Bits 3, 2, 1 = A2 A1 A0 ID Select

  // 24XXX
  // Bits 7, 6, 5, 4, 3 Device ID 10100, Device=160 (10100 00 0=$A0=160)
  // Bits 2, 1 = A1 A0 ID Select

  // Bit 0 1=R, 0=W
  Result.AddressInBlock:=Chip.Actual_Address and (Chip.Size-1);
  if Chip.Size > 2048 then  // 24XXX
  begin
    Result.BlockNumber:=0
  end
  else  // 24XX
  begin
    Result.BlockNumber:=((Result.AddressInBlock >> 8) and $FF) shl 1;   // get block number, will be 0 for 2401 and 2402
    Result.AddressInBlock:=(Result.AddressInBlock mod 256) and $FF;   // get address inside a block
  end;
  Result.BlockNumber:=Result.BlockNumber or Chip.As_Sel;  // hardware select
end;

// Address to read is last Address accesed, is Chip.Actual_Address
procedure TForm1.ReadBytesFromDevice(Chip: TChip; BytesToRead: integer);
var
  i: integer;
  readbytes: integer;
  slice: byte;
begin
  slice:=28; // max continuous bytes read because of firmta
  readbytes:=0;
  for i:=1 to BytesToRead div slice do
  begin
    // Calculate BlockNumber and address in block
    BlockValue:=SetAddress(Chip);

    I2C1.WriteData(Chip.DeviceID or BlockValue.BlockNumber, BlockValue.AddressInBlock, Chip.Address_Size,''); // select address on device
    I2C1.Read(Chip.DeviceID or BlockValue.BlockNumber, -1, slice, False);

    inc(Chip.Actual_Address, slice);
    inc(readbytes, slice);
  end;
  i:=BytesToRead - readBytes;
  if i > 0 then
  begin
    BlockValue:=SetAddress(Chip);
    I2C1.WriteData(Chip.DeviceID or BlockValue.BlockNumber, BlockValue.AddressInBlock, Chip.Address_Size,''); // select address on device
    I2C1.Read(Chip.DeviceID or BlockValue.BlockNumber, -1, i, False);
    inc(Chip.Actual_Address, i);
  end;

end;

function Tform1.StringToMemo(Data: string): string;
var
  i: integer;
begin
  Result:='';
  for i:=1 to length(Data) do
    Result:=Result+IntTostr(ord(Data[i]))+', ';
end;

procedure TForm1.ComboBox1CloseUp(Sender: TObject);
begin
  case TComboBox(Sender).ItemIndex of
     0: begin  // 2401
          Label_A2A1A0.Caption:='A2 A1 A0:';
          EditA2A1A0.Maxlength:=3;
          EditA2A1A0.Visible:=True;
          EditA2A1A0.Text:='000';
        end;
     1: begin  // 2402
          Label_A2A1A0.Caption:='A2 A1 A0:';
          EditA2A1A0.Maxlength:=3;
          EditA2A1A0.Visible:=True;
          EditA2A1A0.Text:='000';
        end;
     2: begin   // 2404
          Label_A2A1A0.Caption:='A2 A1:';
          EditA2A1A0.Maxlength:=2;
          EditA2A1A0.Visible:=True;
          EditA2A1A0.Text:='00';
        end;
     3: begin  // 2408
          Label_A2A1A0.Caption:='A2:';
          EditA2A1A0.MaxLength:=1;
          EditA2A1A0.Visible:=True;
          EditA2A1A0.Text:='0';
        end;
     4: begin   // 2416
          Label_A2A1A0.Caption:='';
          EditA2A1A0.Visible:=False;
          EditA2A1A0.Maxlength:=0;
          EditA2A1A0.Text:='';
        end;
     5: begin   // 2432
           Label_A2A1A0.Caption:='A2 A1 A0:';
           EditA2A1A0.Visible:=True;
           EditA2A1A0.Maxlength:=3;
           EditA2A1A0.Text:='000';
         end;
     6: begin   // 2464
          Label_A2A1A0.Caption:='A2 A1 A0:';
          EditA2A1A0.Visible:=True;
          EditA2A1A0.Maxlength:=3;
          EditA2A1A0.Text:='000';
        end;
     7: begin   // 24128
          Label_A2A1A0.Caption:='A1 A0:';
          EditA2A1A0.Maxlength:=2;
          EditA2A1A0.Visible:=True;
          EditA2A1A0.Text:='00';
        end;
     8: begin   // 24256
          Label_A2A1A0.Caption:='A1 A0:';
          EditA2A1A0.Maxlength:=2;
          EditA2A1A0.Visible:=True;
          EditA2A1A0.Text:='00';
        end;
  end;
  AsignValuesToChip(Chip, TComboBox(Sender).ItemIndex, BinToInt(EditA2A1A0.Text));
end;

function TForm1.BinToInt(Value: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(Value) do
  begin
    Result:=Result shl 1;
    if Value[i] = '1' then
      Inc(Result);
  end;
end;
procedure TForm1.EditA2A1A0EditingDone(Sender: TObject);
var
  i: integer;
  s: string;
begin
  s:=LeftStr(TEdit(Sender).Text+'000', TEdit(Sender).MaxLength);
  for i := 1 to TEdit(Sender).MaxLength do
  begin
    if Pos(s[i],'01') = 0 then
      s[i]:='0';
  end;
  TEdit(Sender).Text:=s;
  Chip.As_Sel:=BinToInt(s);
end;

procedure TForm1.Board1BeforeOpen(sender: TObject);
begin
  // Open way of comunication
  memo1.Append('Wait !!!, Firmata starting....');
  Client.Connect(server.text, puerto.text);

  if client.LastError <> 0 then
  begin
    Board1.Enabled:=False;
    board1.RaiseError(1002,'Could not open connection');
  end;

  Board1SendDataToDevice(self,chr($F9)); // askversion
  Board1SendDataToDevice(self,chr($F0)+chr($79)+chr($F7)); // askFirmware
end;

procedure TForm1.Board1Error(sender: TObject; Error: integer;
  TextError: string; Afected: integer);
begin
     ShowMessage(textError);
end;

procedure TForm1.Board1FirmataData(sender: TObject; Command: Byte; Data: string);
begin
  memo1.lines.Add('Data from firmata: '+Data);
end;

procedure TForm1.Board1FirmataReady(sender: TObject);
begin
  memo1.clear;
  memo1.lines.add('Firmata started in, '+inttostr(Board1.StartingTime)+' milisec');
  memo1.lines.add('Firmata Firmare:' + Board1.FirmataFirmware);

  Board1.printPinInfo(Memo1);
  I2C1.SDApin:=4;
  I2C1.SCLpin:=5;
  I2C1.Enabled:=true;  // prepare i2c and config pins
  if not I2C1.Enabled then
  begin
    memo1.Lines.add('');
    memo1.Lines.add('I2C module is not installed in ConfigurableFirmata');
    exit;
  end;
  Address.Enabled:=True;
  NumBytes.Enabled:=True;
  ReadData.Enabled:=True;
  WriteString.Enabled:=True;
  StringToWrite.Enabled:=True;

  openPort.Enabled:=false;
  Server.Enabled:=false;
  Closeport.Enabled:=true;
end;

procedure TForm1.Memo1Click(Sender: TObject);
begin
  If Board1.Enabled then
  begin
    Memo1.Clear;
    Board1.printPinInfo(Memo1);
  end;
end;

procedure TForm1.AddressEditingDone(Sender: TObject);
var
  i: integer;
begin
  i:=StrToIntDef(TEdit(Sender).Text, -2);
  if (i < -1) or (i > (Chip.Size-1)) then
    i:=0;
  TEdit(Sender).Text:=IntToStr(i);

end;

procedure TForm1.NumBytesEditingDone(Sender: TObject);
var
  i: integer;
begin
  i:=StrToIntDef(TEdit(Sender).Text, -1);
  if (i < 0) or (i > (Chip.Size-1-Strtoint(Address.Text))) then
    i:=0;

  if (Length(TEdit(Sender).Text) > 0) then
  begin
    if TEdit(Sender).Text[1] = '$' then // hex format
      TEdit(Sender).Text:='$'+IntToHex(i, 2)
    else
      TEdit(Sender).Text:=IntToStr(i);
  end;
end;


end.

