unit shift_config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TShift_Config1 }

  TShift_Config1= class(TForm)
    ShiftConfig: TButton;
    Label12: TLabel;
    Label14: TLabel;
    Label6: TLabel;
    Pixelend: TEdit;
    pixelfirst: TEdit;
    RadioSegment: TRadioButton;
    RadioStrip: TRadioButton;
    ShowPixel: TCheckBox;
    wrap: TCheckBox;
    procedure FormActivate(Sender: TObject);
    procedure pixelfirstEditingDone(Sender: TObject);
    procedure RadioStripChange(Sender: TObject);
    procedure ShiftConfigClick(Sender: TObject);
    procedure ShowPixelChange(Sender: TObject);
    procedure wrapChange(Sender: TObject);
  private

  public

  end;

var
  Shift_Config1: TShift_Config1;

implementation

uses
  firmataneopixel_tcpclient;

{$R *.lfm}

{ TShift_Config1}

procedure TShift_Config1.pixelfirstEditingDone(Sender: TObject);
var
  Valid: Boolean;
  Value: integer;
begin
  Valid:=False;
  Try
    Value:=StrToInt(TEdit(Sender).Text);
    Valid:=True;
    if (Value < 0) or (value > 255) or (Value >= FormNeo1.Neopixel1.pixelsNumber) then
      Valid:=false;
    if strtoint(pixelfirst.text) > strtoint(pixelend.text) then
      valid:=false;
  finally
    if not Valid then
    begin
      Showmessage(Tedit(Sender).Text+' is not a valid value');
      TEdit(Sender).Text:='0';
    end
  end;
end;

procedure TShift_Config1.RadioStripChange(Sender: TObject);
begin
  if RadioStrip.Checked then
  begin
    label12.Visible:=False;
    label14.Visible:=False;
    pixelfirst.Visible:=False;
    pixelend.Visible:=False;
  end
  else
  begin
    label12.Visible:=true;
    label14.Visible:=true;
    pixelfirst.Visible:=true;
    pixelend.Visible:=true;  // Shift_Config1 config
  end;
end;

procedure TShift_Config1.FormActivate(Sender: TObject);
begin
  if PixelEnd.Text = '0' then
    PixelEnd.Text:=inttostr(FormNeo1.NeoPixel1.PixelsNumber - 1);
end;

procedure TShift_Config1.ShowPixelChange(Sender: TObject);
begin
  if ShowPixel.checked then
    ShowPixel.caption:='Show pixels'
  else
    ShowPixel.caption:='Not show';
end;

procedure TShift_Config1.ShiftConfigClick(Sender: TObject);
begin
  if RadioStrip.Checked then
    FormNeo1.neopixel1.ShiftStripConfig('R', wrap.Checked, ShowPixel.Checked)  // shift config
  else
    FormNeo1.neopixel1.ShiftSegmentConfig(strtoint(pixelfirst.text), strtoint(pixelend.text), 'R', wrap.Checked, ShowPixel.Checked);  // shift config
  Close;
end;

procedure TShift_Config1.wrapChange(Sender: TObject);
begin
  if wrap.checked then
    wrap.caption:='Wrap on'
  else
    wrap.caption:='Wrap off';
end;

end.

