unit fill_with_color;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFill_with_Color1 }

  TFill_with_Color1 = class(TForm)
    Blue: TEdit;
    ShowPixels: TCheckBox;
    fill: TButton;
    Green: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label14: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Pixelend: TEdit;
    pixelfirst: TEdit;
    RadioSegment: TRadioButton;
    RadioStrip: TRadioButton;
    Red: TEdit;
    White: TEdit;
    procedure fillClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure pixelfirstEditingDone(Sender: TObject);
    procedure RadioStripChange(Sender: TObject);
    procedure RedEditingDone(Sender: TObject);
    procedure ShowPixelsChange(Sender: TObject);
  private

  public

  end;

var
  fill_with_color1: TFill_with_Color1;

implementation

{$R *.lfm}
uses
  firmataneopixel_tcpclient;
{ TFill_with_Color1 }


procedure TFill_with_Color1.RadioStripChange(Sender: TObject);
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
    pixelend.Visible:=true;
  end;
end;

procedure TFill_with_Color1.pixelfirstEditingDone(Sender: TObject);
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

procedure TFill_with_Color1.FormActivate(Sender: TObject);
begin
  if Pos('W', Uppercase(FormNeo1.NeoPixel1.LedColors)) > 0 then
  begin
    White.Visible:=True;
    Label11.Visible:=True;
  end
  else
  begin
    White.Visible:=false;
    Label11.Visible:=false;
  end;
  if PixelEnd.Text = '0' then
    PixelEnd.Text:=inttostr(FormNeo1.NeoPixel1.PixelsNumber - 1);
end;


procedure TFill_with_Color1.fillClick(Sender: TObject);
begin
  if RadioStrip.Checked then // fill strip
  begin
    FormNeo1.neopixel1.FillStrip(Strtoint(Red.Text), Strtoint(Green.Text), Strtoint(Blue.Text), Strtoint(White.Text), ShowPixels.checked);  // fill strip and ShowPixels
  end
  else // fill segment
  begin
    FormNeo1.neopixel1.FillSegment(Strtoint(Red.Text), Strtoint(Green.Text), Strtoint(Blue.Text), Strtoint(White.Text),     // fill segment and ShowPixels
                        strtoint(pixelfirst.text), strtoint(pixelend.text), ShowPixels.checked);
  end;
  Close;
end;

procedure TFill_with_Color1.ShowPixelsChange(Sender: TObject);
begin
  if ShowPixels.checked then
    ShowPixels.caption:='Show pixels'
  else
    ShowPixels.caption:='Not show';
end;

procedure TFill_with_Color1.RedEditingDone(Sender: TObject);
var
  Valid: Boolean;
  Value: integer;
begin
  Valid:=False;
  Try
    Value:=StrToInt(TEdit(Sender).Text);
    Valid:=True;
    if (Value < 0) or (value > 255) then
     Valid:=false;
  finally
    if not Valid then
    begin
      Showmessage(Tedit(Sender).Text+' is not a valid value');
      TEdit(Sender).Text:='0';
    end;
  end;
end;


end.

