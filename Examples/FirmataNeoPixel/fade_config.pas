unit fade_config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFade_config1 }

  TFade_config1 = class(TForm)
    Blue: TEdit;
    config: TButton;
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
    procedure configClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure pixelfirstEditingDone(Sender: TObject);
    procedure RadioStripChange(Sender: TObject);
    procedure RedEditingDone(Sender: TObject);
  private

  public

  end;

var
  Fade_config1: TFade_config1;

implementation

uses
  firmataneopixel;

{$R *.lfm}

procedure TFade_config1.RadioStripChange(Sender: TObject);
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

procedure TFade_config1.pixelfirstEditingDone(Sender: TObject);
var
  Valid: Boolean;
  Value: integer;
begin
  Valid:=False;
  Try
    Value:=StrToInt(TEdit(Sender).Text);
    Valid:=True;
    if (Value < 0) or (value > 255) or (Value >= form1.Neopixel1.pixelsNumber) then
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

procedure TFade_config1.FormActivate(Sender: TObject);
begin
  if Pos('W', Uppercase(form1.NeoPixel1.LedColors)) > 0 then
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
    PixelEnd.Text:=inttostr(form1.NeoPixel1.PixelsNumber - 1);
end;

procedure TFade_config1.configClick(Sender: TObject);
begin
  if RadioStrip.Checked then // fill strip
  begin
    form1.NeoPixel1.FadeStripConfig(Strtoint(Red.Text), Strtoint(Green.Text), Strtoint(Blue.Text), Strtoint(White.Text));
  end
  else // fill segment
  begin
    form1.NeoPixel1.FadeSegmentConfig(Strtoint(Red.Text), Strtoint(Green.Text), Strtoint(Blue.Text), Strtoint(White.Text),
                                strtoint(pixelfirst.text), strtoint(pixelend.text));
  end;
  Close;
end;

procedure TFade_config1.RedEditingDone(Sender: TObject);
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

