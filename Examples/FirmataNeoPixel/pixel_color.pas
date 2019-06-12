unit pixel_color;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TPixel_color1 }

  TPixel_color1 = class(TForm)
    Blue: TEdit;
    gamma: TEdit;
    Green: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    pixelnumber: TEdit;
    Red: TEdit;
    setpixelcolor: TButton;
    ShowPixel: TCheckBox;
    White: TEdit;
    procedure FormActivate(Sender: TObject);
    procedure pixelnumberEditingDone(Sender: TObject);
    procedure RedEditingDone(Sender: TObject);
    procedure setpixelcolorClick(Sender: TObject);
    procedure ShowPixelChange(Sender: TObject);
  private

  public

  end;

var
  Pixel_color1: TPixel_color1;

implementation

uses
  firmataneopixel;

{$R *.lfm}

{ TPixel_color1 }
procedure TPixel_color1.pixelnumberEditingDone(Sender: TObject);
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
  finally
    if not Valid then
    begin
      Showmessage(Tedit(Sender).Text+' is not a valid value');
      TEdit(Sender).Text:='0';
    end
  end;
end;

procedure TPixel_color1.FormActivate(Sender: TObject);
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
end;

procedure TPixel_color1.setpixelcolorClick(Sender: TObject);
begin
  form1.NeoPixel1.PixelColor(strtoint(pixelnumber.text), Strtoint(Red.Text), Strtoint(Green.Text), Strtoint(Blue.Text), Strtoint(white.Text), ShowPixel.checked, true);
  Close;
end;

procedure TPixel_color1.ShowPixelChange(Sender: TObject);
begin
  if ShowPixel.checked then
    ShowPixel.caption:='Show pixels'
  else
    ShowPixel.caption:='Not show';
end;

procedure TPixel_color1.RedEditingDone(Sender: TObject);
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

