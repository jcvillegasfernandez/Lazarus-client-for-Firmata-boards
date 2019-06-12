unit copy_pixels;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TCopy_Pixels1}

  TCopy_Pixels1= class(TForm)
    Label15: TLabel;
    CopyBut: TButton;
    Label12: TLabel;
    Label14: TLabel;
    Label6: TLabel;
    Pixeldest: TEdit;
    Pixelcount: TEdit;
    pixelsrc: TEdit;
    ShowPixel: TCheckBox;
    procedure FormActivate(Sender: TObject);
    procedure PixelcountEditingDone(Sender: TObject);
    procedure PixeldestEditingDone(Sender: TObject);
    procedure pixelsrcEditingDone(Sender: TObject);
    procedure CopyClick(Sender: TObject);
    procedure ShowPixelChange(Sender: TObject);
  private

  public

  end;

var
  Copy_Pixels1: TCopy_Pixels1;

implementation

uses
  firmataneopixel;

{$R *.lfm}

{ TCopy_Pixels1}

procedure TCopy_Pixels1.pixelsrcEditingDone(Sender: TObject);
var
  Valid: Boolean;
  Value: integer;
begin
  Valid:=False;
  Try
    Value:=StrToInt(TEdit(Sender).Text);
    Valid:=True;
    if (Value < 0) or (Value >= Form1.Neopixel1.pixelsNumber) then
      Valid:=false;
  finally
    if not Valid then
    begin
      Showmessage(Tedit(Sender).Text+' is not a valid value');
      TEdit(Sender).Text:='0';
    end
  end;
end;

procedure TCopy_Pixels1.FormActivate(Sender: TObject);
begin
  if Pixeldest.Text = '0' then
    Pixeldest.Text:='1';
  if Pixelcount.Text = '0' then
    Pixelcount.Text:='1';
end;

procedure TCopy_Pixels1.PixelcountEditingDone(Sender: TObject);
var
  Valid: Boolean;
  Value: integer;
begin
  Valid:=False;
  Try
    Value:=StrToInt(TEdit(Sender).Text);
    Valid:=True;
    if (Value < 0) or (Value > Form1.Neopixel1.pixelsNumber) then
      Valid:=false;
    if (strtoint(pixelsrc.text) + Value > Form1.Neopixel1.pixelsNumber) then
      valid:=false;
    if (strtoint(pixeldest.text) + Value > Form1.Neopixel1.pixelsNumber) then
      valid:=false;
  finally
    if not Valid then
    begin
      Showmessage(Tedit(Sender).Text+' is not a valid value');
      TEdit(Sender).Text:='1';
    end
  end;
end;

procedure TCopy_Pixels1.PixeldestEditingDone(Sender: TObject);
var
  Valid: Boolean;
  Value: integer;
begin
  Valid:=False;
  Try
    Value:=StrToInt(TEdit(Sender).Text);
    Valid:=True;
    if (Value < 0) or (Value >= Form1.Neopixel1.pixelsNumber) then
      Valid:=false;
    if strtoint(pixelsrc.text) > strtoint(Pixeldest.text) then
      valid:=false;
  finally
    if not Valid then
    begin
      Showmessage(Tedit(Sender).Text+' is not a valid value');
      TEdit(Sender).Text:='0';
    end
  end;

end;

procedure TCopy_Pixels1.ShowPixelChange(Sender: TObject);
begin
  if ShowPixel.checked then
    ShowPixel.caption:='Show pixels'
  else
    ShowPixel.caption:='Not show';
end;

procedure TCopy_Pixels1.CopyClick(Sender: TObject);
begin
  form1.neopixel1.CopyPixels(strtoint(pixelsrc.text), strtoint(Pixeldest.text), strtoint(Pixelcount.text), ShowPixel.Checked);  // shift config
  Close;
end;

end.

