program FirmataNeoPixels_TCPClient;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, firmataneopixel_tcpclient, firmataboard, fill_with_color, fade_config,
  Pixel_Color, shift_config, copy_pixels;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TFormNeo1, FormNeo1);
  Application.CreateForm(TPixel_color1, Pixel_color1);
  Application.CreateForm(TFill_with_Color1, Fill_with_Color1);
  Application.CreateForm(TShift_config1, Shift_config1);
  Application.CreateForm(TFade_config1, Fade_config1);
  Application.CreateForm(TCopy_Pixels1, Copy_Pixels1);
  Application.Run;
end.

