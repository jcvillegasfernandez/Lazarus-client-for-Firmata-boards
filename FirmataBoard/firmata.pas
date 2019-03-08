{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Firmata;

{$warn 5023 off : no warning about unused units}
interface

uses
  firmataboard, FirmataConstants, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('firmataboard', @firmataboard.Register);
end;

initialization
  RegisterPackage('Firmata', @Register);
end.
