unit UsertilityInstall;

{$IF CompilerVersion >= 24}
{$LEGACYIFEND ON}
{$IFEND}

//{$IF DEFINED(VER270) OR DEFINED(VER260)}
//  {$DEFINE FMX}
//{$ELSE}
  {$UNDEF FMX}
//{$IFEND}

interface

uses {$IF DEFINED(FMX)}UsertilityFMX,{$IFEND}
 UsertilityVCL, Classes, Types;

procedure Register;

implementation

{$IF DEFINED(FMX)}
uses FMX.Types;
{$IFEND}

{$R Usertility.dcr}

procedure Register;
begin
  {$IF DEFINED(FMX)}
  RegisterComponents('Usertility', [UsertilityFMX.TUsertility]);
  {$IFEND}
  RegisterComponents('Usertility', [UsertilityVCL.TUsertility]);
end;

initialization
  {$IF DEFINED(FMX)}
  FMX.Types.RegisterFMXClasses([UsertilityFMX.TUsertility]);
  {$IFEND}

end.
