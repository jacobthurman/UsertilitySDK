unit UsertilityInstall;

interface

uses Usertility, Classes;

procedure Register;

implementation

{$R Usertility.dcr}

procedure Register;
begin
  RegisterComponents('Usertility', [TUsertility]);
end;

end.
