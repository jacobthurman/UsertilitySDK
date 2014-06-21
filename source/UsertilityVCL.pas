unit UsertilityVCL;

{$IF CompilerVersion >= 24}
{$LEGACYIFEND ON}
{$IFEND}

{$IF DEFINED(VER270) OR DEFINED(VER260)}
  {$DEFINE MULTIPLATFORM}
{$IFEND}

interface

uses Usertility, Types, Classes, Windows, Messages, Controls, Forms;

type
{$IFDEF MULTIPLATFORM}
[ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
TUsertility = class(TUsertilityBase)
public
protected
  procedure TrackWindowActivated(AHandle: THandle); reintroduce;
  procedure TrackControlFocused(AHandle: THandle); override;
end;

implementation

{ TUsertility }

procedure TUsertility.TrackControlFocused(AHandle: THandle);
var
  S: string;
  TargetControl: TControl;
begin
  S := 'ControlFocus|' + GetTimestamp;
  TargetControl := FindControl(AHandle);
  if (TargetControl <> nil) and (TargetControl <> FFocusedControl) then
  begin
    S := S + '|' + TargetControl.ClassName + '|' + TargetControl.Name;
    Log(S);
    FFocusedControl := TargetControl;
  end;
end;

procedure TUsertility.TrackWindowActivated(AHandle: THandle);
var
  S: string;
  TargetControl: TControl;
begin
  S := 'FormActivate|' + GetTimestamp;
  TargetControl := FindControl(AHandle);
  if (TargetControl <> nil) and (TargetControl <> FActiveForm) then
  begin
    S := S + '|' + TargetControl.ClassName + '|' + TargetControl.Name;
    Log(S);
    FActiveForm := TForm(TargetControl);
  end;
end;

end.
