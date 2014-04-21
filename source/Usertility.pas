unit Usertility;

{$DEFINE VCL}

interface

uses Types, Classes, Windows, Messages, SysUtils, Math,
Controls, Forms, ExtCtrls;

const

//Don't change this message in source code. Use the PrivacyMessage property of
//the ApplicationAnalytics component instead
sPrivacyMessage = 'Privacy Notice:'#13#10#13#10 +
'This application anonymously tracks your usage and sends it to us for ' +
'analysis. We use this analysis to make the ' +
'software work better for you.'#13#10#13#10 +
'This tracking is completely anonymous. No personally identifying information ' +
'is tracked, and nothing about your ' +
'usage can be tracked back to you.'#13#10#13#10 +
'Thank you for helping us improve this software.';


type
TAnalyticsThread = class;

TAnalyticsOption = (aoTrackStartup, aoTrackFormActivate, aoTrackControlFocus,
  aoTrackExceptions);

TAnalyticsOptions = set of TAnalyticsOption;

TAnalyticsPrivacyMessageEvent = procedure(Sender: TObject; var Activate: Boolean) of Object;

TUsertility = class(TComponent)
private
  FOptions: TAnalyticsOptions;
  FApplicationID: string;
  FPrivacyMessage: TStrings;
  FOnPrivacyMessage: TAnalyticsPrivacyMessageEvent;
  FCBTHookHandle: THandle;
  FActive: Boolean;
  FDataCache: TStringList;
  FUpdateTimer: TTimer;
  FMaxCacheSize: Integer;
  FUserID: string; //ANONYMOUS ID used to track this user through a session
  FSessionID: string;
  FEventCount: Cardinal;
  FActiveForm: TForm;
  FFocusedControl: TControl;

  FOldExceptionHandler: TExceptionEvent;

  procedure InstallHooks;
  procedure RemoveHooks;
  procedure SetActive(const Value: Boolean);
  procedure Log(AMessage: string);
  procedure SendData;
  procedure SendDataNoIndy;
  function GetUpdateInterval: Integer;
  procedure SetUpdateInterval(const Value: Integer);
  procedure UpdateTimerFire(Sender: TObject);

  procedure TrackException(Sender: TObject; E:Exception);
  procedure InstallExceptionHandler;
  procedure RemoveExceptionHandler;

  procedure TrackApplicationStarted;
  procedure TrackApplicationExit;
  procedure TrackWindowActivated(AHandle: THandle);
  procedure TrackControlFocused(AHandle: THandle);
  procedure SetCacheSize(const Value: Integer);
  procedure SetOptions(const Value: TAnalyticsOptions);
    procedure SetPrivacyMessage(const Value: TStrings);
    procedure SetOnPrivacyMessage(const Value: TAnalyticsPrivacyMessageEvent);
protected
  procedure Loaded; override;
public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
  procedure TrackEvent(ACategory: string; AAction: string = '';
    ALabel: string = ''; AValue: Double = 0.0);
  function GetTimestamp: string;
  function StartSending: THandle;
  function CheckPrivacy: Boolean;
published
  property OnPrivacyMessage: TAnalyticsPrivacyMessageEvent read
    FOnPrivacyMessage write SetOnPrivacyMessage;
  property ApplicationID: string read FApplicationID write FApplicationID;
  property Active: Boolean read FActive write SetActive;
  property UpdateInterval: Integer read GetUpdateInterval write
    SetUpdateInterval;
  property CacheSize: Integer read FMaxCacheSize write SetCacheSize;
  property Options: TAnalyticsOptions read FOptions write SetOptions;
  property PrivacyMessage: TStrings read FPrivacyMessage write
  SetPrivacyMessage;
end;

//Various error classes
EInvalidAnalyticsUsage = class(Exception)
end;

EAnalyticsInitializationFailed = class(Exception)
end;

EAnalyticsInactive = class(Exception)
end;

TAnalyticsThread = class(TThread)
public
  procedure Execute; override;
end;

function GetUsertility: TUsertility;

implementation

uses Dialogs, Registry, SyncObjs,
{$IF DEFINED(USERTILITY_USEINDY)}
IdHTTP,
{$ELSEIF DEFINED(MSWINDOWS)}
WinINet
{$IFEND};

var
  GlobalAnalytics: TUsertility = nil;
  AnalyticsCriticalSection: TCriticalSection;
function GetUsertility: TUsertility;
begin
  Result := GlobalAnalytics;
end;

//Non-OO Hook callbacks
function CBTHookProc(nCode: Integer; WPARAM: WPARAM; LPARAM: LPARAM): LRESULT;
  stdcall;
var
  MouseStruct: PMouseHookStruct;
  TargetHandle: THandle;
  TargetControl: TControl;
begin
  case nCode of
  HCBT_ACTIVATE:
    if aoTrackFormActivate in GlobalAnalytics.Options then
      GlobalAnalytics.TrackWindowActivated(WPARAM);
  HCBT_MINMAX:;
  HCBT_MOVESIZE:;
  HCBT_SETFOCUS:
    if aoTrackControlFocus in GlobalAnalytics.Options then
      GlobalAnalytics.TrackControlFocused(WPARAM);
  HCBT_SYSCOMMAND:;
  HCBT_CLICKSKIPPED:
    {begin
      MouseStruct := PMouseHookStruct(Pointer(LPARAM));
      TargetHandle := MouseStruct^.hwnd;
      TargetControl := FindControl(TargetHandle);
      if TargetControl <> nil then
      begin
        if WPARAM = WM_LBUTTONDOWN then
          GlobalAnalytics.Log(Format('Mouse Click: %s (%s)', [TargetControl.Name, TargetControl.ClassName]));
        if WPARAM = WM_RBUTTONDOWN then
          GlobalAnalytics.Log(Format('Mouse Right Click: %s (%s)', [TargetControl.Name, TargetControl.ClassName]));;
        if WPARAM = WM_LBUTTONDBLCLK then
          GlobalAnalytics.Log(Format('Mouse Double Click: %s (%s)', [TargetControl.Name, TargetControl.ClassName]));;
      end;
    end};
  HCBT_KEYSKIPPED:;
  end;
  Result := CallNextHookEx(GlobalAnalytics.FCBTHookHandle, nCode, WPARAM, LPARAM);
end;

//Misc. Support Routines
function GetCPUName: string;
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('Hardware\Description\System\CentralProcessor\0', False) then
      Result := Reg.ReadString('ProcessorNameString')
    else
      Result := '(CPU Unidentified)';
  finally
    Reg.Free;
  end;
end;

function GetCpuSpeed: string;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('Hardware\Description\System\CentralProcessor\0', False) then
    begin
      Result := IntToStr(Reg.ReadInteger('~MHz')) + ' MHz';
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure GetBuildInfo(var V1, V2, V3, V4: word);
var
  VerInfoSize, VerValueSize, Dummy: DWORD;
  VerInfo: Pointer;
  VerValue: PVSFixedFileInfo;
begin
  VerInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy);
  if VerInfoSize > 0 then
  begin
      GetMem(VerInfo, VerInfoSize);
      try
        if GetFileVersionInfo(PChar(ParamStr(0)), 0, VerInfoSize, VerInfo) then
        begin
          VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
          with VerValue^ do
          begin
            V1 := dwFileVersionMS shr 16;
            V2 := dwFileVersionMS and $FFFF;
            V3 := dwFileVersionLS shr 16;
            V4 := dwFileVersionLS and $FFFF;
          end;
        end;
      finally
        FreeMem(VerInfo, VerInfoSize);
      end;
  end;
end;

function GetBuildInfoAsString: string;
var
  V1, V2, V3, V4: word;
begin
  GetBuildInfo(V1, V2, V3, V4);
  Result := IntToStr(V1) + '.' + IntToStr(V2) + '.' +
    IntToStr(V3) + '.' + IntToStr(V4);
end;

{ TApplicationAnalytics }

function TUsertility.CheckPrivacy: Boolean;
var
  AllowTracking: Boolean;
  Reg: TRegistry;
begin
  AllowTracking := True;
  if Assigned(FOnPrivacyMessage) then
  begin
    FOnPrivacyMessage(Self, AllowTracking);
    if not AllowTracking then
    begin
      Active := False;
    end;
  end
  else
  begin
    ShowMessage(PrivacyMessage.Text);
  end;

  //Do the registry thing
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.Access := KEY_WRITE;
    Reg.OpenKey('Software\TwoDesk\Analytics\' + ApplicationID, True);
    Reg.WriteBool('A', AllowTracking);
  finally
    Reg.CloseKey;
    Reg.Free;
  end;

  Result := AllowTracking;

end;

constructor TUsertility.Create(AOwner: TComponent);
var
  GUID: TGUID;
begin
  inherited;

  CreateGUID(GUID);
  FSessionID := GUIDToString(GUID);
  FEventCount := 0;

  Options := [aoTrackStartup, aoTrackFormActivate, aoTrackExceptions];

  if GlobalAnalytics <> nil then
  begin
    raise EInvalidAnalyticsUsage.Create('Only one analytics component can be used per application');
  end;
  GlobalAnalytics := Self;

  FActive := False;

  FDataCache := TStringList.Create;
  FMaxCacheSize := 500;

  FUpdateTimer := TTimer.Create(Self);
  FUpdateTimer.Interval := 600000; //Default update interval: 10 minutes
  FUpdateTimer.OnTimer := UpdateTimerFire;

  FPrivacyMessage := TStringList.Create;
  FPrivacyMessage.Text := sPrivacyMessage;
end;



destructor TUsertility.Destroy;
begin
  //Send any remaining data, if possible
  Active := False;
  FPrivacyMessage.Free;
  FDataCache.Free;
  GlobalAnalytics := nil;
  AnalyticsCriticalSection.Free;
  AnalyticsCriticalSection := nil;
  inherited;
end;

function TUsertility.GetTimestamp: string;
var
  UTC: TSystemTime;
begin
  GetSystemTime(UTC);
  Result := Format('%d-%d-%d %d:%d:%d.%d',
    [UTC.wYear, UTC.wMonth, UTC.wDay,
    UTC.wHour, UTC.wMinute, UTC.wSecond, UTC.wMilliseconds]);
end;

function TUsertility.GetUpdateInterval: Integer;
begin
  Result := FUpdateTimer.Interval div 1000;
end;

procedure TUsertility.InstallExceptionHandler;
begin
  FOldExceptionHandler := Application.OnException;
  Application.OnException := TrackException;
end;

procedure TUsertility.InstallHooks;
begin
  FCBTHookHandle := SetWindowsHookEx(WH_CBT, @CBTHookProc, 0, GetCurrentThreadID);
  if FCBTHookHandle = 0 then
  begin
    raise EAnalyticsInitializationFailed.Create(Format('CBT hook could not be ' +
              'installed. Error code: %d', [GetLastError]));
  end;

  if aoTrackExceptions in Options then
    InstallExceptionHandler;
end;

procedure TUsertility.Loaded;
begin
  inherited;
  //This is here to make sure that the Active property setter gets fully
  //executed AFTER the OnPrivacyMessage event gets set
  if FActive then
  begin
    FActive := False;
    Active := True;
  end;
end;

procedure TUsertility.Log(AMessage: string);
begin
  {$IFDEF DEBUG}
  OutputDebugString(PChar('TwoDesk Software Analytics: ' + AMessage));
  {$ENDIF}
  AnalyticsCriticalSection.Enter;
  try
    FDataCache.Add(IntToStr(FEventCount) + '|' + AMessage);
    Inc(FEventCount);
  finally
    AnalyticsCriticalSection.Leave;
  end;
  if (FDataCache.Count > FMaxCacheSize) and (not Application.Terminated) then
    StartSending;
end;

procedure TUsertility.RemoveExceptionHandler;
begin
  Application.OnException := FOldExceptionHandler;
end;

procedure TUsertility.RemoveHooks;
begin
  if aoTrackExceptions in Options then
    RemoveExceptionHandler;
  UnhookWindowsHookEx(FCBTHookHandle);
end;

//This method should generally be called from the analytics thread, but
//it can be called from main UI thread if needed.
//Note that it will BLOCK on network access, so running it in the main
//Thread is discouraged
procedure TUsertility.SendData;
var
  {$IFDEF USERTILITY_USEINDY}
  http: TIdHttp;
  {$ENDIF}
  DataCount, I: Integer;
  ParamList: TStrings;
  HttpResult: string;
begin
  {$IFDEF USERTILITY_USEINDY}
  ParamList := TStringList.Create;
  http := TIdHTTP.Create(Self);
  try
    AnalyticsCriticalSection.Enter;
    try
      DataCount := FDataCache.Count;
      if DataCount = 0 then
        Exit;
      ParamList.Add(Format('I=%s', [ApplicationID]));
      ParamList.Add(Format('U=%s', [FUserID]));
      ParamList.Add(Format('S=%s', [FSessionID]));
      ParamList.Add(Format('N=%d', [DataCount]));
      for I := 0 to DataCount - 1 do
      begin
        ParamList.Add(Format('L%d=%s',[I, FDataCache[0]]));
        FDataCache.Delete(0);
      end;
    finally
      AnalyticsCriticalSection.Leave;
    end;
    //ShowMessage(paramList.Text);
    //HttpResult := http.Post('http://www.microbie.com/reflectanal.php',
    //ParamList);
    try
      //192.168.2.101 = staging
      HttpResult := http.Post('http://usertility.com/d.php',
                ParamList);
      //ShowMessage(HttpResult);
    except
      //If anything goes wrong, suppress the error.
    end;

  finally
    http.Free;
    ParamList.Free;
  end;
  {$ENDIF}
  SendDataNoIndy;
end;

procedure TUsertility.SendDataNoIndy;
var
  HSession, HConnect, HRequest: HINTERNET;
  ParamList: TStrings;
  Content, EncodedData: string;
  Header: String;
  ANSIContent: ANSIString;
  DataCount, I: Integer;

function URLEncode(S: string):string;
var
  J: Integer;
  StartIndex: Integer;
begin
  StartIndex := Pos('=', S);
  Result := Copy(S, 1, StartIndex);
  for J := StartIndex + 1 to Length(S) do
    if not (S[J] in ['A'..'Z','a'..'z','0','1'..'9','-','_','~','.']) then
      Result := Result + '%' + IntToHex(ord(S[J]),2)
    else Result := Result + S[J];
end;

begin
  ParamList := TStringList.Create;
  try
    try
      AnalyticsCriticalSection.Enter;
      DataCount := FDataCache.Count;
        if DataCount = 0 then
          Exit;
        ParamList.Add(Format('I=%s', [ApplicationID]));
        ParamList.Add(Format('U=%s', [FUserID]));
        ParamList.Add(Format('S=%s', [FSessionID]));
        ParamList.Add(Format('N=%d', [DataCount]));
        for I := 0 to DataCount - 1 do
        begin
          ParamList.Add(Format('L%d=%s',[I, FDataCache[0]]));
          FDataCache.Delete(0);
        end;
    finally
      AnalyticsCriticalSection.Leave;
    end;

    HSession := InternetOpen('Usertility', INTERNET_OPEN_TYPE_PRECONFIG,
      nil, nil, 0);
    {$IFDEF USERTILITY_DEBUG}
    if HSession = nil then
      ShowMessage(IntToStr(GetLastError));
    {$ENDIF}
    try
      HConnect := InternetConnect(HSession, 'usertility.com',
        INTERNET_DEFAULT_HTTP_PORT, nil, nil,
                INTERNET_SERVICE_HTTP, 0, 0);
      {$IFDEF USERTILITY_DEBUG}
      if HConnect = nil then
          ShowMessage(IntToStr(GetLastError));
      {$ENDIF}
      try
        HRequest := HTTPOpenRequest(HConnect, 'POST', '/d.php', nil, nil, nil,
          0, 0);
        {$IFDEF USERTILITY_DEBUG}
        if HRequest = nil then
          ShowMessage(IntToStr(GetLastError));
        {$ENDIF}
        try
          Content := '';
          for I := 0 to ParamList.Count - 1 do
          begin
            EncodedData := URLEncode(ParamList[I]);
            Content := Content + EncodedData;
            if I < ParamList.Count - 1 then
            begin
              Content := Content + '&';
            end;
          end;
          ANSIContent := Content;
          Header := 'Content-Type: application/x-www-form-urlencoded';
          if not HTTPSendRequest(HRequest, PChar(Header), Length(Header),
            PAnsiChar(ANSIContent), Length(ANSIContent) * Sizeof(ANSIChar)) then
              {$IFDEF USERTILITY_DEBUG}
              ShowMessage(IntToStr(GetLastError))
              {$ENDIF};
          {SetLength(ANSIContent, 1024);
          InternetReadFile(HRequest, @ANSIContent[1], 1024, Len);
          SetLength(ANSIContent, Len);
          ShowMessage(ANSIContent);}
        finally
          InternetCloseHandle(HRequest);
        end;
      finally
        InternetCloseHandle(HConnect);
      end;
    finally
      InternetCloseHandle(HSession);
    end;
  finally
    ParamList.Free;
  end;
end;

procedure TUsertility.SetActive(const Value: Boolean);
var
  Reg: TRegistry;
  GUID: TGUID;
  AllowTracking: Boolean;
begin
  if Value = True then
    if ApplicationID = '' then
        raise EAnalyticsInitializationFailed.Create('Invalid Application ID');
  if (not (csDesigning in ComponentState)) and (not (csLoading in ComponentState)) then
  begin
    if (Value = True) and (FActive <> Value) then
    begin
      Reg := TRegistry.Create(KEY_READ);
      try
        Reg.RootKey := HKEY_CURRENT_USER;
        if not Reg.KeyExists('Software\TwoDesk\Analytics\' + ApplicationID) then
        begin
          Reg.Access := KEY_WRITE;
          if not Reg.OpenKey('Software\TwoDesk\Analytics\' + ApplicationID, True) then
            raise EAnalyticsInitializationFailed.Create('Could not store ' +
                      'Application ID in registry');
          CreateGUID(GUID);
          FUserID := GuidToString(GUID);
          Reg.WriteString('U', FUserID);
          AllowTracking := CheckPrivacy;
          if not AllowTracking then
            Active := False;
        end else
        begin
          Reg.OpenKey('Software\TwoDesk\Analytics\' + ApplicationID, False);
          FUserID := Reg.ReadString('U');
          if Reg.ValueExists('A') then
          begin
            AllowTracking := Reg.ReadBool('A')
          end
          else
          begin
            AllowTracking := CheckPrivacy;
          end;
        end;
      finally
        Reg.CloseKey;
        Reg.Free;
      end;
      if AllowTracking then
      begin
        InstallHooks;
        if aoTrackStartup in Options then
          TrackApplicationStarted;
        FUpdateTimer.Enabled := True;
      end;
    end
    else if (Value = False) and (FActive <> Value) then
    begin
      RemoveHooks;
      FUpdateTimer.Enabled := False;
      TrackApplicationExit;
      //WaitForSingleObject(StartSending, INFINITE);
      SendData;
    end;
  end;
  FActive := Value;
end;

procedure TUsertility.SetCacheSize(const Value: Integer);
begin
  FMaxCacheSize := Value;
  if csDesigning in ComponentState then
    Exit;
  if FDataCache.Count >= Value then
    StartSending;
end;

procedure TUsertility.SetOnPrivacyMessage(
  const Value: TAnalyticsPrivacyMessageEvent);
begin
  FOnPrivacyMessage := Value;
end;

procedure TUsertility.SetOptions(const Value: TAnalyticsOptions);
begin
  FOptions := Value;
end;

procedure TUsertility.SetPrivacyMessage(const Value: TStrings);
begin
  FPrivacyMessage.Assign(Value);
end;

procedure TUsertility.SetUpdateInterval(const Value: Integer);
begin
  FUpdateTimer.Interval := Value * 1000;
end;

function TUsertility.StartSending: THandle;
var
  SendThread: TAnalyticsThread;
begin
  SendThread := TAnalyticsThread.Create(True);
  Result := SendThread.Handle;
  SendThread.FreeOnTerminate := True;
  SendThread.Resume;
end;

procedure TUsertility.TrackApplicationExit;
var
  S: string;
begin
  S := 'AppExit|' + GetTimestamp;
  Log(S);
end;

procedure TUsertility.TrackApplicationStarted;
var
  S: string;
  OSVersion: string;
  VersionInfo: TOSVersionInfo;
  MajorVersion, MinorVersion: Cardinal;
  CPUName: string;
  BuildVersion: string;
begin
  VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
  GetVersionEx(VersionInfo);
  MajorVersion := VersionInfo.dwMajorVersion;
  MinorVersion := VersionInfo.dwMinorVersion;
  OSVersion := Format('%d.%d' , [MajorVersion, MinorVersion]);

  CPUName := GetCPUName;
  BuildVersion := GetBuildInfoAsString;

  S := 'AppStart|' + GetTimestamp + '|' + OSVersion + '|' + CPUName + '|' +
            BuildVersion;
  Log(S);
end;

procedure TUsertility.TrackControlFocused(AHandle: THandle);
var
  S: string;
  TargetControl: TControl;
begin
  {$IFDEF VCL}
  S := 'ControlFocus|' + GetTimestamp;
  TargetControl := FindControl(AHandle);
  if (TargetControl <> nil) and (TargetControl <> FFocusedControl) then
  begin
    S := S + '|' + TargetControl.ClassName + '|' + TargetControl.Name;
    Log(S);
    FFocusedControl := TargetControl;
  end;
  {$ENDIF}
end;

procedure TUsertility.TrackEvent(ACategory: string;
  AAction: string = ''; ALabel: string = ''; AValue: Double = 0.0);
var
  S: string;
begin
  if not Active then
    Exit;
  //  raise EAnalyticsInactive.Create('Actions not allowed if analytics are inactive');
  S := ACategory;
  if AAction <> '' then
  begin
    S := S + '|' + AAction;
    if ALabel <> '' then
    begin
      S := S + '|' + ALabel;
      //if not IsNan(AValue) then
      //begin
        S := S + '|' + FloatToStr(AValue);
      //end;
    end;
  end;
  Log('TrackEvent|' + GetTimestamp + '|' + S);
end;

procedure TUsertility.TrackException(Sender: TObject; E: Exception);
begin
  Log('AppCrash|' + GetTimestamp + '|' + E.ClassName + '|' + E.Message);
  if Assigned(FOldExceptionHandler) then
    FOldExceptionHandler(Sender, E)
  else
    Application.ShowException(E);
end;

procedure TUsertility.TrackWindowActivated(AHandle: THandle);
var
  S: string;
  {$IF DEFINED(VCL)}
  TargetControl: TControl;
  {$ELSEIF DEFINED(FMX)}
  TargetControl: TFmxObject;
  {$IFEND}
begin
  S := 'FormActivate|' + GetTimestamp;
  {$IF DEFINED(VCL)}
  TargetControl := FindControl(AHandle);
  {$ELSEIF DEFINED(FMX)}
  TargetControl := Fmx.Platform.Win.FindWindow(AHandle);
  {$IFEND}
  if (TargetControl <> nil) and (TargetControl <> FActiveForm) then
  begin
    S := S + '|' + TargetControl.ClassName + '|' + TargetControl.Name;
    Log(S);
    FActiveForm := TForm(TargetControl);
  end;
end;

procedure TUsertility.UpdateTimerFire(Sender: TObject);
begin
  StartSending;
end;

{ TAnalyticsThread }

procedure TAnalyticsThread.Execute;
begin
  GlobalAnalytics.SendData;
end;

initialization
  AnalyticsCriticalSection := TCriticalSection.Create;

finalization
  AnalyticsCriticalSection.Free;

end.
