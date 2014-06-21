unit Usertility;

{$IF CompilerVersion >= 24}
{$LEGACYIFEND ON}
{$IFEND}

interface

uses Types, Classes,
{$IF DEFINED(MSWINDOWS)}
Windows, Messages, ExtCtrls, Controls, Forms,
{$ELSEIF DEFINED(MACOS) AND NOT DEFINED(IOS)}
Macapi.Foundation, Macapi.AppKit, Macapi.Helpers, Macapi.ObjectiveC,
MacApi.ObjCRuntime, ExtCtrls, Controls, Forms,
{$ELSEIF DEFINED(IOS)}
iOSApi.Foundation, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
MacApi.ObjCRuntime, Macapi.Helpers, Macapi.ObjectiveC, iOSApi.UIKit,
{$IFEND}
SysUtils, Math;

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

TUsertilityBase = class(TComponent)
protected
  FOptions: TAnalyticsOptions;
  FApplicationID: string;
  FPrivacyMessage: TStrings;
  FOnPrivacyMessage: TAnalyticsPrivacyMessageEvent;
  {$IF DEFINED(MSWINDOWS)}
  FCBTHookHandle: THandle;
  {$ELSEIF DEFINED(MACOS)}
  ObjCObserverClass: Pointer;
  ObjCObserver: Pointer;
  {$IFEND}
  FActive: Boolean;
  FDataCache: TStringList;
  FUpdateTimer: TTimer;
  FMaxCacheSize: Integer;
  FUserID: string; //ANONYMOUS ID used to track this user through a session
  FSessionID: string;
  FEventCount: Cardinal;
  FActiveForm: TObject; //Might be a VCL form or an FMX form
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
  procedure TrackWindowActivated(AHandle: THandle); virtual;
  procedure TrackControlFocused(AHandle: THandle); virtual;
  procedure SetCacheSize(const Value: Integer);
  procedure SetOptions(const Value: TAnalyticsOptions);
  procedure SetPrivacyMessage(const Value: TStrings);
  procedure SetOnPrivacyMessage(const Value: TAnalyticsPrivacyMessageEvent);
  procedure Loaded; override;
  function GetUserID: string;
  function GetAllowTracking: Boolean;
public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
  procedure TrackEvent(ACategory: string; AAction: string = '';
    ALabel: string = ''; AValue: Double = 0.0);
  function GetTimestamp: string;
  function StartSending: THandle;
  function CheckPrivacy: Boolean;
  procedure OSXWindowDidBecomeKey(P: Pointer);
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
  property UserID: string read GetUserID;
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

function GetUsertility: TUsertilityBase;

implementation

uses SyncObjs, DateUtils
{$IF DEFINED(MSWINDOWS)}
, Registry
, WinINet
, Dialogs
{$ELSEIF DEFINED(MACOS) AND NOT DEFINED(IOS)}
, FMX.Platform.Mac
, Dialogs
{$ELSEIF DEFINED(IOS)}
, FMX.Platform.iOS
{$IFEND};

var
  GlobalAnalytics: TUsertilityBase = nil;
  AnalyticsCriticalSection: TCriticalSection;
function GetUsertility: TUsertilityBase;
begin
  Result := GlobalAnalytics;
end;

//Non-OO Hook callbacks
{$IFDEF MSWINDOWS}
function CBTHookProc(nCode: Integer; WPARAM: WPARAM; LPARAM: LPARAM): LRESULT;
  stdcall;
//var
  //MouseStruct: PMouseHookStruct; //Reserved for later
  //TargetHandle: THandle; //Reserved for later
  //TargetControl: TControl; //Reserved for later
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
{$ENDIF}

{$IF DEFINED(MACOS)}
procedure ObjCWindowDidBecomeKey(Self: Pointer; Cmd: SEL; Extra: Pointer); cdecl;
var
  Notification: NSNotification;
  Window: {$IF DEFINED(IOS)}UIWindow{$ELSE}NSWindow{$IFEND};
  Handle: THandle;
  S: string;
begin
  Notification := TNSNotification.Wrap(Extra);
  S := NSStrToStr(Notification.name);
//  if S = 'UIWindowDidBecomeKeyNotification' then
//  begin
    Window :=
      {$IF DEFINED(IOS)}
      TUIWindow.Wrap(objc_msgSend(Extra, sel_getUid('object')));
      {$ELSE}
      TNSWindow.Wrap(objc_msgSend(Extra, sel_getUid('object')));
      {$IFEND}
    Handle := THandle(Window);
    GlobalAnalytics.TrackWindowActivated(Handle);
//  end;
end;
{$IFEND}

//Misc. Support Routines
function GetCPUName: string;
{$IFDEF MSWINDOWS}
var
  Reg: TRegistry;
{$ENDIF}
begin
  Result := '';
  {$IFDEF MSWINDOWS}
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
  {$ENDIF}
end;

function GetCpuSpeed: string;
{$IFDEF MSWINDOWS}
var
  Reg: TRegistry;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
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
  {$ENDIF}
end;

procedure GetBuildInfo(var V1, V2, V3, V4: word);
{$IFDEF MSWINDOWS}
var
  VerInfoSize, VerValueSize, Dummy: Cardinal;
  VerInfo: Pointer;
  VerValue: PVSFixedFileInfo;
  {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
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
  {$ENDIF}
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

function TUsertilityBase.CheckPrivacy: Boolean;
var
  AllowTracking: Boolean;
  {$IFDEF MSWINDOWS}
  Reg: TRegistry;
  {$ENDIF}
  {$IF DEFINED(MACOS)}
  Settings: NSUserDefaults;
  {$IFEND}
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
  {$IF DEFINED(MSWINDOWS)}
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
  {$ELSEIF DEFINED(MACOS)}
  Settings := TNSUserDefaults.Wrap(
         TNSUserDefaults.OCClass.standardUserDefaults);
  Settings.setBool(AllowTracking, StrToNSStr('UsertilityA'));
  {$ELSE}
  AllowTracking := False;
  {$IFEND}

  Result := AllowTracking;

end;

{$IF DEFINED(MACOS)}
function class_createInstance(cls: Pointer; extraBytes: LongWord): pointer;
  cdecl; external libobjc name _PU + 'class_createInstance';
{$IFEND}


constructor TUsertilityBase.Create(AOwner: TComponent);
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

  FUserID := '';

  {$IF DEFINED(MACOS)}
  ObjCObserverClass := objc_allocateClassPair(objc_getClass('NSObject'), 'UsertilityObserver', 0);
  class_addMethod(ObjCObserverClass, sel_getUid('OSXWindowDidBecomeKey:'),
    @ObjCWindowDidBecomeKey, 'v@:@');
  objc_registerClassPair(ObjCObserverClass);
  ObjCObserver := class_createInstance(ObjCObserverClass, 0);
  //OBJCObserver := objc_msgSend(objc_msgSend(ObjCObserverClass, sel_getUid(
  //   'alloc')), sel_getUid('init'));
  //objc_msgSend(ObjCObserver, sel_getUid('OSXWindowDidBecomeKey:'));
  {$IFEND}

end;



destructor TUsertilityBase.Destroy;
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

function TUsertilityBase.GetAllowTracking: Boolean;
var
  {$IF DEFINED(MSWINDOWS)}
  Reg: TRegistry;
  {$ELSEIF DEFINED(MACOS)}
  Settings: NSUserDefaults;
  KeyString: NSString;
  {$IFEND}
begin
  try
    {$IF DEFINED(MSWINDOWS)}
    Reg := TRegistry.Create(KEY_READ);
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      Reg.OpenKey('Software\TwoDesk\Analytics\' + ApplicationID, True);
      if Reg.ValueExists('A') then
      begin
        Result := Reg.ReadBool('A');
      end else
      begin
        Result := CheckPrivacy;
      end;
    finally
      Reg.CloseKey;
      Reg.Free;
    end;
    {$ELSEIF DEFINED(MACOS)}
    Settings := TNSUserDefaults.Wrap(
       TNSUserDefaults.OCClass.standardUserDefaults);
    //Settings.setObject((UserIDNS as ILocalObject).getObjectID, StrToNSStr(
    //   'UsertilityU'));
    KeyString := StrToNSStr('UsertilityA');
    if Settings.dictionaryRepresentation.allKeys.containsObject((KeyString as ILocalObject).GetObjectID) then
    begin
      Result := Settings.boolForKey(StrToNSStr('UsertilityA'));
    end else
    begin
      Result := CheckPrivacy;
    end;
    {$IFEND}
  except
    Result := False;
  end;
end;

function TUsertilityBase.GetTimestamp: string;
var
{$IF DEFINED(MSWINDOWS)}
  UTC: TSystemTime;
{$ELSEIF DEFINED(MACOS)}
  UTC: TDateTime;
  Year, Month, Day, Hour, Min, Sec, Milli: Word;
{$IFEND}
begin
  {$IF DEFINED(MSWINDOWS)}
  GetSystemTime(UTC);
  Result := Format('%d-%d-%d %d:%d:%d.%d',
    [UTC.wYear, UTC.wMonth, UTC.wDay,
    UTC.wHour, UTC.wMinute, UTC.wSecond, UTC.wMilliseconds]);
  {$ELSE}
  UTC := TTimeZone.Local.ToUniversalTime(Now);
  DecodeDate(UTC, Year, Month, Day);
  DecodeTime(UTC, Hour, Min, Sec, Milli);
  Result := Format('%d-%d-%d %d:%d:%d.%d',
    [Year, Month, Day, Hour, Min, Sec, Milli]);
  {$IFEND}
end;

function TUsertilityBase.GetUpdateInterval: Integer;
begin
  Result := FUpdateTimer.Interval div 1000;
end;

function TUsertilityBase.GetUserID: string;
var
  {$IF DEFINED(MSWINDOWS)}
  Reg: TRegistry;
  {$ELSEIF DEFINED(MACOS)}
  Settings: NSUserDefaults;
  UserIDNS: NSString;
  {$IFEND}
  GUID: TGUID;
begin
  Result := '';
  if FUserID = '' then
  begin
    {$IF DEFINED(MSWINDOWS)}
    Reg := TRegistry.Create(KEY_READ);
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      Reg.OpenKey('Software\TwoDesk\Analytics\' + ApplicationID, True);
      if Reg.ValueExists('U') then
      begin
        FUserID := Reg.ReadString('U');
      end else
      begin
        Reg.CloseKey;
        Reg.Access := KEY_WRITE;
        Reg.OpenKey('Software\TwoDesk\Analytics\' + ApplicationID, True);
        CreateGUID(GUID);
        FUserID := GuidToString(GUID);
        Reg.WriteString('U', FUserID);
      end;
    finally
      Reg.CloseKey;
      Reg.Free;
    end;
    {$ELSEIF DEFINED(MACOS)}
    CreateGUID(GUID);
    FUserID := GuidToString(GUID);
    Settings := TNSUserDefaults.Wrap(
       TNSUserDefaults.OCClass.standardUserDefaults);
    UserIDNS := StrToNSStr(FUserID);
    Settings.setObject((UserIDNS as ILocalObject).getObjectID, StrToNSStr(
       'UsertilityU'));
    {$IFEND}
  end;
  Result := FUserID;
end;

procedure TUsertilityBase.InstallExceptionHandler;
begin
  FOldExceptionHandler := Application.OnException;
  Application.OnException := TrackException;
end;

procedure TUsertilityBase.InstallHooks;
{$IF DEFINED(MACOS)}
var
  NotificationCenter: NSNotificationCenter;
  //NotificationStr: NSString;
  //Str: AnsiString;
  Selector: SEL;
  StrNS: NSString;
{$IFEND}
begin
  {$IF DEFINED(MSWINDOWS)}
  FCBTHookHandle := SetWindowsHookEx(WH_CBT, CBTHookProc, 0, GetCurrentThreadID);
  if FCBTHookHandle = 0 then
  begin
    raise EAnalyticsInitializationFailed.Create(Format('CBT hook could not be ' +
              'installed. Error code: %d', [GetLastError]));
  end;
  {$ELSEIF DEFINED(MACOS)}
  NotificationCenter := nil;
  NotificationCenter := TNSNotificationCenter.Wrap(
    TNSNotificationCenter.OCClass.defaultCenter
  );
  //NotificationStr := StrToNSStr('NSWindowDidBecomeKeyNotification');
  {$IF DEFINED(IOS)}
  StrNS := StrToNSStr('UIWindowDidBecomeKeyNotification');
  {$ELSE}
  StrNS := StrToNSStr('NSWindowDidBecomeKeyNotification');
  {$IFEND}
  Selector := sel_getUid('OSXWindowDidBecomeKey:');
  NotificationCenter.addObserver(ObjCObserver,
    Selector,
    (StrNS as ILocalObject).GetObjectID,
    nil
    );
  {$IFEND}

  if aoTrackExceptions in Options then
    InstallExceptionHandler;
end;

procedure TUsertilityBase.Loaded;
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

procedure TUsertilityBase.Log(AMessage: string);
begin
  {$IF DEFINED(DEBUG) AND DEFINED(MSWINDOWS)}
  OutputDebugString(PChar('TwoDesk Software Analytics: ' + AMessage));
  {$IFEND}
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

procedure TUsertilityBase.OSXWindowDidBecomeKey(P: Pointer);
begin

end;

procedure TUsertilityBase.RemoveExceptionHandler;
begin
  Application.OnException := FOldExceptionHandler;
end;

procedure TUsertilityBase.RemoveHooks;
begin
  if aoTrackExceptions in Options then
    RemoveExceptionHandler;
  {$IF DEFINED(MSWINDOWS)}
  UnhookWindowsHookEx(FCBTHookHandle);
  {$ELSEIF DEFINED(MACOS)}
  TNSNotificationCenter.Wrap(
    TNSNotificationCenter.OCClass.defaultCenter).removeObserver(
      Self);
  {$IFEND}
end;

//This method should generally be called from the analytics thread, but
//it can be called from main UI thread if needed.
//Note that it will BLOCK on network access, so running it in the main
//Thread is discouraged
procedure TUsertilityBase.SendData;
{$IFDEF USERTILITY_USEINDY}
var
  http: TIdHttp;
  DataCount, I: Integer;
  ParamList: TStrings;
  HttpResult: string;
  {$ENDIF}
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
      ParamList.Add(Format('U=%s', [UserID]));
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

procedure TUsertilityBase.SendDataNoIndy;
var
  {$IF DEFINED(MSWINDOWS)}
  HSession, HConnect, HRequest: HINTERNET;
  {$ELSEIF DEFINED(MACOS)}
  URL: NSUrl;
  URLRequest: NSMutableURLRequest;
  URLConnection: NSURLConnection;
  URLResponse: Pointer;
  PostData: NSData;
  Err: Pointer;
  ErrObj: NSError;
  {$IFEND}
  ParamList: TStrings;
  Content, EncodedData: string;
  Header: String;
  {$IF DEFINED(IOS)}
  ContentBytes: TBytes;
  {$ELSE}
  ANSIContent: ANSIString;
  {$IFEND}
  DataCount, I: Integer;

function URLEncode(S: string):string;
var
  J, ValStart, ValEnd: Integer;
  StartIndex: Integer;
begin
  StartIndex := Pos('=', S);
  {$IF DEFINED(IOS)}
  Result := Copy(S, 0, StartIndex);//0-indexed strings on mobile
  ValStart := StartIndex;
  ValEnd := Length(S) - 1;
  {$ELSE}
  Result := Copy(S, 1, StartIndex);
  ValStart := StartIndex + 1;
  ValEnd := Length(S);
  {$IFEND}
  for J := ValStart to ValEnd do
    {$IFDEF UNICODE}
    if not CharInSet(S[J], ['A'..'Z','a'..'z','0','1'..'9','-','_','~','.']) then
      Result := Result + '%' + IntToHex(ord(S[J]), 2)
    else Result := Result + S[J];
    {$ELSE}
    if not (S[J] in ['A'..'Z','a'..'z','0','1'..'9','-','_','~','.']) then
      Result := Result + '%' + IntToHex(ord(S[J]),2)
    else Result := Result + S[J];
    {$ENDIF}

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
        ParamList.Add(Format('U=%s', [UserID]));
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
    {$IF DEFINED(IOS)}
    ContentBytes := TEncoding.Convert(TEncoding.Unicode, TEncoding.ASCII,
       TEncoding.Unicode.GetBytes(Content));
    {$ELSE}
    ANSIContent := AnsiString(Content);
    {$IFEND}

    {$IF DEFINED(MSWINDOWS)}
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
    {$ELSEIF DEFINED(MACOS)}
    URL := TNSUrl.Wrap(TNSUrl.OCClass.URLWithString(
      StrToNSStr('http://usertility.com/d.php')));
    URLRequest := TNSMutableURLRequest.Wrap(
      TNSMutableURLRequest.OCClass.requestWithURL(URL));
    URLRequest.setHTTPMethod(StrToNSStr('POST'));
    {$IF DEFINED(IOS)}
    PostData := TNSData.Wrap(TNSData.OCClass.dataWithBytes(
      ContentBytes,
      Length(ContentBytes)
    ));
    {$ELSE}
    PostData := TNSData.Wrap(TNSData.OCClass.dataWithBytes(
      PAnsiChar(ANSIContent),
      Length(ANSIContent)
    ));
    {$IFEND}
    URLRequest.setHTTPBody(PostData);
    URLRequest.addValue(StrToNSStr('application/x-www-form-urlencoded'),
      StrToNSStr('Content-Type'));
    if TNSURLConnection.OCClass.sendSynchronousRequest(URLRequest, @URLResponse,
       @Err) = nil then
    begin
      ErrObj := TNSError.Wrap(Pointer(Err));
      //Not doing anything about it. Just supress the error
    end;
    {$IFEND}
  finally
    ParamList.Free;
  end;
end;

procedure TUsertilityBase.SetActive(const Value: Boolean);
var
  AllowTracking: Boolean;
begin
  if Value then
  begin
    if ApplicationID = '' then
      raise EAnalyticsInitializationFailed.Create('Invalid Application ID');
    if (FActive <> Value) and
      not (csDesigning in ComponentState) and
      not (csLoading in ComponentState) then
    begin
      GetUserID;
      AllowTracking := GetAllowTracking;
      if AllowTracking then
      begin
        InstallHooks;
        FUpdateTimer.Enabled := True;
      end else
      begin
        Active := False;
      end;
    end;
  end else
  if (FActive <> Value) then
  begin
    RemoveHooks;
    FUpdateTimer.Enabled := False;
  end;
  FActive := Value;
end;

procedure TUsertilityBase.SetCacheSize(const Value: Integer);
begin
  FMaxCacheSize := Value;
  if csDesigning in ComponentState then
    Exit;
  if FDataCache.Count >= Value then
    StartSending;
end;

procedure TUsertilityBase.SetOnPrivacyMessage(
  const Value: TAnalyticsPrivacyMessageEvent);
begin
  FOnPrivacyMessage := Value;
end;

procedure TUsertilityBase.SetOptions(const Value: TAnalyticsOptions);
begin
  FOptions := Value;
end;

procedure TUsertilityBase.SetPrivacyMessage(const Value: TStrings);
begin
  FPrivacyMessage.Assign(Value);
end;

procedure TUsertilityBase.SetUpdateInterval(const Value: Integer);
begin
  FUpdateTimer.Interval := Value * 1000;
end;

function TUsertilityBase.StartSending: THandle;
var
  SendThread: TAnalyticsThread;
begin
  if csDesigning in ComponentState then
  begin
    Result := 0;
    Exit;
  end;
  SendThread := TAnalyticsThread.Create(True);
  {$IF DEFINED(MSWINDOWS)}
  Result := SendThread.Handle;
  {$ELSE}
  Result := SendThread.ThreadID;
  {$IFEND}
  SendThread.FreeOnTerminate := True;
  {$IF CompilerVersion >= 21}
  SendThread.Start;
  {$ELSE}
  SendThread.Resume;
  {$IFEND}

end;

procedure TUsertilityBase.TrackApplicationExit;
var
  S: string;
begin
  S := 'AppExit|' + GetTimestamp;
  Log(S);
end;

procedure TUsertilityBase.TrackApplicationStarted;
var
  S: string;
  OSVersion: string;
  {$IF DEFINED(MSWINDOWS)}
  VersionInfo: TOSVersionInfo;
  {$IFEND}
  MajorVersion, MinorVersion: Cardinal;
  CPUName: string;
  BuildVersion: string;
begin
  {$IF DEFINED(MSWINDOWS)}
  VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
  GetVersionEx(VersionInfo);
  MajorVersion := VersionInfo.dwMajorVersion;
  MinorVersion := VersionInfo.dwMinorVersion;
  {$ELSE}
  MajorVersion := 0;
  MinorVersion := 0;
  {$IFEND}
  OSVersion := Format('%d.%d' , [MajorVersion, MinorVersion]);

  CPUName := GetCPUName;
  BuildVersion := GetBuildInfoAsString;

  S := 'AppStart|' + GetTimestamp + '|' + OSVersion + '|' + CPUName + '|' +
            BuildVersion;
  Log(S);
end;

procedure TUsertilityBase.TrackControlFocused(AHandle: THandle);
begin

end;

procedure TUsertilityBase.TrackEvent(ACategory: string;
  AAction: string = ''; ALabel: string = ''; AValue: Double = 0.0);
var
  S: string;
begin
  if not Active then
    Exit;
  S := ACategory;
  if AAction <> '' then
  begin
    S := S + '|' + AAction;
    if ALabel <> '' then
    begin
      S := S + '|' + ALabel;
      S := S + '|' + FloatToStr(AValue);
    end;
  end;
  Log('TrackEvent|' + GetTimestamp + '|' + S);
end;

procedure TUsertilityBase.TrackException(Sender: TObject; E: Exception);
begin
  Log('AppCrash|' + GetTimestamp + '|' + E.ClassName + '|' + E.Message);
  if Assigned(FOldExceptionHandler) then
    FOldExceptionHandler(Sender, E)
  else
    Application.ShowException(E);
end;

procedure TUsertilityBase.TrackWindowActivated(AHandle: THandle);
begin
  //Stub. This depends on the framework.
end;

procedure TUsertilityBase.UpdateTimerFire(Sender: TObject);
begin
  //FUpdateTimer.Enabled := False;
  if not (csDesigning in ComponentState) then
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
