unit UVideoDriver;

interface

uses windows, Registry, SysUtils;

Const
  DriverName = 'Winvnc video hook driver';

  ENUM_CURRENT_SETTINGS = DWORD(-1); // Cardinal(-1)
  ENUM_REGISTRY_SETTINGS = DWORD(-2);

  Map1 = 1030;
  UNMap1 = 1031;
  CursorEN = 1060;
  CursorDIS = 1061;
  TestDriver2 = 1060;
  TestMapped2 = 1061;

  MaxChangesBuf = 2000;
  Clip_Limit = 50;
  Ignore = 0;

  FromScreen = 1;
  FromDIB = 2;
  ToScreen = 3;

  ScreenScreen = 11;
  BLIT = 12;
  SolidFill = 13;
  Blend = 14;
  Trans = 15;
  PLG = 17;
  TextOut = 18;

  BMF_1BPP = 1;
  BMF_4BPP = 2;
  BMF_8BPP = 3;
  BMF_16BPP = 4;
  BMF_24BPP = 5;
  BMF_32BPP = 6;
  BMF_4RLE = 7;
  BMF_8RLE = 8;
  BMF_JPEG = 9;
  BMF_PNG = 10;

  NoCache = 1;
  OldCache = 2;
  NewCache = 3;

  SIOCTL_TYPE = 40000;
  METHOD_BUFFERED = 0;
  METHOD_IN_DIRECT = 1;
  METHOD_OUT_DIRECT = 2;
  METHOD_NEITHER = 3;

  FILE_ANY_ACCESS = 0;
  FILE_READ_ACCESS = 1;
  FILE_WRITE_ACCESS = 2;

  IOCTL_SIOCTL_METHOD_IN_DIRECT = (((SIOCTL_TYPE) shl 16) or (($900) shl 2) or
    (METHOD_IN_DIRECT) or ((FILE_ANY_ACCESS) shl 14));
  IOCTL_SIOCTL_METHOD_OUT_DIRECT = (((SIOCTL_TYPE) shl 16) or (($901) shl 2) or
    (METHOD_OUT_DIRECT) or ((FILE_ANY_ACCESS) shl 14));
  IOCTL_SIOCTL_METHOD_BUFFERED = (((SIOCTL_TYPE) shl 16) or (($902) shl 2) or
    (METHOD_BUFFERED) or ((FILE_ANY_ACCESS) shl 14));
  IOCTL_SIOCTL_METHOD_NEITHER = (((SIOCTL_TYPE) shl 16) or (($902) shl 2) or
    (METHOD_NEITHER) or ((FILE_ANY_ACCESS) shl 14));

type
  PChangesRecord = ^TChangesRecord;

  TChangesRecord = record
    ScreenType: ULONG; // screen_to_screen, blit, newcache,oldcache
    Rect: TRECT;
    Point: TPOINT;
  end;

  PChangesBuf = ^TChangesBuf;

  TChangesBuf = record
    Counter: ULONG;
    PointRect: array [0 .. MaxChangesBuf] of TChangesRecord;
  end;

  PGetChangesBuf = ^TGetChangesBuf;

  TGetChangesBuf = record
    Buffer: PChangesBuf;
    UserBufferBegin: Pointer;
    UserBufferEnd: Pointer;
  end;

  TEnumDisplayDevices = function(Unused: Pointer; iDevNum: DWORD;
    var lpDisplayDevice: TDisplayDevice; dwFlags: DWORD): Boolean; stdcall;

  TChangeDisplaySettingsExA = function(lpszDeviceName: PAnsiChar;
    var lpDevMode: TDeviceModeA; wnd: HWND; dwFlags: DWORD; lParam: Pointer)
    : Longint; stdcall;

  TVNCVideoDriver = class(TObject)
  private
    FBlocked: Boolean;
    FDriverSucces: Boolean;
    FFirst: Boolean;
    FResolution: Boolean;
    FFileHandle: THandle;
    FGDC: HDC;
    FOutGetChangesBuf: TGetChangesBuf;
    FOldaantal: Cardinal;
    procedure RestoreReg(RegValue: Integer);
  protected
  public
    // Make the desktop thread & window proc friends
    property Oldaantal: Cardinal read FOldaantal write FOldaantal;
    property GDC: HDC read FGDC write FGDC;
    property DriverSucces: Boolean read FDriverSucces write FDriverSucces;
    property Blocked: Boolean read FBlocked write FBlocked;
    property OutGetChangesBuf: TGetChangesBuf read FOutGetChangesBuf
      write FOutGetChangesBuf;
    property First: Boolean read FFirst write FFirst;
    Property Resolution: Boolean read FResolution write FResolution;
    constructor Create;
    destructor Destroy; virtual;
    function ActivateVideoDriver(Auto: Boolean; X, Y, W, H: Integer): Boolean;
    procedure DesActivateVideoDriver;
    procedure StartMirroring;
    procedure StopMirroring;
    function ExistMirrorDriver: Boolean;
    function HardWareCursor: Boolean;
    function NoHardWareCursor: Boolean;
    function CreateCommunicationBuffer(ScreenSize: Integer): TGetChangesBuf;
    function RemoveCommunicationBuffer: TGetChangesBuf;
    function IsMirrorDriverActive: Boolean;
    function GetDllProductVersion(var VBuffer: string; DllName: string;
      Size: Integer): Boolean;
    procedure UpDateCommunicationBuffer;
    function GetDCMirror: HDC;
    function Tempres: Boolean;
  end;

implementation

{ TVNCVideoDriver }

function TVNCVideoDriver.ActivateVideoDriver(Auto: Boolean;
  X, Y, W, H: Integer): Boolean;
var
  DMPosition: Integer;
  HDeskInput, HDeskCurrent: HDESK;
  j: Integer;
  pd: TEnumDisplayDevices;
  pcdse: TChangeDisplaySettingsExA;
  DeviceName: string;
  TmpDevMode: TDeviceMode;
  bChange: Boolean;
  Moudle: THandle;
  DisplayDevice: TDisplayDevice;
  DeviceNum: Integer;
  RBool: Boolean;
  hKeyProfileMirror, hKeyDevice: HKEY;
  CursorPos: TPOINT;
  FarProc: TFarProc;
begin
  Result := False;
  // #define DM_POSITION             0x00000020L
  DMPosition := 32; // »∑»œ÷–
  First := True;

  // FillMemory(@TmpDevMode, SizeOf(DEVMODE), 0);
  TmpDevMode.dmSize := SizeOf(DEVMODE);
  TmpDevMode.dmDriverExtra := 0;

  bChange := EnumDisplaySettings(nil, 0, TmpDevMode);
  TmpDevMode.dmFields := DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT or
    DM_POSITION or DM_DISPLAYFLAGS;

  Moudle := LoadLibrary('USER32');
  FarProc := GetProcAddress(Moudle, 'EnumDisplayDevicesA');
  if FarProc = nil then
    exit;
  pd := TEnumDisplayDevices(FarProc);
  FarProc := GetProcAddress(Moudle, 'ChangeDisplaySettingsExA');
  pcdse := TChangeDisplaySettingsExA(FarProc);
  FreeLibrary(Moudle);

  if FarProc <> nil then
  begin
    ZeroMemory(@DisplayDevice, SizeOf(DisplayDevice));
    DisplayDevice.cb := SizeOf(DisplayDevice);
    DeviceName := '';
    DeviceNum := 0;
    TmpDevMode.dmDeviceName[0] := #0;
    RBool := pd(nil, DeviceNum, DisplayDevice, 0);
    while RBool do
    begin
      if (CompareStr(DisplayDevice.DeviceString, DriverName) = 0) then
        Break;
      Inc(DeviceNum);
      RBool := pd(nil, DeviceNum, DisplayDevice, 0);
    end;
    if not RBool then
      exit;

    RestoreReg(1);
    StrCopy(Pchar(TmpDevMode.dmDeviceName[0]), Pchar('vncdrv'));
    DeviceName := 'vncdrv';
    HDeskCurrent := GetThreadDesktop(GetCurrentThreadId());
    if HDeskCurrent <> 0 then
    begin
      HDeskInput := OpenInputDesktop(0, False, MAXIMUM_ALLOWED);
      if HDeskInput <> 0 then
        SetThreadDesktop(HDeskInput)
      else
        windows.Beep(1000, 500);
    end;
  end;
  if (Moudle > 0) then
    FreeLibrary(Moudle);
  Sleep(500);
  Result := True;
end;

constructor TVNCVideoDriver.Create;
begin
  DriverSucces := False;
  Blocked := False;
  First := True;
  Resolution := False;
end;

function TVNCVideoDriver.CreateCommunicationBuffer(ScreenSize: Integer)
  : TGetChangesBuf;
begin
  First := True;
  if FFileHandle <> 0 then
    CloseHandle(FFileHandle);
  FFileHandle := CreateFile('\\.\VncIoctl', GENERIC_READ or GENERIC_WRITE, 0,
    Nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  if FFileHandle = INVALID_HANDLE_VALUE then
  begin
    DriverSucces := False;
    exit;
  end;
  UpDateCommunicationBuffer;
  Result := OutGetChangesBuf;
end;

procedure TVNCVideoDriver.DesActivateVideoDriver;
var
  HDeskInput, HDeskCurrent: HDESK;
  pd: TEnumDisplayDevices;
  pcdse: TChangeDisplaySettingsExA;
  i: Integer;
  DeviceMode: TDeviceMode;
  Moudle: THandle;
  FarProc: TFarProc;
  DisplayDevice: TDisplayDevice;
  DeviceName: string;
  DeviceNum: Integer;
  RBool: Boolean;
begin
  First := True;
  {
    FillMemory(DeviceMode, sizeof(DEVMODE), 0);
    TmpDevMode.dmSize := sizeof(DEVMODE);
    TmpDevMode.dmDriverExtra := 0;
  }
  EnumDisplaySettings(Nil, ENUM_CURRENT_SETTINGS, DeviceMode);
  DeviceMode.dmFields := DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
  Moudle := LoadLibrary('USER32');
  pd := TEnumDisplayDevices(GetProcAddress(Moudle, 'EnumDisplayDevicesA'));
  pcdse := TChangeDisplaySettingsExA(GetProcAddress(Moudle,
    'ChangeDisplaySettingsExA'));
  if Moudle <> 0 then
  begin
    // ZeroMemory(@DisplayDevice,SizeOf(DisplayDevice));
    DisplayDevice.cb := SizeOf(TDisplayDevice);
    DeviceName := '';
    DeviceMode.dmDeviceName[0] := #0;
    DeviceNum := 0;
    RBool := pd(nil, DeviceNum, DisplayDevice, 0);
    while RBool do
    begin
      if (CompareStr(Pchar(DisplayDevice.DeviceName[0]), DriverName) = 0) then
        Break;
      Inc(DeviceNum);
    end;

    if not RBool then
      exit;
    RestoreReg(0);

    StrCopy(Pchar(DeviceMode.dmDeviceName[0]), Pchar('vncdrv'));
    DeviceName := 'vncdrv';

    // save the current desktop
    HDeskCurrent := GetThreadDesktop(GetCurrentThreadId());
    if HDeskCurrent <> 0 then
    begin
      HDeskInput := OpenInputDesktop(0, False, MAXIMUM_ALLOWED);
      if HDeskInput <> 0 then
        SetThreadDesktop(HDeskInput)
      else
        windows.Beep(1000, 500);
    end;

    Blocked := False;
    {
      if not Resolution then
      begin
      pcdse(PAnsiChar(DeviceName), DeviceMode, 0, CDS_UPDATEREGISTRY, Nil);
      pcdse(PAnsiChar(DeviceName), DeviceMode, 0, 0, Nil);
      end;
    }
    Blocked := False;
    if Moudle > 0 then
      FreeLibrary(Moudle);
    // reset desktop
    SetThreadDesktop(HDeskCurrent);
    // close the input desktop
    CloseDesktop(HDeskInput);

    RestoreReg(0);

  end;

end;

destructor TVNCVideoDriver.Destroy;
begin
  FDriverSucces := False;
  if (FFileHandle <> 0) then
    CloseHandle(FFileHandle);
  StopMirroring;
  if IsMirrorDriverActive then
    DesActivateVideoDriver;
end;

function TVNCVideoDriver.ExistMirrorDriver: Boolean;
var
  pd: TEnumDisplayDevices;
  DeviceMode: TDeviceMode;
  Module: THandle;
  DisplayDevice: TDisplayDevice;
  DeviceNum: Integer;
  RBool: Boolean;
  FarProc: TFarProc;
begin
  EnumDisplaySettings(nil, ENUM_CURRENT_SETTINGS, DeviceMode);
  DeviceMode.dmFields := DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
  Module := LoadLibrary('USER32');
  if Module <> 0 then
  begin
    FarProc := GetProcAddress(Module, 'EnumDisplayDevicesA');
    if FarProc = nil then
      exit;
    pd := TEnumDisplayDevices(FarProc);
    // ZeroMemory(@DisplayDevice,SizeOf(DisplayDevice));
    DisplayDevice.cb := SizeOf(DisplayDevice);
    DeviceMode.dmDeviceName[0] := #0;
    DeviceNum := 0;
    Result := pd(nil, DeviceNum, DisplayDevice, 0);
    while RBool do
    begin
      if (CompareStr(DisplayDevice.DeviceString, DriverName) = 0) then
        Break;
      Inc(DeviceNum);
      Result := pd(nil, DeviceNum, DisplayDevice, 0);
    end;
    if Module <> 0 then
      FreeLibrary(Module);
  end;
end;

function TVNCVideoDriver.GetDCMirror: HDC;
var
  RootDC: HDC;
  pd: TEnumDisplayDevices;
  ISFoundDriver: Boolean;
  DeviceMode: TDeviceMode;
  DisplayDevice: TDisplayDevice;
  Module: THandle;
  RBool: Boolean;
  DeviceNum: Integer;
begin
  RootDC := 0;
  EnumDisplaySettings(nil, ENUM_CURRENT_SETTINGS, DeviceMode);
  DeviceMode.dmFields := DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
  Module := LoadLibrary('USER32');
  if Module <> 0 then
  begin
    pd := TEnumDisplayDevices(GetProcAddress(Module, 'EnumDisplayDevicesA'));
    // ZeroMemory(@DisplayDevice,SizeOf(DisplayDevice));
    DisplayDevice.cb := SizeOf(TDisplayDevice);
    DeviceMode.dmDeviceName[0] := #0;
    DeviceNum := 0;
    ISFoundDriver := False;
    RBool := pd(nil, DeviceNum, DisplayDevice, 0);
    while RBool do
    begin
      if (CompareStr(DisplayDevice.DeviceString, DriverName) = 0) then
      begin
        ISFoundDriver := True;
        Break;
      end;
      Inc(DeviceNum);
      RBool := pd(nil, DeviceNum, DisplayDevice, 0);
    end;
    if (ISFoundDriver) then
    begin
      RootDC := CreateDC('DISPLAY', Pchar(DeviceMode.dmDeviceName[0]),
        nil, nil);
    end;
  end;
  if Module <> 0 then
    FreeLibrary(Module);
  Result := RootDC;
end;

function TVNCVideoDriver.GetDllProductVersion(var VBuffer: string;
  DllName: string; Size: Integer): Boolean;
var
  VersionInfo: string;
  FileName: string;
  lpVerBuffer: Pointer;
  VerBufferSize: DWORD;
  rBuffer: DWORD;
  sVersion: DWORD;
  RVersion, RValue: Boolean;
  InfoSize, wnd: DWORD;
begin
  Result := False;
  if (not((DllName <> '') and (VBuffer <> ''))) then
    exit;
  // GetModuleFileName(0,Pchar(FileName), sizeof(FileName));
  // FYI only
  InfoSize := GetFileVersionInfoSize(Pchar(DllName), wnd);
  if InfoSize = 0 then
    exit;
  SetLength(VersionInfo, InfoSize);
  GetFileVersionInfo(Pchar(DllName), wnd, InfoSize, Pchar(VersionInfo));
  RValue := VerQueryValue(Pchar(VersionInfo),
    Pchar('\StringFileInfo\040904b0\ProductVersion'), lpVerBuffer,
    VerBufferSize);
  if not RValue then
  begin
    RValue := VerQueryValue(Pchar(VersionInfo),
      Pchar('\StringFileInfo\000004b0\ProductVersion'), lpVerBuffer,
      VerBufferSize);
  end;
  if RValue then
  begin
    VBuffer := strpas(Pchar(lpVerBuffer));
    Result := True;
  end
  else
    VBuffer := '';
end;

function TVNCVideoDriver.HardWareCursor: Boolean;
var
  RInt: Integer;
begin
  GDC := GetDC(0);
  RInt := ExtEscape(GDC, CursorEN, 0, nil, 0, nil);
  ReleaseDC(0, GDC);
  if RInt > 0 then
    Result := True
  else
    Result := False;
end;

function TVNCVideoDriver.IsMirrorDriverActive: Boolean;
var
  pd: TEnumDisplayDevices;
  DeviceMode: TDeviceMode;
  DisplayDevice: TDisplayDevice;
  Module: THandle;
  RBool: Boolean;
  DeviceNum: Integer;
begin
  Result := False;
  EnumDisplaySettings(nil, ENUM_CURRENT_SETTINGS, DeviceMode);
  DeviceMode.dmFields := DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
  Module := LoadLibrary('USER32');
  if Module <> 0 then
  begin
    pd := TEnumDisplayDevices(GetProcAddress(Module, 'EnumDisplayDevicesA'));
    // ZeroMemory(@DisplayDevice,SizeOf(DisplayDevice));
    DisplayDevice.cb := SizeOf(DisplayDevice);
    DeviceMode.dmDeviceName[0] := #0;
    DeviceNum := 0;
    RBool := pd(nil, DeviceNum, DisplayDevice, 0);
    while RBool do
    begin
      if (CompareStr(DisplayDevice.DeviceString, DriverName) = 0) then
        Break;
      Inc(DeviceNum);
      RBool := pd(nil, DeviceNum, DisplayDevice, 0);
    end;
    if (DisplayDevice.StateFlags <> 0) then
      Result := True;
  end;
  if Module <> 0 then
    FreeLibrary(Module);
end;

function TVNCVideoDriver.NoHardWareCursor: Boolean;
var
  RInt: Integer;
begin
  GDC := GetDC(0);
  RInt := ExtEscape(GDC, CursorDIS, 0, nil, 0, nil);
  ReleaseDC(0, GDC);
  if RInt > 0 then
    Result := True
  else
    Result := False;
end;

function TVNCVideoDriver.RemoveCommunicationBuffer: TGetChangesBuf;
begin
  if FFileHandle <> 0 then
    CloseHandle(FFileHandle);
  // OutGetChangesBuf.Buffer:=Nil;
  // OutGetChangesBuf.UserBufferBegin:=Nil;
  // OutGetChangesBuf.UserBufferEnd:=Nil;
  Result := OutGetChangesBuf;
end;

procedure TVNCVideoDriver.RestoreReg(RegValue: Integer);
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    Registry.OpenKey
      ('SYSTEM\CurrentControlSet\Hardware Profiles\Current\System\CurrentControlSet\Services\vncdrv\DEVICE0',
      True);
    Registry.WriteInteger('Attach.ToDesktop', RegValue);
  finally
    Registry.Free;
  end;
end;

procedure TVNCVideoDriver.StartMirroring;
begin
  First := True;
  Oldaantal := 1;
  GDC := GetDC(0);
  DriverSucces := False;
  if ExtEscape(GDC, Map1, 0, nil, 0, nil) > 0 then
    DriverSucces := True;
  HardWareCursor;
  ReleaseDC(0, GDC);
  // We need also to check if the communication service is activer
  if FFileHandle = 0 then
  begin
    FFileHandle := CreateFile('\\.\VncIoctl', GENERIC_READ or GENERIC_WRITE, 0,
      nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
    if (FFileHandle = INVALID_HANDLE_VALUE) then
      DriverSucces := False
    else
    begin
      CloseHandle(FFileHandle);
      FFileHandle := 0;
    end;
  end;
end;

procedure TVNCVideoDriver.StopMirroring;
begin
  DriverSucces := False;
  GDC := GetDC(0);
  ExtEscape(GDC, UNMap1, 0, nil, 0, nil);
  ReleaseDC(0, GDC);
  First := True;
end;

function TVNCVideoDriver.Tempres: Boolean;
var
  pd: TEnumDisplayDevices;
  Module: THandle;
  i: DWORD;
  CurW, CurH, CurP, RegW, RegH, RegP: Integer;
  DisplayDevice: TDisplayDevice;
  DeviceMode: TDeviceMode;
  RBool: Boolean;
  DeviceName: string;
  DeviceNum: Integer;
begin
  Module := LoadLibrary('USER32');
  pd := TEnumDisplayDevices(GetProcAddress(Module, 'EnumDisplayDevicesA'));

  // ZeroMemory(@DisplayDevice,SizeOf(DisplayDevice));
  DisplayDevice.cb := SizeOf(TDisplayDevice);
  DeviceNum := 0;
  RBool := pd(nil, DeviceNum, DisplayDevice, 0);
  while RBool do
  begin
    if (DisplayDevice.StateFlags <> 0) then
    begin
      EnumDisplaySettings(Pchar(DisplayDevice.DeviceName[0]),
        ENUM_CURRENT_SETTINGS, DeviceMode);
      Break;
    end;
    Inc(DeviceNum);
    RBool := pd(nil, DeviceNum, DisplayDevice, 0);
  end;
  DeviceName := Pchar(DisplayDevice.DeviceName[0]);
  EnumDisplaySettings(Pchar(DeviceName), ENUM_CURRENT_SETTINGS, DeviceMode);
  CurW := DeviceMode.dmPelsWidth;
  CurH := DeviceMode.dmPelsHeight;
  CurP := DeviceMode.dmBitsPerPel;
  EnumDisplaySettings(Pchar(DeviceName), ENUM_REGISTRY_SETTINGS, DeviceMode);
  RegW := DeviceMode.dmPelsWidth;
  RegH := DeviceMode.dmPelsHeight;
  RegP := DeviceMode.dmBitsPerPel;
  if (CurW = RegW) and (CurH = RegH) and (CurP = RegP) then
    Result := False
  else
    Result := True;
end;

procedure TVNCVideoDriver.UpDateCommunicationBuffer;
var
  bRc: Boolean;
  BytesReturned: Cardinal;
  InGetChangesbuf: TGetChangesBuf;
begin
  bRc := False;
  BytesReturned := 0;
  InGetChangesbuf.Buffer := nil;
  InGetChangesbuf.UserBufferBegin := nil;
  InGetChangesbuf.UserBufferEnd := nil;
  bRc := DeviceIoControl(FFileHandle, DWORD(IOCTL_SIOCTL_METHOD_BUFFERED),
    @InGetChangesbuf, SizeOf(TGetChangesBuf), @OutGetChangesBuf,
    SizeOf(TGetChangesBuf), BytesReturned, nil);
end;

end.
