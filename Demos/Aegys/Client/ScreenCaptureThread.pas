unit ScreenCaptureThread;

Interface

uses
 Windows, Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, SyncObjs;

Const
 TCaptureTimeThread = 3;

Type
 TCopyThread = Class(TThread)
Private
 FIndex          : DWORD;
 FScrBmp         : TBitmap;
 FTerminateEvent : TEvent;
Protected
 procedure Execute; Override;
Public
 Constructor Create;  Reintroduce;
 Destructor  Destroy; Override;
End;

TCopyDeskService = Class(TService)
 Procedure ServiceCreate (Sender : TObject);
 Procedure ServiceDestroy(Sender : TObject);
Private
 FCopyThread : TCopyThread;
Public
 Function GetServiceController: TServiceController; Override;
End;

Var
 CopyDeskService : TCopyDeskService;

Implementation

Procedure ServiceController(CtrlCode: DWord); Stdcall;
Begin
 CopyDeskService.Controller(CtrlCode);
End;

Function TCopyDeskService.GetServiceController : TServiceController;
Begin
 Result := ServiceController;
End;

Procedure TCopyDeskService.ServiceCreate(Sender: TObject);
Begin
 FCopyThread := TCopyThread.Create;
End;

Procedure TCopyDeskService.ServiceDestroy(Sender: TObject);
Begin
 FCopyThread.Terminate;
End;

Function SelectHDESK(HNewDesk : HDESK): Boolean; Stdcall;
Var
 HOldDesk: HDESK;
 dwDummy: DWORD;
 sName: Array[0..255] Of Char;
Begin
 Result   := False;
 HOldDesk := GetThreadDesktop(GetCurrentThreadId);
 If (not GetUserObjectInformation(HNewDesk, UOI_NAME, @sName[0], 256, dwDummy)) Then
  Begin
   OutputDebugString('GetUserObjectInformation Failed.');
   Exit;
  End;
 If (Not SetThreadDesktop(HNewDesk)) Then
  Begin
   OutputDebugString('SetThreadDesktop Failed.');
   Exit;
  End;
 If (Not CloseDesktop(HOldDesk)) then
  Begin
   OutputDebugString('CloseDesktop Failed.');
   Exit;
  End;
 Result := True;
End;

Function SelectDesktop(pName : PChar): Boolean; stdcall;
Var
 HDesktop: HDESK;
Begin
 Result := False;
 If Assigned(pName) Then
  HDesktop := OpenDesktop(pName, 0, False,
                          DESKTOP_CreateMENU or DESKTOP_CreateWINDOW or
                          DESKTOP_ENUMERATE or DESKTOP_HOOKCONTROL or
                          DESKTOP_WRITEOBJECTS or DESKTOP_READOBJECTS or
                          DESKTOP_SWITCHDESKTOP or GENERIC_WRITE)
 Else
  HDesktop := OpenInputDesktop(0, False,
                               DESKTOP_CreateMENU or DESKTOP_CreateWINDOW or
                               DESKTOP_ENUMERATE or DESKTOP_HOOKCONTROL or
                               DESKTOP_WRITEOBJECTS or DESKTOP_READOBJECTS or
                               DESKTOP_SWITCHDESKTOP or GENERIC_WRITE);
 If (HDesktop = 0) Then
  Begin
   OutputDebugString(PChar('Get Desktop Failed: ' + IntToStr(GetLastError)));
   Exit;
  End;
 Result := SelectHDESK(HDesktop);
End;

Function InputDesktopSelected : Boolean; stdcall;
Var
 HThdDesk,
 HInpDesk  : HDESK;
 dwError,
 dwDummy   : DWORD;
 sThdName,
 sInpName  : Array[0..255] Of Char;
Begin
 Result   := False;
 HThdDesk := GetThreadDesktop(GetCurrentThreadId);
 HInpDesk := OpenInputDesktop(0, False,
                              DESKTOP_CreateMENU or DESKTOP_CreateWINDOW or
                              DESKTOP_ENUMERATE or DESKTOP_HOOKCONTROL or
                              DESKTOP_WRITEOBJECTS or DESKTOP_READOBJECTS or
                              DESKTOP_SWITCHDESKTOP);
 If (HInpDesk = 0) Then
  Begin
   OutputDebugString('OpenInputDesktop Failed.');
   dwError := GetLastError;
   Result := (dwError = 170);
   Exit;
  End;
 If (not GetUserObjectInformation(HThdDesk, UOI_NAME, @sThdName[0], 256, dwDummy)) Then
  Begin
   OutputDebugString('GetUserObjectInformation HThdDesk Failed.');
   CloseDesktop(HInpDesk);
   Exit;
  End;
 If (Not GetUserObjectInformation(HInpDesk, UOI_NAME, @sInpName[0], 256, dwDummy)) Then
  Begin
   OutputDebugString('GetUserObjectInformation HInpDesk Failed.');
   CloseDesktop(HInpDesk);
   Exit;
  End;
 CloseDesktop(HInpDesk);
 Result := (lstrcmp(sThdName, sInpName) = 0);
End;

Procedure CopyScreen(Bmp: TBitmap; out Index: DWORD);
Var
 DC : HDC;
Begin
 DC := GetDC(0);
 Bmp.Width := GetSystemMetrics(SM_CXSCREEN);
 Bmp.Height := GetSystemMetrics(SM_CYSCREEN);
 Bmp.Canvas.Lock;
 Try
  BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, DC, 0, 0, SRCCOPY);
  Bmp.SaveToFile('C:\temp\' + IntToStr(Index) + '.bmp');
  Inc(Index);
 Finally
  Bmp.Canvas.Unlock;
  ReleaseDC(0, DC);
 End;
End;

Constructor TCopyThread.Create;
Begin
 Inherited Create(False);
 FreeOnTerminate     := True;
 FScrBmp := TBitmap.Create;
 FScrBmp.PixelFormat := pf8bit;
 FIndex              := 0;
 FTerminateEvent     := TEvent.Create(Nil, True, False, 'ListenThreadEvent');
End;

Destructor TCopyThread.Destroy;
Begin
 If FTerminateEvent <> Nil Then
  FreeAndNil(FTerminateEvent);
 FScrBmp.Free;
 FScrBmp := Nil;
 Inherited;
End;

Procedure TCopyThread.Execute;
Begin
 While (Not Terminated) Do
  Begin
   If InputDesktopSelected Then
    CopyScreen(FScrBmp, FIndex)
   Else If SelectDesktop(nil) Then
    CopyScreen(FScrBmp, FIndex);
   FTerminateEvent.WaitFor(TCaptureTimeThread);
  End;
End;

End.
