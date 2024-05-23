library AegysData;

uses
  Sharemem,
  Windows,
  System.Classes,
  System.SysUtils,
  UDesktopDuplication in 'UDesktopDuplication.pas';

Var
 DesktopDuplication : TVCLDesktopDuplication = Nil;

{$R *.res}

Function CaptureScreen(aMonitor : Real = 0) : TStream; StdCall; Export;
Begin
 Result := Nil;
 If Not Assigned(DesktopDuplication) Then
  DesktopDuplication := TVCLDesktopDuplication.CreateParamMonitor(Round(aMonitor));
 If DesktopDuplication.vActiveMonitor <> Round(aMonitor) Then
  Begin
   FreeAndNil(DesktopDuplication);
   DesktopDuplication := TVCLDesktopDuplication.CreateParamMonitor(Round(aMonitor));
  End;
 If DesktopDuplication.FrameGet(10) then
  Begin
   Result := TMemoryStream.Create;
   Result.CopyFrom(DesktopDuplication.vImageStream, DesktopDuplication.vImageStream.Size);
   Result.Position := 0;
  End;
End;

Exports
 CaptureScreen;

procedure DLLMain(dwReason: DWORD);
Begin
 Case dwReason of
  DLL_PROCESS_ATTACH : Begin

                       End; {= DLL_PROCESS_ATTACH =}
  DLL_PROCESS_DETACH : Begin
                        If Assigned(DesktopDuplication) Then
                         FreeAndNil(DesktopDuplication);
                       End;{= DLL_PROCESS_DETACH =}
 End; {= case =}
End; {= DLLMain =}

Begin
 DLLProc := @DLLMain;
 DLLMain(DLL_PROCESS_ATTACH);
end.

