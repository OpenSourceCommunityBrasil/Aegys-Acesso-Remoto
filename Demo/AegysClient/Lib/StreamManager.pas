unit StreamManager;

{
   Aegys Remote Access Project.
  Criado por XyberX (Gilbero Rocha da Silva), o Aegys Remote Access Project tem como objetivo o uso de Acesso remoto
  Gratuito para utilização de pessoas em geral.
   O Aegys Remote Access Project tem como desenvolvedores e mantedores hoje

  Membros do Grupo :

  XyberX (Gilberto Rocha)    - Admin - Criador e Administrador  do pacote.
  Wendel Fassarela           - Devel and Admin
  Mobius One                 - Devel, Tester and Admin.
  Gustavo                    - Devel and Admin.
  Roniery                    - Devel and Admin.
  Alexandre Abbade           - Devel and Admin.
  e Outros como você, venha participar também.
}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  FMX.Forms,
//  Execute.DesktopDuplicationAPI,
  FMX.Objects
  ,Vcl.Graphics
  ,Winapi.Windows
  ,Vcl.Forms
  , uAegysBufferPack,
  ActiveX,
  FMX.Graphics,
  FMX.Surfaces,
  uASMTools;

Const
 TNeutroColor     = 255;
 cJPGQual         = 20;
 cCompressionData = True;

Type
 TCaptureScreenProc = Function (aMonitor : Real = 0) : TStream;

Var
 DllHandle         : THandle;
 vActualImage      : TMemoryStream;
 CaptureScreenProc : TCaptureScreenProc = Nil;
 aConnection       : String;
 hDesktop          : HDESK;

procedure GetScreenToMemoryStream(Var aPackClass     : TPackClass;
                                  DrawCur            : Boolean;
                                  PixelFormat        : TPixelFormat = pf16bit;
                                  Monitor            : String       = '0';
                                  FullFrame          : Boolean      = False);
Function StreamToBytes           (Stream             : TStream)     : TBytes;
Function StreamToString          (Stream             : TStream) : AnsiString;

implementation

uses
  uAegysZlib, uAegysDataTypes;

Function StreamToBytes(Stream : TStream) : TBytes;
Var
 aSize : Longint;
Begin
 If Assigned(Stream) Then
  If Stream.Size > 0 Then
   Begin
    aSize := Stream.Size;
    Stream.Position := 0;
    SetLength(Result, aSize);
    Stream.Read(Result[0], aSize);
   End;
End;

Function StreamToString(Stream : TStream) : AnsiString;
Var
 aSize : Longint;
Begin
 If Assigned(Stream) Then
  If Stream.Size > 0 Then
   Begin
    aSize := Stream.Size;
    Stream.Position := 0;
    SetLength(Result, aSize);
    Stream.Read(Result[1], aSize);
   End;
End;

Procedure DrawScreenCursor(Var Bmp: Vcl.Graphics.TBitmap; const MonitorID: Integer);
Var
 R          : TRect;
 CursorInfo : TCursorInfo;
 Left,
 Top        : Integer;
 Icon       : TIcon;
 IconInfo   : TIconInfo;
Begin
 R    := Bmp.Canvas.ClipRect;
 Icon := TIcon.Create;
 Try
  CursorInfo.cbSize := SizeOf(CursorInfo);
  If GetCursorInfo(CursorInfo) Then
   If CursorInfo.Flags = CURSOR_SHOWING Then
    Begin
     Icon.Handle:= CopyIcon(CursorInfo.hCursor);
     If GetIconInfo(Icon.Handle, IconInfo) Then
      Begin
       If CursorInfo.ptScreenPos.x > Screen.Monitors[MonitorID].Left Then
        Left := CursorInfo.ptScreenPos.x - Screen.Monitors[MonitorID].Left
       Else
        Left := CursorInfo.ptScreenPos.x;
       If CursorInfo.ptScreenPos.y > Screen.Monitors[MonitorID].Top  Then
        Top  := CursorInfo.ptScreenPos.y - Screen.Monitors[MonitorID].Top
       Else
        Top  := CursorInfo.ptScreenPos.y;
       Bmp.Canvas.Draw(Left - Integer(IconInfo.xHotspot) - R.Left,
                       Top  - Integer(IconInfo.yHotspot) - R.Top,
                       Icon);
      End;
    End;
 Finally
  Icon.Free;
 End;
End;

procedure GetScreenToMemoryStream(Var aPackClass     : TPackClass;
                                  DrawCur            : Boolean;
                                  PixelFormat        : TPixelFormat = pf16bit;
                                  Monitor            : String       = '0';
                                  FullFrame          : Boolean      = False);
Var
  aFinalBytes         : TAegysBytes;
  vMonitor            : Integer;
  vResultMemoryStream : TMemoryStream;
  aMonitor,
  aResolution         : String;
  TargetMemoryStream  : TStream;
Begin
 aPackClass := Nil;
 vMonitor   := StrToInt(Monitor) +1;
 aMonitor   := IntToStr(FMX.Forms.Screen.DisplayCount);
 If (vMonitor > FMX.Forms.Screen.DisplayCount) then
  Exit;
 vMonitor := vMonitor -1;
 aResolution := Format('%s&%s&%s', [FloatToStr(Screen.Monitors[vMonitor].Height), FloatToStr(Screen.Monitors[vMonitor].Width), aMonitor]);
 Try
  // EUREKA: This is the responsable to interact with UAC. But we need run
  // the software on SYSTEM account to work.
  Application.ProcessMessages;
  Try
   hDesktop := OpenInputDesktop(0, True, MAXIMUM_ALLOWED);
   If hDesktop <> 0 then
    Begin
     SetThreadDesktop(hDesktop);
     CloseHandle(hDesktop);
    End;
  Except
  End;
  TargetMemoryStream := CaptureScreenProc(vMonitor);
  If Not Assigned(TargetMemoryStream) Then
   Exit;
//  If vActualImage.Size = 0 Then
//   Begin
//    TargetMemoryStream.Position := 0;
//    vActualImage.CopyFrom(TargetMemoryStream, TargetMemoryStream.Size);
  Application.ProcessMessages;
  TargetMemoryStream.Position := 0;
//   End
//  Else
//   Begin
//    TargetMemoryStream.Position := 0;
//    vActualImage.Position := 0;
//    vResultMemoryStream   := TMemoryStream.Create;
//    If CompareStreamData(vActualImage, TMemoryStream(TargetMemoryStream), vResultMemoryStream) then
//     Begin
//      vActualImage.Clear;
//      vResultMemoryStream.Position := 0;
//      vActualImage.CopyFrom(vResultMemoryStream, vResultMemoryStream.Size);
//      TmemoryStream(TargetMemoryStream).Clear;
//      vActualImage.Position := 0;
//      TargetMemoryStream.CopyFrom(vActualImage, vActualImage.Size);
//      TargetMemoryStream.Position := 0;
//     End
//    Else
//     FreeAndNil(TargetMemoryStream);
//    FreeAndNil(vResultMemoryStream);
//   End;
 Except
 End;
 If Assigned(TargetMemoryStream) Then
  Begin
   TargetMemoryStream.Position := 0;
   If TargetMemoryStream.Size > 0 then
    Begin
//     Processmessages;
     If cCompressionData Then
      ZCompressStreamBytes(TargetMemoryStream, aFinalBytes)
     Else
      Begin
       SetLength(aFinalBytes, TargetMemoryStream.Size);
       TargetMemoryStream.Read(aFinalBytes[0], Length(aFinalBytes));
      End;
//     Application.ProcessMessages;
     FreeAndNil(TargetMemoryStream);
     aPackClass               := TPackClass.Create;
     Try
      aPackClass.DataCheck    := tdcAsync;
      aPackClass.DataSize     := Length(aFinalBytes);
      aPackClass.ProxyToMyConnectionList := True;
      aPackClass.BufferSize   := aPackClass.DataSize;
      aPackClass.PacksGeral   := 0;
      aPackClass.PackNo       := 0;
      aPackClass.DataMode     := tdmClientCommand;
      aPackClass.DataType     := tdtDataBytes;
      aPackClass.CommandType  := tctScreenCapture;
      aPackClass.DataBytes    := aFinalBytes;
      SetLength(aFinalBytes, 0);
      aPackClass.BytesOptions := aResolution;
      aPackClass.Owner        := aConnection;
      aPackClass.Dest         := '';
     Finally
     End;
    End
   Else
    FreeAndNil(TargetMemoryStream);
   Application.ProcessMessages;
  End;
End;

Initialization
 vActualImage := TMemoryStream.Create;
 DllHandle := LoadLibrary('AegysData.dll');
 If DllHandle > 0 Then
  @CaptureScreenProc := GetProcAddress(DllHandle, 'CaptureScreen');

Finalization
 FreeAndNil(vActualImage);
 If DLLHandle <> 0 Then
  FreeLibrary(DLLHandle);

End.

