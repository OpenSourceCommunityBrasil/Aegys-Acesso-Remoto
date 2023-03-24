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
  System.Classes,
  System.Types,
  FMX.Forms,
  Vcl.Imaging.jpeg,
  Execute.DesktopDuplicationAPI,
  FMX.Objects
  ,Vcl.Graphics
  ,Winapi.Windows
  ,Vcl.Forms
  , uAegysBufferPack,
  FMX.Surfaces;

Const
 TNeutroColor = 255;
 cJPGQual     = 25;

Type
  TRGBTriple = Packed Record
    B: Byte;
    G: Byte;
    R: Byte;
  End;

Type
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = Array [0 .. 4095] of TRGBTriple;

Var
 aFullBmp : Vcl.Graphics.TBitmap;

procedure GetScreenToMemoryStream(Var aPackClass     : TPackClass;
                                  DrawCur            : Boolean;
                                  PixelFormat        : TPixelFormat = pf15bit;
                                  Monitor            : String       = '0';
                                  FullFrame          : Boolean      = False);

procedure ResizeBmp(AImage: TImage; AStream: TMemoryStream; AWidth, AHeight: Single);

Var
 pdst     : Pointer;
 ASMSize,
 muASM    : Integer;
 cpdst     : Pointer;
 cASMSize,
 cmuASM    : Integer;

implementation

uses
  System.SysUtils, uFormConexao, uAegysZlib, uAegysDataTypes;

procedure ResizeBmp(AImage: TImage; AStream: TMemoryStream; AWidth, AHeight: Single);
var
  bmpSrc, bmpDest: Vcl.Graphics.TBitmap;
  msImage: TMemoryStream;
begin
  try
    msImage := TMemoryStream.Create;
    msImage.Clear;
    bmpSrc := Vcl.Graphics.TBitmap.Create;
    bmpDest := Vcl.Graphics.TBitmap.Create;
    AStream.Position := 0;
    bmpSrc.LoadFromStream(AStream);
    bmpDest.Width := Trunc(AWidth);
    bmpDest.Height := Trunc(AHeight);
    SetStretchBltMode(bmpDest.Canvas.Handle, HALFTONE);
    StretchBlt(
      bmpDest.Canvas.Handle,
      0,
      0,
      bmpDest.Width,
      bmpDest.Height,
      bmpSrc.Canvas.Handle,
      0,
      0,
      bmpSrc.Width,
      bmpSrc.Height,
      SRCCOPY);
    bmpDest.SaveToStream(msImage);
    msImage.Position := 0;
    AImage.Bitmap.LoadFromStream(msImage);
  finally
    FreeAndNil(msImage);
    bmpSrc.Free;
    bmpDest.Free;
  end;
end;

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
                                  PixelFormat        : TPixelFormat = pf15bit;
                                  Monitor            : String       = '0';
                                  FullFrame          : Boolean      = False);
Var
  Mybmp,
  MybmpPart      : Vcl.Graphics.TBitmap;
  aPackCountData : AeInteger;
  I,
  pRectTop,
  pRectLeft,
  pRectBottom,
  pRectRight,
  vMonitor    : Integer;
  aSizeData   : AeInt64;
  vFirstImg,
  vNewFrame,
  vMultiPoint : Boolean;
  aFinalBytes,
  aBytes,
  aPackBytes    : TAegysBytes;
  aMonitor,
  aResolution   : String;
  TargetMemoryStream,
  aMemoryStream : TStream;
  vBitmapSurface : TBitmapSurface;
  vSaveParams    : FMX.Graphics.TBitmapCodecSaveParams;
  vBmptTmp       : FMX.Graphics.TBitmap;
  Procedure GetShot;
  Const
   CAPTUREBLT = $40000000;
  Var
   DesktopCanvas : TCanvas;
   Procedure ScreenShoot;
   Var
    DC    : HDC;
   begin
    DC    := GetDC(0);
    Try
     DesktopCanvas := TCanvas.Create;
     DesktopCanvas.Handle := DC;
     Mybmp.SetSize(Screen.Width,
                   Screen.Height);
     Mybmp.PixelFormat := TPixelFormat.pf24bit;
     BitBlt(Mybmp.Canvas.Handle, 0, 0, Screen.Width, Screen.Height, DesktopCanvas.Handle, 0, 0, SRCCOPY or CAPTUREBLT);
    Finally
     ReleaseDC(0, DC);
     FreeAndNil(DesktopCanvas);
     Application.Processmessages;
    End;
   End;
  Begin
   ScreenShoot;
  End;
  procedure VCLtoFMX_Bitmap(const VCLBmp : VCL.Graphics.TBitmap ; out FMXBmp : FMX.Graphics.TBitmap);
  Var
   bData : FMX.Graphics.TBitmapData;
   x, y : Integer;
   pfmxbyte, pvclbyte : PByte;
  Begin
   VCLBmp.PixelFormat := pf24bit;
   FMXBmp.SetSize(VCLBmp.Width, VCLBmp.Height);
   FMXBmp.Map(FMX.Graphics.TMapAccess.ReadWrite, bdata);
   Try
    For y := 0 To FMXBmp.Height - 1 Do
     Begin
      pfmxbyte := bdata.GetScanline(y);
      pvclbyte := VCLBmp.Scanline[y];
      For x := 0 To FMXBmp.Width - 1 Do
       Begin
        pfmxbyte^ := pvclbyte^; Inc(pvclbyte); Inc(pfmxbyte);
        pfmxbyte^ := pvclbyte^; Inc(pvclbyte); Inc(pfmxbyte);
        pfmxbyte^ := pvclbyte^; Inc(pvclbyte); Inc(pfmxbyte);
        pfmxbyte^ := $FF; Inc(pfmxbyte); // Full opacity
       End;
      Application.Processmessages;
     End;
   Finally
    FMXBmp.Unmap(bdata);
   End;
  End;
Begin
 Mybmp      := Nil;
 aPackClass := Nil;
 vMonitor := StrToInt(Monitor) +1;
 aMonitor := IntToStr(FMX.Forms.Screen.DisplayCount);
 If (vMonitor > FMX.Forms.Screen.DisplayCount) then
  Exit;
 vMonitor := vMonitor -1;
 aResolution := Format('%s&%s&%s', [FloatToStr(Screen.Monitors[vMonitor].Height), FloatToStr(Screen.Monitors[vMonitor].Width), aMonitor]);
 Try
  vMultiPoint := False;
  If Not cRFB Then
   Begin
    vNewFrame   := FDuplication.GetFrame(False) Or (FullFrame);
    If vNewFrame Then
     Begin
      Mybmp := Vcl.Graphics.TBitmap.Create;
      FDuplication.DrawFrame(Mybmp, PixelFormat);
     End;
   End
  Else
   Begin
    Mybmp := Vcl.Graphics.TBitmap.Create;
    GetShot;
    vNewFrame := True;
    Application.Processmessages;
   End;
 Finally
 End;
 If vNewFrame Then
  Begin
   If Not cRFB Then
    If DrawCur Then
     DrawScreenCursor(Mybmp, StrToInt(Monitor));
   TargetMemoryStream := TMemoryStream.Create;
   If Assigned(aFullBmp) Then
    FreeAndNil(aFullBmp);
   vBitmapSurface      := TBitmapSurface.Create;
   vBmptTmp            := FMX.Graphics.TBitmap.Create;
   VCLtoFMX_Bitmap(Mybmp, vBmptTmp);
   FreeAndNil(Mybmp);
   vBitmapSurface.Assign(TBitmapSurface(vBmptTmp));
   FreeAndNil(vBmptTmp);
   FreeAndNil(TargetMemoryStream);
   vSaveParams.Quality := cJPGQual;
   TargetMemoryStream  := TMemoryStream.Create;
   FMX.Graphics.TBitmapCodecManager.SaveToStream(TargetMemoryStream, vBitmapSurface, '.jpg', @vSaveParams);
   If Assigned(vBitmapSurface) then
    FreeAndNil(vBitmapSurface);
   TargetMemoryStream.position := 0;
   If TargetMemoryStream.Size > 0 then
    Begin
//     ZCompressStreamBytes(TargetMemoryStream, aFinalBytes);
     SetLength(aFinalBytes, TargetMemoryStream.Size);
     TargetMemoryStream.Read(aFinalBytes[0], Length(aFinalBytes));
     aPackClass  := TPackClass.Create;
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
      aPackClass.BytesOptions := aResolution;
      aPackClass.Owner        := Conexao.Connection;
      aPackClass.Dest         := '';
     Finally
     End;
    End;
   FreeAndNil(TargetMemoryStream);
  End;
 If cRFB Then
  FAegysVarredura.FilaImagens.Clear;
End;

Initialization
 aFullBmp := Vcl.Graphics.TBitmap.Create;

Finalization
 FreeAndNil(aFullBmp);

end.

