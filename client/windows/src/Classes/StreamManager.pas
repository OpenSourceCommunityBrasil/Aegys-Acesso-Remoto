unit StreamManager;

{
 Project Aegys Remote Support.

   Created by Gilberto Rocha da Silva in 04/05/2017 based on project Allakore, has by objective to promote remote access
 and other resources freely to all those who need it, today maintained by a beautiful community. Listing below our
 higly esteemed collaborators:

  Gilberto Rocha da Silva (XyberX) (Creator of Aegys Project/Main Developer/Admin)
  Wendel Rodrigues Fassarella (wendelfassarella) (Creator of Aegys FMX/CORE Developer)
  Rai Duarte Jales (Raí Duarte) (Aegys Server Developer)
  Roniery Santos Cardoso (Aegys Developer)
  Alexandre Carlos Silva Abade (Aegys Developer)
  Mobius One (Aegys Developer)
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
  , uAegysBufferPack;

Const
  TNeutroColor = 255;
  cCaptureJPG  = False;

Type
  TRGBTriple = Packed Record
    B: Byte;
    G: Byte;
    R: Byte;
  End;

Type
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = Array [0 .. 4095] of TRGBTriple;

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
  JPG            : Vcl.Imaging.jpeg.TJPegImage;
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
  vNewFrame,
  vMultiPoint : Boolean;
  aFinalBytes,
  aBytes,
  aPackBytes    : TAegysBytes;
  aMonitor,
  aResolution   : String;
  TargetMemoryStream,
  aMemoryStream : TMemoryStream;
  Procedure BitmapToJpg(Var Bmp : Vcl.Graphics.TBitmap;
                        Var JPG : Vcl.Imaging.jpeg.TJPegImage;
                        Quality : Integer = 15);
  Begin
   Try
    JPG := TJPegImage.Create;
    Try
//     JPG.CompressionQuality := Quality;
     JPG.Assign(BMP);
//     JPG.Compress;
    Finally
    End;
   Finally
    FreeAndNil(BMP);
   End;
  End;
Begin
 aPackClass := Nil;
 JPG        := Nil;
 Mybmp := Vcl.Graphics.TBitmap.Create;
 vMonitor := StrToInt(Monitor) +1;
 aMonitor := IntToStr(FMX.Forms.Screen.DisplayCount);
 If (vMonitor > FMX.Forms.Screen.DisplayCount) then
  Exit;
 vMonitor := vMonitor -1;
 aResolution := Format('%s&%s&%s', [FloatToStr(Screen.Monitors[vMonitor].Height), FloatToStr(Screen.Monitors[vMonitor].Width), aMonitor]);
 Try
  vMultiPoint := False;
  vNewFrame   := FDuplication.GetFrame;
  If vNewFrame Then
   Begin
    FDuplication.DrawFrame(Mybmp, PixelFormat);
    If Not FullFrame Then
     Begin
      vMultiPoint := FDuplication.DirtyCount >= 1;
      If vMultiPoint Then
       Begin
        If FDuplication.DirtyCount = 1 Then
         Begin
         {$POINTERMATH ON}
          pRectTop    := FDuplication.DirtyRects[0].Top;
          pRectLeft   := FDuplication.DirtyRects[0].Left;
          pRectBottom := FDuplication.DirtyRects[0].Bottom;
          pRectRight  := FDuplication.DirtyRects[0].Right;
          vMultiPoint := Not((pRectTop   = 0) And
                             (pRectLeft  = 0) And
                             (pRectRight  = Screen.Monitors[vMonitor].Width) And
                             (pRectBottom = Screen.Monitors[vMonitor].Height));
         End;
       End;
     End;
   End;
 Finally
 End;
 If vNewFrame Then
  Begin
   If DrawCur Then
    DrawScreenCursor(Mybmp, StrToInt(Monitor));
   If Not vMultiPoint Then
    Begin
     TargetMemoryStream := TMemoryStream.Create;
     If cCaptureJPG Then
      Begin
       BitmapToJpg(Mybmp, JPG);
       JPG.SaveToStream(TargetMemoryStream);
       If Assigned(JPG) Then
        FreeAndNil(JPG);
      End
     Else
      Mybmp.SaveToStream(TargetMemoryStream);
     TargetMemoryStream.position := 0;
     If TargetMemoryStream.Size > 0 then
      Begin
       SetLength(aBytes, TargetMemoryStream.Size);
       TargetMemoryStream.Read(aBytes[0], Length(aBytes));
       ZCompressBytes(aBytes, aFinalBytes);
       SetLength(aBytes, 0);
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
    End
   Else
    Begin
     aMemoryStream  := TMemoryStream.Create;
     aPackCountData := FDuplication.DirtyCount;
     aMemoryStream.Write(aPackCountData, SizeOf(aPackCountData));
     For I := 0 To FDuplication.DirtyCount -1 Do
      Begin
       With FDuplication.DirtyRects[I] Do
        Begin
         pRectTop    := Top;
         pRectLeft   := Left;
         pRectBottom := Bottom;
         pRectRight  := Right;
         MybmpPart := Vcl.Graphics.TBitmap.Create;
         Try
          MybmpPart.SetSize(pRectRight  - pRectLeft,
                            pRectBottom - pRectTop);
          MybmpPart.PixelFormat := PixelFormat;
          BitBlt(MybmpPart.Canvas.Handle, 0, 0,
                 pRectRight,              pRectBottom,
                 Mybmp.Canvas.Handle,     pRectLeft,
                 pRectTop,                SRCCOPY);
          TargetMemoryStream := TMemoryStream.Create;
          If cCaptureJPG Then
           Begin
            BitmapToJpg(MybmpPart, JPG);
            JPG.SaveToStream(TargetMemoryStream);
           End
          Else
           MybmpPart.SaveToStream(TargetMemoryStream);
          If TargetMemoryStream.Size > 0 Then
           Begin
            TargetMemoryStream.Position := 0;
            aSizeData                   := TargetMemoryStream.Size;
            aMemoryStream.Write   (pRectTop,           SizeOf(AeInteger));
            aMemoryStream.Write   (pRectLeft,          SizeOf(AeInteger));
            aMemoryStream.Write   (pRectBottom,        SizeOf(AeInteger));
            aMemoryStream.Write   (pRectRight,         SizeOf(AeInteger));
            aMemoryStream.Write   (aSizeData,          SizeOf(aSizeData));
            aMemoryStream.CopyFrom(TargetMemoryStream, aSizeData);
           End;
         Finally
          If Assigned(JPG) Then
           FreeAndNil(JPG);
          FreeAndNil(MybmpPart);
          FreeAndNil(TargetMemoryStream);
         End;
        End;
      End;
     aMemoryStream.Position  := 0;
     aPackClass              := TPackClass.Create;
     Try
      aPackClass.DataCheck    := tdcAsync;
      aPackClass.ProxyToMyConnectionList := True;
      aPackClass.PacksGeral   := aPackCountData;
      aPackClass.PackNo       := 1;
      aPackClass.DataMode     := tdmClientCommand;
      aPackClass.DataType     := tdtDataBytes;
      aPackClass.CommandType  := tctScreenCapture;
      SetLength(aBytes, aMemoryStream.Size);
      aMemoryStream.Read(aBytes[0], Length(aBytes));
      ZCompressBytes(aBytes, aFinalBytes);
      SetLength(aBytes, 0);
      aPackClass.DataSize     := Length(aFinalBytes);
      aPackClass.BufferSize   := aPackClass.DataSize;
      aPackClass.DataBytes    := aFinalBytes;
      aPackClass.BytesOptions := aResolution;
      aPackClass.Owner        := Conexao.Connection;
      aPackClass.Dest         := '';
     Finally
      SetLength(aFinalBytes, 0);
      FreeAndNil(aMemoryStream);
     End;
    End;
  End;
 If Assigned(Mybmp) Then
  FreeAndNil(Mybmp);
End;

end.

