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
  {$IF DEFINED (ANDROID) || (IOS)}
  ,FMX.Types
  ,FMX.Graphics
  ,FMX.Platform
  {$ENDIF}
  {$IF DEFINED (MSWINDOWS)}
  ,Vcl.Graphics
  ,Winapi.Windows
  ,Vcl.Forms
  {$ENDIF}
  ;

Const
  TNeutroColor = 255;

Type
  TRGBTriple = Packed Record
    B: Byte;
    G: Byte;
    R: Byte;
  End;

Type
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = Array [0 .. 4095] of TRGBTriple;

{$IF DEFINED (ANDROID) || (IOS)}
procedure GetScreenToMemoryStream(DrawCur            : Boolean;
                                  TargetMemoryStream : TMemoryStream;
                                  PixelFormat        : TPixelFormat = TPixelFormat.None;
                                  Monitor            : String       = '0');
{$ENDIF}
{$IF DEFINED (MSWINDOWS)}
procedure GetScreenToMemoryStream(DrawCur            : Boolean;
                                  TargetMemoryStream : TMemoryStream;
                                  PixelFormat        : TPixelFormat = pfDevice;
                                  Monitor            : String       = '0');
{$ENDIF}

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
  System.SysUtils, uFormConexao;

procedure ResizeBmp(AImage: TImage; AStream: TMemoryStream; AWidth, AHeight: Single);
var
  {$IF DEFINED (ANDROID) || (IOS)}
  bmpSrc, bmpDest: FMX.Graphics.TBitmap;
  {$ENDIF}
  {$IF DEFINED (MSWINDOWS)}
  bmpSrc, bmpDest: Vcl.Graphics.TBitmap;
  {$ENDIF}

  msImage: TMemoryStream;
begin
  try
    msImage := TMemoryStream.Create;
    msImage.Clear;
    {$IF DEFINED (ANDROID) || (IOS)}
    bmpSrc := FMX.Graphics.TBitmap.Create;
    bmpDest := FMX.Graphics.TBitmap.Create;
    {$ENDIF}
    {$IF DEFINED (MSWINDOWS)}
    bmpSrc := Vcl.Graphics.TBitmap.Create;
    bmpDest := Vcl.Graphics.TBitmap.Create;
    {$ENDIF}

    AStream.Position := 0;
    bmpSrc.LoadFromStream(AStream);
    bmpDest.Width := Trunc(AWidth);
    bmpDest.Height := Trunc(AHeight);

    {$IF DEFINED (ANDROID) || (IOS)}
     bmpDest  :=  bmpSrc;
     bmpDest.Resize(AWidth, AHeight);
    {$ENDIF}
    {$IF DEFINED (MSWINDOWS)}
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
    {$ENDIF}

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

{$IF DEFINED (ANDROID) || (IOS)}
procedure GetScreenToMemoryStream(DrawCur            : Boolean;
                                  TargetMemoryStream : TMemoryStream;
                                  PixelFormat        : TPixelFormat = TPixelFormat.None;
                                  Monitor            : String       = '0');
{$ENDIF}
{$IF DEFINED (MSWINDOWS)}
procedure GetScreenToMemoryStream(DrawCur            : Boolean;
                                  TargetMemoryStream : TMemoryStream;
                                  PixelFormat        : TPixelFormat = pfDevice;
                                  Monitor            : String       = '0');
{$ENDIF}
Var
  JPG : Vcl.Imaging.jpeg.TJPegImage;
  {$IF DEFINED (ANDROID) || (IOS)}
  Mybmp: FMX.Graphics.TBitmap;
  dc: Cardinal;
  {$ENDIF}
  {$IF DEFINED (MSWINDOWS)}
  Mybmp: Vcl.Graphics.TBitmap;
  dc: hdc;
  vMonitor,
  Left, Top: Integer;
  {$ENDIF}
  Procedure BitmapToJpg(Bmp     : Vcl.Graphics.TBitmap;
                        Var JPG : Vcl.Imaging.jpeg.TJPegImage;
                        Quality : Integer = 60);
  Begin
   Try
    JPG := TJPegImage.Create;
    Try
     JPG.Assign(BMP);
     JPG.CompressionQuality := Quality;
//     JPG.Compress;
    Finally
    End;
   Finally
    FreeAndNil(BMP);
   End;
 End;
Begin
  {$IF DEFINED (ANDROID) || (IOS)}
  Mybmp := FMX.Graphics.TBitmap.Create;
  dc := FMX.Forms.Screen.Forms[0].Handle;
  {$ENDIF}
  {$IF DEFINED (MSWINDOWS)}
  Mybmp := Vcl.Graphics.TBitmap.Create;
//  dc := GetWindowDC(0);
  {$ENDIF}
  vMonitor := StrToInt(Monitor) +1;
  If (vMonitor > FMX.Forms.Screen.DisplayCount) then
   Exit;
  vMonitor := vMonitor -1;
  Try
   If FDuplication.GetFrame Then
    FDuplication.DrawFrame(Mybmp);
//
//    Mybmp.Width := Screen.Monitors[vMonitor].Width;
//    Mybmp.Height := Screen.Monitors[vMonitor].Height;
//    Left := Screen.Monitors[vMonitor].Left;
//    Top := Screen.Monitors[vMonitor].Top;
//    DesktopCanvas := TCanvas.Create;
//    Try
//     DesktopCanvas.Handle := DC;
//     {$IF DEFINED (ANDROID) || (IOS)}
// //    R := FMX.Forms.Screen.DesktopRect;
//     {$ENDIF}
//     {$IF DEFINED (MSWINDOWS)}
// //    R := Rect(0, 0, GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN));
//     {$ENDIF}
// //    Mybmp.Width := R.Right;
// //    Mybmp.Height := R.Bottom;
//     {$IF DEFINED (ANDROID) || (IOS)}
// //    BitBlt(Mybmp.Canvas.Handle, 0, 0, Mybmp.Width, Mybmp.Height, dc, 0, 0, SRCCOPY or CAPTUREBLT);
//     {$ENDIF}
//     {$IF DEFINED (MSWINDOWS)}
//      BitBlt(Mybmp.Canvas.Handle, 0, 0, Mybmp.Width, Mybmp.Height, DesktopCanvas.Handle, Left, Top, SRCCOPY or CAPTUREBLT);
//     {$ENDIF}
//    Finally
//     FreeAndNil(DesktopCanvas);
//    End;
  Finally
//    {$IF DEFINED (ANDROID) || (IOS)}
//    FMX.Forms.Screen.Forms[0].ReleaseCapture;
//    {$ENDIF}
//    {$IF DEFINED (MSWINDOWS)}
//    ReleaseDC(0, dc);
//    {$ENDIF}
  End;
  If DrawCur Then
   DrawScreenCursor(Mybmp, StrToInt(Monitor));
  TargetMemoryStream.Clear;
  {$IF DEFINED (ANDROID) || (IOS)}
  If PixelFormat = TPixelFormat.RGB Then
  {$ENDIF}
//  {$IF DEFINED (MSWINDOWS)}
//  If PixelFormat = pf4bit Then
//  {$ENDIF}
//  Begin
//    {$IF DEFINED (ANDROID) || (IOS)}
//    Mybmp.PixelFormat := TPixelFormat.RGBA16;
//    {$ENDIF}
//    {$IF DEFINED (MSWINDOWS)}
//    Mybmp.PixelFormat := pf16bit;
//    {$ENDIF}
//    MakeGrey(Mybmp);
//  End
//  Else
  Mybmp.PixelFormat := PixelFormat;
  BitmapToJpg(Mybmp, JPG);
  JPG.SaveToStream(TargetMemoryStream);
  JPG.Free;
End;

end.

