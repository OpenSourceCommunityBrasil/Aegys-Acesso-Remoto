unit StreamManager;


interface

uses
  System.Classes, FMX.Graphics, Vcl.Graphics, FMX.Forms, Winapi.Windows,
  FMX.Objects;

procedure GetScreenToMemoryStream(DrawCur: Boolean; TargetMemoryStream: TMemoryStream);

procedure CompareStream(MyFirstStream, MySecondStream, MyCompareStream: TMemoryStream);

procedure ResumeStream(MyFirstStream, MySecondStream, MyCompareStream: TMemoryStream);

procedure ResizeBmp(AImage: TImage; AStream: TMemoryStream; AWidth, AHeight: Single);

implementation

uses
  System.SysUtils;

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

procedure GetScreenToMemoryStream(DrawCur: Boolean; TargetMemoryStream: TMemoryStream);
const
  CAPTUREBLT = $40000000;
var
  Mybmp: Vcl.Graphics.TBitmap;
  Cursorx, Cursory: Integer;
  dc: hdc;
  R: TRect;
  DrawPos: TPoint;
  MyCursor: TIcon;
  hld: hwnd;
  Threadld: dword;
  mp: TPoint;
  pIconInfo: TIconInfo;
begin
  Mybmp := Vcl.Graphics.TBitmap.Create;

  dc := GetWindowDC(0);
  try
    R := Rect(0, 0, GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN));
    Mybmp.Width := R.Right;
    Mybmp.Height := R.Bottom;
    BitBlt(Mybmp.Canvas.Handle, 0, 0, Mybmp.Width, Mybmp.Height, dc, 0, 0, SRCCOPY or CAPTUREBLT);
  finally
    releaseDC(0, dc);
  end;

  if DrawCur then
  begin
    GetCursorPos(DrawPos);
    MyCursor := TIcon.Create;
    GetCursorPos(mp);
    hld := WindowFromPoint(mp);
    Threadld := GetWindowThreadProcessId(hld, nil);
    AttachThreadInput(GetCurrentThreadId, Threadld, True);
    MyCursor.Handle := Getcursor();
    AttachThreadInput(GetCurrentThreadId, Threadld, False);
    GetIconInfo(MyCursor.Handle, pIconInfo);
    Cursorx := DrawPos.x - round(pIconInfo.xHotspot);
    Cursory := DrawPos.y - round(pIconInfo.yHotspot);
    Mybmp.Canvas.Draw(Cursorx, Cursory, MyCursor);
    DeleteObject(pIconInfo.hbmColor);
    DeleteObject(pIconInfo.hbmMask);
    MyCursor.ReleaseHandle;
    MyCursor.Free;
  end;
  Mybmp.PixelFormat := pf8bit;
  TargetMemoryStream.Clear;
  Mybmp.SaveToStream(TargetMemoryStream);
  Mybmp.Free;
end;

// Compare Streams and separate when the Bitmap Pixels are equal.
procedure CompareStream(MyFirstStream, MySecondStream, MyCompareStream: TMemoryStream);
var
  I: Integer;
  P1: ^AnsiChar;
  P2: ^AnsiChar;
  P3: ^AnsiChar;
begin
  // Check if the resolution has been changed
  if MyFirstStream.Size <> MySecondStream.Size then
  begin
    MyFirstStream.LoadFromStream(MySecondStream);
    MyCompareStream.LoadFromStream(MySecondStream);
    Exit;
  end;

  MyCompareStream.Clear;

  P1 := MyFirstStream.Memory;
  P2 := MySecondStream.Memory;
  MyCompareStream.SetSize(MyFirstStream.Size);
  P3 := MyCompareStream.Memory;

  for I := 0 to MyFirstStream.Size - 1 do
  begin

    if P1^ = P2^ then
      P3^ := '0'
    else
      P3^ := P2^;

    Inc(P1);
    Inc(P2);
    Inc(P3);

  end;

  MyFirstStream.LoadFromStream(MySecondStream);
end;

// Modifies Streams to set the Pixels of Bitmap
procedure ResumeStream(MyFirstStream, MySecondStream, MyCompareStream: TMemoryStream);
var
  I: Integer;
  P1: ^AnsiChar;
  P2: ^AnsiChar;
  P3: ^AnsiChar;
begin

  // Check if the resolution has been changed
  if MyFirstStream.Size <> MyCompareStream.Size then
  begin
    MyFirstStream.LoadFromStream(MyCompareStream);
    MySecondStream.LoadFromStream(MyCompareStream);
    Exit;
  end;

  P1 := MyFirstStream.Memory;
  MySecondStream.SetSize(MyFirstStream.Size);
  P2 := MySecondStream.Memory;
  P3 := MyCompareStream.Memory;

  for I := 0 to MyFirstStream.Size - 1 do
  begin

    if P3^ = '0' then
      P2^ := P1^
    else
      P2^ := P3^;

    Inc(P1);
    Inc(P2);
    Inc(P3);

  end;

  MyFirstStream.LoadFromStream(MySecondStream);
  MySecondStream.Position := 0;
end;

end.

