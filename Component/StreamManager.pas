unit StreamManager;

interface

Uses
  Vcl.Imaging.pngimage, Vcl.Imaging.jpeg, System.SysUtils, Windows, Classes, Graphics,
  Soap.EncdDecd, System.NetEncoding;

 Procedure GetScreenToBmp(DrawCur: Boolean; StreamName: TMemoryStream; Width, Height: Integer);
 Procedure CompareStream(MyFirstStream, MySecondStream, MyCompareStream: TMemoryStream; Width, Height: Integer);
 Procedure ResumeStream(MyFirstStream, MySecondStream, MyCompareStream: TMemoryStream);
 Function MakePNGtoString(Imagem : TGraphic)  : String;
 Function Base64FromImage(Imagem : TGraphic)  : String;
 Function ImageFromBase64(Const Base64 : String;
                          Tipo: TGraphicClass) : TGraphic;
 //function ResizeBmp(Bitmap: TBitmap; const NewWidth, NewHeight: integer): TBitmap;
 Procedure ResizeBmp(bmp: TBitmap; Width, Height: Integer);

implementation



// Resize the Bitmap
{function ResizeBmp(Bitmap: TBitmap; const NewWidth, NewHeight: integer): TBitmap;
begin
  Bitmap.Canvas.StretchDraw(Rect(0, 0, NewWidth, NewHeight), Bitmap);
  Bitmap.SetSize(NewWidth, NewHeight);

  Result := Bitmap;
end;  }


// Resize the Bitmap ( Best quality )
procedure ResizeBmp(bmp: TBitmap; Width, Height: Integer);
var
  SrcBMP: TBitmap;
  DestBMP: TBitmap;
begin
  SrcBMP := TBitmap.Create;
  try
    SrcBMP.Assign(bmp);
    DestBMP := TBitmap.Create;
    try
      DestBMP.Width := Width;
      DestBMP.Height := Height;
      SetStretchBltMode(DestBMP.Canvas.Handle, HALFTONE);
      StretchBlt(DestBMP.Canvas.Handle, 0, 0, DestBMP.Width, DestBMP.Height, SrcBMP.Canvas.Handle, 0, 0, SrcBMP.Width, SrcBMP.Height, SRCCOPY);
      bmp.Assign(DestBMP);
    finally
      DestBMP.Free;
    end;
  finally
    SrcBMP.Free;
  end;
end;



// Screenshot
procedure GetScreenToBmp(DrawCur: Boolean; StreamName: TMemoryStream; Width, Height: Integer);
var
  Mybmp: Tbitmap;
  Cursorx, Cursory: integer;
  dc: hdc;
  Mycan: Tcanvas;
  R: TRect;
  DrawPos: TPoint;
  MyCursor: TIcon;
  hld: hwnd;
  Threadld: dword;
  mp: TPoint;
  pIconInfo: TIconInfo;
begin
  Mybmp := Tbitmap.Create;
  Mycan := Tcanvas.Create;
  dc := GetWindowDC(0);
  try
    Mycan.Handle := dc;
    R := Rect(0, 0, GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN));
    Mybmp.Width := R.Right;
    Mybmp.Height := R.Bottom;
    Mybmp.Canvas.CopyRect(R, Mycan, R);
  finally
    releaseDC(0, dc);
  end;
  Mycan.Handle := 0;
  Mycan.Free;

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
  //ResizeBMP(Mybmp, Width, Height);
  Mybmp.SaveToStream(StreamName);
  Mybmp.Free;
end;

// Compare Streams and separate when the Bitmap Pixels are equal.
procedure CompareStream(MyFirstStream, MySecondStream, MyCompareStream: TMemoryStream; Width, Height: Integer);
var
  I: integer;
  P1, P2, P3: ^AnsiChar;
begin
  MySecondStream.Clear;
  MyCompareStream.Clear;
  GetScreenToBmp(false, MySecondStream, Width, Height);

  P1 := MyFirstStream.Memory;
  P2 := MySecondStream.Memory;
  MyCompareStream.SetSize(MyFirstStream.Size);
  P3 := MyCompareStream.Memory;

  for I := 0 to MyFirstStream.Size - 1 do
  begin
    if P1^ = P2^then
      P3^ := '0'
    else
      P3^ := P2^;
    Inc(P1);
    Inc(P2);
    Inc(P3);
  end;

  MyFirstStream.Clear;
  MyFirstStream.CopyFrom(MySecondStream, 0);
end;

Function MakePNGtoString(Imagem : TGraphic) : String;
Var
 PNG : TPngObject;
Begin
 PNG := TPNGObject.Create;
 Try
  PNG.Assign(Imagem);
  PNG.CompressionLevel := 9;
  Result := Base64FromImage(PNG);
 Finally
  PNG.Free;
 End;
End;

Function Base64FromImage(Imagem : TGraphic) : String;
Var
 Input  : TBytesStream;
 Output : TStringStream;
Begin
 Input := TBytesStream.Create;
 Try
  Imagem.SaveToStream(Input);
  Input.Position := 0;
  Output := TStringStream.Create('', TEncoding.ASCII);
  Try
   Soap.EncdDecd.EncodeStream(Input, Output);
   Result := Output.DataString;
  Finally
   Output.Free;
  End;
 Finally
  Input.Free;
 End;
End;

Function ImageFromBase64(Const Base64 : String;
                                 Tipo : TGraphicClass) : TGraphic;
Var
 Input  : TStringStream;
 Output : TBytesStream;
Begin
 Result := Nil;
 If Length(base64) > 0 Then
  Begin
   Input := TStringStream.Create(Base64, TEncoding.ASCII);
   if Tipo = Nil then
    Result := TBitmap.Create
   Else
    Result := Tipo.Create;
   Try
    Output := TBytesStream.Create;
    Try
     Soap.EncdDecd.DecodeStream(Input, Output);
     Output.Position := 0;
     Try
      Result.LoadFromStream(Output);
     Except
      Result.Free;
      Raise;
     End;
    Finally
     Output.Free;
    End;
   Finally
    Input.Free;
   End;
  End;
End;

// Modifies Streams to set the Pixels of Bitmap
procedure ResumeStream(MyFirstStream, MySecondStream, MyCompareStream: TMemoryStream);
var
  I: integer;
  P1, P2, P3: ^AnsiChar;
begin
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

  MyFirstStream.Clear;
  MyFirstStream.CopyFrom(MySecondStream, 0);
  MySecondStream.Position := 0;
end;

end.

