unit uCaptureDeviceMode;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Imaging.jpeg;

Type
  TImagePoint = Record
    X, Y: Integer;
    Bitmap: TBitmap;
  End;

Type
  TImages = Array of Array of TImagePoint;

Type
  TImageCapture = Class(TComponent)
  Private
    vPrincipalHeight, vPrincipalWidth, vCols, vRows, vColWidth, vRowHeight,
      vCompareLength: Integer;
    vImages: TImages;
    vBitmap: TBitmap;
    vOwner: TComponent;
    vPixelFormat: TPixelFormat;
    Procedure ResizeBitmap(Var Value: TBitmap; Width, Height: Integer);
    Procedure SetImageBase(Value: TBitmap);
    Procedure ClearMatrix(Var Values: TImages);
    Procedure ClearBase;
    Procedure SetNewHeight(Value: Integer);
    Procedure SetNewWidth(Value: Integer);
    Procedure SetNewCols(Value: Integer);
    Procedure SetNewRows(Value: Integer);
    Procedure CreateImages;
    Procedure SetPixelFormat(Value: TPixelFormat);
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    Function CompareImages(FirstImage, SecondImage: TBitmap): Boolean;
    Procedure GetAltImage(SetImage: TBitmap; Var DestImage: TBitmap;
      Col, Row: Integer; OriginalBase: Boolean = False);
    Property Colums: Integer Read vCols Write SetNewCols;
    Property Rows: Integer Read vRows Write SetNewRows;
    Property Height: Integer Read vPrincipalHeight Write SetNewHeight;
    Property Width: Integer Read vPrincipalWidth Write SetNewWidth;
    Property Images: TImages Read vImages;
    Property ImageBase: TBitmap Read vBitmap Write SetImageBase;
    Property PixelFormat: TPixelFormat Read vPixelFormat Write SetPixelFormat;
    Property CompareLength: Integer Read vCompareLength Write vCompareLength;
  End;

implementation

Function BmpToString(Value: TBitmap): String;
Var
  Stream: TStringStream;
Begin
  Result := '';
  If Value <> Nil Then
  Begin
    Try
      Stream := TStringStream.Create;
      Value.SaveToStream(Stream);
      Stream.Position := 0;
      Result := Stream.DataString;
    Finally
      FreeAndNil(Stream);
    End;
  End;
End;

Function CompareBits(Value1, Value2: String; StartIn, BitsToCompare: Integer;
  Var ReturnBits: String; ReturnValue: Boolean = False): Boolean;
Var
  vString1, vString2: String;
  vLength: Integer;
  Function CreateNullBits(Qtde: Integer): String;
  Var
    I: Integer;
  Begin
    Result := '';
    For I := 0 to Qtde - 1 Do
      Result := Result + '0';
  End;
  Function ComparedBits(Value1, Value2: String): String;
  Var
    I: Integer;
  Begin
    Result := '';
    If Length(Value2) <> Length(Value1) Then
      Result := Value1
    Else
    Begin
      For I := 1 to Length(Value1) Do
      Begin
        If Value1[I] = Value2[I] Then
          Result := Result + '0'
        Else
          Result := Result + Value2[I];
      End;
    End;
  End;

Begin
  vLength := Length(Value1) - (StartIn + BitsToCompare);
  If vLength >= 0 Then
    vLength := BitsToCompare
  Else
    vLength := Length(Value1) - StartIn;
  vString1 := Copy(Value1, StartIn, vLength);
  vLength := Length(Value2) - (StartIn + BitsToCompare);
  If vLength >= 0 Then
    vLength := BitsToCompare
  Else
    vLength := Length(Value2) - StartIn;
  vString2 := Copy(Value2, StartIn, vLength);
  Result := vString1 = vString2;
  If ReturnValue Then
  Begin
    If Result Then
      ReturnBits := CreateNullBits(vLength)
    Else
      ReturnBits := ComparedBits(vString1, vString2);
  End;
End;

Function TImageCapture.CompareImages(FirstImage, SecondImage: TBitmap): Boolean;
Var
  I: Integer;
  FirstStream, SecondStream, vResult: String;
Begin
  Result := True;
  FirstStream := BmpToString(FirstImage);
  SecondStream := BmpToString(SecondImage);
  If (FirstStream = '') or (SecondStream = '') Then
    Exit;
  Try
    I := 1;
    While I <= Length(FirstStream) Do
    Begin
      Result := CompareBits(FirstStream, SecondStream, I,
        vCompareLength, vResult);
      If Not(Result) Then
        Break
      Else
        Inc(I, CompareLength);
    End;
  Except
  End;
End;

Procedure TImageCapture.GetAltImage(SetImage: TBitmap; Var DestImage: TBitmap;
  Col, Row: Integer; OriginalBase: Boolean = False);
Var
  vRect, vDestRect, SRect: TRect;
  Function ExistCord(X, Y: Integer): Boolean;
  Begin
    Result := False;
    If (Length(vImages) > X) And (X > -1) Then
      Result := (Length(vImages[X]) > Y) And (Y > -1);
  End;

Begin
  Try
    If ExistCord(Col, Row) Then
    Begin
      If DestImage <> Nil Then
        FreeAndNil(DestImage);
      DestImage := TBitmap.Create;
      DestImage.Width := vPrincipalWidth;
      DestImage.Height := vPrincipalHeight;
      If OriginalBase Then
        DestImage.Assign(vBitmap)
      Else
      Begin
        vRect := Rect(0, 0, vPrincipalWidth, vPrincipalHeight);
        DestImage.Canvas.Brush.Color := clBlack;
        DestImage.Canvas.FillRect(vRect);
      End;
      // vImages[Col][Row].Bitmap.FreeImage;
      vImages[Col][Row].Bitmap.Assign(SetImage);
      vDestRect := Rect(vImages[Col, Row].X, vImages[Col, Row].Y,
        vImages[Col, Row].X + vColWidth, vImages[Col, Row].Y + vRowHeight);
      SRect := Rect(0, 0, vColWidth, vRowHeight);
      DestImage.Canvas.CopyRect(vDestRect,
        vImages[Col][Row].Bitmap.Canvas, SRect);
    End;
  Finally

  End;
End;

Procedure TImageCapture.SetNewHeight(Value: Integer);
Var
  ImageBase: TBitmap;
Begin
  vPrincipalHeight := Value;
  ImageBase := Nil;
  If Value > 0 Then
  Begin
    Try
      If (vBitmap <> Nil) Then
      Begin
        ImageBase := TBitmap.Create;
        ImageBase.Assign(vBitmap);
        ImageBase.PixelFormat := vPixelFormat;
      End;
      SetImageBase(ImageBase);
    Finally
      If (ImageBase <> Nil) Then
        FreeAndNil(ImageBase);
    End;
  End;
End;

Procedure TImageCapture.SetNewCols(Value: Integer);
Var
  ImageBase: TBitmap;
Begin
  vCols := Value;
  ImageBase := Nil;
  If Value > 0 Then
  Begin
    Try
      If (vBitmap <> Nil) Then
      Begin
        ImageBase := TBitmap.Create;
        ImageBase.Assign(vBitmap);
        ImageBase.PixelFormat := vPixelFormat;
      End;
      SetImageBase(ImageBase);
    Finally
      If (ImageBase <> Nil) Then
        FreeAndNil(ImageBase);
    End;
  End;
End;

Procedure TImageCapture.SetNewRows(Value: Integer);
Var
  ImageBase: TBitmap;
Begin
  vRows := Value;
  ImageBase := Nil;
  If Value > 0 Then
  Begin
    Try
      If (vBitmap <> Nil) Then
      Begin
        ImageBase := TBitmap.Create;
        ImageBase.Assign(vBitmap);
        ImageBase.PixelFormat := vPixelFormat;
      End;
      SetImageBase(ImageBase);
    Finally
      If (ImageBase <> Nil) Then
        FreeAndNil(ImageBase);
    End;
  End;
End;

Procedure TImageCapture.SetNewWidth(Value: Integer);
Var
  ImageBase: TBitmap;
Begin
  vPrincipalWidth := Value;
  ImageBase := Nil;
  If Value > 0 Then
  Begin
    Try
      If (vBitmap <> Nil) Then
      Begin
        ImageBase := TBitmap.Create;
        ImageBase.Assign(vBitmap);
        ImageBase.PixelFormat := vPixelFormat;
      End;
      SetImageBase(ImageBase);
    Finally
      If (ImageBase <> Nil) Then
        FreeAndNil(ImageBase);
    End;
  End;
End;

Constructor TImageCapture.Create(AOwner: TComponent);
Begin
  Inherited;
  vOwner := AOwner;
  vPrincipalHeight := 0;
  vPrincipalWidth := 0;
  vCols := 1;
  vRows := 1;
  vCompareLength := 8;
  vPixelFormat := pf8Bit;
  vBitmap := Nil;
End;

Destructor TImageCapture.Destroy;
Begin
  ClearBase;
  Inherited;
End;

Procedure TImageCapture.ClearBase;
Begin
  If vBitmap <> Nil Then
    FreeAndNil(vBitmap);
  ClearMatrix(vImages);
  SetLength(vImages, 0);
End;

Procedure TImageCapture.ClearMatrix(Var Values: TImages);
Var
  I, A: Integer;
Begin
  For I := 0 To Length(Values) - 1 Do
  Begin
    For A := 0 To Length(Values[I]) - 1 Do
    Begin
      If Values[I][A].Bitmap <> Nil Then
      Begin
        // Values[I][A].Bitmap.FreeImage;
        FreeAndNil(Values[I][A].Bitmap);
      End;
    End;
  End;
End;

Procedure TImageCapture.SetImageBase(Value: TBitmap);
Begin
  ClearBase;
  If Value <> Nil Then
  Begin
    If vBitmap <> Nil Then
      FreeAndNil(vBitmap);
    // ResizeBitmap(Value, vPrincipalWidth, vPrincipalHeight);
    vBitmap := TBitmap.Create;
    vBitmap.Assign(Value);
    If (vPrincipalWidth = 0) Or (vPrincipalHeight = 0) Then
    Begin
      vPrincipalWidth := vBitmap.Width;
      vPrincipalHeight := vBitmap.Height;
    End;
    vBitmap.PixelFormat := vPixelFormat;
    CreateImages;
  End;
End;

Procedure TImageCapture.ResizeBitmap(Var Value: TBitmap;
  Width, Height: Integer);
Var
  vResult: TBitmap;
Begin
  Try
    vResult := TBitmap.Create;
    vResult.Assign(Value);
    If (Width > 0) And (Height > 0) Then
    Begin
      vResult.Width := Width;
      vResult.Height := Height;
      vResult.PixelFormat := vPixelFormat;
      SetStretchBltMode(vResult.Canvas.Handle, COLORONCOLOR);
      StretchBlt(vResult.Canvas.Handle, 0, 0, Width, Height,
        Value.Canvas.Handle, 0, 0, Value.Width, Value.Height, SRCCOPY);
    End;
    Value.Assign(vResult);
  Finally
    FreeAndNil(vResult);
  End;
End;

Procedure TImageCapture.SetPixelFormat(Value: TPixelFormat);
Var
  ImageBase: TBitmap;
Begin
  vPixelFormat := Value;
  ImageBase := Nil;
  Try
    If (vBitmap <> Nil) Then
    Begin
      ImageBase := TBitmap.Create;
      ImageBase.Assign(vBitmap);
      ImageBase.PixelFormat := vPixelFormat;
    End;
    SetImageBase(ImageBase);
  Finally
    If (ImageBase <> Nil) Then
      FreeAndNil(ImageBase);
  End;
End;

Procedure TImageCapture.CreateImages;
Var
  Col, Row, C, R: Integer;
  vDestRect, SRect: TRect;
Begin
  If vBitmap <> Nil Then
  Begin
    ClearMatrix(vImages);
    vColWidth := vBitmap.Width Div vCols;
    vRowHeight := vBitmap.Height Div vRows;
    SetLength(vImages, vCols, vRows);
    For Col := 0 To vCols - 1 Do
    Begin
      For Row := 0 To vRows - 1 Do
      Begin
        If vImages[Col, Row].Bitmap <> Nil Then
        Begin
          // vImages[Col, Row].Bitmap.FreeImage;
          vImages[Col, Row].Bitmap := Nil;
        End;
      End;
    End;
    For Col := 0 To vCols - 1 Do
    Begin
      For Row := 0 To vRows - 1 Do
      Begin
        vImages[Col, Row].X := Col * vColWidth;
        vImages[Col, Row].Y := Row * vRowHeight;
        SRect := Rect(vImages[Col, Row].X, vImages[Col, Row].Y,
          vImages[Col, Row].X + vColWidth, vImages[Col, Row].Y + vRowHeight);
        vDestRect := Rect(0, 0, vColWidth, vRowHeight);
        vImages[Col, Row].Bitmap := TBitmap.Create;
        vImages[Col, Row].Bitmap.Height := vRowHeight;
        vImages[Col, Row].Bitmap.Width := vColWidth;
        vImages[Col, Row].Bitmap.Canvas.CopyRect(vDestRect,
          vBitmap.Canvas, SRect);
      End;
    End;
  End;
End;

end.
