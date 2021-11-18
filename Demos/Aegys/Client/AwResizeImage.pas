unit AwResizeImage;

interface

uses
  Windows, SysUtils, Classes, Graphics, Math, JPEG, GR32, Vcl.Imaging.GIFImg,
  GR32_Resamplers,Vcl.Imaging.PNGImage;

type
  TImageType = (itUnknown, itBMP, itGIF, itJPG, itPNG);

  TImageInfo = record
    ImgType: TImageType;
    Width: Cardinal;
    Height: Cardinal;
  end;

function GetImageInfo(const AFilename: String): TImageInfo; overload;
function GetImageInfo(const AStream: TStream): TImageInfo; overload;

function ResizeImage(const ASource, ADest: String;
  const AWidth, AHeight: Integer; const ABackColor: TColor;
  const AType: TImageType = itUnknown): Boolean; overload;
function ResizeImage(const ASource, ADest: TStream;
  const AWidth, AHeight: Integer; const ABackColor: TColor;
  const AType: TImageType = itUnknown): Boolean; overload;

implementation

type
  TGetDimensions = procedure(const ASource: TStream;
    var AImageInfo: TImageInfo);

  TCardinal = record
    case Byte of
      0:
        (Value: Cardinal);
      1:
        (Byte1, Byte2, Byte3, Byte4: Byte);
  end;

  TWord = record
    case Byte of
      0:
        (Value: Word);
      1:
        (Byte1, Byte2: Byte);
  end;

  TPNGIHDRChunk = packed record
    Width: Cardinal;
    Height: Cardinal;
    Bitdepth: Byte;
    Colortype: Byte;
    Compression: Byte;
    Filter: Byte;
    Interlace: Byte;
  end;

  TGIFHeader = packed record
    Signature: array [0 .. 2] of AnsiChar;
    Version: array [0 .. 2] of AnsiChar;
    Width: Word;
    Height: Word;
  end;

  TJPGChunk = record
    ID: Word;
    Length: Word;
  end;

  TJPGHeader = packed record
    Reserved: Byte;
    Height: Word;
    Width: Word;
  end;

const
  SIG_BMP: array [0 .. 1] of AnsiChar = ('B', 'M');
  SIG_GIF: array [0 .. 2] of AnsiChar = ('G', 'I', 'F');
  SIG_JPG: array [0 .. 2] of AnsiChar = (#255, #216, #255);
  SIG_PNG: array [0 .. 7] of AnsiChar = (#137, #80, #78, #71, #13, #10,
    #26, #10);

function SwapBytes(const ASource: Cardinal): Cardinal; overload;
var
  mwSource: TCardinal;
  mwDest: TCardinal;
begin
  mwSource.Value := ASource;
  mwDest.Byte1 := mwSource.Byte4;
  mwDest.Byte2 := mwSource.Byte3;
  mwDest.Byte3 := mwSource.Byte2;
  mwDest.Byte4 := mwSource.Byte1;
  Result := mwDest.Value;
end;

function SwapBytes(const ASource: Word): Word; overload;
var
  mwSource: TWord;
  mwDest: TWord;
begin
  mwSource.Value := ASource;
  mwDest.Byte1 := mwSource.Byte2;
  mwDest.Byte2 := mwSource.Byte1;
  Result := mwDest.Value;
end;

procedure GetBMPDimensions(const ASource: TStream; var AImageInfo: TImageInfo);
var
  bmpFileHeader: TBitmapFileHeader;
  bmpInfoHeader: TBitmapInfoHeader;
begin
  FillChar(bmpFileHeader, SizeOf(TBitmapFileHeader), #0);
  FillChar(bmpInfoHeader, SizeOf(TBitmapInfoHeader), #0);
  ASource.Read(bmpFileHeader, SizeOf(TBitmapFileHeader));
  ASource.Read(bmpInfoHeader, SizeOf(TBitmapInfoHeader));
  AImageInfo.Width := bmpInfoHeader.biWidth;
  AImageInfo.Height := bmpInfoHeader.biHeight;
end;

procedure GetGIFDimensions(const ASource: TStream; var AImageInfo: TImageInfo);
Var
  gifHeader: TGIFHeader;
Begin
  FillChar(gifHeader, SizeOf(TGIFHeader), #0);
  ASource.Read(gifHeader, SizeOf(TGIFHeader));
  AImageInfo.Width := gifHeader.Width;
  AImageInfo.Height := gifHeader.Height;
End;

procedure GetJPGDimensions(const ASource: TStream; var AImageInfo: TImageInfo);
var
  cSig: array [0 .. 1] of AnsiChar;
  jpgChunk: TJPGChunk;
  jpgHeader: TJPGHeader;
  iSize: Integer;
  iRead: Integer;
begin
  FillChar(cSig, SizeOf(cSig), #0);
  ASource.Read(cSig, SizeOf(cSig));
  iSize := SizeOf(TJPGChunk);
  repeat
    FillChar(jpgChunk, iSize, #0);
    iRead := ASource.Read(jpgChunk, iSize);
    if iRead <> iSize then
      Break;
    if jpgChunk.ID = $C0FF then
    begin
      ASource.Read(jpgHeader, SizeOf(TJPGHeader));
      AImageInfo.Width := SwapBytes(jpgHeader.Width);
      AImageInfo.Height := SwapBytes(jpgHeader.Height);
      Break;
    end
    else
      ASource.Position := ASource.Position + (SwapBytes(jpgChunk.Length) - 2);
  until False;
end;

procedure GetPNGDimensions(const ASource: TStream; var AImageInfo: TImageInfo);
var
  cSig: array [0 .. 7] of AnsiChar;
  cChunkLen: Cardinal;
  cChunkType: array [0 .. 3] of AnsiChar;
  ihdrData: TPNGIHDRChunk;
begin
  FillChar(cSig, SizeOf(cSig), #0);
  FillChar(cChunkType, SizeOf(cChunkType), #0);
  ASource.Read(cSig, SizeOf(cSig));
  cChunkLen := 0;
  ASource.Read(cChunkLen, SizeOf(Cardinal));
  cChunkLen := SwapBytes(cChunkLen);
  if cChunkLen = SizeOf(TPNGIHDRChunk) then
  begin
    ASource.Read(cChunkType, SizeOf(cChunkType));
    if AnsiUpperCase(cChunkType) = 'IHDR' then
    begin
      FillChar(ihdrData, SizeOf(TPNGIHDRChunk), #0);
      ASource.Read(ihdrData, SizeOf(TPNGIHDRChunk));
      AImageInfo.Width := SwapBytes(ihdrData.Width);
      AImageInfo.Height := SwapBytes(ihdrData.Height);
    end;
  end;
end;

function GetImageInfo(const AFilename: String): TImageInfo;
var
  fsImage: TFileStream;
begin
  fsImage := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite);
  try
    Result := GetImageInfo(fsImage);
  finally
    FreeAndNil(fsImage);
  end;
end;

function GetImageInfo(const AStream: TStream): TImageInfo;
var
  iPos: Integer;
  cBuffer: array [0 .. 2] of AnsiChar;
  cPNGBuffer: array [0 .. 4] of AnsiChar;
  GetDimensions: TGetDimensions;
begin
  GetDimensions := nil;
  Result.ImgType := itUnknown;
  Result.Width := 0;
  Result.Height := 0;
  FillChar(cBuffer, SizeOf(cBuffer), #0);
  FillChar(cPNGBuffer, SizeOf(cPNGBuffer), #0);
  iPos := AStream.Position;
  AStream.Read(cBuffer, SizeOf(cBuffer));
  if cBuffer = SIG_GIF then
  begin
    Result.ImgType := itGIF;
    GetDimensions := GetGIFDimensions;
  end
  else if cBuffer = SIG_JPG then
  begin
    Result.ImgType := itJPG;
    GetDimensions := GetJPGDimensions;
  end
  else if cBuffer = Copy(SIG_PNG, 1, 3) then
  begin
    AStream.Read(cPNGBuffer, SizeOf(cPNGBuffer));
    if cPNGBuffer = Copy(SIG_PNG, 4, 5) then
    begin
      Result.ImgType := itPNG;
      GetDimensions := GetPNGDimensions;
    end;
  end
  else if Copy(cBuffer, 1, 2) = SIG_BMP then
  begin
    Result.ImgType := itBMP;
    GetDimensions := GetBMPDimensions;
  end;
  AStream.Position := iPos;
  if Assigned(GetDimensions) then
  begin
    GetDimensions(AStream, Result);
    AStream.Position := iPos;
  end;
end;

procedure GIFToBMP(const ASource: TStream; const ADest: TBitmap);
var
  imgSource: TGIFImage;
begin
  imgSource := TGIFImage.Create();
  try
    imgSource.LoadFromStream(ASource);
    ADest.Assign(imgSource);
  finally
    FreeAndNil(imgSource);
  end;
end;

procedure JPGToBMP(const ASource: TStream; const ADest: TBitmap);
var
  imgSource: TJPEGImage;
begin
  imgSource := TJPEGImage.Create();
  try
    imgSource.LoadFromStream(ASource);
    ADest.Assign(imgSource);
  finally
    FreeAndNil(imgSource);
  end;
end;

procedure PNGToBMP(const ASource: TStream; const ADest: TBitmap);
var
  imgSource: TPNGImage;
begin
  imgSource := TPNGImage.Create();
  try
    imgSource.LoadFromStream(ASource);
    ADest.Assign(imgSource);
  finally
    FreeAndNil(imgSource);
  end;
end;

function ResizeImage(const ASource, ADest: String;
  const AWidth, AHeight: Integer; const ABackColor: TColor;
  const AType: TImageType = itUnknown): Boolean;
var
  fsSource: TFileStream;
  fsDest: TFileStream;
begin
  Result := False;
  fsSource := TFileStream.Create(ASource, fmOpenRead or fmShareDenyWrite);
  try
    fsDest := TFileStream.Create(ADest, fmCreate or fmShareExclusive);
    try
      Result := not Result; // hide compiler hint
      Result := ResizeImage(fsSource, fsDest, AWidth, AHeight,
        ABackColor, AType);
    finally
      FreeAndNil(fsDest);
    end;
  finally
    FreeAndNil(fsSource);
  end;
end;

Function ResizeImage(const ASource, ADest: TStream;
  const AWidth, AHeight: Integer; const ABackColor: TColor;
  const AType: TImageType = itUnknown): Boolean;
var
  itImage: TImageType;
  ifImage: TImageInfo;
  bmpTemp: TBitmap;
  bmpSource: TBitmap32;
  bmpResized: TBitmap32;
  cBackColor: TColor32;
  rSource: TRect;
  rDest: TRect;
  dWFactor: Double;
  dHFactor: Double;
  dFactor: Double;
  iSrcWidth: Integer;
  iSrcHeight: Integer;
  iWidth: Integer;
  iHeight: Integer;
  jpgTemp: TJPEGImage;
begin
  Result := False;
  itImage := AType;
  if itImage = itUnknown then
  begin
    ifImage := GetImageInfo(ASource);
    itImage := ifImage.ImgType;
    if itImage = itUnknown then
      Exit;
  end;
  bmpTemp := TBitmap.Create();
  try
    case itImage of
      itBMP:
        bmpTemp.LoadFromStream(ASource);
      itGIF:
        GIFToBMP(ASource, bmpTemp);
      itJPG:
        JPGToBMP(ASource, bmpTemp);
      itPNG:
        PNGToBMP(ASource, bmpTemp);
    end;
    bmpSource := TBitmap32.Create();
    bmpResized := TBitmap32.Create();
    try
      cBackColor := Color32(ABackColor);
      bmpSource.Assign(bmpTemp);
      bmpResized.Width := AWidth;
      bmpResized.Height := AHeight;
      bmpResized.Clear(cBackColor);
      iSrcWidth := bmpSource.Width;
      iSrcHeight := bmpSource.Height;
      iWidth := iSrcWidth;
      iHeight := iSrcHeight;
      with rSource do
      begin
        Left := 0;
        Top := 0;
        Right := iSrcWidth;
        Bottom := iSrcHeight;
      end;
      if (iWidth > AWidth) or (iHeight > AHeight) then
      begin
        dWFactor := AWidth / iWidth;
        dHFactor := AHeight / iHeight;
        if (dWFactor > dHFactor) then
          dFactor := dHFactor
        else
          dFactor := dWFactor;
        iWidth := Floor(iWidth * dFactor);
        iHeight := Floor(iHeight * dFactor);
      end;
      with rDest do
      begin
        Left := Floor((AWidth - iWidth) / 2);
        Top := Floor((AHeight - iHeight) / 2);
        Right := Left + iWidth;
        Bottom := Top + iHeight;
      end;
      bmpSource.Resampler := TKernelResampler.Create;
      TKernelResampler(bmpSource.Resampler).Kernel := TLanczosKernel.Create;
      bmpSource.DrawMode := dmOpaque;
      bmpResized.Draw(rDest, rSource, bmpSource);
      {
        jpgTemp := TJPEGImage.Create();
        jpgTemp.CompressionQuality := 80;
      }
      try
        bmpTemp.Assign(bmpResized);
        bmpTemp.SaveToStream(ADest);
        {
          jpgTemp.Assign(bmpTemp);
          jpgTemp.SaveToStream(ADest);
        }
        Result := True;
      finally
        // FreeAndNil(jpgTemp);
      end;
    finally
      FreeAndNil(bmpResized);
      FreeAndNil(bmpSource);
    end;
  finally
    FreeAndNil(bmpTemp);
  end;
end;

end.
