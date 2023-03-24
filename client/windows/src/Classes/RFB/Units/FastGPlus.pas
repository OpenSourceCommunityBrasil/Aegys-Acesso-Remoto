unit FastGPlus; // FastDIB: sourceforge.net/projects/tfastdib

// image loading/saving via GDI+
// loading code adds only 3 kb to .exe
// (c) Sapersky <sapersky@gmail.com>

interface
Uses Windows, ActiveX, FastDIB, uAegysUtils;

Type
  PImageSize = ^TImageSize;
  TImageSize = record
    Width, Height, Bpp : Integer;
  end;

  TGPFormatDesc = record
    Ext, Desc : String;
    FormatID : TGUID;
    CanSave : Boolean;
  end;
  DGPFormatArr = array of TGPFormatDesc;

{$I platform.inc}

//function gpInit: Boolean;
//procedure gpClose;

function gpLoadImage(Const FName : String; Dst : TFastDIB) : Boolean;

function gpLoadImageMem(Ptr : Pointer; Size : Integer;
                        Dst : TFastDIB): Boolean;
function gpLoadImageRes(Const ResName : String; ResType : PChar;
                        Dst : TFastDIB): Boolean;

function gpSaveImage(Src : TFastDIB; Const FName : String;
                     Quality : Integer = 100) : Boolean;

function gpGetExtList: DGPFormatArr;

implementation

// standart header is used for x64 (with static linking)
{.$IFDEF DXE2_UP}
{$IFDEF CPUX64}
  {$DEFINE USE_STD_HEADER}
{$ENDIF}

{$IFDEF USE_STD_HEADER} Uses Winapi.GDIPAPI; {$ENDIF}

Var
  gpInitToken : Cardinal;
{$IFNDEF USE_STD_HEADER}
  gpDLL : HModule;
  gpFailed : Boolean;

Const
  gdiplus_dllname = 'GdiPlus.dll';
  Ok = 0;
  EncoderParameterValueTypeByte          : Integer = 1;    // 8-bit unsigned int
  EncoderParameterValueTypeASCII         : Integer = 2;    // 8-bit byte containing one 7-bit ASCII
  EncoderParameterValueTypeShort         : Integer = 3;    // 16-bit unsigned int
  EncoderParameterValueTypeLong          : Integer = 4;    // 32-bit unsigned int
  EncoderParameterValueTypeRational      : Integer = 5;    // Two Longs, numerator / denominator
  EncoderParameterValueTypeLongRange     : Integer = 6;    // Two longs (Int64)
  EncoderParameterValueTypeUndefined     : Integer = 7;    // 8-bit byte that can take any value
  EncoderParameterValueTypeRationalRange : Integer = 8;    // Two Rationals

  PixelFormatIndexed     = $00010000; // Indexes into a palette
  PixelFormatGDI         = $00020000; // Is a GDI-supported format
  PixelFormatAlpha       = $00040000; // Has an alpha component
  PixelFormatPAlpha      = $00080000; // Pre-multiplied alpha
  PixelFormatExtended    = $00100000; // Extended color 16 bits/channel

Type
  PBitmapData = ^TBitmapData;
  TBitmapData = packed record
    Width       : UINT;
    Height      : UINT;
    Stride      : Integer;
    PixelFormat : Integer;
    Scan0       : Pointer;
    Reserved    : UINT;
  end;
  PColorPalette = ^TColorPalette;
  TColorPalette = packed record
    Flags  : UINT ;                 // Palette flags
    Count  : UINT ;                 // Number of color entries
    Entries: array [0..0] of DWORD; // Palette color entries
  end;
//  PropTagType = ( ptUnused1, ptByte, ptAscii, ptShort, ptLong, ptRational,
//                  ptUnused2, ptUndefined, ptUnused3, ptSLong, ptSRational );

  PPropertyItem = ^TPropertyItem;
  TPropertyItem = packed record
    ID: Integer;
    Len: Integer;
    Type_: Word;
    Unused: Word;
    Value: Pointer;
  end;

  PImageCodecInfo = ^TImageCodecInfo;
  TImageCodecInfo = packed record
    Clsid             : TGUID;
    FormatID          : TGUID;
    CodecName         : PWCHAR;
    DllName           : PWCHAR;
    FormatDescription : PWCHAR;
    FilenameExtension : PWCHAR;
    MimeType          : PWCHAR;
    Flags             : DWORD;
    Version           : DWORD;
    SigCount          : DWORD;
    SigSize           : DWORD;
    SigPattern        : PBYTE;
    SigMask           : PBYTE;
  end;

  PEncoderParameter = ^TEncoderParameter;
  TEncoderParameter = packed record
    Guid           : TGUID;   // GUID of the parameter
    NumberOfValues : ULONG;   // Number of the parameter values
    Type_          : ULONG;   // Value type, like ValueTypeLONG  etc.
    Value          : Pointer; // A pointer to the parameter values
  end;

  PEncoderParameters = ^TEncoderParameters;
  TEncoderParameters = packed record
    Count     : UINT;               // Number of parameters in this structure
    Parameter : array[0..0] of TEncoderParameter;  // Parameter values
  end;

  PGPRect = ^TGPRect;
  TGPRect = packed record
    X     : Integer;
    Y     : Integer;
    Width : Integer;
    Height: Integer;
  end;

  TImageType   = ( ImageUnknown, ImageTypeBitmap, ImageMetaFile );

  GpStatus = Integer;

  GPImage = Pointer;//Integer;

  GPGraphics = Integer;
Var
  GdiplusStartup : function (var Token: DWord; const Input, Output: Pointer): Integer; stdcall;
  GdiplusShutdown : procedure (Token: DWord); stdcall;
  GdipDeleteGraphics : function (Graphics: GPGraphics): Integer; stdcall;
  GdipCreateFromHDC : function (hdc: HDC; var Graphics: GPGraphics): Integer; stdcall;

  GdipDrawImageI : function (Graphics : GPGraphics; Image : GPImage; X, Y: Integer): Integer; stdcall;
  GdipDisposeImage : function (Image : GPImage): Integer; stdcall;

  GdipLoadImageFromFile : function (const FileName: PWideChar; var Image: GPImage): Integer; stdcall;
  GdipLoadImageFromFileICM : function (const FileName: PWideChar; var Image: GPImage): Integer; stdcall;
  GdipLoadImageFromStream : function (stream: ISTREAM; out image: GPImage): Integer; stdcall;
  GdipLoadImageFromStreamICM: function (stream: ISTREAM; out image: GPImage): Integer; stdcall;

  GdipCreateBitmapFromGdiDib : function (gdiBitmapInfo: PBitmapInfo; gdiBitmapData: Pointer; out bitmap: GPImage): Integer; stdcall;

  GdipSaveImageToFile : function (Image : GPImage; filename: PWCHAR; clsidEncoder: PGUID; encoderParams: PENCODERPARAMETERS): Integer; stdcall;

  GdipGetImageWidth : function (Image: GPImage; var Width: DWord): Integer; stdcall;
  GdipGetImageHeight : function (Image: GPImage; var Height: DWord): Integer; stdcall;
  GdipGetImageType : function (Image: GPImage; var Typ: TImageType): Integer; stdcall;
  GdipGetImagePixelFormat : function (image: GPImage; out format: Integer): Integer; stdcall;
  GdipGetImagePaletteSize : function (image: GPImage; var size: Integer): Integer; stdcall;
  GdipGetImagePalette : function (image: GPImage; palette: PCOLORPALETTE; size: Integer): Integer; stdcall;

  GdipImageGetFrameDimensionsCount : function (Image: GPImage; var Count: DWord): Integer; stdcall;
  GdipImageGetFrameDimensionsList : function (Image: GPImage; DimensionsList: Pointer; Count: Integer): Integer; stdcall;
  GdipImageGetFrameCount : function (Image: GPImage; ID: PGUID; var Count: DWord): Integer; stdcall;
  GdipImageSelectActiveFrame : function (Image: GPImage; ID: PGUID; FrameIndex: Integer): Integer; stdcall;

  GdipGetPropertyItemSize : function (Image : GPImage; PropID: Integer; var Size: DWord): Integer; stdcall;
  GdipGetPropertyItem : function (Image : GPImage; PropId, PropSize: Integer; Buffer: Pointer): Integer; stdcall;

  GdipGetImageDecodersSize : function (out numDecoders: UINT; out size: UINT): Integer; stdcall;
  GdipGetImageDecoders : function (numDecoders: UINT; size: UINT; decoders: PIMAGECODECINFO): Integer; stdcall;
  GdipGetImageEncodersSize : function (out numEncoders: UINT; out size: UINT): Integer; stdcall;
  GdipGetImageEncoders : function (numEncoders: UINT; size: UINT; encoders: PIMAGECODECINFO): Integer; stdcall;

  GdipBitmapLockBits : function (Image: GPImage; Rect: PGpRect; flags: UINT;
    format: Integer; lockedBitmapData: PBitmapData): Integer; stdcall;
  GdipBitmapUnlockBits : function (Image: GPImage;
    lockedBitmapData: PBitmapData): Integer; stdcall;



function gpInit: Boolean;
type
  TGDIStartup = packed record
    Version: Integer;                       // Must be one
    DebugEventCallback: Pointer;            // Only for debug builds
    SuppressBackgroundThread: Bool;         // True if replacing GDI+ background processing
    SuppressExternalCodecs: Bool;           // True if only using internal codecs
  end;
var
  Err: Integer;
  Startup: TGDIStartup;
begin
If gpFailed then Exit;
gpDLL := LoadLibrary(gdiplus_dllname);
gpFailed := (gpDLL = 0);
If gpFailed then Exit;

GdiplusStartup  := GetProcAddress(gpDLL, 'GdiplusStartup');
GdiplusShutdown  := GetProcAddress(gpDLL, 'GdiplusShutdown');

Result := (gpInitToken <> 0);
If (not Result) and (gpDLL <> 0) and (Assigned(GdiPlusStartup)) then begin
  FillChar(Startup, SizeOf (Startup), 0);
  Startup.Version := 1;
  Err := GdiPlusStartup(gpInitToken, @Startup, nil);
  Result := (Err = 0) and (gpInitToken <> 0);
  If Result then begin
    GdipCreateFromHDC  := GetProcAddress(gpDLL, 'GdipCreateFromHDC');
    GdipDeleteGraphics  := GetProcAddress(gpDLL, 'GdipDeleteGraphics');

    GdipDrawImageI  := GetProcAddress(gpDLL, 'GdipDrawImageI');
    GdipDisposeImage  := GetProcAddress(gpDLL, 'GdipDisposeImage');
    GdipCreateBitmapFromGdiDib:= GetProcAddress(gpDLL, 'GdipCreateBitmapFromGdiDib');

    GdipLoadImageFromFile  := GetProcAddress(gpDLL, 'GdipLoadImageFromFile');
    GdipLoadImageFromFileICM  := GetProcAddress(gpDLL, 'GdipLoadImageFromFileICM');
    GdipLoadImageFromStream := GetProcAddress(gpDLL, 'GdipLoadImageFromStream');
    GdipLoadImageFromStreamICM := GetProcAddress(gpDLL, 'GdipLoadImageFromStreamICM');

    GdipSaveImageToFile := GetProcAddress(gpDLL, 'GdipSaveImageToFile');

    GdipGetImageWidth  := GetProcAddress(gpDLL, 'GdipGetImageWidth');
    GdipGetImageHeight  := GetProcAddress(gpDLL, 'GdipGetImageHeight');
    GdipGetImageType  := GetProcAddress(gpDLL, 'GdipGetImageType');
    GdipGetImagePixelFormat := GetProcAddress(gpDLL, 'GdipGetImagePixelFormat');;
    GdipGetImagePaletteSize := GetProcAddress(gpDLL, 'GdipGetImagePaletteSize');
    GdipGetImagePalette := GetProcAddress(gpDLL, 'GdipGetImagePalette');

    GdipImageGetFrameDimensionsCount  := GetProcAddress(gpDLL, 'GdipImageGetFrameDimensionsCount');
    GdipImageGetFrameDimensionsList  := GetProcAddress(gpDLL, 'GdipImageGetFrameDimensionsList');
    GdipImageGetFrameCount  := GetProcAddress(gpDLL, 'GdipImageGetFrameCount');
    GdipImageSelectActiveFrame  := GetProcAddress(gpDLL, 'GdipImageSelectActiveFrame');

    GdipGetPropertyItemSize  := GetProcAddress(gpDLL, 'GdipGetPropertyItemSize');
    GdipGetPropertyItem := GetProcAddress(gpDLL, 'GdipGetPropertyItem');

    GdipGetImageDecodersSize := GetProcAddress(gpDLL, 'GdipGetImageDecodersSize');
    GdipGetImageDecoders := GetProcAddress(gpDLL, 'GdipGetImageDecoders');

    GdipGetImageEncodersSize := GetProcAddress(gpDLL, 'GdipGetImageEncodersSize');
    GdipGetImageEncoders := GetProcAddress(gpDLL, 'GdipGetImageEncoders');

    GdipBitmapLockBits := GetProcAddress(gpDLL, 'GdipBitmapLockBits');
    GdipBitmapUnlockBits := GetProcAddress(gpDLL, 'GdipBitmapUnlockBits');
  end;
end;
end;

{$ELSE}

function gpInit: Boolean;
var
  Startup: GdiplusStartupInput;
begin
Result := (gpInitToken <> 0);
If (not Result) then begin
  FillChar(Startup, SizeOf (Startup), 0);
  Startup.GdiplusVersion := 1;
  Result := (GdiPlusStartup(gpInitToken, @Startup, nil) = Ok) and (gpInitToken <> 0);
end;
end;
{$ENDIF}

procedure gpClose;
begin
If (gpInitToken <> 0) then begin GdiplusShutdown(gpInitToken); gpInitToken := 0; end;
end;


function gpGetCodecs(GetDecoders : Boolean; Var Codecs: PImageCodecInfo): Integer;
Var Res : GpStatus;
    CodecCnt, Size : DWord;
begin
Result := 0; Codecs := nil;
If GetDecoders then Res := GdipGetImageDecodersSize(CodecCnt, Size)
               else Res := GdipGetImageEncodersSize(CodecCnt, Size);
If (Integer(Res) = 0) then begin
  GetMem(Codecs, Size);
  If GetDecoders then Res := GdipGetImageDecoders(CodecCnt, Size, Codecs)
                 else Res := GdipGetImageEncoders(CodecCnt, Size, Codecs);
  Result := CodecCnt;
end;
end;

function gpCodecFromExt(Const Ext : String; Var ID, Fmt : TGUID): Integer;
Var Codecs, ec : PImageCodecInfo;
    n, i, CodecCnt : Integer;
    Ext2, s : String;
begin
Result := -1;
CodecCnt := gpGetCodecs(False, Codecs);
ec := Codecs;
CharLowerBuff(@Ext[1], Length(Ext));
For n := 0 to CodecCnt-1 do begin
  //CharLowerW(ec.FilenameExtension);
  s := ec.FilenameExtension;
  CharLowerBuff(@s[1], Length(s));
  //s := Str_LowerCase(ec.FilenameExtension);
  If (Pos(Ext, s) > 0) then begin
    Result := n; ID := ec.Clsid; Fmt := ec.FormatID;
    Break;
  end;
  Inc(ec);
end;
FreeMem(Codecs);
end;

function gpCodecFromIdx(Idx : Integer; Var ID : TGUID): Boolean;
Var Codecs, ec : PImageCodecInfo;
    CodecCnt : Integer;
begin
CodecCnt := gpGetCodecs(False, Codecs);
Result := (CodecCnt > 0);
If (Result) then begin
  If Idx > CodecCnt then Idx := 0;
  ec := Codecs; Inc(ec, Idx);
  ID := ec.Clsid; Result := True;
end;
FreeMem(Codecs);
end;


function gpGetExtList: DGPFormatArr;
Var Decoders, Codecs, dc, ec : PImageCodecInfo;
    n, m, i, DecCnt, CodecCnt : Integer;
    s : String;
begin
Result := nil;
If (not gpInit) then Exit;
DecCnt := gpGetCodecs(True, Decoders);
CodecCnt := gpGetCodecs(False, Codecs);

If CodecCnt > DecCnt then begin
  Num_Swap(CodecCnt, DecCnt); Num_Swap(Decoders, Codecs);
end;

SetLength(Result, DecCnt);
dc := Decoders;

For n := 0 to DecCnt-1 do begin
  ec := Codecs; i := -1;
  For m:=0 to CodecCnt-1 do begin
    If IsEqualGUID(dc.FormatID, ec.FormatID) then begin
      i := m; Break;
    end;
    Inc(ec);
  end;
  CharLowerW(dc.FilenameExtension);
  Result[n].Ext := dc.FilenameExtension;
  Result[n].Desc := dc.FormatDescription;
  Result[n].FormatID := dc.FormatID;
  Result[n].CanSave := (i <> -1);
//  If (i <> -1) then s := 'S|' else s := '_|';
//  Result[n] := s + dc.FormatDescription + ';' + Str_LowerCase( dc.FilenameExtension );
  Inc(dc);
end;

FreeMem(Codecs); FreeMem(Decoders);
end;


function xGPImageGetSize(gpImg : GPImage; Var iSize : TImageSize): Integer;
Const
  PixelFormatMax = 15;
  PixelFormatBPPs : array[ 1..PixelFormatMax-1 ] of Byte =
    (1, 4, 8, 8, 16, 16, 32, 24, 32, 32, 32, 24, 32, 32);
Var w, h : DWord;
    format, fIndex : Integer;
    gType : TImageType;
begin
GdipGetImageWidth(gpImg, w); GdipGetImageHeight(gpImg, h);
GdipGetImagePixelFormat(gpImg, format);
fIndex := format and $FF;
If (fIndex < 1) or (fIndex > PixelFormatMax) then fIndex := 8; // 24 bpp

iSize.Width := w; iSize.Height := h; iSize.Bpp := PixelFormatBPPs[fIndex];
Result := format;
//GdipGetImageType(gpImg, gType);
//Result := (format and PixelFormatGDI <> 0) and (gType = ImageTypeBitmap);
end;

procedure xDrawMem(SrcPtr : PByte; SrcPitch : Integer; Dst : TFastDIB;
                   FlipV : Boolean = False);
Var n, i, w : Integer;
begin
w := Dst.BWidth; If Abs(SrcPitch) < w then w := Abs(SrcPitch);
For n:=0 to Dst.AbsHeight-1 do begin
  If FlipV then i := (Dst.AbsHeight-1) - n
           else i := n;
  Move(SrcPtr^, Dst.Scanlines[i]^, w);
  Inc(SrcPtr, SrcPitch);
end;
end;

function xCopyGP2FDIB(gpImg : GPImage; Dst : TFastDIB; Const sz : TImageSize;
                      gpFormat : Integer) : Boolean;
Var
  n, PalSize : Integer;
  Pal : PColorPalette;
  pc : PFColorA;
  bInfo : TBitmapData;
  gpGraph : GPGraphics;
  gType : TImageType;
begin
Dst.SetSize(sz.Width, sz.Height, sz.Bpp);

If (sz.Bpp <= 8) then begin
  GdipGetImagePaletteSize(gpImg, PalSize);
  GetMem(Pal, PalSize);
  GdipGetImagePalette(gpImg, Pal, PalSize);
  pc := @Pal.Entries;
  For n:=0 to Pal.Count-1 do begin Dst.Colors[n] := pc^; Inc(pc); end;
  Dst.UpdateColors;
  FreeMem(Pal);
end;

GdipGetImageType(gpImg, gType);
If (gpFormat and PixelFormatGDI <> 0) and (gType = ImageTypeBitmap) then begin
  // faster for 8 bpp
  Result := (GdipBitmapLockBits(gpImg, nil, 0, gpFormat, @bInfo) = Ok);
  If Result then
    xDrawMem(bInfo.Scan0, bInfo.Stride, Dst, True);
  GdipBitmapUnlockBits(gpImg, @bInfo);
end else
  If (Dst.hDC <> 0) then begin
    Result := (GdipCreateFromHDC(Dst.hDC, gpGraph) = Ok);
    If Result then
      GdipDrawImageI(gpGraph, gpImg, 0, 0);
    GdipDeleteGraphics(gpGraph);
  end;

end;

function xGPImage2FDIB(gpImg : GPImage; Dst : TFastDIB; iSize : PImageSize = nil) : Boolean;
Var sz : TImageSize;
    format : Integer;
begin
format := xGPImageGetSize(gpImg, sz);
If (iSize <> nil) then iSize^ := sz;
If (Dst <> nil) then Result := xCopyGP2FDIB(gpImg, Dst, sz, format)
                else Result := True;
end;


function gpStreamFromMem(Ptr : Pointer; Res : HGlobal; Size : Integer): IStream;
Var
 i64 : LargeUInt;
 Written : Integer;
begin
 CreateStreamOnHGlobal(Res, True, Result);
  // creating Stream from Res doesn't work really
 If (Result <> nil) and (Res = 0) and (Ptr <> nil) then begin
  Result.Write(Ptr, Size, @Written);
 If (Written = Size) then
  Result.Seek(0, 0, i64)
 else
  Result := nil;
 end;
end;

function gpLoadImageMem(Ptr : Pointer; Size : Integer;
                        Dst : TFastDIB): Boolean;
Var Stream : IStream;
    gpImg : GPImage;
    Res : HGlobal;
begin
Result := False; If (not gpInit) then Exit;
Res := 0;
Stream := gpStreamFromMem(Ptr, Res, Size);
gpImg := nil;
Result := (Stream <> nil) and (GdipLoadImageFromStream(Stream, gpImg) = Ok) and
          (xGPImage2FDIB(gpImg, Dst));
If (gpImg <> nil) then GdipDisposeImage(gpImg);
end;

function gpLoadImageRes(Const ResName : String; ResType : PChar;
                        Dst : TFastDIB): Boolean;
Var
  ResInfo: HRSRC;
  Global: THandle;
  Data : PByte;
  Size : Integer;
begin
Result := False; If (not gpInit) then Exit;
ResInfo := FindResource(HInstance, PChar(ResName), ResType);
If (ResInfo <> 0) then begin
  Global := LoadResource(HInstance, ResInfo);
  Size := SizeOfResource(HInstance, ResInfo);
  If (Global <> 0) and (Size > 0) then begin
    Data := LockResource(Global);
    If Data <> nil then begin
      Result := gpLoadImageMem(Data, Size, Dst);
      UnlockResource(Global);
    end;
    FreeResource(Global);
  end;
end;
end;


function gpLoadImage(Const FName : String; Dst : TFastDIB) : Boolean;
Var gpImg : GPImage;
    Err : GPStatus;
    Buffer: array [0..MAX_PATH] of WideChar;
begin
Result := False; If (not gpInit) then Exit;
gpImg := nil;
Err := GdipLoadImageFromFile(StringToWideChar(FName, Buffer, sizeof(Buffer)), gpImg);
Result := (Err = Ok) and (xGPImage2FDIB(gpImg, Dst));
If (gpImg <> nil) then GdipDisposeImage(gpImg);
end;


function gpSaveImage(Src : TFastDIB; Const FName : String;
                     Quality : Integer = 100) : Boolean;
{$IFNDEF USE_STD_HEADER}
Const
//---------------------------------------------------------------------------
// Image file format identifiers
//---------------------------------------------------------------------------
  ImageFormatUndefined : TGUID = '{b96b3ca9-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatMemoryBMP : TGUID = '{b96b3caa-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatBMP       : TGUID = '{b96b3cab-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatEMF       : TGUID = '{b96b3cac-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatWMF       : TGUID = '{b96b3cad-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatJPEG      : TGUID = '{b96b3cae-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatPNG       : TGUID = '{b96b3caf-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatGIF       : TGUID = '{b96b3cb0-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatTIFF      : TGUID = '{b96b3cb1-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatEXIF      : TGUID = '{b96b3cb2-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatIcon      : TGUID = '{b96b3cb5-0728-11d3-9d7b-0000f81ef32e}';

  EncoderCompression      : TGUID = '{e09d739d-ccd4-44ee-8eba-3fbf8be4fc58}';
  EncoderColorDepth       : TGUID = '{66087055-ad66-4c7c-9a18-38a2310b8337}';
  EncoderScanMethod       : TGUID = '{3a4e2661-3109-4e56-8536-42c156e7dcfa}';
  EncoderVersion          : TGUID = '{24d18c76-814a-41a4-bf53-1c219cccf797}';
  EncoderRenderMethod     : TGUID = '{6d42c53a-229a-4825-8bb7-5c99e2b9a8b8}';
  EncoderQuality          : TGUID = '{1d5be4b5-fa4a-452d-9cdd-5db35105e7eb}';
  EncoderTransformation   : TGUID = '{8d0eb2d1-a58e-4ea8-aa14-108074b7b6f9}';
  EncoderLuminanceTable   : TGUID = '{edb33bce-0266-4a77-b904-27216099e717}';
  EncoderChrominanceTable : TGUID = '{f2e455dc-09b3-4316-8260-676ada32481c}';
  EncoderSaveFlag         : TGUID = '{292266fc-ac40-47bf-8cfc-a85b89a655de}';
{$ENDIF}
Var Ext : String;
    Bitmap : GPImage;
    Encoder, eFmt : TGUID;
    wName : WideString;
    Res : GPStatus;
    Params : TENCODERPARAMETERS;
    ParamPtr : PEncoderParameters;
begin
Result := False; If (not gpInit) then Exit;

Ext := File_ExtractExt(FName);
If (Ext = '') or (gpCodecFromExt(Ext, Encoder, eFmt) = -1) then Exit;

// doesn't support subsets as Src
Res := GdipCreateBitmapFromGdiDib(PBitmapInfo(@Src.Info), Src.Bits, Bitmap);
If (Integer(Res) = 0) then begin
  wName := FName;

//  If Mem_Equal(@eFmt, @ImageFormatJPEG, SizeOf(TGUID)) then begin
  If IsEqualGUID(eFmt, ImageFormatJPEG) then begin
    // seems that this works only for jpeg
    // ... and only 32 bit (x86)
    Params.Count := 1;
    Params.Parameter[0].Guid := EncoderQuality;
    Params.Parameter[0].NumberOfValues := 1;
    Params.Parameter[0].Type_ := EncoderParameterValueTypeLong;
    Params.Parameter[0].Value := @Quality;
    ParamPtr := @Params;
  end else
    ParamPtr := nil;

  // note: при сохранении 8bpp tiff программа подвисает,
  // хотя картинка сохраняется
  Res := GdipSaveImageToFile(Bitmap, PWideChar(wName), @Encoder, ParamPtr);
  Result := (Integer(Res) = 0);
end;
end;


initialization
finalization
gpClose;
{$IFNDEF USE_STD_HEADER}
If (gpDLL <> 0) then begin FreeLibrary(gpDLL); gpDLL := 0; end;
{$ENDIF}

end.
