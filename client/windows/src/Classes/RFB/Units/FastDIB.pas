unit FastDIB;    // FastDIB: sourceforge.net/projects/tfastdib
                 // by: gordy <gordy@dynamicsdirect.com>
interface        //     cmr.Pent <cmr.Pent@gmail.com>
                 //     Sapersky <sapersky@gmail.com>
{
History:
09.04.13, Sapersky:
  Changes at Assign/SetInterface to use reference counting
  Overloaded constructors added
  GenLut16, GenBackLut16 added (built-in 16 bpp conversion tables)
  x64 ready
}
{$R-}
uses Windows, uAegysUtils;

{$I platform.inc}
type
  PFColor =^TFColor;
  TFColor = packed record
    b,g,r: Byte;
  end;

  PFColorA =^TFColorA;
  TFColorA = packed record
    case Integer of
      0: (i: DWord);
      1: (c: TFColor);
      2: (hi,lo: Word);
      3: (b,g,r,a: Byte);
    end;

  PFColorTable =^TFColorTable;
  TFColorTable = array[Byte]of TFColorA;

  PFPackedColorTable =^TFPackedColorTable;
  TFPackedColorTable = array[Byte]of TFColor;

  TLines    = array[Word]of Pointer;  PLines    =^TLines;
  TLine8    = array[Word]of Byte;     PLine8    =^TLine8;
  TLine16   = array[Word]of Word;     PLine16   =^TLine16;
  TLine24   = array[Word]of TFColor;  PLine24   =^TLine24;
  TLine32   = array[Word]of TFColorA; PLine32   =^TLine32;
  TPixels8  = array[Word]of PLine8;   PPixels8  =^TPixels8;
  TPixels16 = array[Word]of PLine16;  PPixels16 =^TPixels16;
  TPixels24 = array[Word]of PLine24;  PPixels24 =^TPixels24;
  TPixels32 = array[Word]of PLine32;  PPixels32 =^TPixels32;

  PBMInfoHeader =^TBMInfoHeader; // = windows.TBitmapInfoHeader
  TBMInfoHeader = packed record
    Size:          DWord;
    Width:         Integer;
    Height:        Integer;
    Planes:        Word;
    BitCount:      Word;
    Compression:   DWord;
    SizeImage:     DWord;
    XPelsPerMeter: Integer;
    YPelsPerMeter: Integer;
    ClrUsed:       DWord;
    ClrImportant:  DWord;
  end;

  PBMInfo =^TBMInfo; // = windows.TBitmapInfo
  TBMInfo = packed record
    Header: TBMInfoHeader;
    case Integer of
      0: (Colors: TFColorTable);
      1: (RMask,GMask,BMask: DWord);
    end;

  PLut16 = ^TLut16;
  TLut16 = array [Word] of TFColor;

  PBackLut16 = ^TBackLut16;
  TBackLut16 = record
    ri, gi, bi: array [Byte] of Word;
  end;

Const
  UseLut16Offsets = True;
//  UseLut16Offsets = False;
  // more correct brightness for 16<>24 conversions,
  // (changed behaviour comparing to older versions)

Type
  TFastDIB = class
    hDC:    HDC;     // handle to memory device context (when UseGDI=True)
    hBrush: HBRUSH;  // current brush in hDC
    hPen:   HPEN;    // current pen in hDC
    hFont:  HFONT;   // current font in hDC
    Handle: HBITMAP; // current DIB in hDC

    BWidth:    Integer;      // number of bytes per scanline (aka Pitch)
    AbsHeight: Integer;      // number of scanlines in bitmap
    Gap:       Integer;      // number of pad bytes at end of scanline
    Bits:      PLine8;       // typed pointer to bits
    Colors:    PFColorTable; // typed pointer to color table
    Info:      TBMInfo;      // bitmap information

    BytesPP : Integer; // Bpp div 8
    Bpb,Bpg,Bpr:    Byte; // bits per channel (only 16 & 32bpp)
    BShr,GShr,RShr: Byte; // (B shr BShr)or(G shr GShr shl GShl)or
    BShl,GShl,RShl: Byte; // (R shr RShr shl RShl) = 16bit/32bit pixel

    Scanlines:  PLines;    // typed pointer to array of scanline offsets
    Pixels8:    PPixels8;  // typed scanlines - Pixels8[y,x]:  Byte
    Pixels16:   PPixels16; // typed scanlines - Pixels16[y,x]: Word
    Pixels24:   PPixels24; // typed scanlines - Pixels24[y,x]: TFColor
    Pixels32:   PPixels32; // typed scanlines - Pixels32[y,x]: TFColorA

    UseGDI:     Boolean; // default true, allocate GDI handle & surface
    FreeDC:     Boolean; // default true, free GDI surface on destroy
    FreeBits:   Boolean; // default true, free Bits on destroy (non GDI only)
    FreeHandle: Boolean; // default true, free GDI handle on destroy
  private
    RefCnt : PInteger;
    Lut16 : PLut16; // LUTs for 16 bpp conversions - allocated as needed
    BackLut16 : PBackLut16; // placed here to skip allocation at every 16 bpp proc
  public
    constructor Create; overload;
    constructor Create(AWidth, AHeight, ABpp : Integer;
                       AUseGDI : Boolean = True); overload;
    constructor Create(ASrc : TFastDIB; AMakeCopy : Boolean = False;
                       AForceBpp : Integer = -1; AUseGDI : Boolean = True); overload;
    constructor Create(Const AFilename : String; AsResource : Boolean = False;
                       AUseGDI : Boolean = True); overload;
    destructor Destroy; override;
    procedure PreDestroy;

    // note: RefCounts (FreeSrc = False) are not thread-safe (unlike strings etc.)
    procedure Assign(Src : TFastDIB; FreeSrc : Boolean = True);
    procedure Release; // not needed really, for compability with 3.9.9

    // use these for debugging or reference (these don't belong in long loops)
    // (Sap: anybody really use it?)
    procedure SetPixel(y,x:Integer;c:TFColor);
    procedure SetPixelB(y,x:Integer;c:Byte);
    function GetPixel(y,x:Integer):TFColor;
    function GetPixelB(y,x:Integer):Byte;
    property Pixels[y,x:Integer]:TFColor read GetPixel write SetPixel;
    property PixelsB[y,x:Integer]:Byte read GetPixelB write SetPixelB;

    // convenience (BCB doesn't like this)
    property Width: Integer read Info.Header.Width write Info.Header.Width;
    property Height: Integer read Info.Header.Height write Info.Header.Height;
    property Bpp: Word read Info.Header.BitCount write Info.Header.BitCount;
    property Compression: DWord read Info.Header.Compression write Info.Header.Compression;
    property Size: DWord read Info.Header.SizeImage write Info.Header.SizeImage;
    property ClrUsed: DWord read Info.Header.ClrUsed write Info.Header.ClrUsed;
    property RMask: DWord read Info.RMask write Info.RMask;
    property GMask: DWord read Info.GMask write Info.GMask;
    property BMask: DWord read Info.BMask write Info.BMask;

    // initializers
    procedure SetSize(fWidth,fHeight:Integer;fBpp:Byte);
    procedure SetSizeEx(fWidth,fHeight:Integer;fBpp,fBpr,fBpg,fBpb:Byte);
    procedure SetSizeIndirect(Const bmInfo : TBMInfo);
    procedure SetInterface(fBits:Pointer;fWidth,fHeight:Integer;fBpp,fBpr,fBpg,fBpb:Byte);
    procedure SetInterfaceIndirect(fBits : Pointer; Const bmInfo : TBMInfo;
                                   bmHasColors : Boolean = True);
    procedure SetSubset(Bmp:TFastDIB;x,y,w,h:Integer); overload;
    procedure SetSubset(Bmp:TFastDIB; Const r : TRect); overload;
    procedure MakeCopy(Bmp:TFastDIB;CopyBits:Boolean);
    procedure LoadFromHandle(hBmp:HBITMAP);
    function LoadFromFile(Const FileName : String): Boolean;
    function LoadFromRes(hInst:HINST;ResID,ResType:PChar): Boolean;
    procedure LoadFromClipboard;
    procedure AttachToHandle(hDC:HDC;hBmp:HBITMAP);

    // blitting methods
    procedure UpdateColors;
    procedure Draw(fDC:HDC;x,y:Integer);
    procedure Stretch(fDC:HDC;x,y,w,h:Integer);
    procedure DrawRect(fDC:HDC;x,y,w,h,sx,sy:Integer); overload;
    procedure DrawRect(fDC:HDC; Const SrcRect : TRect; dx, dy : Integer); overload;
    procedure StretchRect(fDC:HDC;x,y,w,h,sx,sy,sw,sh:Integer);
    procedure PlgDraw(fDC:HDC;x1,y1,x2,y2,x3,y3:Integer);
    procedure MaskDraw(fDC:HDC;x,y:Integer;Mono:TFastDIB);
    procedure MaskRect(fDC:HDC;x,y,w,h,sx,sy,mx,my:Integer;Mono:TFastDIB);
    procedure TransDraw(fDC:HDC;x,y:Integer;c:TFColor);
    procedure TransStretch(fDC:HDC;x,y,w,h:Integer;c:TFColor);
    procedure AlphaDraw(fDC:HDC;x,y:Integer;a:Byte;hasAlpha:Boolean);
    procedure AlphaStretch(fDC:HDC;x,y,w,h:Integer;a:Byte;hasAlpha:Boolean);
    procedure TileDraw(fDC:HDC;x,y,w,h:Integer);

    // drawing tools (UseGDI=True)
    procedure SetPen(Style,Width,Color:DWord);
    procedure SetBrush(Style,Hatch,Color:DWord);
    procedure SetFont(Font:string;Size:Integer);
    procedure SetFontEx(Font:string;Width,Height,Weight:Integer;Italic,Underline,Strike:Boolean);
    procedure SetTextColor(Color:DWord);
    procedure SetTransparent(Transparent:Boolean);
    procedure SetBkColor(Color:DWord);
    procedure Ellipse(x1,y1,x2,y2:Integer);
    procedure FillRect(Rect:TRect);
    procedure LineTo(x,y:Integer);
    procedure MoveTo(x,y:Integer);
    procedure Polygon(Points:array of TPoint);
    procedure Polyline(Points:array of TPoint);
    procedure Rectangle(x1,y1,x2,y2:Integer);
    procedure TextOut(x,y:Integer;Text:string);
    procedure DrawText(Text:string;Rect:TRect;Flags:Integer);

    // utilities
    procedure Clear(c:TFColor);
    procedure ClearB(c:DWord);
    procedure SaveToClipboard;
    procedure SaveToFile(FileName:string);
    procedure CopyRect(Src:TFastDIB;x,y,w,h,sx,sy:Integer);
    procedure FillColors(i1,i2:Integer;Keys:array of TFColor);
    procedure ShiftColors(i1,i2,Amount:Integer);

    function GenLut16(Lut8 : PLine8 = nil): PLut16;
    function GenBackLut16(Lut8 : PLine8 = nil): PBackLut16;
  end;

  TCPUFeature = (cfCX8,cfCMOV,cfMMX,cfMMX2,cfSSE,cfSSE2,cf3DNow,cf3DNow2);
  TCPUFeatureSet = set of TCPUFeature;

  TCPUInfo = record
    VendorID: string[12];
    Features: TCPUFeatureSet;
    CPUCount,Family,Model: Byte;
  end;
{
  TBlendFunction = record
    BlendOp,BlendFlags,Alpha,Format: Byte;
  end;
}
var
  CPUInfo: TCPUInfo;

// Windows.pas already has this at Delphi5 and later...
{
  TransBlit: function(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11:DWord):DWord; stdcall;
  AlphaBlit: function(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10:DWord;p11:TBlendFunction):DWord; stdcall;

function CreateDIB(fDC:HDC;bmInfo:PBMInfo;iColor:DWord;var Bits:PLine8;hSection,dwOffset:DWord):HBITMAP; stdcall;
function MaskBlit(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12:DWord):DWord; stdcall;
}

// extra goodies(8.3k)!
procedure SetAlphaChannel(Bmp,Alpha:TFastDIB);
procedure MultiplyAlpha(Bmp:TFastDIB);
procedure SwapChannels(Bmp:TFastDIB);
procedure FillMem(Mem:Pointer;Size,Value:Integer);
procedure Clear(Bmp:TFastDIB;c:TFColor);
procedure ClearB(Bmp:TFastDIB;c:DWord);
procedure DecodeRLE4(Bmp:TFastDIB;Data:Pointer);
procedure DecodeRLE8(Bmp:TFastDIB;Data:Pointer);
procedure FillColors(Pal:PFColorTable;i1,i2,nKeys:Integer;Keys:PLine24);
procedure FillGray(Pal : PFColorTable; Bpp : Integer);
function  ClosestColor(Pal:PFColorTable;Max:Integer;c:TFColor):Byte;
function  LoadHeader(Data:Pointer;var bmInfo:TBMInfo):Integer;
function  PackedDIB(Bmp:TFastDIB):Pointer;
function  CountColors(Bmp:TFastDIB):DWord;

procedure IntToMask(Bpr,Bpg,Bpb:DWord;var RMsk,GMsk,BMsk:DWord);
procedure MaskToInt(RMsk,GMsk,BMsk:DWord;var Bpr,Bpg,Bpb:DWord);
function  UnpackColorTable(Table:TFPackedColorTable):TFColorTable;
function  PackColorTable(Table:TFColorTable):TFPackedColorTable;
function  FRGB(r,g,b:Byte):TFColor;
function  FRGBA(r,g,b,a:Byte):TFColorA;
function  ColorToInt(c:TFColor):DWord;
function  ColorToIntA(c:TFColorA):DWord;
function  IntToColor(i:DWord):TFColor;
function  IntToColorA(i:DWord):TFColorA;
function  Scale8(i,n:Integer):Integer;
function  Get16Bpg:Byte;

procedure EMMS;
function BmpPitch(Width, Bpp : Integer; Align: Integer = 32): Integer;
procedure CopyDIB(Src, Dst : TFastDIB);
procedure CopyPal(Src, Dst : TFastDIB);
procedure FillMem24(Dst : PLine24; Cnt : Integer; Color : TFColor);
function GrowSize(FDIB : TFastDIB; NewWidth, NewHeight, Bpp : Integer;
                  MaxSize : PPoint = nil; AddVal : Integer = 0): Boolean;

const
  tfBlack:   TFColor=(b:$00;g:$00;r:$00);
  tfMaroon:  TFColor=(b:$00;g:$00;r:$80);
  tfGreen:   TFColor=(b:$00;g:$80;r:$00);
  tfOlive:   TFColor=(b:$00;g:$80;r:$80);
  tfNavy:    TFColor=(b:$80;g:$00;r:$00);
  tfPurple:  TFColor=(b:$80;g:$00;r:$80);
  tfTeal:    TFColor=(b:$80;g:$80;r:$00);
  tfGray:    TFColor=(b:$80;g:$80;r:$80);
  tfSilver:  TFColor=(b:$C0;g:$C0;r:$C0);
  tfRed:     TFColor=(b:$00;g:$00;r:$FF);
  tfLime:    TFColor=(b:$00;g:$FF;r:$00);
  tfYellow:  TFColor=(b:$00;g:$FF;r:$FF);
  tfBlue:    TFColor=(b:$FF;g:$00;r:$00);
  tfFuchsia: TFColor=(b:$FF;g:$00;r:$FF);
  tfAqua:    TFColor=(b:$FF;g:$FF;r:$00);
  tfLtGray:  TFColor=(b:$C0;g:$C0;r:$C0);
  tfDkGray:  TFColor=(b:$80;g:$80;r:$80);
  tfWhite:   TFColor=(b:$FF;g:$FF;r:$FF);

implementation

//function CreateDIB; external 'gdi32.dll' name 'CreateDIBSection';
//function MaskBlit;  external 'gdi32.dll' name 'MaskBlt';

constructor TFastDIB.Create;
begin
  inherited Create;
  UseGDI:=True;
  Bits:=nil;
  Scanlines:=nil;
  FillChar(Info,SizeOf(Info),0);
  Info.Header.Size:=SizeOf(TBMInfoHeader);
  Info.Header.Planes:=1;
  Colors:=@Info.Colors;
  GetMem(RefCnt, SizeOf(Integer)); RefCnt^ := 1;
end;

constructor TFastDIB.Create(AWidth, AHeight, ABpp : Integer; AUseGDI : Boolean = True);
begin
  Create;
  UseGDI := AUseGDI;
  SetSize(AWidth, AHeight, ABpp);
  If (ABpp <= 8) then begin FillGray(Colors, ABpp); UpdateColors; end;
end;

constructor TFastDIB.Create(ASrc : TFastDIB; AMakeCopy : Boolean = False;
                            AForceBpp : Integer = -1; AUseGDI : Boolean = True);
begin
  Create;
  If AForceBpp = -1 then AForceBpp := ASrc.Bpp;
  UseGDI := AUseGDI;
//  MakeCopy(ASrc, False);
  // here Bpp = 16 mean really 16 (5-6-5), at SetSize it mean 15 (5-5-5)
  If AForceBpp = 16 then SetSizeEx(ASrc.Width, ASrc.Height, AForceBpp, 5, 6, 5)
                    else SetSize(ASrc.Width, ASrc.Height, AForceBpp);
  CopyPal(ASrc, Self);
  If AMakeCopy then CopyDIB(ASrc, Self);
end;

constructor TFastDIB.Create(Const AFilename : String; AsResource : Boolean = False;
                            AUseGDI : Boolean = True);
begin
  Create;
  UseGDI := AUseGDI;
  If AsResource then LoadFromRes(hInstance, PChar(AFilename), RT_BITMAP)
                else LoadFromFile(AFilename);
end;

destructor TFastDIB.Destroy;
begin
  PreDestroy;
  If (RefCnt^ <= 0) then FreeMem(RefCnt);
  inherited Destroy;
end;

procedure TFastDIB.PreDestroy;
begin
  Dec(RefCnt^);
  If (RefCnt^ <= 0) then begin
    if(hDC<>0)and FreeDC then DeleteDC(hDC);
    if(Handle<>0)and FreeHandle then DeleteObject(Handle);
    if(hPen<>0)then DeleteObject(hPen);
    if(hFont<>0)then DeleteObject(hFont);
    if(hBrush<>0)then DeleteObject(hBrush);
    If (Scanlines <> nil) then ReallocMem(Scanlines,0);
    If (Bits <> nil) and (FreeBits) then ReallocMem(Bits,0);
    ReallocMem(Lut16, 0); ReallocMem(BackLut16, 0);
    // ReallocMem sets pointers to nil
  end else begin
    Scanlines := nil;
    Lut16 := nil; BackLut16 := nil;
  end;

  Handle := 0; FreeHandle := False;
  hDC := 0; FreeDC := False;
  FreeBits := False; Bits := nil;
end;

procedure TFastDIB.Assign(Src : TFastDIB; FreeSrc : Boolean = True);
begin
  PreDestroy;
  If (RefCnt^ <= 0) then FreeMem(RefCnt);

  hDC:=Src.hDC;             hBrush:=Src.hBrush;
  hPen:=Src.hPen;           hFont:=Src.hFont;
  Handle:=Src.Handle;       BWidth:=Src.BWidth;
  AbsHeight:=Src.AbsHeight; Gap:=Src.Gap;
  Bits:=Src.Bits;           Colors^:=Src.Colors^;
  Info:=Src.Info;           BShr:=Src.BShr;
  GShr:=Src.GShr;           GShl:=Src.GShl;
  RShr:=Src.RShr;           RShl:=Src.RShl;
  Bpr:=Src.Bpr;             Bpg:=Src.Bpg;
  Bpb:=Src.Bpb;             Scanlines:=Src.Scanlines;
  Pixels8:=Src.Pixels8;     Pixels16:=Src.Pixels16;
  Pixels24:=Src.Pixels24;   Pixels32:=Src.Pixels32;
  UseGDI:=Src.UseGDI;       FreeDC:=Src.FreeDC;
  FreeBits:=Src.FreeBits;   FreeHandle:=Src.FreeHandle;

  Lut16 := Src.Lut16; BackLut16 := Src.BackLut16;
  RefCnt := Src.RefCnt;

  Inc(RefCnt^);
  If FreeSrc then Src.Free;
{
  Src.FreeDC:=False;
  Src.FreeHandle:=False;
  Src.hPen:=0;
  Src.hFont:=0;
  Src.hBrush:=0;
  Src.Scanlines:=nil;
  Src.FreeBits:=False;
  Src.Free;
}
end;

procedure TFastDIB.Release;
begin
end;

procedure TFastDIB.SetPixel(y,x:Integer;c:TFColor);
begin
  case Bpp of
    1,4,8: PixelsB[y,x]:=ClosestColor(Colors,(1 shl Bpp)-1,c);
    16: Pixels16[y,x]:=
          c.r shr RShr shl RShl or
          c.g shr GShr shl GShl or
          c.b shr BShr;
    24: Pixels24[y,x]:=c;
    32: if Compression=0 then Pixels32[y,x].c:=c else
        Pixels32[y,x].i:=
          c.r shr RShr shl RShl or
          c.g shr GShr shl GShl or
          c.b shr BShr;
  end;
end;

procedure TFastDIB.SetPixelB(y,x:Integer;c:Byte);
var
  mo: Byte;
  pb: PByte;
begin
  case Bpp of
    1:
    begin
      c:=c and 1;
      mo:=(x and 7)xor 7;
      pb:=@Pixels8[y,x shr 3];
      pb^:=pb^ and(not(1 shl mo))or(c shl mo);
    end;
    4:
    begin
      c:=c and 15;
      pb:=@Pixels8[y,x shr 1];
      if(x and 1)=0 then pb^:=(pb^and $0F)or(c shl 4)else pb^:=(pb^and $F0)or c;
    end;
    8: Pixels8[y,x]:=c;
  end;
end;

function TFastDIB.GetPixel(y,x:Integer):TFColor;
var
  w: Word;
  d: DWord;
begin
  case Bpp of
    1,4,8: Result:=Colors[PixelsB[y,x]].c;
    16:
    begin
      w:=Pixels16[y,x];
      Result.b:=Scale8(w and BMask,Bpb);
      Result.g:=Scale8(w and GMask shr GShl,Bpg);
      Result.r:=Scale8(w and RMask shr RShl,Bpr);
    end;
    24: Result:=Pixels24[y,x];
    32:
    if Compression=0 then Result:=Pixels32[y,x].c else
    begin
      d:=Pixels32[y,x].i;
      Result.b:=Scale8(d and BMask,Bpb);
      Result.g:=Scale8(d and GMask shr GShl,Bpg);
      Result.r:=Scale8(d and RMask shr RShl,Bpr);
    end;
  end;
end;

function TFastDIB.GetPixelB(y,x:Integer):Byte;
var
  mo: Byte;
begin
  case Bpp of
    1:
    begin
      mo:=(x and 7)xor 7;
      Result:=Pixels8[y,x shr 3]and(1 shl mo)shr mo;
    end;
    4: if(x and 1)=0 then Result:=Pixels8[y,x shr 1]shr 4 else Result:=Pixels8[y,x shr 1]and 15;
    8: Result:=Pixels8[y,x];
    else Result:=0;
  end;
end;

procedure TFastDIB.SetSize(fWidth,fHeight:Integer;fBpp:Byte);
begin
  SetInterface(nil,fWidth,fHeight,fBpp,0,0,0);
end;

procedure TFastDIB.SetSizeEx(fWidth,fHeight:Integer;fBpp,fBpr,fBpg,fBpb:Byte);
begin
  SetInterface(nil,fWidth,fHeight,fBpp,fBpr,fBpg,fBpb);
end;

procedure TFastDIB.SetSizeIndirect(Const bmInfo : TBMInfo);
begin
  SetInterfaceIndirect(nil, bmInfo);
end;

procedure TFastDIB.SetInterface(fBits:Pointer;fWidth,fHeight:Integer;fBpp,fBpr,fBpg,fBpb:Byte);
var
  x,i: Integer;
  sDC: Windows.HDC;
  pb : PByte;
  p : Pointer;
begin

  if fBpp=0 then
  begin
    sDC:=GetDC(0);
    fBpp:=GetDeviceCaps(sDC,BITSPIXEL);
    ReleaseDC(0,sDC);
    if fBpp=16 then
    begin
      fBpr:=5;
      fBpg:=Get16Bpg;
      fBpb:=5;
    end
    else if fBpp=32 then
    begin
      fBpr:=8;
      fBpg:=8;
      fBpb:=8;
    end;
  end;

  if(fBpr=0)and(fBpg=0)and(fBpb=0)then
  begin
    Compression := BI_RGB; // common color masks
    if fBpp=16 then
    begin
      fBpr:=5;
      fBpg:=5;
      fBpb:=5;
    end
    else if fBpp=32 then
    begin
      fBpr:=8;
      fBpg:=8;
      fBpb:=8;
    end;
  end else Compression := BI_BITFIELDS;
  // custom color masks (bpr, bpg, bpb - 16/32 bpp)

  if(fBpp=16)or(fBpp=32)then
    IntToMask(fBpr,fBpg,fBpb,Info.RMask,Info.GMask,Info.BMask);

  if ( (fBits=nil) and (fWidth=Width) and (fHeight=Height) and
       (fBpp=Bpp) and (fBpr=Bpr) and (fBpg=Bpg) and (fBpb=Bpb) ) and
      // new UseGDI = current UseGDI
     ( (UseGDI) and (hDC<>0) ) then Exit;

  // if 8 bpp bitmap is created for the 1st time,
  // fill gray palette, which is usually needed
  // even if not needed, it is very fast

  //If (fBpp = 8) and (Size = 0) and (fBits = nil) then
  //  FillGray(@Info.Colors);

  Width:=fWidth;            Height:=fHeight;
  AbsHeight:=Abs(fHeight);  Bpp:=fBpp;
  Bpr:=fBpr;                Bpg:=fBpg;
  Bpb:=fBpb;                GShl:=Bpb;
  RShl:=Bpb+Bpg;
  BytesPP := Bpp shr 3;

  if Bpb<8 then BShr:=8-Bpb else BShr:=0;
  if Bpg<8 then GShr:=8-Bpg else GShr:=0;
  if Bpr<8 then RShr:=8-Bpr else RShr:=0;
{
  case Bpp of
    1:
    begin
      x:=(Width+7)and -8;
      BWidth:=((x+31)and -32)shr 3;
      Gap:=BWidth-(x shr 3);
    end;
    4:
    begin
      x:=((Width shl 2)+7)and -8;
      BWidth:=((x+31)and -32)shr 3;
      Gap:=BWidth-(x shr 3);
    end;
    8:
    begin
      BWidth:=(((Width shl 3)+31)and -32)shr 3;
      Gap:=BWidth-Width;
    end;
    16:
    begin
      BWidth:=(((Width shl 4)+31)and -32)shr 3;
      Gap:=BWidth-(Width shl 1);
    end;
    24:
    begin
      BWidth:=(((Width*24)+31)and -32)shr 3;
      Gap:=BWidth-((Width shl 1)+Width);
    end;
    32:
    begin
      BWidth:=(((Width shl 5)+31)and -32)shr 3;
      Gap:=0;
    end;
  end;
}
  // Pitch calculation from Graphics.pas... MUCH shorter
  BWidth := BmpPitch(Width, Bpp);
  i := Width * Bpp;
  If Bpp < 8 then
    i := ((i + 7) and -8); // to make upper-rounding (shr is lower)
  Gap := BWidth - (i shr 3);

  Size := AbsHeight * BWidth;

  // a bit surprising, but FreeMem + GetMem (UseGDI = False) is faster than ReallocMem
  // especially with FastMM (ReallocMem(nil) = GetMem)
  PreDestroy;

  // RefCnt > 0 - some image(s) use this RefCnt, so leave it and create new
  If (RefCnt^ > 0) then begin GetMem(RefCnt, SizeOf(Integer)); RefCnt^ := 1; end
                   else Inc(RefCnt^);

  if(fBits<>nil)then Bits:=fBits else
  begin
{
    if(hDC<>0)and FreeDC then DeleteDC(hDC);
    if(Handle<>0)and FreeHandle then DeleteObject(Handle);
    if(hPen<>0)then DeleteObject(hPen);
    if(hFont<>0)then DeleteObject(hFont);
    if(hBrush<>0)then DeleteObject(hBrush);
}
    if UseGDI then
    begin
      //if(Bits<>nil)and FreeBits then ReallocMem(Bits,0);
      Handle := CreateDIBSection(0, PBitmapInfo(@Info)^, 0, p, 0, 0);
      Bits := p;
      //if (Handle = 0) then Sys_RaiseLastError('CreateDIB');

      hDC:=CreateCompatibleDC(0);
      SelectObject(hDC,Handle);
      //FreeBits:=False;
      FreeDC:=True;
      FreeHandle:=True;
      If (Bpp <= 8) then UpdateColors;
    end else
    begin
      //if not FreeBits then Bits:=nil;
      ReallocMem(Bits,Size);
      FreeBits:=True;
{
      FreeDC:=False;
      FreeHandle:=False;
      Handle:=0;
      hDC:=0;
}
    end;
  end;

  ReallocMem(Scanlines, AbsHeight * SizeOf(Pointer));
  Pixels8:=Pointer(Scanlines);
  Pixels16:=Pointer(Scanlines);
  Pixels24:=Pointer(Scanlines);
  Pixels32:=Pointer(Scanlines);

  if AbsHeight>0 then begin
    pb := PByte(Bits);
    for i:=0 to AbsHeight-1 do begin
      Scanlines[i] := pb;
      Inc(pb, BWidth);
    end;
  end;
end;

procedure TFastDIB.SetInterfaceIndirect(fBits : Pointer; Const bmInfo : TBMInfo;
                                        bmHasColors : Boolean = True);
var
  r,g,b: DWord;
  Comp : Integer;
begin
{
  if(bmInfo.Header.Compression=1)or(bmInfo.Header.Compression=2)then
    if(bmInfo.RMask<>0)or(bmInfo.GMask<>0)or(bmInfo.BMask<>0)then
      bmInfo.Header.Compression:=3 else bmInfo.Header.Compression:=0;
  if((bmInfo.Header.BitCount=16)or(bmInfo.Header.BitCount=32))
  and(bmInfo.Header.Compression=3)then
    MaskToInt(bmInfo.RMask,bmInfo.GMask,bmInfo.BMask,r,g,b)else
  begin
    r:=0; g:=0; b:=0;
  end;
  if bmInfo.Header.BitCount<=8 then Colors^:=bmInfo.Colors;
  SetInterface(fBits, bmInfo.Header.Width, bmInfo.Header.Height,
               bmInfo.Header.BitCount, r, g, b);
}
  Comp := bmInfo.Header.Compression;
  if (Comp in [BI_RLE8, BI_RLE4]) then
    if(bmInfo.RMask <> 0) or (bmInfo.GMask <> 0) or (bmInfo.BMask <> 0)then
      Comp := BI_BITFIELDS
    else
      Comp := BI_RGB;

  if (bmInfo.Header.BitCount in [16, 32]) and (Comp = BI_BITFIELDS)
  then
    MaskToInt(bmInfo.RMask, bmInfo.GMask, bmInfo.BMask, r, g, b)
  else begin
    r := 0; g := 0; b := 0;
  end;
  if (bmHasColors) and (bmInfo.Header.BitCount <= 8) then Colors^ := bmInfo.Colors;
  SetInterface(fBits, bmInfo.Header.Width, bmInfo.Header.Height,
               bmInfo.Header.BitCount, r, g, b);

end;

procedure TFastDIB.SetSubset(Bmp:TFastDIB; Const r : TRect);
begin
SetSubset(Bmp, r.Left, r.Top, r.Right - r.Left, r.Bottom - r.Top);
end;

procedure TFastDIB.SetSubset(Bmp:TFastDIB;x,y,w,h:Integer);
var
  xOff,i: Integer;
  pb : PByte;
begin
  if(Bmp.Bits=nil)or(x>=Bmp.Width)or(y>=Bmp.AbsHeight)then Exit;

  if Bmp.Height>0 then y:=Bmp.AbsHeight-h-y;

  if x<0 then
  begin
    Inc(w,x);
    x:=0;
  end;

  if y<0 then
  begin
    Inc(h,y);
    y:=0;
  end;

  if w+x>=Bmp.Width then w:=Bmp.Width-x;
  if h+y>=Bmp.AbsHeight then h:=Bmp.AbsHeight-y;

  if(w<=0)or(h<=0)then Exit;

  xOff:=0;
  case Bmp.Bpp of
    1:  xOff:=x shr 3;
    4:  xOff:=x shr 1;
    8:  xOff:=x;
    16: xOff:=x shl 1;
    24: xOff:=x*3; // lea xOff,[x+x*2] ! (cool)
    32: xOff:=x shl 2;
  end;

//  Bits := Bmp.Scanlines[y]; Inc(PByte(Bits), xOff);
  pb := Bmp.Scanlines[y]; Inc(pb, xOff);
  Bits := PLine8(pb);
  SetInterface(Bits,w,h,Bmp.Bpp,Bmp.Bpr,Bmp.Bpg,Bmp.Bpb);
  Size:=0;
  Dec(BWidth,Gap);
  Gap:=Bmp.BWidth-BWidth;
  Inc(BWidth,Gap);
  Colors:=Bmp.Colors;
  hDC:=Bmp.hDC;
  FreeDC:=False;
  Handle:=Bmp.Handle;
  FreeHandle:=False;
  //pb := PByte(Bits);
  for i:=0 to AbsHeight-1 do begin
    Scanlines[i] := pb;
    Inc(pb, BWidth);
  end;
end;

procedure TFastDIB.MakeCopy(Bmp:TFastDIB; CopyBits:Boolean);
begin
  SetSizeIndirect(Bmp.Info);
  if CopyBits then Move(Bmp.Bits^,Bits^,Size);
end;

procedure TFastDIB.LoadFromHandle(hBmp:HBITMAP);
var
  dsInfo: TDIBSection;
begin
  if GetObject(hBmp,SizeOf(dsInfo),@dsInfo) = SizeOf(TDIBSection) then
  begin
    SetInterfaceIndirect(nil, PBMInfo(@dsInfo.dsBmih)^, False);
    Case dsInfo.dsBmih.biCompression of
      BI_RLE8 : DecodeRLE8(Self, dsInfo.dsBm.bmBits);
      BI_RLE4 : DecodeRLE4(Self, dsInfo.dsBm.bmBits);
      else Move(dsInfo.dsBm.bmBits^, Bits^, dsInfo.dsBmih.biSizeImage);
    end;
    if Bpp<=8 then
    begin
      GetDIBits(hDC,hBmp,0,0,nil,PBitmapInfo(@Info)^,0);
      UpdateColors;
    end;
  end else
  begin
    SetSize(dsInfo.dsBm.bmWidth, dsInfo.dsBm.bmHeight, 0); // 0??
    GetDIBits(hDC, hBmp, 0, AbsHeight, Bits, PBitmapInfo(@Info)^, 0);
    if Bpp<=8 then UpdateColors;
  end;
end;

function TFastDIB.LoadFromFile(Const FileName : String): Boolean;
var
  i: DWord;
  Buffer: Pointer;
  bmInfo: TBMInfo;
  hFile:  THandle;// Windows.HFILE;
  fBits,xSize,fSize: DWord;
  Comp : Integer;
begin
  Result := False;
  hFile:=CreateFile(PChar(FileName),GENERIC_READ,FILE_SHARE_READ,nil,OPEN_EXISTING,0,0);
  if (hFile = INVALID_HANDLE_VALUE) then Exit;

  fSize:=GetFileSize(hFile,nil);
  xSize:=fSize; if xSize>1078 then xSize:=1078;
  GetMem(Buffer,1078);
  ReadFile(hFile,Buffer^,xSize,i,nil);
  fBits := LoadHeader(Buffer, bmInfo);
  If fBits > 0 then begin
    SetSizeIndirect(bmInfo);
    SetFilePointer(hFile,fBits-xSize,nil,FILE_CURRENT);
    Comp := bmInfo.Header.Compression;
    if (Comp in [BI_RLE8, BI_RLE4]) then
      xSize := PDWord(NativeInt(Buffer)+2)^-fBits
    else
      if ((fSize-fBits) > Size) then xSize := Size
                                else xSize := fSize-fBits;
    if (Comp in [BI_RGB, BI_BITFIELDS])then
      ReadFile(hFile,Bits^,xSize,i,nil)
    else begin
      ReAllocMem(Buffer,xSize);
      ReadFile(hFile,Buffer^,xSize,i,nil);
      if (Comp = BI_RLE8) then DecodeRLE8(Self,Buffer)
                          else DecodeRLE4(Self,Buffer);
    end;
    Result := True;
  end;
  CloseHandle(hFile);
  FreeMem(Buffer);
end;

function TFastDIB.LoadFromRes(hInst:HINST;ResID,ResType:PChar): Boolean;
var
  pMem: Pointer;
  bmInfo: TBMInfo;
  fSize, fBits: DWord;
begin
  Result := False;
  pMem:=LockResource(LoadResource(hInst,FindResource(hInst,ResID,ResType)));
  if pMem<>nil then
  begin
    fBits := LoadHeader(pMem, bmInfo);
    If fBits > 0 then begin
      fSize:=PDWord(pMem)^ - fBits;
      SetSizeIndirect(bmInfo);
      if Size < fSize then fSize:=Size;
      if bmInfo.Header.Compression=1 then DecodeRLE8(Self,Pointer(DWord(pMem)+fBits))
      else if bmInfo.Header.Compression=2 then DecodeRLE4(Self,Pointer(DWord(pMem)+fBits))
      else Move(PByte(DWord(pMem)+fBits)^,Bits^,fSize);
      Result := True;
    end;
  end;
end;

procedure TFastDIB.LoadFromClipboard;
var
  hMem,i: Integer;
  pMem: PBMInfo;
begin
  if OpenClipboard(0) then
  begin
    hMem:=GetClipboardData(CF_DIB);
    if hMem<>0 then
    begin
      pMem:=GlobalLock(hMem);
      SetSizeIndirect(pMem^);
      if pMem.Header.BitCount<=8 then i:=40+((1 shl pMem.Header.BitCount)shl 2)else
      if(((pMem.Header.BitCount=16)or(pMem.Header.BitCount=32))and(pMem.Header.Compression=3))then i:=52 else i:=40;
      if Bpp<=8 then Move(pMem.Colors,Colors^,(1 shl Bpp)shl 2);
      if pMem.Header.Compression=1 then DecodeRLE8(Self,Pointer(NativeInt(pMem)+i))
      else if pMem.Header.Compression=2 then DecodeRLE4(Self,Pointer(NativeInt(pMem)+i))
      else Move(PByte(NativeInt(pMem)+i)^,Bits^,pMem.Header.SizeImage);
      GlobalUnlock(hMem);
    end;
    CloseClipboard;
  end;
end;

procedure TFastDIB.AttachToHandle(hDC:HDC;hBmp:HBITMAP);
var
  dsInfo: TDIBSection;
begin
  if GetObject(hBmp,SizeOf(dsInfo),@dsInfo) = SizeOf(TDIBSection) then
    if not (dsInfo.dsBmih.biCompression in [BI_RLE8, BI_RLE4]) then
    begin
      SetInterfaceIndirect(dsInfo.dsBm.bmBits, PBMInfo(@dsInfo.dsBmih)^, False);
      if Bpp<=8 then
      begin
        // s: can't get palette in x64. Don't know why...
        GetDIBits(hDC,hBmp,0,0,nil,PBitmapInfo(@Info)^,DIB_RGB_COLORS);
        UpdateColors;
      end;
      Handle:=hBmp;
      Self.hDC:=0;  // bitmaps can be selected for only one device context at a time
      FreeDC:=False;
      FreeBits:=False;
      FreeHandle:=False;
    end;
end;

procedure TFastDIB.UpdateColors;
begin
  SetDIBColorTable(hDC,0,1 shl Bpp,Colors^);
end;

procedure TFastDIB.Draw(fDC:HDC;x,y:Integer);
begin
  if hDC=0 then
    StretchDIBits(fDC,x,y,Width,AbsHeight,0,0,
      Width,AbsHeight,Bits,PBitmapInfo(@Info)^,0,SRCCOPY)
  else BitBlt(fDC,x,y,Width,AbsHeight,hDC,0,0,SRCCOPY);
end;

procedure TFastDIB.Stretch(fDC:HDC;x,y,w,h:Integer);
begin
  if hDC=0 then
    StretchDIBits(fDC,x,y,w,h,0,0,
      Width,AbsHeight,Bits,PBitmapInfo(@Info)^,0,SRCCOPY)
  else StretchBlt(fDC,x,y,w,h,hDC,0,0,Width,AbsHeight,SRCCOPY);
end;

procedure TFastDIB.DrawRect(fDC:HDC;x,y,w,h,sx,sy:Integer);
begin
  if hDC=0 then
    StretchDIBits(fDC,x,y,w,h,sx,sy,w,h,Bits,PBitmapInfo(@Info)^,0,SRCCOPY)
  else BitBlt(fDC,x,y,w,h,hDC,sx,sy,SRCCOPY);
end;

procedure TFastDIB.DrawRect(fDC:HDC; Const SrcRect : TRect; dx, dy : Integer);
begin
DrawRect(fDC, dx, dy, SrcRect.Right - SrcRect.Left,
         SrcRect.Bottom - SrcRect.Top, SrcRect.Left, SrcRect.Top);
end;


procedure TFastDIB.StretchRect(fDC:HDC;x,y,w,h,sx,sy,sw,sh:Integer);
begin
  if hDC=0 then
    StretchDIBits(fDC,x,y,w,h,sx,sy,sw,sh,Bits,PBitmapInfo(@Info)^,0,SRCCOPY)
  else StretchBlt(fDC,x,y,w,h,hDC,sx,sy,sw,sh,SRCCOPY);
end;

procedure TFastDIB.PlgDraw(fDC:HDC;x1,y1,x2,y2,x3,y3:Integer);
var
  Pts: array[0..2]of TPoint;
begin
  Pts[0].x:=x1; Pts[0].y:=y1;
  Pts[1].x:=x2; Pts[1].y:=y2;
  Pts[2].x:=x3; Pts[2].y:=y3;
  PlgBlt(fDC,Pts,hDC,0,0,Width,AbsHeight,0,0,0);
end;

procedure TFastDIB.MaskDraw(fDC:HDC;x,y:Integer;Mono:TFastDIB);
begin
  MaskBlt(fDC,x,y,Width,AbsHeight,hDC,0,0,Mono.Handle,0,0,$CCAA0029);
end;

procedure TFastDIB.MaskRect(fDC:HDC;x,y,w,h,sx,sy,mx,my:Integer;Mono:TFastDIB);
begin
  MaskBlt(fDC,x,y,Width,AbsHeight,hDC,sx,sy,Mono.Handle,mx,my,$CCAA0029);
end;

procedure TFastDIB.TransDraw(fDC:HDC;x,y:Integer;c:TFColor);
begin
  TransparentBlt(fDC,x,y,Width,AbsHeight,hDC,0,0,Width,AbsHeight,ColorToInt(c));
end;

procedure TFastDIB.TransStretch(fDC:HDC;x,y,w,h:Integer;c:TFColor);
begin
  TransparentBlt(fDC,x,y,w,h,hDC,0,0,Width,AbsHeight,ColorToInt(c));
end;

procedure TFastDIB.AlphaDraw(fDC:HDC;x,y:Integer;a:Byte;hasAlpha:Boolean);
var
  Blend: TBlendFunction;
begin
  Blend.BlendOp:=0;
  Blend.BlendFlags:=0;
  Blend.SourceConstantAlpha:=a;
  Blend.AlphaFormat:=Integer(hasAlpha);
  AlphaBlend(fDC,x,y,Width,AbsHeight,hDC,0,0,Width,AbsHeight,Blend);
end;

procedure TFastDIB.AlphaStretch(fDC:HDC;x,y,w,h:Integer;a:Byte;hasAlpha:Boolean);
var
  Blend: TBlendFunction;
begin
  Blend.BlendOp:=0;
  Blend.BlendFlags:=0;
  Blend.SourceConstantAlpha:=a;
  Blend.AlphaFormat:=Integer(hasAlpha);
  AlphaBlend(fDC,x,y,w,h,hDC,0,0,Width,AbsHeight,Blend);
end;

procedure TFastDIB.TileDraw(fDC:HDC;x,y,w,h:Integer);
var
  hBmp: HBITMAP;
  wd,hd: Integer;
  memDC: Windows.HDC;
begin
  memDC:=CreateCompatibleDC(fDC);
  hBmp:=CreateCompatibleBitmap(fDC,w,h);
  SelectObject(memDC,hBmp);
  Draw(memDC,0,0);
  wd:=Width;
  hd:=AbsHeight;
  while wd<w do
  begin
    BitBlt(memDC,wd,0,wd shl 1,h,memDC,0,0,SRCCOPY);
    Inc(wd,wd);
  end;
  while hd<h do
  begin
    BitBlt(memDC,0,hd,w,hd shl 1,memDC,0,0,SRCCOPY);
    Inc(hd,hd);
  end;
  BitBlt(fDC,x,y,w,h,memDC,0,0,SRCCOPY);
  DeleteDC(memDC);
  DeleteObject(hBmp);
end;

procedure TFastDIB.SetPen(Style,Width,Color:DWord);
begin
  hPen:=CreatePen(Style,Width,Color);
  DeleteObject(SelectObject(hDC,hPen));
end;

procedure TFastDIB.SetBrush(Style,Hatch,Color:DWord);
var
  Brush: TLOGBRUSH;
begin
  Brush.lbStyle:=Style;
  Brush.lbHatch:=Hatch;
  Brush.lbColor:=Color;
  hBrush:=CreateBrushIndirect(Brush);
  DeleteObject(SelectObject(hDC,hBrush));
end;

procedure TFastDIB.SetFont(Font:string;Size:Integer);
begin
  hFont:=CreateFont(Size,0,0,0,0,0,0,0,DEFAULT_CHARSET,0,0,0,0,PChar(Font));
  DeleteObject(SelectObject(hDC,hFont));
end;

procedure TFastDIB.SetFontEx(Font:string;Width,Height,Weight:Integer;Italic,Underline,Strike:Boolean);
var
  LogFont: TLOGFONT;
begin
  FillChar(LogFont,SizeOf(LogFont),0);
  with LogFont do
  begin
    lfHeight:=Height;
    lfWidth:=Width;
    lfWeight:=Weight;
    lfItalic:=Byte(Italic);
    lfUnderline:=Byte(Underline);
    lfStrikeOut:=Byte(Strike);
    lfCharSet:=DEFAULT_CHARSET;
    Move(Font[1],lfFaceName,Length(Font));
  end;
  hFont:=CreateFontIndirect(LogFont);
  DeleteObject(SelectObject(hDC,hFont));
end;

procedure TFastDIB.SetTextColor(Color:DWord);
begin
  Windows.SetTextColor(hDC,Color);
end;

procedure TFastDIB.SetTransparent(Transparent:Boolean);
begin
  if Transparent then SetBkMode(hDC,1) else SetBkMode(hDC,2);
end;

procedure TFastDIB.SetBkColor(Color:DWord);
begin
  Windows.SetBkColor(hDC,Color);
end;

procedure TFastDIB.Ellipse(x1,y1,x2,y2:Integer);
begin
  Windows.Ellipse(hDC,x1,y1,x2,y2);
end;

procedure TFastDIB.FillRect(Rect:TRect);
begin
  Windows.FillRect(hDC,Rect,hBrush);
end;

procedure TFastDIB.LineTo(x,y:Integer);
begin
  Windows.LineTo(hDC,x,y);
end;

procedure TFastDIB.MoveTo(x,y:Integer);
begin
  MoveToEx(hDC,x,y,nil);
end;

procedure TFastDIB.Polygon(Points:array of TPoint);
begin
  Windows.Polygon(hDC,Points,High(Points)+1);
end;

procedure TFastDIB.Polyline(Points:array of TPoint);
begin
  Windows.Polyline(hDC,Points,High(Points)+1);
end;

procedure TFastDIB.Rectangle(x1,y1,x2,y2:Integer);
begin
  Windows.Rectangle(hDC,x1,y1,x2,y2);
end;

procedure TFastDIB.TextOut(x,y:Integer;Text:string);
begin
  Windows.TextOut(hDC,x,y,PChar(Text),Length(Text));
end;

procedure TFastDIB.DrawText(Text:string;Rect:TRect;Flags:Integer);
begin
  Windows.DrawText(hDC,PChar(Text),Length(Text),Rect,Flags);
end;

procedure TFastDIB.Clear(c:TFColor);
begin
  FastDIB.Clear(Self,c);
end;

procedure TFastDIB.ClearB(c:DWord);
begin
  FastDIB.ClearB(Self,c);
end;

procedure TFastDIB.SaveToClipboard;
var
  pMem: Pointer;
  i,hMem: DWord;
begin
  if Bpp<=8 then i:=40+((1 shl Bpp)shl 2)else
  if(((Bpp=16)or(Bpp=32))and(Compression=3))then i:=52 else i:=40;
  hMem:=GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE,Size+i);
  pMem:=GlobalLock(hMem);
  Move(Info,pMem^,i);
  Move(Bits^,PByte(DWord(pMem)+i)^,Size);
  GlobalUnlock(hMem);
  OpenClipboard(0);
  SetClipboardData(CF_DIB,hMem);
  CloseClipboard;
end;

procedure TFastDIB.SaveToFile(FileName:string);
var
  cSize,i: DWord;
  hFile: Windows.HFILE;
  fHead: TBitmapFileHeader;
begin
  hFile:=CreateFile(PChar(FileName),GENERIC_WRITE,0,nil,CREATE_ALWAYS,0,0);
  if Info.Header.ClrUsed<>0 then cSize:=(Info.Header.ClrUsed shl 2)
  else if Info.Header.Compression=BI_BITFIELDS then cSize:=12
  else if Bpp<=8 then cSize:=(1 shl Bpp)shl 2
  else cSize:=0;
  fHead.bfType:=$4D42;
  fHead.bfSize:=54+Size+cSize;
  fHead.bfOffBits:=54+cSize;
  WriteFile(hFile,fHead,SizeOf(fHead),i,nil);
  WriteFile(hFile,Info,cSize+40,i,nil);
  WriteFile(hFile,Bits^,Size,i,nil);
  CloseHandle(hFile);
end;

procedure TFastDIB.CopyRect(Src:TFastDIB;x,y,w,h,sx,sy:Integer);
var
  iy,pc,sc,b: NativeInt;
begin
  if Height>0 then y:=AbsHeight-h-y;
  if Src.Height>0 then sy:=Src.Height-h-sy;

  if x<0 then
  begin
    Dec(sx,x);
    Inc(w,x);
    x:=0;
  end;

  if y<0 then
  begin
    Dec(sy,y);
    Inc(h,y);
    y:=0;
  end;

  if sx<0 then
  begin
    Dec(x,sx);
    Inc(w,sx);
    sx:=0;
  end;

  if sy<0 then
  begin
    Dec(y,sy);
    Inc(h,sy);
    sy:=0;
  end;

  if(sx<Src.Width)and(sy<Src.AbsHeight)and(x<Width)and(y<AbsHeight)then
  begin

    if w+sx>=Src.Width then w:=Src.Width-sx;
    if h+sy>=Src.AbsHeight then h:=Src.AbsHeight-sy;
    if w+x>=Width then w:=Width-x;
    if h+y>=AbsHeight then h:=AbsHeight-y;

    if(Bpp>=8)and(Bpp=Src.Bpp)then
    begin

      b:=w;
      case Bpp of
        16:
        begin
          b:=w shl 1;
          x:=x shl 1;
          sx:=sx shl 1;
        end;
        24:
        begin
          b:=w*3;
          x:=x*3;
          sx:=sx*3;
        end;
        32:
        begin
          b:=w shl 2;
          x:=x shl 2;
          sx:=sx shl 2;
        end;
      end;

      pc:=NativeInt(Scanlines[y])+x;
      sc:=NativeInt(Src.Scanlines[sy])+sx;

      for iy:=0 to h-1 do
      begin
        Move(PByte(sc)^,PByte(pc)^,b);
        Inc(pc,BWidth);
        Inc(sc,Src.BWidth);
      end;

    end
    else
    begin
      for iy:=0 to h-1 do
      for b:=0 to w-1 do
        Pixels[y+iy,x+b]:=Src.Pixels[sy+iy,sx+b];
    end;

  end;
end;

procedure TFastDIB.FillColors(i1,i2:Integer;Keys:array of TFColor);
begin
  FastDIB.FillColors(Colors,i1,i2,High(Keys)+1,PLine24(@Keys));
  if hDC<>0 then UpdateColors;
end;

procedure TFastDIB.ShiftColors(i1,i2,Amount:Integer);
var
  p: PFColorTable;
  i: Integer;
begin
  i:=i2-i1;
  if(Amount<i)and(Amount>0)then
  begin
    GetMem(p,i shl 2);
    Move(Colors[i1],p[0],i shl 2);
    Move(p[0],Colors[i1+Amount],(i-Amount)shl 2);
    Move(p[i-Amount],Colors[i1],Amount shl 2);
    FreeMem(p);
  end;
  if hDC<>0 then UpdateColors;
end;


function TFastDIB.GenLut16(Lut8 : PLine8 = nil): PLut16;
Var n, i, Offset : Integer;
    pc : PFColor;
    c : TFColor;

  function SetByte(Src : Integer): Byte;
  begin
  If Src > 255 then Result := 255 else
    If Src < 0 then Result := 0 else Result := Src;
  end;

begin
//If (Lut16 <> nil) then begin FreeMem(Lut16); Lut16 := nil; end;
If (Lut16 = nil) then begin
  GetMem(Lut16, SizeOf(TLut16));
  pc := @Lut16[0];

  If UseLut16Offsets then
    If (Bpg = 6) then Offset := 3
                 else Offset := 6
  else
    Offset := 0;
    // more correct brightness

  For n := 0 to 65535 do begin

    c.b := n shl BShr;
    c.g := n shr GShl shl GShr;
    c.r := n shr RShl shl RShr;

    c.b := SetByte(c.b+Offset);
    c.g := SetByte(c.g+Offset);
    c.r := SetByte(c.r+Offset);

{
    c.b := SetByte(n shl BShr);
    c.g := SetByte(n shr GShl shl GShr);
    c.r := SetByte(n shr RShl shl RShr);
}
    If (Lut8 <> nil) then begin
      pc.b := Lut8[c.b]; pc.g := Lut8[c.g]; pc.r := Lut8[c.r];
    end else
      pc^ := c;
    Inc(pc);
  end;
end;
Result := Lut16;
end;

function TFastDIB.GenBackLut16(Lut8 : PLine8 = nil): PBackLut16;
Const
  Offset : Integer = -1; // more correct brightness
Var n, i : Integer;
begin
//If (BackLut16 <> nil) then begin FreeMem(BackLut16); BackLut16 := nil; end;
If (BackLut16 = nil) then begin
  GetMem(BackLut16, SizeOf(TBackLut16));
  For n:=0 to 255 do
    With BackLut16^ do begin
      If UseLut16Offsets then begin
        i := n + Offset; Num_InRange(i, 0, 255);
      end else
        i := n;
      If (Lut8 <> nil) then i := Lut8[i];
      bi[n] := i shr BShr;
      gi[n] := i shr GShr shl GShl;
      ri[n] := i shr RShr shl RShl;
    end;
end;
Result := BackLut16;
end;


////////////////////////////////////////////////////////////////////////////////

procedure SetAlphaChannel(Bmp,Alpha:TFastDIB);
var
  pb: PByte;
  pc: PFColorA;
  x,y: Integer;
begin
  pb:=Pointer(Alpha.Bits);
  pc:=Pointer(Bmp.Bits);
  for y:=0 to Alpha.AbsHeight-1 do
  begin
    for x:=0 to Alpha.Width-1 do
    begin
      pc^.a:=pb^;
      Inc(pc);
      Inc(pb);
    end;
    pc:=PFColorA(NativeInt(pc)+Bmp.Gap);
    Inc(pb,Alpha.Gap);
  end;
end;

procedure MultiplyAlpha(Bmp:TFastDIB);
var
  pc: PFColorA;
  x,y,i: Integer;
begin
  pc:=Pointer(Bmp.Bits);
  for y:=0 to Bmp.AbsHeight-1 do
  begin
    for x:=0 to Bmp.Width-1 do
    begin
      i:=pc.a;
      if i=0 then
      begin
        pc.b:=0;
        pc.g:=0;
        pc.r:=0;
      end
      else if i<255 then
      begin
        pc.b:=(pc.b*i)shr 8;
        pc.g:=(pc.g*i)shr 8;
        pc.r:=(pc.r*i)shr 8;
      end;
      Inc(pc);
    end;
    pc:=PFColorA(NativeInt(pc)+Bmp.Gap);
  end;
end;

procedure SwapChannels24(Bmp:TFastDIB);
var
  pc: PFColor;
  x,y,z: Integer;
begin
  pc:=Pointer(Bmp.Bits);
  for y:=0 to Bmp.AbsHeight-1 do
  begin
    for x:=0 to Bmp.Width-1 do
    begin
      z:=pc.r;
      pc.r:=pc.b;
      pc.b:=z;
      Inc(pc);
    end;
    pc:=PFColor(NativeInt(pc)+Bmp.Gap);
  end;
end;

procedure SwapChannels32(Bmp:TFastDIB);
var
  pc: PFColorA;
  x,y,z: Integer;
begin
  pc:=Pointer(Bmp.Bits);
  for y:=0 to Bmp.AbsHeight-1 do
  begin
    for x:=0 to Bmp.Width-1 do
    begin
      z:=pc.r;
      pc.r:=pc.b;
      pc.b:=z;
      Inc(pc);
    end;
    pc:=PFColorA(NativeInt(pc)+Bmp.Gap);
  end;
end;

procedure SwapChannels(Bmp:TFastDIB);
begin
  case Bmp.Bpp of
    24: SwapChannels24(Bmp);
    32: SwapChannels32(Bmp);
  end;
end;

procedure FillMem(Mem:Pointer;Size,Value:Integer);
{$IFDEF CPUX86}
asm
  push edi
  push ebx

  mov ebx,edx
  mov edi,eax
  mov eax,ecx
  mov ecx,edx
  shr ecx,2
  jz  @word
  rep stosd

  @word:
  mov ecx,ebx
  and ecx,2
  jz  @byte
  mov [edi],ax
  add edi,2

  @byte:
  mov ecx,ebx
  and ecx,1
  jz  @exit
  mov [edi],al

  @exit:
  pop ebx
  pop edi
end;
{$ELSE}
Var i : Int64;
begin

i := DWord(Value); i := i or (i shl 32);
While (Size >= 8) do begin
  PInt64(Mem)^ := i; Inc(PByte(Mem), 8); Dec(Size, 8);
end;

While (Size >= 4) do begin
  PInteger(Mem)^ := Value; Inc(PByte(Mem), 4); Dec(Size, 4);
end;

While (Size > 0) do begin
  PByte(Mem)^ := Value; Inc(PByte(Mem)); Dec(Size);
end;
end;
{$ENDIF}

procedure Clear(Bmp:TFastDIB;c:TFColor);
begin
  case Bmp.Bpp of
    1,4,8: ClearB(Bmp,ClosestColor(Bmp.Colors,(1 shl Bmp.Bpp)-1,c));
    16: ClearB(Bmp,c.r shr Bmp.RShr shl Bmp.RShl or
          c.g shr Bmp.GShr shl Bmp.GShl or
          c.b shr Bmp.BShr);
    24: ClearB(Bmp,PDWord(@c)^);
    32: if Bmp.Compression=0 then ClearB(Bmp,PDWord(@c)^) else
        ClearB(Bmp,c.r shr Bmp.RShr shl Bmp.RShl or
          c.g shr Bmp.GShr shl Bmp.GShl or
          c.b shr Bmp.BShr);
  end;
end;

procedure ClearB(Bmp:TFastDIB;c:DWord);
var
  i: Integer;
  pc: PFColor;
begin
  if(Bmp.Bpp=1)and(c=1)then c:=15;
  if Bmp.Bpp<=4 then c:=c or c shl 4;
  if Bmp.Bpp<=8 then
  begin
    c:=c or c shl 8;
    c:=c or c shl 16;
  end
  else if Bmp.Bpp=16 then c:=c or c shl 16;
  if Bmp.Bpp=24 then
  begin
    for i:=0 to Bmp.AbsHeight-1 do
      FillMem24(Bmp.Scanlines[i], Bmp.Width, PFColor(@c)^);
{
    pc:=Pointer(Bmp.Bits);
    for i:=0 to Bmp.Width-1 do
    begin
      pc^:=PFColor(@c)^;
      Inc(pc);
    end;
    for i:=1 to Bmp.AbsHeight-1 do
      Move(Bmp.Bits^,Bmp.Scanlines[i]^,Bmp.BWidth-Bmp.Gap);
}
  end
  else
  begin
    if Bmp.Size<>0 then FillMem(Bmp.Bits,Bmp.Size,c) else
      for i:=0 to Bmp.AbsHeight-1 do
        FillMem(Bmp.Scanlines[i],Bmp.BWidth-Bmp.Gap,c);
  end;
end;

procedure DecodeRLE4(Bmp:TFastDIB;Data:Pointer);
  procedure OddMove(Src,Dst:PByte;Size:Integer);
  begin
    if Size=0 then Exit;
    repeat
      Dst^:=(Dst^ and $F0)or(Src^ shr 4);
      Inc(Dst);
      Dst^:=(Dst^ and $0F)or(Src^ shl 4);
      Inc(Src);
      Dec(Size);
    until Size=0;
  end;
  procedure OddFill(Mem:PByte;Size,Value:Integer);
  begin
    Value:=(Value shr 4)or(Value shl 4);
    Mem^:=(Mem^ and $F0)or(Value and $0F);
    Inc(Mem);
    if Size>1 then FillChar(Mem^,Size,Value);
    Mem^:=(Mem^ and $0F)or(Value and $F0);
  end;
var
  pb: PByte;
  x,y,z,i: Integer;
begin
  pb:=Data; x:=0; y:=0;
  while y<Bmp.AbsHeight do
  begin
    if pb^=0 then
    begin
      Inc(pb);
      z:=pb^;
      case pb^ of
        0: begin
             Inc(y);
             x:=0;
           end;
        1: Break;
        2: begin
             Inc(pb); Inc(x,pb^);
             Inc(pb); Inc(y,pb^);
           end;
        else
        begin
          Inc(pb);
          i:=(z+1)shr 1;
          if(z and 2)=2 then Inc(i);
          if((x and 1)=1)and(x+i<Bmp.Width)then
            OddMove(pb,@Bmp.Pixels8[y,x shr 1],i)
          else
            Move(pb^,Bmp.Pixels8[y,x shr 1],i);
          Inc(pb,i-1);
          Inc(x,z);
        end;
      end;
    end
    else
    begin
      z:=pb^;
      Inc(pb);
      if((x and 1)=1)and(x+z<Bmp.Width)then
        OddFill(@Bmp.Pixels8[y,x shr 1],z shr 1,pb^)
      else
        FillChar(Bmp.Pixels8[y,x shr 1],z shr 1,pb^);
      Inc(x,z);
    end;
    Inc(pb);
  end;
end;

procedure DecodeRLE8(Bmp:TFastDIB;Data:Pointer);
var
  pb: PByte;
  x,y,z,i,s: Integer;
begin
  pb:=Data; y:=0; x:=0;
  while y<Bmp.AbsHeight do
  begin
    if pb^=0 then
    begin
      Inc(pb);
      case pb^ of
        0: begin
             Inc(y);
             x:=0;
           end;
        1: Break;
        2: begin
             Inc(pb); Inc(x,pb^);
             Inc(pb); Inc(y,pb^);
           end;
        else
        begin
          i:=pb^;
          s:=(i+1)and(not 1);
          z:=s-1;
          Inc(pb);
          if x+s>Bmp.Width then s:=Bmp.Width-x;
          Move(pb^,Bmp.Pixels8[y,x],s);
          Inc(pb,z);
          Inc(x,i);
        end;
      end;
    end
    else
    begin
      i:=pb^; Inc(pb);
      if i+x>Bmp.Width then i:=Bmp.Width-x;
      FillChar(Bmp.Pixels8[y,x],i,pb^);
      Inc(x,i);
    end;
    Inc(pb);
  end;
end;

procedure FillColors(Pal:PFColorTable;i1,i2,nKeys:Integer;Keys:PLine24);
var
  pc: PFColorA;
  c1,c2: TFColor;
  i,n,cs,w1,w2,x,ii: Integer;
begin
  i:=0;
  n:=i2-i1;
  Dec(nKeys);
  ii:=(nKeys shl 16)div n;
  pc:=@Pal[i1];
  for x:=0 to n-1 do
  begin
    cs:=i shr 16;
    c1:=Keys[cs];
    if cs<nKeys then Inc(cs);
    c2:=Keys[cs];
    w1:=((not i)and $FFFF)+1;
    w2:=i and $FFFF;
    if(w1<(ii-w1))then pc.c:=c2 else
    if(w2<(ii-w2))then pc.c:=c1 else
    begin
      pc.b:=((c1.b*w1)+(c2.b*w2))shr 16;
      pc.g:=((c1.g*w1)+(c2.g*w2))shr 16;
      pc.r:=((c1.r*w1)+(c2.r*w2))shr 16;
    end;
    Inc(i,ii);
    Inc(pc);
  end;
  pc.c:=c2;
end;

procedure FillGray(Pal : PFColorTable; Bpp : Integer);
Var n, i, c : Integer;
begin
If Bpp = 1 then begin
  Pal[0].c := tfBlack; Pal[1].c := tfWhite;
end else begin
  If (Bpp = 4) then i := 17 //not 16, to fill range 0..255
               else i := 1;
  i := i or (i shl 8) or (i shl 16); c := 0;
  For n := 0 to (1 shl Bpp)-1 do begin Pal[n].i := c; Inc(c, i); end;
end;
end;

function ClosestColor(Pal:PFColorTable;Max:Integer;c:TFColor):Byte;
var
  n: Byte;
  pc: PFColorA;
  i,x,d: Integer;
begin
  x:=765; n:=0;
  pc:=Pointer(Pal);
  for i:=0 to Max do
  begin
    if pc.b>c.b then d:=pc.b-c.b else d:=c.b-pc.b;
    if pc.g>c.g then Inc(d,pc.g-c.g) else Inc(d,c.g-pc.g);
    if pc.r>c.r then Inc(d,pc.r-c.r) else Inc(d,c.r-pc.r);
    if d<x then
    begin
      x:=d;
      n:=i;
    end;
    Inc(pc);
  end;
  Result:=n;
end;


function LoadHeader(Data:Pointer;var bmInfo:TBMInfo):Integer;
var
  i: Integer;
begin
//CheckImageType(Data);
  Result := PBitmapFileHeader(Data).bfOffBits;
  if PDWord(NativeInt(Data)+14)^=40 then
    Move(PByte(NativeInt(Data)+14)^, bmInfo, SizeOf(bmInfo))
  else
    if PDWord(NativeInt(Data)+14)^=12 then
      With PBitmapCoreInfo(NativeInt(Data)+14)^ do begin
        FillChar(bmInfo,SizeOf(bmInfo),0);
        bmInfo.Header.Width:=bmciHeader.bcWidth;
        bmInfo.Header.Height:=bmciHeader.bcHeight;
        bmInfo.Header.BitCount:=bmciHeader.bcBitCount;
        if bmciHeader.bcBitCount<=8 then
          for i:=0 to (1 shl bmciHeader.bcBitCount)-1 do
            bmInfo.Colors[i]:=PFColorA(@bmciColors[i])^;
      end
    else
      Result := 0;
end;

function PackedDIB(Bmp:TFastDIB):Pointer;
var
  i: DWord;
begin
  if Bmp.Bpp<=8 then i:=40+((1 shl Bmp.Bpp)shl 2)else
  if(((Bmp.Bpp=16)or(Bmp.Bpp=32))and(Bmp.Compression=3))then i:=52 else i:=40;
  GetMem(Result,Bmp.Size+i);
  Move(Bmp.Info,Result^,i);
  Move(Bmp.Bits^,PByte(DWord(Result)+i)^,Bmp.Size);
end;

function Count1(Bmp:TFastDIB):Integer;
var
  pb: PByte;
  w,c,x,y: Integer;
begin
  Result:=2;
  pb:=Pointer(Bmp.Bits); c:=pb^;
  if(c<>0)and(c<>255)then Exit;
  w:=(Bmp.Width div 8)-1;
  for y:=0 to Bmp.AbsHeight-1 do
  begin
    for x:=0 to w do
    begin
      if pb^<>c then Exit;
      Inc(pb);
    end;
    Inc(pb,Bmp.Gap);
  end;
  Result:=1;
end;

function Count4(Bmp:TFastDIB):Integer;
var
  pb,pc: PByte;
  x,y,w: Integer;
  Check: array[0..15]of Byte;
begin
  Result:=0;
  FillChar(Check,SizeOf(Check),0);
  pb:=Pointer(Bmp.Bits);
  w:=(Bmp.Width div 2)-1;
  for y:=0 to Bmp.AbsHeight-1 do
  begin
    for x:=0 to w do
    begin
      pc:=@Check[pb^ shr 4];
      if pc^=0 then
      begin
        Inc(Result);
        pc^:=1;
      end;
      pc:=@Check[pb^ and 15];
      if pc^=0 then
      begin
        Inc(Result);
        pc^:=1;
      end;
      if Result=16 then Exit;
      Inc(pb);
    end;
    Inc(pb,Bmp.Gap);
  end;
end;

function Count8(Bmp:TFastDIB):Integer;
var
  x,y: Integer;
  pb,pc: PByte;
  Check: array[Byte]of Byte;
begin
  Result:=0;
  FillChar(Check,SizeOf(Check),0);
  pb:=Pointer(Bmp.Bits);
  for y:=0 to Bmp.AbsHeight-1 do
  begin
    for x:=0 to Bmp.Width-1 do
    begin
      pc:=@Check[pb^];
      if pc^=0 then
      begin
        Inc(Result);
        pc^:=1;
      end;
      if Result=256 then Exit;
      Inc(pb);
    end;
    Inc(pb,Bmp.Gap);
  end;
end;

function Count16(Bmp:TFastDIB):Integer;
var
  pw: PWord;
  pc: PByte;
  x,y: Integer;
  Check: array[Word]of Byte;
begin
  Result:=0;
  FillChar(Check,SizeOf(Check),0);
  pw:=Pointer(Bmp.Bits);
  for y:=0 to Bmp.AbsHeight-1 do
  begin
    for x:=0 to Bmp.Width-1 do
    begin
      pc:=@Check[pw^];
      if pc^=0 then
      begin
        Inc(Result);
        pc^:=1;
      end;
      Inc(pw);
    end;
    pw:=PWord(NativeInt(pw)+Bmp.Gap);
  end;
end;

function Count24(Bmp:TFastDIB):Integer;
type
  PCheck =^TCheck;
  TCheck = array[Byte,Byte,0..31]of Byte;
var
  pb: PByte;
  pc: PFColor;
  Check: PCheck;
  x,y,c: Integer;
begin
  Result:=0;
  New(Check);
  FillChar(Check^,SizeOf(TCheck),0);
  pc:=Pointer(Bmp.Bits);
  for y:=0 to Bmp.AbsHeight-1 do
  begin
    for x:=0 to Bmp.Width-1 do
    begin
      pb:=@Check[pc.r,pc.g,pc.b shr 3];
      c:=1 shl(pc.b and 7);
      if(c and pb^)=0 then
      begin
        Inc(Result);
        pb^:=pb^ or c;
      end;
      Inc(pc);
    end;
    pc:=PFColor(NativeInt(pc)+Bmp.Gap);
  end;
  Dispose(Check);
end;

function Count32(Bmp:TFastDIB):Integer;
type
  PCheck =^TCheck;
  TCheck = array[Byte,Byte,0..31]of Byte;
var
  pb: PByte;
  pc: PFColorA;
  i,c: Integer;
  Check: PCheck;
begin
  Result:=0;
  New(Check);
  FillChar(Check^,SizeOf(TCheck),0);
  pc:=Pointer(Bmp.Bits);
  for i:=0 to(Bmp.Size shr 2)-1 do
  begin
    pb:=@Check[pc.r,pc.g,pc.b shr 3];
    c:=1 shl(pc.b and 7);
    if(c and pb^)=0 then
    begin
      Inc(Result);
      pb^:=pb^ or c;
    end;
    Inc(pc);
  end;
  Dispose(Check);
end;

function CountColors(Bmp:TFastDIB):DWord;
begin
  case Bmp.Bpp of
    1:  Result:=Count1(Bmp);
    4:  Result:=Count4(Bmp);
    8:  Result:=Count8(Bmp);
    16: Result:=Count16(Bmp);
    24: Result:=Count24(Bmp);
    32: Result:=Count32(Bmp);
    else Result:=0;
  end;
end;

procedure IntToMask(Bpr,Bpg,Bpb:DWord;var RMsk,GMsk,BMsk:DWord);
begin
  BMsk:=(1 shl Bpb)-1;
  GMsk:=((1 shl(Bpb+Bpg))-1)and not BMsk;
  if(Bpr+Bpg+Bpb)=32 then RMsk:=$FFFFFFFF else RMsk:=(1 shl(Bpr+Bpb+Bpg))-1;
  RMsk:=RMsk and not(BMsk or GMsk);
end;

procedure MaskToInt(RMsk,GMsk,BMsk:DWord;var Bpr,Bpg,Bpb:DWord);
  function CountBits(i:DWord):DWord;
{$IFDEF CPUX86}
  asm
    bsr edx,eax
    bsf ecx,eax
    sub edx,ecx
    inc edx
    mov eax,edx
  end;
{$ELSE}
  Var n,m : DWord;
  begin
  Result:=0; m:=1;
  For n:=0 to 31 do begin
    If (i and m)=m then Inc(Result); m:=m shl 1;
  end;
  end;
{$ENDIF}
begin
  Bpb:=CountBits(BMsk);
  Bpg:=CountBits(GMsk);
  Bpr:=CountBits(RMsk);
end;

function UnpackColorTable(Table:TFPackedColorTable):TFColorTable;
var
  i: Integer;
begin
  for i:=0 to 255 do
    Result[i].c:=Table[i];
end;

function PackColorTable(Table:TFColorTable):TFPackedColorTable;
var
  i: Integer;
begin
  for i:=0 to 255 do
    Result[i]:=Table[i].c;
end;

function FRGB(r,g,b:Byte):TFColor;
begin
  Result.b:=b;
  Result.g:=g;
  Result.r:=r;
end;

function FRGBA(r,g,b,a:Byte):TFColorA;
begin
  Result.b:=b;
  Result.g:=g;
  Result.r:=r;
  Result.a:=a;
end;

function ColorToInt(c:TFColor):DWord;
begin
  Result:=c.b shl 16 or c.g shl 8 or c.r;
end;

function ColorToIntA(c:TFColorA):DWord;
begin
  Result:=c.b shl 24 or c.g shl 16 or c.r shl 8 or c.a;
end;

function IntToColor(i:DWord):TFColor;
begin
  Result.b:=i shr 16;
  Result.g:=i shr 8;
  Result.r:=i;
end;

function IntToColorA(i:DWord):TFColorA;
begin
  Result.a:=i shr 24;
  Result.b:=i shr 16;
  Result.g:=i shr 8;
  Result.r:=i;
end;

function Scale8(i,n:Integer):Integer;
begin // Result:=(i*255)div([1 shl n]-1);
  case n of
    1: if Boolean(i) then Result:=255 else Result:=0;
    2: Result:=(i shl 6)or(i shl 4)or(i shl 2)or i;
    3: Result:=(i shl 5)or(i shl 2)or(i shr 1);
    4: Result:=(i shl 4)or i;
    5: Result:=(i shl 3)or(i shr 2);
    6: Result:=(i shl 2)or(i shr 4);
    7: Result:=(i shl 1)or(i shr 6);
    else Result:=i;
  end;
end;

function Get16Bpg:Byte;
var
  c: DWord;
  hBM: HBITMAP;
  sDC,bDC: Windows.HDC;
begin
  sDC:=GetDC(0);
  bDC:=CreateCompatibleDC(sDC);
  hBM:=CreateCompatibleBitmap(sDC,1,1);
  SelectObject(bDC,hBM);
  SetPixel(bDC,0,0,RGB(0,100,0));
  c:=GetPixel(bDC,0,0);
  DeleteDC(bDC);
  DeleteObject(hBM);
  ReleaseDC(0,sDC);
  if GetGValue(c)>=100 then Result:=6 else Result:=5;
end;

procedure EMMS;
asm
  db $0F,$77 // emms
end;

// from Graphics.pas
function BmpPitch(Width, Bpp : Integer; Align: Integer = 32): Integer;
begin
Dec(Align);
Result := ((Width * Bpp) + Align) and not Align;
Result := Result div 8;
end;

procedure CopyDIB(Src, Dst : TFastDIB);
Var n, LSize : Integer;
begin
If (Dst.Bpp = Src.Bpp) and (Dst.Bpg = Src.Bpg) then

  If (Dst.BWidth = Src.BWidth) and (Dst.Size <> 0) and
     (Src.BWidth = BmpPitch(Src.Width, Src.Bpp))
  then
    Move(Src.Bits^, Dst.Bits^, Dst.Size)
  else begin
    LSize := Src.Width * Src.Bpp shr 3;
    // for FDIB subsets, line size can be far less than BWidth
    For n:=0 to Dst.AbsHeight-1 do
      Move(Src.Scanlines[n]^, Dst.Scanlines[n]^, LSize);// Dst.BWidth)
  end

else // fmt conversion via GDI
  Src.Draw(Dst.hDC, 0, 0);
end;

procedure CopyPal(Src, Dst : TFastDIB);
Var c : Integer;
begin
{
If (Src.Bpp <= 8) and (Dst.Bpp = Src.Bpp) then begin
  Move(Src.Colors[0], Dst.Colors[0], (1 shl Src.Bpp) * SizeOf(TFColorA));
  Dst.UpdateColors;
end;
}
If (Src.Bpp <= 8) and (Dst.Bpp <= 8) then begin

  If (Dst.Bpp = 1) then begin
    Dst.Colors[0].c := tfBlack; Dst.Colors[1].c := tfWhite;
  end else

  begin
    c := Num_Min(Src.Bpp, Dst.Bpp);
    Move(Src.Colors[0], Dst.Colors[0], (1 shl c) * SizeOf(TFColorA));
  end;
  If (Dst.hDC <> 0) then Dst.UpdateColors;
end;
end;

procedure FillMem24(Dst : PLine24; Cnt : Integer; Color : TFColor);
Var n : Integer;
    dc : PIntArr;
begin
If (Cnt >= 4) then begin
  // fill first 4 colors
  Dst[0] := Color; Dst[1] := Color;
  Dst[2] := Color; Dst[3] := Color;
  Dec(Cnt, 4);
  If (Cnt >= 4) then begin
    // copy first 4 colors to overs, 4 * 3 bytes per cycle
    dc := PIntArr(Dst); Inc(PByte(dc), 12);
    Repeat
      dc[0] := PIntArr(Dst)[0];
      dc[1] := PIntArr(Dst)[1];
      dc[2] := PIntArr(Dst)[2];
      Inc(PByte(dc), 12);
      Dec(Cnt, 4);
    Until (Cnt < 4);
    Dst := PLine24(dc);
  end else
    Inc(PByte(Dst), 12);
end;
// fill the rest
For n:=0 to Cnt-1 do Dst[n] := Color;
end;

// increasing FDIB size with large steps
function GrowSize(FDIB : TFastDIB; NewWidth, NewHeight, Bpp : Integer;
                  MaxSize : PPoint = nil; AddVal : Integer = 0): Boolean;
Var w, h, NewH : Integer;
begin
Result := False;
w := FDIB.Width; h := FDIB.AbsHeight; NewH := Abs(NewHeight);
If (NewWidth > w) then begin
  If (AddVal <> 0) then Inc(w, AddVal)
                   else w := (w * 3) div 2; // * 1.5
  If w < NewWidth then w := NewWidth;
  If (MaxSize <> nil) and (w > MaxSize.x) then w := MaxSize.x;
  Result := True;
end;
If (NewH > h) then begin
  If (AddVal <> 0) then Inc(h, AddVal)
                   else h := (h * 3) div 2;
  If h < NewH then h := NewH;
  If (MaxSize <> nil) and (h > MaxSize.y) then h := MaxSize.y;
  Result := True;
end;
If (NewHeight < 0) then h := -h;     // diff. height signs
If (Result) or (Bpp <> FDIB.Bpp) or (h <> FDIB.Height) then
  FDIB.SetSize(w, h, Bpp)
end;


procedure GetCPUInfo;
{$IFDEF CPUX86}
  function HasCPUID:LongBool;
  asm
    pushfd
    pop  eax
    mov  ecx,eax
    xor  eax,$00200000
    push eax
    popfd
    pushfd
    pop  eax
    xor  eax,ecx
  end;

  procedure CPUID(Flag:DWord;var Signature,Features:DWord);
  asm
    push ebx
    push esi
    push edi
    mov  esi,edx
    mov  edi,ecx
    db   $0F,$A2 // cpuid
    mov  [esi],eax
    mov  [edi],edx
    pop  edi
    pop  esi
    pop  ebx
  end;

  function GetVendorID(VendorID:ShortString):DWord;
  asm
    push edi
    push ebx
    push eax
    xor  eax,eax
    db   $0F,$A2 // cpuid
    pop  edi
    mov  [edi],Byte(12)
    inc  edi
    push ecx
    push edx
    mov  ecx,3
    @loop:
      mov [edi],bl
      inc edi
      mov [edi],bh
      inc edi
      shr ebx,16
      mov [edi],bl
      inc edi
      mov [edi],bh
      inc edi
      pop ebx
      dec ecx
    jnz @loop
    pop edi
  end;
{$ENDIF}
var
  SysInfo: TSystemInfo;
  Signature,Features: DWord;
begin
{$IFDEF CPUX86}
  if HasCPUID then
  begin
    if GetVendorID(CPUInfo.VendorID) > 0 then
    begin
      // standard features
      CPUID(1,Signature,Features);
      CPUInfo.Family:=(Signature shr 8)and $0F;
      CPUInfo.Model:=(Signature shr 4)and $0F;
      if LongBool(Features and(1 shl 8)) then
        CPUInfo.Features:=CPUInfo.Features + [cfCX8];
      if LongBool(Features and(1 shl 15)) then
        CPUInfo.Features:=CPUInfo.Features + [cfCMOV];
      if LongBool(Features and(1 shl 23)) then
        CPUInfo.Features:=CPUInfo.Features + [cfMMX];
      if LongBool(Features and(1 shl 25)) then
        CPUInfo.Features:=CPUInfo.Features + [cfSSE];
      if LongBool(Features and(1 shl 26)) then
        CPUInfo.Features:=CPUInfo.Features + [cfSSE2];
      // extended features
      CPUID($80000000,Signature,Features);
      if Signature > $80000000 then
      begin
        CPUID($80000001,Signature,Features);
        if LongBool(Features and(1 shl 22)) then
          CPUInfo.Features:=CPUInfo.Features + [cfMMX2];
        if LongBool(Features and(1 shl 31)) then
          CPUInfo.Features:=CPUInfo.Features + [cf3DNow];
        if LongBool(Features and(1 shl 30)) then
          CPUInfo.Features:=CPUInfo.Features + [cf3DNow2];
      end;
    end;
  end;
{$ENDIF}
{$IFDEF CPUX64}
// assuming that modern CPU with x64 supports all features,
// except AMD's special... well, sometimes I'll make correct check
CPUInfo.Features := [cfCX8, cfCMOV, cfMMX, cfMMX2, cfSSE, cfSSE2];
{$ENDIF}
  GetSystemInfo(SysInfo);
  CPUInfo.CPUCount:=SysInfo.dwNumberOfProcessors;
end;



//var hMSIMG32: HINST;

initialization

  GetCPUInfo;
{
  hMSIMG32:=LoadLibrary('msimg32.dll');
  @TransBlit:=GetProcAddress(hMSIMG32,'TransparentBlt');
  @AlphaBlit:=GetProcAddress(hMSIMG32,'AlphaBlend');
}
finalization

//  FreeLibrary(hMSIMG32);

end.
