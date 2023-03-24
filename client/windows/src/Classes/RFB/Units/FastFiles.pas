unit FastFiles; // FastDIB: sourceforge.net/projects/tfastdib
interface
// Sapersky: GDI+ is used when loading via IJL is impossible (no DLL, x64)

uses Windows, FastDIB, FastGPlus, Classes;

{$I platform.inc}

const
  jBufferMode   = 1;
  jReadHeader   = 0;
  jReadEntropy  = 6;
  jWriteImage   = 8;
  jWriteHeader  = 10;
  jWriteEntropy = 12;
  jReadMode: array[0..4]of Byte = (2,14,16,18,20);

type
  TJPGScale = (jsNone,jsHalf,jsQuarter,jsEighth,jsThumbnail);

  TJPGProps = packed record
    UseExProps:     LongBool;
    DIBBytes:       Pointer;
    DIBWidth:       DWord;
    DIBHeight:      Integer;
    DIBPadBytes:    DWord;
    DIBChannels:    DWord;
    DIBColor:       Integer;
    DIBSubsampling: Integer;
    FileName:       PAnsiChar;
    JPGBytes:       Pointer;
    JPGSizeBytes:   DWord;
    JPGWidth:       DWord;
    JPGHeight:      DWord;
    JPGChannels:    DWord;
    JPGColor:       Integer;
    JPGSubsampling: Integer;
    JPGThumbWidth:  DWord;
    JPGThumbHeight: DWord;
    NeedsConvert:   LongBool;
    NeedsResample:  LongBool;
    Quality:        DWord;
    UselessCrap1:   array[0..7]of Byte;
    Rect:           TRect;
    IntelDCT:       Boolean;
    UselessCrap2:   array[0..19922]of Byte;
    Smooth:         Boolean;
    UselessCrap3:   array[0..38]of Byte;
  end;

  TFastJPG = class
  private
    fBmp: TFastDIB;
    procedure SetBitmap(Bmp:TFastDIB);
  public
    Props: TJPGProps;
    Scale: TJPGScale;
    constructor Create;
    destructor Destroy; override;
    procedure FlushProps;
    property Bitmap:TFastDIB read fBmp write SetBitmap;
    procedure LoadFile(Const FileName: Ansistring);
    procedure LoadStream(Stream:TStream);
    procedure LoadRes(hInst:Integer;ResID,ResType:string);
    procedure LoadMem(Mem:Pointer;Size:Integer);
    procedure SaveFile(Const FileName: Ansistring);
    procedure SaveStream(Stream:TStream);
    function SaveMem(Mem:Pointer;Size:Integer):Integer;
  end;

procedure LoadBMPFile(Bmp:TFastDIB;FileName:string);
procedure SaveBMPFile(Bmp:TFastDIB;FileName:string);
procedure LoadBMPStream(Bmp:TFastDIB;Stream:TStream);
procedure SaveBMPStream(Bmp:TFastDIB;Stream:TStream);

procedure LoadJPGFile(Bmp:TFastDIB;FileName:string;Smooth:Boolean);
procedure SaveJPGFile(Bmp:TFastDIB;FileName:string;Quality:Integer);
procedure LoadJPGStream(Bmp:TFastDIB;Stream:TStream;Smooth:Boolean);
procedure SaveJPGStream(Bmp:TFastDIB;Stream:TStream;Quality:Integer);
procedure LoadJPGMem(Bmp:TFastDIB;Mem:Pointer;Size:Integer;Smooth:Boolean);
procedure LoadJPGRes(Bmp:TFastDIB;hInst:Integer;ResID,ResType:string;Smooth:Boolean);
function  SaveJPGMem(Bmp:TFastDIB;Mem:Pointer;Size:Integer;Quality:Integer):Integer;
function  FindJPGEOI(Mem:PByte;Size:Integer):Integer;

var
  HasJPG: Boolean = False; // Intel® Jpeg Library
  JInit:  function(Props:Pointer):Integer; stdcall;
  JFree:  function(Props:Pointer):Integer; stdcall;
  JRead:  function(Props:Pointer;Method:Integer):Integer; stdcall;
  JWrite: function(Props:Pointer;Method:Integer):Integer; stdcall;

implementation

procedure LoadBMPFile(Bmp:TFastDIB;FileName:string);
begin
  Bmp.LoadFromFile(FileName);
end;

procedure SaveBMPFile(Bmp:TFastDIB;FileName:string);
begin
  Bmp.SaveToFile(FileName);
end;

procedure LoadBMPStream(Bmp:TFastDIB;Stream:TStream);
var
  fBits,fSize: Integer;
  Buffer: Pointer;
  bmInfo: TBMInfo;
  MemPtr : PByte;
begin
  fSize:=Stream.Size;
  if fSize>1078 then fSize:=1078;
  GetMem(Buffer,1078);
  Stream.ReadBuffer(Buffer^,fSize);
  fBits:=LoadHeader(Buffer,bmInfo);
  Bmp.SetSizeIndirect(bmInfo);
  Stream.Seek(fBits-fSize,soFromCurrent);
  if(bmInfo.Header.Compression=1)or(bmInfo.Header.Compression=2)then fSize:=(PDWord(NativeInt(Buffer)+2)^-DWord(fBits))else
  if Stream.Size-fBits > Integer(Bmp.Size) then fSize:=Bmp.Size else fSize:=Stream.Size-fBits;
  if(bmInfo.Header.Compression=0)or(bmInfo.Header.Compression=3)then
    Stream.ReadBuffer(Bmp.Bits^,fSize)
  else begin
    if Stream is TCustomMemoryStream then begin
      MemPtr := TCustomMemoryStream(Stream).Memory; Inc(MemPtr, fBits);
      if bmInfo.Header.Compression = 1 then DecodeRLE8(Bmp, MemPtr)
                                       else DecodeRLE4(Bmp, MemPtr);
    end else begin
      ReAllocMem(Buffer,fSize);
      Stream.ReadBuffer(Buffer^,fSize);
      if bmInfo.Header.Compression=1 then DecodeRLE8(Bmp,Buffer) else DecodeRLE4(Bmp,Buffer);
    end;
  end;
  FreeMem(Buffer);
end;

procedure SaveBMPStream(Bmp:TFastDIB;Stream:TStream);
var
  cSize: Integer;
  fHead: TBitmapFileHeader;
begin
  if Bmp.Info.Header.ClrUsed<>0 then cSize:=(Bmp.Info.Header.ClrUsed shl 2)
  else if Bmp.Info.Header.Compression=BI_BITFIELDS then cSize:=12
  else if Bmp.Bpp<=8 then cSize:=(1 shl Bmp.Bpp)shl 2
  else cSize:=0;
  fHead.bfType:=$4D42;
  fHead.bfSize:=54+Bmp.Size+DWord(cSize);
  fHead.bfOffBits:=54+cSize;
  Stream.WriteBuffer(fHead,SizeOf(fHead));
  Stream.WriteBuffer(Bmp.Info,40+cSize);
  Stream.WriteBuffer(Bmp.Bits^,Bmp.Size);
end;

procedure LoadJPGFile(Bmp:TFastDIB;FileName:string;Smooth:Boolean);
begin
  if HasJPG then with TFastJPG.Create do
  begin
    Bitmap:=Bmp;
    Props.Smooth:=Smooth;
    LoadFile(FileName);
    Free;
  end else
    gpLoadImage(FileName, Bmp);
end;

procedure SaveJPGFile(Bmp:TFastDIB;FileName:string;Quality:Integer);
begin
  if HasJPG then with TFastJPG.Create do
  begin
    Bitmap:=Bmp;
    Props.Quality:=Quality;
    SaveFile(FileName);
    Free;
  end else
    gpSaveImage(Bmp, FileName, Quality);
end;

procedure LoadJPGStream(Bmp:TFastDIB;Stream:TStream;Smooth:Boolean);
begin
  With TFastJPG.Create do begin
    Bitmap:=Bmp;
    Props.Smooth:=Smooth;
    LoadStream(Stream);
    Free;
  end;
end;

procedure SaveJPGStream(Bmp:TFastDIB;Stream:TStream;Quality:Integer);
begin
  if HasJPG then with TFastJPG.Create do
  begin
    Bitmap:=Bmp;
    Props.Quality:=Quality;
    SaveStream(Stream);
    Free;
  end;
end;

procedure LoadJPGMem(Bmp:TFastDIB;Mem:Pointer;Size:Integer;Smooth:Boolean);
begin
  With TFastJPG.Create do begin
    Bitmap:=Bmp;
    Props.Smooth:=Smooth;
    LoadMem(Mem,Size);
    Free;
  end;
end;

procedure LoadJPGRes(Bmp:TFastDIB;hInst:Integer;ResID,ResType:string;Smooth:Boolean);
begin
  if HasJPG then With TFastJPG.Create do
  begin
    Bitmap:=Bmp;
    Props.Smooth:=Smooth;
    LoadRes(hInst,ResID,ResType);
    Free;
  end else
    gpLoadImageRes(ResID, PChar(ResType), Bmp);
end;

function SaveJPGMem(Bmp:TFastDIB;Mem:Pointer;Size,Quality:Integer):Integer;
begin
  if HasJPG then with TFastJPG.Create do
  begin
    Bitmap:=Bmp;
    Props.Quality:=Quality;
    Result:=SaveMem(Mem,Size);
    Free;
  end else Result:=0;
end;

function FindJPGEOI(Mem:PByte;Size:Integer):Integer;
var
  Count: Integer;
begin
  Count:=1;
  while(Size<>0)and(PWord(Mem)^<>$D9FF)do
  begin
    Inc(Mem);
    Inc(Count);
    Dec(Size);
  end;
  Result:=Count;
end;

// TFastJPG 

constructor TFastJPG.Create;
begin
  FillChar(Props,SizeOf(Props),0);
  JInit(@Props);
end;

destructor TFastJPG.Destroy;
begin
  JFree(@Props);
  inherited Destroy;
end;

procedure TFastJPG.FlushProps;
begin
  JFree(@Props);
  FillChar(Props,SizeOf(Props),0);
  JInit(@Props);
end;

procedure TFastJPG.SetBitmap(Bmp:TFastDIB);
begin
  fBmp:=Bmp;
  Props.DIBBytes:=fBmp.Bits;
  Props.DIBWidth:=fBmp.Width;
  Props.DIBHeight:=-fBmp.Height;
  Props.DIBPadBytes:=fBmp.Gap;
  case fBmp.Bpp of
    8:
    begin
      Props.DIBChannels:=1;
      Props.DIBColor:=4;
    end;
    24:
    begin
      Props.DIBChannels:=3;
      Props.DIBColor:=2;
    end;
  end;
end;

procedure TFastJPG.LoadFile(Const FileName: Ansistring);
var
  w,h: Integer;
begin
  Props.FileName:=PAnsiChar(FileName);
  JRead(@Props,jReadHeader);

  w:=Props.JPGWidth shr Byte(Scale);
  h:=Props.JPGHeight shr Byte(Scale);
  if Scale=jsThumbnail then
  begin
    w:=Props.JPGThumbWidth;
    h:=Props.JPGThumbHeight;
  end;

  if(Props.JPGChannels=1)then
  begin
    Bitmap.SetSize(w,h,8);
    Bitmap.FillColors(0,255,[tfBlack,tfWhite]);
    Props.DIBChannels:=1;
    Props.DIBColor:=4;
  end else
  begin
    Bitmap.SetSize(w,h,24);
    Props.DIBChannels:=3;
    Props.DIBColor:=2;
  end;

  Props.DIBBytes:=Bitmap.Bits;
  Props.DIBWidth:=Bitmap.Width;
  Props.DIBHeight:=-Bitmap.Height;
  Props.DIBPadBytes:=Bitmap.Gap;

  JRead(@Props,jReadMode[Byte(Scale)]);
end;

procedure TFastJPG.LoadStream(Stream:TStream);
var
  Buffer: Pointer;
  BSize,JSize,CurPos: Integer;
  MemPtr : PByte;
begin
  CurPos := Stream.Position;
  BSize := Stream.Size - CurPos;
  if Stream is TCustomMemoryStream then
  begin
    MemPtr := TCustomMemoryStream(Stream).Memory; Inc(MemPtr, CurPos);
    JSize := FindJPGEOI(MemPtr, BSize);
    LoadMem(MemPtr, BSize);
    Stream.Position := CurPos + JSize;
  end else begin
    GetMem(Buffer,BSize);
    Stream.Read(Buffer^,BSize);
    JSize:=FindJPGEOI(Buffer,BSize);
    LoadMem(Buffer,BSize);
    Stream.Position:=CurPos+JSize;
    FreeMem(Buffer);
  end;
end;

procedure TFastJPG.LoadRes(hInst:Integer;ResID,ResType:string);
var
  pMem: Pointer;
  hRes,Size: Integer;
begin
  hRes:=FindResource(hInst,PChar(ResID),PChar(ResType));
  Size:=SizeofResource(hInst,hRes);
  pMem:=LockResource(LoadResource(hInst,hRes));
  if pMem<>nil then LoadMem(pMem,Size);
end;

procedure TFastJPG.LoadMem(Mem:Pointer;Size:Integer);
var
  w,h: Integer;
begin
if HasJPG then begin
  Props.JPGBytes:=Mem;
  Props.JPGSizeBytes:=Size;
  JRead(@Props,jReadHeader or jBufferMode);

  w:=Props.JPGWidth shr Byte(Scale);
  h:=Props.JPGHeight shr Byte(Scale);
  if Scale=jsThumbnail then
  begin
    w:=Props.JPGThumbWidth;
    h:=Props.JPGThumbHeight;
  end;

  if(Props.JPGChannels=1)then
  begin
    Bitmap.SetSize(w,h,8);
    Bitmap.FillColors(0,255,[tfBlack,tfWhite]);
    Props.DIBChannels:=1;
    Props.DIBColor:=4;
  end else
  begin
    Bitmap.SetSize(w,h,24);
    Props.DIBChannels:=3;
    Props.DIBColor:=2;
  end;

  Props.DIBBytes:=Bitmap.Bits;
  Props.DIBWidth:=Bitmap.Width;
  Props.DIBHeight:=-Bitmap.Height;
  Props.DIBPadBytes:=Bitmap.Gap;
  JRead(@Props,jReadMode[Byte(Scale)]or jBufferMode);
end else
  gpLoadImageMem(Mem, Size, Bitmap);
end;

procedure TFastJPG.SaveFile(Const FileName: Ansistring);
begin
  Props.DIBBytes:=Bitmap.Bits;
  Props.DIBWidth:=Bitmap.Width;
  Props.DIBHeight:=-Bitmap.Height;
  Props.DIBPadBytes:=Bitmap.Gap;
  if(Bitmap.Bpp=8)then
  begin
    Props.DIBChannels:=1;
    Props.DIBColor:=4;
  end;

  Props.FileName:=PAnsiChar(FileName);
  Props.JPGWidth:=Bitmap.Width;
  Props.JPGHeight:=Bitmap.AbsHeight;

  JWrite(@Props,jWriteImage)
end;

procedure TFastJPG.SaveStream(Stream:TStream);
var
  Buffer: Pointer;
  Size: Integer;
begin
  GetMem(Buffer,Bitmap.Size);
  Size:=SaveMem(Buffer,Bitmap.Size);
  Stream.WriteBuffer(Buffer^,Size);
  FreeMem(Buffer);
end;

function TFastJPG.SaveMem(Mem:Pointer;Size:Integer):Integer;
begin
  Props.DIBBytes:=Bitmap.Bits;
  Props.DIBWidth:=Bitmap.Width;
  Props.DIBHeight:=-Bitmap.Height;
  Props.DIBPadBytes:=Bitmap.Gap;
  if(Bitmap.Bpp=8)then
  begin
    Props.DIBChannels:=1;
    Props.DIBColor:=4;
  end;

  Props.JPGBytes:=Mem;
  Props.JPGSizeBytes:=Size;
  Props.JPGWidth:=Bitmap.Width;
  Props.JPGHeight:=Bitmap.AbsHeight;

  JWrite(@Props,jWriteImage or jBufferMode);
  Result:=Props.JPGSizeBytes;
end;

procedure SetIJLCPUKey;
var
  Key: HKEY;
  CPUKey,Dummy: Integer;
begin
  CPUKey:=0;

  // determine Pentium, Pentium Pro, Pentium II
  if CPUInfo.VendorID='GenuineIntel' then
    if CPUInfo.Family=5 then CPUKey:=1 else if CPUInfo.Family=6 then
      if CPUInfo.Model > 1 then CPUKey:=4 else CPUKey:=2;

  // determine MMX, Pentium III, Pentium 4
  if(cfMMX in CPUInfo.Features)and(CPUKey < 3)then CPUKey:=3;
  if cfSSE in CPUInfo.Features then CPUKey:=5;
  if cfSSE2 in CPUInfo.Features then CPUKey:=6;

  RegCreateKeyEx(
    HKEY_LOCAL_MACHINE,'SOFTWARE\Intel Corporation\PLSuite\IJLib',0,nil,
    REG_OPTION_NON_VOLATILE,KEY_WRITE,nil,Key,@Dummy);

  RegSetValueEx(Key,'USECPU',0,REG_DWORD,@CPUKey,4);
  RegCloseKey(Key);
end;

var
  hIJL: HINST;

initialization

  hIJL:=LoadLibrary('ijl15.dll');
  if hIJL<>0 then
  begin
    SetIJLCPUKey;
    HasJPG:=True;

    @JInit:=GetProcAddress(hIJL,'ijlInit');
    @JFree:=GetProcAddress(hIJL,'ijlFree');
    @JRead:=GetProcAddress(hIJL,'ijlRead');
    @JWrite:=GetProcAddress(hIJL,'ijlWrite');
  end;

finalization

  FreeLibrary(hIJL);

end.
