unit FastSize; // FastDIB: sourceforge.net/projects/tfastdib
               // by: gordy <gordy@dynamicsdirect.com>
interface      //     cmr.Pent <cmr.Pent@gmail.com>
               //     Sapersky <sapersky@gmail.com>

//  Routines for resampling one TFastDIB to another TFastDIB of different size.

{
History:
07.04.13, Sapersky:
  Quick2x: fixed bugs with non-zero gap, optimized
  SmoothResize : optimized (1.5-2x), added built-in BPP conversions
  Added: FastDec2X, FastDec4X_8, Projective, FastResizeTo24, BilinearTo24
  x64 ready
}

{$R-}
{$DEFINE ChkDim} //  verifies that both Src and Dst have (Width*Height)>0 before resizing
uses Windows, FastDIB, uAegysUtils;

{$I platform.inc}

// Dst.Bpp = Src.Bpp
procedure Quick2x(Src,Dst:TFastDIB);
procedure   Quick2x8(Src,Dst:TFastDIB);
procedure   Quick2x16(Src,Dst:TFastDIB);
procedure   Quick2x24(Src,Dst:TFastDIB);
procedure   Quick2x32(Src,Dst:TFastDIB);

// Src.Bpp -> Dst.Bpp: (8 -> 8) (SrcHasSmoothPal = True);
// (8 || 16 || 24 || 32) -> (24 || 32)
procedure FastDec2X(Src, Dst : TFastDIB; SrcHasSmoothPal : Boolean = False);
procedure FastDec2X_24(Src, Dst : TFastDIB; w, h : Integer);
procedure FastDec4X_8(Src, Dst : TFastDIB);

// quick-and-dirty resize, Dst.Bpp = Src.Bpp
procedure FastResize(Src,Dst:TFastDIB);
procedure   FastResize8(Src,Dst:TFastDIB);
procedure   FastResize16(Src,Dst:TFastDIB);
procedure   FastResize24(Src,Dst:TFastDIB);
procedure   FastResize32(Src,Dst:TFastDIB);
// (8 -> 8);  (8 || 16 || 24 || 32) -> (24 || 32)
// built-in Lut conversion (brightness/constrast/gamma)
procedure FastResizeTo24(Src, Dst : TFastDIB; Lut : PLine8 = nil;
                         VFlip : Boolean = False);

// fast bilinear resampling (2*2 pts), same as used by graphics hardware
// quality is good if Scale > 0.5
procedure Bilinear(Src,Dst:TFastDIB);
procedure   Bilinear8(Src,Dst:TFastDIB);
procedure   Bilinear16(Src,Dst:TFastDIB);
procedure   Bilinear24(Src,Dst:TFastDIB);
procedure   Bilinear32(Src,Dst:TFastDIB);
procedure BilinearTo24(Src, Dst : TFastDIB; Lut : PLine8 = nil;
                       VFlip : Boolean = False);

// high-quality resampling (slower) 
Type
  TResizeFilter = (rfNearest, rfBox, rfBilinear, rfHanning, rfHamming, rfHermite,
                   rfBell, rfCatrom, rfBicubic, rfSpline2, rfMitchell, rfGaussian,
                   rfSpline3, rfSinC3, rfLanczos3, rfBlackman3);
  // rfCatrom seems to be a good filter (optimal balance of blurring/sharpening)
  // equal to "Bicubic with A = -0.5"
  // visually equal to rfBlackman3, but faster

  TFloatType = Single;
//  TFloatType = Double;

  TRFuncRes = record
    X, Value : TFloatType;
    Idx, Idx2 : Integer;
    Valid : Boolean;
  end;

  DFuncResArr = array of TRFuncRes;

Const
  FilterRads : array [ TResizeFilter ] of Double =
    ( 0, 0.5, 1, 1, 1, 1, 1.5, 2, 2, 2, 2, 2, 3, 3, 3, 3 );

// (8 -> 8);  (8 || 16 || 24 || 32) -> (16 || 24 || 32)
// UseMMX - 20-30% faster, but less precise
// (not recommended for final processing)
procedure SmoothResize(Src, Dst : TFastDIB; Filter : TResizeFilter;
                       UseMMX : Boolean = False; Buf : TFastDIB = nil);
// legacy, for backward compability
procedure   SmoothResize8(Src,Dst:TFastDIB;Filter:TResizeFilter);
procedure   SmoothResize24(Src,Dst:TFastDIB;Filter:TResizeFilter);
procedure   SmoothResize32(Src,Dst:TFastDIB;Filter:TResizeFilter);
// for debug/display
function SResizeFunc(x, Rad: Double; FilterType : TResizeFilter): Double;
function FSizeScaled(Scale, NormalRad : Single; FloatRad : PSingle = nil): Integer;
function GetResizeFunc(Var Arr : DFuncResArr; Const CurX : TFloatType; MaxSrcSize : Integer;
                       Const Scale, Rad : TFloatType; Filter : TResizeFilter;
                       Normalize : Boolean = True): Integer;

// Projective image transform (aka texture mapping). Based on Graphics32 code.
// 2x or more faster (SSE used)
// 24/32 bpp. Smooth = true -> bilinear filter - slower
// PtsOnDst = True -> Pts is dst image coords, False - source image
// (like texture coordinates, but in 0..ImgSize-1 range)
function Projective(Src, Dst : TFastDIB; Pts : PRPointArr; Smooth : Boolean = False;
                    PtsOnDst : Boolean = True): Boolean;

implementation

// ********************************* Quick2x ***********************************

procedure Quick2x8(Src,Dst:TFastDIB);
var
  x,y: Integer;
  pd : PWord;
  ps : PByte;
begin
for y:=0 to Src.AbsHeight-1 do begin
  ps := Src.Scanlines[y]; pd := Dst.Scanlines[y*2];
  for x:=0 to Src.Width-1 do begin
    pd^ := ps^ or (ps^ shl 8);
    Inc(ps); Inc(pd);
  end;
  Move(Dst.Scanlines[y*2]^, Dst.Scanlines[y*2+1]^, Dst.Width);
end;
end;

procedure Quick2x16(Src,Dst:TFastDIB);
var
  x,y: Integer;
  pd : PDWord;
  ps : PWord;
begin
for y:=0 to Src.AbsHeight-1 do begin
  ps := Src.Scanlines[y]; pd := Dst.Scanlines[y*2];
  for x:=0 to Src.Width-1 do begin
    pd^ := ps^ or (ps^ shl 16);
    Inc(ps); Inc(pd);
  end;
  Move(Dst.Scanlines[y*2]^, Dst.Scanlines[y*2+1]^, Dst.Width*2);
end;
end;

procedure Quick2x24(Src,Dst:TFastDIB);
var
  x,y,i: Integer;
  ps, pd : PDWord;
//  ps, pd : PFColor;
begin
for y:=0 to Src.AbsHeight-1 do begin
  ps := Src.Scanlines[y]; pd := Dst.Scanlines[y*2];
  for x:=0 to Src.Width-2 do begin
{
    pd^:=ps^; Inc(pd);
    pd^:=ps^; Inc(pd);
    Inc(ps);
}
    // processing 3+3 bytes as 4+2
    i := ps^ and $FFFFFF;
    i := i or (i shl 24);
    pd^ := i; Inc(pd);
    PWord(pd)^ := i shr 8; Inc(PWord(pd));
    Inc(PByte(ps), 3);
  end;
  PFColor(pd)^ := PFColor(ps)^; Inc(pd); // last pixel - common way
  PFColor(pd)^ := PFColor(ps)^;

  Move(Dst.Scanlines[y*2]^, Dst.Scanlines[y*2+1]^, Dst.Width*3);
end;
end;

procedure Quick2x32(Src,Dst:TFastDIB);
var
  x,y: Integer;
  ps, pd : PDWord;
begin
for y:=0 to Src.AbsHeight-1 do begin
  ps := Src.Scanlines[y]; pd := Dst.Scanlines[y*2];
  for x:=0 to Src.Width-1 do begin
    {$IFDEF CPUX64} // must be faster, but not tested 
    PInt64(pd)^ := ps^ or (Int64(ps^) shl 32); Inc(PInt64(pd));
    {$ELSE}
    pd^:=ps^; Inc(pd);
    pd^:=ps^; Inc(pd);
    {$ENDIF}
    Inc(ps);
  end;
  Move(Dst.Scanlines[y*2]^, Dst.Scanlines[y*2+1]^, Dst.Width*4);
end;
end;

procedure Quick2x(Src,Dst:TFastDIB);
begin
  case Dst.Bpp of
    8:  Quick2x8(Src,Dst);
    16: Quick2x16(Src,Dst);
    24: Quick2x24(Src,Dst);
    32: Quick2x32(Src,Dst);
  end;
end;


// ******************************* FastDec2X ***********************************

// 8 -> 8
procedure xFastDec2X_8(Src, Dst : TFastDIB; w, h : Integer);
Var x, y : Integer;
    sb1, sb2 : PLine8;
    db : PByte;
begin
For y:=0 to h-1 do begin
  sb1 := Src.Scanlines[y*2];
  sb2 := sb1; Inc(PByte(sb2), Src.BWidth);
  db := Dst.Scanlines[y];
  For x:=0 to w-1 do begin
    db^ := ( sb1[0] + sb1[1] + sb2[0] + sb2[1] ) shr 2;
    Inc(PByte(sb1), 2); Inc(PByte(sb2), 2);
    Inc(db);
  end;
end;
end;

// 8 -> 24
procedure xFastDec2X_8_24(Src, Dst : TFastDIB; w, h : Integer);
Var x, y : Integer;
    dc : PFColor;
    sb1, sb2 : PLine8;
    c : PFColorTable;
begin
c := Src.Colors;
For y:=0 to h-1 do begin
  sb1 := Src.Scanlines[y*2];
  If y < h then sb2 := PLine8(NativeInt(sb1) + Src.BWidth)
           else sb2 := sb1;
  dc := Dst.Scanlines[y];
  For x:=0 to w-1 do begin
    dc.b := ( c[sb1[0]].b + c[sb1[1]].b + c[sb2[0]].b + c[sb2[1]].b ) shr 2;
    dc.g := ( c[sb1[0]].g + c[sb1[1]].g + c[sb2[0]].g + c[sb2[1]].g ) shr 2;
    dc.r := ( c[sb1[0]].r + c[sb1[1]].r + c[sb2[0]].r + c[sb2[1]].r ) shr 2;
    Inc(PByte(sb1), 2); Inc(PByte(sb2), 2);
    Inc(dc);
  end;
end;
end;


procedure xFastDec2X_16_24(Src, Dst : TFastDIB; w, h : Integer);
Var x, y : Integer;
    sc1, sc2 : PLine16;
    dc : PFColor;
    Lut : PLut16;
begin
Lut := Src.GenLut16;//(Src);
For y:=0 to h-1 do begin
  sc1 := Src.Scanlines[y*2];
  sc2 := sc1; Inc(PByte(sc2), Src.BWidth);
  dc := Dst.Scanlines[y];
  For x:=0 to w-1 do begin
    dc.b := ( Lut[sc1[0]].b + Lut[sc1[1]].b + Lut[sc2[0]].b + Lut[sc2[1]].b ) shr 2;
    dc.g := ( Lut[sc1[0]].g + Lut[sc1[1]].g + Lut[sc2[0]].g + Lut[sc2[1]].g ) shr 2;
    dc.r := ( Lut[sc1[0]].r + Lut[sc1[1]].r + Lut[sc2[0]].r + Lut[sc2[1]].r ) shr 2;
    Inc(PWord(sc1), 2); Inc(PWord(sc2), 2);
    Inc(dc);
  end;
end;
end;


// 24 -> 24, Dst can be the same as Src
procedure FastDec2X_24(Src, Dst : TFastDIB; w, h : Integer);
Var x, y, DstW, DstH : Integer;
    dc : PFColor;
    sc1, sc2 : PLine24;
begin
If (Dst <> Src) then begin DstW := Dst.Width; DstH := Dst.AbsHeight; end
                else begin DstW := w; DstH := h; end;
For y := 0 to DstH-1 do begin
  sc1 := Src.Scanlines[y*2];
  If y < h then sc2 := PLine24(NativeInt(sc1) + Src.BWidth)
           else sc2 := sc1;
  dc := Dst.Scanlines[y];
  For x := 0 to DstW-1 do begin
    dc.b := ( sc1[0].b + sc1[1].b + sc2[0].b + sc2[1].b ) shr 2;
    dc.g := ( sc1[0].g + sc1[1].g + sc2[0].g + sc2[1].g ) shr 2;
    dc.r := ( sc1[0].r + sc1[1].r + sc2[0].r + sc2[1].r ) shr 2;
    Inc(PFColor(sc1), 2); Inc(PFColor(sc2), 2);
    Inc(dc);
  end;
  // processing edge pixels for case with upper dst size rounding
  If DstW > w then begin
    dc.b := (sc1[0].b + sc2[0].b) shr 2; dc.g := (sc1[0].g + sc2[0].g) shr 2;
    dc.r := (sc1[0].r + sc2[0].r) shr 2;
  end;
end;
end;

// 32 -> 32 with MMX
{$IFDEF CPUX86}
procedure xLine_Dec2X_MMX(Src1, Src2, Dst : Pointer; Count : Integer);
// EAX = Src1,  EDX = Src2, ECX = Dst, [EBP + 8] = Count
asm
  push ebx
  mov ebx, ecx // ebx - dst
  mov ecx, Count
  db $0F,$EF,$F6           /// pxor mm6, mm6
  db $0F,$EF,$FF           /// pxor mm7, mm7
@quads:

  db $0F,$6F,$00           /// movq mm0, [eax]
  db $0F,$6F,$12           /// movq mm2, [edx]
  db $0F,$6F,$C8           /// movq mm1, mm0
  db $0F,$6F,$DA           /// movq mm3, mm2
  db $0F,$60,$C6           /// punpcklbw mm0, mm6
  db $0F,$60,$D7           /// punpcklbw mm2, mm7
  db $0F,$68,$CE           /// punpckhbw mm1, mm6
  db $0F,$68,$DF           /// punpckhbw mm3, mm7
  db $0F,$DD,$C1           /// paddusw mm0, mm1
  db $0F,$DD,$D3           /// paddusw mm2, mm3
  db $0F,$DD,$C2           /// paddusw mm0, mm2
  db $0F,$71,$D0,$02       /// psrlw mm0, 2
  db $0F,$67,$C6           /// packuswb mm0, mm6
  db $0F,$7E,$03           /// movd  [ebx], mm0

  add eax, 8
  add edx, 8
  add ebx, 4

  dec ecx
  jnz @quads

  pop ebx
end;
{$ENDIF}

// 32 -> 32
procedure xFastDec2X_32(Src, Dst : TFastDIB; w, h : Integer);
Var x, y : Integer;
    sc1, sc2 : PLine32;
    dc : PFColorA;
begin
{$IFDEF CPUX86}
If (cfMMX in CPUInfo.Features) and (w >= 4) then begin
  sc1 := Src.Scanlines[0]; sc2 := Src.Scanlines[1];
  x := Src.BWidth div 2; // * (2 lines) div (4 BytePP)
  For y:=0 to h-1 do begin
    xLine_Dec2X_MMX(sc1, sc2, Dst.Scanlines[y], w);
    Inc(PFColorA(sc1), x); Inc(PFColorA(sc2), x);
  end;
  asm
    db $0F,$77               /// emms
  end;
end else
{$ENDIF}
  For y:=0 to h-1 do begin
    sc1 := Src.Scanlines[y*2];
    sc2 := sc1; Inc(PByte(sc2), Src.BWidth);
    dc := Dst.Scanlines[y];
    For x:=0 to w-1 do begin
      dc.b := ( sc1[0].b + sc1[1].b + sc2[0].b + sc2[1].b ) shr 2;
      dc.g := ( sc1[0].g + sc1[1].g + sc2[0].g + sc2[1].g ) shr 2;
      dc.r := ( sc1[0].r + sc1[1].r + sc2[0].r + sc2[1].r ) shr 2;
      dc.a := ( sc1[0].a + sc1[1].a + sc2[0].a + sc2[1].a ) shr 2;
      Inc(PFColorA(sc1), 2); Inc(PFColorA(sc2), 2);
      Inc(dc);
    end;
  end;
end;


procedure FastDec2X(Src, Dst : TFastDIB; SrcHasSmoothPal : Boolean = False);
Var w, h, dw, dh, dBpp : Integer;
begin
w := Src.Width div 2; h := Src.Height div 2;
//If w*2 < Src.Width then dw := w+1 else dw := w;
//If h*2 < Src.Height then dh := h+1 else dh := h;
dw := w; dh := h;

If ((Src.Bpp = 8) and (not SrcHasSmoothPal)) or (Src.Bpp = 16) then
  dBpp := 24
else
  dBpp := Src.Bpp;

If (Dst.Width <> dw) or (Dst.Height <> dh) or (Dst.Bpp <> dBpp) then
  Dst.SetSize(dw, dh, dBpp);

h := Abs(h);  
Case Src.Bpp of
  8  : If (dBpp = 24) then xFastDec2X_8_24(Src, Dst, w, h) else begin
         xFastDec2X_8(Src, Dst, w, h);
         Move(Src.Colors[0], Dst.Colors[0], 1024);
         Dst.UpdateColors;
       end;
  16 : xFastDec2X_16_24(Src, Dst, w, h);
  24 : FastDec2X_24(Src, Dst, w, h);
  32 : xFastDec2X_32(Src, Dst, w, h);
end;
end;


{$IFDEF CPUX86}
procedure xLine_Dec4X_8_MMX(Src, Dst : Pointer; Count, SrcPitch : Integer);
asm
  push esi
  push edi
  push ebx
  mov esi, eax
  mov edi, edx
  mov ebx, SrcPitch
  imul eax, ebx, 3
  mov SrcPitch, eax
  db $0F,$EF,$FF           /// pxor mm7, mm7
@quads:
  mov eax, SrcPitch
  db $0F,$6E,$06           /// movd mm0, [esi]
  db $0F,$6E,$0C,$1E       /// movd mm1, [esi + ebx]
  db $0F,$6E,$14,$5E       /// movd mm2, [esi + ebx*2]
  db $0F,$6E,$1C,$06       /// movd mm3, [esi + eax]
  db $0F,$60,$C7           /// punpcklbw mm0, mm7
  db $0F,$60,$CF           /// punpcklbw mm1, mm7
  db $0F,$60,$D7           /// punpcklbw mm2, mm7
  db $0F,$60,$DF           /// punpcklbw mm3, mm7
  db $0F,$DD,$C1           /// paddusw mm0, mm1
  db $0F,$DD,$D3           /// paddusw mm2, mm3
  db $0F,$DD,$C2           /// paddusw mm0, mm2

  db $0F,$6F,$C8           /// movq mm1, mm0
  db $0F,$73,$D1,$20       /// psrlq mm1, 32
  db $0F,$DD,$C1           /// paddusw mm0, mm1

  db $0F,$7E,$C0           /// movd eax, mm0

  mov edx, eax
  and edx, $FFFF
  shr eax, 16
  add eax, edx
  shr eax, 4
  mov [edi], al

  add esi, 4
  inc edi
  dec ecx
  jnz @quads

  pop ebx
  pop edi
  pop esi
end;
{$ENDIF}


procedure FastDec4X_8(Src, Dst : TFastDIB);
Var x, y, dw, dh, bw, bw3 : Integer;
    sb : PLine8;
    db : PByte;
begin
dw := Src.Width div 4; dh := Src.AbsHeight div 4;
If (dw = 0) or (dh = 0) then Exit;

bw := Src.BWidth;
{$IFDEF CPUX86}
If (cfMMX in CPUInfo.Features) then begin
  For y:=0 to dh-1 do begin
    xLine_Dec4X_8_MMX(Src.Scanlines[y*4], Dst.Scanlines[y], dw, bw);
  end;
  asm
    db $0F,$77               /// emms
  end;
end else
{$ENDIF}
begin
  bw3 := bw * 3;
  For y:=0 to dh-1 do begin
    sb := Src.Scanlines[y*4]; db := Dst.Scanlines[y];
    For x:=0 to dw-1 do begin
      db^ := ( sb[0]    + sb[1]      + sb[2]      + sb[3] +
               sb[bw]   + sb[bw+1]   + sb[bw+2]   + sb[bw+3] +
               sb[bw*2] + sb[bw*2+1] + sb[bw*2+2] + sb[bw*2+3] +
               sb[bw3]  + sb[bw3+1]  + sb[bw3+2]  + sb[bw3+3] ) shr 4;
      Inc(PByte(sb), 4); Inc(db);
    end;
  end;
end;
end;



// ******************************* FastResize **********************************

procedure FastResize8(Src,Dst:TFastDIB);
var
  x,y,xp,yp,sx,sy: Integer;
  Line: PLine8;
  pc: PByte;
begin
  sx:=(Src.Width shl 16)div Dst.Width;
  sy:=(Src.AbsHeight shl 16)div Dst.AbsHeight;
  yp:=sy shr 1; pc:=Pointer(Dst.Bits);
  for y:=0 to Dst.AbsHeight-1 do
  begin
    Line:=Src.Scanlines[yp shr 16]; xp:=sx shr 1;
    for x:=0 to Dst.Width-1 do
    begin
      pc^:=Line[xp shr 16];
      Inc(pc); Inc(xp,sx);
    end;
    Inc(yp,sy);
    Inc(pc,Dst.Gap);
  end;
end;

procedure FastResize16(Src,Dst:TFastDIB);
var
  x,y,xp,yp,sx,sy: Integer;
  Line: PLine16;
  pc: PWord;
begin
  sx:=(Src.Width shl 16)div Dst.Width;
  sy:=(Src.AbsHeight shl 16)div Dst.AbsHeight;
  yp:=sy shr 1; pc:=Pointer(Dst.Bits);
  for y:=0 to Dst.AbsHeight-1 do
  begin
    Line:=Src.Scanlines[yp shr 16]; xp:=sx shr 1;
    for x:=0 to Dst.Width-1 do
    begin
      pc^:=Line[xp shr 16];
      Inc(pc); Inc(xp,sx);
    end;
    Inc(yp,sy);
    pc:=PWord(NativeInt(pc)+Dst.Gap);
  end;
end;

procedure FastResize24(Src,Dst:TFastDIB);
var
  x,y,xp,yp,sx,sy: Integer;
  Line: PLine24;
  pc: PFColor;
begin
  sx:=(Src.Width shl 16)div Dst.Width;
  sy:=(Src.AbsHeight shl 16)div Dst.AbsHeight;
  yp:=sy shr 1; pc:=Pointer(Dst.Bits);
  for y:=0 to Dst.AbsHeight-1 do
  begin
    Line:=Src.Scanlines[yp shr 16]; xp:=sx shr 1;
    for x:=0 to Dst.Width-1 do
    begin
      pc^:=Line[xp shr 16];
      Inc(pc); Inc(xp,sx);
    end;
    Inc(yp,sy);
    pc:=PFColor(NativeInt(pc)+Dst.Gap);
  end;
end;

procedure FastResize32(Src,Dst:TFastDIB);
var
  x,y,xp,yp,sx,sy: Integer;
  Line: PLine32;
  pc: PFColorA;
begin
  sx:=(Src.Width shl 16)div Dst.Width;
  sy:=(Src.AbsHeight shl 16)div Dst.AbsHeight;
  yp:=sy shr 1; pc:=Pointer(Dst.Bits);
  for y:=0 to Dst.AbsHeight-1 do
  begin
    Line:=Src.Scanlines[yp shr 16]; xp:=sx shr 1;
    for x:=0 to Dst.Width-1 do
    begin
      pc^:=Line[xp shr 16];
      Inc(pc); Inc(xp,sx);
    end;
    Inc(yp,sy);
    pc:=PFColorA(NativeInt(pc)+Dst.Gap);
  end;
end;

procedure FastResize(Src,Dst:TFastDIB);
begin
{$IFDEF ChkDim}
  if (Src.Width*Src.Height=0)or(Dst.Width*Dst.Height=0) then Exit;
{$ENDIF}
  case Src.Bpp of
    8:  FastResize8(Src,Dst);
    16: FastResize16(Src,Dst);
    24: FastResize24(Src,Dst);
    32: FastResize32(Src,Dst);
  end;
end;

// Src can be 8, 16, 24, 32 bpp, Dst is 24 or 32 bpp
procedure FastResizeTo24(Src, Dst : TFastDIB; Lut : PLine8 = nil;
                         VFlip : Boolean = False);
var
  x,y,xp,yp,sx,sy,offs,dh: Integer;
  Line : PLine8;
  pc, sc: PFColor;
  Lut16 : PLut16;
begin
If (Src.Bpp = Dst.Bpp) and (Lut = nil) and (not VFlip) then begin
  FastResize(Src, Dst); Exit;
end else
  If not (Dst.Bpp in [24, 32]) then Exit;

If (Src.Bpp = 16) then Lut16 := Src.GenLut16(Lut); //GenLut16(Src, Lut);

sx := (Src.Width shl 16) div Dst.Width;
sy := (Src.AbsHeight shl 16) div Dst.AbsHeight;
offs := Dst.Bpp shr 3;
yp := sy shr 1;
dh := Dst.AbsHeight-1;
for y:=0 to dh do begin
  xp := sx shr 1;
  If VFlip then pc := Dst.Scanlines[dh-y]
           else pc := Dst.Scanlines[y];
  Line := Src.Scanlines[yp shr 16];
  Case Src.Bpp of
    8 : begin // move 4 bytes instead of 3 - seems to be more efficient
              // (although 24-to-24 bpp version is not)
          For x:=0 to Dst.Width-2 do begin
            PFColorA(pc)^ := Src.Colors[ Line[xp shr 16] ];
            Inc(PByte(pc), offs);
            Inc(xp, sx);
          end; // last iteration - 3 bytes
          With Src.Colors[ Line[xp shr 16] ] do begin
            pc.b := b; pc.g := g; pc.r := r;
          end;
        end;
    16 : For x:=0 to Dst.Width-1 do begin
           pc^ := Lut16[ PLine16(Line)[xp shr 16] ];
           Inc(PByte(pc), offs); Inc(xp,sx);
         end;
    24 : If (Lut <> nil) then
           For x:=0 to Dst.Width-1 do begin
             sc := @PLine24(Line)[xp shr 16];
             pc.b := Lut[sc.b]; pc.g := Lut[sc.g]; pc.r := Lut[sc.r];
             Inc(PByte(pc), offs); Inc(xp,sx);
           end
         else
           For x:=0 to Dst.Width-1 do begin
             pc^ := PFColor(NativeInt(Line) + (xp shr 16) * 3)^;
             Inc(PByte(pc), offs); Inc(xp,sx);
           end;
    32 : If (Lut <> nil) then
           For x:=0 to Dst.Width-1 do begin
             sc := @PLine32(Line)[xp shr 16];
             pc.b := Lut[sc.b]; pc.g := Lut[sc.g]; pc.r := Lut[sc.r];
             Inc(PByte(pc), offs); Inc(xp,sx);
           end else
             For x:=0 to Dst.Width-1 do begin
               pc^ := PFColor(NativeInt(Line) + (xp shr 16) * 4)^;
               Inc(PByte(pc), offs); Inc(xp,sx);
             end;
  end;
  Inc(yp,sy);
end;

end;



// ******************************* Bilinear ************************************

procedure Bilinear8(Src,Dst:TFastDIB);
var
  x,y,xp,yp,ypp,xpp,t1,t2,
  zx,zy,izy,w1,w2,w3,w4: Integer;
  y1,y2: PLine8;
  pc: PByte;
begin
  xpp:=(Src.Width shl 16)div Dst.Width;
  ypp:=(Src.AbsHeight shl 16)div Dst.AbsHeight;
  yp:=(ypp shr 1)-$8000;
  pc:=Pointer(Dst.Bits);
  for y:=0 to Dst.AbsHeight-1 do
  begin
    if yp<0 then
    begin
      t1:=0;
      zy:=0;
      izy:=$10000;
    end
    else begin
      t1:=yp shr 16;
      zy:=yp and $FFFF;
      izy:=((not yp)and $FFFF)+1;
    end;
    if t1<Src.AbsHeight-1 then t2:=t1+1 else t2:=t1;
    y1:=Src.Scanlines[t1];
    y2:=Src.Scanlines[t2];
    xp:=(xpp shr 1)-$8000;
    for x:=0 to Dst.Width-1 do
    begin
      if xp<0 then
      begin
        t1:=0;
        zx:=0;                                                        
      end
      else begin
        t1:=xp shr 16;
        zx:=xp and $FFFF;
      end;
      if t1<Src.Width-1 then t2:=t1+1 else t2:=t1;
      w2:=(izy*zx)shr 16;  w1:=izy-w2;
      w4:=( zy*zx)shr 16;  w3:= zy-w4;
      pc^:=(y1[t1]*w1+y1[t2]*w2+y2[t1]*w3+y2[t2]*w4+$8000)shr 16;
      Inc(xp,xpp);
      Inc(pc);
    end;
    Inc(yp,ypp);
    Inc(pc,Dst.Gap);
  end;
end;


procedure Bilinear16(Src,Dst:TFastDIB);
var
  x,y,xp,yp,ypp,xpp,t1,t2,
  zx,zy,izy,w1,w2,w3,w4: Integer;
  y1,y2: PLine16;
  pc: PWord;
  c1, c2, c3, c4 : PFColor;
  Lut : PLut16;
  bLut : PBackLut16;
begin
  Lut := Src.GenLut16;
  bLut := Dst.GenBackLut16;
  xpp:=(Src.Width shl 16)div Dst.Width;
  ypp:=(Src.AbsHeight shl 16)div Dst.AbsHeight;
  yp:=(ypp shr 1)-$8000;
  pc:=Pointer(Dst.Bits);
  for y:=0 to Dst.AbsHeight-1 do
  begin
    if yp<0 then
    begin
      t1:=0;
      zy:=0;
      izy:=$10000;
    end
    else begin
      t1:=yp shr 16;
      zy:=yp and $FFFF;
      izy:=((not yp)and $FFFF)+1;
    end;
    if t1<Src.AbsHeight-1 then t2:=t1+1 else t2:=t1;
    y1:=Src.Scanlines[t1];
    y2:=Src.Scanlines[t2];
    xp:=(xpp shr 1)-$8000;
    for x:=0 to Dst.Width-1 do
    begin
      if xp<0 then
      begin
        t1:=0;
        zx:=0;
      end
      else begin
        t1:=xp shr 16;
        zx:=xp and $FFFF;
      end;
      if t1<Src.Width-1 then t2:=t1+1 else t2:=t1;
      w2:=(izy*zx)shr 16;  w1:=izy-w2;
      w4:=( zy*zx)shr 16;  w3:= zy-w4;
      c1 := @Lut[y1[t1]]; c2 := @Lut[y1[t2]]; c3 := @Lut[y2[t1]]; c4 := @Lut[y2[t2]];
      With bLut^ do
        pc^:= bi[(c1.b*w1 + c2.b*w2 + c3.b*w3 + c4.b*w4) shr 16] or
              gi[(c1.g*w1 + c2.g*w2 + c3.g*w3 + c4.g*w4) shr 16] or
              ri[(c1.r*w1 + c2.r*w2 + c3.r*w3 + c4.r*w4) shr 16];

      Inc(xp,xpp);
      Inc(pc);
    end;
    Inc(yp,ypp);
    pc:=PWord(NativeInt(pc)+Dst.Gap);
  end;
end;



procedure Bilinear24(Src,Dst:TFastDIB);
var
  x,y,xp,yp,ypp,xpp,t1,t2,
  zx,zy,izy,w1,w2,w3,w4: Integer;
  y1,y2: PLine24;
  pc: PFColor;
begin
  xpp:=(Src.Width shl 16)div Dst.Width;
  ypp:=(Src.AbsHeight shl 16)div Dst.AbsHeight;
  yp:=(ypp shr 1)-$8000;
  pc:=Pointer(Dst.Bits);
  for y:=0 to Dst.AbsHeight-1 do
  begin
    if yp<0 then
    begin
      t1:=0;
      zy:=0;
      izy:=$10000;
    end
    else begin
      t1:=yp shr 16;
      zy:=yp and $FFFF;
      izy:=((not yp)and $FFFF)+1;
    end;
    if t1<Src.AbsHeight-1 then t2:=t1+1 else t2:=t1;
    y1:=Src.Scanlines[t1];
    y2:=Src.Scanlines[t2];
    xp:=(xpp shr 1)-$8000;
    for x:=0 to Dst.Width-1 do
    begin
      if xp<0 then
      begin
        t1:=0;
        zx:=0;                                                        
      end
      else begin
        t1:=xp shr 16;
        zx:=xp and $FFFF;
      end;
      if t1<Src.Width-1 then t2:=t1+1 else t2:=t1;
      w2:=(izy*zx)shr 16;  w1:=izy-w2;
      w4:=( zy*zx)shr 16;  w3:= zy-w4;
      pc.b:=(y1[t1].b*w1+y1[t2].b*w2+y2[t1].b*w3+y2[t2].b*w4+$8000)shr 16;
      pc.g:=(y1[t1].g*w1+y1[t2].g*w2+y2[t1].g*w3+y2[t2].g*w4+$8000)shr 16;
      pc.r:=(y1[t1].r*w1+y1[t2].r*w2+y2[t1].r*w3+y2[t2].r*w4+$8000)shr 16;
      Inc(xp,xpp);
      Inc(pc);
    end;
    Inc(yp,ypp);
    pc:=PFColor(NativeInt(pc)+Dst.Gap);
  end;
end;

procedure Bilinear32(Src,Dst:TFastDIB);
var
  x,y,xp,yp,ypp,xpp,t1,t2,
  zx,zy,izy,w1,w2,w3,w4: Integer;
  y1,y2: PLine32;
  pc: PFColorA;
begin
  xpp:=(Src.Width shl 16)div Dst.Width;
  ypp:=(Src.AbsHeight shl 16)div Dst.AbsHeight;
  yp:=(ypp shr 1)-$8000;
  pc:=Pointer(Dst.Bits);
  for y:=0 to Dst.AbsHeight-1 do
  begin
    if yp<0 then
    begin
      t1:=0;
      zy:=0;
      izy:=$10000;
    end
    else begin
      t1:=yp shr 16;
      zy:=yp and $FFFF;
      izy:=((not yp)and $FFFF)+1;
    end;
    if t1<Src.AbsHeight-1 then t2:=t1+1 else t2:=t1;
    y1:=Src.Scanlines[t1];
    y2:=Src.Scanlines[t2];
    xp:=(xpp shr 1)-$8000;
    for x:=0 to Dst.Width-1 do
    begin
      if xp<0 then
      begin
        t1:=0;
        zx:=0;                                                        
      end
      else begin
        t1:=xp shr 16;
        zx:=xp and $FFFF;
      end;
      if t1<Src.Width-1 then t2:=t1+1 else t2:=t1;
      w2:=(izy*zx)shr 16;  w1:=izy-w2;
      w4:=( zy*zx)shr 16;  w3:= zy-w4;
      pc.b:=(y1[t1].b*w1+y1[t2].b*w2+y2[t1].b*w3+y2[t2].b*w4+$8000)shr 16;
      pc.g:=(y1[t1].g*w1+y1[t2].g*w2+y2[t1].g*w3+y2[t2].g*w4+$8000)shr 16;
      pc.r:=(y1[t1].r*w1+y1[t2].r*w2+y2[t1].r*w3+y2[t2].r*w4+$8000)shr 16;
      pc.a:=(y1[t1].a*w1+y1[t2].a*w2+y2[t1].a*w3+y2[t2].a*w4+$8000)shr 16;
      Inc(xp,xpp);
      Inc(pc);
    end;
    Inc(yp,ypp);
    pc:=PFColorA(NativeInt(pc)+Dst.Gap);
  end;
end;

procedure Bilinear(Src,Dst:TFastDIB);
begin
{$IFDEF ChkDim}
  if (Src.Width*Src.Height=0)or(Dst.Width*Dst.Height=0) then Exit;
{$ENDIF}
  case Dst.Bpp of
    8:  Bilinear8(Src,Dst);
    16: Bilinear16(Src,Dst);
    24: Bilinear24(Src,Dst);
    32: Bilinear32(Src,Dst);
  end;
end;


Const
  BiAddVal = $8000;

procedure xLine_Bilinear8_24(y1, y2: PLine8; Dst : PFColor; SrcPal : PFColorTable;
                             SrcWidth, DstWidth, DstStep, z2, iz2 : Integer);
var
  x, xp, xpp, t, z, sStep2, w1, w2, w3, w4: Integer;
  c11, c12, c21, c22 : PFColor;
begin
xpp := (SrcWidth shl 16) div DstWidth;
xp := (xpp shr 1) - $8000; sStep2 := 1; Dec(SrcWidth);

for x:=0 to DstWidth-1 do begin
  If (xp < 0) then begin
    t := 0; z := 0;
  end else begin
    t := xp shr 16; z := xp and $FFFF;
  end;
  If (t >= SrcWidth) then sStep2 := 0;

  w2 := (z * iz2) shr 16;
  w1 := iz2 - w2;
  w4 := (z * z2) shr 16;
  w3 := z2 - w4;

  c11 := PFColor( NativeInt(SrcPal) + 4 * y1[t] );
  c12 := PFColor( NativeInt(SrcPal) + 4 * y1[t + sStep2] );
  c21 := PFColor( NativeInt(SrcPal) + 4 * y2[t] );
  c22 := PFColor( NativeInt(SrcPal) + 4 * y2[t + sStep2] );

  Dst.b := (c11.b * w1 + c12.b * w2 + c21.b * w3 + c22.b * w4 + BiAddVal) shr 16;
  Dst.g := (c11.g * w1 + c12.g * w2 + c21.g * w3 + c22.g * w4 + BiAddVal) shr 16;
  Dst.r := (c11.r * w1 + c12.r * w2 + c21.r * w3 + c22.r * w4 + BiAddVal) shr 16;

  Inc(xp, xpp);
  Inc(PByte(Dst), DstStep);
end;
end;


procedure xLine_Bilinear16(y1, y2: PLine16; Dst : PFColor; Lut : PLut16;
                           SrcWidth, DstWidth, DstStep, z2, iz2 : Integer);
var
  x, xp, xpp, sStep2, t, z, w1, w2, w3, w4: Integer;
  c11, c12, c21, c22 : PFColor;
begin
xpp := (SrcWidth shl 16) div DstWidth;
xp := (xpp shr 1) - $8000; sStep2 := 2; Dec(SrcWidth);

for x:=0 to DstWidth-1 do begin
  If (xp < 0) then begin
    t := 0; z := 0;
  end else begin
    t := xp shr 16; z := xp and $FFFF;
  end;
  If (t >= SrcWidth) then sStep2 := 0;

  w2 := (z * iz2) shr 16;
  w1 := iz2 - w2;
  w4 := (z * z2) shr 16;
  w3 := z2 - w4;

  t := t * 2;
  c11 := @Lut[ PWord( NativeInt(y1) + t )^ ];
  c12 := @Lut[ PWord( NativeInt(y1) + t + sStep2 )^ ];
  c21 := @Lut[ PWord( NativeInt(y2) + t )^ ];
  c22 := @Lut[ PWord( NativeInt(y2) + t + sStep2 )^ ];

  Dst.b := (c11.b * w1 + c12.b * w2 + c21.b * w3 + c22.b * w4 + BiAddVal) shr 16;
  Dst.g := (c11.g * w1 + c12.g * w2 + c21.g * w3 + c22.g * w4 + BiAddVal) shr 16;
  Dst.r := (c11.r * w1 + c12.r * w2 + c21.r * w3 + c22.r * w4 + BiAddVal) shr 16;
  Inc(xp, xpp);
  Inc(PByte(Dst), DstStep);
end;
end;


procedure xLine_Bilinear24(y1, y2 : PFColor; Dst : PFColor; Lut : PLine8;
                           SrcWidth, SrcStep, DstWidth, DstStep, z2, iz2 : Integer);
Var x, xp, xpp, sStep2, t, z, w1, w2, w3, w4 : Integer;
    c11, c12, c21, c22 : PFColor;
begin
xpp := (SrcWidth shl 16) div DstWidth;
xp := (xpp shr 1) - $8000; Dec(SrcWidth);
sStep2 := SrcStep;
For x:=0 to DstWidth-1 do begin
  If (xp < 0) then begin
    t := 0; z := 0;
  end else begin
    t := xp shr 16; z := xp and $FFFF;
  end;
  If (t >= SrcWidth) then sStep2 := 0;

  w2 := (iz2 * z) shr 16;
  w1 := iz2 - w2;
  w4 := (z2 * z) shr 16;
  w3 := z2 - w4;

  c11 := PFColor( NativeInt(y1)  + SrcStep * t );
  c12 := PFColor( NativeInt(c11) + sStep2 );
  c21 := PFColor( NativeInt(y2)  + SrcStep * t );
  c22 := PFColor( NativeInt(c21) + sStep2 );

  If Lut <> nil then begin
    Dst.b := Lut[ (c11.b * w1 + c12.b * w2 + c21.b * w3 + c22.b * w4 + BiAddVal) shr 16 ];
    Dst.g := Lut[ (c11.g * w1 + c12.g * w2 + c21.g * w3 + c22.g * w4 + BiAddVal) shr 16 ];
    Dst.r := Lut[ (c11.r * w1 + c12.r * w2 + c21.r * w3 + c22.r * w4 + BiAddVal) shr 16 ];
  end else begin
    Dst.b := (c11.b * w1 + c12.b * w2 + c21.b * w3 + c22.b * w4 + BiAddVal) shr 16;
    Dst.g := (c11.g * w1 + c12.g * w2 + c21.g * w3 + c22.g * w4 + BiAddVal) shr 16;
    Dst.r := (c11.r * w1 + c12.r * w2 + c21.r * w3 + c22.r * w4 + BiAddVal) shr 16;
  end;

  Inc(xp, xpp);
  Inc(PByte(Dst), DstStep);
end;
end;


procedure BilinearTo24(Src, Dst : TFastDIB; Lut : PLine8 = nil;
                       VFlip : Boolean = False);
var
  y, yp, ypp, yp16, z2, iz2, sBytesPP, dBytesPP, h, dh: Integer;
  y1, y2 : PLine8;
  dc : PFColor;
  Lut16 : PLut16;
begin
If (Src.Bpp = Dst.Bpp) and (Lut = nil) and (not VFlip) then begin
  Bilinear(Src, Dst); Exit;
end else
  If not (Dst.Bpp in [24, 32]) then Exit;

If (Src.Bpp = 16) then Lut16 := Src.GenLut16(Lut); //GenLut16(Src, Lut);

dBytesPP := Dst.Bpp shr 3; sBytesPP := Src.Bpp shr 3;
h := Src.AbsHeight;
ypp := (h shl 16) div Dst.AbsHeight;
yp := (ypp shr 1) - $8000;
dh := Dst.AbsHeight-1;
For y:=0 to dh do begin
  If VFlip then dc := Dst.Scanlines[dh-y]
           else dc := Dst.Scanlines[y];
  If (yp < 0) then begin
    yp16 := 0; z2 := 0; iz2 := $10000;
  end else begin
    yp16 := yp shr 16;
    z2 := (yp and $FFFF); iz2 := ((not yp) and $FFFF);
  end;

  y1 := Src.Scanlines[yp16];
  if yp16 < Src.AbsHeight-1 then y2 := PLine8( NativeInt(y1) + Src.BWidth )
                            else y2 := y1;
  Case sBytesPP of
    1  : xLine_Bilinear8_24(y1, y2, dc, Src.Colors,
                            Src.Width, Dst.Width, dBytesPP, z2, iz2);
    2 : xLine_Bilinear16(PLine16(y1), PLine16(y2), dc, Lut16,
                          Src.Width, Dst.Width, dBytesPP, z2, iz2);
    3, 4 :
        xLine_Bilinear24(PFColor(y1), PFColor(y2), dc, Lut,
                         Src.Width, sBytesPP, Dst.Width, dBytesPP, z2, iz2)
  end;
  Inc(yp,ypp);
end;
end;



// ************************ SmoothResize *********************************

function SResizeFunc(x, Rad: Double; FilterType : TResizeFilter): Double;
Const
  Pi = System.Pi;
  B = 1/3;
  C = 1/3;
  p0 = (  6.0 -2.0*B       )/6.0;
  p2 = (-18.0+12.0*B +6.0*C)/6.0;
  p3 = ( 12.0 -9.0*B -6.0*C)/6.0;
  q0 = (       8.0*B+24.0*C)/6.0;
  q1 = (     -12.0*B-48.0*C)/6.0;
  q2 = (       6.0*B+30.0*C)/6.0;
  q3 = (          -B -6.0*C)/6.0;
  A = -0.75;

  function SinC(x: Double): Double;
  begin
    if x=0 then Result:=1
    else begin
      x:=Pi*x;
      Result:=Sin(x)/x;
    end;
  end;

Var G, Bk : Double;
begin
Case FilterType of
  rfNearest : Result := 1;
  rfBox : begin
    if x<0 then x:=-x;
    if x<=Rad then Result:=1 else Result:=0;
  end;
  rfBilinear : begin
    if x<0 then x:=-x;
    if x<Rad then Result:=1-x else Result:=0;
  end;
  rfHanning : begin
    if x<0 then x:=-x;
    if x<Rad then Result:=0.5+0.5*Cos(Pi*x) else Result:=0;
  end;
  rfHamming : begin
    if x<0 then x:=-x;
    if x<Rad then Result:=0.54+0.46*Cos(Pi*x) else Result:=0;
  end;
  rfHermite : begin
    if x<0 then x:=-x;
    if x<Rad then Result:=(2*x-3)*Sqr(x)+1 else Result:=0;
  end;
  rfBell : begin
    if x<0 then x:=-x;
    if x<0.5 then Result:=0.75-Sqr(x)
    else if x<1.5 then Result:=0.5*Sqr(x-1.5)
    else Result:=0;
  end;

  rfBicubic : begin
    if x<0 then x:=-x;
//      if x<1 then Result:=(1/6)*(3*Sqr(x)*(x-2)+4)
//      else if x<2 then Result:=-(1/6)*(x*(x*(x-6)+12)-8)
    if x<1 then Result := 0.5   *Sqr(x)*x - Sqr(x)       + 4/6 else
    if x<2 then Result := -(1/6)*Sqr(x)*x + Sqr(x) - 2*x + 4/3
    else Result:=0;
  end;
{
  // more common way of bicubic kernel calculation, with koeff A
  rfBicubic : begin
    if x<0 then x:=-x;
    if x<1 then Result := (A+2)* Sqr(x)*x - (A+3)*Sqr(x) + 1
    else if x<2 then Result := A*Sqr(x)*x - 5.0*A*Sqr(x) + 8.0*A*x - 4.0*A
    else Result:=0;
  end;
}
  rfCatrom : begin
    if x<0 then x:=-x;
    if x<1 then Result:=0.5*(2+Sqr(x)*(-5+x*3))
    else if x<2 then Result:=0.5*(4+x*(-8+x*(5-x)))
    else Result:=0;
  end;
  rfSpline2 : begin
    if x<0 then x:=-x;
    if x<1 then Result:=((x-9/5)*x-1/5)*x+1
    else if x<2 then Result:=((-1/3*(x-1)+4/5)*(x-1)-7/15)*(x-1)
    else Result:=0;
  end;
  rfMitchell : begin
    if x<0 then x:=-x;
    if x<1 then Result:=p0+Sqr(x)*(p2+x*p3)
    else if x<2 then Result:=q0+x*(q1+x*(q2+x*q3))
    else Result:=0;
  end;
  rfGaussian : begin
    G := 1.5957691216057307117597842397375 / Rad;
    if x<0 then x:=-x;
    if x<Rad then Result:=G*Exp(-8*Sqr(x/Rad))
    else Result:=0;
  end;
  rfSpline3 : begin
    if x<0 then x:=-x;
    if x<1 then Result:=((13/11*x-453/209)*x-3/209)*x+1
    else if x<2 then Result:=((-6/11*(x-1)+270/209)*(x-1)-156/209)*(x-1)
    else if x<3 then Result:=(( 1/11*(x-2) -45/209)*(x-2) +26/209)*(x-2)
    else Result:=0;
  end;
  rfSinC3 : begin
    if x<0 then x:=-x;
    if x<Rad then Result:=SinC(x)
    else Result:=0;
  end;
  rfLanczos3 : begin
    if x<0 then x:=-x;
    if x<Rad then Result:=SinC(x)*SinC(x/Rad)
    else Result:=0;
  end;
  rfBlackman3 : begin
    Bk := Pi / Rad;
    if x<0 then x:=-x;
    if x<Rad then Result:=SinC(x)*(0.42+0.5*Cos(Bk*x)+0.08*Cos(2*Bk*x))
    else Result:=0;
  end;
  else Result:=0;
end;
end;


function FSizeScaled(Scale, NormalRad : Single;
                     FloatRad : PSingle = nil): Integer;
Var r : Single;
begin
If Scale > 1 then Scale := 1 else
  If Scale <= 0 then begin Result := 0; Exit; end;
r := NormalRad / Scale;
If FloatRad <> nil then FloatRad^ := r;
Result := 2 * Trunc(r) + 1;
end;


function GetResizeFunc(Var Arr : DFuncResArr; Const CurX : TFloatType;
                       MaxSrcSize : Integer; Const Scale, Rad : TFloatType;
                       Filter : TResizeFilter; Normalize : Boolean = True): Integer;
Var Cnt, x1, x2, i, i1, c : Integer;
    xf, r, m, v, tv, sm : TFloatType;
begin
If Scale < 1 then sm := Scale else sm := 1;
r := Rad / sm;
Cnt := 2 * Trunc(r) + 1;

If (Length(Arr) < Cnt) then SetLength(Arr, Cnt);

m := CurX/Scale - 0.5 * (1 - 1 / Scale);
// m примерно = -0.5..SrcSize-0.5
// - 0.5 * (1 - 1 / Scale) - дл€ точного попадани€ на место

if Cnt > 1 then x1 := Math_Ceil(m - r)
           else x1 := Round(m);
x2 := x1 + Cnt - 1;

tv := 0; c := 0;
For i := x1 to x2 do begin
  i1 := i - x1;
  arr[i1].Idx := i; arr[i1].Idx2 := i - ((x1 + x2) shr 1);
  If (i >= 0) and (i < MaxSrcSize) then begin
    xf := (m - i) * sm;
    v := SResizeFunc(xf, Rad, Filter) * sm;
    tv := tv + v;
//      arr[i1] := v; arr[i1 + n] := xf;
    arr[i1].Value  := v; arr[i1].X := xf; arr[i1].Valid := True;
    Inc(c);
  end else
    arr[i1].Valid := False;
end;

// note: в результате этой коррекции весовые коэфф. могут
// получитьс€ больше 1 (в целочисленном варианте больше ContMax)

If (Normalize) and (c <> 0) and (tv <> 1) then begin
  v := (1 - tv) / c; tv := 0;
  For i := 0 to Cnt-1 do
    If arr[i].Valid then begin
      arr[i].Value := arr[i].Value + v; tv := tv + arr[i].Value;
    end;
end;

Result := Cnt;
end;


procedure SmoothResize(Src, Dst : TFastDIB; Filter : TResizeFilter;
                       UseMMX : Boolean = False; Buf : TFastDIB = nil);
Const
  ForceOneCont = True;
    // force sum of all contributors to be 1 (65536)
    // to use 'shr' instead of 'div'
//  ForceOneCont = False;

  ContShift = 14;
//  ContMax = 1 shl ContShift;
  ContMax = (1 shl ContShift) - 1;
  MaxVal = ContMax * $FF;

Type
  TContributor = record
    pos : Integer;
    Case Integer of
      0 : (w : Integer);
      1 : (InvFW : TFloatType);
  end;
  PContributor = ^TContributor;

  TFilterProc =  procedure (Src, Dst : TFastDIB; FilterLen : Integer;
                            Cont : PContributor; Horisontal : Boolean);

Var
  Lut16 : PLut16;
  BLut16 : PBackLut16;

  function PreCalcEx2(Var Cont : PContributor; Count, MaxCnt : Integer; s, Rad : TFloatType): Integer;
  Var Cnt, i, t, x : Integer;
      pw : PContributor;
      arr : DFuncResArr;
  begin
  Cnt := FSizeScaled(s, Rad);
  ReAllocMem(Cont, Count * (Cnt + 1) * SizeOf(TContributor));
  pw := Cont;
  SetLength(arr, Cnt);
  for x := 0 to Count-1 do begin
    GetResizeFunc(arr, x, MaxCnt, s, Rad, Filter, ForceOneCont);
    t := 0;
    For i := 0 to Cnt-1 do begin
      If (arr[i].Valid) then begin
        pw.pos := arr[i].Idx;
        pw.w := Round(arr[i].Value * ContMax);
        Inc(t, pw.w);
      end else begin
        pw.pos := 0; pw.w := 0;
      end;
      Inc(pw);
    end;
//    If t <> 32767 then asm int 3; end;
    If t > 0 then pw.w := t else pw.w := 1;
    Inc(pw);
  end;
  Result := Cnt;
  end;

  procedure DoFilter(Src, Dst : TFastDIB; FilterLen : Integer;
                     Cont : PContributor; Horisontal : Boolean);
  Var
    i, x, y, zr, zg, zb, za, w, sBytesPP, dBytesPP : Integer;
    pc: PFColor;
    c: PFColorA;
    pw, ph : PContributor;
    SLine : PLine8;
  begin
  ph := Cont;
  sBytesPP := Src.Bpp shr 3;
  dBytesPP := Dst.Bpp shr 3;
  For y:=0 to Dst.AbsHeight-1 do begin
    pw := Cont; pc := Dst.Scanlines[y];
    If Horisontal then SLine := Src.Scanlines[y];
    
    For x := 0 to Dst.Width-1 do begin
      zr := 0; zg := 0; zb := 0; za := 0;
      // this Case does not affect performance,
      // although those which sets dst pixel makes proc 20% slower
      If Horisontal then
        Case sBytesPP of
          1 : If (dBytesPP = 1) then
                For i := 0 to FilterLen - 1 do begin
//                  If pw.w <> 0 then Inc(zb, pw.w * Src.Pixels8[y, pw.pos]);
                  If pw.w <> 0 then Inc(zb, pw.w * SLine[pw.pos]);
                  Inc(pw);
                end
              else
                For i := 0 to FilterLen - 1 do begin
                  w := pw.w;
                  If (w <> 0) then begin
                    c := @Src.Colors[ SLine[pw.pos] ];
//                    c := @Src.Colors[ Src.Pixels8[y, pw.pos] ];
                    Inc(zr, w * c.r); Inc(zg, w * c.g); Inc(zb, w * c.b);
                  end;
                  Inc(pw);
                end;
          2 : For i := 0 to FilterLen - 1 do begin
                w := pw.w;
                If w <> 0 then begin
                  c := @Lut16[ PLine16(SLine)[pw.pos] ];
//                  c := @Lut16[ Src.Pixels16[y, pw.pos] ];
                  Inc(zr, w * c.r); Inc(zg, w * c.g); Inc(zb, w * c.b);
                end;
                Inc(pw);
              end;
          3 : For i := 0 to FilterLen - 1 do begin
                w := pw.w;
                If w <> 0 then begin
                  c := @PLine24(SLine)[pw.pos];
//                  c := @Src.Pixels24[y, pw.pos];
                  Inc(zr, w * c.r); Inc(zg, w * c.g); Inc(zb, w * c.b);
                end;
                Inc(pw);
              end;
          4 : For i := 0 to FilterLen - 1 do begin
                w := pw.w;
                If w <> 0 then begin
                  c := @PLine32(SLine)[pw.pos];
//                  c := @Src.Pixels32[y, pw.pos];
                  Inc(zr, w * c.r); Inc(zg, w * c.g); Inc(zb, w * c.b); Inc(za, w * c.a);
                end;
                Inc(pw);
              end;
         end
      else begin
        pw := ph;
        Case sBytesPP of
          1 : If (dBytesPP = 1) then
                For i := 0 to FilterLen - 1 do begin
                  If pw.w <> 0 then Inc(zb, pw.w * Src.Pixels8[pw.pos, x]);
                  Inc(pw);
                end
              else
                For i := 0 to FilterLen - 1 do begin
                  w := pw.w;
                  If w <> 0 then begin
                    c := @Src.Colors[ Src.Pixels8[pw.pos, x] ];
                    Inc(zr, w * c.r); Inc(zg, w * c.g); Inc(zb, w * c.b);
                  end;
                  Inc(pw);
                end;
          2 : For i := 0 to FilterLen - 1 do begin
                w := pw.w;
                If w <> 0 then begin
                  c := @Lut16[ Src.Pixels16[pw.pos, x] ];
                  Inc(zr, w * c.r); Inc(zg, w * c.g); Inc(zb, w * c.b);
                end;
                Inc(pw);
              end;
          3 : For i := 0 to FilterLen - 1 do begin
                w := pw.w;
                If w <> 0 then begin
                  c := @Src.Pixels24[pw.pos, x];
                  Inc(zr, w * c.r); Inc(zg, w * c.g); Inc(zb, w * c.b);
                end;
                Inc(pw);
              end;
          4 : For i := 0 to FilterLen - 1 do begin
                w := pw.w;
                If w <> 0 then begin
                  c := @Src.Pixels32[pw.pos, x];
                  Inc(zr, w * c.r); Inc(zg, w * c.g); Inc(zb, w * c.b); Inc(za, w * c.a);
                end;
                Inc(pw);
              end;
        end;
      end;

      w := pw.w;
      If ForceOneCont then begin

        Inc(zr, w shr 1); Inc(zg, w shr 1); Inc(zb, w shr 1);
          // по смыслу это добавление 0.5 чтобы был Round, а не Trunc
        If zr < 0 then zr := 0 else If zr > MaxVal then zr := MaxVal;
        If zg < 0 then zg := 0 else If zg > MaxVal then zg := MaxVal;
        If zb < 0 then zb := 0 else If zb > MaxVal then zb := MaxVal;
//        zb := zb shr 16; zg := zg shr 16; zr := zr shr 16;
        Case dBytesPP of
          1 : PByte(pc)^ := zb shr ContShift;
          2 : With BLut16^ do
                PWord(pc)^ := ri[zr shr ContShift] or gi[zg shr ContShift] or bi[zb shr ContShift];
          3 : begin pc.b := zb shr ContShift; pc.g := zg shr ContShift; pc.r := zr shr ContShift; end;
          4 : begin
                pc.b := zb shr ContShift; pc.g := zg shr ContShift; pc.r := zr shr ContShift;
                Inc(za, w shr 1);
                If za < 0 then za := 0 else If za > MaxVal then za := MaxVal;
                PFColorA(pc).a := za shr ContShift;
              end;
        end;
      end else begin

        zr := (zr + (w shr 1)) div w;
        zg := (zg + (w shr 1)) div w;
        zb := (zb + (w shr 1)) div w;

        If zr < 0 then zr := 0 else If zr > $FF then zr := $FF;
        If zg < 0 then zg := 0 else If zg > $FF then zg := $FF;
        If zb < 0 then zb := 0 else If zb > $FF then zb := $FF;

        Case dBytesPP of
          1 : PByte(pc)^ := zb;
          2 : With BLut16^ do
                PWord(pc)^ := ri[zr] or gi[zg] or bi[zb];
          3 : begin pc.b := zb; pc.g := zg; pc.r := zr; end;
          4 : begin
                pc.b := zb; pc.g := zg; pc.r := zr;
                za := (za + (w shr 1)) div w;
                If za < 0 then PFColorA(pc).a := 0 else
                  If za > $FF then PFColorA(pc).a := $FF else PFColorA(pc).a := za;
              end;
        end;

      end;

      Inc(pw);
      Inc(PByte(pc), dBytesPP);
    end;
    Inc(ph, FilterLen + 1);
  end;
  end;

  // 8 -> 8
  procedure DoFilter8(Src, Dst : TFastDIB; FilterLen : Integer;
                      Cont : PContributor; Horisontal : Boolean);
  Var
    i, x, y, zb, w : Integer;
    pb: PByte;
    pw, ph : PContributor;
  begin
  ph := Cont;
  For y:=0 to Dst.AbsHeight-1 do begin
    pw := Cont; pb := Dst.Scanlines[y];
    For x := 0 to Dst.Width-1 do begin
      zb := 0;
      If Horisontal then
        For i := 0 to FilterLen - 1 do begin
          If pw.w <> 0 then Inc(zb, pw.w * Src.Pixels8[y, pw.pos]);
          Inc(pw);
        end
      else begin
        pw := ph;
        For i := 0 to FilterLen - 1 do begin
          If pw.w <> 0 then Inc(zb, pw.w * Src.Pixels8[pw.pos, x]);
          Inc(pw);
        end;
      end;

      w := pw.w;
      If ForceOneCont then begin
        Inc(zb, w shr 1);
        If zb < 0 then zb := 0 else If zb > MaxVal then zb := MaxVal;
        pb^ := zb shr ContShift;
      end else begin
        zb := (zb + (w shr 1)) div w;
        // checking needed because contributor value may be <0
        If zb<0 then pb^:=0 else if zb>$FF then pb^:=$FF else pb^:=zb;
      end;
      Inc(pw);
      Inc(pb);
    end;
    Inc(ph, FilterLen + 1);
  end;
  end;

  // 24->24,32
  procedure DoFilter24(Src, Dst : TFastDIB; FilterLen : Integer;
                       Cont : PContributor; Horisontal : Boolean);
  Var
    i, x, y, zr, zg, zb, w, dBytesPP : Integer;
    pc: PFColor;
    c: PFColorA;
    pw, ph : PContributor;
    SLine : PLine24;
  begin
  ph := Cont; dBytesPP := Dst.Bpp shr 3;
  For y:=0 to Dst.AbsHeight-1 do begin
    pw := Cont; pc := Dst.Scanlines[y];
    If Horisontal then SLine := Src.Scanlines[y];

    For x := 0 to Dst.Width-1 do begin
      zr := 0; zg := 0; zb := 0;
      If Horisontal then
        For i := 0 to FilterLen - 1 do begin
          w := pw.w;
          If w <> 0 then begin
//            c := @Src.Pixels24[y, pw.pos];
            c := @SLine[pw.pos];
            Inc(zr, w * c.r); Inc(zg, w * c.g); Inc(zb, w * c.b);
          end;
          Inc(pw);
        end
      else begin
        pw := ph;
        For i := 0 to FilterLen - 1 do begin
          w := pw.w;
          If w <> 0 then begin
            c := @Src.Pixels24[pw.pos, x];
            Inc(zr, w * c.r); Inc(zg, w * c.g); Inc(zb, w * c.b);
          end;
          Inc(pw);
        end;
      end;

      w := pw.w;
      If ForceOneCont then begin
        Inc(zr, w shr 1); Inc(zg, w shr 1); Inc(zb, w shr 1);
        If zr < 0 then zr := 0 else If zr > MaxVal then zr := MaxVal;
        If zg < 0 then zg := 0 else If zg > MaxVal then zg := MaxVal;
        If zb < 0 then zb := 0 else If zb > MaxVal then zb := MaxVal;
        pc.b := zb shr ContShift; pc.g := zg shr ContShift;
        pc.r := zr shr ContShift;
      end else begin
        zr := (zr + (w shr 1)) div w;
        zg := (zg + (w shr 1)) div w;
        zb := (zb + (w shr 1)) div w;
        // checking needed because contributor value may be <0
        If zr<0 then pc.r:=0 else if zr>$FF then pc.r:=$FF else pc.r:=zr;
        If zg<0 then pc.g:=0 else if zg>$FF then pc.g:=$FF else pc.g:=zg;
        If zb<0 then pc.b:=0 else if zb>$FF then pc.b:=$FF else pc.b:=zb;
      end;
      Inc(pw);
      Inc(PByte(pc), dBytesPP);
    end;
    Inc(ph, FilterLen + 1);
  end;
  end;

  // special proc for kernel size = 5 (rad = 2)
  procedure xMulPix_MMX_5(Src : PFColorA; Dst : PFColorA);
  asm
  db $0F,$EF,$ED           /// pxor mm5, mm5
  db $0F,$EF,$F6           /// pxor mm6, mm6
  db $0F,$EF,$FF           /// pxor mm7, mm7

  db $0F,$6F,$00           /// movq mm0, [eax]
  db $0F,$6F,$48,$08       /// movq mm1, [eax+8]
  db $0F,$6F,$D0           /// movq mm2, mm0
  db $0F,$6F,$D9           /// movq mm3, mm1
  db $0F,$60,$C6           /// punpcklbw mm0, mm6
  db $0F,$60,$CF           /// punpcklbw mm1, mm7
  db $0F,$70,$D2,$AA       /// pshufw mm2, mm2, 170
  db $0F,$70,$DB,$AA       /// pshufw mm3, mm3, 170
  db $0F,$71,$F0,$07       /// psllw mm0, 7
  db $0F,$71,$F1,$07       /// psllw mm1, 7
  db $0F,$E5,$C2           /// pmulhw mm0, mm2
  db $0F,$E5,$CB           /// pmulhw mm1, mm3
  db $0F,$ED,$E8           /// paddsw mm5, mm0
  db $0F,$ED,$E9           /// paddsw mm5, mm1

  db $0F,$6F,$40,$10       /// movq mm0, [eax+16]
  db $0F,$6F,$48,$18       /// movq mm1, [eax+24]
  db $0F,$6F,$D0           /// movq mm2, mm0
  db $0F,$6F,$D9           /// movq mm3, mm1
  db $0F,$60,$C6           /// punpcklbw mm0, mm6
  db $0F,$60,$CF           /// punpcklbw mm1, mm7
  db $0F,$70,$D2,$AA       /// pshufw mm2, mm2, 170
  db $0F,$70,$DB,$AA       /// pshufw mm3, mm3, 170
  db $0F,$71,$F0,$07       /// psllw mm0, 7
  db $0F,$71,$F1,$07       /// psllw mm1, 7
  db $0F,$E5,$C2           /// pmulhw mm0, mm2
  db $0F,$E5,$CB           /// pmulhw mm1, mm3
  db $0F,$ED,$E8           /// paddsw mm5, mm0
  db $0F,$ED,$E9           /// paddsw mm5, mm1

  db $0F,$6F,$40,$20       /// movq mm0, [eax+32]
  db $0F,$6F,$D0           /// movq mm2, mm0
  db $0F,$60,$C6           /// punpcklbw mm0, mm6
  db $0F,$70,$D2,$AA       /// pshufw mm2, mm2, 170
  db $0F,$71,$F0,$07       /// psllw mm0, 7
  db $0F,$E5,$C2           /// pmulhw mm0, mm2
  db $0F,$ED,$E8           /// paddsw mm5, mm0

  db $0F,$70,$40,$28,$00   /// pshufw mm0, [eax+40], 0
  db $0F,$ED,$E8           /// paddsw mm5, mm0

  db $0F,$71,$E5,$05       /// psraw mm5, 5
  db $0F,$67,$EE           /// packuswb mm5, mm6
  db $0F,$7E,$2A           /// movd [edx], mm5
  end;

  // processing 2 pixels at cycle step
  procedure xMulPix_MMX_2(Src : PFColorA; Dst : PFColorA; FilterLen : Integer);
  asm
  db $0F,$EF,$ED           /// pxor mm5, mm5
  db $0F,$EF,$F6           /// pxor mm6, mm6
  db $0F,$EF,$FF           /// pxor mm7, mm7

  @cycle:
    db $0F,$6F,$00           /// movq mm0, [eax]
    db $0F,$6F,$48,$08       /// movq mm1, [eax+8]
    db $0F,$6F,$D0           /// movq mm2, mm0
    db $0F,$6F,$D9           /// movq mm3, mm1
    db $0F,$60,$C6           /// punpcklbw mm0, mm6
    db $0F,$60,$CF           /// punpcklbw mm1, mm7
    db $0F,$70,$D2,$AA       /// pshufw mm2, mm2, 170
    db $0F,$70,$DB,$AA       /// pshufw mm3, mm3, 170
    db $0F,$71,$F0,$07       /// psllw mm0, 7
    db $0F,$71,$F1,$07       /// psllw mm1, 7
    db $0F,$E5,$C2           /// pmulhw mm0, mm2
    db $0F,$E5,$CB           /// pmulhw mm1, mm3
    db $0F,$ED,$E8           /// paddsw mm5, mm0
    db $0F,$ED,$E9           /// paddsw mm5, mm1
    add eax, 16
    dec ecx
  jnz @cycle

  db $0F,$70,$00,$00       /// pshufw mm0, [eax], 0
  db $0F,$ED,$E8           /// paddsw mm5, mm0

  db $0F,$71,$E5,$05       /// psraw mm5, 5
  db $0F,$67,$EE           /// packuswb mm5, mm6
  db $0F,$7E,$2A           /// movd [edx], mm5
  end;


  procedure xMulPix_MMX(Src : PFColorA; Dst : PFColorA; FilterLen : Integer);
  asm
  db $0F,$EF,$F6           /// pxor mm6, mm6
  db $0F,$EF,$C9           /// pxor mm1, mm1

  @cycle:
    //mm0->pixels
{
    db $0F,$6E,$00           /// movd mm0, [eax]
    db $0F,$60,$C6           /// punpcklbw mm0, mm6
    // mm2->w = -32768..32767 (при ContShift = 15)
    db $0F,$70,$50,$04,$00   /// pshufw mm2, [eax + 4], 0
    // pshufw reg, [mem + X] - "плоха€" команда?
}

    db $0F,$6F,$00           /// movq mm0, [eax]
    db $0F,$6F,$D0           /// movq mm2, mm0
    db $0F,$60,$C6           /// punpcklbw mm0, mm6
    db $0F,$70,$D2,$AA       /// pshufw mm2, mm2, 170
    // приводим пиксели к диапазону 0..32767, вернее, 0..255*128 = 0..32640
    db $0F,$71,$F0,$07       /// psllw mm0, 7

{
    db $0F,$6F,$18           /// movq mm3, [eax]
    db $0F,$60,$C3           /// punpcklbw mm0, mm3
    db $0F,$70,$D3,$AA       /// pshufw mm2, mm3, 170
    db $0F,$71,$D0,$01       /// psrlw mm0, 1
}
    // умножаем 32768*32768, берЄм старшие 16 бит,
    // в результате имеем 128*128
    // точнее (16383..16383)*32640 -> -8159..8159
    db $0F,$E5,$C2           /// pmulhw mm0, mm2
    db $0F,$ED,$C8           /// paddsw mm1, mm0

    add eax, 8
    dec ecx
  jnz @cycle

  db $0F,$70,$00,$00       /// pshufw mm0, [eax], 0 // w
//  db $0F,$71,$D0,$0A       /// psrlw mm0, 10
  db $0F,$ED,$C8           /// paddsw mm1, mm0

  // 8159 / 32 = 254
  db $0F,$71,$E1,$05       /// psraw mm1, 5 // к диапазону 0..255
  db $0F,$67,$CE           /// packuswb mm1, mm6
  db $0F,$7E,$0A           /// movd [edx], mm1
  end;


  //24,32 -> 24,32
  // !!при выборке из исх. данных может вылезать за границы - вроде исправлено
  procedure DoFilter24A(Src, Dst : TFastDIB; FilterLen : Integer;
                        Cont : PContributor; Horisontal : Boolean);
  Const
    UseSpcProcForKernel5 = True;
//    UseSpcProcForKernel5 = False;
    UseDoublePixProc = True;
//    UseDoublePixProc = False;
  Var
    i : NativeInt;
    i2 : Integer;
    CBuf : array of TFColorA;
    BLine : PLine32;
    SLine : PLine24;
    x, y, dBytesPP, flc, flDiff : Integer;
    dc : PByte;
    c: PFColorA;
    pw, ph : PContributor;
    SrcLastRow, DstLastRow : Boolean;
  begin
//  BLine := xAllocAligned(CBuf, (FilterLen+1)*2 + 4);
  SetLength(CBuf, (FilterLen+1)*2 + 4);
  BLine := @CBuf[0]; i := NativeInt(BLine);
  // align to 8
  While ( ((i shr 3) shl 3) <> i ) do
    Inc(i, 4);
  BLine := PLine32(i);

  dBytesPP := Dst.Bpp shr 3;

  ph := Cont; SrcLastRow := False; DstLastRow := False;
  flc := FilterLen shr 1; If (flc shl 1) <> FilterLen then Inc(flc);
  flDiff := ((flc shl 1) - FilterLen) * 2;

  For y:=0 to Dst.AbsHeight-1 do begin
//    If (y = Dst.AbsHeight-1) and (dBytesPP = 3) then LastRow := True;
    If (y = Dst.AbsHeight-1) then begin
      If (Src.Bpp = 24) then SrcLastRow := True;
      If (dBytesPP = 3) then DstLastRow := True;
    end;
    pw := Cont;
    If Horisontal then SLine := Src.Scanlines[y];
    dc := Dst.Scanlines[y];

    If (Src.Bpp = 24) then
      For x := 0 to Dst.Width-1 do begin
        c := @BLine[0];
        If Horisontal then begin
          If SrcLastRow then
            For i := 0 to FilterLen - 1 do begin
              c.c := SLine[pw.pos]; Inc(c);
              c.i := pw.w; Inc(c); Inc(pw);
            end
          else // 1 move instead of 2 (although unaligned)
            For i := 0 to FilterLen - 1 do begin
              c^ := PFColorA(@SLine[pw.pos])^; Inc(c);
              c.i := pw.w; Inc(c); Inc(pw);
            end;
        end else begin
          pw := ph;
          For i := 0 to FilterLen - 1 do begin
            c.c := Src.Pixels24[pw.pos, x]; Inc(c);
            c.i := pw.w; Inc(c);
            Inc(pw);
          end;
        end;
        c.i := pw.w shr 10;
        If (DstLastRow) and (x = Dst.Width-1) then begin
          xMulPix_MMX(@BLine[0], @i2, FilterLen);
          PFColor(dc)^ := PFColor(@i2)^;
        end else
          If (UseSpcProcForKernel5) and (FilterLen = 5) then
            xMulPix_MMX_5(@BLine[0], PFColorA(dc))
          else
            If (UseDoublePixProc) and (FilterLen >= 4) then begin
              Inc(c, flDiff); c.i := pw.w shr 10;
              xMulPix_MMX_2(@BLine[0], PFColorA(dc), flc);
            end else
              xMulPix_MMX(@BLine[0], PFColorA(dc), FilterLen);

        Inc(pw); Inc(dc, dBytesPP);
      end
    else
      For x := 0 to Dst.Width-1 do begin
        c := @BLine[0];
        If Horisontal then begin
          For i := 0 to FilterLen - 1 do begin
            c^ := PLine32(SLine)[pw.pos]; Inc(c);
            c.i := pw.w; Inc(c); Inc(pw);
          end;
        end else begin
          pw := ph;
          For i := 0 to FilterLen - 1 do begin
            c^ := Src.Pixels32[pw.pos, x]; Inc(c);
            c.i := pw.w; Inc(c); Inc(pw);
          end;
        end;
        c.i := pw.w shr 10;

        // на случай 32->24 - но немного тормозит
        // может, ну его нафиг, этот случай?
        If (DstLastRow) and (x = Dst.Width-1) then begin
          xMulPix_MMX(@BLine[0], @i2, FilterLen);
          PFColor(dc)^ := PFColor(@i2)^;
        end else
          If (UseSpcProcForKernel5) and (FilterLen = 5) then
            xMulPix_MMX_5(@BLine[0], PFColorA(dc))
          else
            If (UseDoublePixProc) and (FilterLen >= 4) then
              xMulPix_MMX_2(@BLine[0], PFColorA(dc), flc)
            else
              xMulPix_MMX(@BLine[0], PFColorA(dc), FilterLen);

        Inc(pw); Inc(dc, dBytesPP);
      end;

    Inc(ph, FilterLen + 1);
  end;

  EMMS;
  end;

var
  Bmp : TFastDIB;
  sx, sy : TFloatType;
  w : PContributor;
  c : Integer;
  SameBpp, IntBufBpp : Integer;
begin
If (Src.Width = 0) or (Src.Height = 0) or
   (Src.Bpp < 8) or (Dst.Bpp < 8) or
   ((Src.Bpp > 8) and (Dst.Bpp = 8)) then Exit;

If (Dst.Width = Src.Width) and (Dst.Height = Src.Height) then begin
  Src.Draw(Dst.hDC, 0, 0); Exit;
end;

{$IFNDEF CPUX86} UseMMX := False; {$ENDIF}

//If (Src.Bpp = 16) then Lut16 := GenLut16(Src);
//If (Dst.Bpp = 16) then BLut16 := GenBackLut16(Dst);
If (Src.Bpp = 16) then Lut16 := Src.GenLut16;
If (Dst.Bpp = 16) then BLut16 := Dst.GenBackLut16;

SameBpp := 0;
If (Src.Bpp in [24, 32]) and (Dst.Bpp in [24, 32]) then begin

  If (ForceOneCont) and (ContShift = 14) and (UseMMX) and
     ((cfSSE in CPUInfo.Features) or (cfMMX2 in CPUInfo.Features)) then
    SameBpp := 32
  else

    If (Src.Bpp = 24) then SameBpp := 24;
end else
  If (Src.Bpp = 8) and (Dst.Bpp = 8) then SameBpp := 8;

IntBufBpp := SameBpp;
If (IntBufBpp = 0) then IntBufBpp := 24;

sx := Dst.Width / Src.Width; sy := Dst.AbsHeight / Src.AbsHeight;
w := nil;

If (sx <> 1) then begin
  If (sy <> 1) then begin

    Bmp := TFastDIB.Create;
    If (Buf = nil) then
      Bmp.SetSize(Dst.Width, Src.Height, IntBufBpp)
    else begin
      GrowSize(Buf, Dst.Width, Src.Height, IntBufBpp);
      Bmp.SetSubset(Buf, 0, 0, Dst.Width, Src.Height);
    end;

  end else
    Bmp := Dst;

  c := PreCalcEx2(w, Dst.Width, Src.Width, sx, FilterRads[Filter]);
  Case SameBpp of
    8  : DoFilter8(Src, Bmp, c, w, True);
    24 : DoFilter24(Src, Bmp, c, w, True);
    32 : DoFilter24A(Src, Bmp, c, w, True);
    else DoFilter(Src, Bmp, c, w, True);
  end;
//  Bmp.SaveToFile('aaa.bmp');
end else
  Bmp := Src;

If (sy <> 1) then begin
  c := PreCalcEx2(w, Dst.AbsHeight, Src.AbsHeight, sy, FilterRads[Filter]);
  Case SameBpp of
    8  : DoFilter8(Bmp, Dst, c, w, False);
    24 : DoFilter24(Bmp, Dst, c, w, False);
    32 : DoFilter24A(Bmp, Dst, c, w, False)
    else DoFilter(Bmp, Dst, c, w, False);
  end;
end;

FreeMem(w);
If (Bmp <> Src) and (Bmp <> Dst) then Bmp.Free;
end;


procedure SmoothResize8(Src,Dst:TFastDIB;Filter:TResizeFilter);
begin
SmoothResize(Src, Dst, Filter);
end;

procedure SmoothResize24(Src,Dst:TFastDIB;Filter:TResizeFilter);
begin
SmoothResize(Src, Dst, Filter);
end;

procedure SmoothResize32(Src,Dst:TFastDIB;Filter:TResizeFilter);
begin
SmoothResize(Src, Dst, Filter);
end;


// *************************** Projective **************************************

{$IFDEF CPUX64}
  {$EXCESSPRECISION OFF}
{$ENDIF}

function Projective(Src, Dst : TFastDIB; Pts : PRPointArr; Smooth : Boolean = False;
                    PtsOnDst : Boolean = True): Boolean;

Const
  ClearBk = False;
//  ClearBk = True;

type
  TFloatMatrix = array[0..2, 0..2] of Single;  // 3x3 single precision
  TFloatVec = array[0..2] of Single;

  TFloatQuad = array[0..3] of Single;
  PQuadVec = ^TQuadVec;
  TQuadVec = array[0..2] of TFloatQuad;
const
  IdentityMatrix: TFloatMatrix = (
    (1, 0, 0),
    (0, 1, 0),
    (0, 0, 1));

  Shift = 16;
  MVal = (1 shl Shift);
  AVal = MVal - 1;
  Half = MVal div 2;

Var
  w, h : Integer;

  function _DET(a1, a2, b1, b2: Single): Single; overload;
  begin
    Result := a1 * b2 - a2 * b1;
  end;

  function _DET(a1, a2, a3, b1, b2, b3, c1, c2, c3: Single): Single; overload;
  begin
    Result :=
      a1 * (b2 * c3 - b3 * c2) -
      b1 * (a2 * c3 - a3 * c2) +
      c1 * (a2 * b3 - a3 * b2);
  end;

  procedure Adjoint(var M: TFloatMatrix);
  var
    a1, a2, a3: Single;
    b1, b2, b3: Single;
    c1, c2, c3: Single;
  begin
    a1 := M[0,0]; a2:= M[0,1]; a3 := M[0,2];
    b1 := M[1,0]; b2:= M[1,1]; b3 := M[1,2];
    c1 := M[2,0]; c2:= M[2,1]; c3 := M[2,2];

    M[0,0]:= _DET(b2, b3, c2, c3);
    M[0,1]:=-_DET(a2, a3, c2, c3);
    M[0,2]:= _DET(a2, a3, b2, b3);

    M[1,0]:=-_DET(b1, b3, c1, c3);
    M[1,1]:= _DET(a1, a3, c1, c3);
    M[1,2]:=-_DET(a1, a3, b1, b3);

    M[2,0]:= _DET(b1, b2, c1, c2);
    M[2,1]:=-_DET(a1, a2, c1, c2);
    M[2,2]:= _DET(a1, a2, b1, b2);
  end;

  function Determinant(const M: TFloatMatrix): Single;
  begin
    Result := _DET(M[0,0], M[1,0], M[2,0],
                   M[0,1], M[1,1], M[2,1],
                   M[0,2], M[1,2], M[2,2]);
  end;

  procedure Scale(var M: TFloatMatrix; Factor: Single);
  var
    i, j: Integer;
  begin
    for i := 0 to 2 do
      for j := 0 to 2 do
        M[i,j] := M[i,j] * Factor;
  end;

  function Invert(var M: TFloatMatrix): Boolean;
  var
    Det: Single;
  begin
    Det := Determinant(M);
    Result := (Abs(Det) >= 1E-5);
    If Result then begin
      Adjoint(M);
      Scale(M, 1 / Det);
    end else
      M := IdentityMatrix;
  end;

  function Mult(const M1, M2: TFloatMatrix): TFloatMatrix;
  var
    i, j: Integer;
  begin
    for i := 0 to 2 do
      for j := 0 to 2 do
        Result[i, j] :=
          M1[0, j] * M2[i, 0] +
          M1[1, j] * M2[i, 1] +
          M1[2, j] * M2[i, 2];
  end;


  function PrepMatrix(Const SrcRect, DstRect : TRect; Pts : {PPointArr;} PRPointArr;
                      Var M : TFloatMatrix): Boolean;
  var
    Wx0, Wx1, Wx2, Wx3: Single;
    Wy0, Wy1, Wy2, Wy3: Single;
    dx1, dx2, px, dy1, dy2, py: Single;
    g, h, k: Single;
    R: TFloatMatrix;
    dh : Integer;
  begin
  Wx0 := Pts[0].x; Wx1 := Pts[1].x; Wx2 := Pts[2].x; Wx3 := Pts[3].x;
  Wy0 := Pts[0].y; Wy1 := Pts[1].y; Wy2 := Pts[2].y; Wy3 := Pts[3].y;

  If Dst.Height > 0 then begin
    dh := DstRect.Bottom - DstRect.Top;
    Wy0 := dh - Wy0; Wy1 := dh - Wy1; Wy2 := dh - Wy2; Wy3 := dh - Wy3;
  end;

  px  := Wx0 - Wx1 + Wx2 - Wx3;
  py  := Wy0 - Wy1 + Wy2 - Wy3;

  if (px = 0) and (py = 0) then
  begin
    // affine mapping
    M[0,0] := Wx1 - Wx0;
    M[1,0] := Wx2 - Wx1;
    M[2,0] := Wx0;

    M[0,1] := Wy1 - Wy0;
    M[1,1] := Wy2 - Wy1;
    M[2,1] := Wy0;

    M[0,2] := 0;
    M[1,2] := 0;
    M[2,2] := 1;
  end
  else
  begin
    // projective mapping
    dx1 := Wx1 - Wx2;
    dx2 := Wx3 - Wx2;
    dy1 := Wy1 - Wy2;
    dy2 := Wy3 - Wy2;
    k := dx1 * dy2 - dx2 * dy1;
    if k <> 0 then
    begin
      g := (px * dy2 - py * dx2) / k;
      h := (dx1 * py - dy1 * px) / k;

      M[0,0] := Wx1 - Wx0 + g * Wx1;
      M[1,0] := Wx3 - Wx0 + h * Wx3;
      M[2,0] := Wx0;

      M[0,1] := Wy1 - Wy0 + g * Wy1;
      M[1,1] := Wy3 - Wy0 + h * Wy3;
      M[2,1] := Wy0;

      M[0,2] := g;
      M[1,2] := h;
      M[2,2] := 1;
    end
    else
    begin
      FillChar(M, SizeOf(M), 0);
      Result := False; Exit;
    end;
  end;

  // denormalize texture space (u, v)

  R := IdentityMatrix;
  R[0,0] := 1 / (SrcRect.Right - SrcRect.Left);
  R[1,1] := -1 / (SrcRect.Bottom - SrcRect.Top);
  M := Mult(M, R);

  R := IdentityMatrix;
  R[2,0] := -SrcRect.Left;
  R[2,1] := -(SrcRect.Bottom - SrcRect.Top);// -SrcRect.Top;
  M := Mult(M, R);

  If PtsOnDst then Result := Invert(M)
              else Result := True;
  end;

  procedure Transform(Const M : TFloatMatrix; X, Y: Single;
                      out SrcX, SrcY: Single);
  var Z: Single;
  begin
    Z := M[0,2] * X + M[1,2] * Y + M[2,2];
    if Z = 0 then Exit
    else if Z = 1 then
    begin
      SrcX := (M[0,0] * X + M[1,0] * Y + M[2,0]);
      SrcY := (M[0,1] * X + M[1,1] * Y + M[2,1]);
    end
    else
    begin
      Z := 1 / Z;
      SrcX := ((M[0,0] * X + M[1,0] * Y + M[2,0]) * Z);
      SrcY := ((M[0,1] * X + M[1,1] * Y + M[2,1]) * Z);
    end;
  end;

  procedure MulVec(Const M : TFloatMatrix; x, y: Single;
                   out dy : TFloatVec);
  begin
  dy[0] := M[0,0] * X + M[1,0] * Y + M[2,0];
  dy[1] := M[0,1] * X + M[1,1] * Y + M[2,1];
  dy[2] := M[0,2] * X + M[1,2] * Y + M[2,2];
  end;


{$IFDEF CPUX86}
  procedure xLine_GetOffs_SSE(Src, Dst : Pointer; Cnt : Integer;
                              AddVal : Pointer);
  asm
    db $0F,$28,$00           /// movaps xmm0, [eax] // fill dy[0]- dy[2]
    db $0F,$28,$48,$10       /// movaps xmm1, [eax+16]
    db $0F,$28,$50,$20       /// movaps xmm2, [eax+32]
    mov eax, AddVal

//    movaps xmm3, [eax]
    db $0F,$28,$70,$10       /// movaps xmm6, [eax+16]
    db $0F,$28,$78,$20       /// movaps xmm7, [eax+32]

  @loop:
{
    db $0F,$28,$E0           /// movaps xmm4, xmm0
    db $0F,$28,$E9           /// movaps xmm5, xmm1
    db $0F,$5E,$E2           /// divps xmm4, xmm2
    db $0F,$5E,$EA           /// divps xmm5, xmm2
}
    // Newton-Raphson approximation, x = 1/d => x*(2 - d*x)
    // xmm4  - input, xmm3  - output

    db $0F,$28,$E2           /// movaps xmm4, xmm2
    db $0F,$53,$DC           /// rcpps xmm3, xmm4
    db $0F,$59,$E3           /// mulps xmm4, xmm3
    db $0F,$59,$E3           /// mulps xmm4, xmm3
    db $0F,$58,$DB           /// addps xmm3, xmm3
    db $0F,$5C,$DC           /// subps xmm3, xmm4

    // rcpps has low precision!!
//    db $0F,$53,$DA           /// rcpps xmm3, xmm2 // iz = 1 / dy[2]
    db $0F,$28,$E0           /// movaps xmm4, xmm0
    db $0F,$28,$E9           /// movaps xmm5, xmm1
    db $0F,$59,$E3           /// mulps xmm4, xmm3 //dy[0] * iz, dy[1] * iz
    db $0F,$59,$EB           /// mulps xmm5, xmm3

    db $0F,$2D,$C4           /// CVTPS2PI mm0, xmm4 //Round, lower halfs
    db $0F,$2D,$CD           /// CVTPS2PI mm1, xmm5
    db $0F,$7F,$02           /// movq [edx], mm0
    db $0F,$7F,$4A,$08       /// movq [edx+8], mm1
    db $0F,$C6,$E4,$0E       /// shufps xmm4, xmm4, 14 // 1110 - 4 and 3
    db $0F,$C6,$ED,$0E       /// shufps xmm5, xmm5, 14
    db $0F,$2D,$C4           /// CVTPS2PI mm0, xmm4 // high halfs
    db $0F,$2D,$CD           /// CVTPS2PI mm1, xmm5
    db $0F,$7F,$42,$10       /// movq [edx+16], mm0
    db $0F,$7F,$4A,$18       /// movq [edx+24], mm1
    // dy[0] := dy[0] + M[0,0]; dy[1] := dy[1] + M[0,1]; dy[2] := dy[2] + M[0,2];
    db $0F,$58,$00           /// addps xmm0, [eax]
    db $0F,$58,$CE           /// addps xmm1, xmm6
    db $0F,$58,$D7           /// addps xmm2, xmm7
    add edx, 32 //(8 * 4)
    dec ecx
  jnz @loop
  db $0F,$77               /// emms
  end;
{$ENDIF}


  procedure SetPixS(x, y, sBytesPP, dBytesPP : Integer; Dst : PFColor);//; Both32 : Boolean);
  Const SrcStep = 3;
  Var xi, xf, yi, yf, yfi, w1, w2, w3, w4 : Integer;
//      ln1, ln2 : PLine24;
      ln1, ln2 : PLine8;
  begin
  If (x >= 0) and (x < w) and (y >= 0) and (y < h) then begin
    xf := x and AVal;
    yf := y and AVal;

    w2 := ((AVal - yf) * xf) shr Shift;
    w1 := (AVal - yf) - w2;
    w4 := (yf * xf) shr Shift;
    w3 := yf - w4;
{
    ln1 := @Src.Pixels24[y shr Shift, x shr Shift];
    ln2 := ln1; Inc(PByte(ln2), Src.BWidth);
    Dst.b := (ln1[0].b * w1 + ln1[1].b * w2 + ln2[0].b * w3 + ln2[1].b * w4 + (MVal shr 1)) shr Shift;
    Dst.g := (ln1[0].g * w1 + ln1[1].g * w2 + ln2[0].g * w3 + ln2[1].g * w4 + (MVal shr 1)) shr Shift;
    Dst.r := (ln1[0].r * w1 + ln1[1].r * w2 + ln2[0].r * w3 + ln2[1].r * w4 + (MVal shr 1)) shr Shift;
}
    ln1 := Src.Scanlines[(y shr Shift)]; Inc(PByte(ln1), (x shr Shift) * sBytesPP);
    ln2 := ln1; Inc(PByte(ln2), Src.BWidth);
    Dst.b := (ln1[0] * w1 + ln1[sBytesPP]   * w2 + ln2[0] * w3 + ln2[sBytesPP]   * w4 + (MVal shr 1)) shr Shift;
    Dst.g := (ln1[1] * w1 + ln1[sBytesPP+1] * w2 + ln2[1] * w3 + ln2[sBytesPP+1] * w4 + (MVal shr 1)) shr Shift;
    Dst.r := (ln1[2] * w1 + ln1[sBytesPP+2] * w2 + ln2[2] * w3 + ln2[sBytesPP+2] * w4 + (MVal shr 1)) shr Shift;
    If (sBytesPP = 4) and (dBytesPP = 4) then
      PFColorA(Dst).a := (ln1[3] * w1 + ln1[sBytesPP+3] * w2 + ln2[3] * w3 + ln2[sBytesPP+3] * w4 + (MVal shr 1)) shr Shift;
  end else
    If ClearBk then Dst^ := tfBlack;
  end;

  // unused for now - left as more simple algo example
  procedure ProcCommon(Const m : TFloatMatrix; k : Single; sBytesPP, dBytesPP : Integer);
  Var x,y,sx,sy : Integer;
      iz : Single;
      dy : TFloatVec;
      dc : PFColor;
  begin
  for y:=0 to Dst.AbsHeight-1 do begin
    MulVec(m, 0, y, dy); dc := Dst.Scanlines[y];
    If Smooth then
      for x:=0 to Dst.Width-1 do begin
        iz := k / dy[2];
        SetPixS(Round(dy[0] * iz), Round(dy[1] * iz), sBytesPP, dBytesPP, dc);//, False);
        Inc(PByte(dc), dBytesPP);
        dy[0] := dy[0] + M[0,0]; dy[1] := dy[1] + M[0,1]; dy[2] := dy[2] + M[0,2];
      end
    else
      for x:=0 to Dst.Width-1 do begin
        iz := 1 / dy[2]; sx := Round(dy[0] * iz); sy := Round(dy[1] * iz);
        If (sx >= 0) and (sx < w) and (sy >= 0) and (sy < h) then
          dc^ := Src.Pixels24[sy, sx]
        else
          If ClearBk then dc^ := tfBlack;
        Inc(dc);
        dy[0] := dy[0] + M[0,0]; dy[1] := dy[1] + M[0,1]; dy[2] := dy[2] + M[0,2];
      end;
  end;
  end;

  procedure ProcNS(px, py : PIntArr; sBytesPP, dBytesPP : Integer; dc : PFColor);
  Var x : Integer;
      pb : PByte;
  begin
  If (sBytesPP = 4) and (dBytesPP = 4) then // 32->32
    for x:=0 to (Dst.Width shr 1)-1 do begin
      If (px[0] >= 0) and (px[0] < w) and (py[0] >= 0) and (py[0] < h) then
        PFColorA(dc)^ := Src.Pixels32[(py[0] + Half) shr Shift, (px[0] + Half) shr Shift]
      else
        If ClearBk then PFColorA(dc).i := 0;
      Inc(PByte(dc), 4);
      If (px[1] >= 0) and (px[1] < w) and (py[1] >= 0) and (py[1] < h) then
        PFColorA(dc)^ := Src.Pixels32[(py[1] + Half) shr Shift, (px[1] + Half) shr Shift]
      else
        If ClearBk then PFColorA(dc).i := 0;
      Inc(PByte(dc), 4);
      Inc(PByte(px), 16); Inc(PByte(py), 16);
    end
  else   // 24->32, 32->24
    for x:=0 to (Dst.Width shr 1)-1 do begin
      If (px[0] >= 0) and (px[0] < w) and (py[0] >= 0) and (py[0] < h) then begin
        pb := Src.Scanlines[(py[0] + Half) shr Shift];
        Inc(pb, ((px[0] + Half) shr Shift) * sBytesPP);
        dc^ := PFColor(pb)^;
      end else
        If ClearBk then dc^ := tfBlack;
      Inc(PByte(dc), dBytesPP);
      If (px[1] >= 0) and (px[1] < w) and (py[1] >= 0) and (py[1] < h) then begin
        pb := Src.Scanlines[(py[1] + Half) shr Shift];
        Inc(pb, ((px[1] + Half) shr Shift) * sBytesPP);
        dc^ := PFColor(pb)^;
      end else
        If ClearBk then dc^ := tfBlack;
      Inc(PByte(dc), dBytesPP);
      Inc(PByte(px), 16); Inc(PByte(py), 16);
    end;
  end;

  procedure xLine_GetOffs_PAS(Src : PQuadVec; Dst : PIntArr; Cnt : Integer;
                              AddVal : PQuadVec);
  Var n : Integer;
      dy, ad : TFloatVec;
      iz : Single;
  begin
  dy[0] := Src[0, 0]; dy[1] := Src[1, 0]; dy[2] := Src[2, 0];
  ad[0] := AddVal[0,0]*0.25; ad[1] := AddVal[1,0]*0.25; ad[2] := AddVal[2,0]*0.25;
  For n:=0 to Cnt-1 do begin
    iz := 1 / dy[2];
    Dst[0] := Round(dy[0] * iz); Dst[2] := Round(dy[1] * iz);
    dy[0] := dy[0] + ad[0]; dy[1] := dy[1] + ad[1]; dy[2] := dy[2] + ad[2];
    iz := 1 / dy[2];
    Dst[1] := Round(dy[0] * iz); Dst[3] := Round(dy[1] * iz);
    dy[0] := dy[0] + ad[0]; dy[1] := dy[1] + ad[1]; dy[2] := dy[2] + ad[2];
    Inc(PByte(Dst), 16);
  end;
  end;

Const
{$IFDEF CPUX86}
  UseSSE = True;
//  UseSSE = False;
{$ELSE}
  UseSSE = False;
{$ENDIF}

// todo: draw only dst quad, not all dst rect

Var x,y,i,k,sBytesPP,dBytesPP : Integer;
    dc : PFColor;
    m : TFloatMatrix;
    dy : TFloatVec;
    a : DByteArr;
    pa : PByteArr;
    pBase, pAdd : PQuadVec;
    px, py : PIntArr;
begin
w := Src.Width-1; h := Src.AbsHeight-1;
If not PrepMatrix(Rect(0, 0, w, h), Rect(0, 0, Dst.Width-1, Dst.AbsHeight-1),
                  Pts, m) then
begin
  Result := False; Exit;
end;

//If (m[1, 2] <= 1E-5) then asm int 3; end;

//If Smooth then begin
  k := MVal; w := w * MVal; h := h * MVal;
//end else
//  k := 1;

sBytesPP := Src.Bpp shr 3; dBytesPP := Dst.Bpp shr 3;

// aligned to 16 memory block
pa := Arr_Aligned(a, SizeOf(TQuadVec)*2 + (Dst.Width+4) * SizeOf(TPoint), 4 );
pBase := @pa[0]; pAdd := @pa[SizeOf(TQuadVec)];
Inc(PByte(pa), SizeOf(TQuadVec)*2);
For i:=0 to 2 do
  For x := 0 to 3 do
    If i = 2 then pAdd[i, x] := (M[0,i] * 4 / k)
             else pAdd[i, x] := (M[0,i] * 4);

for y:=0 to Dst.AbsHeight-1 do begin
  For x:=0 to 3 do begin
    MulVec(m, x, y, dy);
    For i:=0 to 1 do pBase[i, x] := dy[i];
    pBase[2, x] := dy[2] / k;
  end;
{$IFDEF CPUX86}
  If (UseSSE) and (cfSSE in CPUInfo.Features) then
    xLine_GetOffs_SSE(pBase, @pa[0], (Dst.Width shr 2)+1, pAdd)
  else
{$ENDIF}
    xLine_GetOffs_PAS(pBase, @pa[0], (Dst.Width shr 1)+1, pAdd);

  px := @pa[0]; py := @pa[8]; dc := Dst.Scanlines[y];
  If Smooth then
    for x:=0 to (Dst.Width shr 1)-1 do begin
      SetPixS(px[0], py[0], sBytesPP, dBytesPP, dc); Inc(PByte(dc), dBytesPP);
      SetPixS(px[1], py[1], sBytesPP, dBytesPP, dc); Inc(PByte(dc), dBytesPP);
      Inc(PByte(px), 16); Inc(PByte(py), 16);
    end
  else
    If (sBytesPP = 3) and (dBytesPP = 3) then
      for x:=0 to (Dst.Width shr 1)-1 do begin
        If (px[0] >= 0) and (px[0] < w) and (py[0] >= 0) and (py[0] < h) then
//          dc^ := Src.Pixels24[py[0], px[0]]
          dc^ := Src.Pixels24[((py[0] + Half) shr Shift), ((px[0] + Half) shr Shift)]
        else
          If ClearBk then dc^ := tfBlack;
        Inc(dc);
        If (px[1] >= 0) and (px[1] < w) and (py[1] >= 0) and (py[1] < h) then
//          dc^ := Src.Pixels24[py[1], px[1]]
          dc^ := Src.Pixels24[((py[1] + Half) shr Shift), ((px[1] + Half) shr Shift)]
        else
          If ClearBk then dc^ := tfBlack;
        Inc(dc);
        Inc(PByte(px), 16); Inc(PByte(py), 16);
      end
      else
        ProcNS(px, py, sBytesPP, dBytesPP, dc);
end;
Result := True;
end;


end.
