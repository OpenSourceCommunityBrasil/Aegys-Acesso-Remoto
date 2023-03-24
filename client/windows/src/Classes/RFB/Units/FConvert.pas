unit FConvert;   // FastDIB: sourceforge.net/projects/tfastdib
                 // by: gordy <gordy@dynamicsdirect.com>
interface        //     cmr.Pent <cmr.Pent@gmail.com>
                 //     Sapersky <sapersky@gmail.com>

{  Routines for conversion between different color depths.  }

{
History:
11.03.13, Sapersky:
  Correct quantization and dithering when converting to lower bpps
  (ConvertEx, Quantize)
  x64 ready
}

Uses Windows, FastDIB;

{$I platform.inc}

procedure Convert(Src,Dst:TFastDIB; Dither16Bpp : Boolean = False);
procedure ConvertTo(Bmp:TFastDIB;Bpp:Integer);
procedure ConvertToEx(Bmp:TFastDIB;Bpp:Integer;Bpr,Bpg,Bpb:Byte);

// makes correct quantization and dithering when converting to lower bpps
procedure ConvertEx(Src, Dst : TFastDIB; Dithering : Boolean = False;
                    DstColorCnt : Integer = 0);

// subset of most usable conversions: (8 || 16 || 24 || 32) -> (16 || 24 || 32)
// with optional Lut (Gamma correction etc.) and VFlip
// (2 times faster than Convert + separate Lut operation)
// also has smaller code size than Convert
// for 32 bpp source image must have normal color order (BGRA)
procedure ConvertRGBLut(Src, Dst : TFastDIB; Lut : PLine8 = nil; VFlip : Boolean = False);
// the same for bk. compability
procedure ConvertRGB(Src, Dst : TFastDIB; Lut : PLine8 = nil; VFlip : Boolean = False);

// (8+8A || 32) -> (16 || 32) [with alpha]
// mostly for conversion to DX textures formats (R4G4B4A4, R5G5B5A1, R8G8B8A8)
function ConvertAlpha(Src, SrcA, Dst : TFastDIB; DstAShr, DstAShl : Byte): Boolean;

// low-level func for quantization/dithering
// Src can be 8, 24, 32 bpp, Dst - 8, 24, 32 or nil (result will be written at Src)
// if DstColorDisp <> nil, color counts for each palette entry writed there
function Quantize(Src, Dst : TFastDIB; ColorCnt : Integer = 256;
                  ColorBits : Integer = 6; Dithering : Boolean = False;
                  DstColorDisp : PInteger = nil): Integer;

procedure To1_4(Src,Dst:TFastDIB);
procedure To1_8(Src,Dst:TFastDIB);
procedure To1_16(Src,Dst:TFastDIB);
procedure To1_24(Src,Dst:TFastDIB);
procedure To1_32(Src,Dst:TFastDIB);

procedure To4_1(Src,Dst:TFastDIB);
procedure To4_8(Src,Dst:TFastDIB);
procedure To4_16(Src,Dst:TFastDIB);
procedure To4_24(Src,Dst:TFastDIB);
procedure To4_32(Src,Dst:TFastDIB);

procedure To8_1(Src,Dst:TFastDIB);
procedure To8_4(Src,Dst:TFastDIB);
procedure To8_16(Src,Dst:TFastDIB);
procedure To8_24(Src,Dst:TFastDIB);
procedure To8_32(Src,Dst:TFastDIB);

procedure To16_1(Src,Dst:TFastDIB);
procedure To16_4(Src,Dst:TFastDIB);
procedure To16_8(Src,Dst:TFastDIB);
procedure To16_16(Src,Dst:TFastDIB);
procedure To16_24(Src,Dst:TFastDIB; Dither : Boolean = False);
procedure To16_32(Src,Dst:TFastDIB);

procedure To24_1(Src,Dst:TFastDIB);
procedure To24_4(Src,Dst:TFastDIB);
procedure To24_8(Src,Dst:TFastDIB);
procedure To24_16(Src,Dst:TFastDIB);
procedure To24_32(Src,Dst:TFastDIB);

procedure To32_1(Src,Dst:TFastDIB);
procedure To32_4(Src,Dst:TFastDIB);
procedure To32_8(Src,Dst:TFastDIB);
procedure To32_16(Src,Dst:TFastDIB);
procedure To32_24(Src,Dst:TFastDIB);
procedure To32_32(Src,Dst:TFastDIB);


implementation
Type
  TConvertLineProc = procedure(Src,Dst:Pointer;Size:Integer);

function xConvSameBpp(Src, Dst : TFastDIB): Boolean;
var y: Integer;
begin
Result := (not (Src.Bpp in [16, 32])) or
          ( (Dst.BMask = Src.BMask) and (Dst.GMask = Src.GMask) and
            (Dst.RMask = Src.RMask) );
If Result then begin
  For y:=0 to Src.AbsHeight-1 do
    Move(Src.Scanlines[y]^, Dst.Scanlines[y]^, Dst.BWidth-Dst.Gap);
  If (Dst.Bpp <= 8) then CopyPal(Src, Dst);
end;
end;

// can be used for: any bpp -> 1; 1,4 -> any bpp; 8 -> 4
procedure xConvGDI(Src, Dst : TFastDIB);
begin
If (Dst.hDC = 0) then Exit;
If (Dst.Bpp <= 8) then CopyPal(Src, Dst);
Src.Draw(Dst.hDC, 0, 0);
end;

procedure xGetMinSize(Src, Dst : TFastDIB; out w, h : Integer);
begin
If Src.Width < Dst.Width then w := Src.Width - 1
                         else w := Dst.Width - 1;
If Src.AbsHeight < Dst.AbsHeight then h := Src.AbsHeight - 1
                                 else h := Dst.AbsHeight - 1;
end;


procedure Convert(Src,Dst:TFastDIB; Dither16Bpp : Boolean = False);
begin
  If (Src.Bpp = Dst.Bpp) and (xConvSameBpp(Src, Dst)) then Exit;

  case Dst.Bpp of
    1:
    case Src.Bpp of
      4:  To1_4(Src,Dst);
      8:  To1_8(Src,Dst);
      16: To1_16(Src,Dst);
      24: To1_24(Src,Dst);
      32: To1_32(Src,Dst);
    end;
    4:
    case Src.Bpp of
      1:  To4_1(Src,Dst);
      8:  To4_8(Src,Dst);
      16: To4_16(Src,Dst);
      24: To4_24(Src,Dst);
      32: To4_32(Src,Dst);
    end;
    8:
    case Src.Bpp of
      1:  To8_1(Src,Dst);
      4:  To8_4(Src,Dst);
      16: To8_16(Src,Dst);
      24: To8_24(Src,Dst);
      32: To8_32(Src,Dst);
    end;
    16:
    case Src.Bpp of
      1:  To16_1(Src,Dst);
      4:  To16_4(Src,Dst);
      8:  To16_8(Src,Dst);
      16: To16_16(Src,Dst);
      24, 32: To16_24(Src, Dst, Dither16Bpp);
    end;
    24:
    case Src.Bpp of
      1:  To24_1(Src,Dst);
      4:  To24_4(Src,Dst);
      8:  To24_8(Src,Dst);
      16: To24_16(Src,Dst);
      32: To24_32(Src,Dst);

    end;
    32:
    case Src.Bpp of
      1:  To32_1(Src,Dst);
      4:  To32_4(Src,Dst);
      8:  To32_8(Src,Dst);
      16: To32_16(Src,Dst);
      24: To32_24(Src,Dst);
      32: To32_32(Src,Dst);
    end;
  end;
end;

procedure ConvertTo(Bmp:TFastDIB;Bpp:Integer);
var
  Tmp: TFastDIB;
begin
  if Bmp.Bpp<>Bpp then
  begin
    Tmp:=TFastDIB.Create;
    Tmp.SetSize(Bmp.Width,Bmp.Height,Bpp);
    Convert(Bmp,Tmp);
    Bmp.Assign(Tmp);
  end;
end;

procedure ConvertToEx(Bmp:TFastDIB;Bpp:Integer;Bpr,Bpg,Bpb:Byte);
var
  Tmp: TFastDIB;
begin
  if(Bmp.Bpp<>Bpp)or(Bmp.Bpr<>Bpr)or(Bmp.Bpg<>Bpg)or(Bmp.Bpb<>Bpb)then
  begin
    Tmp:=TFastDIB.Create;
    Tmp.SetSizeEx(Bmp.Width,Bmp.Height,Bpp,Bpr,Bpg,Bpb);
    Convert(Bmp,Tmp);
    Bmp.Assign(Tmp);
  end;
end;


procedure ConvertEx(Src, Dst : TFastDIB; Dithering : Boolean = False;
                    DstColorCnt : Integer = 0);
Const CanQuantBpp = [8, 24, 32];

  function ClosestMax(SrcBpp : Integer): Integer;
  Var n : Integer;
  begin
  Result := 0;
  for n := 4 to 32 do
    If (n in CanQuantBpp) and (n > SrcBpp) then begin Result := n; Exit; end;
  end;

Var fSrc, fDst : TFastDIB;
    MaxColorCnt : Integer;
    UseGDI, NeedQuant : Boolean;
begin
If (Src.Bpp = Dst.Bpp) and (DstColorCnt = 0) and (xConvSameBpp(Src, Dst)) then Exit;

if (Dst.Bpp = 32) then MaxColorCnt := $7FFFFFFF
                  else MaxColorCnt := 1 shl Dst.Bpp;

NeedQuant := ((Dst.Bpp <= 8) and (Dst.Bpp < Src.Bpp)) or
             ((DstColorCnt <> 0) and (DstColorCnt < MaxColorCnt));

fSrc := Src; fDst := Dst;
If NeedQuant then begin
  if not (Src.Bpp in CanQuantBpp) then begin
    fSrc := TFastDIB.Create(Src, False, ClosestMax(Src.Bpp));
    Convert(Src, fSrc);
  end;
  if not (Dst.Bpp in CanQuantBpp) then
    fDst := TFastDIB.Create(Dst, False, ClosestMax(Dst.Bpp));

  if (DstColorCnt = 0) then DstColorCnt := MaxColorCnt;
  Quantize(fSrc, fDst, DstColorCnt, 6, Dithering);
  if (fDst <> Dst) then begin
    Convert(fDst, Dst, Dithering);
    fDst.Free;
  end;
  If (fSrc <> Src) then fSrc.Free;
end else
  Convert(Src, Dst, Dithering);
end;


// ****************************** ConvertRGBLut ********************************

procedure xLine_FastConv16(Src : PFColor; Dst : PWord; Count, sBytes : Integer;
                           SLut : PLine8; BLut16 : PBackLut16);
var
  x : Integer;
  pc : PFColorA;
begin
Case sBytes of
  1 : For x:=0 to Count-1 do begin
        pc := @PFColorTable(SLut)[ PByte(Src)^ ];
        With BLut16^ do
          Dst^ := bi[pc.b] or gi[pc.g] or ri[pc.r];
        Inc(PByte(Src)); Inc(Dst);
      end;
  2 : For x:=0 to Count-1 do begin
        pc := @PLut16(SLut)[ PWord(Src)^ ];
        With BLut16^ do
          Dst^ := bi[pc.b] or gi[pc.g] or ri[pc.r];
        Inc(PByte(Src), 2); Inc(Dst);
      end;
  3, 4 :
    For x:=0 to Count-1 do begin
      With BLut16^ do
//        Dst^ := bi[SLut[Src.b]] or gi[SLut[Src.g]] or ri[SLut[Src.r]];
        Dst^ := bi[Src.b] or gi[Src.g] or ri[Src.r];
      Inc(PByte(Src), sBytes); Inc(Dst);
    end
end;
end;

{$IFDEF CPUX86}
procedure xLine_FastConv24(Src, Dst : PFColor; Count : Integer; Lut : PLine8;
                           sBytes, dBytes : Integer);
asm
  push ebx
  push esi
  push edi
  mov esi, eax
  mov edi, edx
  mov ebx, Lut
  mov edx, dBytes
  mov ebp, sBytes // we can use ebp, if there'll be no subsequent access to stack-passed variables
@bytes:
  movzx eax, byte ptr [esi] // eax = Src
  movzx eax, byte ptr [ebx + eax] // eax = Lut[eax]
  mov byte ptr [edi], al   // Dst.b := eax
  movzx eax, byte ptr [esi + 1]
  movzx eax, byte ptr [ebx + eax]
  mov byte ptr [edi + 1], al
  movzx eax, byte ptr [esi + 2]
  movzx eax, byte ptr [ebx + eax]
  mov byte ptr [edi + 2], al
  add esi, ebp
  add edi, edx
  dec ecx
jnz @bytes
  pop edi
  pop esi
  pop ebx
end;
{$ELSE}
procedure xLine_FastConv24(Src, Dst : PFColor; Count : Integer; Lut : PLine8;
                           sBytes, dBytes : Integer);
Var n : Integer;
begin
For n:=0 to Count-1 do begin
  Dst.b := Lut[Src.b]; Dst.g := Lut[Src.g]; Dst.r := Lut[Src.r];
  Inc(PByte(Src), sBytes); Inc(PByte(Dst), dBytes);
end;
end;
{$ENDIF}


// (8 || 16 || 24 || 32) -> (16 || 24 || 32)
// with Lut (Gamma correction etc.) and optional VFlip
// this procs assume that Dst.BShr/GShr/RShr = Src
// (i.e. the order of color components is the same)

procedure ConvertRGBLut(Src, Dst : TFastDIB; Lut : PLine8 = nil; VFlip : Boolean = False);
Const
  ApplyLut2Pal = True;
//  ApplyLut2Pal = False;
var
  PlainLut8 : array [Byte] of Byte;
  x, y, w, h, dBytes: Integer;
  sc, dc : PFColor;
  Lut16 : PLut16;
  BLut16 : PBackLut16;
  SLut : PLine8;
  Pal : PFColorTable;
  dw : DWord;
begin
If (Src.Bpp = Dst.Bpp) and (Lut = nil) and (xConvSameBpp(Src, Dst)) then Exit;

dBytes := Dst.Bpp shr 3; Pal := nil;

If (Src.BytesPP = 2) then Lut16 := Src.GenLut16(Lut) else begin
  If (Src.BytesPP = 1) then
    If ApplyLut2Pal then
      If (Lut <> nil) then begin
        GetMem(Pal, SizeOf(TFColorTable)); sc := @Src.Colors[0];
        for x:=0 to 255 do begin
          Pal[x].b := Lut[sc.b]; Pal[x].g := Lut[sc.g]; Pal[x].r := Lut[sc.r];
          Inc(PFColorA(sc));
        end;
      end else
        Pal := Src.Colors
    else
      Pal := PFColorTable(Lut)
  else // Src.Bpp = 24, 32
    If (Lut = nil) then begin
  //      For x := 0 to 255 do PlainLut8[x] := x;
      Pal := @PlainLut8; dw := $03020100;
      For x := 0 to 63 do begin Pal[x].i := dw; Inc(dw, $04040404); end;
      Pal := nil;
      Lut := @PlainLut8;
    end;
end;

If (dBytes = 2) then
  If (Src.BytesPP > 2) then begin
    BLut16 := Dst.GenBackLut16(Lut); sLut := nil;
  end else begin
    BLut16 := Dst.GenBackLut16;
    If Src.BytesPP = 1 then sLut := Lut
                       else sLut := PLine8(Lut16);
  end;

xGetMinSize(Src, Dst, w, h);

for y:=0 to h do begin
  If VFlip then sc := Src.Scanlines[(Src.AbsHeight - 1) - y]
           else sc := Src.Scanlines[y];
  dc := Dst.Scanlines[y];

  If (dBytes = 2) then
    xLine_FastConv16(sc, PWord(dc), w+1, Src.BytesPP, sLut, BLut16)
  else
    Case Src.BytesPP of
      1 : begin
        For x:=0 to w-1 do begin
          PFColorA(dc)^ := Pal[PByte(sc)^];
          Inc(PByte(sc)); Inc(PByte(dc), dBytes);
        end;
        With Pal[PByte(sc)^] do begin
          dc.b := b; dc.g := g; dc.r := r;
        end;
      end;
      2 : for x:=0 to w do begin
            dc^ := Lut16[ PWord(sc)^ ];
            Inc(PWord(sc)); Inc(PByte(dc), dBytes);
          end;
      3, 4 :
        If (dBytes = 3) or ((Dst.BShl = 0) and (Dst.GShl = 8)) then
          xLine_FastConv24(sc, dc, w+1, Lut, Src.BytesPP, dBytes)
        else
          for x:=0 to w do begin
            PDWord(dc)^ := (Lut[sc.b] shl Dst.BShl) or (Lut[sc.g] shl Dst.GShl) or
                           (Lut[sc.r] shl Dst.RShl);
            Inc(PByte(sc), Src.BytesPP); Inc(PByte(dc), 4);
          end;
     end;
end;
If (Pal <> Src.Colors) and (Pal <> PFColorTable(Lut)) then FreeMem(Pal);
end;

procedure ConvertRGB(Src, Dst : TFastDIB; Lut : PLine8 = nil; VFlip : Boolean = False);
begin
ConvertRGBLut(Src, Dst, Lut, VFlip);
end;

// ************************ Floyd - Steinberg dithering ************************

Type
  PColorError = ^TColorError;
  TColorError = record
    Case Integer of
      0 : (b, g, r : Integer);
      1 : (Data : array [0..2] of Integer)
  end;

  PCEArr = ^TCEArr;
  TCEArr = array [0..0] of TColorError;

  PErrorMetrics = ^TErrorMetrics;
  TErrorMetrics = record
    Cur : TColorError;
    ErrLn, NextErr : PCEArr;
    Width, Direction, ColorCnt : Integer;
    Pal : PFColorTable;

    Src, Dst : TFastDIB;
    SrcPtr, DstPtr : PByte;
    yLine, SrcOffs, DstOffs : Integer;

    ErrLine : array of TColorError;
  end;

procedure xErrInit(Var Err : TErrorMetrics; Src : TFastDIB;
                   Dst : TFastDIB = nil);
Var w : Integer;
begin
FillChar(Err, SizeOf(TErrorMetrics), 0);
w := Src.Width + 2;
SetLength(Err.ErrLine, w * 2);
Err.ErrLn := @Err.ErrLine[1];
Err.NextErr := @Err.ErrLine[w + 1];
Err.Width := Src.Width; Err.Direction := 1;
Err.Src := Src; Err.SrcPtr := Src.Scanlines[0];
Err.SrcOffs := Src.Bpp shr 3;
If (Dst <> nil) then begin
  Err.Dst := Dst;
  Err.DstPtr := Dst.Scanlines[0]; Err.DstOffs := Dst.Bpp shr 3;
end;
end;

function xErrGetCurPix(Var Err : TErrorMetrics; c : PFColor): TFColorA;
begin
With Err do begin
  Cur.b := c.b + ErrLn[0].b DIV 16;
  Cur.g := c.g + ErrLn[0].g DIV 16;
  Cur.r := c.r + ErrLn[0].r DIV 16;
  If Cur.b < 0 then Cur.b := 0 else If Cur.b > 255 then Cur.b := 255;
  If Cur.g < 0 then Cur.g := 0 else If Cur.g > 255 then Cur.g := 255;
  If Cur.r < 0 then Cur.r := 0 else If Cur.r > 255 then Cur.r := 255;
  With Result do begin
    b := Cur.b; g := Cur.g; r := Cur.r; a := 0;
  end;
end;
end;

procedure xErrNextPix(Var Err : TErrorMetrics; Res : PFColor);
VAR n, k1, k2, k3 : Integer;
begin
With Err do begin
  Dec(Cur.b, Res.b); Dec(Cur.g, Res.g); Dec(Cur.r, Res.r);
  For n := 0 to 2 do
    If (Cur.Data[n] <> 0) then begin
      k1 := Cur.Data[n] * 7;
      k2 := Cur.Data[n] * 3;
      k3 := Cur.Data[n] * 5;
      Inc(ErrLn[Direction].Data[n], k1);
      Inc(NextErr[-Direction].Data[n], k2);
      Inc(NextErr[0].Data[n], k3);
      Inc(NextErr[Direction].Data[n], Cur.Data[n]);// - k1 - k2 - k3);
    end;
  Inc(PColorError(ErrLn), Direction);
  Inc(PColorError(NextErr), Direction);
  Inc(SrcPtr, SrcOffs);
  If (Dst <> nil) then Inc(DstPtr, DstOffs);
end;
end;

procedure xErrNextLine(Var Err : TErrorMetrics);
Var a : PCEArr;
    c : Integer;
begin
With Err do begin
  a := ErrLn;
  ErrLn := NextErr;
  NextErr := a;
  Direction := -Direction;
//    if (Direction = 1) then NextErr := @ErrLine[Width + 2]
//                       else NextErr := @ErrLine[Width];
  c := (Width+2) * SizeOf(TColorError);
  If Direction = -1 then Dec(a, Width+1);
  FillChar(a[0], c, 0);

  Inc(PColorError(NextErr), Direction);
  Inc(PColorError(ErrLn), Direction);

  Inc(yLine);
  If (Src <> nil) then begin
    SrcOffs := - SrcOffs;
    SrcPtr := Src.Scanlines[yLine];
    If Direction = -1 then
      Inc(SrcPtr, (Width-1) * (Src.Bpp shr 3));
  end;
  If (Dst <> nil) then begin
    DstOffs := - DstOffs;
    DstPtr := Dst.Scanlines[yLine];
    If Direction = -1 then
      Inc(DstPtr, (Width-1) * (Dst.Bpp shr 3));
  end;
end;
end;


// ****************************** All -> 1 bpp *********************************

{$IFDEF CPUX86}
procedure xLineTo1_4MMX(Src,Dst:Pointer;Size:Integer);
var
  a,b: Integer; //[ebp-8]
asm
  push esi
  push edi

  mov  a,0
  mov  b,0
  mov  esi,eax
  mov  edi,edx

  db $0F,$76,$D2           /// pcmpeqd mm2,mm2   //11111111111111111111111111111111
  db $0F,$71,$D2,$0C       /// psrlw   mm2,12    //00000000000011110000000000001111
  db $0F,$71,$F2,$08       /// psllw   mm2,8     //00001111000000000000111100000000

  push ecx
  shr  ecx,3
  jz   @skip

  @quads:
    db $0F,$6F,$06           /// movq  mm0,[esi] //00020001000400030006000500080007
  @cleanup:
    db $0F,$6F,$C8           /// movq  mm1,mm0
    db $0F,$71,$D1,$03       /// psrlw mm1,3     //---0002000100040---0006000500080
    db $0F,$EB,$C8           /// por   mm1,mm0   //00020021001400430006006500580087
    db $0F,$6F,$C1           /// movq  mm0,mm1
    db $0F,$71,$F1,$0A       /// psllw mm1,10    //140043----------580087----------
    db $0F,$EB,$C1           /// por   mm0,mm1   //14004321001400435806876500580087
    db $0F,$DB,$C2           /// pand  mm0,mm2   //00004321000000000000876500000000
    db $0F,$6F,$C8           /// movq  mm1,mm0
    db $0F,$72,$F1,$14       /// pslld mm1,20    //876500000000--------------------
    db $0F,$EB,$C1           /// por   mm0,mm1   //87654321000000000000876500000000
    db $0F,$6F,$C8           /// movq  mm1,mm0
    db $0F,$72,$D0,$18       /// psrld mm0,24
    db $0F,$73,$D1,$30       /// psrlq mm1,48
    db $0F,$EB,$C8           /// por   mm1,mm0
    db $0F,$7E,$C8           /// movd  eax,mm1
    mov [edi],ax

    add esi,8
    add edi,2
    dec ecx
  jnz @quads

  cmp esi,0
  je @last

  @skip:
  pop  ecx
  and  ecx,111b
  jz   @exit
  push edi
  mov  edi,ebp
  sub  edi,8  //edi = ebp-8
  mov  edx,ecx
  rep  movsb
  sub  edi,edx
  inc  ecx //ecx = 1
  mov  esi,-8
  db $0F,$6F,$85,$F8,$FF,$FF,$FF/// movq mm0,[ebp-8]
  jmp  @cleanup

  @last:
  pop edi
  mov ecx,edx
  mov [edi],al
  shr ecx,2
  jz  @exit
  mov [edi+1],ah

  @exit:
  db $0F,$77 // emms
  pop edi
  pop esi
end;

procedure xLineTo1_4(Src,Dst:Pointer;Size:Integer);
var
  a: Integer; //[ebp-4]
asm
  push esi
  push edi

  mov  a,0
  mov  esi,eax
  mov  edi,edx
  push ecx
  shr  ecx,2
  jz   @skip

  @dwords:
    mov eax,[esi]
  @cleanup:
    mov edx,eax      //00020001000400030006000500080007
    shr edx,3        //---00020001000400030006000500080
    or  edx,eax      //00020021001400430036006500580087
    mov eax,edx
    and edx,$3000300 //00000021000000000000006500000000
    and eax,$0030003 //00000000000000430000000000000087
    shl eax,10       //0000430000000000000087----------
    or  eax,edx      //00004321000000000000876500000000
    mov edx,eax
    shr eax,20       //--------------------000043210000
    or  edx,eax      //--------------------876543210000
    shr edx,4        //------------------------87654321

    mov [edi],dl
    add esi,4
    inc edi
    dec ecx
  jnz @dwords

  cmp esi,0
  je  @exit

  @skip:
  pop  ecx
  and  ecx,11b
  jz   @exit
  push edi
  mov  edi,ebp
  sub  edi,4  //edi = ebp-4
  rep  movsb
  pop  edi
  mov  eax,[ebp-4]
  mov  esi,-4
  inc  ecx
  jmp  @cleanup

  @exit:
  pop edi
  pop esi
end;
{$ENDIF}

// asm procs assume that source 4-bpp image has only 0 or 1 pixel values
// PAS version doesn't, but it is much slower
// anyway, it is hard to make correct conversion without quantization

procedure To1_4(Src,Dst:TFastDIB);
var
  x,y,w4,Gap,i : Integer;
  pb: PByte;
  SLine : PLine8;
  bt : array[0..3] of Byte;
  Proc: TConvertLineProc;
begin
{$IFDEF CPUX86}
  if cfMMX in CPUInfo.Features then Proc := xLineTo1_4MMX
                               else Proc := xLineTo1_4;
  for y:=0 to Src.AbsHeight-1 do
    Proc(Src.Scanlines[y],Dst.Scanlines[y],Src.BWidth-Src.Gap);
{$ELSE}

  w4 := (Src.Width div 8); Gap := Src.Width - w4*8;
  for y:=0 to Src.AbsHeight-1 do begin
    SLine := Src.Scanlines[y]; pb := Dst.Scanlines[y];
    for x:=0 to w4-1 do begin
      i := PInteger(@SLine[0])^;
      PInteger(@bt[0])^ := (i shr 3) and $11111111;
      // selecting bits: 1000 -> 1 (8..15 -> 1, 0..7 -> 0)
      //PInteger(@bt[0])^ := ((i shr 3) or (i shr 2)) and $11111111;
      pb^ := (bt[3] and 15) or         ((bt[3] shr 4) shl 1) or
             ((bt[2] and 15) shl 2) or ((bt[2] shr 4) shl 3) or
             ((bt[1] and 15) shl 4) or ((bt[1] shr 4) shl 5) or
             ((bt[0] and 15) shl 6) or ((bt[0] shr 4) shl 7);
      Inc(pb); Inc(PByte(SLine), 4);
    end;
    If (Gap > 0) then begin
      bt[1] := 0;
      For x:=0 to Gap-1 do begin
        If Odd(x) then bt[0] := SLine[x] and 15
                  else bt[0] := SLine[x] shr 4;
        bt[1] := bt[1] or (bt[0] and 1) shl (7 - x);
      end;
      pb^ := bt[1];
    end;
  end;
  {$ENDIF}
  CopyPal(Src, Dst);
end;

{$IFDEF CPUX86}
procedure xLineTo1_8MMX(Src,Dst:Pointer;Size:Integer);
var
  a,b: Integer; // a:b = [ebp-8]
asm
  push esi
  push edi

  mov  a,0
  mov  b,0
  mov  esi,eax
  mov  edi,edx
  push ecx
  shr  ecx,3
  jz   @skip

  @quads:
    db $0F,$6F,$06       /// movq  mm0,[esi] //00000001000000020000000300000004
  @cleanup:
    db $0F,$6F,$C8       /// movq  mm1,mm0   //00000001000000020000000300000004
    db $0F,$71,$F1,$09   /// psllw mm1,9     //0000002---------0000004---------
    db $0F,$EB,$C1       /// por   mm0,mm1   //00000021000000020000004300000004
    db $0F,$6F,$C8       /// movq  mm1,mm0   //00000021000000020000004300000004
    db $0F,$72,$F1,$12   /// pslld mm1,18    //00004300000004------------------
    db $0F,$EB,$C1       /// por   mm0,mm1   //00004321000004020000004300000004
    db $0F,$6F,$C8       /// movq  mm1,mm0   //00004321000004020000004300000004
    db $0F,$73,$D0,$24   /// psrlq mm0,36    //----0000432100000402000000430000
    db $0F,$EB,$C8       /// por   mm1,mm0   //00008765432100000402000000430000
    db $0F,$72,$D1,$14   /// psrld mm1,20    //--------------------000087654321
    db $0F,$7E,$C8       /// movd eax,mm1
    mov [edi],al

    add esi,8
    inc edi
    dec ecx
  jnz @quads

  cmp esi,0
  je @exit

  @skip:
  pop  ecx
  and  ecx,111b
  jz   @exit
  push edi
  mov  edi,ebp
  sub  edi,8  // edi = ebp-8
  rep  movsb
  db $0F,$6F,$85,$F8,$FF,$FF,$FF /// movq mm0,[ebp-8]
  pop  edi
  inc  ecx
  mov  esi,-8
  jmp  @cleanup

  @exit:
  db $0F,$77 // emms
  pop edi
  pop esi
end;

procedure xLineTo1_8(Src,Dst:Pointer;Size:Integer);
// a:b = [ebp-8]
// c = [ebp-12]
var
  a,b,c: Integer;
asm
  push esi
  push edi
  push ebx

  mov  a,0
  mov  b,0
  mov  esi,eax
  mov  edi,edx
  push ecx
  shr  ecx,3
  jz   @skip
  add  edx,ecx
  mov  c,edx

  @quads:
    mov eax,[esi]   //00000001000000020000000300000004
    mov ebx,[esi+4] 
  @cleanup:
    mov edx,eax     //00000001000000020000000300000004
    mov ecx,ebx
    shl edx,9       //00000020000000300000004---------
    shl ecx,9
    or  eax,edx     //00000021000000320000004300000004
    or  ebx,ecx
    mov edx,eax     //00000021000000320000004300000004
    mov ecx,ebx
    shr edx,18      //------------------00000021000000
    shr ecx,18
    or  eax,edx     //00000021000000320000004321000004
    or  ebx,ecx
    shl eax,4
    or  eax,ebx
    shr eax,6
    mov [edi],al

    add esi,8
    inc edi
    cmp edi,[ebp-12]
  jne @quads

  cmp esi,0
  je @exit

  @skip:
  pop  ecx
  and  ecx,111b
  jz   @exit
  push edi
  mov  edi,ebp
  sub  edi,8  // edi = ebp-8
  rep  movsb
  pop  ecx
  mov  edi,ecx
  inc  ecx
  mov  [ebp-12],ecx
  mov  eax,[ebp-8]
  mov  ebx,[ebp-4]
  mov  esi,-8
  jmp  @cleanup

  @exit:
  pop ebx
  pop edi
  pop esi
end;
{$ENDIF}

procedure To1_8(Src,Dst:TFastDIB);
var
  x,y,w8,Gap : Integer;
  pb: PByte;
  SLine : PLine8;
  bt : array[0..7] of Byte;
  Proc: TConvertLineProc;
begin
{$IFDEF CPUX86}
  if cfMMX in CPUInfo.Features then Proc := xLineTo1_8MMX
                               else Proc := xLineTo1_8;
  for y:=0 to Src.AbsHeight-1 do
    Proc(Src.Scanlines[y],Dst.Scanlines[y],Src.BWidth-Src.Gap);
{$ELSE}
  w8 := (Src.Width div 8); Gap := Src.Width - w8*8;
  for y:=0 to Src.AbsHeight-1 do begin
    SLine := Src.Scanlines[y]; pb := Dst.Scanlines[y];
    for x:=0 to w8-1 do begin
      PInteger(@bt[0])^ := PInteger(@SLine[0])^ and $01010101;
      PInteger(@bt[4])^ := PInteger(@SLine[4])^ and $01010101;
      pb^ :=  (bt[7])       or (bt[6] shl 1) or (bt[5] shl 2) or (bt[4] shl 3) or
              (bt[3] shl 4) or (bt[2] shl 5) or (bt[1] shl 6) or (bt[0] shl 7);
      Inc(pb); Inc(PByte(SLine), 8);
    end;
    If (Gap > 0) then begin
      bt[0] := 0;
      For x:=0 to Gap-1 do
        bt[0] := bt[0] or (SLine[x] and 1) shl (7 - x);
      pb^ := bt[0];
    end;
  end;
  {$ENDIF}
  CopyPal(Src, Dst);
end;


// asm version seems to be unfinished - removed
procedure To1_16(Src,Dst:TFastDIB);
label n16;
var
  Gap,x,y,i: Integer;
  pc: PFColorA;
  pw: PWord;
  pb: PByte;
  fr: Word;
begin
  pw:=Pointer(Src.Bits);
  fr:=pw^;
  for y:=0 to Src.AbsHeight-1 do
  begin
    for x:=0 to Src.Width-1 do
    begin
      if pw^<>fr then goto n16;
      Inc(pw);
    end;
    pw:=PWord(NativeInt(pw)+Src.Gap);
  end;
  n16:
  pc:=Pointer(Dst.Colors);
  pc.b:=(pw^ and Src.BMask+1)shl Src.BShr-1;
  pc.g:=(pw^ shr Src.GShl+1)shl Src.GShr-1;
  pc.r:=(pw^ shr Src.RShl+1)shl Src.RShr-1;
  Inc(pc);
  pc.b:=(fr and Src.BMask+1)shl Src.BShr-1;
  pc.g:=(fr shr Src.GShl+1)shl Src.GShr-1;
  pc.r:=(fr shr Src.RShl+1)shl Src.RShr-1;
  i:=(Src.Width shr 3)-1;
  Gap:=(Src.Width mod 8)shl 1;
  for y:=0 to Src.AbsHeight-1 do
  begin
    pw := Src.Scanlines[y]; pb := Dst.Scanlines[y];
    for x:=0 to i do
    begin
      if pw^=fr then pb^:=128 else pb^:=0; Inc(pw);
      if pw^=fr then pb^:=pb^ or 64; Inc(pw);
      if pw^=fr then pb^:=pb^ or 32; Inc(pw);
      if pw^=fr then pb^:=pb^ or 16; Inc(pw);
      if pw^=fr then pb^:=pb^ or 08; Inc(pw);
      if pw^=fr then pb^:=pb^ or 04; Inc(pw);
      if pw^=fr then pb^:=pb^ or 02; Inc(pw);
      if pw^=fr then pb^:=pb^ or 01; Inc(pw);
      Inc(pb);
    end;
    if Gap>0 then if pw^=fr then pb^:=128 else pb^:=0;
    if Gap>1 then if PWord(NativeInt(pw)+02)^=fr then pb^:=pb^ or 64;
    if Gap>2 then if PWord(NativeInt(pw)+04)^=fr then pb^:=pb^ or 32;
    if Gap>3 then if PWord(NativeInt(pw)+06)^=fr then pb^:=pb^ or 16;
    if Gap>4 then if PWord(NativeInt(pw)+08)^=fr then pb^:=pb^ or 08;
    if Gap>5 then if PWord(NativeInt(pw)+10)^=fr then pb^:=pb^ or 04;
    if Gap>6 then if PWord(NativeInt(pw)+12)^=fr then pb^:=pb^ or 02;
    if Gap>7 then if PWord(NativeInt(pw)+14)^=fr then pb^:=pb^ or 01;
  end;
  if Dst.hDC<>0 then Dst.UpdateColors;
end;


procedure To1_24(Src,Dst:TFastDIB);
label n24;
var
  Gap,x,y,i: Integer;
  pc,pc2: PFColor;
  pca: PFColorA;
  frc: TFColor;
  pb: PByte;
begin
  pc:=Pointer(Src.Bits);
  frc:=pc^; // looking for first different color? hmm..
  for y:=0 to Src.AbsHeight-1 do
  begin
    for x:=0 to Src.Width-1 do
    begin
      if(pc.b<>frc.b)or(pc.g<>frc.g)or(pc.r<>frc.r)then goto n24;
      Inc(pc);
    end;
    pc:=PFColor(NativeInt(pc)+Src.Gap);
  end;
  n24:
  pca:=Pointer(Dst.Colors);
  PFColor(pca)^:=frc; Inc(pca);
  PFColor(pca)^:=pc^; frc:=pc^;
  i:=(Src.Width shr 3)-1;
  Gap:=(Src.Width mod 8)*3;
  for y:=0 to Src.AbsHeight-1 do
  begin
    pc := Src.Scanlines[y]; pb := Dst.Scanlines[y];
    for x:=0 to i do
    begin
      if(pc.b=frc.b)and(pc.g=frc.g)and(pc.r=frc.r)then pb^:=128 else pb^:=0; Inc(pc);
      if(pc.b=frc.b)and(pc.g=frc.g)and(pc.r=frc.r)then pb^:=pb^ or 64; Inc(pc);
      if(pc.b=frc.b)and(pc.g=frc.g)and(pc.r=frc.r)then pb^:=pb^ or 32; Inc(pc);
      if(pc.b=frc.b)and(pc.g=frc.g)and(pc.r=frc.r)then pb^:=pb^ or 16; Inc(pc);
      if(pc.b=frc.b)and(pc.g=frc.g)and(pc.r=frc.r)then pb^:=pb^ or 08; Inc(pc);
      if(pc.b=frc.b)and(pc.g=frc.g)and(pc.r=frc.r)then pb^:=pb^ or 04; Inc(pc);
      if(pc.b=frc.b)and(pc.g=frc.g)and(pc.r=frc.r)then pb^:=pb^ or 02; Inc(pc);
      if(pc.b=frc.b)and(pc.g=frc.g)and(pc.r=frc.r)then pb^:=pb^ or 01; Inc(pc);
      Inc(pb);
    end;
    if Gap>0 then if(pc.b=frc.b)and(pc.g=frc.b)and(pc.r=frc.r)then pb^:=128 else pb^:=0;
    if Gap>1 then begin pc2:=PFColor(NativeInt(pc)+03); if(pc2.b=frc.b)and(pc2.g=frc.g)and(pc2.r=frc.r)then pb^:=pb^ or 64; end;
    if Gap>2 then begin pc2:=PFColor(NativeInt(pc)+06); if(pc2.b=frc.b)and(pc2.g=frc.g)and(pc2.r=frc.r)then pb^:=pb^ or 32; end;
    if Gap>3 then begin pc2:=PFColor(NativeInt(pc)+09); if(pc2.b=frc.b)and(pc2.g=frc.g)and(pc2.r=frc.r)then pb^:=pb^ or 16; end;
    if Gap>4 then begin pc2:=PFColor(NativeInt(pc)+12); if(pc2.b=frc.b)and(pc2.g=frc.g)and(pc2.r=frc.r)then pb^:=pb^ or 08; end;
    if Gap>5 then begin pc2:=PFColor(NativeInt(pc)+15); if(pc2.b=frc.b)and(pc2.g=frc.g)and(pc2.r=frc.r)then pb^:=pb^ or 04; end;
    if Gap>6 then begin pc2:=PFColor(NativeInt(pc)+18); if(pc2.b=frc.b)and(pc2.g=frc.g)and(pc2.r=frc.r)then pb^:=pb^ or 02; end;
    if Gap>7 then begin pc2:=PFColor(NativeInt(pc)+21); if(pc2.b=frc.b)and(pc2.g=frc.g)and(pc2.r=frc.r)then pb^:=pb^ or 01; end;
  end;
  if Dst.hDC<>0 then Dst.UpdateColors;
end;

procedure To1_32(Src,Dst:TFastDIB);
label n32;
var
  Gap,x,y,i: Integer;
  pca: PFColorA;
  pdw: PDWord;
  frd: DWord;
  pb: PByte;
begin
  pdw:=Pointer(Src.Bits);
  frd:=pdw^;
  for y:=0 to Src.AbsHeight-1 do
  for x:=0 to Src.Width-1 do
  begin
    if pdw^<>frd then goto n32;
    Inc(pdw);
  end;
  n32:
  pca:=Pointer(Dst.Colors);
  pca.b:=(frd and Src.BMask+1)shl Src.BShr-1;
  pca.g:=(frd shr Src.GShl+1)shl Src.GShr-1;
  pca.r:=(frd shr Src.RShl+1)shl Src.RShr-1;
  Inc(pca);
  frd:=pdw^;
  pca.b:=(frd and Src.BMask+1)shl Src.BShr-1;
  pca.g:=(frd shr Src.GShl+1)shl Src.GShr-1;
  pca.r:=(frd shr Src.RShl+1)shl Src.RShr-1;
  i:=(Src.Width shr 3)-1;
  Gap:=(Src.Width mod 8)shl 2;
  for y:=0 to Src.AbsHeight-1 do
  begin
    pdw := Src.Scanlines[y]; pb := Dst.Scanlines[y];
    for x:=0 to i do
    begin
      if(pdw^=frd)then pb^:=128 else pb^:=0; Inc(pdw);
      if(pdw^=frd)then pb^:=pb^ or 64; Inc(pdw);
      if(pdw^=frd)then pb^:=pb^ or 32; Inc(pdw);
      if(pdw^=frd)then pb^:=pb^ or 16; Inc(pdw);
      if(pdw^=frd)then pb^:=pb^ or 08; Inc(pdw);
      if(pdw^=frd)then pb^:=pb^ or 04; Inc(pdw);
      if(pdw^=frd)then pb^:=pb^ or 02; Inc(pdw);
      if(pdw^=frd)then pb^:=pb^ or 01; Inc(pdw);
      Inc(pb);
    end;
    if Gap>0 then if(pdw^=frd)then pb^:=128 else pb^:=0;
    if Gap>1 then if(PDWord(NativeInt(pdw)+04)^=frd)then pb^:=pb^ or 64;
    if Gap>2 then if(PDWord(NativeInt(pdw)+08)^=frd)then pb^:=pb^ or 32;
    if Gap>3 then if(PDWord(NativeInt(pdw)+12)^=frd)then pb^:=pb^ or 16;
    if Gap>4 then if(PDWord(NativeInt(pdw)+16)^=frd)then pb^:=pb^ or 08;
    if Gap>5 then if(PDWord(NativeInt(pdw)+20)^=frd)then pb^:=pb^ or 04;
    if Gap>6 then if(PDWord(NativeInt(pdw)+24)^=frd)then pb^:=pb^ or 02;
    if Gap>7 then if(PDWord(NativeInt(pdw)+28)^=frd)then pb^:=pb^ or 01;
  end;
  if Dst.hDC<>0 then Dst.UpdateColors;
end;


// ****************************** All -> 4 bpp *********************************

procedure xMakeLut4_1(Lut:array of Integer);
asm
{$IFDEF CPUX64}
  push rdi
  mov rdi, rcx
  xor rcx,rcx
{$ELSE}
  push edi
  mov edi,eax
  xor ecx,ecx
{$ENDIF}

  @bytes:
    mov al,cl
    mov ah,cl
    mov dx,ax
    shl ah,6   //21000000
    shl dh,2   //65432100
    shl al,4   //43210000
    sar ah,6   //22222221
    sar al,6   //44444443
    sar dh,6   //66666665
    sar dl,6   //88888887
    shl eax,16
    mov ax,dx         //22222221444444436666666588888887
    and eax,11111111h //00020001000400030006000500080007
{$IFDEF CPUX64}
    mov [rdi],eax
    add rdi,4
{$ELSE}
    mov [edi],eax
    add edi,4
{$ENDIF}
    inc ecx
    cmp ecx,256
  jne @bytes

{$IFDEF CPUX64}
  pop rdi
{$ELSE}
  pop edi
{$ENDIF}
end;

// LineTo4_1 had no advantage over PAS code - removed

procedure To4_1(Src,Dst:TFastDIB);
var
  x,y,w: Integer;
  pb: PByte;
  dc : PInteger;
  Lut: array[Byte]of Integer;
begin
  xMakeLut4_1(Lut);
  w := Src.BWidth - Src.Gap;
  for y:=0 to Dst.AbsHeight-1 do begin
    pb := Src.Scanlines[y]; dc := Dst.Scanlines[y];
    for x:=0 to w-1 do begin
      dc^ := Lut[pb^]; Inc(pb); Inc(dc);
    end;
  end;
  CopyPal(Src, Dst);
end;


// original asm functions are slower at modern CPUs - removed

procedure To4_8(Src,Dst:TFastDIB);
var
  x,y,w2,Gap: Integer;
  pb: PByte;
  SLine : PLine8;
begin
  w2:=(Src.Width div 2); Gap := Src.Width - w2*2;
  for y:=0 to Src.AbsHeight-1 do begin
    SLine := Src.Scanlines[y]; pb := Dst.Scanlines[y];
    for x:=0 to w2-1 do begin
      pb^ := (SLine[1] and 15) or (SLine[0] shl 4);
      Inc(pb); Inc(PByte(SLine), 2);
    end;
    If Gap > 0 then pb^ := (SLine[0] shl 4);
  end;
  CopyPal(Src, Dst);
end;


procedure To4_16(Src,Dst:TFastDIB);
var
  Bytes: array[Word]of Byte;
  x,y,i: Integer;
  pb,lk: PByte;
  pc: PFColorA;
  pw: PWord;
begin
  FillChar(Bytes,SizeOf(Bytes),$FF);
  pc:=Pointer(Dst.Colors);
  i:=0;

  for y:=0 to Src.AbsHeight-1 do
  begin
    pb:=Dst.Scanlines[y];
    pw:=Src.Scanlines[y];

    for x:=0 to (Src.Width div 2)-1 do
    begin
      lk:=@Bytes[pw^];
      if(lk^=255)and(i<16)then
      begin
        pc.b:=Scale8(pw^ and Src.BMask,Src.Bpb);
        pc.g:=Scale8(pw^ and Src.GMask shr Src.GShl,Src.Bpg);
        pc.r:=Scale8(pw^ and Src.RMask shr Src.RShl,Src.Bpr);
        lk^:=i;
        Inc(i);
        Inc(pc);
      end;
      pb^:=lk^ shl 4;
      Inc(pw);

      lk:=@Bytes[pw^];
      if(lk^=255)and(i<16)then
      begin
        pc.b:=Scale8(pw^ and Src.BMask,Src.Bpb);
        pc.g:=Scale8(pw^ and Src.GMask shr Src.GShl,Src.Bpg);
        pc.r:=Scale8(pw^ and Src.RMask shr Src.RShl,Src.Bpr);
        lk^:=i;
        Inc(i);
        Inc(pc);
      end;
      pb^:=pb^ or lk^;
      Inc(pb);
      Inc(pw);
    end;

    if Src.Gap>0 then
    begin
      lk:=@Bytes[pw^];
      if(lk^=255)and(i<16)then
      begin
        pc.b:=Scale8(pw^ and Src.BMask,Src.Bpb);
        pc.g:=Scale8(pw^ and Src.GMask shr Src.GShl,Src.Bpg);
        pc.r:=Scale8(pw^ and Src.RMask shr Src.RShl,Src.Bpr);
        lk^:=i;
        Inc(i);
        Inc(pc);
      end;
      pb^:=Bytes[pw^]shl 4;
    end;
  end;

  if Dst.hDC<>0 then Dst.UpdateColors;
end;

procedure To4_24(Src,Dst:TFastDIB);
type
  TSpace8 = array[Byte,Byte,Byte]of Byte;
  PSpace8 =^TSpace8;
var
  Space: PSpace8;
  pb,lk: PByte;
  x,y,i: Integer;
  pca: PFColorA;
  pc: PFColor;
begin
  New(Space);
  FillChar(Space^,SizeOf(TSpace8),$FF);
  pca:=Pointer(Dst.Colors);
  i:=0;

  for y:=0 to Src.AbsHeight-1 do
  begin
    pb:=Dst.Scanlines[y];
    pc:=Src.Scanlines[y];

    for x:=0 to (Src.Width div 2)-1 do
    begin
      lk:=@Space[pc.b,pc.g,pc.r];
      if(lk^=255)and(i<16)then
      begin
        pca.c:=pc^;
        Inc(pca);
        lk^:=i;
        Inc(i);
      end;
      pb^:=lk^ shl 4;
      Inc(pc);

      lk:=@Space[pc.b,pc.g,pc.r];
      if(lk^=255)and(i<16)then
      begin
        pca.c:=pc^;
        Inc(pca);
        lk^:=i;
        Inc(i);
      end;
      pb^:=pb^ or lk^;
      Inc(pc);
      Inc(pb);
    end;

    if (Src.Gap and 1)=1 then
    begin
      lk:=@Space[pc.b,pc.g,pc.r];
      if(lk^=255)and(i<16)then
      begin
        pca.c:=pc^;
        Inc(pca);
        lk^:=i;
        Inc(i);
      end;
      pb^:=lk^ shl 4;
    end;
  end;

  Dispose(Space);
  if Dst.hDC<>0 then Dst.UpdateColors;
end;

procedure To4_32(Src,Dst:TFastDIB);
type
  TSpace8 = array[Byte,Byte,Byte]of Byte;
  PSpace8 =^TSpace8;
var
  Space: PSpace8;
  pb,lk: PByte;
  x,y,i: Integer;
  pc,pca: PFColorA;
begin
  New(Space);
  FillChar(Space^,SizeOf(TSpace8),$FF);
  pca:=Pointer(Dst.Colors);
  i:=0;

  for y:=0 to Src.AbsHeight-1 do
  begin
    pb:=Dst.Scanlines[y];
    pc:=Src.Scanlines[y];

    for x:=0 to (Src.Width div 2)-1 do
    begin
      lk:=@Space[pc.b,pc.g,pc.r];
      if(lk^=255)and(i<16)then
      begin
        pca^:=pc^;
        Inc(pca);
        lk^:=i;
        Inc(i);
      end;
      pb^:=lk^ shl 4;
      Inc(pc);

      lk:=@Space[pc.b,pc.g,pc.r];
      if(lk^=255)and(i<16)then
      begin
        pca^:=pc^;
        Inc(pca);
        lk^:=i;
        Inc(i);
      end;
      pb^:=pb^ or lk^;
      Inc(pc);
      Inc(pb);
    end;

    if (Src.Width and 1)=1 then
    begin
      lk:=@Space[pc.b,pc.g,pc.r];
      if(lk^=255)and(i<16)then
      begin
        pca^:=pc^;
        Inc(pca);
        lk^:=i;
        Inc(i);
      end;
      pb^:=lk^ shl 4;
    end;
  end;

  Dispose(Space);
  if Dst.hDC<>0 then Dst.UpdateColors;
end;


// ****************************** All -> 8 bpp *********************************

procedure To8_1(Src,Dst:TFastDIB);
type
  T8Byte  = array[0..7]of Byte;   P8Byte  =^T8Byte;
  T8Bytes = array[Byte]of T8Byte; P8Bytes =^T8Bytes;
var
  Gap,x,y,i: Integer;
  p8bs: P8Bytes;
  p8b: P8Byte;
  pb: PByte;
begin
  New(p8bs);
  pb:=Pointer(p8bs);
  for i:=0 to 255 do
  begin
    pb^:=Byte((i or 127)=255); Inc(pb);
    pb^:=Byte((i or 191)=255); Inc(pb);
    pb^:=Byte((i or 223)=255); Inc(pb);
    pb^:=Byte((i or 239)=255); Inc(pb);
    pb^:=Byte((i or 247)=255); Inc(pb);
    pb^:=Byte((i or 251)=255); Inc(pb);
    pb^:=Byte((i or 253)=255); Inc(pb);
    pb^:=Byte((i or 254)=255); Inc(pb);
  end;
  i:=(Src.Width div 8)-1;
  Gap:=Src.Width mod 8;
  for y:=0 to Src.AbsHeight-1 do begin
    pb := Src.Scanlines[y];
    p8b := Dst.Scanlines[y];
    for x:=0 to i do begin
      p8b^ := p8bs[pb^];
      Inc(pb); Inc(p8b);
    end;
    if Gap>0 then Move(p8bs[pb^],p8b^,Gap);
  end;
  Dispose(p8bs);
  CopyPal(Src, Dst);
end;


procedure To8_4(Src,Dst:TFastDIB);
type
  T2Bytes = array[Byte]of Word;
  P2Bytes =^T2Bytes;
var
  Gap,x,y,i: Integer;
  p2bs: P2Bytes;
  p2b: PWord;
  pb: PByte;
begin
New(p2bs); pb:=Pointer(p2bs);
for i:=0 to 255 do begin
  pb^:=i shr 4;  Inc(pb);
  pb^:=i and 15; Inc(pb);
end;
i:=(Src.Width div 2)-1; Gap:=Src.Width mod 2;
for y:=0 to Src.AbsHeight-1 do begin
  pb := Src.Scanlines[y];
  p2b := Dst.Scanlines[y];
  for x:=0 to i do begin
    p2b^:=p2bs[pb^];
    Inc(pb); Inc(p2b);
  end;
  If Gap>0 then Move(p2bs[pb^],p2b^,Gap);
end;
Dispose(p2bs);
CopyPal(Src, Dst);
end;

procedure To8_16(Src,Dst:TFastDIB);
type
  TWords = array[Word]of Word;
  PWords =^TWords;
var
  x,y,i: Integer;
  Words: PWords;
  pw,ph: PWord;
  cc: PFColorA;
  pb: PByte;
begin
  New(Words);
  FillChar(Words^,SizeOf(TWords),255);
  cc:=Pointer(Dst.Colors);
  pb:=Pointer(Dst.Bits);
  pw:=Pointer(Src.Bits);
  i:=0;
  for y:=0 to Src.AbsHeight-1 do
  begin
    for x:=0 to Src.Width-1 do
    begin
      ph:=@Words[pw^];
      if(ph^=$FFFF)and(i<256)then
      begin
        cc.b:=(pw^ and Src.BMask+1)shl Src.BShr-1;
        cc.g:=(pw^ shr Src.GShl+1)shl Src.GShr-1;
        cc.r:=(pw^ shr Src.RShl+1)shl Src.RShr-1;
        ph^:=i;
        Inc(cc);
        Inc(i);
      end;
      pb^:=Words[pw^];
      Inc(pw);
      Inc(pb);
    end;
    Inc(pb,Dst.Gap);
    pw:=PWord(NativeInt(pw)+Src.Gap);
  end;
  Dispose(Words);
  if Dst.hDC<>0 then Dst.UpdateColors;
end;

procedure To8_24(Src,Dst:TFastDIB);
type
  TSpace1 = array[Byte,Byte,0..31]of Byte;
  PSpace1 =^TSpace1;
  TSpace8 = array[Byte,Byte,Byte]of Byte;
  PSpace8 =^TSpace8;
var
  x,y,i: Integer;
  Space1: PSpace1;
  Space8: PSpace8;
  pb,ph: PByte;
  cc: PFColorA;
  pc: PFColor;
  ch: Byte;
begin
  New(Space1);
  New(Space8);
  FillChar(Space1^,SizeOf(TSpace1),0);
  FillChar(Space8^,SizeOf(TSpace8),0);
  pc:=Pointer(Src.Bits);
  pb:=Pointer(Dst.Bits);
  cc:=Pointer(Dst.Colors);
  i:=0;
  for y:=0 to Src.AbsHeight-1 do
  begin
    for x:=0 to Src.Width-1 do
    begin
      ph:=@Space1[pc.r,pc.g,pc.b shr 3];
      ch:=(1 shl(pc.b and 7));
      if((ph^ and ch)=0)and(i<256)then
      begin
        Space8[pc.r,pc.g,pc.b]:=i;
        PFColor(cc)^:=pc^;
        Inc(cc);
        Inc(i);
        ph^:=ph^ or ch;
      end;
      pb^:=Space8[pc.r,pc.g,pc.b];
      Inc(pb);
      Inc(pc);
    end;
    Inc(pb,Dst.Gap);
    pc:=PFColor(NativeInt(pc)+Src.Gap);
  end;
  Dispose(Space1);
  Dispose(Space8);
  if Dst.hDC<>0 then Dst.UpdateColors;
end;

procedure To8_32(Src,Dst:TFastDIB);
type
  TSpace1 = array[Byte,Byte,0..31]of Byte;
  PSpace1 =^TSpace1;
  TSpace8 = array[Byte,Byte,Byte]of Byte;
  PSpace8 =^TSpace8;
var
  x,y,i: Integer;
  Space1: PSpace1;
  Space8: PSpace8;
  cc,pc: PFColorA;
  pb,ph: PByte;
  ch: Byte;
begin
  New(Space1);
  New(Space8);
  FillChar(Space1^,SizeOf(TSpace1),0);
  FillChar(Space8^,SizeOf(TSpace8),0);
  pc:=Pointer(Src.Bits);
  pb:=Pointer(Dst.Bits);
  cc:=Pointer(Dst.Colors);
  i:=0;
  for y:=0 to Src.AbsHeight-1 do
  begin
    for x:=0 to Src.Width-1 do
    begin
      ph:=@Space1[pc.r,pc.g,pc.b shr 3];
      ch:=(1 shl(pc.b and 7));
      if((ph^ and ch)=0)and(i<256)then
      begin
        Space8[pc.r,pc.g,pc.b]:=i;
        cc.b:=(pc.i and Src.BMask+1)shl Src.BShr-1;
        cc.g:=(pc.i shr Src.GShl+1)shl Src.GShr-1;
        cc.r:=(pc.i shr Src.RShl+1)shl Src.RShr-1;
        Inc(cc);
        Inc(i);
        ph^:=ph^ or ch;
      end;
      pb^:=Space8[pc.r,pc.g,pc.b];
      Inc(pb);
      Inc(pc);
    end;
    Inc(pb,Dst.Gap);
    pc:=PFColorA(NativeInt(pc)+Src.Gap);
  end;
  Dispose(Space1);
  Dispose(Space8);
  if Dst.hDC<>0 then Dst.UpdateColors;
end;


// ****************************** All -> 16 bpp ********************************

procedure To16_1(Src,Dst:TFastDIB);
type
  T8Word  = array[0..7]of Word;
  P8Word  =^T8Word;
  T8Words = array[Byte]of T8Word;
  P8Words =^T8Words;
var
  Gap,x,y,i: Integer;
  p8ws: P8Words;
  p8w: P8Word;
  pw1: PWord;
  fr,bk: Word;
  pca: PFColorA;
  pb: PByte;
begin
  New(p8ws);
  pca:=Pointer(Src.Colors);
  bk:=pca.r shr Dst.RShr shl Dst.RShl or
      pca.g shr Dst.GShr shl Dst.GShl or
      pca.b shr Dst.BShr; Inc(pca);
  fr:=pca.r shr Dst.RShr shl Dst.RShl or
      pca.g shr Dst.GShr shl Dst.GShl or
      pca.b shr Dst.BShr;
  pw1:=Pointer(p8ws);
  for i:=0 to 255 do
  begin
    if(i or 127)=255 then pw1^:=fr else pw1^:=bk; Inc(pw1);
    if(i or 191)=255 then pw1^:=fr else pw1^:=bk; Inc(pw1);
    if(i or 223)=255 then pw1^:=fr else pw1^:=bk; Inc(pw1);
    if(i or 239)=255 then pw1^:=fr else pw1^:=bk; Inc(pw1);
    if(i or 247)=255 then pw1^:=fr else pw1^:=bk; Inc(pw1);
    if(i or 251)=255 then pw1^:=fr else pw1^:=bk; Inc(pw1);
    if(i or 253)=255 then pw1^:=fr else pw1^:=bk; Inc(pw1);
    if(i or 254)=255 then pw1^:=fr else pw1^:=bk; Inc(pw1);
  end;
  i:=(Src.Width shr 3)-1;
  Gap:=(Src.Width mod 8)shl 1;
  for y:=0 to Src.AbsHeight-1 do
  begin
    pb := Src.Scanlines[y];
    p8w := Dst.Scanlines[y];
    for x:=0 to i do begin
      p8w^:=p8ws[pb^];
      Inc(pb); Inc(p8w);
    end;
    if Gap>0 then Move(p8ws[pb^],p8w^,Gap);
  end;
  Dispose(p8ws);
end;

procedure To16_4(Src,Dst:TFastDIB);
type
  TDWords = array[Byte]of DWord;
  PDWords =^TDWords;
var
  Gap,x,y,i: Integer;
  Words: array [0..15] of Word; //PLine16;
  pdws: PDWords;
  pca: PFColorA;
  pdw: PDWord;
  pw: PWord;
  pb: PByte;
begin
  pca:=Pointer(Src.Colors);
  for i:=0 to 15 do begin
    Words[i] := pca.r shr Dst.RShr shl Dst.RShl or
                pca.g shr Dst.GShr shl Dst.GShl or
                pca.b shr Dst.BShr;
    Inc(pca);
  end;

  New(pdws); pw:=Pointer(pdws);
  for i:=0 to 255 do
  begin
    pw^:=Words[i shr 4];  Inc(pw);
    pw^:=Words[i and 15]; Inc(pw);
  end;

  i:=(Src.Width shr 1)-1; Gap:=(Src.Width mod 2)shl 1;
  pdw:=Pointer(Dst.Bits); pb:=Pointer(Src.Bits);

  for y:=0 to Src.AbsHeight-1 do begin
    pb := Src.Scanlines[y]; pdw := Dst.Scanlines[y];
    for x:=0 to i do begin
      pdw^ := pdws[pb^];
      Inc(pb); Inc(pdw);
    end;
    if Gap>0 then Move(pdws[pb^],pdw^,Gap);
  end;
  Dispose(pdws);
end;

procedure To16_8(Src,Dst:TFastDIB);
var
  x,y,i: Integer;
  Words: PLine16;
  pca: PFColorA;
  pb: PByte;
  pw: PWord;
begin
  pca:=Pointer(Src.Colors);
  GetMem(Words,512);
  pw:=Pointer(Words);
  for i:=0 to 255 do
  begin
    pw^:=pca.r shr Dst.RShr shl Dst.RShl or
         pca.g shr Dst.GShr shl Dst.GShl or
         pca.b shr Dst.BShr;
    Inc(pca);
    Inc(pw);
  end;
  pw:=Pointer(Dst.Bits);
  pb:=Pointer(Src.Bits);
  for y:=0 to Src.AbsHeight-1 do
  begin
    for x:=0 to Src.Width-1 do
    begin
      pw^:=Words[pb^];
      Inc(pw);
      Inc(pb);
    end;
    Inc(pb,Src.Gap);
    pw:=PWord(NativeInt(pw)+Dst.Gap);
  end;
  FreeMem(Words);
end;

procedure To16_16(Src,Dst:TFastDIB);
var
  Words: PLine16;
  pw1,pw2: PWord;
  x,y: Integer;
  i: DWord;
begin
  GetMem(Words,131072);
  pw1:=Pointer(Words);
  for i:=0 to 65535 do
  begin
    pw1^:=((i shr Src.RShl+1)shl Src.RShr-1)and $FF shr Dst.RShr shl Dst.RShl or
          ((i shr Src.GShl+1)shl Src.GShr-1)and $FF shr Dst.GShr shl Dst.GShl or
          ((i and Src.BMask+1)shl Src.BShr-1)and $FF shr Dst.BShr;
    Inc(pw1);
  end;
  for y:=0 to Src.AbsHeight-1 do
  begin
    pw1 := Src.Scanlines[y]; pw2 := Dst.Scanlines[y];
    for x:=0 to Src.Width-1 do
    begin
      pw2^:=Words[pw1^];
      Inc(pw1); Inc(pw2);
    end;
  end;
  FreeMem(Words);
end;

{
procedure To16_24(Src,Dst:TFastDIB);
var
  r16,g16,b16: PLine16;
  x,y: Integer;
  pc: PFColor;
  pw: PWord;
begin
  GetMem(r16,512);
  GetMem(g16,512);
  GetMem(b16,512);
  for x:=0 to 255 do
  begin
    r16[x]:=x shr Dst.RShr shl Dst.RShl;
    g16[x]:=x shr Dst.GShr shl Dst.GShl;
    b16[x]:=x shr Dst.BShr;
  end;

  for y:=0 to Src.AbsHeight-1 do
  begin
    pc := Src.Scanlines[y]; pw := Dst.Scanlines[y];
    for x:=0 to Src.Width-1 do
    begin
      pw^:=b16[pc.b]or g16[pc.g]or r16[pc.r];
      Inc(pw); Inc(pc);
    end;
  end;
  FreeMem(b16);
  FreeMem(g16);
  FreeMem(r16);
end;
}

// original proc increased brightness at bit - fixed with LUT tuning

procedure To16_24(Src, Dst : TFastDIB; Dither : Boolean = False);
var
  BLut: PBackLut16;
  Lut : PLut16;
  x,y,i,sBytesPP: Integer;
  pc: PFColor;
  pw: PWord;
  cErr : TFColorA;
  Err : TErrorMetrics;
begin
BLut := Dst.GenBackLut16;
If Dither then begin
  Lut := Dst.GenLut16;
  xErrInit(Err, Src, Dst);
  for y:=0 to Src.AbsHeight-1 do begin
    for x:=0 to Src.Width-1 do begin
      cErr := xErrGetCurPix(Err, PFColor(Err.SrcPtr));
      With BLut^ do
        i := bi[cErr.b] or gi[cErr.g] or ri[cErr.r];
      cErr.c := Lut[i];
      PWord(Err.DstPtr)^ := i;
      xErrNextPix(Err, @cErr);
    end;
    xErrNextLine(Err);
  end;
end else begin
  sBytesPP := Src.Bpp shr 3;
  for y:=0 to Src.AbsHeight-1 do begin
    pc := Src.Scanlines[y]; pw := Dst.Scanlines[y];
    for x:=0 to Src.Width-1 do begin
      With BLut^ do
        pw^ := bi[pc.b] or gi[pc.g] or ri[pc.r];
      Inc(pw); Inc(PByte(pc), sBytesPP);
    end;
  end;
end;
end;


procedure To16_32(Src,Dst:TFastDIB);
var
  r16,g16,b16: PLine16;
  x,y: Integer;
  pc: PDWord;
  pw: PWord;
begin
  GetMem(r16,512);
  GetMem(g16,512);
  GetMem(b16,512);
  for x:=0 to 255 do
  begin
    r16[x]:=x shr Dst.RShr shl Dst.RShl;
    g16[x]:=x shr Dst.GShr shl Dst.GShl;
    b16[x]:=x shr Dst.BShr;
  end;

  for y:=0 to Src.AbsHeight-1 do
  begin
    pc := Src.Scanlines[y]; pw := Dst.Scanlines[y];
    for x:=0 to Src.Width-1 do
    begin
      pw^:=b16[((pc^ and Src.BMask+1)shl Src.BShr-1)and $FF]or
           g16[((pc^ shr Src.GShl+1)shl Src.GShr-1)and $FF]or
           r16[((pc^ shr Src.RShl+1)shl Src.RShr-1)and $FF];
      Inc(pw); Inc(pc);
    end;
  end;
  FreeMem(b16);
  FreeMem(g16);
  FreeMem(r16);
end;


// ****************************** All -> 24 bpp ********************************

procedure To24_1(Src,Dst:TFastDIB);
type
  T8Color  = array[0..7]of TFColor;
  P8Color  =^T8Color;
  T8Colors = array[Byte]of T8Color;
  P8Colors =^T8Colors;
var
  Gap,x,y,i: Integer;
  p8cs: P8Colors;
  p8c: P8Color;
  fr,bk: TFColor;
  pc: PFColor;
  pb: PByte;
begin
  bk:=Src.Colors[0].c;
  fr:=Src.Colors[1].c;
  New(p8cs);
  pc:=Pointer(p8cs);
  for i:=0 to 255 do
  begin
    if(i or 127)=255 then pc^:=fr else pc^:=bk; Inc(pc);
    if(i or 191)=255 then pc^:=fr else pc^:=bk; Inc(pc);
    if(i or 223)=255 then pc^:=fr else pc^:=bk; Inc(pc);
    if(i or 239)=255 then pc^:=fr else pc^:=bk; Inc(pc);
    if(i or 247)=255 then pc^:=fr else pc^:=bk; Inc(pc);
    if(i or 251)=255 then pc^:=fr else pc^:=bk; Inc(pc);
    if(i or 253)=255 then pc^:=fr else pc^:=bk; Inc(pc);
    if(i or 254)=255 then pc^:=fr else pc^:=bk; Inc(pc);
  end;
  i:=(Src.Width shr 3)-1;
  Gap:=(Src.Width mod 8)*3;
  for y:=0 to Src.AbsHeight-1 do begin
    pb := Src.Scanlines[y];
    p8c := Dst.Scanlines[y];
    for x:=0 to i do begin
      p8c^ := p8cs[pb^];
      Inc(pb); Inc(p8c);
    end;
    if Gap>0 then Move(p8cs[pb^],p8c^,Gap);
  end;
  Dispose(p8cs);
end;

procedure To24_4(Src,Dst:TFastDIB);
type
  T2Color  = array[0..5]of Byte;
  P2Color  =^T2Color;
  T2Colors = array[Byte]of T2Color;
  P2Colors =^T2Colors;
var
  Gap,x,y,i: Integer;
  p2cs: P2Colors;
  p2c: P2Color;
  pc: PFColor;
  pb: PByte;
begin
  New(p2cs);
  pc:=Pointer(p2cs);
  for i:=0 to 255 do
  begin
    pc^:=Src.Colors[i shr 4].c;  Inc(pc);
    pc^:=Src.Colors[i and 15].c; Inc(pc);
  end;
  i:=(Src.Width shr 1)-1;
  Gap:=(Src.Width mod 2)*3;
  for y:=0 to Src.AbsHeight-1 do
  begin
    pb := Src.Scanlines[y];
    p2c := Dst.Scanlines[y];
    for x:=0 to i do begin
      p2c^:=p2cs[pb^];
      Inc(pb); Inc(p2c);
    end;
    if Gap>0 then Move(p2cs[pb^],p2c^,Gap);
  end;
  Dispose(p2cs);
end;

procedure To24_8(Src,Dst:TFastDIB);
var
  x,y: Integer;
  pc: PFColor;
  pb: PByte;
begin
  pc:=Pointer(Dst.Bits);
  pb:=Pointer(Src.Bits);
  for y:=0 to Src.AbsHeight-1 do
  begin
    for x:=0 to Src.Width-1 do
    begin
      pc^:=Src.Colors[pb^].c;
      Inc(pc); Inc(pb);
    end;
    Inc(pb,Src.Gap);
    pc:=PFColor(NativeInt(pc)+Dst.Gap);
  end;
end;
{
procedure To24_16(Src,Dst:TFastDIB);
var
  x,y,i: Integer;
  m24: PLine24;
  pc: PFColor;
  pw: PWord;
begin
  GetMem(m24,196608);
  pc:=Pointer(m24);
  for i:=0 to 65535 do
  begin
    pc.b:=(i and Src.BMask+1)shl Src.BShr-1;
    pc.g:=(i shr Src.GShl+1)shl Src.GShr-1;
    pc.r:=(i shr Src.RShl+1)shl Src.RShr-1;
    Inc(pc);
  end;
  for y:=0 to Src.AbsHeight-1 do
  begin
    pw := Src.Scanlines[y]; pc := Dst.Scanlines[y];
    for x:=0 to Src.Width-1 do
    begin
      pc^:=m24[pw^];
      Inc(pc); Inc(pw);
    end;
  end;
  FreeMem(m24);
end;
}

procedure To24_16(Src,Dst:TFastDIB);
var
  x,y: Integer;
  pc: PFColor;
  pw: PWord;
  Lut : PLut16;
begin
Lut := Src.GenLut16;
pc:=Pointer(Dst.Bits); pw:=Pointer(Src.Bits);
for y:=0 to Src.AbsHeight-1 do begin
  pw := Src.Scanlines[y]; pc := Dst.Scanlines[y];
  for x:=0 to Src.Width-1 do begin
    pc^:=Lut[pw^];
    Inc(pc); Inc(pw);
  end;
end;
end;


procedure To24_32(Src,Dst:TFastDIB);
var
  x,y: Integer;
  pd: PFColorA;
  pc: PFColor;
begin
  pc:=Pointer(Dst.Bits);
  pd:=Pointer(Src.Bits);
  if(Src.Bpb=8)and(Src.Bpg=8)and(Src.Bpr=8)then
  begin
    for y:=0 to Src.AbsHeight-1 do
    begin
      for x:=0 to Src.Width-1 do
      begin
        pc^:=pd.c;
        Inc(pc);
        Inc(pd);
      end;
      pc:=PFColor(NativeInt(pc)+Dst.Gap);
      pd:=PFColorA(NativeInt(pd)+Src.Gap);
    end;
  end else
  begin
    for y:=0 to Src.AbsHeight-1 do
    begin
      for x:=0 to Src.Width-1 do
      begin
        pc.b:=(pd.i and Src.BMask+1)shl Src.BShr-1;
        pc.g:=(pd.i shr Src.GShl+1)shl Src.GShr-1;
        pc.r:=(pd.i shr Src.RShl+1)shl Src.RShr-1;
        Inc(pc);
        Inc(pd);
      end;
      pc:=PFColor(NativeInt(pc)+Dst.Gap);
      pd:=PFColorA(NativeInt(pd)+Src.Gap);
    end;
  end;
end;


// ****************************** All -> 32 bpp ********************************

procedure To32_1(Src,Dst:TFastDIB);
type
  T8Color  = array[0..7]of Integer;
  P8Color  =^T8Color;
  T8Colors = array[Byte]of T8Color;
  P8Colors =^T8Colors;
const
  Masks: array[0..7]of Byte = (127,191,223,239,247,251,253,254);
var
  Gap,x,y,i: Integer;
  p8cs: P8Colors;
  p8c: P8Color;
  pc: TFColorA;
  pd: PDWord;
  pb: PByte;
begin
  New(p8cs);
  pd:=Pointer(p8cs);
  for i:=0 to 255 do
  for x:=0 to 7 do
  begin
    pc:=Src.Colors[Byte((i or Masks[x])=255)];
    pd^:=pc.r shr Dst.RShr shl Dst.RShl or
         pc.g shr Dst.GShr shl Dst.GShl or
         pc.b shr Dst.BShr;
    Inc(pd);
  end;
  i:=(Src.Width shr 3)-1;
  Gap:=(Src.Width mod 8)shl 2;
  for y:=0 to Src.AbsHeight-1 do begin
    pb := Src.Scanlines[y];
    p8c := Dst.Scanlines[y];
    for x:=0 to i do begin
      p8c^ := p8cs[pb^];
      Inc(pb); Inc(p8c);
    end;
    if Gap>0 then Move(p8cs[pb^],p8c^,Gap);
  end;
  Dispose(p8cs);
end;

procedure To32_4(Src,Dst:TFastDIB);
type
  T2Color  = array[0..1]of Integer;
  P2Color  =^T2Color;
  T2Colors = array[Byte]of T2Color;
  P2Colors =^T2Colors;
var
  Gap,x,y,i: Integer;
  p2cs: P2Colors;
  p2c: P2Color;
  pc: TFColorA;
  pd: PDWord;
  pb: PByte;
begin
  New(p2cs);
  pd:=Pointer(p2cs);
  for i:=0 to 255 do
  begin
    pc:=Src.Colors[i shr 4];
    pd^:=pc.r shr Dst.RShr shl Dst.RShl or
         pc.g shr Dst.GShr shl Dst.GShl or
         pc.b shr Dst.BShr;
    Inc(pd);
    pc:=Src.Colors[i and 15];
    pd^:=pc.r shr Dst.RShr shl Dst.RShl or
         pc.g shr Dst.GShr shl Dst.GShl or
         pc.b shr Dst.BShr;
    Inc(pd);
  end;
  i:=(Src.Width shr 1)-1;
  Gap:=(Src.Width mod 2)shl 2;
  for y:=0 to Src.AbsHeight-1 do begin
    pb := Src.Scanlines[y];
    p2c := Dst.Scanlines[y];
    for x:=0 to i do begin
      p2c^ := p2cs[pb^];
      Inc(pb); Inc(p2c);
    end;
    if Gap>0 then Move(p2cs[pb^],p2c^,Gap);
  end;
  Dispose(p2cs);
end;

procedure To32_8(Src,Dst:TFastDIB);
type
  TDWords = array[Word]of DWord;
  PDWords =^TDWords;
var
  DWords: PDWords;
  x,y: Integer;
  pc: PFColorA;
  pd: PDWord;
  pb: PByte;
begin
  GetMem(DWords,1024);
  pd:=Pointer(DWords);
  pc:=Pointer(Src.Colors);
  for x:=0 to 255 do
  begin
    pd^:=pc.r shr Dst.RShr shl Dst.RShl or
         pc.g shr Dst.GShr shl Dst.GShl or
         pc.b shr Dst.BShr;
    Inc(pd);
    Inc(pc);
  end;

  for y:=0 to Src.AbsHeight-1 do
  begin
    pb := Src.Scanlines[y]; pd := Dst.Scanlines[y];
    for x:=0 to Src.Width-1 do
    begin
      pd^:=DWords[pb^];
      Inc(pd); Inc(pb);
    end;
  end;
  FreeMem(DWords);
end;

procedure To32_16(Src,Dst:TFastDIB);
type
  TDWords = array[Word]of DWord;
  PDWords =^TDWords;
var
  DWords: PDWords;
  x,y,i: DWord;
  pd: PDWord;
  pw: PWord;
begin
  GetMem(DWords,262144);
  pd:=Pointer(DWords);
  pd^:=0;
  Inc(pd);
  for i:=1 to 65535 do
  begin
    pd^:=((i and Src.BMask+1)shl Src.BShr-1)and $FF shr Dst.BShr or
         ((i shr Src.GShl+1)shl Src.GShr-1)and $FF shr Dst.GShr shl Dst.GShl or
         ((i shr Src.RShl+1)shl Src.RShr-1)and $FF shr Dst.RShr shl Dst.RShl;
    Inc(pd);
  end;

  for y:=0 to Src.AbsHeight-1 do
  begin
    pw := Src.Scanlines[y]; pd := Dst.Scanlines[y];
    for x:=0 to Src.Width-1 do
    begin
      pd^:=DWords[pw^];
      Inc(pd); Inc(pw);
    end;
  end;
  FreeMem(DWords);
end;

procedure To32_24(Src,Dst:TFastDIB);
var
  x,y: Integer;
  pfc: PFColor;
  pc: PFColorA;
  pd: PDWord;
begin
  if(Dst.Bpr=8)and(Dst.Bpg=8)and(Dst.Bpb=8)then
  begin
    for y:=0 to Src.AbsHeight-1 do
    begin
      pfc := Src.Scanlines[y]; pc := Dst.Scanlines[y];
      for x:=0 to Src.Width-1 do
      begin
        PFColor(pc)^:=pfc^;
        Inc(pc);
        Inc(pfc);
      end;
    end;
  end else
  begin
    for y:=0 to Src.AbsHeight-1 do
    begin
      pfc := Src.Scanlines[y]; pd := Dst.Scanlines[y];
      for x:=0 to Src.Width-1 do
      begin
        pd^:=pfc.r shr Dst.RShr shl Dst.RShl or
             pfc.g shr Dst.GShr shl Dst.GShl or
             pfc.b shr Dst.BShr;
        Inc(pd);
        Inc(pfc);
      end;
    end;
  end;
end;

procedure To32_32(Src,Dst:TFastDIB);
var
  x,y: Integer;
  pd1,pd2: PDWord;
begin
  pd1:=Pointer(Dst.Bits);
  pd2:=Pointer(Src.Bits);
  if(Dst.Bpr=Src.Bpr)and(Dst.Bpg=Src.Bpg)and(Dst.Bpb=Src.Bpb)then
  begin
    for y:=0 to Src.AbsHeight-1 do
      Move(Src.Scanlines[y]^,Dst.Scanlines[y]^,Dst.BWidth-Dst.Gap);
  end else
  begin
    for y:=0 to Src.AbsHeight-1 do
    begin
      for x:=0 to Src.Width-1 do
      begin
        pd1^:=((pd2^ and Src.BMask+1)shl Src.BShr-1)and $FF shr Dst.BShr or
              ((pd2^ shr Src.GShl+1)shl Src.GShr-1)and $FF shr Dst.GShr shl Dst.GShl or
              ((pd2^ shr Src.RShl+1)shl Src.RShr-1)and $FF shr Dst.RShr shl Dst.RShl;
        Inc(pd1);
        Inc(pd2);
      end;
      pd1:=PDWord(NativeInt(pd1)+Dst.Gap);
      pd2:=PDWord(NativeInt(pd2)+Src.Gap);
    end;
  end;
end;


// ************************ Alpha procs ****************************************

procedure To16_8A(Src, SrcA, Dst : TFastDIB; DstAShr, DstAShl : Byte);
var
  x,y,i: Integer;
  Words: PLine16;
  pca: PFColorA;
  pb, pa: PByte;
  pw: PWord;
begin
  pca:=Pointer(Src.Colors);
  GetMem(Words,512);
  pw:=Pointer(Words);
  for i:=0 to 255 do
  begin
    pw^:=pca.r shr Dst.RShr shl Dst.RShl or
         pca.g shr Dst.GShr shl Dst.GShl or
         pca.b shr Dst.BShr;
    Inc(pca);
    Inc(pw);
  end;
  pw:=Pointer(Dst.Bits);
  pb:=Pointer(Src.Bits); pa:=Pointer(SrcA.Bits);
  for y:=0 to Src.AbsHeight-1 do
  begin
    for x:=0 to Src.Width-1 do
    begin
      pw^:=Words[pb^] or (SrcA.Colors[pa^].r shr DstAShr shl DstAShl);
      Inc(pw);
      Inc(pb); Inc(pa);
    end;
    Inc(pb, Src.Gap); Inc(pa, SrcA.Gap);
    pw:=PWord(NativeInt(pw)+Dst.Gap);
  end;
  FreeMem(Words);
end;


procedure To32_8A(Src, SrcA, Dst : TFastDIB; DstAShr, DstAShl : Byte);
type
  TDWords = array[Word]of DWord;
  PDWords =^TDWords;
var
  DWords: PDWords;
  x,y: Integer;
  pc: PFColorA;
  pd: PDWord;
  pb, pa: PByte;
begin
  GetMem(DWords,1024);
  pd:=Pointer(DWords);
  pc:=Pointer(Src.Colors);
  for x:=0 to 255 do
  begin
    pd^:=pc.r shr Dst.RShr shl Dst.RShl or
         pc.g shr Dst.GShr shl Dst.GShl or
         pc.b shr Dst.BShr;
    Inc(pd);
    Inc(pc);
  end;

  for y:=0 to Src.AbsHeight-1 do begin
    pb := Src.Scanlines[y]; pa := SrcA.Scanlines[y];
    pd := Dst.Scanlines[y];
    for x:=0 to Src.Width-1 do
    begin
      pd^:=DWords[pb^] or (pa^ shr DstAShr shl DstAShl);
      Inc(pd); Inc(pb); Inc(pa);
    end;
  end;
  FreeMem(DWords);
end;


procedure To16_32A(Src, Dst : TFastDIB; {SrcAShr, SrcAShl, }DstAShr, DstAShl : Byte);
var
  r16,g16,b16,a16: PLine16;
  x,y: Integer;
  pc: PDWord;
  pw: PWord;
begin
  GetMem(r16,512);
  GetMem(g16,512);
  GetMem(b16,512);
  GetMem(a16,512);
  for x:=0 to 255 do
  begin
    r16[x]:=x shr Dst.RShr shl Dst.RShl;
    g16[x]:=x shr Dst.GShr shl Dst.GShl;
    b16[x]:=x shr Dst.BShr shl Dst.BShl;
    a16[x]:=x shr DstAShr  shl DstAShl;
  end;
  for y:=0 to Src.AbsHeight-1 do
  begin
    pc := Src.Scanlines[y]; pw := Dst.Scanlines[y];
    for x:=0 to Src.Width-1 do
    begin
      pw^:=b16[((pc^ and Src.BMask+1)shl Src.BShr-1)and $FF] or
           g16[((pc^ shr Src.GShl+1)shl Src.GShr-1)and $FF] or
           r16[((pc^ shr Src.RShl+1)shl Src.RShr-1)and $FF] or
           a16[(pc^ shr 24) and $FF];
      Inc(pw);
      Inc(pc);
    end;
  end;
  FreeMem(a16);
  FreeMem(b16);
  FreeMem(g16);
  FreeMem(r16);
end;

procedure To32_32A(Src, Dst : TFastDIB; DstAShr, DstAShl : Byte);
var
  x,y: Integer;
  pd1,pd2: PDWord;
begin
  pd1:=Pointer(Dst.Bits);
  pd2:=Pointer(Src.Bits);
  If (Dst.Bpr=Src.Bpr) and (Dst.Bpg=Src.Bpg) and
     (Dst.Bpb=Src.Bpb) then
  begin
    for y:=0 to Src.AbsHeight-1 do
      Move(Src.Scanlines[y]^,Dst.Scanlines[y]^,Dst.BWidth-Dst.Gap);
  end else
  begin
    for y:=0 to Src.AbsHeight-1 do
    begin
      for x:=0 to Src.Width-1 do
      begin
        pd1^:=((pd2^ and Src.BMask+1)shl Src.BShr-1)and $FF shr Dst.BShr or
              ((pd2^ shr Src.GShl+1)shl Src.GShr-1)and $FF shr Dst.GShr shl Dst.GShl or
              ((pd2^ shr Src.RShl+1)shl Src.RShr-1)and $FF shr Dst.RShr shl Dst.RShl or
              ((pd2^ shr 24) {shl SrcAShr-1)}and $FF shr DstAShr shl DstAShl);
        Inc(pd1);
        Inc(pd2);
      end;
      pd1:=PDWord(NativeInt(pd1)+Dst.Gap);
      pd2:=PDWord(NativeInt(pd2)+Src.Gap);
    end;
  end;
end;


function ConvertAlpha(Src, SrcA, Dst : TFastDIB; DstAShr, DstAShl : Byte): Boolean;
begin
Result := True;
If (Src.Bpp = 8) and (SrcA <> nil) then
  Case Dst.Bpp of
    16 : To16_8A(Src, SrcA, Dst, DstAShr, DstAShl);
    32 : To32_8A(Src, SrcA, Dst, DstAShr, DstAShl);
    else Result := False;
  end
else
  If (Src.Bpp = 32) then
    Case Dst.Bpp of
      16 : To16_32A(Src, Dst, DstAShr, DstAShl);
      32 : To32_32A(Src, Dst, DstAShr, DstAShl);
      else Result := False;
    end
  else Result := False;
end;


// ************************ Quantization ***************************************
// based on code by Earl F. Glynn, http://www.efg2.com

Const
  Mask:  ARRAY[0..7] OF BYTE = ($80, $40, $20, $10, $08, $04, $02, $01);

Type
  PColorNode = ^TColorNode;
  TChildren = ARRAY[0..7] OF PColorNode;
  TColorNode = record
    IsLeaf      :  BOOLEAN;
    RedSum, GreenSum, BlueSum, PixelCount : DWord;// Integer;
    Next        :  PColorNode;
    Child       :  TChildren;
    PalIndex : Integer;
  end;


  procedure xAddColor(VAR Node : PColorNode; c : PFColor;
                      ColorBits, Level :  INTEGER;
                      VAR LeafCount : INTEGER;
                      VAR Reducible:  TChildren);
  VAR Index, Shift : INTEGER;
  begin
  IF (Node = NIL) then begin // If the node doesn't exist, create it.
    GetMem(Node, SizeOf(TColorNode));
    FillChar(Node^, SizeOf(TColorNode), 0);
    Node.IsLeaf := (Level = ColorBits);
    IF (Node.IsLeaf) then Inc(LeafCount) else begin
      Node.Next := Reducible[Level];
      Reducible[Level] := Node;
    end;
  end;
  IF Node.IsLeaf then With Node^ do begin
    INC(PixelCount);
    INC(RedSum, c.r); INC(GreenSum, c.g); INC(BlueSum, c.b);
  end else begin // Recurse a level deeper if the node is not a leaf.
    Shift := 7 - Level;
    Index := (((c.r AND mask[Level]) SHR Shift) SHL 2)  OR
             (((c.g AND mask[Level]) SHR Shift) SHL 1)  OR
              ((c.b AND mask[Level]) SHR Shift);
    xAddColor(Node.Child[Index], c, ColorBits, Level+1, LeafCount, Reducible)
  end;
  end;

  function xReduceTree(VAR Reducible:  TChildren; VAR LeafCount:  INTEGER;
                       ColorBits, ColorCnt:  INTEGER): Boolean;
  VAR
    RedSum, GreenSum, BlueSum :  INTEGER;
    Children:  INTEGER;
    i       :  INTEGER;
    Node    :  PColorNode;
  BEGIN
    // Find the deepest level containing at least one reducible node
  i := Colorbits - 1;
  WHILE (i > 0) AND (Reducible[i] = NIL) DO
    DEC(i);

  Result := (i <> 0);
  If (not Result) then Exit;
  // Reduce the node most recently added to the list at level i.
  Node := Reducible[i];
  Reducible[i] := Node.Next;

  RedSum   := 0; GreenSum := 0; BlueSum  := 0; Children := 0;

  FOR i := Low(TChildren) TO High(TChildren) DO BEGIN
    IF (Node.Child[i] <> NIL) // and (LeafCount - Children + 1 >= ColorCnt)
    then begin
      INC(RedSum,   Node.Child[i].RedSum);
      INC(GreenSum, Node.Child[i].GreenSum);
      INC(BlueSum,  Node.Child[i].BlueSum);
      INC(Node.PixelCount, Node.Child[i].PixelCount);
      FreeMem(Node.Child[i]);
      Node.Child[i] := NIL;
      INC(Children);
    END;
  END;

  Node.IsLeaf   := TRUE;
  Node.RedSum   := RedSum; Node.GreenSum := GreenSum; Node.BlueSum  := BlueSum;
  DEC(LeafCount, Children-1)
  END;

  function xFindRareColor(Node : PColorNode; Var Dst : PColorNode;
                          VAR Counter :  INTEGER): Integer;
  VAR i, sum:  INTEGER;
      c : PColorNode;
  BEGIN
  Result := 0;
  If (Counter <= 1) then Exit;
  FOR i := Low(Node.Child) TO High(Node.Child) DO BEGIN
//    If (Counter = 1) then Break;
    c := Node.Child[i];
    IF (c <> NIL) THEN
      If (c.IsLeaf) then Inc(Result, c.PixelCount)
                    else Inc(Result, xFindRareColor(c, Dst, Counter))
  END;
  If (Counter > 1) and (Result < Counter) then begin
    Counter := Result; Dst := Node;
  end;
  END;

  procedure xDelNode(Node : PColorNode; VAR LeafCount :  INTEGER);
  VAR
    i, DelCnt : INTEGER;
    c :  PColorNode;
  BEGIN
  DelCnt := 0;
  FOR i := Low(TChildren) TO High(TChildren) DO BEGIN
    c := Node.Child[i];
    If (c <> NIL) then begin
      If (not c.IsLeaf) then
        xDelNode(c, LeafCount);
      INC(Node.RedSum, c.RedSum); INC(Node.GreenSum, c.GreenSum);
      INC(Node.BlueSum, c.BlueSum); INC(Node.PixelCount, c.PixelCount);
      FreeMem(c);
      Node.Child[i] := NIL;
      INC(DelCnt);
    end;
  END;
  Node.IsLeaf := TRUE;
  DEC(LeafCount, DelCnt-1)
  END;

  procedure xFillPalette(Node : PColorNode; Palette:  PFColorTable;
                         VAR Counter :  INTEGER; Var ColorDisp : PInteger);
  VAR i:  INTEGER;
      c : TFColor;
  BEGIN
  IF Node.IsLeaf THEN BEGIN
    If (Node.PixelCount <> 0) then WITH Palette[Counter] DO BEGIN
      r := BYTE(Node.RedSum   DIV Node.PixelCount);
      g := BYTE(Node.GreenSum DIV Node.PixelCount);
      b := BYTE(Node.BlueSum  DIV Node.PixelCount);
    end;
    If (ColorDisp <> nil) then begin
      ColorDisp^ := Node.PixelCount; Inc(ColorDisp);
    end;
    Node.PalIndex := Counter;
    INC(Counter)
  END ELSE BEGIN
    FOR i := Low(Node.Child) TO High(Node.Child) DO BEGIN
      IF Node.Child[i] <> NIL THEN
        xFillPalette(Node.Child[i], Palette, Counter, ColorDisp)
    END
  END
  END;

  function xGetColorIndex(Node : PColorNode; c : PFColor;
                          Level : INTEGER): Integer;
  VAR
    Index, Shift : INTEGER;
  BEGIN
  IF (Node = nil) THEN Result := -1 else
    IF Node.IsLeaf THEN
      Result := Node.PalIndex
    ELSE BEGIN // Recurse a level deeper if the node is not a leaf.
      Shift := 7 - Level;
      Index := (((c.r AND mask[Level]) SHR Shift) SHL 2)  OR
               (((c.g AND mask[Level]) SHR Shift) SHL 1)  OR
               ((c.b AND mask[Level]) SHR Shift);
      Result := xGetColorIndex(Node.Child[Index], c, Level+1);
    END;
  END;

  procedure xDeleteTree(Node : PColorNode);
  Var i : Integer;
  BEGIN
  If Node <> nil then begin
    FOR i := Low(TChildren) TO High(TChildren) DO
      If Node.Child[i] <> NIL then
        xDeleteTree(Node.Child[i]);
    FreeMem(Node);
  end;
  end;

  function xErrNearestColor(Tree : PColorNode; c : PFColor; Data : Integer): Integer;
  Var n, Delta, MinDelta : Integer;
      pc : PFColorA;
  begin
  MinDelta := 3 * (256 * 256);
  pc := @PErrorMetrics(Data).Pal[0];
  For n:=0 to PErrorMetrics(Data).ColorCnt-1 do begin
//    Delta := Abs(pc.b - cErr.b) + Abs(pc.g - cErr.g) + Abs(pc.r - cErr.r);
    Delta := Sqr(pc.b - c.b) + Sqr(pc.g - c.g) + Sqr(pc.r - c.r);
    If (Delta < MinDelta) then begin MinDelta := Delta; Result := n; end;
    Inc(pc);
  end;
  end;

  procedure xLine_Dither(Var Err : TErrorMetrics; Tree : PColorNode;
                         sBytesPP, dBytesPP : Integer);
  Var x, i : Integer;
      pc, dc : PFColor;
      cErr : TFColorA;
      SrcPal : PFColorTable;
  begin
  If (sBytesPP > 1) and (dBytesPP = 1)  then
    For x:=0 to Err.Width-1 do begin
      // Get Floyd-Steinberg color approximation from current color
      cErr := xErrGetCurPix(Err, PFColor(Err.SrcPtr));
      // Try to find exact color from color tree (faster)
      i := xGetColorIndex(Tree, @cErr.c, 0);
      // ...or get nearest from palette. Slow, nearly 50% of all proc time
      If (i = -1) then //i := 0;
        i := xErrNearestColor(nil, @cErr.c, Integer(@Err));
      Err.DstPtr^ := i;
      // Modify F-S error state according to real color used
      xErrNextPix(Err, @Err.Pal[i]);
    end
  else begin
    SrcPal := Err.Src.Colors;
    For x:=0 to Err.Width-1 do begin
      If sBytesPP = 1 then pc := @SrcPal[ Err.SrcPtr^ ]
                      else pc := PFColor(Err.SrcPtr);
      cErr := xErrGetCurPix(Err, pc );
      i := xGetColorIndex(Tree, @cErr.c, 0);
      If (i = -1) then
        i := xErrNearestColor(nil, @cErr.c, Integer(@Err));
      dc := @Err.Pal[i];
      If (dBytesPP = 1) then Err.DstPtr^ := i
                        else PFColor(Err.DstPtr)^ := dc^;
      xErrNextPix(Err, dc);
    end;
  end;
  xErrNextLine(Err);
  end;


// Src can be 8, 24, 32 bpp, Dst - 8, 24, 32 bpp or nil
// if DstColorDisp <> nil, color counts for each palette entry writed there
function Quantize(Src, Dst : TFastDIB; ColorCnt : Integer = 256;
                  ColorBits : Integer = 6; Dithering : Boolean = False;
                  DstColorDisp : PInteger = nil): Integer;
Const
//  ColorCntBased = True;
  ColorCntBased = False;
Var x, y, i, sBytesPP, dBytesPP, LeafCnt, Cnt, ResColorCnt, cData : Integer;
    pb : PByte;
    pc, pc1 : PFColor;
    Tree, DelNode : PColorNode;
    Reducible: TChildren;
    Pal : array of TFColorA;
    PalPtr : PFColorTable;
    Errors : TErrorMetrics;
    ColorFunc : function (Tree : PColorNode; c : PFColor; Data : Integer): Integer;
begin
//TimeReset;

sBytesPP := Src.Bpp shr 3;
If (Dst <> nil) then dBytesPP := Dst.Bpp shr 3
                else dBytesPP := sBytesPP;
If (sBytesPP in [0, 2]) or (dBytesPP in [0, 2]) then Exit;

If (dBytesPP = 1) and (ColorCnt > 256) then ColorCnt := 256;

FillChar(Reducible, SizeOf(TChildren), 0);
Tree := nil;

If ColorCnt > 2 then begin
  LeafCnt := 0;
  // building/reducing color tree
  For y:=0 to Src.Height-1 do begin
    pc := Src.Scanlines[y];
    For x:=0 to Src.Width-1 do begin
      If (sBytesPP <= 1) then pc1 := @Src.Colors[ PByte(pc)^ ]
                         else pc1 := pc;
      xAddColor(Tree, pc1, ColorBits, 0, LeafCnt, Reducible);
      While (not ColorCntBased) and (LeafCnt > ColorCnt) and
            (xReduceTree(Reducible, LeafCnt, ColorBits, ColorCnt)) do ;
      Inc(PByte(pc), sBytesPP);
    end;
  end;
  If ColorCntBased then
    While (LeafCnt > ColorCnt) do begin
      Cnt := MAXINT; DelNode := nil;
//      xReduceTree(Reducible, LeafCnt, ColorBits, ColorCnt);
      xFindRareColor(Tree, DelNode, Cnt);
      If (DelNode <> nil) then xDelNode(DelNode, LeafCnt)
                          else Break;
    end;
end else
  LeafCnt := 2;

Result := LeafCnt;

// filling palette
If (Dst = nil) or (Dst.Bpp > 8) then begin
  SetLength(Pal, LeafCnt); PalPtr := PFColorTable(Pal);
end else begin
  FillChar(Dst.Colors[0], SizeOf(TFColorTable), 0);
  PalPtr := Dst.Colors;
end;

If ColorCnt > 2 then begin
  ResColorCnt := 0;
  xFillPalette(Tree, PalPtr, ResColorCnt, DstColorDisp);
end else begin
  PalPtr[0].c := tfBlack; PalPtr[1].c := tfWhite; ResColorCnt := 2;
end;

If (Dst <> nil) and (Dst.Bpp = 8) then Dst.UpdateColors;

cData := 0;
If Dithering then begin
  If Dst <> nil then xErrInit(Errors, Src, Dst)
                else xErrInit(Errors, Src, Src);
//  ColorFunc := xGetColorFS;//xGetColorDA;
end else
  If ColorCnt > 2 then ColorFunc := xGetColorIndex
                  else ColorFunc := xErrNearestColor;

If (Dithering) or (ColorCnt <= 2) then begin
  Errors.ColorCnt := ResColorCnt; Errors.Pal := PalPtr;
  cData := Integer(@Errors);
end;

If (Dithering) then
  For y:=0 to Src.AbsHeight-1 do
    xLine_Dither(Errors, Tree, sBytesPP, dBytesPP)
else
  For y:=0 to Src.AbsHeight-1 do begin
    pc := Src.Scanlines[y];
    If (Dst <> nil) then pb := Dst.Scanlines[y]
                    else pb := PByte(pc);
    // most common case - 24->8
    If (sBytesPP > 1) and (dBytesPP = 1)  then
      For x:=0 to Src.Width-1 do begin
        pb^ := ColorFunc(Tree, pc, cData);
        Inc(PByte(pc), sBytesPP); Inc(pb);
      end
    else
      For x:=0 to Src.Width-1 do begin
        If (sBytesPP <= 1) then pc1 := @Src.Colors[ PByte(pc)^ ]
                           else pc1 := pc;
        i := ColorFunc(Tree, pc1, cData);
        If (dBytesPP = 1) then pb^ := i
                          else PFColor(pb)^ := Pal[i].c;
        Inc(PByte(pc), sBytesPP); Inc(pb, dBytesPP);
      end;
  end;

If (Dst = nil) and (Src.Bpp = 8) then begin
  FillChar(Src.Colors[0], SizeOf(TFColorTable), 0);
  Move(Pal[0], Src.Colors[0], ResColorCnt * 4);
  Src.UpdateColors;
end;

//TimeShowMks;

xDeleteTree(Tree);
end;


end.
