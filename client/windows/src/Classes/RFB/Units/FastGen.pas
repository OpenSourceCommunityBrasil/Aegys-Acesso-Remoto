unit FastGen; // FastDIB: sourceforge.net/projects/tfastdib

// texture generation, conv. to spheremap, force tiling
// (c) Sapersky <sapersky@gmail.com>
// note: 16 bpp here is hi-res grayscale, must be converted to 8 bpp for drawing

interface
Uses windows, uAegysUtils,
     FastDIB, FastFX, FastSize, FastBlend;
{$I platform.inc}     
Type
  TBorderProcMode = (bpmNone, bpmClip, bpmTile);
  TFunctionType = (ftSphere, ftCos);

procedure GenMapSpheres(Dest : TFastDIB; MinDiamK, MaxDiamK : Single; Steps : Integer;
                        Borders : TBorderProcMode = bpmClip; Func : TFunctionType = ftSphere);
function GenMapFractal(Dst : TFastDIB; Rough : Single;
                       MakeTile : Boolean = True; StartRandom : Boolean = False): Boolean;
function GenMapFractalEx(Src, Dst : TFastDIB; Rough : Single;
                         StartLevel : Single = 1; MakeTile : Boolean = True): Boolean;
procedure GenMapPerlin(Dst : TFastDIB; Cycles : Integer;
                       MakeTile : Boolean = False; HQScaling : Boolean = True);
procedure GenMapSinCos(Dst : TFastDIB; FormulaNum, XSteps, YSteps : Integer;
                       DistPower : Single = 0);
procedure GenMapSinCos2(Dst : TFastDIB; FormulaNum, XPer, YPer : Integer;
                        DistPower : Single = 0; NewStyle : Boolean = True);
procedure GenMapCells(Dst : TFastDIB; PointCnt, FormulaNum : Integer;
                      FastDistCalc : Boolean = True);

procedure GenLightCos(Dst : TFastDIB);
procedure GenLightEllipse(Bmp : TFastDIB; SizeX, SizeY : Integer);

procedure MakeSphereMap(Src, Dst : TFastDIB; Smooth : Boolean = True;
                        TileKoeff : Integer = 8); // 0 - no tiling
procedure MakeTiling(Src, Dst : TFastDIB);
procedure NormalizeFDIB16(Src : TFastDIB; Dst : TFastDIB = nil;
                          DstChannel : Integer = 0; DoNorm : Boolean = True);

implementation

// if Dst is not nil, result is copied to it
// otherwize all ops are done at Src
// Src = 16 bpp, Dst = [8, 16], [24, 32] (fills one channel)
procedure NormalizeFDIB16(Src : TFastDIB; Dst : TFastDIB = nil;
                          DstChannel : Integer = 0; DoNorm : Boolean = True);
Var x, y, w, h, dBytesPP : Integer;
    pw : PWord;
    pb : PByte;
    Min, Max, MM : Integer;
    v : Single;
begin
If Dst <> nil then begin
  w := Dst.Width - 1; h := Dst.Height - 1;
end else begin
  w := Src.Width - 1; h := Src.Height - 1;
end;
If DoNorm then begin
  Min:=Src.Pixels16[0,0]; Max:=Min;
  For y:=0 to h do begin
    pw := Src.Scanlines[y];
    For x:=0 to w do begin
      If pw^ > Max then Max:=pw^;
      If pw^ < Min then Min:=pw^;
      Inc(pw);
    end;
  end;
end else begin
  Min := 0; Max := 65535;
end;
MM := Max - Min;
If (MM = 0) then Exit;

If (Dst <> nil) then dBytesPP := Dst.Bpp shr 3
                else dBytesPP := 2;

If (dBytesPP = 2) then v := 65535 / mm
                  else v := 255 / mm;

If (dBytesPP <= 2) then DstChannel := 0 else
  If DstChannel > dBytesPP then DstChannel := dBytesPP;

For y:=0 to h do begin
  pw := Src.Scanlines[y];
  If (Dst <> nil) then pb := Dst.Scanlines[y]
                  else pb := PByte(pw);
  Inc(pb, DstChannel);
  If (dBytesPP = 2) then
    For x:=0 to w do begin
//      PWord(pb)^ := ((pw^ - Min) * 65535) div mm;
      PWord(pb)^ := Round((pw^ - Min) * v);
      // for [some] modern CPUs float mul + Round seems to be faster than div
      Inc(pw); Inc(PWord(pb));
    end
  else
    For x:=0 to w do begin
//      pb^:=((pw^ - Min) * 255) div mm;
      pb^ := Round((pw^ - Min) * v);
      Inc(pw); Inc(pb, dBytesPP);
    end;
end;
end;


procedure xNormalizeFDIB8(Src : TFastDIB);
Var x, y, w, h : Integer;
    pb : PByte;
    Min, Max, MM : Integer;
begin
w := Src.Width - 1; h := Src.Height - 1;
Min := Src.Pixels8[0,0]; Max := Min;
For y:=0 to h do begin
  pb := Src.Scanlines[y];
  For x:=0 to w do begin
    If pb^ > Max then Max := pb^;
    If pb^ < Min then Min := pb^;
    Inc(pb);
  end;
end;
MM := Max - Min;
If (MM <> 0) then begin
  MM := 65536 div mm;
  For y:=0 to h do begin
    pb := Src.Scanlines[y];
    For x:=0 to w do begin
      pb^ := ((pb^ - Min) * MM) shr 8;
      Inc(pb);
    end;
  end;
end;
end;


procedure xGenSphere(Const Dest : TFastDIB; Diam, Rad : Integer);
Var x, y, xm, ym, s, d : Integer;
    pw : PWord;
begin
s:=Sqr(Rad); ym:=-Rad;
For y:=0 to Diam-1 do begin
  xm:=-Rad; pw := Dest.Scanlines[y];
  For x:=0 to Diam-1 do begin
    d:=s - (xm * xm + ym * ym); If d < 0 then d := 0;
    pw^:=d div 16;
    Inc(xm); Inc(pw);
  end;
  Inc(ym);
end;
end;


procedure xGetSinCosTable(Table : PInteger; Count : Integer;
                          StartAngle, MulValue : Single; Sinus : Boolean = True);
Var n : Integer;
    a, ast, v : Single;
begin
a := StartAngle; ast := Pi * 2 / Count;
For n:=0 to Count-1 do begin
  If Sinus then v := Sin(a) else v := Cos(a);
  Table^ := Round(v * MulValue);
  Inc(Table);
  a:=a + ast;
end;
end;


procedure xGenCos(Const Dest : TFastDIB; Diam{MapSize}, Rad : Integer);
Var x, y, sRad  : Integer;
    pw : PWord;
    SCTable : TSinCosTable;
begin
sRad := Sqr(Rad);

SetLength(SCTable, Diam);
Math_GetSinCosTable(@SCTable[0], Diam, 256, -Pi);
For x := 0 to Diam - 1 do Inc(SCTable[x].iCos, 256);

For y:=0 to Diam-1 do begin
  pw := Dest.Scanlines[y];
  For x:=0 to Diam-1 do begin
//    d := Round( s * ( (Cos(xm / Rad * Pi) + 1) * (Cos(ym / Rad * Pi) + 1) ) );
    pw^ := ( Rad * SCTable[x].iCos * SCTable[y].iCos ) shr 16;
    Inc(pw);
  end;
end;
end;

procedure GenLightCos(Dst : TFastDIB);
Var fd : TFastDIB;
    d : Integer;
begin
fd := TFastDIB.Create(Dst, False, 16);
d := Num_Min(fd.Width, fd.Height);
xGenCos(fd, d, d div 2);
NormalizeFDIB16(fd, Dst);
end;

// based on RenderLightmap code
procedure GenLightEllipse(Bmp : TFastDIB; SizeX, SizeY : Integer);
var
  x,y,yy,rx,ry,fx,fy,i: Integer;
  pb: PByte;
begin
  rx := Bmp.Width shr 1; ry := Bmp.Height shr 1;
  fx := Round(65536/(SizeX/((256/(SizeX/2))*2)));
  fy := Round(65536/(SizeY/((256/(SizeY/2))*2)));
    // оставил как в оригинале, хотя можно слегка :) упростить, конечно

  for y:=0 to Bmp.Height-1 do begin
    pb := Bmp.Scanlines[y];
    yy := (y-ry)*(y-ry);
//    yy := Abs(y-ry);
    for x:=0 to Bmp.Width-1 do
    begin
      i:=((x-rx)*(x-rx)*fx + yy*fy) shr 16;
//      i:=(Abs(x-rx)*fx + yy*fy) shr 8;
      if i>255 then i:=255;
      i:=i xor -1; // инвертирование
      pb^:=i;
      Inc(pb);
    end;
  end;
end;

{$IFDEF CPUX86}
procedure xAddLineMMX(Src, Dest : Pointer; Size:Integer);
// Src  = eax, Dest = edx, Size = ecx
asm
  @quads:
    db $0F,$6F,$00           /// movq    mm0,[eax]
    db $0F,$6F,$0A           /// movq    mm1,[edx]
    db $0F,$DD,$C1           /// paddusw mm0,mm1
    db $0F,$7F,$02           /// movq    [edx],mm0
    add eax,8
    add edx,8
    dec ecx
  jnz @quads
end;
{$ENDIF}
{$IFDEF CPUX64}
procedure xAddLineMMX(Src, Dest : Pointer; Size:Integer);
// Src = rcx, Dest = rdx, Size = r8
asm
  @quads:
    movq    mm0,[rcx]
    movq    mm1,[rdx]
    paddusw mm0,mm1
    movq    [rdx],mm0
    add rcx,8
    add rdx,8
    dec r8
  jnz @quads
end;
{$ENDIF}

procedure xApplyMap16(Const Src, Dest : TFastDIB; Diam, dx, dy : Integer);
Var x, y, d4 : Integer;
    sp, dp : PWord;
begin
d4 := Diam div 4;
For y:=0 to Diam-1 do begin
  sp:=Src.Scanlines[y]; dp:=@Dest.Pixels16[dy,dx];
  xAddLineMMX(sp, dp, d4);
  Inc(dy);
end;
end;


procedure xApplyMap16Clip(Const Src, Dest : TFastDIB; Diam, dx, dy : Integer);
Var x, y, xr, yr, iw, ih, sx, sw, ddx : Integer;
    sp, dp : PWord;
begin
iw := Dest.Width;// - 1;
ih := Dest.Height;// - 1;
For y:=0 to Diam-1 do begin
  yr := dy + y;
  If (yr>=0) and (yr<ih) then begin
    sx := 0; sw := Diam; ddx := dx;

    xr := dx + Diam;
    If dx<0 then begin sx := -dx; sw := xr; ddx := 0; end;
    If xr > iw then begin sw := iw - dx; end;

    sp:=@Src.Pixels16[y, sx];
    dp:=@Dest.Pixels16[yr, ddx];
    xAddLineMMX(sp, dp, sw div 4);
  end;
end;
end;


procedure xApplyMap16Tile(Const Src, Dest : TFastDIB; Diam, dx, dy{, DstWidth} : Integer);
Var x, y, xr, yr, iw, ih, sx, sw, ddx : Integer;
    sp, dp : PWord;
begin
iw := {DstWidth;}Dest.Width;// - 1;
ih := Dest.Height;// - 1;
For y:=0 to Diam-1 do begin
  yr := dy + y; Num_CycleRange(yr, 0, ih-1);

  sx := 0; sw := Diam; ddx := dx;

  xr := dx + Diam;
  If dx<0 then begin
    sp:=@Src.Pixels16[y, 0];
    dp:=@Dest.Pixels16[yr, iw + dx];
    xAddLineMMX(sp, dp, (-dx) div 4);
    sx := -dx; sw := xr; ddx := 0;
  end;
  If xr > iw then begin
//    xr := dx + Diam;
    sp:=@Src.Pixels16[y, iw - dx];
    dp:=@Dest.Pixels16[yr, 0];
    xAddLineMMX(sp, dp, (xr - iw) div 4);
    sw := iw - dx;
  end;

  sp:=@Src.Pixels16[y, sx];
  dp:=@Dest.Pixels16[yr, ddx];
  xAddLineMMX(sp, dp, sw div 4);
end;
end;


// Dest can be 8,16 bpp;
// for tiling image width must be aligned to 4
// http://www.lighthouse3d.com/opengl/terrain/index.php3?circles
procedure GenMapSpheres(Dest : TFastDIB; MinDiamK, MaxDiamK : Single;
                        Steps : Integer; Borders : TBorderProcMode = bpmClip;
                        Func : TFunctionType = ftSphere);
Const OneDiamStep = 16;
Var n, m, c, x, y, d, s, r, msMin, MaxDiam, iDiam, iSt, w, w1 : Integer;
    TempMap, TempBuf : TFastDIB;
    InRange : Boolean;
begin
msMin := Num_Min(Dest.Width, Dest.Height);
MaxDiam := Round(MaxDiamK * msMin); MaxDiam := (MaxDiam div 4) * 4;
iDiam := MaxDiam shl 8;

If Steps <= OneDiamStep then begin
  s := 1; c := Steps;
end else begin
  s := Steps div OneDiamStep; c := OneDiamStep;
end;
iSt := Round((MaxDiamK - MinDiamK) * msMin / s * 256);

w := Trunc(Dest.Width * 0.25) * 4;
//w := Dest.Width;
If (Dest.Bpp = 8) then begin
  If (w <> Dest.Width) then w1 := w + 4
                       else w1 := w;
  TempMap := TFastDIB.Create(w1, Dest.Height, 16, True)
// UseGDI = True because CreateDIBSection usually gives fine alignment
// (important for MMX)
end else begin
  TempMap := Dest; TempMap.ClearB(0);
end;  

TempBuf := TFastDIB.Create(MaxDiam, MaxDiam, 16, True);

For n:=0 to s-1 do begin
//  d:=Round(Diam * (0.8 + 0.2*Random));
  d := (iDiam * (200 + Random(55))) shr 16;
  d := (d div 4) * 4;
  r:=d div 2;

  If r > 0 then begin
    If Func = ftSphere then xGenSphere(TempBuf, d, r)
                       else xGenCos(TempBuf, d, r);

    For m:=0 to c-1 do begin
      x:=Random(w{Dest.Width}); y:=Random(Dest.Height); // get center of sphere
      Dec(x,r); Dec(y,r); // set x,y to left,top of SphereMap
      x := (x div 4) * 4; y := (y div 4) * 4; // align values
      InRange:=(x>0) and (y>0) and (x<w{Dest.Width}-d-1) and (y<Dest.Height-d-1);
      If InRange then begin
        xApplyMap16(TempBuf, TempMap, d, x, y)
      end else begin
        If (Borders = bpmClip) then
          xApplyMap16Clip(TempBuf, TempMap, d, x, y)
        else
          If (Borders = bpmTile) then
            xApplyMap16Tile(TempBuf, TempMap, d, x, y);
      end;

    end;

    EMMS;
  end;
//  Diam:=Diam - dst;
  iDiam := iDiam - iSt;
end;

TempBuf.Free;

If (Dest.Bpp = 8) then begin
  NormalizeFDIB16(TempMap, Dest);
  TempMap.Free;
end else
  NormalizeFDIB16(Dest);

end;




procedure xDisplaceA(Const Map : TFastDIB; stx, sty, disp: Integer);
Var x0, x1, x2, y, d2 : Integer;
    Lin0, Lin1, Lin2: PLine16;
begin
d2 := disp div 2;
y := 0;
Repeat
  Lin0 := Map.Scanlines[y];
  Lin1 := Map.Scanlines[y + sty div 2];
  Lin2 := Map.Scanlines[y + sty];

  x0 := 0; x1 := stx div 2; x2 := stx;
  Repeat
    Lin1[x1] := ( (Lin0[x0] + Lin0[x2] + Lin2[x0] + Lin2[x2]) div 4 ) + (Random(disp) - d2); // e
    Lin1[x0] := ( (Lin0[x0] + Lin2[x0]) div 2) + (Random(disp) - d2); //f
    Lin0[x1] := ( (Lin0[x0] + Lin0[x2]) div 2) + (Random(disp) - d2); //g
//    Lin1[x2]:=( (Lin0[x2] + Lin2[x2]) div 2) + (Random(disp) - d2); //h
//    Lin2[x1]:=( (Lin2[x0] + Lin2[x2]) div 2) + (Random(disp) - d2); //i

    If (x2 = Map.Width - 1) then
      Lin1[x2]:=( (Lin0[x2] + Lin2[x2]) div 2) + (Random(disp) - d2); //h

    If (y + sty = Map.Height - 1) then
      Lin2[x1]:=( (Lin2[x0] + Lin2[x2]) div 2) + (Random(disp) - d2); //i

    Inc(x0, stx); Inc(x1, stx); Inc(x2, stx);
  Until (x0 = Map.Width - 1);

  Inc(y, sty);
Until (y = Map.Height - 1);
end;


procedure xDisplaceB(Const Map : TFastDIB; stx, sty, disp: Integer;
                     MakeTile, CalcCenter : Boolean);
Var x0, x1, x2, y, d2, i : Integer;
    Lin0, Lin1, Lin2: PLine16;
begin
d2 := disp div 2;
y := 0;
Repeat
  Lin0 := Map.Scanlines[y];
  Lin1 := Map.Scanlines[y + sty div 2];
  Lin2 := Map.Scanlines[y + sty];

  x0 := 0; x1 := stx div 2; x2 := stx;
  Repeat
    If CalcCenter then begin
      i := ( (Lin0[x0] + Lin0[x2] + Lin2[x0] + Lin2[x2]) div 4 ) + (Random(disp) - d2); // e;
      If i < 0 then i := 0 else If i > 65535 then i := 65535;
      Lin1[x1] := i;
    end;
    i := ( (Lin0[x0] + Lin2[x0]) div 2) + (Random(disp) - d2); //f
    If i < 0 then i := 0 else If i > 65535 then i := 65535;
    Lin1[x0] := i;
    i := ( (Lin0[x0] + Lin0[x2]) div 2) + (Random(disp) - d2); //g
    If i < 0 then i := 0 else If i > 65535 then i := 65535;
    Lin0[x1] := i;
//    Lin1[x2]:=( (Lin0[x2] + Lin2[x2]) div 2) + (Random(disp) - d2); //h
//    Lin2[x1]:=( (Lin2[x0] + Lin2[x2]) div 2) + (Random(disp) - d2); //i

    If (x2 = Map.Width - 1) then
      If MakeTile then Lin1[x2] := Lin1[0] else begin
        i := ( (Lin0[x2] + Lin2[x2]) div 2) + (Random(disp) - d2); //h
        If i < 0 then i := 0 else If i > 65535 then i := 65535;
        Lin1[x2] := i;
      end;

    If (y + sty = Map.Height - 1) then
      If MakeTile then Lin2[x1] := Map.Pixels16[0, x1] else begin
        i := ( (Lin2[x0] + Lin2[x2]) div 2) + (Random(disp) - d2); //i
        If i < 0 then i := 0 else If i > 65535 then i := 65535;
        Lin2[x1] := i;
      end;

    Inc(x0, stx); Inc(x1, stx); Inc(x2, stx);
  Until (x0 >= Map.Width - 1);

  Inc(y, sty);
Until (y >= Map.Height - 1);
end;


// Dst is 8, 16 bpp (square, binary size)
// http://www.lighthouse3d.com/opengl/terrain/index.php3?mpd2
function GenMapFractal(Dst : TFastDIB; Rough : Single;
                       MakeTile : Boolean = True; StartRandom : Boolean = False): Boolean;
Var w, h, d, d2, stx, sty, k : Integer;
    pw : Single;
    Map : TFastDIB;
begin
Result := False;
w := Dst.Width; h := Dst.Height;
If (w <> h) or ((Dst.Bpp <> 8) and (Dst.Bpp <> 16)) then Exit;

// temp map must be created because of size increment (binary + 1)
Map := TFastDIB.Create(w+1, h+1, 16, False);

d := 32767; d2:=d div 2;
If (StartRandom) then begin
  Map.Pixels16[0,0]:=(Random(d) - d2);
  Map.Pixels16[0,w]:=(Random(d) - d2);
  Map.Pixels16[h,0]:=(Random(d) - d2);
  Map.Pixels16[h,w]:=(Random(d) - d2);
end else begin
  Map.Pixels16[0,0]:=d; Map.Pixels16[0,w]:=d;
  Map.Pixels16[h,0]:=d; Map.Pixels16[h,w]:=d;
//Map.Pixels16[h div 2,w div 2]:=d;
end;

stx := w; sty := h;

If Rough = 0 then pw := 1
             else pw := Exp(-0.693147 * Rough);
// same as pw := Power(2.0, -Rough);
k := Round( pw * $FFFF );

//k := Round( Math_Power(2.0, -Rough) * $FFFF );

Repeat
  If (MakeTile) or (Rough < 0.3) then xDisplaceB(Map, stx, sty, d, MakeTile, True)
                                 else xDisplaceA(Map, stx, sty, d);
//  xDisplaceB(Map, stx, sty, d, MakeTile, stx <> w);
  stx := stx div 2; sty := sty div 2;
  d := d * k shr 16;
Until (stx = 1) and (sty = 1);

NormalizeFDIB16(Map, Dst);

Result := True;
end;


procedure xFastResizeTo16(Src, Dst : TFastDIB; Channel : Integer = 0);
var
  x,y,xp,yp,sx,sy,w,h,sBytesPP: Integer;
  Line: PLine8;
  pc: PWord;
begin
sBytesPP := Src.Bpp shr 3;
w := Dst.Width; h := Dst.AbsHeight;
sx:=(Src.Width shl 16) div w;
sy:=(Src.AbsHeight shl 16) div h;
yp:=0;
for y:=0 to h-1 do begin
  Line := Src.Scanlines[yp shr 16];
  pc := Dst.Scanlines[y];
  xp:=0;
  If (sBytesPP = 2) then
    for x:=0 to w-1 do begin
      pc^ := PLine16(Line)[xp shr 16];
      Inc(pc); Inc(xp,sx);
    end
  else
    for x:=0 to w-1 do begin
      pc^ := Line[(xp shr 16) * sBytesPP + Channel] shl 8;
      Inc(pc); Inc(xp,sx);
    end;
  Inc(yp,sy);
end;
end;


// GenMapFractal with ability to set custom source map (Src)
// unfinished
// Dst is 8, 24, 32 (binary size) or 16 bpp (binary + 1 size)
// Src can be nil (no Src map) or 8, 24, 32 bpp, size smaller than Dst
function GenMapFractalEx(Src, Dst : TFastDIB; Rough : Single;
                         StartLevel : Single = 1; MakeTile : Boolean = True): Boolean;
Var x, y, w, h, d, d2, stx, sty, k, sMin,
    m, n, nstx, nsty, nd, c, sChannel : Integer;
    pw : Single;
    Map : TFastDIB;
begin
Result := False;
w := Dst.Width; h := Dst.Height;
//If (w <> h) then Exit;

x := w; y := h;
If (Dst.Bpp = 16) then begin Dec(x); Dec(y); end;
If ((x and (x-1)) <> 0) or ((y and (y-1)) <> 0) then Exit;
// binary size test

If (Dst.Bpp = 16) then Map := Dst else
  Map := TFastDIB.Create(w+1, h+1, 16, False);

nd := 32767;
nstx := w; nsty := h;
If Rough = 0 then pw := 1
             else pw := Exp(-0.693147 * Rough); // same as pw := Power(2.0, -Rough);
k := Round( pw * $FFFF );

If (Src <> nil) then begin
  sMin := Num_Min(Dst.Width div Src.Width, Dst.Height div Src.Height);
//  Map.SaveToFile('aaa.bmp');
  Repeat
    nstx := nstx div 2; nsty := nsty div 2;
    nd := nd * k shr 16;
  Until (nstx <= sMin);
end;

If (Dst.Bpp = 16) then c := 1
                  else c := Dst.Bpp shr 3;

For n:=0 to c-1 do begin
  stx := nstx; sty := nsty;
  d := nd; d2 := d div 2;

  If (Src <> nil) then begin
    If (Src.Bpp <= 16) then sChannel := 0
                       else sChannel := n;
    xFastResizeTo16(Src, Map, sChannel)
  end else
    For m := 0 to 3 do begin
      x := SimpleQuad[m].x * w; y := SimpleQuad[m].y * w;
      Map.Pixels16[x,y] := {d + }Round(d * StartLevel);
    end;

  Repeat
    If (MakeTile) or (Rough < 0.3) then xDisplaceB(Map, stx, sty, d, MakeTile, True)
                                   else xDisplaceA(Map, stx, sty, d);
  //  xDisplaceB(Map, stx, sty, d, MakeTile, stx <> w);
    stx := stx div 2; sty := sty div 2;
    d := d * k shr 16;
  Until (stx <= 1) and (sty <= 1);

  If (Dst.Bpp <> 16) then NormalizeFDIB16(Map, Dst, n)
                     else NormalizeFDIB16(Dst);
end;

If (Dst.Bpp <> 16) then Map.Free;

Result := True;
end;


function xDrawAlpha16(Src, Dst : TFastDIB; AlphaConst : Integer): Boolean;
Var x, y, a, ia : Integer;
    sw, dw : PWord;
begin
Result := (Src.Bpp in [8, 16]) and (Dst.Bpp = 16);
If (not Result) or (AlphaConst = 0) then Exit;
a := AlphaConst; ia := 255 - a;
for y:=0 to Src.AbsHeight-1 do begin
  sw := Src.Scanlines[y]; dw := Dst.Scanlines[y];
  If Src.Bpp = 8 then
    For x:=0 to Src.Width-1 do begin
      dw^ := ((PByte(sw)^ * a) shl 8 + dw^ * ia) shr 8;
      Inc(PByte(sw)); Inc(dw);
    end
  else
    For x:=0 to Src.Width-1 do begin
      dw^ := (sw^ * a + dw^ * ia) shr 8;
      Inc(sw); Inc(dw);
    end;
end;
end;

// http://freespace.virgin.net/hugo.elias/models/m_perlin.htm
// although this is not "classical" implementation
// uses multi-pass blending instead of calculating pixel value from
// all "octaves" at one pass (not very fast, but simple)
procedure GenMapPerlin(Dst : TFastDIB; Cycles : Integer;
                       MakeTile : Boolean = False; HQScaling : Boolean = True);
Const
  MaxCycles = 16;
Var fd, fd1, sub : TFastDIB;
    x,y,w,h,s,v,c : Integer;
    pb : PByte;
    RandX, RandY : array [0..MaxCycles-1] of Integer;

  procedure Scale(Src, Dst : TFastDIB);
  begin
//  If HQScaling then SmoothResize(Src, Dst, rfHermite) // doesn't tile
//               else Bilinear8(Src, Dst);
  Bilinear8(Src, Dst);
  end;

  procedure Blend(Src, Dst : TFastDIB; Alpha : Integer);
  begin
  If Alpha > 255 then Alpha := 255;
  If (Dst.Bpp = 16) then xDrawAlpha16(Src, Dst, Alpha)
                    else AlphaBlend(Dst, Src, Dst, Alpha);
  end;

begin
If (not (Dst.Bpp in [8, 16])) then Exit;
fd := TFastDIB.Create(Dst, False, 8);
If (Cycles > 1) then fd1 := TFastDIB.Create(Dst, False, 8);
w := Dst.Width; h := Dst.Height;

For y:=0 to h-1 do begin // fill fd with random noize
  pb := fd.Scanlines[y];
  For x:=0 to w-1 do begin
    pb^ := Random(255); Inc(pb);
  end;
end;

sub := TFastDIB.Create;

If Cycles > MaxCycles then Cycles := MaxCycles;
s := w div 2;
For x:=0 to Cycles-1 do begin
  RandX[x] := Random(w-s); RandY[x] := Random(h-s);
  s := s shr 1;
end;

s := w div (1 shl Cycles); If s < 1 then s := 1;
v := 256; c := Cycles-1;
Repeat
//  x := Random(w-s); y := Random(h-s);
  x := RandX[c]; y := RandY[c];
  sub.SetSubset(fd, x, y, s, s); // select subset from fd (s * s) size

  If MakeTile then begin
    For x:=0 to s-1 do sub.Pixels8[x, s-1] := sub.Pixels8[x, 0];
    For x:=0 to s-1 do sub.Pixels8[s-1, x] := sub.Pixels8[0, x];
  end;

  If (v = 256) and (Dst.Bpp = 8) then Scale(Sub, Dst) else begin
    Scale(Sub, fd1);    // stretch it (with bilinear filtering) to full size
    Blend(fd1, Dst, v); // blend with previous results
  end;

  v := v div 2; s := s * 2; Dec(c);
Until (s >= w);
// last iteration - no need for scale, just add random map
Blend(fd, Dst, v);


sub.Free;
If (Cycles > 1) then fd1.Free;
fd.Free;
If (Dst.Bpp = 16) then NormalizeFDIB16(Dst)
                  else xNormalizeFDIB8(Dst);
end;


// generates regular image (8, 16 bpp), mixing sin/con functions
// Dst can contain distortion map on input (a sort of bump-mapping used)
// FormulaNum: 0 - slanted grid, 1 - big balls, 2 - small balls, 3 - more dense balls,
// 4 - slanted stripes, 5 - hor. stripes, 6 - vert. stripes
// 7 - unknown stuff, 8 - circles (doesn't tile well)
// XSteps, YSteps - element size, DistPower - distortion koeff
// based on code by Zer0 (zeexeen@mail.ru)
procedure GenMapSinCos(Dst : TFastDIB; FormulaNum, XSteps, YSteps : Integer;
                       DistPower : Single = 0);
Var x, y, w, h, i, c, x1, y1, XPer, YPer, StartX, StartY,
    DPower, DMid, BytesPP, MaxPVal, MaxPVal2, Shift : Integer;
    xa, ya : Integer; 
    XTable, YTable : TSinCosTable;
    pb, pbi : PByte;
    DMap : TFastDIB;
begin
BytesPP := Dst.Bpp shr 3;
DMap := TFastDIB.Create(Dst, True);
w := Dst.Width-1; h := Dst.Height-1;

XSteps := Num_Binary(XSteps); YSteps := Num_Binary(YSteps);
XPer := Num_Binary(w+1); YPer := Num_Binary(h+1);

StartX := 0; StartY := 0;
If (BytesPP = 2) and (FormulaNum in [2,3]) then begin
  Shift := 14; MaxPVal := (1 shl 15)-1;
end else begin
  Shift := Dst.Bpp; MaxPVal := (1 shl Shift)-1;
end;
//MaxPVal := (1 shl Shift)-1;
MaxPVal2 := ((1 shl Dst.Bpp)-1) div 2;
Case FormulaNum of
  0..3 :
      begin
        SetLength(XTable, XPer);
        Math_GetSinCosTable(@XTable[0], XPer, MaxPVal);
        If (YPer = XPer) then YTable := XTable else begin
          SetLength(YTable, YPer);
          Math_GetSinCosTable(@YTable[0], YPer, MaxPVal);
        end;
      end;
  4..6 :
      begin
        SetLength(XTable, XPer + YPer);
        Math_GetSinCosTable(@XTable[0], XPer + YPer, MaxPVal);
      end;
  7 : begin
        c := XPer + YPer;
        SetLength(XTable, c); SetLength(YTable, YPer);
        Math_GetSinCosTable(@XTable[0], c, MaxPVal);
        Math_GetSinCosTable(@YTable[0], YPer, MaxPVal);
      end;
  8 : begin
        c := XPer;//Math_Round( Sqrt( Sqr(XPer) + Sqr(XPer) ) );
        SetLength(XTable, c);
        Math_GetSinCosTable(@XTable[0], c, MaxPVal);
        StartX := -XPer; StartY := -YPer;
      end;
end;

If (BytesPP = 1) then DPower := Round(DistPower * 65536)
                 else DPower := Round(DistPower * 256);
DPower := DPower * ((XPer+YPer) div 512);
DMid := (MaxPVal2 * DPower shr 16);
y1 := StartY;
For y := 0 to h do begin
  pb := Dst.Scanlines[y]; x1 := StartX;
  pbi := DMap.Scanlines[h-y]; Inc(pbi, w * BytesPP);
  For x := 0 to w do begin
    If (DPower <> 0) then begin
      If (BytesPP = 1) then begin
        xa := (x1 + (pb^  * DPower shr 16) - DMid) and (XPer - 1);
        ya := (y1 + (pbi^ * DPower shr 16) - DMid) and (YPer - 1);
      end else begin
        xa := (x1 + (PWord(pb)^  * DPower shr 16) - DMid) and (XPer - 1);
        ya := (y1 + (PWord(pbi)^ * DPower shr 16) - DMid) and (YPer - 1);
      end;
    end else begin
      xa := x1; ya := y1;
    end;

    Case FormulaNum of
      0 : i := MaxPVal - Abs( (XTable[xa].iSin + YTable[ya].iSin) div 2 );
      1 : i := MaxPVal2 + ( (XTable[xa].iSin + YTable[ya].iSin) div 4 );
      2 : begin
            c := XTable[xa].iSin * YTable[ya].iSin;
            If c < 0 then c := 0;
            i := c shr Shift;
          end;
      3 : i := Abs(XTable[xa].iSin * YTable[ya].iSin) shr Shift;
      4 : i := MaxPVal - Abs( XTable[xa + ya].iSin );
      5 : i := MaxPVal - Abs( XTable[ya].iSin );
      6 : i := MaxPVal - Abs( XTable[xa].iSin );
      7 : i := MaxPVal2 + ( Abs( XTable[xa + ya].iSin + YTable[ya].iSin ) div 4 );
      8 : begin
            //Inc(xa, 128); Inc(ya, 128);
            c := Round( Sqrt( xa * xa + ya * ya ) ) and (XPer-1);
            i := MaxPVal - Abs( XTable[ c ].iCos );
          end;
    end;
    If (BytesPP = 1) then pb^ := i
                     else PWord(pb)^ := i;
    Inc(x1, XSteps); If (x1 >= XPer) then x1 := StartX;
    Inc(pb, BytesPP); Dec(pbi, BytesPP);
  end;
  Inc(y1, YSteps); If y1 >= YPer then y1 := StartY;
end;

DMap.Free;
end;

// old version for bk. compability
// 8 bpp only, 10% faster
procedure GenMapSinCos2(Dst : TFastDIB; FormulaNum, XPer, YPer : Integer;
                        DistPower : Single = 0; NewStyle : Boolean = True);
Var x, y, w, h, c, x1, y1, xadd, yadd, xk, yk, StartX, StartY,
    DPower, DMid : Integer;
//    xa, ya : Byte;
    xa, ya : Integer; // <- this is needed for FormulaNum = 8
    XTable, YTable : TSinCosTable;
    pb, sb1 : PByte;
    DMap : TFastDIB;
begin
DMap := TFastDIB.Create(Dst, True);
w := Dst.Width-1; h := Dst.Height-1;

If NewStyle then begin
//  xk := 1; yk := 1;
//  XPer := Num_Binary(XPer); YPer := Num_Binary(YPer);

  x := Num_Binary(w+1); y := Num_Binary(h+1);
  xk := Num_Binary(XPer); yk := Num_Binary(YPer);
  XPer := x; YPer := y;
end else begin
  xk := 256 div XPer; yk := 256 div YPer;
  XPer := 256; YPer := 256;
end;

StartX := 0; StartY := 0;
Case FormulaNum of
  0..3 :
      begin
        SetLength(XTable, XPer);
        Math_GetSinCosTable(@XTable[0], XPer, 255);
        If (YPer = XPer) then YTable := XTable else begin
          SetLength(YTable, YPer);
          Math_GetSinCosTable(@YTable[0], YPer, 255);
        end;
      end;
  4..6 :
      begin
        SetLength(XTable, XPer + YPer);
        Math_GetSinCosTable(@XTable[0], XPer + YPer, 255);
      end;
  7 : begin
        c := XPer + YPer;
        SetLength(XTable, c); SetLength(YTable, YPer);
        Math_GetSinCosTable(@XTable[0], c, 255);
        Math_GetSinCosTable(@YTable[0], YPer, 255);
      end;
  8 : begin
        c := XPer;//Math_Round( Sqrt( Sqr(XPer) + Sqr(XPer) ) );
        SetLength(XTable, c);
        Math_GetSinCosTable(@XTable[0], c, 255);
        StartX := -XPer; StartY := -YPer;
      end;
end;

DPower := Round(DistPower * 65536);
If NewStyle then DPower := DPower * ((XPer+YPer) div 512);
DMid := (128 * DPower shr 16);
y1 := StartY;
For y := 0 to h do begin
  pb := Dst.Scanlines[y]; x1 := {0;}StartX;
  sb1 := @DMap.Pixels8[h-y, w];
  For x := 0 to w do begin

    If (DPower <> 0) then begin
      xadd := (pb^ * DPower shr 16) - DMid;
      yadd := (sb1^ * DPower shr 16) - DMid;

      xa := x1 + xadd;
      ya := y1 + yadd;
{
      xadd := pb^ - 128;
      yadd := DistMap.Pixels8[h-y, w-x] - 128;
      xa := x1 + xadd * DistPower;
      ya := y1 + yadd * DistPower;
}
    end else begin
      xa := x1; ya := y1;
    end;

    xa := xa and (XPer - 1);
    ya := ya and (YPer - 1);

    Case FormulaNum of
      0 : pb^ := 255 - Abs( (XTable[xa].iSin + YTable[ya].iSin) div 2 );
      1 : pb^ := 128 + ( (XTable[xa].iSin + YTable[ya].iSin) div 4 );
      2 : begin
            c := XTable[xa].iSin * YTable[ya].iSin;
            If c < 0 then c := 0;
            pb^ := c shr 8;
          end;
      3 : pb^ := Abs(XTable[xa].iSin * YTable[ya].iSin) shr 8;
      4 : pb^ := 255 - Abs( XTable[xa + ya].iSin );
      5 : pb^ := 255 - Abs( XTable[ya].iSin );
      6 : pb^ := 255 - Abs( XTable[xa].iSin );
      7 : pb^ := 128 + ( Abs( XTable[xa + ya].iSin + YTable[ya].iSin ) div 4 );
      8 : begin
            Inc(xa, 128); Inc(ya, 128);
            c := Round( Sqrt( xa * xa + ya * ya ) );
            While c >= XPer do Dec(c, XPer);
            pb^ := 255 - Abs( XTable[ c ].iCos );
          end;
    end;

    Inc(x1, xk); If x1 >= XPer then x1 := StartX;//0;
    Inc(pb); Dec(sb1);
  end;
  Inc(y1, yk); If y1 >= YPer then y1 :=  StartY;//0;
end;

DMap.Free;
end;


// http://www.blackpawn.com/texts/cellular/default.html
// 8, 16 bpp
// FastDistCalc - calc distances for every 4-th pixel, much faster,
// but produces artefacts sometimes
procedure GenMapCells(Dst : TFastDIB; PointCnt, FormulaNum : Integer;
                      FastDistCalc : Boolean = True);
Type
//  TDistType = Integer;
  TDistType = DWord;
//  TDistType = Word;

  PDistSet = ^TDistSet;
  TDistSet = array [0..3] of TDistType;

Var n, x, y, w, h, w2, h2, dy, x2, y2, tix, Cnt, ti, bi : Integer;
    pt : DPointArr;
    ptYOffs : DIntArr;
    pb : PByte;
    pi, pi1, pw : PWord;
    fd : TFastDIB;
    dsv, dsv1 : TDistSet;
    DLut : DWordArr;
    Dist : array of TDistSet;
    MaxD, MinD : TDistType;
    K : Single;


  procedure GetCloseSet(x, w2 : Integer; pts : PPoint;
                        ds : PDistSet; yOffs : PInteger);
  Var n, dx : Integer;
      d : TDistType;
  begin
  dx := Abs(x - pts.x{pt[0].x}); If dx > w2 then dx := w - dx;
  ds[0] := DLut[yOffs^ + dx];
  Inc(yOffs); Inc(pts);
  ds[1] := 65535; ds[2] := 65535; ds[3] := 65535;

  For n := 1 to PointCnt-1 do begin
    dx := Abs(x - pts.x);
    If dx > w2 then dx := w - dx;
    d := DLut[yOffs^ + dx];

    If (d < ds[0]) then begin
      ds[3] := ds[2]; ds[2] := ds[1]; ds[1] := ds[0]; ds[0] := d;
    end else
      If (d < ds[1]) then begin
        ds[3] := ds[2]; ds[2] := ds[1]; ds[1] := d;
      end else
        If (d < ds[2]) then begin
          ds[3] := ds[2]; ds[2] := d;
        end else
          If (d < ds[3]) then ds[3] := d;
    Inc(yOffs); Inc(pts);
  end;
  end;


  procedure GetCloseLine(y : Integer; yOffs : PInteger; ds : PDistSet);
  Var n, x, x2, dy : Integer;
      yStart : PInteger;
      ptStart : PPoint;
  begin
  yStart := yOffs;
  For n:=0 to PointCnt-1 do begin
    dy := Abs(y - pt[n].y);
    If dy > h2 then dy := h - dy;
    yOffs^ := dy * (w2 + 1); Inc(yOffs);
//    ptYOffs[n] := dy * (w2 + 1);
  end;

  ptStart := @pt[0];
  For x:=0 to w2-1 do begin
    GetCloseSet(x*2, w2, ptStart, ds, yStart);
    Inc(ds);
  end;
  end;


  procedure UnifySets(ds : PDistSet; Cnt : Integer; Src : array of PDistSet);
  Var n, m : Integer;
      d : TDistType;
  begin
  For n := 0 to 3 do ds[n] := 65535;
  For n := 0 to Length(Src)-1 do
    For m := 0 to Cnt do begin
      d := Src[n, m];
      If (d < ds[0]) then begin
        ds[3] := ds[2]; ds[2] := ds[1]; ds[1] := ds[0]; ds[0] := d;
      end else
        If (d < ds[1]) then begin
          ds[3] := ds[2]; ds[2] := ds[1]; ds[1] := d;
        end else
          If (d < ds[2]) then begin
            ds[3] := ds[2]; ds[2] := d;
          end else
            If (d < ds[3]) then ds[3] := d;
    end;
  end;

  procedure UnifySetsA(ds : PDistSet; Cnt : Integer; Src : array of PDistSet);
  Var n, m : Integer;
      d : TDistType;
  begin
  For m := 0 to Cnt do begin
    d := Src[0, m];
    For n := 1 to Length(Src)-1 do
      If Src[n, m] < d then d := Src[n, m];
    ds[m] := d;
  end;
  end;


  procedure SetPixelVal(Var pi : PWord; Var MaxVal : TDistType; ds : PDistSet;
                        Method : Integer);
  Var d : TDistType;// Integer;
  begin
  Case Method of
    0 : d := ds[0]; // Sqr(ds[0]);
    1 : d := ds[1] - ds[0];
    2 : d := (ds[0] + ds[1]) shr 1;
    3 : d := (ds[0] * ds[1]) shr 16;//4;
  end;
  If d > MaxVal then MaxVal := d;
  pi^ := d;
  Inc(pi);
  end;

begin
If (not (Dst.Bpp in [8, 16])) then Exit;
w := Dst.Width; h := Dst.Height;
w2 := w shr 1; h2 := h shr 1;

//TimeReset;

// preparing distance LUT
SetLength(DLut, (w2 + 1) * (h2 + 1)); //w * h);
k := 65534 / Sqrt(Sqr(w2+1) + Sqr(h2+1)); // to make effective usage of 0..65535 range
//k := 1;
For y:=0 to h2 do begin // 32 ms
  pw := @DLut[y * (w2 + 1)];
  For x:=0 to w2 do begin
    pw^ := Round(Sqrt(Sqr(x) + Sqr(y)) * k );
    Inc(pw);
  end;
end;

//TimeShowMks;

SetLength(pt, PointCnt); SetLength(ptYOffs, PointCnt);
For n:=0 to PointCnt-1 do begin
  pt[n].x := Random(w); pt[n].y := Random(h);
end;

If Dst.Bpp = 8 then fd := TFastDIB.Create(Dst, False, 16)
               else fd := Dst;
MaxD := 0; MinD := High(TDistType);

If (FastDistCalc) and (PointCnt > 8) then begin
  SetLength(Dist, w);
  ti := 0; bi := w2; y2 := 0;
  Cnt := Num_Min(PointCnt-1, 3);
  GetCloseLine(0, @ptYOffs[0], @Dist[bi]);
  For y:=0 to h2-1 do begin
    pi := fd.Scanlines[y2];
    pi1 := fd.Scanlines[y2 + 1];
    Inc(y2, 2);
    Num_CycleRange(y2, 0, h-1);
    Num_Swap(ti, bi);
    GetCloseLine(y2, @ptYOffs[0], @Dist[bi]);

    x2 := 1;
    For x:=0 to w2-1 do begin
      tix := ti + x;
      SetPixelVal(pi, MaxD, @Dist[tix, 0], FormulaNum);

      UnifySetsA(@dsv, Cnt, [ @Dist[tix],  @Dist[ti + x2] ]);
      SetPixelVal(pi, MaxD, @dsv, FormulaNum);

      UnifySetsA(@dsv, Cnt, [ @Dist[tix], @Dist[bi + x] ]);
      SetPixelVal(pi1, MaxD, @dsv, FormulaNum);

      UnifySetsA(@dsv1, Cnt, [ @dsv, @Dist[ti + x2], @Dist[bi + x2] ]);
      SetPixelVal(pi1, MaxD, @dsv1, FormulaNum);

      Inc(x2); If x2 >= w2 then x2 := 0;
    end;

  end;

  Finalize(Dist);
end else begin

//TimeReset;

  For y:=0 to h-1 do begin // 47, 60  400-700 ms - slowest part
    pi := fd.Scanlines[y];

    For n:=0 to PointCnt-1 do begin
      dy := Abs(y - pt[n].y);
      If dy > h2 then dy := h - dy;
      ptYOffs[n] := dy * (w2 + 1);
    end;

    For x:=0 to w-1 do begin
      GetCloseSet(x, w2, @pt[0], @dsv, @ptYOffs[0]);
      SetPixelVal(pi, MaxD, @dsv, FormulaNum);
    end;
  end;
end;


//Sys_ShowValues([PointCnt, c div (w * h)]);
//TimeShowMks;

//TimeReset;
//Dec(MaxD, MinD);
If (MaxD <> 0) then begin
  MaxD := 65536 * 255 div MaxD;
  For y:=0 to h-1 do begin // 25 ms
    pb := Dst.Scanlines[y];
    pi := fd.Scanlines[y];
    If Dst.Bpp = 8 then
      For x:=0 to w-1 do begin
  //      pb^ := Round((pi^ / MaxD) * 255);// shr 8;
        pb^ := pi^ * MaxD shr 16;
        Inc(pb); Inc(pi);
      end
    else
      For x:=0 to w-1 do begin
//        PWord(pb)^ := Round((pi^ / MaxD) * 65535);// shr 8;
        PWord(pb)^ := pi^ * MaxD shr 8;
        Inc(PWord(pb)); Inc(pi);
      end
  end;
end;
//TimeShowMks;

If fd <> Dst then fd.Free;

{
For n:=0 to PointCnt-1 do begin
  Dst.Pixels8[ pt[n].y, pt[n].x ] := 255;
  Dst.TextOut(pt[n].x+2, h-pt[n].y+2, Num_2Str(n));
end;
}
end;


procedure xProcSphereLine8(SLine : PLine8; Dst : PByte; w : Integer; theta : Single;
                           KLine : PWord; Smooth : Boolean);
Const
  UseRealFP = False;
//  UseRealFP = True;
Var x, w2, i2, i3, t, t2, z, iz : Integer;
    i, k : Single;
begin
w2 := w div 2;
t := Round(theta * 65536);
t2 := w2 * (65536 - t);

If Smooth then
  For x:=0 to w-1 do begin
    // get average from neighbour pixels
    If UseRealFP then begin       // note: floating point calculations here
      i := (x - w2) * theta + w2; // are slower 6-7 times
      i2 := Trunc(i); k := Frac(i);
      i3 := Round(SLine[i2] * (1-k) + SLine[i2+1] * (k));
    end else begin
      i2 := (x * t + t2);
      z := i2 and $FFFF;
      iz := $FFFF - z;
      i2 := i2 shr 16;
      i3 := (SLine[i2] * iz + SLine[i2 + 1] * z) shr 16;
    end;

    If (KLine <> nil) then begin
      Dst^ := (i3 * (65536 - KLine^) + SLine[w - i2] * (KLine^)) shr 16;
      Inc(KLine);
    end else
      Dst^ := i3;
    Inc(Dst);
  end
else // non-smooth
  If (KLine <> nil) then
    For x:=0 to w-1 do begin
      i2 := (x * t + t2) shr 16;
      Dst^ := (SLine[i2] * (65536 - KLine^) + SLine[w - i2] * (KLine^)) shr 16;
      Inc(Dst); Inc(KLine);
    end
  else
    For x:=0 to w-1 do begin
      i2 := (x * t + t2) shr 16;
      Dst^ := SLine[i2]; Inc(Dst);
    end;
end;

procedure xProcSphereLine32(SLine : PLine32; Dst : PFColorA; w : Integer; theta : Single;
                            KLine : PWord);
Var x, i2, t, t2, k, ik : Integer;
    pc : PFColorA;
    pb : PLine8;
    i3 : TFColorA;
begin
t := Round(theta * 65536);
t2 := (w div 2) * (65536 - t);

For x:=0 to w-1 do begin
  i2 := (x * t + t2);
  k := i2 and $FFFF;
  ik := $FFFF - k;
  i2 := i2 shr 16;

  pb := @SLine[i2];
  i3.b := (pb[0] * ik + pb[4] * k) shr 16;
  i3.g := (pb[1] * ik + pb[5] * k) shr 16;
  i3.r := (pb[2] * ik + pb[6] * k) shr 16;
//  i3.a := (pb[3] * iz + pb[7] * z) shr 16;

  If (KLine <> nil) then begin
    pc := @SLine[w - i2]; k := KLine^; ik := 65535 - k;
    Dst.b := (i3.b * ik + pc.b * k) shr 16;
    Dst.g := (i3.g * ik + pc.g * k) shr 16;
    Dst.r := (i3.r * ik + pc.r * k) shr 16;
//    Dst.a := (i3.a * ik + pc.a * k) shr 16;
    Inc(KLine);
  end else
    Dst^ := i3;
  Inc(Dst);
end;
end;


// http://paulbourke.net/texture_colour/tiling/ (middle of the page)
// 8, 32 bpp
procedure MakeSphereMap(Src, Dst : TFastDIB; Smooth : Boolean = True;
                        TileKoeff : Integer = 8); // 0 - no tiling
Var x, y, w : Integer;
    theta, PiH : Single;
    KLine, PK : PWord;
begin
w := Src.Width; PiH := Pi / (Dst.Height - 1);
If (TileKoeff <> 0) then begin // precalc koeffs for tiling
  GetMem(KLine, w * 2); PK := KLine;
  For x:=0 to w-1 do begin
    PK^ := Round( Math_IntPower(x / w, TileKoeff) * 65536 );
    Inc(PK);
  end;
end else
  KLine := nil;

For y:=0 to Dst.Height - 1 do begin
  theta := Cos( y * PiH - PiD2 );
  Case Dst.Bpp of
    8  : xProcSphereLine8(Src.Scanlines[y], Dst.Scanlines[y], w, theta, KLine, Smooth);
    32 : xProcSphereLine32(Src.Scanlines[y], Dst.Scanlines[y], w, theta, KLine);
  end;
end;

If (KLine <> nil) then FreeMem(KLine);
end;



// forces texture tiling (size must be power of 2), 24 bpp
// http://paulbourke.net/texture_colour/tiling/
procedure MakeTiling(Src, Dst : TFastDIB);
Var x, y, x1, y1, w2, h2, aSum : Integer;
    Mask : TFastDIB;
    mp : PByte;
    sc1, sc2, dc : PFColor;
    a1, a2 : Byte;
begin
Mask := TFastDIB.Create(Src, False, 8, True);

//RenderLightmap(Mask, Mask.Width);
GenLightEllipse(Mask, Mask.Width, Mask.Height);
//GenMapFractalEx(SubMask, 0.7);

w2 := Src.Width div 2; h2 := Src.Height div 2;

For y:=0 to Src.Height-1 do begin
  mp := Mask.Scanlines[y];
  sc1 := Src.Scanlines[y];
  dc := Dst.Scanlines[y];
  For x:=0 to Src.Width-1 do begin
    a1 := mp^;
    x1 := (x + w2) and (Src.Width-1);// mod Src.Width;
    y1 := (y + h2) and (Src.Height-1);// mod Src.Height;
    a2 := Mask.Pixels8[y1, x1];
    sc2 := @Src.Pixels24[y1, x1];

    aSum := a1 + a2;
    If (aSum <> 0) then begin
      aSum := 32768 div aSum; // doing 1 div instead of 3
      dc.r := (a1 * sc1.r + a2 * sc2.r) * aSum shr 15; // div aSum;
      dc.g := (a1 * sc1.g + a2 * sc2.g) * aSum shr 15;
      dc.b := (a1 * sc1.b + a2 * sc2.b) * aSum shr 15;
    end;// else dc^ := tfBlack;

    Inc(mp); Inc(sc1); Inc(dc);
  end;
end;

Mask.Free;
end;

end.
