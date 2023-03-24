unit FastBlend; // FastDIB: sourceforge.net/projects/tfastdib

interface
// Sapersky: per-pixel alpha, add with koeff, transparent color (Draw* procs)
// x64 compability (except 16 bpp blending)

{$R-}
uses Windows, FastDIB;
{$I platform.inc}

procedure AvgBlend(Dst,Src1,Src2:TFastDIB); // Dst = (Src1 + Src2) / 2
procedure DifBlend(Dst,Src1,Src2:TFastDIB);
  // Dst = Sat(Src1 - Src2) + Sat(Src2 - Src1), Sat = force range 0..255
procedure SubBlend(Dst,Src1,Src2:TFastDIB); // Dst = Src1 - Src2
procedure AddBlend(Dst,Src1,Src2:TFastDIB); // Dst = Src1 + Src2
procedure MulBlend(Dst,Src1,Src2:TFastDIB); // Dst = Src1 * Src2
procedure AlphaBlend(Dst,Src1,Src2:TFastDIB;Alpha:Integer);
  // Dst = Src1 * Alpha +  Src2 * (1 - Alpha)

function DrawAlpha(Src, Dst : TFastDIB; AlphaConst : Integer = 255;
                   dx : Integer = 0; dy : Integer = 0;
                   Alpha : TFastDIB = nil): Boolean;
  // Dst = Src * Src.Alpha * AlphaConst + Dst * (1 - Src.Alpha * AlphaConst)

function DrawAdd(Src, Dst : TFastDIB; K : Integer = 256;
                 dx : Integer = 0; dy : Integer = 0): Boolean;
  // Dst = Src * K + Dst
procedure DrawTrans(Src, Dst : TFastDIB; dx : Integer = 0; dy : Integer = 0;
                    TransColor : DWord = 0);
  // if Src <> TransColor then Dst = Src

implementation

// ********************************* common ************************************

type
  TBlend8Proc = procedure(Dst, Src1, Src2 : PLine8; Size : Integer);
  TBlend8ProcP = procedure(Dst, Src1, Src2 : PLine32; Size, Param : Integer);
  TBppSet = set of 1..32;

procedure xAnyBlend(Dst, Src1, Src2 : TFastDIB; Blend8, Blend8MMX : TBlend8Proc);
var y: Integer;
begin
If (Src1.Bpp <> Dst.Bpp) or (Src2.Bpp <> Dst.Bpp) or (Src1.Bpp <> Src2.Bpp) then Exit;
If Dst.Bpp in [8, 24, 32] then begin
  if cfMMX in CPUInfo.Features then Blend8 := Blend8MMX;

  for y:=0 to Dst.AbsHeight-1 do
    Blend8(Dst.Scanlines[y], Src1.Scanlines[y], Src2.Scanlines[y],
           Dst.BWidth - Dst.Gap);
end;
end;

procedure xAnyBlendT(Dst, Src1, Src2 : TFastDIB; MainShift : Integer;
                     Blend8, Blend8MMX, TailProc : TBlend8Proc);
var y, w, ws, wTail: Integer;
begin
If (Src1.Bpp <> Dst.Bpp) or (Src2.Bpp <> Dst.Bpp) or (Src1.Bpp <> Src2.Bpp) then Exit;
If Dst.Bpp in [8, 24, 32] then begin
  if cfMMX in CPUInfo.Features then Blend8 := Blend8MMX;

  w := Dst.BWidth - Dst.Gap;
  If (MainShift > 0) and (Assigned(TailProc)) then begin
    ws := w shr MainShift; wTail := w - (ws shl MainShift);
    for y:=0 to Dst.AbsHeight-1 do begin
      Blend8(Dst.Scanlines[y], Src1.Scanlines[y], Src2.Scanlines[y], ws shl MainShift);
      If wTail > 0 then
        TailProc(Dst.Scanlines[y], Src1.Scanlines[y], Src2.Scanlines[y], wTail);
    end;
  end else
    for y:=0 to Dst.AbsHeight-1 do
      Blend8(Dst.Scanlines[y], Src1.Scanlines[y], Src2.Scanlines[y], w);
end;
end;

procedure xAnyBlendP(Dst, Src1, Src2 : TFastDIB; Blend8, Blend8MMX : TBlend8ProcP;
                     Param : Integer; ValidBpps : TBppSet);
var y: Integer;
begin
If (Src1.Bpp <> Dst.Bpp) or (Src2.Bpp <> Dst.Bpp) or (Src1.Bpp <> Src2.Bpp) then Exit;
If Dst.Bpp in ValidBpps then begin
  if cfMMX in CPUInfo.Features then Blend8 := Blend8MMX;

  for y:=0 to Dst.AbsHeight-1 do
    Blend8(Dst.Scanlines[y], Src1.Scanlines[y], Src2.Scanlines[y],
           Dst.BWidth - Dst.Gap, Param);
end;
end;

// ********************************* AvgBlend **********************************

{$IFDEF CPUX86}
// no-MMX proc had a little advantage over PAS version (10%) - removed

// 2 times faster than PAS version
procedure xAvgMemMMX(Dst,Src1,Src2:PLine32;Size,Mask:Integer);
// Dst  = eax
// Src1 = edx
// Src2 = ecx
// Size = [ebp + 12]
// Mask = [ebp +  8]
// a:b  = [ebp -  8]
var
  a,b: Integer;
asm
  push edi
  push esi

  mov a,0
  mov b,0

  mov edi,edx
  mov esi,ecx
  mov ecx,[ebp+12]

  db $0F,$6E,$5D,$08 /// movd  mm3,[ebp+8]
  db $0F,$6F,$E3     /// movq  mm4,mm3
  db $0F,$73,$F4,$20 /// psllq mm4,32
  db $0F,$EB,$DC     /// por   mm3,mm4

  shr ecx,3
  jz  @skip

  @quads:
    db $0F,$6F,$07     /// movq  mm0,[edi]
    db $0F,$6F,$0E     /// movq  mm1,[esi]
    db $0F,$6F,$D0     /// movq  mm2,mm0
    db $0F,$DB,$C1     /// pand  mm0,mm1
    db $0F,$EF,$CA     /// pxor  mm1,mm2
    db $0F,$73,$D1,$01 /// psrlq mm1,1
    db $0F,$DB,$CB     /// pand  mm1,mm3
    db $0F,$FE,$C8     /// paddd mm1,mm0
    db $0F,$7F,$08     /// movq  [eax],mm1
    add edi,8
    add esi,8
    add eax,8
    dec ecx
  jnz @quads

  @skip:
  mov ecx,[ebp+12]
  and ecx,111b
  jz  @exit

  mov edx,ecx
  push edi
  lea edi,[ebp-8]
  rep movsb
  db $0F,$6F,$8D,$F8,$FF,$FF,$FF /// movq mm1,[ebp-8]

  mov ecx,edx
  pop esi
  lea edi,[ebp-8]
  rep movsb
  db $0F,$6F,$85,$F8,$FF,$FF,$FF /// movq mm0,[ebp-8]

  db $0F,$6F,$D0                 /// movq  mm2,mm0
  db $0F,$DB,$C1                 /// pand  mm0,mm1
  db $0F,$EF,$CA                 /// pxor  mm1,mm2
  db $0F,$73,$D1,$01             /// psrlq mm1,1
  db $0F,$DB,$CB                 /// pand  mm1,mm3
  db $0F,$FE,$C8                 /// paddd mm1,mm0
  db $0F,$7F,$8D,$F8,$FF,$FF,$FF /// movq  [ebp-8],mm1

  mov ecx,edx
  lea esi,[ebp-8]
  mov edi,eax
  rep movsb

  @exit:
  db $0F,$77 // emms
  pop esi
  pop edi
end;
{$ENDIF}

procedure xLine_AvgSameBpp(Dst, Src1, Src2 : PLine32; Size, Mask : Integer);
Var x, i : Integer;
begin
For x:=0 to (Size shr 4)-1 do begin
  // (A+B)/2 = (A and B)+((A xor B)/2) : Paul Hsieh
  // (can be done at dword without overflow)
  Dst[0].i := (Src1[0].i and Src2[0].i) + ((Src1[0].i xor Src2[0].i) shr 1) and Mask;
  Dst[1].i := (Src1[1].i and Src2[1].i) + ((Src1[1].i xor Src2[1].i) shr 1) and Mask;
  Dst[2].i := (Src1[2].i and Src2[2].i) + ((Src1[2].i xor Src2[2].i) shr 1) and Mask;
  Dst[3].i := (Src1[3].i and Src2[3].i) + ((Src1[3].i xor Src2[3].i) shr 1) and Mask;
  Inc(PByte(Dst), 16); Inc(PByte(Src1), 16); Inc(PByte(Src2), 16);
end;

For x:=0 to ((Size and 15) shr 2)-1 do begin
  Dst[0].i := (Src1[0].i and Src2[0].i) + ((Src1[0].i xor Src2[0].i) shr 1) and Mask;
  Inc(PByte(Dst), 4); Inc(PByte(Src1), 4); Inc(PByte(Src2), 4);
end;

i := 0;
For x:=0 to (Size and 3)-1 do begin
  Dst[0].b := (Src1[0].b and Src2[0].b) + ((Src1[0].b xor Src2[0].b) shr 1) and //Mask;
    PLine8(@Mask)[i];
  Inc(i); If i > 3 then i := 0;
  Inc(PByte(Dst)); Inc(PByte(Src1)); Inc(PByte(Src2));
end;
end;


procedure AvgBlend(Dst,Src1,Src2:TFastDIB);
Var Mask : Integer;
begin
If Dst.Bpp in [16, 32] then begin
  Mask := ( (Dst.RMask+(1 shl Dst.RShl)) or
            (Dst.GMask+(1 shl Dst.GShl)) or
            (Dst.BMask+1) ) shr 1;
  if Dst.Bpp=16 then Mask:=(Mask shl 16 or Mask)xor -1
                else Mask:=Mask xor -1;
end else
  Mask:=$7F7F7F7F;
{$IFDEF CPUX86}
xAnyBlendP(Dst, Src1, Src2, xLine_AvgSameBpp, xAvgMemMMX, Mask, [8, 16, 24, 32]);
{$ELSE}
xAnyBlendP(Dst, Src1, Src2, xLine_AvgSameBpp, xLine_AvgSameBpp, Mask, [8, 16, 24, 32]);
{$ENDIF}
end;


// ********************************* DifBlend **********************************

{$IFDEF CPUX86}
{
procedure DifMem16(Dst,Src1,Src2:Pointer;Size:Integer);
// Dst  = eax
// Src1 = edx
// Src2 = ecx
// Size = [ebp +  8]
// a    = [ebp -  4]
// b    = [ebp -  8]
// c    = [ebp - 12]
var
  a,b,c: Integer;
asm
  push ebx
  push edi
  push esi

  mov  edi,edx
  mov  esi,ecx
  mov  ecx,[ebp+8]
  push ecx
  and  ecx,$FFFFFFFC
  jz   @skip

  add ecx,eax
  mov [ebp+8],ecx

  @dwords:
    mov ecx,[edi]
    mov ebx,[esi]
    and ecx,$001F001F
    and ebx,$001F001F
    or  ecx,$00200020
    sub ecx,ebx
    mov ebx,ecx
    and ebx,$00200020
    shr ebx,5
    imul ebx,$1F
    xor ecx,ebx

    mov edx,[edi]
    mov ebx,[esi]
    and edx,$07E007E0
    and ebx,$07E007E0
    shr edx,5
    shr ebx,5
    or  edx,$00400040
    sub edx,ebx
    mov ebx,edx
    and ebx,$00400040
    shr ebx,6
    imul ebx,$3F
    xor edx,ebx
    shl edx,5
    or  ecx,edx

    mov edx,[edi]
    mov ebx,[esi]
    and edx,$F800F800
    and ebx,$F800F800
    shr edx,11
    shr ebx,11
    or  edx,$00200020
    sub edx,ebx
    mov ebx,edx
    and ebx,$00200020
    shr ebx,5
    imul ebx,$1F
    xor edx,ebx
    shl edx,11
    or  ecx,edx
    xor ecx,-1

    mov [eax],ecx
    add edi,4
    add esi,4
    add eax,4
    cmp eax,[ebp+8]
  jne @dwords

  cmp edi,ebp
  je  @last

  @skip:
  pop ecx
  and ecx,11b
  shr ecx,1
  jz  @exit

  mov  cx,[edi]
  mov  bx,[esi]
  mov  a,ecx
  mov  b,ebx
  lea  edi,a
  lea  esi,b
  push eax
  lea  eax,c
  lea  ebx,[eax+4]
  mov  [ebp+8],ebx
  jmp  @dwords

  @last:
  pop eax
  mov ebx,[ebp-12]
  mov [eax],bx

  @exit:
  pop esi
  pop edi
  pop ebx
end;

procedure DifMem16MMX(Dst,Src1,Src2:Pointer;Size:Integer);
// Dst   = eax
// Src1  = edx
// Src2  = ecx
// Size  = [ebp +  8]
// B1:B2 = [ebp -  8]
// G1:G2 = [ebp - 16]
// M1:M2 = [ebp - 24]
var
  B1,B2,G1,G2,M1,M2: Integer;
asm
  push ebx
  push edi
  push esi

  mov B1,$001F001F
  mov B2,$001F001F
  mov G1,$07E007E0
  mov G2,$07E007E0
  mov M2,0
  mov M1,0

  pcmpeqd mm7,mm7
  psrlw   mm7,11
  psllw   mm7,11

  mov edi,edx
  mov esi,ecx
  mov ecx,[ebp+8]
  push ecx
  shr ecx,3
  jz  @skip

  @quads:
    movq    mm0,[edi]
    movq    mm1,[esi]
  @cleanup:
    movq    mm2,mm0
    movq    mm3,mm1
    movq    mm4,mm0
    movq    mm5,mm1

    pand    mm0,[ebp-8]
    pand    mm1,[ebp-8]
    movq    mm6,mm0
    psubusw mm0,mm1
    psubusw mm1,mm6
    paddusw mm0,mm1

    pand    mm2,[ebp-16]
    pand    mm3,[ebp-16]
    psrlw   mm2,5
    psrlw   mm3,5
    movq    mm6,mm2
    psubusw mm2,mm3
    psubusw mm3,mm6
    paddusw mm2,mm3
    psllw   mm2,5
    por     mm0,mm2

    pand    mm4,mm7
    pand    mm5,mm7
    psrlw   mm4,11
    psrlw   mm5,11
    movq    mm6,mm4
    psubusw mm4,mm5
    psubusw mm5,mm6
    paddusw mm4,mm5
    psllw   mm4,11
    por     mm0,mm4

    movq   [eax],mm0

    add edi,8
    add esi,8
    add eax,8
    dec ecx
  jnz @quads

  cmp edi,0
  je @last

  @skip:
  pop ecx
  and ecx,111b
  shr ecx,1
  jz  @exit

  mov  edx,ecx
  push edi
  lea  edi,M1
  rep  movsw
  movq mm1,[ebp-24]
  pop  esi
  mov  ecx,edx
  lea  edi,M1
  rep  movsw
  movq mm0,[ebp-24]
  push eax
  lea  eax,M1
  mov  edi,-8
  mov  ecx,1
  jmp  @cleanup

  @last:
  pop edi
  lea esi,M1
  mov ecx,edx
  rep movsw

  @exit:
  emms
  pop esi
  pop edi
  pop ebx
end;
}

procedure xDifBlend16(Dst,Src1,Src2:TFastDIB);
type
  TDifMem16REG = array[0..256]of Byte; PDifMem16REG =^TDifMem16REG;
  TDifMem16MMX = array[0..299]of Byte; PDifMem16MMX =^TDifMem16MMX;
  TDifMem16Proc = procedure(Dst,Src1,Src2:Pointer;Size:Integer);
const
  DifMem16REG: TDifMem16REG =
  ($55,$8B,$EC,$83,$C4,$F4,$53,$57,$56,$89,$D7,$89,$CE,$8B,$4D,$08,$51,$83,$E1,
   $FC,$0F,$84,$AE,$00,$00,$00,$01,$C1,$89,$4D,$08,$8B,$0F,$8B,$1E,$81,$E1,$1F,
   $00,$1F,$00,$81,$E3,$1F,$00,$1F,$00,$81,$C9,$20,$00,$20,$00,$29,$D9,$89,$CB,
   $81,$E3,$20,$00,$20,$00,$C1,$EB,$05,$6B,$DB,$1F,$31,$D9,$8B,$17,$8B,$1E,$81,
   $E2,$E0,$07,$E0,$07,$81,$E3,$E0,$07,$E0,$07,$C1,$EA,$05,$C1,$EB,$05,$81,$CA,
   $40,$00,$40,$00,$29,$DA,$89,$D3,$81,$E3,$40,$00,$40,$00,$C1,$EB,$06,$6B,$DB,
   $3F,$31,$DA,$C1,$E2,$05,$09,$D1,$8B,$17,$8B,$1E,$81,$E2,$00,$F8,$00,$F8,$81,
   $E3,$00,$F8,$00,$F8,$C1,$EA,$0B,$C1,$EB,$0B,$81,$CA,$20,$00,$20,$00,$29,$DA,
   $89,$D3,$81,$E3,$20,$00,$20,$00,$C1,$EB,$05,$6B,$DB,$1F,$31,$DA,$C1,$E2,$0B,
   $09,$D1,$83,$F1,$FF,$89,$08,$83,$C7,$04,$83,$C6,$04,$83,$C0,$04,$3B,$45,$08,
   $0F,$85,$5B,$FF,$FF,$FF,$39,$EF,$74,$29,$59,$83,$E1,$03,$D1,$E9,$74,$28,$66,
   $8B,$0F,$66,$8B,$1E,$89,$4D,$FC,$89,$5D,$F8,$8D,$7D,$FC,$8D,$75,$F8,$50,$8D,
   $45,$F4,$8D,$58,$04,$89,$5D,$08,$E9,$2E,$FF,$FF,$FF,$58,$8B,$5D,$F4,$66,$89,
   $18,$5E,$5F,$5B,$8B,$E5,$5D,$C2,$04,$00);

  DifMem16MMX: TDifMem16MMX =
  ($55,$8B,$EC,$83,$C4,$E8,$53,$57,$56,$C7,$45,$FC,$1F,$00,$1F,$00,$C7,$45,$F8,
   $1F,$00,$1F,$00,$C7,$45,$F4,$E0,$07,$E0,$07,$C7,$45,$F0,$E0,$07,$E0,$07,$C7,
   $45,$EC,$00,$00,$00,$00,$C7,$45,$E8,$00,$00,$00,$00,$0F,$76,$FF,$0F,$71,$D7,
   $0B,$0F,$71,$F7,$0B,$89,$D7,$89,$CE,$8B,$4D,$08,$51,$C1,$E9,$03,$0F,$84,$8E,
   $00,$00,$00,$0F,$6F,$07,$0F,$6F,$0E,$0F,$6F,$D0,$0F,$6F,$D9,$0F,$6F,$E0,$0F,
   $6F,$E9,$0F,$DB,$85,$F8,$FF,$FF,$FF,$0F,$DB,$8D,$F8,$FF,$FF,$FF,$0F,$6F,$F0,
   $0F,$D9,$C1,$0F,$D9,$CE,$0F,$DD,$C1,$0F,$DB,$95,$F0,$FF,$FF,$FF,$0F,$DB,$9D,
   $F0,$FF,$FF,$FF,$0F,$71,$D2,$05,$0F,$71,$D3,$05,$0F,$6F,$F2,$0F,$D9,$D3,$0F,
   $D9,$DE,$0F,$DD,$D3,$0F,$71,$F2,$05,$0F,$EB,$C2,$0F,$DB,$E7,$0F,$DB,$EF,$0F,
   $71,$D4,$0B,$0F,$71,$D5,$0B,$0F,$6F,$F4,$0F,$D9,$E5,$0F,$D9,$EE,$0F,$DD,$E5,
   $0F,$71,$F4,$0B,$0F,$EB,$C4,$0F,$7F,$00,$83,$C7,$08,$83,$C6,$08,$83,$C0,$08,
   $49,$0F,$85,$77,$FF,$FF,$FF,$83,$FF,$00,$74,$3B,$59,$83,$E1,$07,$D1,$E9,$74,
   $3C,$89,$CA,$57,$8D,$7D,$E8,$66,$F3,$A5,$0F,$6F,$8D,$E8,$FF,$FF,$FF,$5E,$89,
   $D1,$8D,$7D,$E8,$66,$F3,$A5,$0F,$6F,$85,$E8,$FF,$FF,$FF,$50,$8D,$45,$E8,$BF,
   $F8,$FF,$FF,$FF,$B9,$01,$00,$00,$00,$E9,$3D,$FF,$FF,$FF,$5F,$8D,$75,$E8,$89,
   $D1,$66,$F3,$A5,$0F,$77,$5E,$5F,$5B,$8B,$E5,$5D,$C2,$04,$00);
var
  i: Integer;
  Code: PLine8;
begin
  if cfMMX in CPUInfo.Features then
  begin
    GetMem(Code,SizeOf(TDifMem16MMX));
    PDifMem16MMX(Code)^:=DifMem16MMX;

    i:=Dst.BMask shl 16 or Dst.BMask;
    PDWord(@Code[12])^:=i;
    PDWord(@Code[19])^:=i;
    i:=Dst.GMask shl 16 or Dst.GMask;
    PDWord(@Code[26])^:=i;
    PDWord(@Code[33])^:=i;
    Code[57]:=16-Dst.Bpr;
    Code[61]:=Dst.RShl;
    Code[140]:=Dst.GShl;
    Code[144]:=Dst.GShl;
    Code[160]:=Dst.GShl;
    Code[173]:=Dst.RShl;
    Code[177]:=Dst.RShl;
    Code[193]:=Dst.RShl;
  end else
  begin
    GetMem(Code,SizeOf(TDifMem16REG));
    PDifMem16REG(Code)^:=DifMem16REG;

    i:=Dst.BMask shl 16 or Dst.BMask;
    PDWord(@Code[37])^:=i;
    PDWord(@Code[43])^:=i;
    i:=(i+i)and(not i);
    PDWord(@Code[49])^:=i;
    PDWord(@Code[59])^:=i;
    i:=Dst.GMask shl 16 or Dst.GMask;
    PDWord(@Code[77])^:=i;
    PDWord(@Code[83])^:=i;
    i:=i shr Dst.GShl;
    i:=(i+i)and(not i);
    PDWord(@Code[95])^:=i;
    PDWord(@Code[105])^:=i;
    i:=Dst.RMask shl 16 or Dst.RMask;
    PDWord(@Code[128])^:=i;
    PDWord(@Code[134])^:=i;
    i:=i shr Dst.RShl;
    i:=(i+i)and(not i);
    PDWord(@Code[146])^:=i;
    PDWord(@Code[156])^:=i;
    Code[65]:=Dst.Bpb;
    Code[68]:=(1 shl Dst.Bpb)-1;
    Code[89]:=Dst.GShl;
    Code[92]:=Dst.GShl;
    Code[111]:=Dst.Bpg;
    Code[114]:=(1 shl Dst.Bpg)-1;
    Code[119]:=Dst.GShl;
    Code[140]:=Dst.RShl;
    Code[143]:=Dst.RShl;
    Code[162]:=Dst.Bpr;
    Code[165]:=(1 shl Dst.Bpr)-1;
    Code[170]:=Dst.RShl;
  end;

  for i:=0 to Dst.AbsHeight-1 do
    TDifMem16Proc(Code)(Dst.Scanlines[i],Src1.Scanlines[i],Src2.Scanlines[i],Dst.BWidth-Dst.Gap);

  FreeMem(Code);
end;

procedure xDifMem8(Dst,Src1,Src2:PLine8;Size:Integer);
// Dst  = eax
// Src1 = edx
// Src2 = ecx
// Size = [ebp + 8]
// s    = [ebp - 4]
var
  s: Integer;
asm
  push ebx
  push edi
  push esi

  mov edi,edx
  mov esi,ecx
  mov ecx,[ebp+8]
  push ecx
  mov s,esp
  mov esp,eax
  and ecx,$FFFFFFFC
  jz  @skip
  add ecx,eax
  mov [ebp+8],ecx
  
  @dwords:
    mov  ecx,[edi]
    mov  ebx,[esi]
    mov  eax,ecx
    mov  edx,ebx

    and  ecx,$00FF00FF
    and  ebx,$00FF00FF
    or   ecx,$01000100
    sub  ecx,ebx
    mov  ebx,ecx
    and  ebx,$01000100
    imul ebx,$FF
    shr  ebx,8
    xor  ecx,ebx

    and  eax,$FF00FF00
    and  edx,$FF00FF00
    shr  eax,8
    shr  edx,8
    or   eax,$01000100
    sub  eax,edx
    mov  edx,eax
    and  edx,$01000100
    imul edx,$FF
    shr  edx,8
    xor  eax,edx
    shl  eax,8
    or   ecx,eax
    xor  ecx,-1

    mov [esp],ecx
    add edi,4
    add esi,4
    add esp,4
    cmp esp,[ebp+8]
  jne @dwords

  @skip:
  mov eax,esp
  mov esp,s
  pop ecx
  and ecx,11b
  jz  @exit

  @bytes:
    movzx ebx,Byte([edi])
    movzx edx,Byte([esi])
    sub ebx,edx
    mov edx,ebx
    shr edx,8
    xor ebx,edx
    mov [eax],bl
    inc edi
    inc esi
    inc eax
    dec ecx
  jnz @bytes

  @exit:
  pop esi
  pop edi
  pop ebx
end;

// 7 times faster than no-MMX version
procedure xDifMem8MMX(Dst,Src1,Src2:PLine8;Size:Integer);
// Dst  = eax
// Src1 = edx
// Src2 = ecx
// Size = [ebp + 8]
asm
  push ebx
  push edi
  push esi

  mov edi,edx
  mov esi,ecx
  mov ecx,[ebp+8]
  push ecx
  shr ecx,3
  jz  @skip

  @quads:
    db $0F,$6F,$07  /// movq    mm0,[edi]
    db $0F,$6F,$0E  /// movq    mm1,[esi]
    db $0F,$6F,$D0  /// movq    mm2,mm0
    db $0F,$D8,$C1  /// psubusb mm0,mm1
    db $0F,$D8,$CA  /// psubusb mm1,mm2
    db $0F,$DC,$C1  /// paddusb mm0,mm1
    db $0F,$7F,$00  /// movq    [eax],mm0
    add edi,8
    add esi,8
    add eax,8
    dec ecx
  jnz @quads

  db $0F,$77 // emms

  @skip:
  pop ecx
  and ecx,111b
  jz  @exit

  @bytes:
    movzx ebx,Byte([edi])
    movzx edx,Byte([esi])
    sub ebx,edx
    mov edx,ebx
    shr edx,8
    xor ebx,edx
    mov [eax],bl
    inc edi
    inc esi
    inc eax
    dec ecx
  jnz @bytes

  @exit:
  pop esi
  pop edi
  pop ebx
end;
{$ENDIF}

{$IFDEF CPUX64}
// Dst = rcx, Src1 = rdx, Src2 = r8, Size = r9
procedure xDifMem8MMX(Dst,Src1,Src2:PLine8;Size:Integer);
asm
  mov rax, r9
  shr rax, 3
  jz  @skip

  @quads:
    movq    mm0,[rdx]
    movq    mm1,[r8]
    movq    mm2,mm0
    psubusb mm0,mm1
    psubusb mm1,mm2
    paddusb mm0,mm1
    movq    [rcx],mm0
    add rdx,8
    add r8,8
    add rcx,8
    dec rax
  jnz @quads
  emms

  @skip:
  mov rax, r9
  and r9,111b
  jz  @exit

  @bytes:
    movzx r10, byte ptr [rdx]
    movzx rax, byte ptr [r8]
    sub r10,rax
    mov rax,r10
    shr rax,8
    xor rax, r10
    mov [rcx],al
    inc rdx
    inc r8
    inc rcx
    dec r9
  jnz @bytes

  @exit:
end;
{$ENDIF}

procedure DifBlend(Dst,Src1,Src2:TFastDIB);
begin
{$IFDEF CPUX86}
If (Src1.Bpp = 16) and (Src2.Bpp = 16) and (Dst.Bpp = 16) then
  xDifBlend16(Dst, Src1, Src2)
else
  xAnyBlend(Dst, Src1, Src2, xDifMem8, xDifMem8MMX);
{$ELSE}
xAnyBlend(Dst, Src1, Src2, xDifMem8MMX, xDifMem8MMX);
{$ENDIF}
end;


// ********************************* SubBlend **********************************

{$IFDEF CPUX86}
{
procedure SubMem16(Dst,Src1,Src2:Pointer;Size:Integer);
// Dst  = eax
// Src1 = edx
// Src2 = ecx
// Size = [ebp +  8]
// a    = [ebp -  4]
// b    = [ebp -  8]
// c    = [ebp - 12]
var
  a,b,c: Integer;
asm
  push ebx
  push edi
  push esi

  mov  edi,edx
  mov  esi,ecx
  mov  ecx,[ebp+8]
  push ecx
  and  ecx,$FFFFFFFC
  jz   @skip

  add ecx,eax
  mov [ebp+8],ecx

  @dwords:
    mov ecx,[edi]
    mov ebx,[esi]
    and ecx,$001F001F
    and ebx,$001F001F
    or  ecx,$00200020
    sub ecx,ebx
    mov ebx,ecx
    and ebx,$00200020
    shr ebx,5
    imul ebx,$1F
    and ecx,ebx

    mov edx,[edi]
    mov ebx,[esi]
    and edx,$07E007E0
    and ebx,$07E007E0
    shr edx,5
    shr ebx,5
    or  edx,$00400040
    sub edx,ebx
    mov ebx,edx
    and ebx,$00400040
    shr ebx,6
    imul ebx,$3F
    and edx,ebx
    shl edx,5
    or  ecx,edx

    mov edx,[edi]
    mov ebx,[esi]
    and edx,$F800F800
    and ebx,$F800F800
    shr edx,11
    shr ebx,11
    or  edx,$00200020
    sub edx,ebx
    mov ebx,edx
    and ebx,$00200020
    shr ebx,5
    imul ebx,$1F
    and edx,ebx
    shl edx,11
    or  ecx,edx

    mov [eax],ecx
    add edi,4
    add esi,4
    add eax,4
    cmp eax,[ebp+8]
  jne @dwords

  cmp edi,ebp
  je  @last

  @skip:
  pop ecx
  and ecx,11b
  shr ecx,1
  jz  @exit

  mov  cx,[edi]
  mov  bx,[esi]
  mov  a,ecx
  mov  b,ebx
  lea  edi,a
  lea  esi,b
  push eax
  lea  eax,c
  lea  ebx,[eax+4]
  mov  [ebp+8],ebx
  jmp  @dwords

  @last:
  pop eax
  mov ebx,[ebp-12]
  mov [eax],bx

  @exit:
  pop esi
  pop edi
  pop ebx
end;

procedure SubMem16MMX(Dst,Src1,Src2:Pointer;Size:Integer);
// Dst   = eax
// Src1  = edx
// Src2  = ecx
// Size  = [ebp +  8]
// B1:B2 = [ebp -  8]
// M1:M2 = [ebp - 16]
var
  B1,B2,M1,M2: Integer;
asm
  push ebx
  push edi
  push esi

  mov B1,$001F001F
  mov B2,$001F001F
  mov M2,0
  mov M1,0

  pcmpeqd mm6,mm6
  psrlw   mm6,10
  psllw   mm6,5
  pcmpeqd mm7,mm7
  psrlw   mm7,11
  psllw   mm7,11

  mov edi,edx
  mov esi,ecx
  mov ecx,[ebp+8]
  push ecx
  shr ecx,3
  jz  @skip

  @quads:
    movq    mm0,[edi]
    movq    mm1,[esi]
  @cleanup:
    movq    mm2,mm0
    movq    mm3,mm1
    movq    mm4,mm0
    movq    mm5,mm1

    pand    mm0,[ebp-8]
    pand    mm1,[ebp-8]
    psubusw mm0,mm1

    pand    mm2,mm6
    pand    mm3,mm6
    psrlw   mm2,5
    psrlw   mm3,5
    psubusw mm2,mm3
    psllw   mm2,5
    por     mm0,mm2

    pand    mm4,mm7
    pand    mm5,mm7
    psrlw   mm4,11
    psrlw   mm5,11
    psubusw mm4,mm5
    psllw   mm4,11
    por     mm0,mm4

    movq [eax],mm0

    add edi,8
    add esi,8
    add eax,8
    dec ecx
  jnz @quads

  cmp edi,0
  je @last

  @skip:
  pop ecx
  and ecx,111b
  shr ecx,1
  jz  @exit

  mov  edx,ecx
  push edi
  lea  edi,M1
  rep  movsw
  movq mm1,[ebp-16]
  pop  esi
  mov  ecx,edx
  lea  edi,M1
  rep  movsw
  movq mm0,[ebp-16]
  push eax
  lea  eax,M1
  mov  edi,-8
  mov  ecx,1
  jmp  @cleanup

  @last:
  pop edi
  lea esi,M1
  mov ecx,edx
  rep movsw

  @exit:
  emms
  pop esi
  pop edi
  pop ebx
end;
}

procedure xSubBlend16(Dst,Src1,Src2:TFastDIB);
type
  TSubMem16REG = array[0..253]of Byte; PSubMem16REG =^TSubMem16REG;
  TSubMem16MMX = array[0..253]of Byte; PSubMem16MMX =^TSubMem16MMX;
  TSubMem16Proc = procedure(Dst,Src1,Src2:Pointer;Size:Integer);
const
  SubMem16REG: TSubMem16REG =
  ($55,$8B,$EC,$83,$C4,$F4,$53,$57,$56,$89,$D7,$89,$CE,$8B,$4D,$08,$51,$83,$E1,
   $FC,$0F,$84,$AB,$00,$00,$00,$01,$C1,$89,$4D,$08,$8B,$0F,$8B,$1E,$81,$E1,$1F,
   $00,$1F,$00,$81,$E3,$1F,$00,$1F,$00,$81,$C9,$20,$00,$20,$00,$29,$D9,$89,$CB,
   $81,$E3,$20,$00,$20,$00,$C1,$EB,$05,$6B,$DB,$1F,$21,$D9,$8B,$17,$8B,$1E,$81,
   $E2,$E0,$07,$E0,$07,$81,$E3,$E0,$07,$E0,$07,$C1,$EA,$05,$C1,$EB,$05,$81,$CA,
   $40,$00,$40,$00,$29,$DA,$89,$D3,$81,$E3,$40,$00,$40,$00,$C1,$EB,$06,$6B,$DB,
   $3F,$21,$DA,$C1,$E2,$05,$09,$D1,$8B,$17,$8B,$1E,$81,$E2,$00,$F8,$00,$F8,$81,
   $E3,$00,$F8,$00,$F8,$C1,$EA,$0B,$C1,$EB,$0B,$81,$CA,$20,$00,$20,$00,$29,$DA,
   $89,$D3,$81,$E3,$20,$00,$20,$00,$C1,$EB,$05,$6B,$DB,$1F,$21,$DA,$C1,$E2,$0B,
   $09,$D1,$89,$08,$83,$C7,$04,$83,$C6,$04,$83,$C0,$04,$3B,$45,$08,$0F,$85,$5E,
   $FF,$FF,$FF,$39,$EF,$74,$29,$59,$83,$E1,$03,$D1,$E9,$74,$28,$66,$8B,$0F,$66,
   $8B,$1E,$89,$4D,$FC,$89,$5D,$F8,$8D,$7D,$FC,$8D,$75,$F8,$50,$8D,$45,$F4,$8D,
   $58,$04,$89,$5D,$08,$E9,$31,$FF,$FF,$FF,$58,$8B,$5D,$F4,$66,$89,$18,$5E,$5F,
   $5B,$8B,$E5,$5D,$C2,$04,$00);

  SubMem16MMX: TSubMem16MMX =
  ($55,$8B,$EC,$83,$C4,$F0,$53,$57,$56,$C7,$45,$FC,$1F,$00,$1F,$00,$C7,$45,$F8,
   $1F,$00,$1F,$00,$C7,$45,$F4,$00,$00,$00,$00,$C7,$45,$F0,$00,$00,$00,$00,$0F,
   $76,$F6,$0F,$71,$D6,$0A,$0F,$71,$F6,$05,$0F,$76,$FF,$0F,$71,$D7,$0B,$0F,$71,
   $F7,$0B,$89,$D7,$89,$CE,$8B,$4D,$08,$51,$C1,$E9,$03,$74,$67,$0F,$6F,$07,$0F,
   $6F,$0E,$0F,$6F,$D0,$0F,$6F,$D9,$0F,$6F,$E0,$0F,$6F,$E9,$0F,$DB,$85,$F8,$FF,
   $FF,$FF,$0F,$DB,$8D,$F8,$FF,$FF,$FF,$0F,$D9,$C1,$0F,$DB,$D6,$0F,$DB,$DE,$0F,
   $71,$D2,$05,$0F,$71,$D3,$05,$0F,$D9,$D3,$0F,$71,$F2,$05,$0F,$EB,$C2,$0F,$DB,
   $E7,$0F,$DB,$EF,$0F,$71,$D4,$0B,$0F,$71,$D5,$0B,$0F,$D9,$E5,$0F,$71,$F4,$0B,
   $0F,$EB,$C4,$0F,$7F,$00,$83,$C7,$08,$83,$C6,$08,$83,$C0,$08,$49,$75,$9E,$83,
   $FF,$00,$74,$3B,$59,$83,$E1,$07,$D1,$E9,$74,$3C,$89,$CA,$57,$8D,$7D,$F0,$66,
   $F3,$A5,$0F,$6F,$8D,$F0,$FF,$FF,$FF,$5E,$89,$D1,$8D,$7D,$F0,$66,$F3,$A5,$0F,
   $6F,$85,$F0,$FF,$FF,$FF,$50,$8D,$45,$F0,$BF,$F8,$FF,$FF,$FF,$B9,$01,$00,$00,
   $00,$E9,$64,$FF,$FF,$FF,$5F,$8D,$75,$F0,$89,$D1,$66,$F3,$A5,$0F,$77,$5E,$5F,
   $5B,$8B,$E5,$5D,$C2,$04,$00);
var
  i: Integer;
  Code: PLine8;
begin
  if cfMMX in CPUInfo.Features then
  begin
    GetMem(Code,SizeOf(TSubMem16MMX));
    PSubMem16MMX(Code)^:=SubMem16MMX;

    i:=Dst.BMask shl 16 or Dst.BMask;
    PDWord(@Code[12])^:=i;
    PDWord(@Code[19])^:=i;
    Code[43]:=16-Dst.Bpg;
    Code[47]:=Dst.GShl;
    Code[54]:=16-Dst.Bpr;
    Code[58]:=Dst.RShl;
    Code[116]:=Dst.GShl;
    Code[120]:=Dst.GShl;
    Code[127]:=Dst.GShl;
    Code[140]:=Dst.RShl;
    Code[144]:=Dst.RShl;
    Code[151]:=Dst.RShl;
  end else
  begin
    GetMem(Code,SizeOf(TSubMem16REG));
    PSubMem16REG(Code)^:=SubMem16REG;

    i:=Dst.BMask shl 16 or Dst.BMask;
    PDWord(@Code[37])^:=i;
    PDWord(@Code[43])^:=i;
    i:=(i+i)and(not i);
    PDWord(@Code[49])^:=i;
    PDWord(@Code[59])^:=i;
    i:=Dst.GMask shl 16 or Dst.GMask;
    PDWord(@Code[77])^:=i;
    PDWord(@Code[83])^:=i;
    i:=i shr Dst.GShl;
    i:=(i+i)and(not i);
    PDWord(@Code[95])^:=i;
    PDWord(@Code[105])^:=i;
    i:=Dst.RMask shl 16 or Dst.RMask;
    PDWord(@Code[128])^:=i;
    PDWord(@Code[134])^:=i;
    i:=i shr Dst.RShl;
    i:=(i+i)and(not i);
    PDWord(@Code[146])^:=i;
    PDWord(@Code[156])^:=i;
    Code[65]:=Dst.Bpb;
    Code[68]:=(1 shl Dst.Bpb)-1;
    Code[89]:=Dst.GShl;
    Code[92]:=Dst.GShl;
    Code[111]:=Dst.Bpg;
    Code[114]:=(1 shl Dst.Bpg)-1;
    Code[119]:=Dst.GShl;
    Code[140]:=Dst.RShl;
    Code[143]:=Dst.RShl;
    Code[162]:=Dst.Bpr;
    Code[165]:=(1 shl Dst.Bpr)-1;
    Code[170]:=Dst.RShl;
  end;

  for i:=0 to Dst.AbsHeight-1 do
    TSubMem16Proc(Code)(Dst.Scanlines[i],Src1.Scanlines[i],Src2.Scanlines[i],Dst.BWidth-Dst.Gap);

  FreeMem(Code);
end;


// no-MMX proc had no advantage over PAS version - removed

// 6.6 times faster than PAS version
procedure xSubMem8MMX(Dst,Src1,Src2:PLine8;Size:Integer);
// Dst  = eax
// Src1 = edx
// Src2 = ecx
// Size = [ebp + 8]
asm
  push ebx
  push edi
  push esi

  mov edi,edx
  mov esi,ecx
  mov ecx,[ebp+8]
  push ecx
  shr ecx,3
  jz  @skip

  @quads:
    db $0F,$6F,$07 /// movq    mm0,[edi]
    db $0F,$6F,$0E /// movq    mm1,[esi]
    db $0F,$D8,$C1 /// psubusb mm0,mm1
    db $0F,$7F,$00 /// movq    [eax],mm0
    add edi,8
    add esi,8
    add eax,8
    dec ecx
  jnz @quads

  db $0F,$77 // emms

  @skip:
  pop ecx
  and ecx,111b
  jz  @exit

  @bytes:
    movzx ebx,Byte([edi])
    movzx edx,Byte([esi])
    sub ebx,edx
    mov edx,ebx
    shr edx,8
    xor edx,-1
    and ebx,edx
    mov [eax],bl
    inc edi
    inc esi
    inc eax
    dec ecx
  jnz @bytes

  @exit:
  pop esi
  pop edi
  pop ebx
end;
{$ENDIF}

{$IFDEF CPUX64}
// Dst = rcx, Src1 = rdx, Src2 = r8, Size = r9
procedure xSubMem8MMX(Dst,Src1,Src2:PLine8;Size:Integer);
asm
  mov rax, r9
  shr rax, 3
  jz  @skip

  @quads:
    movq    mm0,[rdx]
    movq    mm1,[r8]
    psubusb mm0,mm1
    movq    [rcx],mm0
    add rdx,8
    add r8,8
    add rcx,8
    dec rax
  jnz @quads
  emms

  @skip:
  and r9,111b
  jz  @exit

  @bytes:
    movzx r10, byte ptr [rdx]
    movzx rax, byte ptr [r8]
    sub r10,rax
    mov rax,r10
    shr rax,8
    xor rax,-1
    and rax, rbx
    mov [rcx],al
    inc rdx
    inc r8
    inc rcx
    dec r9
  jnz @bytes

  @exit:
end;
{$ENDIF}


procedure xLine_SubSameBpp(Dst, Src1, Src2 : PLine8; Size : Integer);
Var x, i : Integer;
begin
For x:=0 to (Size shr 2)-1 do begin // sort of loop unrolling...
  i := (Src1[0] - Src2[0]); If i < 0 then Dst[0] := 0 else Dst[0] := i;
  i := (Src1[1] - Src2[1]); If i < 0 then Dst[1] := 0 else Dst[1] := i;
  i := (Src1[2] - Src2[2]); If i < 0 then Dst[2] := 0 else Dst[2] := i;
  i := (Src1[3] - Src2[3]); If i < 0 then Dst[3] := 0 else Dst[3] := i;
  Inc(PByte(Dst), 4); Inc(PByte(Src1), 4); Inc(PByte(Src2), 4);
end;
For x:=0 to (Size and 3)-1 do begin
  i := (Src1[0] - Src2[0]); If i < 0 then Dst[0] := 0 else Dst[0] := i;
  Inc(PByte(Dst)); Inc(PByte(Src1)); Inc(PByte(Src2));
end;
end;


procedure SubBlend(Dst,Src1,Src2:TFastDIB);
begin
{$IFDEF CPUX86}
If (Src1.Bpp = 16) and (Src2.Bpp = 16) and (Dst.Bpp = 16) then
  xSubBlend16(Dst, Src1, Src2)
else
  xAnyBlend(Dst, Src1, Src2, xLine_SubSameBpp, xSubMem8MMX);
//  xAnyBlendT(Dst, Src1, Src2, 3, xLine_SubSameBpp, xSubMem8MMX, xLine_SubSameBpp);
{$ELSE}
  xAnyBlend(Dst, Src1, Src2, xSubMem8MMX, xSubMem8MMX);
{$ENDIF}
end;

// ********************************* AddBlend **********************************

{$IFDEF CPUX86}
{
procedure AddMem16(Dst,Src1,Src2:Pointer;Size:Integer);
// Dst  = eax
// Src1 = edx
// Src2 = ecx
// Size = [ebp +  8]
// a    = [ebp -  4]
// b    = [ebp -  8]
// c    = [ebp - 12]
var
  a,b,c: Integer;
asm
  push ebx
  push edi
  push esi

  mov  edi,edx
  mov  esi,ecx
  mov  ecx,[ebp+8]
  push ecx
  and  ecx,$FFFFFFFC
  jz   @skip

  add ecx,eax
  mov [ebp+8],ecx

  @dwords:
    mov ecx,[edi]
    mov ebx,[esi]
    and ecx,$001F001F
    and ebx,$001F001F
    add ecx,ebx
    mov ebx,ecx
    and ebx,$00200020
    shr ebx,5
    imul ebx,$1F
    or  ecx,ebx

    mov edx,[edi]
    mov ebx,[esi]
    and edx,$07E007E0
    and ebx,$07E007E0
    shr edx,5
    shr ebx,5
    add edx,ebx
    mov ebx,edx
    and ebx,$00400040
    shr ebx,6
    imul ebx,$3F
    or  edx,ebx
    shl edx,5
    or  ecx,edx

    mov edx,[edi]
    mov ebx,[esi]
    and edx,$F800F800
    and ebx,$F800F800
    shr edx,11
    shr ebx,11
    add edx,ebx
    mov ebx,edx
    and ebx,$00200020
    shr ebx,5
    imul ebx,$1F
    or  edx,ebx
    shl edx,11
    or  ecx,edx

    mov [eax],ecx
    add edi,4
    add esi,4
    add eax,4
    cmp eax,[ebp+8]
  jne @dwords

  cmp edi,ebp
  je  @last

  @skip:
  pop ecx
  and ecx,11b
  shr ecx,1
  jz  @exit

  mov  cx,[edi]
  mov  bx,[esi]
  mov  a,ecx
  mov  b,ebx
  lea  edi,a
  lea  esi,b
  push eax
  lea  eax,c
  lea  ebx,[eax+4]
  mov  [ebp+8],ebx
  jmp  @dwords

  @last:
  pop eax
  mov ebx,[ebp-12]
  mov [eax],bx

  @exit:
  pop esi
  pop edi
  pop ebx
end;

procedure AddMem16MMX(Dst,Src1,Src2:Pointer;Size:Integer);
// Dst   = eax
// Src1  = edx
// Src2  = ecx
// Size  = [ebp +  8]
// B1:B2 = [ebp -  8]
// G1:G2 = [ebp - 16]
// R1:R2 = [ebp - 24]
// M1:M2 = [ebp - 32]
var
  B1,B2,G1,G2,R1,R2,M1,M2: Integer;
asm
  push edi
  push esi

  mov B1,$001F001F
  mov B2,$001F001F
  mov G1,$07E007E0
  mov G2,$07E007E0
  mov R1,$F800F800
  mov R2,$F800F800
  mov M2,0
  mov M1,0

  movq  mm6,[ebp-16]
  psrlw mm6,5
  movq  mm7,[ebp-24]
  psrlw mm7,11

  mov edi,edx
  mov esi,ecx
  mov ecx,[ebp+8]
  push ecx
  shr ecx,3
  jz  @skip

  @quads:
    movq    mm0,[edi]
    movq    mm1,[esi]
  @cleanup:
    movq    mm2,mm0
    movq    mm3,mm1
    movq    mm4,mm0
    movq    mm5,mm1
    pand    mm0,[ebp-8]
    pand    mm1,[ebp-8]
    paddusw mm0,mm1
    movq    mm1,mm0
    pcmpgtw mm1,[ebp-8]
    psrlw   mm1,11
    por     mm0,mm1

    pand    mm2,[ebp-16]
    pand    mm3,[ebp-16]
    psrlw   mm2,5
    psrlw   mm3,5
    paddusw mm2,mm3
    movq    mm3,mm2
    pcmpgtw mm3,mm6
    psrlw   mm3,10
    por     mm2,mm3
    psllw   mm2,5
    por     mm0,mm2

    pand    mm4,[ebp-24]
    pand    mm5,[ebp-24]
    psrlw   mm4,11
    psrlw   mm5,11
    paddusw mm4,mm5
    movq    mm5,mm4
    pcmpgtw mm5,mm7
    psrlw   mm5,11
    por     mm4,mm5
    psllw   mm4,11
    por     mm0,mm4
    movq    [eax],mm0

    add edi,8
    add esi,8
    add eax,8
    dec ecx
  jnz @quads

  cmp edi,0
  je @last

  @skip:
  pop ecx
  and ecx,111b
  shr ecx,1
  jz  @exit

  mov  edx,ecx
  push edi
  lea  edi,M1
  rep  movsw
  movq mm1,[ebp-32]
  pop  esi
  mov  ecx,edx
  lea  edi,M1
  rep  movsw
  movq mm0,[ebp-32]
  push eax
  lea  eax,M1
  mov  edi,-8
  mov  ecx,1
  jmp  @cleanup

  @last:
  pop edi
  lea esi,M1
  mov ecx,edx
  rep movsw

  @exit:
  emms
  pop esi
  pop edi
end;
}

procedure xAddBlend16(Dst,Src1,Src2:TFastDIB);
type
  TAddMem16REG = array[0..235]of Byte; PAddMem16REG =^TAddMem16REG;
  TAddMem16MMX = array[0..346]of Byte; PAddMem16MMX =^TAddMem16MMX;
  TAddMemProc = procedure(Dst,Src1,Src2:Pointer;Size:Integer);
const
  AddMem16REG: TAddMem16REG =
  ($55,$8B,$EC,$83,$C4,$F4,$53,$57,$56,$89,$D7,$89,$CE,$8B,$4D,$08,$51,$83,$E1,
   $FC,$0F,$84,$99,$00,$00,$00,$01,$C1,$89,$4D,$08,$8B,$0F,$8B,$1E,$81,$E1,$1F,
   $00,$1F,$00,$81,$E3,$1F,$00,$1F,$00,$01,$D9,$89,$CB,$81,$E3,$20,$00,$20,$00,
   $C1,$EB,$05,$6B,$DB,$1F,$09,$D9,$8B,$17,$8B,$1E,$81,$E2,$E0,$07,$E0,$07,$81,
   $E3,$E0,$07,$E0,$07,$C1,$EA,$05,$C1,$EB,$05,$01,$DA,$89,$D3,$81,$E3,$40,$00,
   $40,$00,$C1,$EB,$06,$6B,$DB,$3F,$09,$DA,$C1,$E2,$05,$09,$D1,$8B,$17,$8B,$1E,
   $81,$E2,$00,$F8,$00,$F8,$81,$E3,$00,$F8,$00,$F8,$C1,$EA,$0B,$C1,$EB,$0B,$01,
   $DA,$89,$D3,$81,$E3,$20,$00,$20,$00,$C1,$EB,$05,$6B,$DB,$1F,$09,$DA,$C1,$E2,
   $0B,$09,$D1,$89,$08,$83,$C7,$04,$83,$C6,$04,$83,$C0,$04,$3B,$45,$08,$0F,$85,
   $70,$FF,$FF,$FF,$39,$EF,$74,$29,$59,$83,$E1,$03,$D1,$E9,$74,$28,$66,$8B,$0F,
   $66,$8B,$1E,$89,$4D,$FC,$89,$5D,$F8,$8D,$7D,$FC,$8D,$75,$F8,$50,$8D,$45,$F4,
   $8D,$58,$04,$89,$5D,$08,$E9,$43,$FF,$FF,$FF,$58,$8B,$5D,$F4,$66,$89,$18,$5E,
   $5F,$5B,$8B,$E5,$5D,$C2,$04,$00);

  AddMem16MMX: TAddMem16MMX =
  ($55,$8B,$EC,$83,$C4,$E0,$57,$56,$C7,$45,$FC,$1F,$00,$1F,$00,$C7,$45,$F8,$1F,
   $00,$1F,$00,$C7,$45,$F4,$E0,$07,$E0,$07,$C7,$45,$F0,$E0,$07,$E0,$07,$C7,$45,
   $EC,$00,$F8,$00,$F8,$C7,$45,$E8,$00,$F8,$00,$F8,$C7,$45,$E4,$00,$00,$00,$00,
   $C7,$45,$E0,$00,$00,$00,$00,$0F,$6F,$B5,$F0,$FF,$FF,$FF,$0F,$71,$D6,$05,$0F,
   $6F,$BD,$E8,$FF,$FF,$FF,$0F,$71,$D7,$0B,$89,$D7,$89,$CE,$8B,$4D,$08,$51,$C1,
   $E9,$03,$0F,$84,$A6,$00,$00,$00,$0F,$6F,$07,$0F,$6F,$0E,$0F,$6F,$D0,$0F,$6F,
   $D9,$0F,$6F,$E0,$0F,$6F,$E9,$0F,$DB,$85,$F8,$FF,$FF,$FF,$0F,$DB,$8D,$F8,$FF,
   $FF,$FF,$0F,$DD,$C1,$0F,$6F,$C8,$0F,$65,$8D,$F8,$FF,$FF,$FF,$0F,$71,$D1,$0B,
   $0F,$EB,$C1,$0F,$DB,$95,$F0,$FF,$FF,$FF,$0F,$DB,$9D,$F0,$FF,$FF,$FF,$0F,$71,
   $D2,$05,$0F,$71,$D3,$05,$0F,$DD,$D3,$0F,$6F,$DA,$0F,$65,$DE,$0F,$71,$D3,$0A,
   $0F,$EB,$D3,$0F,$71,$F2,$05,$0F,$EB,$C2,$0F,$DB,$A5,$E8,$FF,$FF,$FF,$0F,$DB,
   $AD,$E8,$FF,$FF,$FF,$0F,$71,$D4,$0B,$0F,$71,$D5,$0B,$0F,$DD,$E5,$0F,$6F,$EC,
   $0F,$65,$EF,$0F,$71,$D5,$0B,$0F,$EB,$E5,$0F,$71,$F4,$0B,$0F,$EB,$C4,$0F,$7F,
   $00,$83,$C7,$08,$83,$C6,$08,$83,$C0,$08,$49,$0F,$85,$5F,$FF,$FF,$FF,$83,$FF,
   $00,$74,$3B,$59,$83,$E1,$07,$D1,$E9,$74,$3C,$89,$CA,$57,$8D,$7D,$E0,$66,$F3,
   $A5,$0F,$6F,$8D,$E0,$FF,$FF,$FF,$5E,$89,$D1,$8D,$7D,$E0,$66,$F3,$A5,$0F,$6F,
   $85,$E0,$FF,$FF,$FF,$50,$8D,$45,$E0,$BF,$F8,$FF,$FF,$FF,$B9,$01,$00,$00,$00,
   $E9,$25,$FF,$FF,$FF,$5F,$8D,$75,$E0,$89,$D1,$66,$F3,$A5,$0F,$77,$5E,$5F,$8B,
   $E5,$5D,$C2,$04,$00);
var
  i: Integer;
  Code: PLine8;
begin
  if cfMMX in CPUInfo.Features then
  begin
    GetMem(Code,SizeOf(TAddMem16MMX));
    PAddMem16MMX(Code)^:=AddMem16MMX;

    i:=Dst.BMask shl 16 or Dst.BMask;
    PDWord(@Code[11])^:=i;
    PDWord(@Code[18])^:=i;
    i:=Dst.GMask shl 16 or Dst.GMask;
    PDWord(@Code[25])^:=i;
    PDWord(@Code[32])^:=i;
    i:=Dst.RMask shl 16 or Dst.RMask;
    PDWord(@Code[39])^:=i;
    PDWord(@Code[46])^:=i;
    Code[74]:=Dst.GShl;
    Code[85]:=Dst.RShl;
    Code[151]:=16-Dst.Bpb;
    Code[172]:=Dst.GShl;
    Code[176]:=Dst.GShl;
    Code[189]:=16-Dst.Bpg;
    Code[196]:=Dst.GShl;
    Code[217]:=Dst.RShl;
    Code[221]:=Dst.RShl;
    Code[234]:=16-Dst.Bpr;
    Code[241]:=Dst.RShl;
  end else
  begin
    GetMem(Code,SizeOf(TAddMem16REG));
    PAddMem16REG(Code)^:=AddMem16REG;

    i:=Dst.BMask shl 16 or Dst.BMask;
    PDWord(@Code[37])^:=i;
    PDWord(@Code[43])^:=i;
    PDWord(@Code[53])^:=(i+i)and(not i);
    i:=Dst.GMask shl 16 or Dst.GMask;
    PDWord(@Code[71])^:=i;
    PDWord(@Code[77])^:=i;
    i:=i shr Dst.GShl;
    PDWord(@Code[93])^:=(i+i)and(not i);
    i:=Dst.RMask shl 16 or Dst.RMask;
    PDWord(@Code[116])^:=i;
    PDWord(@Code[122])^:=i;
    i:=i shr Dst.RShl;
    PDWord(@Code[138])^:=(i+i)and(not i);
    Code[59]:=Dst.Bpb;
    Code[62]:=(1 shl Dst.Bpb)-1;
    Code[83]:=Dst.GShl;
    Code[86]:=Dst.GShl;
    Code[99]:=Dst.Bpg;
    Code[102]:=(1 shl Dst.Bpg)-1;
    Code[107]:=Dst.GShl;
    Code[128]:=Dst.RShl;
    Code[131]:=Dst.RShl;
    Code[144]:=Dst.Bpr;
    Code[147]:=(1 shl Dst.Bpr)-1;
    Code[152]:=Dst.RShl;
  end;

  for i:=0 to Dst.AbsHeight-1 do
    TAddMemProc(Code)(Dst.Scanlines[i],Src1.Scanlines[i],Src2.Scanlines[i],Dst.BWidth-Dst.Gap);

  FreeMem(Code);
end;

// 25% faster than PAS version
procedure xAddMem8(Dst,Src1,Src2:PLine8;Size:Integer);
// Dst  = eax
// Src1 = edx
// Src2 = ecx
// Size = [ebp + 8]
var
  s: Integer;
asm
  push ebx
  push edi
  push esi

  mov edi,edx
  mov esi,ecx
  mov ecx,[ebp+8]
  push ecx
  mov s,esp
  mov esp,eax
  and ecx,$FFFFFFFC
  jz @skip

  add ecx,eax
  mov [ebp+8],ecx

  @dwords:
    mov eax,[edi]
    mov ebx,[esi]
    mov ecx,eax
    mov edx,ebx

    and eax,$00FF00FF
    and ebx,$00FF00FF
    add eax,ebx
    mov ebx,eax
    and ebx,$01000100
    shr ebx,8
    imul ebx,$FF
    or  eax,ebx

    and ecx,$FF00FF00
    and edx,$FF00FF00
    shr ecx,8
    shr edx,8
    add ecx,edx
    mov edx,ecx
    and edx,$01000100
    shr edx,8
    imul edx,$FF
    or  ecx,edx

    shl ecx,8
    or eax,ecx
    mov [esp],eax

    add edi,4
    add esi,4
    add esp,4
    cmp esp,[ebp+8]
  jne @dwords

  @skip:
  mov eax,esp
  mov esp,s
  pop ecx
  and ecx,11b
  jz @exit

  @bytes:
    movzx ebx,Byte([esi])
    movzx edx,Byte([edi])
    add ebx,edx
    mov edx,ebx
    and edx,$0100
    sub edx,$0100
    xor edx,-1
    shr edx,8
    or  ebx,edx
    mov [eax],bl
    inc esi
    inc edi
    inc eax
    dec ecx
  jnz @bytes

  @exit:
  pop esi
  pop edi
  pop ebx
end;

// 8 times faster than PAS version
procedure xAddMem8MMX(Dst,Src1,Src2:PLine8;Size:Integer);
// Dst  = eax
// Src1 = edx
// Src2 = ecx
// Size = [ebp + 8]
asm
  push ebx
  push edi
  push esi

  mov edi,edx
  mov esi,ecx
  mov ecx,[ebp+8]
  push ecx
  shr ecx,3
  jz @skip

  @quads:
    db $0F,$6F,$07  /// movq    mm0,[edi]
    db $0F,$6F,$0E  /// movq    mm1,[esi]
    db $0F,$DC,$C1  /// paddusb mm0,mm1
    db $0F,$7F,$00  /// movq    [eax],mm0
    add edi,8
    add esi,8
    add eax,8
    dec ecx
  jnz @quads

  db $0F,$77 // emms

  @skip:
  pop ecx
  and ecx,111b
  jz  @exit

  @bytes:
    movzx ebx,Byte([esi])
    movzx edx,Byte([edi])
    add ebx,edx
    mov edx,ebx
    and edx,$0100
    sub edx,$0100
    xor edx,-1
    shr edx,8
    or  ebx,edx
    mov [eax],bl

    inc edi
    inc esi
    inc eax
    dec ecx
  jnz @bytes

  @exit:
  pop esi
  pop edi
  pop ebx
end;
{$ENDIF}

{$IFDEF CPUX64}
// Dst = rcx, Src1 = rdx, Src2 = r8, Size = r9
procedure xAddMem8MMX(Dst,Src1,Src2:PLine8;Size:Integer);
asm
  mov rax, r9
  shr rax, 3
  jz  @skip

  @quads:
    movq    mm0,[rdx]
    movq    mm1,[r8]
    paddusb mm0,mm1
    movq    [rcx],mm0
    add rdx,8
    add r8,8
    add rcx,8
    dec rax
  jnz @quads
  emms

  @skip:
  and r9,111b
  jz  @exit

  @bytes:
    movzx r10, byte ptr [rdx]
    movzx rax, byte ptr [r8]
    add r10,rax
    mov rax,r10
    and rax,$0100
    sub rax,$0100
    xor rax,-1
    shr rax,8
    or  rax,r10
    mov [rcx],al
    inc rdx
    inc r8
    inc rcx
    dec r9
  jnz @bytes

  @exit:
end;
{$ENDIF}


procedure xLine_AddSameBpp(Dst, Src1, Src2 : PLine8; Size : Integer);
Var x, i : Integer;
begin
For x:=0 to (Size shr 2)-1 do begin // sort of loop unrolling...
  i := Src1[0] + Src2[0]; If i > 255 then Dst[0] := 255 else Dst[0] := i;
  i := Src1[1] + Src2[1]; If i > 255 then Dst[1] := 255 else Dst[1] := i;
  i := Src1[2] + Src2[2]; If i > 255 then Dst[2] := 255 else Dst[2] := i;
  i := Src1[3] + Src2[3]; If i > 255 then Dst[3] := 255 else Dst[3] := i;
  Inc(PByte(Dst), 4); Inc(PByte(Src1), 4); Inc(PByte(Src2), 4);
end;
For x:=0 to (Size and 3)-1 do begin
  i := Src1[0] + Src2[0]; If i > 255 then Dst[0] := 255 else Dst[0] := i;
  Inc(PByte(Dst)); Inc(PByte(Src1)); Inc(PByte(Src2));
end;
end;


procedure AddBlend(Dst,Src1,Src2:TFastDIB);
begin
{$IFDEF CPUX86}
If (Src1.Bpp = 16) and (Src2.Bpp = 16) and (Dst.Bpp = 16) then
  xAddBlend16(Dst, Src1, Src2)
else
  xAnyBlend(Dst, Src1, Src2, xAddMem8, xAddMem8MMX);
{$ELSE}
//xAnyBlend(Dst, Src1, Src2, xLine_AddSameBpp, xLine_AddSameBpp);
xAnyBlend(Dst, Src1, Src2, xAddMem8MMX, xAddMem8MMX);
{$ENDIF}
end;


// ********************************* MulBlend **********************************

{$IFDEF CPUX86}

{
procedure MulMem16(Dst,Src1,Src2:Pointer;Size:Integer);
// Dst  = eax
// Src1 = edx
// Src2 = ecx
// Size = [ebp + 8]
// a    = [ebp - 4]
// b    = [ebp - 8]
var
  a,b,s: Integer;
asm
  push ebx
  push edi
  push esi
  mov s,esp

  mov edi,edx
  mov esi,ecx
  mov esp,eax

  mov ecx,[ebp+8]
  shr ecx,1
  jz @exit

  @words:
    movzx ebx,Word([edi])
    movzx edx,Word([esi])
    mov a,ebx
    mov b,edx

    and ebx,$0000001F
    and edx,$0000001F
    imul ebx,edx
    shr ebx,5

    mov eax,a
    mov edx,b
    and eax,$000007E0
    and edx,$000007E0
    shr eax,5
    shr edx,5
    imul eax,edx
    shr eax,6
    shl eax,5
    or ebx,eax

    mov eax,a
    mov edx,b
    and eax,$0000F800
    and edx,$0000F800
    shr eax,11
    shr edx,11
    imul eax,edx
    shr eax,5
    shl eax,11
    or ebx,eax

    mov [esp],bx
    add edi,2
    add esi,2
    add esp,2
    dec ecx
  jnz @words

  @exit:
  mov esp,s
  pop esi
  pop edi
  pop ebx
end;

procedure MulMem16MMX(Dst,Src1,Src2:Pointer;Size:Integer);
// Dst   = eax
// Src1  = edx
// Src2  = ecx
// Size  = [ebp +  8]
// HB:LB = [ebp -  8]
// HG:LG = [ebp - 16]
// HR:LR = [ebp - 24]
// AA:BB = [ebp - 32]
var
  HB,LB,HG,LG,HR,LR,AA,BB: Integer;
asm
  push edi
  push esi

  mov HR,$F800F800
  mov LR,$F800F800
  mov HG,$07E007E0
  mov LG,$07E007E0
  mov HB,$001F001F
  mov LB,$001F001F
  mov AA,0
  mov BB,0

  mov  edi,edx
  mov  esi,ecx
  mov  ecx,[ebp+8]
  push ecx
  shr  ecx,3
  jz   @skip

  @quads:
    movq   mm0,[edi]
    movq   mm1,[esi]
  @cleanup:
    movq   mm2,mm0
    movq   mm3,mm0
    movq   mm4,mm1
    movq   mm5,mm1
    pand   mm0,[ebp-24]
    pand   mm1,[ebp-24]
    pmullw mm0,mm1
    psrlw  mm0,5
    pand   mm2,[ebp-16]
    pand   mm4,[ebp-16]
    psrlw  mm2,5
    psrlw  mm4,5
    pmullw mm2,mm4
    psrlw  mm2,6
    psllw  mm2,5
    por    mm0,mm2
    pand   mm3,[ebp-8]
    pand   mm5,[ebp-8]
    psrlw  mm3,11
    psrlw  mm5,11
    pmullw mm3,mm5
    psrlw  mm3,5
    psllw  mm3,11
    por    mm0,mm3
    movq   [eax],mm0

    add edi,8
    add esi,8
    add eax,8
    dec ecx
  jnz @quads

  cmp edi,0
  je @last

  @skip:
  pop ecx
  and ecx,111b
  shr ecx,1
  jz  @exit

  mov  edx,ecx
  push edi
  mov  edi,ebp
  sub  edi,32 // edi = ebp-32
  rep  movsw
  movq mm1,[ebp-32]
  pop  esi
  sub  edi,edx // edi = ebp-32
  mov  ecx,edx
  rep  movsw
  movq mm0,[ebp-32]
  mov  ecx,1
  mov  edi,-8
  push eax
  mov  eax,ebp
  sub  eax,32
  jmp  @cleanup

  @last:
  mov ecx,edx
  pop edi
  mov esi,ebp
  sub esi,32
  rep movsw

  @exit:
  emms
  pop esi
  pop edi
end;
}

procedure xMulBlend16(Dst,Src1,Src2:TFastDIB);
type
  TMulMem16REG = array[0..150]of Byte; PMulMem16REG =^TMulMem16REG;
  TMulMem16MMX = array[0..294]of Byte; PMulMem16MMX =^TMulMem16MMX;
  TMulMemProc = procedure(Dst,Src1,Src2:Pointer;Size:Integer);
const
  MulMem16REG: TMulMem16REG =
  ($55,$8B,$EC,$83,$C4,$F4,$53,$57,$56,$89,$65,$FC,$89,$D7,$89,$CE,$89,$C4,$8B,
   $4D,$08,$D1,$E9,$74,$72,$0F,$B7,$1F,$0F,$B7,$16,$89,$5D,$F8,$89,$55,$F4,$81,
   $E3,$1F,$00,$00,$00,$81,$E2,$1F,$00,$00,$00,$0F,$AF,$DA,$C1,$EB,$05,$8B,$45,
   $F8,$8B,$55,$F4,$25,$E0,$07,$00,$00,$81,$E2,$E0,$07,$00,$00,$C1,$E8,$05,$C1,
   $EA,$05,$0F,$AF,$C2,$C1,$E8,$06,$C1,$E0,$05,$09,$C3,$8B,$45,$F8,$8B,$55,$F4,
   $25,$00,$F8,$00,$00,$81,$E2,$00,$F8,$00,$00,$C1,$E8,$0B,$C1,$EA,$0B,$0F,$AF,
   $C2,$C1,$E8,$05,$C1,$E0,$0B,$09,$C3,$66,$89,$1C,$24,$83,$C7,$02,$83,$C6,$02,
   $83,$C4,$02,$49,$75,$8E,$8B,$65,$FC,$5E,$5F,$5B,$8B,$E5,$5D,$C2,$04,$00);

  MulMem16MMX: TMulMem16MMX =
  ($55,$8B,$EC,$83,$C4,$E0,$57,$56,$C7,$45,$FC,$00,$F8,$00,$F8,$C7,$45,$F8,$00,
   $F8,$00,$F8,$C7,$45,$F4,$E0,$07,$E0,$07,$C7,$45,$F0,$E0,$07,$E0,$07,$C7,$45,
   $EC,$1F,$00,$1F,$00,$C7,$45,$E8,$1F,$00,$1F,$00,$C7,$45,$E4,$00,$00,$00,$00,
   $C7,$45,$E0,$00,$00,$00,$00,$89,$D7,$89,$CE,$8B,$4D,$08,$51,$C1,$E9,$03,$0F,
   $84,$83,$00,$00,$00,$0F,$6F,$07,$0F,$6F,$0E,$0F,$6F,$D0,$0F,$6F,$D8,$0F,$6F,
   $E1,$0F,$6F,$E9,$0F,$DB,$85,$E8,$FF,$FF,$FF,$0F,$DB,$8D,$E8,$FF,$FF,$FF,$0F,
   $D5,$C1,$0F,$71,$D0,$05,$0F,$DB,$95,$F0,$FF,$FF,$FF,$0F,$DB,$A5,$F0,$FF,$FF,
   $FF,$0F,$71,$D2,$05,$0F,$71,$D4,$05,$0F,$D5,$D4,$0F,$71,$D2,$06,$0F,$71,$F2,
   $05,$0F,$EB,$C2,$0F,$DB,$9D,$F8,$FF,$FF,$FF,$0F,$DB,$AD,$F8,$FF,$FF,$FF,$0F,
   $71,$D3,$0B,$0F,$71,$D5,$0B,$0F,$D5,$DD,$0F,$71,$D3,$05,$0F,$71,$F3,$0B,$0F,
   $EB,$C3,$0F,$7F,$00,$83,$C7,$08,$83,$C6,$08,$83,$C0,$08,$49,$75,$82,$83,$FF,
   $00,$74,$3E,$59,$83,$E1,$07,$D1,$E9,$74,$41,$89,$CA,$57,$89,$EF,$83,$EF,$20,
   $66,$F3,$A5,$0F,$6F,$8D,$E0,$FF,$FF,$FF,$5E,$29,$D7,$89,$D1,$66,$F3,$A5,$0F,
   $6F,$85,$E0,$FF,$FF,$FF,$B9,$01,$00,$00,$00,$BF,$F8,$FF,$FF,$FF,$50,$89,$E8,
   $83,$E8,$20,$E9,$45,$FF,$FF,$FF,$89,$D1,$5F,$89,$EE,$83,$EE,$20,$66,$F3,$A5,
   $0F,$77,$5E,$5F,$8B,$E5,$5D,$C2,$04,$00);
var
  i: Integer;
  Code: PLine8;
begin
  if cfMMX in CPUInfo.Features then
  begin
    GetMem(Code,SizeOf(TMulMem16MMX));
    PMulMem16MMX(Code)^:=MulMem16MMX;

    i:=Dst.RMask shl 16 or Dst.RMask;
    PDWord(@Code[11])^:=i;
    PDWord(@Code[18])^:=i;
    i:=Dst.GMask shl 16 or Dst.GMask;
    PDWord(@Code[25])^:=i;
    PDWord(@Code[32])^:=i;
    i:=Dst.BMask shl 16 or Dst.BMask;
    PDWord(@Code[39])^:=i;
    PDWord(@Code[46])^:=i;
    Code[119]:=Dst.Bpb;
    Code[137]:=Dst.GShl;
    Code[141]:=Dst.GShl;
    Code[148]:=Dst.Bpg;
    Code[152]:=Dst.GShl;
    Code[173]:=Dst.RShl;
    Code[177]:=Dst.RShl;
    Code[184]:=Dst.Bpr;
    Code[188]:=Dst.RShl;
  end else
  begin
    GetMem(Code,SizeOf(TMulMem16REG));
    PMulMem16REG(Code)^:=MulMem16REG;

    PDWord(@Code[39])^:=Dst.BMask;
    PDWord(@Code[45])^:=Dst.BMask;
    PDWord(@Code[62])^:=Dst.GMask;
    PDWord(@Code[68])^:=Dst.GMask;
    PDWord(@Code[96])^:=Dst.RMask;
    PDWord(@Code[102])^:=Dst.RMask;
    Code[54]:=Dst.Bpb;
    Code[74]:=Dst.GShl;
    Code[77]:=Dst.GShl;
    Code[83]:=Dst.Bpg;
    Code[86]:=Dst.GShl;
    Code[108]:=Dst.RShl;
    Code[111]:=Dst.RShl;
    Code[117]:=Dst.Bpr;
    Code[120]:=Dst.RShl;
  end;

  for i:=0 to Dst.AbsHeight-1 do
    TMulMemProc(Code)(Dst.Scanlines[i],Src1.Scanlines[i],Src2.Scanlines[i],Dst.BWidth-Dst.Gap);

  FreeMem(Code);
end;


// no-MMX proc removed - no advantage over PAS version

// 2 times faster than PAS version
procedure xMulMem8MMX(Dst,Src1,Src2:PLine8;Size:Integer);
// Dst  = eax
// Src1 = edx
// Src2 = ecx
// Size = [ebp + 8]
asm
  push ebx
  push edi
  push esi

  mov edi,edx
  mov esi,ecx
  mov ecx,[ebp+8]
  push ecx
  shr ecx,2
  jz @skip

  db $0F,$EF,$D2       /// pxor mm2,mm2

  @dwords:
    db $0F,$6E,$07       /// movd       mm0,[edi]
    db $0F,$6E,$0E       /// movd       mm1,[esi]
    db $0F,$60,$C2       /// punpcklbw  mm0,mm2
    db $0F,$60,$CA       /// punpcklbw  mm1,mm2
    db $0F,$D5,$C1       /// pmullw     mm0,mm1
    db $0F,$71,$D0,$08   /// psrlw      mm0,8
    db $0F,$67,$C0       /// packuswb   mm0,mm0
    db $0F,$7E,$00       /// movd       [eax],mm0

    add edi,4
    add esi,4
    add eax,4
    dec ecx
  jnz @dwords

  db $0F,$77 // emms

  @skip:
  pop ecx
  and ecx,11b
  jz @exit

  @bytes:
    movzx ebx,Byte([edi])
    movzx edx,Byte([esi])
    imul ebx,edx
    shr ebx,8
    mov [eax],bl
    inc edi
    inc esi
    inc eax
    dec ecx
  jnz @bytes

  @exit:
  pop esi
  pop edi
  pop ebx
end;
{$ENDIF}

{$IFDEF CPUX64}
// Dst = rcx, Src1 = rdx, Src2 = r8, Size = r9
procedure xMulMem8MMX(Dst,Src1,Src2:PLine8;Size:Integer);
asm
  mov rax, r9
  shr rax, 2
  jz  @skip

  pxor mm2,mm2
  @quads:
    movd    mm0,[rdx]
    movd    mm1,[r8]
    punpcklbw  mm0,mm2
    punpcklbw  mm1,mm2
    pmullw     mm0,mm1
    psrlw      mm0,8
    packuswb   mm0,mm0
    movd    [rcx],mm0
    add rdx,4
    add r8,4
    add rcx,4
    dec rax
  jnz @quads
  emms

  @skip:
  and r9,11b
  jz  @exit

  @bytes:
    movzx r10, byte ptr [rdx]
    movzx rax, byte ptr [r8]
    imul rax, r10
    shr rax,8
    mov [rcx],al
    inc rdx
    inc r8
    inc rcx
    dec r9
  jnz @bytes

  @exit:
end;
{$ENDIF}



procedure xLine_MulSameBpp(Dst, Src1, Src2 : PLine8; Size : Integer);
Var x : Integer;
begin
For x:=0 to (Size shr 2)-1 do begin // sort of loop unrolling...
  Dst[0] := (Src1[0] * Src2[0]) shr 8;
  Dst[1] := (Src1[1] * Src2[1]) shr 8;
  Dst[2] := (Src1[2] * Src2[2]) shr 8;
  Dst[3] := (Src1[3] * Src2[3]) shr 8;
  Inc(PByte(Dst), 4); Inc(PByte(Src1), 4); Inc(PByte(Src2), 4);
end;
For x:=0 to (Size and 3)-1 do begin
  Dst[0] := (Src1[0] * Src2[0]) shr 8;
  Inc(PByte(Dst)); Inc(PByte(Src1)); Inc(PByte(Src2));
end;
end;

procedure MulBlend(Dst,Src1,Src2:TFastDIB);
begin
{$IFDEF CPUX86}
If (Src1.Bpp = 16) and (Src2.Bpp = 16) and (Dst.Bpp = 16) then
  xMulBlend16(Dst, Src1, Src2)
else
  xAnyBlend(Dst, Src1, Src2, xLine_MulSameBpp, xMulMem8MMX);
{$ELSE}
//xAnyBlend(Dst, Src1, Src2, xLine_MulSameBpp, xLine_MulSameBpp);
xAnyBlend(Dst, Src1, Src2, xMulMem8MMX, xMulMem8MMX);
{$ENDIF}
end;


// ******************************** AlphaBlend *********************************

{$IFDEF CPUX86}

{
procedure BlendMem16(Dst,Src1,Src2:Pointer;Size,Alpha:Integer);
// Dst   = eax
// Src1  = edx
// Src2  = ecx
// Size  = [ebp + 12]
// Alpha = [ebp +  8]
// a     = [ebp -  4]
// b     = [ebp -  8]
// c     = [ebp - 12]
var
  a,b,c: Cardinal;
asm
  push ebx
  push edi
  push esi
  mov  edi,edx
  mov  esi,ecx

  mov  ecx,[ebp+12]
  and  ecx,3
  push ecx
  sub  [ebp+12],ecx
  jz   @skip
  add  [ebp+12],eax

  @dwords:
    mov  edx,[edi]     // edx = src1
    mov  ecx,[esi]     // ecx = src2
  @words:              //
    mov  a,edx         // a = copy of src1
    mov  b,ecx         // b = copy of src2
                       //
    and  edx,$001F001F // edx = blue_src1
    and  ecx,$001F001F // ecx = blue_src2
    sub  edx,ecx       // (blue_src1-blue_src2)
    imul edx,[ebp+8]   // (blue_src1-blue_src2)*blue_alpha
    shr  edx,8         // (blue_src1-blue_src2)*blue_alpha / 256
    add  edx,ecx       // (blue_src1-blue_src2)*blue_alpha / 256 + blue_src2
    and  edx,$001F001F // edx = [-----------bbbbb-----------bbbbb]
                       //
    mov  ebx,a         // ebx = src1
    mov  ecx,b         // ecx = src2
    and  ebx,$03E003E0 // ebx = green_src1
    and  ecx,$03E003E0 // ecx = green_src2
    shr  ebx,5         // [------ggggg-----] >> [-----------ggggg]
    shr  ecx,5         // [------ggggg-----] >> [-----------ggggg]
    sub  ebx,ecx       // (green_src1-green_src2)
    imul ebx,[ebp+8]   // (green_src1-green_src2)*green_alpha
    shr  ebx,8         // (green_src1-green_src2)*green_alpha / 256
    add  ebx,ecx       // (green_src1-green_src2)*green_alpha / 256 + green_src2
    shl  ebx,5         // [------ggggg-----] << [-----------ggggg]
    and  ebx,$03E003E0 // ebx = [------ggggg-----------ggggg-----]
    or   edx,ebx       // edx = [------gggggbbbbb------gggggbbbbb]
                       //
    mov  ebx,a         // ebx = src1
    mov  ecx,b         // ecx = src2
    and  ebx,$7C007C00 // ebx = red_src1
    and  ecx,$7C007C00 // ecx = red_src2
    shr  ebx,10        // [-rrrrr----------] >> [-----------rrrrr]
    shr  ecx,10        // [-rrrrr----------] >> [-----------rrrrr]
    sub  ebx,ecx       // (red_src1-red_src2)
    imul ebx,[ebp+8]   // (red_src1-red_src2)*red_alpha
    shr  ebx,8         // (red_src1-red_src2)*red_alpha / 256
    add  ebx,ecx       // (red_src1-red_src2)*red_alpha / 256 + red_src2
    shl  ebx,10        // [-rrrrr----------] << [-----------rrrrr]
    and  ebx,$7C007C00 // ebx = [-rrrrr-----------rrrrr----------]
    or   edx,ebx       // edx = [-rrrrrgggggbbbbb-rrrrrgggggbbbbb]

    mov  [eax],edx

    add  eax,4
    add  edi,4
    add  esi,4
    cmp  eax,[ebp+12]
  jne @dwords

  mov  ebx,ebp
  sub  ebx,8
  cmp  eax,ebx
  je   @last //eax = @c (ebp-12)

  @skip:
  pop  ecx
  and  ecx,2
  jz   @exit

  push eax
  mov  ebx,ebp
  sub  ebx,12
  mov  eax,ebx //eax = @c (ebp-12)
  add  ebx,4
  mov  [ebp+12],ebx
  mov  dx,[edi]
  mov  cx,[esi]
  jmp  @words

  @last:
  mov  ebx,c
  pop  eax
  mov  [eax],bx

  @exit:
  pop esi
  pop edi
  pop ebx
end;

procedure BlendMem16MMX(Dst,Src1,Src2:Pointer;Size,Alpha:Integer);
// Dst   = eax
// Src1  = edx
// Src2  = ecx
// Size  = [ebp + 12]
// Alpha = [ebp +  8]
// HA:LA = [ebp -  8]
// HB:LB = [ebp - 16]
var
  HA,LA,HB,LB: Integer;
asm
  push ebx
  push edi
  push esi

  mov edi,edx
  mov esi,ecx
  mov ecx,[ebp+12]
  shr ecx,3
  jz  @skip

  mov    ebx,[ebp+8]
  shl    ebx,16
  or     ebx,[ebp+8]
  mov    HA,ebx
  mov    LA,ebx   // HA:LA = 00aa00aa00aa00aa
  mov    HB,0
  mov    LB,0
  mov    ebx,$001F001F
  movd   mm2,ebx
  movd   mm3,ebx
  psllq  mm2,32
  por    mm2,mm3  // mm2 = 001F001F001F001F
  mov    ebx,$03E003E0
  movd   mm3,ebx
  movd   mm4,ebx
  psllq  mm3,32
  por    mm3,mm4  // mm3 = 03E003E003E003E0
  mov    ebx,$7C007C00
  movd   mm4,ebx
  movd   mm5,ebx
  psllq  mm4,32
  por    mm4,mm5  // mm4 = 7C007C007C007C00

  @quads:
    movq   mm7,[edi]
    movq   mm1,[esi]
  @words:
    movq   mm0,mm7
    movq   mm5,mm1
    pand   mm7,mm2
    pand   mm1,mm2

    psubw  mm7,mm1
    pmullw mm7,[ebp-8]
    psrlw  mm7,8
    paddb  mm7,mm1

    movq   mm1,mm0
    movq   mm6,mm5
    pand   mm0,mm3
    pand   mm5,mm3
    psrlw  mm0,5
    psrlw  mm5,5
    psubw  mm0,mm5
    pmullw mm0,[ebp-8]
    psrlw  mm0,8
    paddb  mm0,mm5
    psllw  mm0,5
    por    mm7,mm0

    pand   mm1,mm4
    pand   mm6,mm4
    psrlw  mm1,10
    psrlw  mm6,10
    psubw  mm1,mm6
    pmullw mm1,[ebp-8]
    psrlw  mm1,8
    paddb  mm1,mm6
    psllw  mm1,10
    por    mm7,mm1
    movq   [eax],mm7

    add eax,8
    add edi,8
    add esi,8
    dec ecx
  jnz @quads

  mov ebx,ebp
  sub ebx,8
  cmp eax,ebx
  je  @last

  @skip:
  mov ecx,[ebp+12]
  and ecx,7
  shr ecx,1
  jz  @exit

  push ecx
  push edi
  push ecx
  mov  edi,ebp
  sub  edi,16
  rep  movsw
  movq mm1,[ebp-16]
  pop  ecx
  pop  esi
  mov  edi,ebp
  sub  edi,16
  rep  movsw
  movq mm7,[ebp-16]
  push eax
  mov  eax,ebp
  sub  eax,16
  mov  ecx,1
  jmp  @words

  @last:
  pop edi
  pop ecx
  mov esi,ebp
  sub esi,16
  rep movsw

  @exit:
  pop esi
  pop edi
  pop ebx

  emms
end;
}

procedure xAlphaBlend16(Dst,Src1,Src2:TFastDIB;Alpha:Integer);
type
  TBlendMem16REG = array[0..238]of Byte; PBlendMem16REG =^TBlendMem16REG;
  TBlendMem16MMX = array[0..334]of Byte; PBlendMem16MMX =^TBlendMem16MMX;
  TBlendMemProc = procedure(Dst,Src1,Src2:Pointer;Size,Alpha:Integer);
const
  BlendMem16REG: TBlendMem16REG =
  ($55,$8B,$EC,$83,$C4,$F4,$53,$57,$56,$89,$D7,$89,$CE,$8B,$4D,$0C,$83,$E1,$03,$51,
   $29,$4D,$0C,$0F,$84,$A3,$00,$00,$00,$01,$45,$0C,$8B,$17,$8B,$0E,$89,$55,$FC,$89,
   $4D,$F8,$81,$E2,$1F,$00,$1F,$00,$81,$E1,$1F,$00,$1F,$00,$29,$CA,$0F,$AF,$55,$08,
   $C1,$EA,$08,$01,$CA,$81,$E2,$1F,$00,$1F,$00,$8B,$5D,$FC,$8B,$4D,$F8,$81,$E3,$E0,
   $03,$E0,$03,$81,$E1,$E0,$03,$E0,$03,$C1,$EB,$05,$C1,$E9,$05,$29,$CB,$0F,$AF,$5D,
   $08,$C1,$EB,$08,$01,$CB,$C1,$E3,$05,$81,$E3,$E0,$03,$E0,$03,$09,$DA,$8B,$5D,$FC,
   $8B,$4D,$F8,$81,$E3,$00,$7C,$00,$7C,$81,$E1,$00,$7C,$00,$7C,$C1,$EB,$0A,$C1,$E9,
   $0A,$29,$CB,$0F,$AF,$5D,$08,$C1,$EB,$08,$01,$CB,$C1,$E3,$0A,$81,$E3,$00,$7C,$00,
   $7C,$09,$DA,$89,$10,$83,$C0,$04,$83,$C7,$04,$83,$C6,$04,$3B,$45,$0C,$0F,$85,$69,
   $FF,$FF,$FF,$89,$EB,$83,$EB,$08,$39,$D8,$74,$1F,$59,$83,$E1,$02,$74,$20,$50,$89,
   $EB,$83,$EB,$0C,$89,$D8,$83,$C3,$04,$89,$5D,$0C,$66,$8B,$17,$66,$8B,$0E,$E9,$45,
   $FF,$FF,$FF,$8B,$5D,$F4,$58,$66,$89,$18,$5E,$5F,$5B,$8B,$E5,$5D,$C2,$08,$00);

  BlendMem16MMX: TBlendMem16MMX =
  ($55,$8B,$EC,$83,$C4,$F0,$53,$57,$56,$89,$D7,$89,$CE,$8B,$4D,$0C,$C1,$E9,$03,$0F,
   $84,$E4,$00,$00,$00,$8B,$5D,$08,$C1,$E3,$10,$0B,$5D,$08,$89,$5D,$FC,$89,$5D,$F8,
   $C7,$45,$F4,$00,$00,$00,$00,$C7,$45,$F0,$00,$00,$00,$00,$BB,$1F,$00,$1F,$00,$0F,
   $6E,$D3,$0F,$6E,$DB,$0F,$73,$F2,$20,$0F,$EB,$D3,$BB,$E0,$03,$E0,$03,$0F,$6E,$DB,
   $0F,$6E,$E3,$0F,$73,$F3,$20,$0F,$EB,$DC,$BB,$00,$7C,$00,$7C,$0F,$6E,$E3,$0F,$6E,
   $EB,$0F,$73,$F4,$20,$0F,$EB,$E5,$0F,$6F,$3F,$0F,$6F,$0E,$0F,$6F,$C7,$0F,$6F,$E9,
   $0F,$DB,$FA,$0F,$DB,$CA,$0F,$F9,$F9,$0F,$D5,$BD,$F8,$FF,$FF,$FF,$0F,$71,$D7,$08,
   $0F,$FC,$F9,$0F,$6F,$C8,$0F,$6F,$F5,$0F,$DB,$C3,$0F,$DB,$EB,$0F,$71,$D0,$05,$0F,
   $71,$D5,$05,$0F,$F9,$C5,$0F,$D5,$85,$F8,$FF,$FF,$FF,$0F,$71,$D0,$08,$0F,$FC,$C5,
   $0F,$71,$F0,$05,$0F,$EB,$F8,$0F,$DB,$CC,$0F,$DB,$F4,$0F,$71,$D1,$0A,$0F,$71,$D6,
   $0A,$0F,$F9,$CE,$0F,$D5,$8D,$F8,$FF,$FF,$FF,$0F,$71,$D1,$08,$0F,$FC,$CE,$0F,$71,
   $F1,$0A,$0F,$EB,$F9,$0F,$7F,$38,$83,$C0,$08,$83,$C7,$08,$83,$C6,$08,$49,$0F,$85,
   $78,$FF,$FF,$FF,$89,$EB,$83,$EB,$08,$39,$D8,$74,$3D,$8B,$4D,$0C,$83,$E1,$07,$D1,
   $E9,$74,$3D,$51,$57,$51,$89,$EF,$83,$EF,$10,$66,$F3,$A5,$0F,$6F,$8D,$F0,$FF,$FF,
   $FF,$59,$5E,$89,$EF,$83,$EF,$10,$66,$F3,$A5,$0F,$6F,$BD,$F0,$FF,$FF,$FF,$50,$89,
   $E8,$83,$E8,$10,$B9,$01,$00,$00,$00,$E9,$38,$FF,$FF,$FF,$5F,$59,$89,$EE,$83,$EE,
   $10,$66,$F3,$A5,$5E,$5F,$5B,$0F,$77,$8B,$E5,$5D,$C2,$08,$00);
var
  i: Integer;
  Code: PLine8;
begin
  if cfMMX in CPUInfo.Features then
  begin
    GetMem(Code,SizeOf(BlendMem16MMX));
    PBlendMem16MMX(Code)^:=BlendMem16MMX;

    PDWord(@Code[55])^:=Dst.BMask shl 16 or Dst.BMask;
    PDWord(@Code[73])^:=Dst.GMask shl 16 or Dst.GMask;
    PDWord(@Code[91])^:=Dst.RMask shl 16 or Dst.RMask;
    Code[158]:=Dst.GShl;
    Code[162]:=Dst.GShl;
    Code[183]:=Dst.GShl;
    Code[196]:=Dst.RShl;
    Code[200]:=Dst.RShl;
    Code[221]:=Dst.RShl;
  end else
  begin
    GetMem(Code,SizeOf(BlendMem16REG));
    PBlendMem16REG(Code)^:=BlendMem16REG;

    i:=Dst.BMask shl 16 or Dst.BMask;
    PDWord(@Code[44])^:=i;
    PDWord(@Code[50])^:=i;
    PDWord(@Code[67])^:=i;
    i:=Dst.GMask shl 16 or Dst.GMask;
    PDWord(@Code[79])^:=i;
    PDWord(@Code[85])^:=i;
    PDWord(@Code[111])^:=i;
    i:=Dst.RMask shl 16 or Dst.RMask;
    PDWord(@Code[125])^:=i;
    PDWord(@Code[131])^:=i;
    PDWord(@Code[157])^:=i;
    Code[91]:=Dst.GShl;
    Code[94]:=Dst.GShl;
    Code[108]:=Dst.GShl;
    Code[137]:=Dst.RShl;
    Code[140]:=Dst.RShl;
    Code[154]:=Dst.RShl;
  end;

  for i:=0 to Dst.AbsHeight-1 do
    TBlendMemProc(Code)(Dst.Scanlines[i],Src1.Scanlines[i],Src2.Scanlines[i],Dst.BWidth-Dst.Gap,Alpha);

  FreeMem(Code);
end;

// 30% faster than PAS version
procedure xBlendMem8(Dst,Src1,Src2:PLine32;Size,Alpha:Integer);
// Dst   = eax
// Src1  = edx
// Src2  = ecx
// Size  = [ebp + 12]
// Alpha = [ebp +  8]
asm
  push ebx
  push edi
  push esi

  mov  edi,edx
  mov  esi,ecx

  mov  ebx,[ebp+12]
  and  ebx,3
  push ebx
  sub  [ebp+12],ebx
  jz   @skip
  add  [ebp+12],eax

  @dwords:
    mov  ebx,[edi]
    mov  edx,[esi]
    and  ebx,$00FF00FF
    and  edx,$00FF00FF
    sub  ebx,edx
    imul ebx,[ebp+8]
    shr  ebx,8
    add  ebx,edx
    and  ebx,$00FF00FF

    mov  ecx,[edi]
    mov  edx,[esi]
    and  ecx,$FF00FF00
    and  edx,$FF00FF00
    shr  ecx,8
    shr  edx,8
    sub  ecx,edx
    imul ecx,[ebp+8]
    shr  ecx,8
    add  ecx,edx
    shl  ecx,8
    and  ecx,$FF00FF00
    or   ecx,ebx
    mov  [eax],ecx

    add  eax,4
    add  esi,4
    add  edi,4
    cmp  eax,[ebp+12]
  jne @dwords

  @skip:
  pop ebx
  cmp ebx,0
  je  @exit
  add ebx,eax
  mov [ebp+12],ebx

  @bytes:
    mov  bl,[edi]
    mov  cl,[esi]
    and  ebx,$FF
    and  ecx,$FF
    sub  ebx,ecx
    imul ebx,[ebp+8]
    shr  ebx,8
    add  ebx,ecx
    mov  [eax],bl
    inc  eax
    inc  esi
    inc  edi
    cmp  eax,[ebp+12]
  jne @bytes

  @exit:
  pop esi
  pop edi
  pop ebx
end;

// 3.5 times faster than PAS version
procedure xBlendMem8MMX(Dst,Src1,Src2:PLine32;Size,Alpha:Integer);
// Dst   = eax
// Src1  = edx
// Src2  = ecx
// Size  = [ebp + 12]
// Alpha = [ebp +  8]
asm
  push ebx
  push edi
  push esi

  mov edi,edx
  mov esi,ecx

  mov ecx,[ebp+12]
  shr ecx,3
  jz  @skip

  mov ebx,[ebp+8]
  shl ebx,16
  or  ebx,[ebp+8]

  db $0F,$6E,$F3     /// movd   mm6,ebx
  db $0F,$6E,$EB     /// movd   mm5,ebx
  db $0F,$73,$F5,$20 /// psllq  mm5,32
  db $0F,$EB,$F5     /// por    mm6,mm5 // mm6 = 00aa00aa00aa00aa
  db $0F,$EF,$FF     /// pxor   mm7,mm7

  @quads:
    db $0F,$6F,$07     /// movq       mm0,[edi] // src1
    db $0F,$6F,$0E     /// movq       mm1,[esi] // src2
    db $0F,$6F,$D0     /// movq       mm2,mm0   // second copy of src1
    db $0F,$6F,$D9     /// movq       mm3,mm1   // second copy of src2
    db $0F,$6F,$E1     /// movq       mm4,mm1   // third copy of src2
    db $0F,$60,$C7     /// punpcklbw  mm0,mm7   // mm0 = unpacked low half of src1
    db $0F,$60,$CF     /// punpcklbw  mm1,mm7   // mm1 = unpacked low half of src2
    db $0F,$68,$D7     /// punpckhbw  mm2,mm7   // mm2 = unpacked high half of src1
    db $0F,$68,$DF     /// punpckhbw  mm3,mm7   // mm3 = unpacked high half of src2
    db $0F,$F9,$C1     /// psubw      mm0,mm1   // mm0 = low half of (src1-src2)
    db $0F,$F9,$D3     /// psubw      mm2,mm3   // mm2 = high half of (src1-src2)
    db $0F,$D5,$C6     /// pmullw     mm0,mm6   // low (src1-src2)*alpha
    db $0F,$D5,$D6     /// pmullw     mm2,mm6   // high (src1-src2)*alpha
    db $0F,$71,$D0,$08 /// psrlw      mm0,8     // low (src1-src2)*alpha / 256
    db $0F,$71,$D2,$08 /// psrlw      mm2,8     // high (src1-src2)*alpha / 256
    db $0F,$67,$C2     /// packuswb   mm0,mm2   // combine with unsigned saturation
    db $0F,$FC,$C4     /// paddb      mm0,mm4   // (src1-src2)*alpha / 256 + src2
    db $0F,$7F,$00     /// movq       [eax],mm0 // store the result
    add eax,8
    add edi,8
    add esi,8
    dec ecx
  jnz @quads

  @skip:
  mov ecx,[ebp+12]
  and ecx,111b
  jz  @exit
  add ecx,eax
  mov [ebp+12],ecx

  @bytes:
    mov  bl,[edi]
    mov  cl,[esi]
    and  ebx,$FF
    and  ecx,$FF
    sub  ebx,ecx
    imul ebx,[ebp+8]
    shr  ebx,8
    add  ebx,ecx
    mov  [eax],bl
    inc  eax
    inc  edi
    inc  esi
    cmp  eax,[ebp+12]
  jne @bytes

  @exit:
  pop esi
  pop edi
  pop ebx

  db $0F,$77 // emms
end;
{$ENDIF}


procedure xLine_AlphaSameBpp(Dst, Src1, Src2 : PLine32; Size, Alpha : Integer);
Var x, ia : Integer;
begin
ia := 255 - Alpha;
For x:=0 to (Size shr 2)-1 do begin // sort of loop unrolling...
  Dst[0].b := (Src1[0].b * Alpha + Src2[0].b * ia) shr 8;
  Dst[0].g := (Src1[0].g * Alpha + Src2[0].g * ia) shr 8;
  Dst[0].r := (Src1[0].r * Alpha + Src2[0].r * ia) shr 8;
  Dst[0].a := (Src1[0].a * Alpha + Src2[0].a * ia) shr 8;
  Inc(PByte(Dst), 4); Inc(PByte(Src1), 4); Inc(PByte(Src2), 4);
end;
For x:=0 to (Size and 3)-1 do begin
  Dst[0].b := (Src1[0].b * Alpha + Src2[0].b * ia) shr 8;
  Inc(PByte(Dst)); Inc(PByte(Src1)); Inc(PByte(Src2));
end;
end;

procedure AlphaBlend(Dst,Src1,Src2:TFastDIB;Alpha:Integer);
begin
{$IFDEF CPUX86}
If (Src1.Bpp = 16) and (Src2.Bpp = 16) and (Dst.Bpp = 16) then
  xAlphaBlend16(Dst, Src1, Src2, Alpha)
else
  xAnyBlendP(Dst, Src1, Src2, xBlendMem8, xBlendMem8MMX, Alpha, [8, 24, 32]);
{$ELSE}
xAnyBlendP(Dst, Src1, Src2, xLine_AlphaSameBpp, xLine_AlphaSameBpp, Alpha, [8, 24, 32]);
  // for better performance use DrawAlpha, it has x64 MMX version
{$ENDIF}
end;


// ******************************** Draw-procs *********************************

{$IFDEF CPUX86}
// code based on SpriteUtils-2
// using pshufw (integer SSE) command for alpha replication
// if pand command is disabled, Dst.Alpha is not cleared
// (calculated as: Dst.alpha = Src.Alpha * Src.Alpha + (255 - Src.Alpha) * Dst.Alpha )
procedure xLine_DrawAlpha32_SSE(Src, Dst : Pointer; Count : Integer);
Const
  Mask : Int64 = $000000FF00FF00FF;
asm
  push esi
  mov esi, eax
  lea eax, [Mask]
  db $0F,$6F,$28           /// movq mm5, [eax]  // mm5 - $0000.00FF|00FF.00FF
  db $0F,$EF,$FF           /// pxor      mm7, mm7    // mm7 = 0
@inner_loop:

  mov       eax, [esi]
  test      eax, $FF000000
  jz        @noblend

  db $0F,$6E,$06           /// movd      mm0, [esi]
  db $0F,$6E,$0A           /// movd      mm1, [edx]
  db $0F,$60,$C7           /// punpcklbw mm0, mm7    // mm0 - src
  db $0F,$60,$CF           /// punpcklbw mm1, mm7    // mm1 - dst
  db $0F,$70,$F0,$FF       /// pshufw mm6, mm0, 255  // mm6 - src alpha
  //  db $0F,$DB,$F5           /// pand mm6, mm5
    // clear alpha component of mm6 - can be skipped if not needed

//  add       esi, 4

  db $0F,$D5,$C6           /// pmullw    mm0, mm6    // получить произведение источника в mm0
  db $0F,$EF,$F5           /// pxor      mm6, mm5    // получить обратную прозрачность в mm6
  db $0F,$D5,$CE           /// pmullw    mm1, mm6    // получить произведение источника в mm0

  db $0F,$FD,$C1           /// paddw     mm0, mm1    // сложить
  db $0F,$71,$D0,$08       /// psrlw     mm0, 8      // переместить в младшую сторону для упаковки
  db $0F,$67,$C7           /// packuswb  mm0, mm7    // упаковать перед записью на место
  db $0F,$7E,$02           /// movd      [edx], mm0  // записать результат
@noblend:
  add       esi, 4
  add       edx, 4
  dec       ecx
  jnz       @inner_loop
  db $0F,$77               /// emms
  pop esi
end;

// same combining per-pixel and constant Alpha
procedure xLine_DrawAlpha32_SSE_2(Src, Dst : Pointer; Count, AlphaC : Integer);
Const Mask : Int64 = $000000FF00FF00FF;
asm
  push esi
  mov esi, eax
  lea eax, [Mask]
  db $0F,$6F,$28           /// movq mm5, [eax]  // mm5 - $0000.00FF|00FF.00FF
  db $0F,$EF,$FF           /// pxor      mm7, mm7    // mm7 = 0
  mov eax, AlphaC
  db $0F,$6E,$C0           /// movd mm0, eax
  db $0F,$70,$E0,$00       /// pshufw mm4, mm0, 0  // mm4 - const alpha
@inner_loop:

  mov       eax, [esi]
  test      eax, $FF000000 // alpha = 0 - can skip blending; this check
  jz        @noblend       // makes proc faster at CoreDuo, but slower at P4 (also depends on data)

  db $0F,$6E,$06           /// movd      mm0, [esi]
  db $0F,$6E,$0A           /// movd      mm1, [edx]

  db $0F,$60,$C7           /// punpcklbw mm0, mm7    // mm0 - src
  db $0F,$60,$CF           /// punpcklbw mm1, mm7    // mm1 - dst
  db $0F,$70,$F0,$FF       /// pshufw mm6, mm0, 255  // mm6 - src alpha
  db $0F,$D5,$F4           /// pmullw mm6, mm4       // alpha = alpha * [const alpha]
  db $0F,$71,$D6,$08       /// psrlw  mm6, 8

  //  db $0F,$DB,$F5           /// pand mm6, mm5
    // clear alpha component of mm6 - can be skipped if not needed

  db $0F,$D5,$C6           /// pmullw    mm0, mm6    // src = src * alpha
  db $0F,$EF,$F5           /// pxor      mm6, mm5    // alpha = 1 - alpha
  db $0F,$D5,$CE           /// pmullw    mm1, mm6    // dst = dst * (1 - alpha)

  db $0F,$FD,$C1           /// paddw     mm0, mm1    // src = src + dst
  db $0F,$71,$D0,$08       /// psrlw     mm0, 8      // div 256
  db $0F,$67,$C7           /// packuswb  mm0, mm7    // packing to bytes
  db $0F,$7E,$02           /// movd      [edx], mm0
@noblend:
  add       esi, 4
  add       edx, 4
  dec       ecx
  jnz       @inner_loop
  db $0F,$77               /// emms
  pop esi
end;
{$ENDIF}

{$IFDEF CPUX64}
// Src = rcx, Dst = rdx, Count = r8, AlphaC = r9
procedure xLine_DrawAlpha32_SSE_2(Src, Dst : Pointer; Count, AlphaC : Integer);
Const Mask : Int64 = $000000FF00FF00FF;
asm
  movq mm5, Mask  // mm5 - $0000.00FF|00FF.00FF
  pxor mm7, mm7    // mm7 = 0
  mov eax, AlphaC
  movd mm0, eax
  pshufw mm4, mm0, 0  // mm4 - const alpha
@inner_loop:
  mov       eax, [rcx]
  test      eax, $FF000000 // alpha = 0 - can skip blending; this check
  jz        @noblend       // makes proc faster at CoreDuo, but slower at P4 (also depends on data)

  movd      mm0, [rcx]
//  movd      mm0, eax
  movd      mm1, [rdx]
  punpcklbw mm0, mm7    // mm0 - src
  punpcklbw mm1, mm7    // mm1 - dst
  pshufw mm6, mm0, 255  // mm6 - src alpha
  pmullw mm6, mm4       // alpha = alpha * [const alpha]
  psrlw  mm6, 8

//  pand mm6, mm5
    // clear alpha component of mm6 - can be skipped if not needed

  pmullw    mm0, mm6    // src = src * alpha
  pxor      mm6, mm5    // alpha = 1 - alpha
  pmullw    mm1, mm6    // dst = dst * (1 - alpha)

  paddw     mm0, mm1    // src = src + dst
  psrlw     mm0, 8      // div 256
  packuswb  mm0, mm7    // packing to bytes
  movd      [rdx], mm0
@noblend:
  add       rcx, 4
  add       rdx, 4
  dec       r8
  jnz       @inner_loop
  emms
end;
{$ENDIF}


procedure xLine_DrawAlpha8(Src : PByte; Dst : PFColorA; SrcPal : PFColorTable;
                           Count, dStep : Integer; Alpha : Integer;
                           UsePalAlpha : Boolean = False);
Var x, a, ia : Integer;
    sc : PFColorA;
begin
If (UsePalAlpha) then
  For x:=0 to Count-1 do begin
    sc := @SrcPal[Src^];
    a := sc.a * Alpha; ia := (65535 - a);
    Dst.b := (sc.b * a + Dst.b * ia) shr 16;
    Dst.g := (sc.g * a + Dst.g * ia) shr 16;
    Dst.r := (sc.r * a + Dst.r * ia) shr 16;
    Inc(Src); Inc(PByte(Dst), dStep);
  end
else begin
  a := Alpha; ia := 255 - Alpha;
  For x:=0 to Count-1 do begin
    sc := @SrcPal[Src^];
    Dst.b := (sc.b * a + Dst.b * ia) shr 8;
    Dst.g := (sc.g * a + Dst.g * ia) shr 8;
    Dst.r := (sc.r * a + Dst.r * ia) shr 8;
    Inc(Src); Inc(PByte(Dst), dStep);
  end
end;
end;

// Drawing Src to Dst with per-pixel alpha
// Src is 8 (alpha from palette), 24 (separate 8 bpp Alpha), 32 bpp
// Dst is 24, 32 bpp
// using constant alpha at same proc makes it 10-12% slower
// MMX/SSE version is 1.5 times faster
function DrawAlpha(Src, Dst : TFastDIB; AlphaConst : Integer = 255;
                   dx : Integer = 0; dy : Integer = 0;
                   Alpha : TFastDIB = nil): Boolean;
var
  x, y, a, ia, sStep, dStep: Integer;
  sc, dc: PFColorA;
  pa : PByte;
  UseMMX : Boolean;
begin
Result := ( (Src.Bpp in [8, 32]) or ((Src.Bpp = 24) and (Alpha <> nil)) ) and
          (Dst.Bpp >= 24);

If (not Result) or (AlphaConst = 0) then Exit;

sStep := Src.Bpp shr 3; dStep := Dst.Bpp shr 3;
If (AlphaConst > 255) then AlphaConst := 255;

UseMMX := (sStep = 4) and (dStep = 4) and (Alpha = nil) and
          ([cfSSE, cfMMX2] * CpuInfo.Features <> []);

for y:=0 to Src.AbsHeight-1 do begin
  sc := Src.Scanlines[y];
  dc := Dst.Scanlines[y + dy]; Inc(PByte(dc), dx * dStep);

  If UseMMX then begin
  {$IFDEF CPUX86}
     If (AlphaConst = 255) then xLine_DrawAlpha32_SSE(sc, dc, Src.Width) else
  {$ENDIF}
       xLine_DrawAlpha32_SSE_2(sc, dc, Src.Width, AlphaConst);
  end else
    If (Alpha <> nil) then begin
      pa := Alpha.Scanlines[y];
      For x:=0 to Src.Width-1 do begin
        a := pa^ * AlphaConst;
        If (a <> 0) then begin
          ia := (65535 - a);
          dc.b := (sc.b * a + dc.b * ia) shr 16;
          dc.g := (sc.g * a + dc.g * ia) shr 16;
          dc.r := (sc.r * a + dc.r * ia) shr 16;
        end;
        Inc(PByte(sc), sStep); Inc(PByte(dc), dStep); Inc(pa);
      end;
    end else
      If (sStep = 1) then
        xLine_DrawAlpha8(PByte(sc), dc, Src.Colors, Src.Width, dStep, AlphaConst, True)
      else
        For x:=0 to Src.Width-1 do begin
          a := sc.a * (AlphaConst);
          // checking alpha = 0 for typical data is faster at CoreDuo,
          // but can be slower at P4 for bad data
          If (a <> 0) then
          begin
            ia := (65535 - a);
            dc.b := (sc.b * a + dc.b * ia) shr 16;
            dc.g := (sc.g * a + dc.g * ia) shr 16;
            dc.r := (sc.r * a + dc.r * ia) shr 16;
          end;
          Inc(sc); Inc(PByte(dc), dStep);
        end;
end;
end;



{$IFDEF CPUX86}
// Additive blending with MMX
// Src - 8 bpp, Dst - 32 bpp
procedure xLine_Add8_32_MMX(Src, Dst : Pointer; Count : Integer;
                            SrcPal : PFColorTable);
asm
  push esi
  push edi
  push ebx
  mov esi, eax
  mov edi, edx
  mov edx, SrcPal
@loop:
  movzx eax, byte ptr [esi]
  movzx ebx, byte ptr [esi + 1]

  db $0F,$6E,$04,$82       /// movd  mm0, [eax * 4 + edx]
  db $0F,$6E,$0C,$9A       /// movd  mm1, [ebx * 4 + edx]
  db $0F,$73,$F1,$20       /// psllq mm1, 32
  db $0F,$EB,$C1           /// por mm0, mm1
  db $0F,$6F,$17           /// movq  mm2, [edi]
  db $0F,$DC,$C2           /// paddusb mm0, mm2
  db $0F,$7F,$07           /// movq  [edi], mm0

  add       esi, 2
  add       edi, 8
  dec       ecx
  jnz       @loop
  db $0F,$77               /// emms
  pop ebx
  pop edi
  pop esi
end;


procedure xLine_AddK32_MMX(Src, Dst : Pointer; Count, K : Integer);
asm
  push esi
  mov esi, eax
  // запихиваем коэфф. во все word'ы MMX-регистра
  // несколько корявый вариант на "чистом" MMX
  imul eax, K, $10001
  db $0F,$6E,$E8           /// movd mm5, eax
  db $0F,$6F,$E5           /// movq mm4, mm5
  db $0F,$73,$F4,$20       /// psllq mm4, 32
  db $0F,$EB,$EC           /// por mm5, mm4
  // или более изящный на enhanced MMX (P3):
//  pshufw mm5, [ebp + 8], 0

  db $0F,$EF,$FF           /// pxor mm7, mm7    // mm7 = 0
  db $0F,$74,$F6           /// pcmpeqb mm6, mm6 // mm6 = $FFFFFFFF.FFFFFFFF
@inner_loop:
  db $0F,$6E,$06           /// movd mm0, [esi]
  db $0F,$6E,$0A           /// movd mm1, [edx]
  db $0F,$60,$C7           /// punpcklbw mm0, mm7
  db $0F,$6F,$D5           /// movq mm2, mm5    // copy Alpha
  db $0F,$E5,$D0           /// pmulhw mm2, mm0  // HiWord(Res)
  db $0F,$D5,$C5           /// pmullw mm0, mm5  // LoWord(Res)
  db $0F,$75,$D7           /// pcmpeqw mm2, mm7 // HiWord = 0?
  db $0F,$DB,$C2           /// pand mm0, mm2    // да - используем LoWord
  db $0F,$EF,$D6           /// pxor mm2, mm6    // иначе инверсию маски
  db $0F,$EB,$C2           /// por mm0, mm2     // (= $FFFFFFFF.FFFFFFFF)

  db $0F,$71,$D0,$08       /// psrlw mm0, 8      // сокращаем до байтов
  db $0F,$67,$C7           /// packuswb mm0, mm7 // пакуем
  db $0F,$DC,$C1           /// paddusb mm0, mm1  // складываем
  db $0F,$7E,$02           /// movd [edx], mm0
  add esi, 4
  add edx, 4
  dec ecx
  jnz @inner_loop
  db $0F,$77               /// emms
  pop esi
end;

// from SpriteUtils-2
procedure xLine_Add32_MMX(Src, Dst : Pointer; Count : Integer);
asm
  push ecx
  shr ecx, 5
  jz @tail
@inner_loop:
  db $0F,$6F,$00           /// movq    mm0, [eax]
  db $0F,$6F,$48,$08       /// movq    mm1, [eax+8]
  db $0F,$6F,$50,$10       /// movq    mm2, [eax+16]
  db $0F,$6F,$58,$18       /// movq    mm3, [eax+24]
  db $0F,$6F,$22           /// movq    mm4, [edx]
  db $0F,$6F,$6A,$08       /// movq    mm5, [edx+8]
  db $0F,$6F,$72,$10       /// movq    mm6, [edx+16]
  db $0F,$6F,$7A,$18       /// movq    mm7, [edx+24]
  db $0F,$DC,$E0           /// paddusb mm4, mm0
  db $0F,$DC,$E9           /// paddusb mm5, mm1
  db $0F,$DC,$F2           /// paddusb mm6, mm2
  db $0F,$DC,$FB           /// paddusb mm7, mm3
  db $0F,$7F,$22           /// movq    [edx], mm4
  db $0F,$7F,$6A,$08       /// movq    [edx+8], mm5
  db $0F,$7F,$72,$10       /// movq    [edx+16], mm6
  db $0F,$7F,$7A,$18       /// movq    [edx+24], mm7
  add     eax, 32
  add     edx, 32
  dec     ecx
  jnz     @inner_loop

//  db $0F,$77               /// emms
@tail:
  pop ecx
  and ecx, 31
  jz @exit

  push esi
  push ebx
  mov esi, eax
@bytes:
  mov al, [esi]
  add [edx], al
  setnc bl
  dec bl
  or [edx], bl
  inc esi
  inc edx
  dec ecx
  jnz @bytes

  pop ebx
  pop esi
@exit:
end;
{$ENDIF}

procedure xLine_Add8_32(Src : PByte; Dst : PFColorA; SrcPal : PFColorTable;
                        Count, dStep : Integer);
Var x, i : Integer;
    sc : PFColorA;
begin
For x:=0 to Count-1 do begin
  sc := @SrcPal[Src^];
  i := sc.b + Dst.b; If i > 255 then Dst.b := 255 else Dst.b := i;
  i := sc.g + Dst.g; If i > 255 then Dst.g := 255 else Dst.g := i;
  i := sc.r + Dst.r; If i > 255 then Dst.r := 255 else Dst.r := i;
  Inc(Src); Inc(PByte(Dst), dStep);
end;
end;


procedure xLine_AddK32(sc, dc : PFColorA; Count, K, sStep, dStep : Integer);
Var x, i : Integer;
begin
For x:=0 to Count-1 do begin
  i := sc.b * K shr 8 + dc.b; If i > 255 then dc.b := 255 else dc.b := i;
  i := sc.g * K shr 8 + dc.g; If i > 255 then dc.g := 255 else dc.g := i;
  i := sc.r * K shr 8 + dc.r; If i > 255 then dc.r := 255 else dc.r := i;
  Inc(PByte(sc), sStep); Inc(PByte(dc), dStep);
end;
end;

procedure xLine_Add24_32(sc, dc : PFColorA; Count, sStep, dStep : Integer);
Var x, i : Integer;
begin
For x:=0 to Count-1 do begin
  i := sc.b + dc.b; If i > 255 then dc.b := 255 else dc.b := i;
  i := sc.g + dc.g; If i > 255 then dc.g := 255 else dc.g := i;
  i := sc.r + dc.r; If i > 255 then dc.r := 255 else dc.r := i;
  Inc(PByte(sc), sStep); Inc(PByte(dc), dStep);
end;
end;

// Additive blending with optional mult. by K
// Dst = Src * K + Dst [K <> 256]
// Dst = Src + Dst [K = 256]
// (8 || 24 || 32) -> (24 || 32)
// also 8 -> 8 without scaling
function DrawAdd(Src, Dst : TFastDIB; K : Integer = 256;
                 dx : Integer = 0; dy : Integer = 0): Boolean;
var
  x, y, w, w2, gap: Integer;
  sc, dc: PFColorA;
  UseMMX : Boolean;
begin
  Result := ( Src.Bpp in [8, 24, 32] ) and
            ( (Dst.Bpp >= 24) or (Dst.Bpp = Src.Bpp) );
  If (not Result) then Exit;

  UseMMX := False;
  {.$IFDEF CPUX86}
  UseMMX := (cfMMX in CPUInfo.Features);
  If (UseMMX) and (Dst.Bpp = Src.Bpp) and (K = 256) then
    for y:=0 to Src.AbsHeight-1 do begin
      dc := Dst.Scanlines[y + dy]; Inc(PByte(dc), dx * Dst.BytesPP);
      xAddMem8MMX(PLine8(dc), Src.Scanlines[y], PLine8(dc), Src.Width * Src.BytesPP);
//      xLine_Add32_MMX(Src.Scanlines[y], dc, Src.Width * Src.BytesPP)
    end
  else
  {.$ENDIF}
  begin
    If (Src.BytesPP = 1) then begin
      w2 := Src.Width shr 1; gap := Src.Width - w2 * 2;
    end;

    for y:=0 to Src.AbsHeight-1 do begin
      sc := Src.Scanlines[y];
      dc := Dst.Scanlines[y + dy]; Inc(PByte(dc), dx * Dst.BytesPP);
      // 8 -> 24, 32
      If (Src.BytesPP = 1) then begin
    {$IFDEF CPUX86}
        If (UseMMX) and (Dst.BytesPP = 4) then begin
          xLine_Add8_32_MMX(sc, dc, w2, Src.Colors);
          Inc(PByte(sc), w2*2); Inc(PByte(dc), w2*2*Dst.BytesPP);
          w := gap;
        end else
    {$ENDIF}
          w := Src.Width;
        If (w <> 0) then
          xLine_Add8_32(PByte(sc), dc, Src.Colors, w, Dst.BytesPP);
      end
      else // 24,32 -> 24,32
        If (K <> 256) then
    {$IFDEF CPUX86}
          If (UseMMX) and (Src.BytesPP = 4) and (Dst.BytesPP = 4) then
            xLine_AddK32_MMX(sc, dc, Src.Width, K)
          else
    {$ENDIF}
            xLine_AddK32(sc, dc, Src.Width, K, Src.BytesPP, Dst.BytesPP)
        else
          xLine_Add24_32(sc, dc, Src.Width, Src.BytesPP, Dst.BytesPP);
    end;
  end;
  If UseMMX then EMMS;
end;


// code mainly from SpriteUtils-2
procedure xLine_Trans8_MMX(Src, Dst : PByte; Count : Integer;
                               TransColor : PByte);
// EAX = Src, EDX = Dst, ECX = Count
asm
{$IFDEF CPUX86}
  push eax
  mov eax, TransColor
  db $0F,$6F,$10           /// movq mm2, [eax]
  pop eax
  db $0F,$74,$E4           /// pcmpeqb  mm4, mm4
@quads:
  db $0F,$6F,$18           /// movq mm3, [eax]
  db $0F,$6F,$00           /// movq mm0, [eax]
  db $0F,$74,$DA           /// pcmpeqb  mm3, mm2
  db $0F,$EF,$DC           /// pxor     mm3, mm4
  db $0F,$DB,$C3           /// pand     mm0, mm3
  db $0F,$DF,$1A           /// pandn    mm3, [edx]
  db $0F,$EB,$C3           /// por      mm0, mm3
  db $0F,$7F,$02           /// movq     [edx], mm0
  add eax, 8
  add edx, 8
  dec ecx
jnz @quads
{$ENDIF}
end;

procedure xLine_Trans8(Src, Dst : PByte; Count : Integer; Trans : Byte);
var x: Integer;
begin
for x := 0 to Count - 1 do begin
  If (Src^ <> Trans) then Dst^ := Src^;
  Inc(Src); Inc(Dst);
end;
end;

procedure xLine_Trans16(Src, Dst : PWord; Count : Integer; Trans : Word);
var x: Integer;
begin
for x := 0 to Count - 1 do begin
  If (Src^ <> Trans) then Dst^ := Src^;
  Inc(Src); Inc(Dst);
end;
end;

procedure xLine_Trans24(Src, Dst : PFColor; Count : Integer; Trans : TFColorA);
var x : Integer;
    t24 : DWord;
begin
t24 := DWord(Trans) and $FFFFFF00;
for x := 0 to Count - 2 do begin
  If (PDWord(Src)^ and $FFFFFF00) <> t24 then Dst^ := Src^;
  Inc(Src); Inc(Dst);
end;
If (Src.r <> Trans.r) or (Src.g <> Trans.g) or (Src.b <> Trans.b) then
  Dst^ := Src^;
end;

{$IFDEF CPUX86}
procedure xLine_Trans32(Src, Dst : PDWord; Count : Integer; Trans : DWord);
asm
  push esi
  push edi
  mov esi, eax
  mov edi, edx
  mov edx, Trans
@inner_loop:
  mov eax, [esi]
  cmp eax, edx
  jz @skip
  mov [edi], eax
@skip:
  add esi, 4
  add edi, 4
  dec ecx
jnz @inner_loop
  pop edi
  pop esi
end;
{$ELSE}
procedure xLine_Trans32(Src, Dst : PDWord; Count : Integer; Trans : DWord);
var x: Integer;
begin
for x := 0 to Count - 1 do begin
  If (Src^ <> Trans) then Dst^ := Src^;
  Inc(Src); Inc(Dst);
end;
end;
{$ENDIF}


// Drawing Src to Dst with transparent color
// TransColor for 8 bpp - palette index,
// for other modes - $BBGGRRAA (TFColorA)
// GDI TransparentBlt (TransBlit) function can also be used for this
procedure DrawTrans(Src, Dst : TFastDIB; dx : Integer = 0; dy : Integer = 0;
                    TransColor : DWord = 0);
Var TransArr : array[ 0..7 ] of Byte;
    y, w, gap, c16 : Integer;
    dp : PByte;
begin
// Note: seems surprising a bit, but calling procedure
// for each scanline is FASTER than writing double loop here...
// due to better variable-to-register fitting, I suppose.
Case Src.Bpp of
  8 : begin
    w := 0; gap := Src.Width;
    // MMX is nearly 2 times faster here, more with x alignment
    If (cfMMX in CPUInfo.Features) then begin
      For y := 0 to 7 do TransArr[y] := TransColor;
      w := Src.Width shr 3; gap := Src.Width mod 8;
    end;

    For y := 0 to Src.AbsHeight-1 do begin
      dp := @Dst.Pixels8[y + dy, dx];
      If (w <> 0) then
        xLine_Trans8_MMX(Src.Scanlines[y], dp, w, @TransArr );
      If (gap <> 0) then begin
        Inc(dp, w*8);
        xLine_Trans8(@Src.Pixels16[y, w*8], dp, gap, TransColor);
      end;
    end;
    If (w <> 0) then EMMS;
  end;

  16 : begin
    // can also be done with MMX, but it has very few effect here
    c16 := (TransColor shr Src.RShr) or
           (TransColor shr (8 + Src.GShr)) or
           (TransColor shr (16 + Src.BShr));
     For y := 0 to Src.AbsHeight-1 do // 8 ms
       xLine_Trans16(Src.Scanlines[y], @Dst.Pixels16[y + dy, dx],
                     Src.Width, c16);
  end;

  24 : For y := 0 to Src.AbsHeight-1 do // 15 ms
         xLine_Trans24(Src.Scanlines[y], @Dst.Pixels24[y + dy, dx],
                       Src.Width, TFColorA(TransColor));

  32 : For y := 0 to Src.AbsHeight-1 do // 19/12 ms
         xLine_Trans32(Src.Scanlines[y], @Dst.Pixels32[y + dy, dx],
                       Src.Width, TransColor);

end;
end;

end.
