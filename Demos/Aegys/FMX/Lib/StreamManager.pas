unit StreamManager;

{
 Projeto Aegys.

  Criado por Gilberto Rocha da Silva em 05/04/2017 baseado no projeto Allakore, tem por objetivo promover acesso remoto e outros
 de forma gratuita a todos que necessitarem, hoje mantido por uma bela comunidade listando aqui nossos colaboradores de grande estima.

  Gilberto Rocha da Silva(XyberX) (Creator of Aegys Project/Main Desenveloper/Admin).
  Wendel Rodrigues Fassarella(wendelfassarella) (Creator of Aegys FMX/CORE Desenveloper).
  Rai Duarte Jales(Raí Duarte) (Aegys Server Desenveloper).
  Roniery Santos Cardoso (Aegys Desenveloper).
  Alexandre Carlos Silva Abade (Aegys Desenveloper).
}

interface

uses
  System.Classes, FMX.Graphics, Vcl.Graphics, FMX.Forms, Winapi.Windows,
  FMX.Objects;

procedure GetScreenToMemoryStream(DrawCur: Boolean; TargetMemoryStream: TMemoryStream);

procedure CompareStream(MyFirstStream, MySecondStream, MyCompareStream: TMemoryStream);

procedure ResumeStream(MyFirstStream      : TMemoryStream;
                       Var MySecondStream,
                       MyCompareStream    : TMemoryStream);

procedure ResizeBmp(AImage: TImage; AStream: TMemoryStream; AWidth, AHeight: Single);

Var
 pdst     : Pointer;
 ASMSize,
 muASM    : Integer;

implementation

uses
  System.SysUtils;

procedure ResizeBmp(AImage: TImage; AStream: TMemoryStream; AWidth, AHeight: Single);
var
  bmpSrc, bmpDest: Vcl.Graphics.TBitmap;
  msImage: TMemoryStream;
begin
  try
    msImage := TMemoryStream.Create;
    msImage.Clear;
    bmpSrc := Vcl.Graphics.TBitmap.Create;
    bmpDest := Vcl.Graphics.TBitmap.Create;
    AStream.Position := 0;
    bmpSrc.LoadFromStream(AStream);
    bmpDest.Width := Trunc(AWidth);
    bmpDest.Height := Trunc(AHeight);
    SetStretchBltMode(bmpDest.Canvas.Handle, HALFTONE);
    StretchBlt(
      bmpDest.Canvas.Handle,
      0,
      0,
      bmpDest.Width,
      bmpDest.Height,
      bmpSrc.Canvas.Handle,
      0,
      0,
      bmpSrc.Width,
      bmpSrc.Height,
      SRCCOPY);
    bmpDest.SaveToStream(msImage);
    msImage.Position := 0;
    AImage.Bitmap.LoadFromStream(msImage);
  finally
    FreeAndNil(msImage);
    bmpSrc.Free;
    bmpDest.Free;
  end;
end;

procedure GetScreenToMemoryStream(DrawCur: Boolean; TargetMemoryStream: TMemoryStream);
const
  CAPTUREBLT = $40000000;
var
  Mybmp: Vcl.Graphics.TBitmap;
  Cursorx, Cursory: Integer;
  dc: hdc;
  R: TRect;
  DrawPos: TPoint;
  MyCursor: TIcon;
  hld: hwnd;
  Threadld: dword;
  mp: TPoint;
  pIconInfo: TIconInfo;
begin
  Mybmp := Vcl.Graphics.TBitmap.Create;

  dc := GetWindowDC(0);
  try
    R := Rect(0, 0, GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN));
    Mybmp.Width := R.Right;
    Mybmp.Height := R.Bottom;
    BitBlt(Mybmp.Canvas.Handle, 0, 0, Mybmp.Width, Mybmp.Height, dc, 0, 0, SRCCOPY or CAPTUREBLT);
  finally
    releaseDC(0, dc);
  end;

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
  Mybmp.PixelFormat := pf15bit;
  TargetMemoryStream.Clear;
  Mybmp.SaveToStream(TargetMemoryStream);
  Mybmp.Free;
end;

// Compare Streams and separate when the Bitmap Pixels are equal.
procedure CompareStream(MyFirstStream, MySecondStream, MyCompareStream: TMemoryStream);
var
  I: Integer;
  P1: ^AnsiChar;
  P2: ^AnsiChar;
  P3: ^AnsiChar;
begin
  // Check if the resolution has been changed
  if MyFirstStream.Size <> MySecondStream.Size then
  begin
    MyFirstStream.LoadFromStream(MySecondStream);
    MyCompareStream.LoadFromStream(MySecondStream);
    Exit;
  end;

  MyCompareStream.Clear;

  P1 := MyFirstStream.Memory;
  P2 := MySecondStream.Memory;
  MyCompareStream.SetSize(MyFirstStream.Size);
  P3 := MyCompareStream.Memory;

  for I := 0 to MyFirstStream.Size - 1 do
  begin

    if P1^ = P2^ then
      P3^ := '0'
    else
      P3^ := P2^;

    Inc(P1);
    Inc(P2);
    Inc(P3);

  end;

  MyFirstStream.LoadFromStream(MySecondStream);
end;

Function ResumeStreamASM(Const S, d: Pointer; Var c: Pointer): Integer;
  Assembler;
Var
  src: ^Char;
  dest: ^Char;
  n1, n2: Cardinal;
Begin
  Asm
    mov muASM, 0
    mov pdst, ECX              // Move resolutado pra PDST
    mov src, EAX               // Move S pra src
    mov dest, EDX              // Move D pra dest
    call System.@LStrLen       // Tamanho de string S
    mov n1, EAX                // Move tamanho do S para n1
    mov EAX, dest              // Move dest para EAX
    call System.@LStrLen       // Tamanho do dst/D
    mov n2, EAX                // Move Tamanho D para n2
    mov EDX, EAX               // Move tamanho D para EDX segundo parametro setlenght
    mov EAX, pdst              // Move Result/pdst para EAX primeiro parametro strlenght
    call System.@LStrSetLength // Seta parametro pdst para tamanho n2
    mov ECX, ASMSize           // Mov n2 para ECX para controlar loopings
    test ECX, ECX              // Testa ECX
    jz @@end                   // Se EXX = 0 Termina
    push ESI                   // Guarda ESI na pilha
    push EDI
    mov EAX, pdst              // EAX := pdst; //Endereço da string de resultado
    mov ESI, src               // ESI := src; //String de origem
    mov EDI, dest
    mov EDX, [EAX]             // EDX := pdst^; //String de resultado
  @@cycle:
    mov AL, [EDI]             // Move um caracter do primeiro stream para AL
    cmp AL, '0'               // Copara se o caracter é 0 no segundo stream
    jne @@diferente           // Se for Diferente pula para igual
    mov AL, [ESI]             // Se defente copia Caracter do Segund stream para AL
    mov [EDX], AL             // Coloca caracter no terceiro stream
    mov muASM, 1
    cmp AL, AL                // Apenas para gerra um Je
    je @@incremento           // Incrementa caracter
  @@diferente:
    mov AL, [EDI]             // Se for <> Coloca '0' em AL
    mov [EDX], AL             // Move o caracter correto para terceiro Stream
  @@incremento:
    inc ESI
    inc EDI
    inc EDX
    dec ECX
    cmp ECX, 0
    ja @@cycle
    pop EDI
    pop ESI                   // Recupera ESI na pilha
  @@end:
  End;
  Result := muASM;
End;

Procedure ResumeStream(MyFirstStream      : TMemoryStream;
                       Var MySecondStream,
                       MyCompareStream    : TMemoryStream);
Var
 P1,
 P2,
 P3  : Pointer;
Begin
 Try
  If MyFirstStream.Size <> MyCompareStream.Size Then
   MyFirstStream.SetSize(MyCompareStream.Size);
  MyFirstStream.Position := 0;
  If MySecondStream.Size <> MyCompareStream.Size Then
   MySecondStream.SetSize(MyCompareStream.Size);
  MySecondStream.Position := 0;
  P1 := MyFirstStream.Memory;
  P2 := MyCompareStream.Memory;
  P3 := MySecondStream.Memory;
  muASM := 0;
  ASMSize := MyCompareStream.Size;
  If ResumeStreamASM(P1, P2, P3) <> 0 Then
   Begin
    MySecondStream.Clear;
    MySecondStream.Write(P3^, MyCompareStream.Size);
    MySecondStream.Position := 0;
    MyFirstStream.Clear;
    MyFirstStream.CopyFrom(MySecondStream, 0);
    MyFirstStream.Position := 0;
   End;
 Finally
 End;
 Asm
  mov EDX, 0                 // Move tamanho D para EDX segundo parametro setlenght
  mov EAX, pdst              // Move Result/pdst para EAX primeiro para metro strlenght
  call System.@LStrSetLength // Seta parametro pdst para tamanho n2
 End;
End;

//// Modifies Streams to set the Pixels of Bitmap
//procedure ResumeStream(MyFirstStream, MySecondStream, MyCompareStream: TMemoryStream);
//var
//  I: Integer;
//  P1: ^AnsiChar;
//  P2: ^AnsiChar;
//  P3: ^AnsiChar;
//begin
//
//  // Check if the resolution has been changed
//  if MyFirstStream.Size <> MyCompareStream.Size then
//  begin
//    MyFirstStream.LoadFromStream(MyCompareStream);
//    MySecondStream.LoadFromStream(MyCompareStream);
//    Exit;
//  end;
//
//  P1 := MyFirstStream.Memory;
//  MySecondStream.SetSize(MyFirstStream.Size);
//  P2 := MySecondStream.Memory;
//  P3 := MyCompareStream.Memory;
//
//  for I := 0 to MyFirstStream.Size - 1 do
//  begin
//
//    if P3^ = '0' then
//      P2^ := P1^
//    else
//      P2^ := P3^;
//
//    Inc(P1);
//    Inc(P2);
//    Inc(P3);
//
//  end;
//
//  MyFirstStream.LoadFromStream(MySecondStream);
//  MySecondStream.Position := 0;
//end;

end.

