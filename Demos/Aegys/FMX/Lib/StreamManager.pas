unit StreamManager;

{
 Project Aegys Remote Support.

   Created by Gilberto Rocha da Silva in 04/05/2017 based on project Allakore, has by objective to promote remote access
 and other resources freely to all those who need it, today maintained by a beautiful community. Listing below our
 higly esteemed collaborators:

  Gilberto Rocha da Silva (XyberX) (Creator of Aegys Project/Main Developer/Admin)
  Wendel Rodrigues Fassarella (wendelfassarella) (Creator of Aegys FMX/CORE Developer)
  Rai Duarte Jales (Raí Duarte) (Aegys Server Developer)
  Roniery Santos Cardoso (Aegys Developer)
  Alexandre Carlos Silva Abade (Aegys Developer)
  Mobius One (Aegys Developer)
}

interface

uses
  System.Classes,
  System.Types,
  FMX.Forms,
  FMX.Objects

  {$IF DEFINED (ANDROID) || (IOS)}
  ,FMX.Types
  ,FMX.Graphics
  ,FMX.Platform
  {$ENDIF}
  {$IF DEFINED (MSWINDOWS)}
  ,Vcl.Graphics
  ,Winapi.Windows
  ,Vcl.Forms
  {$ENDIF}
  ;

Const
  TNeutroColor = 255;

Type
  TRGBTriple = Packed Record
    B: Byte;
    G: Byte;
    R: Byte;
  End;

Type
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = Array [0 .. 4095] of TRGBTriple;

{$IF DEFINED (ANDROID) || (IOS)}
procedure GetScreenToMemoryStream(DrawCur            : Boolean;
                                  TargetMemoryStream : TMemoryStream;
                                  PixelFormat        : TPixelFormat = TPixelFormat.None;
                                  Monitor            : String       = '0');
{$ENDIF}
{$IF DEFINED (MSWINDOWS)}
procedure GetScreenToMemoryStream(DrawCur            : Boolean;
                                  TargetMemoryStream : TMemoryStream;
                                  PixelFormat        : TPixelFormat = pfDevice;
                                  Monitor            : String       = '0');
{$ENDIF}


Function CompareStream(Var MyFirstStream,
                        MySecondStream,
                        MyCompareStream    : TMemoryStream) : Boolean;

procedure ResumeStream(MyFirstStream       : TMemoryStream;
                       Var MySecondStream,
                       MyCompareStream     : TMemoryStream);

procedure ResizeBmp(AImage: TImage; AStream: TMemoryStream; AWidth, AHeight: Single);

Var
 pdst     : Pointer;
 ASMSize,
 muASM    : Integer;
 cpdst     : Pointer;
 cASMSize,
 cmuASM    : Integer;

implementation

uses
  System.SysUtils;

procedure ResizeBmp(AImage: TImage; AStream: TMemoryStream; AWidth, AHeight: Single);
var
  {$IF DEFINED (ANDROID) || (IOS)}
  bmpSrc, bmpDest: FMX.Graphics.TBitmap;
  {$ENDIF}
  {$IF DEFINED (MSWINDOWS)}
  bmpSrc, bmpDest: Vcl.Graphics.TBitmap;
  {$ENDIF}

  msImage: TMemoryStream;
begin
  try
    msImage := TMemoryStream.Create;
    msImage.Clear;
    {$IF DEFINED (ANDROID) || (IOS)}
    bmpSrc := FMX.Graphics.TBitmap.Create;
    bmpDest := FMX.Graphics.TBitmap.Create;
    {$ENDIF}
    {$IF DEFINED (MSWINDOWS)}
    bmpSrc := Vcl.Graphics.TBitmap.Create;
    bmpDest := Vcl.Graphics.TBitmap.Create;
    {$ENDIF}

    AStream.Position := 0;
    bmpSrc.LoadFromStream(AStream);
    bmpDest.Width := Trunc(AWidth);
    bmpDest.Height := Trunc(AHeight);

    {$IF DEFINED (ANDROID) || (IOS)}
     bmpDest  :=  bmpSrc;
     bmpDest.Resize(AWidth, AHeight);
    {$ENDIF}
    {$IF DEFINED (MSWINDOWS)}
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
    {$ENDIF}

    bmpDest.SaveToStream(msImage);
    msImage.Position := 0;
    AImage.Bitmap.LoadFromStream(msImage);
  finally
    FreeAndNil(msImage);
    bmpSrc.Free;
    bmpDest.Free;
  end;
end;

Procedure DrawScreenCursor(Var Bmp: Vcl.Graphics.TBitmap; const MonitorID: Integer);
Var
 R          : TRect;
 CursorInfo : TCursorInfo;
 Icon       : TIcon;
 IconInfo   : TIconInfo;
Begin
 R    := Bmp.Canvas.ClipRect;
 Icon := TIcon.Create;
 Try
  CursorInfo.cbSize := SizeOf(CursorInfo);
  If GetCursorInfo(CursorInfo) Then
   If CursorInfo.Flags = CURSOR_SHOWING Then
    Begin
     Icon.Handle:= CopyIcon(CursorInfo.hCursor);
     If GetIconInfo(Icon.Handle, IconInfo) Then
      Begin
       Bmp.Canvas.Draw(CursorInfo.ptScreenPos.x - Integer(IconInfo.xHotspot) - R.Left,
                       CursorInfo.ptScreenPos.y - Integer(IconInfo.yHotspot) - R.Top,
                       Icon);
      End;
    End;
 Finally
  Icon.Free;
 End;
End;

{$IF DEFINED (ANDROID) || (IOS)}
procedure GetScreenToMemoryStream(DrawCur            : Boolean;
                                  TargetMemoryStream : TMemoryStream;
                                  PixelFormat        : TPixelFormat = TPixelFormat.None;
                                  Monitor            : String       = '0');
{$ENDIF}
{$IF DEFINED (MSWINDOWS)}
procedure GetScreenToMemoryStream(DrawCur            : Boolean;
                                  TargetMemoryStream : TMemoryStream;
                                  PixelFormat        : TPixelFormat = pfDevice;
                                  Monitor            : String       = '0');
{$ENDIF}

Const
 CAPTUREBLT = $40000000;
Var
  {$IF DEFINED (ANDROID) || (IOS)}
  Mybmp: FMX.Graphics.TBitmap;
  dc: Cardinal;
  {$ENDIF}
  {$IF DEFINED (MSWINDOWS)}
  Mybmp: Vcl.Graphics.TBitmap;
  dc: hdc;
  vMonitor,
  Left, Top: Integer;
  {$ENDIF}
  DesktopCanvas: TCanvas;
  {$IF DEFINED (ANDROID) || (IOS)}
  Procedure MakeGrey(Bitmap: FMX.Graphics.TBitmap);
  {$ENDIF}
  {$IF DEFINED (MSWINDOWS)}
  Procedure MakeGrey(Bitmap: Vcl.Graphics.TBitmap);
  {$ENDIF}

  Var
   w, h, y, x: Integer;
   sl: PRGBTripleArray;
   grey: Byte;
  Begin
    {$IF DEFINED (ANDROID) || (IOS)}
    Bitmap.PixelFormat := TPixelFormat.RGBA32F;
    {$ENDIF}
    {$IF DEFINED (MSWINDOWS)}
    Bitmap.PixelFormat := pf32bit;
    {$ENDIF}

    w := Bitmap.Width;
    h := Bitmap.Height;
    For y := 0 To h - 1 Do
    Begin
      {$IF DEFINED (ANDROID) || (IOS)}
//       sl := Bitmap.ScanLine[y];
      {$ENDIF}
      {$IF DEFINED (MSWINDOWS)}
      sl := Bitmap.ScanLine[y];
      {$ENDIF}

     For x := 0 To w - 1 Do
     Begin
       With sl[x] Do
       Begin
         grey := (B + G + R) div 3;
         B := grey;
         G := grey;
         R := grey;
       End;
     End;
    End;
  End;
Begin
  {$IF DEFINED (ANDROID) || (IOS)}
  Mybmp := FMX.Graphics.TBitmap.Create;
  dc := FMX.Forms.Screen.Forms[0].Handle;
  {$ENDIF}
  {$IF DEFINED (MSWINDOWS)}
  Mybmp := Vcl.Graphics.TBitmap.Create;
//  dc := GetWindowDC(0);
  {$ENDIF}
  vMonitor := StrToInt(Monitor) +1;
  If (vMonitor > FMX.Forms.Screen.DisplayCount) then
   Exit;
  vMonitor := vMonitor -1;
  Try
    DC:= GetDC(0);
    If (DC = 0) Then
     Exit;
//    If (vMonitor = 0) Then
//     Begin
//      Mybmp.Width := Screen.DesktopWidth;
//      Mybmp.Height := Screen.DesktopHeight;
//      Left := Screen.DesktopLeft;
//      Top := Screen.DesktopTop;
//     End
//    Else
//     Begin
    Mybmp.Width := Screen.Monitors[vMonitor].Width;
    Mybmp.Height := Screen.Monitors[vMonitor].Height;
    Left := Screen.Monitors[vMonitor].Left;
    Top := Screen.Monitors[vMonitor].Top;
//     End;
    DesktopCanvas := TCanvas.Create;
    Try
     DesktopCanvas.Handle := DC;
     {$IF DEFINED (ANDROID) || (IOS)}
 //    R := FMX.Forms.Screen.DesktopRect;
     {$ENDIF}
     {$IF DEFINED (MSWINDOWS)}
 //    R := Rect(0, 0, GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN));
     {$ENDIF}
 //    Mybmp.Width := R.Right;
 //    Mybmp.Height := R.Bottom;
     {$IF DEFINED (ANDROID) || (IOS)}
 //    BitBlt(Mybmp.Canvas.Handle, 0, 0, Mybmp.Width, Mybmp.Height, dc, 0, 0, SRCCOPY or CAPTUREBLT);
     {$ENDIF}
     {$IF DEFINED (MSWINDOWS)}
      BitBlt(Mybmp.Canvas.Handle, 0, 0, Mybmp.Width, Mybmp.Height, DesktopCanvas.Handle, Left, Top, SRCCOPY or CAPTUREBLT);
     {$ENDIF}
    Finally
     FreeAndNil(DesktopCanvas);
    End;
  Finally
    {$IF DEFINED (ANDROID) || (IOS)}
    FMX.Forms.Screen.Forms[0].ReleaseCapture;
    {$ENDIF}
    {$IF DEFINED (MSWINDOWS)}
    ReleaseDC(0, dc);
    {$ENDIF}
  End;
  If DrawCur Then
   DrawScreenCursor(Mybmp, StrToInt(Monitor));
  TargetMemoryStream.Clear;
  {$IF DEFINED (ANDROID) || (IOS)}
  If PixelFormat = TPixelFormat.RGB Then
  {$ENDIF}
  {$IF DEFINED (MSWINDOWS)}
  If PixelFormat = pf4bit Then
  {$ENDIF}
  Begin
    {$IF DEFINED (ANDROID) || (IOS)}
    Mybmp.PixelFormat := TPixelFormat.RGBA16;
    {$ENDIF}
    {$IF DEFINED (MSWINDOWS)}
    Mybmp.PixelFormat := pf16bit;
    {$ENDIF}
    MakeGrey(Mybmp);
  End
  Else
  Mybmp.PixelFormat := PixelFormat;
  Mybmp.SaveToStream(TargetMemoryStream);
  Mybmp.Free;
End;

Function CompareStreamASM(Const s, d: Pointer; Var c: Pointer) : Integer; Assembler;
Var
 src     : ^AnsiChar;
 dest    : ^AnsiChar;
 n1, n2  : Cardinal;
Begin
 Asm
  mov cmuASM, 0
  mov cpdst, ECX             //Move resolutado pra cPDST
  mov src, EAX               //Move S pra src
  mov dest, EDX              //Move D pra dest
  call System.@LStrLen       //Tamanho de string S
  mov n1, EAX                //Move tamanho do S para n1
  mov EAX, dest              //Move dest para EAX
  call System.@LStrLen       //Tamanho do dst/D
  mov n2, EAX                //Move Tamanho D para n2
  mov EDX, EAX               //Move tamanho D para EDX segundo parametro setlenght
  mov EAX, cpdst             //Move Result/pdst para EAX primeiro parametro strlenght
  call System.@LStrSetLength //Seta parametro pdst para tamanho n2
  mov ECX, cASMSize          //Mov n2 para ECX para controlar loopings
  test ECX, ECX              //Testa ECX
  jz @@end                   //Se EXX = 0 Termina
  push ESI                   //Guarda ESI na pilha
  push EDI
  mov EAX, cpdst              //EAX := pdst; //Endereço da string de resultado
  mov ESI, src               //ESI := src; //String de origem
  mov EDI, dest
  mov EDX, [EAX]             //EDX := pdst^; //String de resultado
  @@cycle:
   mov AL, [ESI]             //Move um caracter do primeiro stream para AL
   cmp AL, [EDI]             //Copara caracter com o segundo stream
   je @@igual                //Se for igual pula para igual
   mov AL, [EDI]             //Se defente copia Carcter do Segund stream para AL
   mov [EDX], AL             //Coloca caracter no terceiro stream
   mov cmuASM, 1
   cmp AL, AL                //Apenas para gerra um Je
   je @@incremento           //Incrementa caracter
  @@igual:
   mov AL, '0'               //Se for igual Coloca '0' em AL
   mov [EDX], AL             //Move '0' para terceiro Stream
  @@incremento:
   inc ESI
   inc EDI
   inc EDX
   dec ECX
   cmp ECX, 0
   ja @@cycle
   pop EDI
   pop ESI                   //Recupera ESI na pilha
  @@end:
 End;
 Result := cmuASM;
End;

Function CompareStream(Var MyFirstStream,
                       MySecondStream,
                       MyCompareStream    : TMemoryStream) : Boolean;
Var
 P1, P2, P3 : Pointer;
Begin
 Result := False;
 If MyFirstStream.Size <> MySecondStream.Size Then
  Begin
   MyFirstStream.LoadFromStream(MySecondStream);
   MyCompareStream.LoadFromStream(MySecondStream);
   Exit;
  End;
 MyFirstStream.Position := 0;
 MySecondStream.Position := 0;
 MyCompareStream.Clear;
 P1 := MyFirstStream.Memory;
 P2 := MySecondStream.Memory;
 P3 := MyCompareStream.Memory;
 cmuASM   := 0;
 cASMSize := MySecondStream.Size;
 If CompareStreamASM(P1, P2, P3) > 0 Then
  Begin
   MyCompareStream.Clear;
   MyCompareStream.Write(P3^, MySecondStream.Size);
   MyCompareStream.Position := 0;
   MyFirstStream.Clear;
   MySecondStream.Position  := 0;
   MyFirstStream.CopyFrom(MySecondStream, 0);
   MyFirstStream.Position   := 0;
   MySecondStream.Position  := 0;
   Result := True;
  End;
 Asm
  mov EDX, 0                 //Move tamanho D para EDX segundo parametro setlenght
  mov EAX, cpdst             //Move Result/pdst para EAX primeiro para metro strlenght
  call System.@LStrSetLength //Seta parametro pdst para tamanho n2
 End;
End;

{
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
}

Function ResumeStreamASM(Const S, d: Pointer; Var c: Pointer): Integer;Assembler;
Var
 src,
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

