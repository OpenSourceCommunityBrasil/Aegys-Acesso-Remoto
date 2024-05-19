unit uASMTools;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  FMX.Forms,
  Winapi.Windows;

Var
 ASMSize,
 muASM               : Integer;
 pdst                : Pointer;

 Function CompareStreamS   (Const FirstStream,
                            SecondStream      : AnsiString;
                            Var CompareStream : AnsiString)   : Boolean;
 Function CompareStreamData(Const MyFirstStream,
                            MySecondStream,
                            MyCompareStream  : TMemoryStream) : Boolean;
 Procedure ResumeStreamB   (FirstStream      : TMemoryStream;
                            Var SecondStream : TMemoryStream;
                            CompareStream    : TMemoryStream);
 Procedure ResumeStreamS   (FirstStream      : PChar;
                            Var SecondStream : PChar;
                            CompareStream    : PChar);

implementation

Procedure Processmessages;
Begin
 Application.ProcessMessages;
End;

Function ResumeStreamASM(Const s, d: Pointer; Var c: Pointer) : Integer; Assembler;
Var
 src     : ^Char;
 dest    : ^Char;
 n1, n2  : Cardinal;
Begin
 Asm
  mov muASM, 0
  mov pdst, ECX              //Move resolutado pra PDST
  mov src, EAX               //Move S pra src
  mov dest, EDX              //Move D pra dest
  call System.@LStrLen       //Tamanho de string S
  mov n1, EAX                //Move tamanho do S para n1
  mov EAX, dest              //Move dest para EAX
  call System.@LStrLen       //Tamanho do dst/D
  mov n2, EAX                //Move Tamanho D para n2
  mov EDX, EAX               //Move tamanho D para EDX segundo parametro setlenght
  mov EAX, pdst              //Move Result/pdst para EAX primeiro parametro strlenght
  call System.@LStrSetLength //Seta parametro pdst para tamanho n2
  mov ECX, ASMSize           //Mov n2 para ECX para controlar loopings
  test ECX, ECX              //Testa ECX
  jz @@end                   //Se EXX = 0 Termina
  push ESI                   //Guarda ESI na pilha
  push EDI
  mov EAX, pdst              //EAX := pdst; //Endereço da string de resultado
  mov ESI, src               //ESI := src; //String de origem
  mov EDI, dest
  mov EDX, [EAX]             //EDX := pdst^; //String de resultado
  @@cycle:
   mov AL, [EDI]             //Move um caracter do primeiro stream para AL
   cmp AL, '0'               //Copara se o caracter é 0 no segundo stream
   jne @@diferente           //Se for Diferente pula para igual
   mov AL, [ESI]             //Se defente copia Caracter do Segund stream para AL
   mov [EDX], AL             //Coloca caracter no terceiro stream
   mov muASM, 1
   cmp AL, AL                //Apenas para gerra um Je
   je @@incremento           //Incrementa caracter
  @@diferente:
   mov AL, [EDI]             //Se for <> Coloca '0' em AL
   mov [EDX], AL             //Move o caracter correto para terceiro Stream
  @@incremento:
   inc ESI
   inc EDI
   inc EDX
   dec ECX
   cmp ECX, 0
   ja @@cycle
   pop EDI
   pop ESI                   //Recupera ESI na pilha
   call Processmessages
  @@end:
 End;
 Result := muASM;
End;

Procedure ResumeStreamS(FirstStream      : PChar;
                        Var SecondStream : PChar;
                        CompareStream    : PChar);
Var
 MyFirstStream,
 MySecondStream,
 MyCompareStream : TStringStream;
 P1, P2, P3      : Pointer;
 vSource,
 vDest,
 vCompared       : TBitmap;
Begin
 MyFirstStream   := TStringStream.Create(FirstStream);
 MySecondStream  := TStringStream.Create('');
 MyCompareStream := TStringStream.Create(CompareStream);
 Try
  MyFirstStream.Position := 0;
  MySecondStream.Clear;
  If MyFirstStream.Size <> MyCompareStream.Size Then
   MyFirstStream.SetSize(MyCompareStream.Size);
  MyFirstStream.Position := 0;
  P1      := MyFirstStream.Memory;
  P2      := MyCompareStream.Memory;
  P3      := MySecondStream.Memory;
  muASM   := 0;
  ASMSize := MyCompareStream.Size;
  If ResumeStreamASM(P1, P2, P3) <> 0 Then
   Begin
    MySecondStream.Clear;
    MySecondStream.Write(P3^, MyCompareStream.Size);
    MySecondStream.Position := 0;
    SecondStream            := PChar(MySecondStream.DataString);
    MyFirstStream.Clear;
    MyFirstStream.CopyFrom(MyCompareStream, 0);
   End;
 Finally
  MyFirstStream.Free;
  MySecondStream.Free;
  MyCompareStream.Free;
 End;
 Asm
  mov EDX, 0                 //Move tamanho D para EDX segundo parametro setlenght
  mov EAX, pdst              //Move Result/pdst para EAX primeiro para metro strlenght
  call System.@LStrSetLength //Seta parametro pdst para tamanho n2
 End;
End;

Procedure ResumeStreamB(FirstStream      : TMemoryStream;
                        Var SecondStream : TMemoryStream;
                        CompareStream    : TMemoryStream);
Var
 MyFirstStream,
 MySecondStream,
 MyCompareStream : TStringStream;
 P1, P2, P3      : Pointer;
 vSource,
 vDest,
 vCompared       : TBitmap;
Begin
 MyFirstStream   := TStringStream.Create;
 FirstStream.Position := 0;
 MyFirstStream.CopyFrom(FirstStream, FirstStream.Size);
 MySecondStream  := TStringStream.Create('');
 MyCompareStream := TStringStream.Create;
 SecondStream    := TMemoryStream.Create;
 CompareStream.Position := 0;
 MyCompareStream.CopyFrom(CompareStream, CompareStream.Size);
 Try
  MyFirstStream.Position := 0;
  MySecondStream.Clear;
  If MyFirstStream.Size <> MyCompareStream.Size Then
   MyFirstStream.SetSize(MyCompareStream.Size);
  MyFirstStream.Position := 0;
  P1      := MyFirstStream.Memory;
  P2      := MyCompareStream.Memory;
  P3      := MySecondStream.Memory;
  muASM   := 0;
  ASMSize := MyCompareStream.Size;
  Processmessages;
  If ResumeStreamASM(P1, P2, P3) <> 0 Then
   Begin
    SecondStream.Write(P3^, ASMSize);
    SecondStream.Position := 0;
    Application.ProcessMessages;
   End;
 Finally
  MyFirstStream.Free;
  MySecondStream.Free;
  MyCompareStream.Free;
 End;
 Asm
  mov EDX, 0                 //Move tamanho D para EDX segundo parametro setlenght
  mov EAX, pdst              //Move Result/pdst para EAX primeiro para metro strlenght
  call System.@LStrSetLength //Seta parametro pdst para tamanho n2
 End;
End;

Function CompareStreamASM(Const s, d: Pointer; Var c: Pointer) : Integer; Assembler;
Var
 src     : ^AnsiChar;
 dest    : ^AnsiChar;
 n1, n2  : Cardinal;
Begin
 Asm
  mov muASM, 0
  mov pdst,  ECX             //Move resolutado pra PDST
  mov src,   EAX             //Move S pra src
  mov dest,  EDX             //Move D pra dest
  call System.@LStrLen       //Tamanho de string S
  mov n1, EAX                //Move tamanho do S para n1
  mov EAX, dest              //Move dest para EAX
  call System.@LStrLen       //Tamanho do dst/D
  mov n2, EAX                //Move Tamanho D para n2
  mov EDX, EAX               //Move tamanho D para EDX segundo parametro setlenght
  mov EAX, pdst              //Move Result/pdst para EAX primeiro parametro strlenght
  call System.@LStrSetLength //Seta parametro pdst para tamanho n2
  mov ECX, ASMSize           //Mov n2 para ECX para controlar loopings
  test ECX, ECX              //Testa ECX
  jz @@end                   //Se EXX = 0 Termina
  push ESI                   //Guarda ESI na pilha
  push EDI
  mov EAX, pdst              //EAX := pdst; //Endereço da string de resultado
  mov ESI, src               //ESI := src; //String de origem
  mov EDI, dest
  mov EDX, [EAX]             //EDX := pdst^; //String de resultado
  @@cycle:
   mov AL, [ESI]             //Move um caracter do primeiro stream para AL
   cmp AL, [EDI]             //Copara caracter com o segundo stream
   je @@igual                //Se for igual pula para igual
   mov AL, [EDI]             //Se defente copia Carcter do Segund stream para AL
   mov [EDX], AL             //Coloca caracter no terceiro stream
   mov muASM, 1
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
 Result := muASM;
End;

Function CompareStreamData(Const MyFirstStream,
                           MySecondStream,
                           MyCompareStream : TMemoryStream) : Boolean;
Var
 P1, P2, P3 : Pointer;
Begin
 MyFirstStream.Position := 0;
// MySecondStream.Clear;
 MyCompareStream.Clear;
// GetScreenToBmp(CapMouse, MySecondStream);
 If MyFirstStream.Size <> MySecondStream.Size Then
  MyFirstStream.SetSize(MySecondStream.Size);
 MyFirstStream.Position := 0;
 P1      := MyFirstStream.Memory;
 P2      := MySecondStream.Memory;
 P3      := MyCompareStream.Memory;
 muASM   := 0;
 ASMSize := MySecondStream.Size;
 If CompareStreamASM(P1, P2, P3) = 0 Then
  Result := False
 Else
  Begin
   MyCompareStream.Clear;
   MyCompareStream.Write(P3^, MySecondStream.Size);
   MyFirstStream.Clear;
   MyFirstStream.CopyFrom(MySecondStream, 0);
   Result := True;
  End;
 Asm
  mov EDX, 0                 //Move tamanho D para EDX segundo parametro setlenght
  mov EAX, pdst              //Move Result/pdst para EAX primeiro para metro strlenght
  call System.@LStrSetLength //Seta parametro pdst para tamanho n2
 End;
End;

Function CompareStreamS(Const FirstStream,
                        SecondStream      : AnsiString;
                        Var CompareStream : AnsiString) : Boolean;
Var
 I : Integer;
 vResult : AnsiString;
 aFirstStream,
 aSecondStream  : TBytes;
 vFirstStream,
 vSecondStream,
 vCompareStream : TMemoryStream;
 Function MemoryStreamToString(Const Value : TStream) : AnsiString;
 Begin
  Try
   SetLength(Result, Value.Size);
   Value.Position := 0;
   Value.Read(Result[1], Value.Size);
  Finally
  End;
 End;
 Function StringToMemoryStream(Const Value : TBytes) : TMemoryStream;
 Begin
  Result        := TMemoryStream.Create;
  Try
   Result.Write(Value[0], Length(Value));
   Result.Position := 0;
  Finally
  End;
 End;
Begin
 aFirstStream   := TBytes(FirstStream);
 aSecondStream  := TBytes(SecondStream);
 vFirstStream   := StringToMemoryStream(aFirstStream);
 vSecondStream  := StringToMemoryStream(aSecondStream);
 vCompareStream := TMemoryStream.Create;
 Try
  Result := CompareStreamData(TMemoryStream(vFirstStream),
                              TMemoryStream(vSecondStream),
                              TMemoryStream(vCompareStream));
  If Result Then
   CompareStream := MemoryStreamToString(vCompareStream);
 Finally
  FreeAndNil(vFirstStream);
  FreeAndNil(vSecondStream);
  FreeAndNil(vCompareStream);
 End;
End;

end.
