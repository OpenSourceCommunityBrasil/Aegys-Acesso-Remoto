unit uAegysTools;

{
   Aegys Remote Access Project.
  Criado por XyberX (Gilbero Rocha da Silva), o Aegys Remote Access Project tem como objetivo o uso de Acesso remoto
  Gratuito para utilização de pessoas em geral.
   O Aegys Remote Access Project tem como desenvolvedores e mantedores hoje

  Membros do Grupo :

  XyberX (Gilberto Rocha)    - Admin - Criador e Administrador  do pacote.
  Wendel Fassarela           - Devel and Admin
  Mobius One                 - Devel, Tester and Admin.
  Gustavo                    - Devel and Admin.
  Roniery                    - Devel and Admin.
  Alexandre Abbade           - Devel and Admin.
  e Outros como você, venha participar também.
}

Interface

Uses
 SysUtils, Classes, Variants,
 {$IFDEF FPC}
  LConvEncoding, lazutf8,
 {$ELSE}
  {$IFDEF RESTDWWINDOWS}Windows,{$ENDIF}
  EncdDecd,
  {$IF Defined(RESTDWFMX)}IOUtils,{$IFEND}
  {$IF CompilerVersion > 27}NetEncoding,{$IFEND}
 {$ENDIF}
 uAegysDataTypes;

 //Service Functions
  Function  MoveBytes    (Source            : TAegysBytes;
                          Var Dest          : TAegysBytes;
                          Position,
                          Length            : AeInteger) : String;
  Procedure DeleteString (Var Value         : AeString;
                          InitPos, EndPos   : AeInteger);
  Function  StringToBytes(AStr              : String)       : TAegysBytes;
  Function  BytesToString(Const AValue      : TAegysBytes;
                          Const AStartIndex : Integer;
                          Const ALength     : Integer = -1) : String;
  Function  VarToBytes   (Value             : Variant;
                          vType             : TVarType)     : TAegysBytes;
  Function  BytesToVar   (ByteValue         : TAegysBytes;
                          vType             : TVarType)     : Variant;

Implementation

Function AeMax(Const AValueOne,
               AValueTwo        : Int64) : Int64;
Begin
 If AValueOne < AValueTwo Then
  Result := AValueTwo
 Else
  Result := AValueOne;
End;

Function AeMin(Const AValueOne,
               AValueTwo        : Int64) : Int64;
Begin
 If AValueOne > AValueTwo Then
  Result := AValueTwo
 Else
  Result := AValueOne;
End;

Function AeLength(Const ABuffer : String;
                  Const ALength : Integer = -1;
                  Const AIndex  : Integer = 1) : Integer;Overload;
Var
 LAvailable: Integer;
Begin
 Assert(AIndex >= 1);
 LAvailable := AeMax(Length(ABuffer)-AIndex+1, 0);
 If ALength < 0 Then
  Result := LAvailable
 Else
  Result := AeMin(LAvailable, ALength);
End;

Function AeLength(Const ABuffer : TAegysBytes;
                  Const ALength : Integer = -1;
                  Const AIndex  : Integer = 0) : Integer;Overload;
                  {$IFDEF USE_INLINE}Inline;{$ENDIF}
Var
 LAvailable : Integer;
Begin
 Assert(AIndex >= 0);
 LAvailable := AeMax(Length(ABuffer)-AIndex, 0);
 If ALength < 0 Then
  Result := LAvailable
 Else
  Result := AeMin(LAvailable, ALength);
End;

Function  MoveBytes(Source            : TAegysBytes;
                    Var Dest          : TAegysBytes;
                    Position,
                    Length            : AeInteger) : String;
Begin
 SetLength(Dest, Length);
 Move(Source[Position], Dest[0], Length);
End;

Procedure DeleteString (Var Value         : AeString;
                        InitPos, EndPos   : AeInteger);
Begin
 Delete(Value, InitPos, EndPos);
End;

Function StringToBytes(AStr: String) : TAegysBytes;
Begin
 SetLength(Result, 0);
 If AStr <> '' Then
  Begin
   {$IFDEF FPC}
    Result := TRESTDWBytes(TEncoding.ANSI.GetBytes(Astr));
   {$ELSE}
    {$IF CompilerVersion < 23}
     SetLength(Result, Length(AStr));
     Move(Pointer(@AStr[InitStrPos])^, Pointer(Result)^, Length(AStr));
    {$ELSE}
     Result :=  TAegysBytes(TEncoding.ANSI.GetBytes(Astr));
    {$IFEND}
   {$ENDIF}
  End;
End;

Function BytesToString(Const AValue      : TAegysBytes;
                       Const AStartIndex : Integer;
                       Const ALength     : Integer = -1) : String;
Var
 LLength : Integer;
 LBytes  : TAegysBytes;
Begin
 Result := '';
 {$IFDEF STRING_IS_ANSI}
  LBytes := Nil; // keep the compiler happy
 {$ENDIF}
 LLength := AeLength(AValue, ALength, AStartIndex);
 If LLength > 0 Then
  Begin
   If (AStartIndex = 0)                And
      (LLength = AeLength(AValue)) Then
    LBytes := AValue
   Else
    LBytes := Copy(AValue, AStartIndex, LLength);
  {$IFDEF FPC}
   SetString(Result, PAnsiChar(LBytes), restdwLength(LBytes));
  {$ELSE}
   {$IF CompilerVersion < 23}
    SetString(Result, PAnsiChar(LBytes), restdwLength(LBytes));
   {$ELSE}
    {$IFDEF MSWINDOWS}
     Result := TEncoding.ANSI.GetString(TBytes(LBytes));
    {$ELSE}
     Result := AnsiToUtf8(TEncoding.ANSI.GetString(TBytes(LBytes)));
    {$ENDIF}
   {$IFEND}
  {$ENDIF}
  End;
End;

Function BytesToVar(ByteValue  : TAegysBytes;
                    vType      : TVarType)  : Variant;
Var
 P         : Pointer;
 aBoolean  : Boolean;
 aValue,
 aSize     : AEInteger;
 aByte     : Byte;
 aLongWord : LongWord;
 aDouble   : Double;
 {$IF (Not DEFINED(FPC)) AND (CompilerVersion < 21)}
 aDate     : TDateTime;
 {$ELSE}
 aDate     : TDate;
 {$IFEND}
 S         : Integer;
 vSt,
 aString   : AEString;
Begin
 Case vType Of
  varByte     : Begin
                 S      := SizeOf(Byte);
                 Move(Pointer(@ByteValue[0])^, aByte, S);
                 Result := aByte;
                End;
  varShortInt,
  varSmallint,
  varInteger,
  varInt64,
  {$IF (Defined(FPC)) OR (not(Defined(FPC)) AND (CompilerVersion > 24))}
  varUInt64,
  {$IFEND}
  varSingle   : Begin
                 S      := SizeOf(AEInteger);
                 Move(Pointer(@ByteValue[0])^, aValue, S);
                 Result := aValue;
                End;
  varWord,
  varLongWord : Begin
                 S := SizeOf(LongWord);
                 Move(Pointer(@ByteValue[0])^, aLongWord, S);
                 Result := aLongWord;
                End;
  varString
  {$IFDEF FPC}
   , varUString
  {$ELSE}
   {$IF CompilerVersion >= 22}
    , varUString
   {$IFEND}
  {$ENDIF}     : Begin
                  Move(Pointer(@ByteValue[0])^, Pointer(@aSize)^,  SizeOf(AEInteger));
                  If aSize > 0 Then
                   Begin
                    aString := BytesToString(ByteValue, SizeOf(AEInteger), aSize);
                    Result := aString;
                   End
                  Else
                   Result := '';
                 End;
  varDouble,
  varCurrency  : Begin
                  S := SizeOf(Double);
                  Move(Pointer(@ByteValue[0])^, aDouble, S);
                  Result := aDouble;
                 End;
  varDate      : Begin
                  S := SizeOf(Date);
                  Move(Pointer(@ByteValue[0])^, aDate, S);
                  Result := aDate;
                 End;
  varBoolean   : Begin
                  S := SizeOf(Boolean);
                  vSt := BytesToString(ByteValue, 0, 1);
                  aBoolean    := vSt = 'T';
                  Result      := aBoolean;
                 End;
  varNull,
  varEmpty     : Result := Null;
  Else
   Variant(Result) := Null;
 End;
End;

Function VarToBytes(Value : Variant;
                    vType : TVarType) : TAegysBytes;
Var
 P         : Pointer;
 aValue,
 aSize     : AeInteger;
 aByte     : Byte;
 aLongWord : LongWord;
 aDouble   : Double;
 {$IF (Not DEFINED(FPC)) AND (CompilerVersion < 21)}
 aDate     : TDateTime;
 {$ELSE}
 aDate     : TDate;
 {$IFEND}
 S         : Integer;
 vSt       : Char;
 aString   : AeString;
Begin
 SetLength(Result, 0);
 P := @Value;
 Case vType Of
  varByte     : Begin
                 S         := SizeOf(Byte);
                 SetLength(Result, S);
                 aByte     := Value;
                 Move(aByte, Pointer(@Result[0])^, S);
                End;
  varShortInt,
  varSmallint,
  varInteger,
  varInt64,
  {$IF (Defined(FPC)) OR (not(Defined(FPC)) AND (CompilerVersion > 24))}
  varUInt64,
  {$IFEND}
  varSingle   : Begin
                 S         := SizeOf(AeInteger);
                 SetLength(Result, S);
                 aValue    := Value;
                 Move(aValue, Pointer(@Result[0])^, S);
                End;
  varWord,
  varLongWord : Begin
                 S := SizeOf(LongWord);
                 SetLength(Result, S);
                 aLongWord  := Value;
                 Move(aLongWord, Pointer(@Result[0])^, S);
                End;
  varString
  {$IF (Defined(FPC)) OR (not(Defined(FPC)) AND (CompilerVersion > 24))}
  , varUString
  {$IFEND}
              : Begin
                 aString := Value;
                 S       := Length(aString);
                 SetLength(Result, SizeOf(AeInteger) + S);
                 aSize := S;
                 Move(Pointer(@aSize)^,                Pointer(@Result[0])^, SizeOf(AeInteger));
                 If S > 0 Then
                  Begin
                   {$IFDEF FPC}
                    Move(AnsiString(aString)[InitStrPos], Pointer(@Result[SizeOf(AeInteger)])^, S);
                   {$ELSE}
                    {$IF CompilerVersion <= 22}
                     Move(AnsiString(aString)[InitStrPos], Pointer(@Result[SizeOf(AeInteger)])^, S);
                    {$ELSE}
                     {$IF CompilerVersion <= 33}
                      Move(AeString(aString)[InitStrPos], Pointer(@Result[SizeOf(AeInteger)])^, S);
                     {$ELSE}
                      Move(AnsiString(aString)[InitStrPos], Pointer(@Result[SizeOf(AeInteger)])^, S);
                     {$IFEND}
                    {$IFEND}
                   {$ENDIF}
                  End;
                End;
  varDouble,
  varCurrency  : Begin
                  S := SizeOf(Double);
                  SetLength(Result, S);
                  aDouble    := Value;
                  Move(aDouble, Pointer(@Result[0])^, S);
                 End;
  varDate      : Begin
                  S := SizeOf(Date);
                  SetLength(Result, S);
                  aDate    := VarToDateTime(Value);
                  Move(aDate, Pointer(@Result[0])^, S);
                 End;
  varBoolean   : Begin
                  S := 1;
                  SetLength(Result, S);
                  If Value Then
                   vSt := 'T'
                  Else
                   vSt := 'F';
                  Move(vSt, Pointer(@Result[0])^, S);
                 End;
  varNull,
  varEmpty     : Begin
                  P := Nil;
                  S := 0;
                 End;
  Else
   Begin
    P := Nil;
    S := 0;
   End;
 End;
End;

end.
