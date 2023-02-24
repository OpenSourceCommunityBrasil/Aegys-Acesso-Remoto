unit uAegysTools;

{$I ..\Includes\uAegys.inc}

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
  Function  MoveBytes    (Source              : TAegysBytes;
                          Var Dest            : TAegysBytes;
                          Position,
                          Length              : AeInteger) : String;
  Procedure DeleteString (Var Value           : AeString;
                          InitPos, EndPos     : AeInteger);
  Function  StringToBytes(AStr                : String)       : TAegysBytes;
  Function  BytesToString(Const AValue        : TAegysBytes;
                          Const AStartIndex   : Integer;
                          Const ALength       : Integer = -1) : String;
  Function  VarToBytes   (Value               : Variant;
                          vType               : TVarType)     : TAegysBytes;
  Function  BytesToVar   (ByteValue           : TAegysBytes;
                          vType               : TVarType)     : Variant;
  Procedure ParseCommand (Var Command         : String;
                          Var InternalCommand : TInternalCommand);
  Procedure ParseValues  (Var Source          : String;
                          Values              : TArrayOfPointer);
  Function  StreamToHex  (Stream              : TStream;
                          QQuoted             : Boolean = True) : String;
  Procedure HexToStream  (Str                 : String;
                          Stream              : TStream);

Implementation

Uses uAegysConsts;

Procedure LimpaLixoHex(Var Value : String);
Begin
 If Length(Value) > 0 Then
  Begin
   If Value[1] = '{' Then
    Delete(Value, 1, 1);
  End;
 If Length(Value) > 0 Then
  Begin
   If Value[1] = #13 Then
    Delete(Value, 1, 1);
  End;
 If Length(Value) > 0 Then
  Begin
   If Value[1] = '"' Then
    Delete(Value, 1, 1);
  End;
 If Length(Value) > 0 Then
  Begin
   If Value[1] = 'L' Then
    Delete(Value, 1, 1);
  End;
 If Length(Value) > 0 Then
  Begin
   If Value[Length(Value)] = '"' Then
    Delete(Value, Length(Value), 1);
  End;
End;

Procedure HexToStream(Str    : String;
                      Stream : TStream);
{$IFDEF POSIX} //Android}
var bytes: TBytes;
{$ENDIF}
Begin
 LimpaLixoHex(Str);
 {$IF Defined(ANDROID) or Defined(IOS)} //Alteardo para IOS Brito
  SetLength(bytes, Length(str) div 2);
  HexToBin(PChar(str), 0, bytes, 0, Length(bytes));
  stream.WriteBuffer(bytes[0], length(bytes));
 {$ELSE}
   TMemoryStream(Stream).Size := Length(Str) Div 2;
   {$IFDEF FPC}
   HexToBin(PChar(Str), TMemoryStream(Stream).Memory, TMemoryStream(Stream).Size);
   {$ELSE}
    {$IF CompilerVersion > 21} // Delphi 2010 pra cima
    {$IF (NOT Defined(FPC) AND Defined(LINUX))} //Alteardo para Lazarus LINUX Brito
     SetLength(bytes, Length(str) div 2);
     HexToBin(PChar(str), 0, bytes, 0, Length(bytes));
     stream.WriteBuffer(bytes[0], length(bytes));
    {$ELSE}
     HexToBin(PWideChar (Str),   TMemoryStream(Stream).Memory, TMemoryStream(Stream).Size);
    {$IFEND}
    {$ELSE}
     HexToBin(PChar (Str),   TMemoryStream(Stream).Memory, TMemoryStream(Stream).Size);
    {$IFEND}
   {$ENDIF}
 {$IFEND}
 Stream.Position := 0;
End;

Function StreamToHex(Stream  : TStream; QQuoted : Boolean = True) : String;
{$IFDEF POSIX} //Android}
var bytes, bytes2: TBytes;
{$ENDIF}
Begin
 Stream.Position := 0;
 {$IFNDEF FPC}
  {$IF Defined(ANDROID) or Defined(IOS)} //Alteardo para IOS Brito
   Result := abbintohexstring(stream);
  {$ELSE}
   {$IFDEF LINUX} // Android}
    Result := abbintohexstring(stream); // BytesToString(bytes2);  // TEncoding.UTF8.GetString(bytes2);
   {$ELSE}
    SetLength     (Result, Stream.Size * 2);
    BinToHex      (TMemoryStream(Stream).Memory, PChar(Result), Stream.Size);
   {$ENDIF}
  {$IFEND}
 {$ELSE}
  SetLength     (Result, Stream.Size * 2);
  BinToHex      (TMemoryStream(Stream).Memory, PChar(Result), Stream.Size);
 {$ENDIF}
 If QQuoted Then
  Result := '"' + Result + '"';
End;

Procedure ParseCommand(Var Command         : String;
                       Var InternalCommand : TInternalCommand);
Begin
 InternalCommand := ticNone;
 If Copy(Command, InitStrPos, Length(cStatusDesc))            = cStatusDesc           Then
  Begin
   InternalCommand := ticDataStatus;
   Delete(Command, InitStrPos, Length(cStatusDesc));
  End
 Else If Copy(Command, InitStrPos, Length(cFindID))           = cFindID               Then
  Begin
   InternalCommand := ticFindID;
   Delete(Command, InitStrPos, Length(cFindID));
  End
 Else If Copy(Command, InitStrPos, Length(cIDExistsReqPass))  = cIDExistsReqPass      Then
  Begin
   InternalCommand := ticIDExistsReqPass;
   Delete(Command, InitStrPos, Length(cIDExistsReqPass));
  End
 Else If Copy(Command, InitStrPos, Length(cIDNotFound))       = cIDNotFound           Then
  Begin
   InternalCommand := ticIDNotFound;
   Delete(Command, InitStrPos, Length(cIDNotFound));
  End
 Else If Copy(Command, InitStrPos, Length(cPing))             = cPing                 Then
  Begin
   InternalCommand := ticPing;
   Delete(Command, InitStrPos, Length(cPing));
  End
 Else If Copy(Command, InitStrPos, Length(cCheckPass))        = cCheckPass            Then
  Begin
   InternalCommand := ticCheckPass;
   Delete(Command, InitStrPos, Length(cCheckPass));
  End
 Else If Copy(Command, InitStrPos, Length(cAccessGranted))    = cAccessGranted        Then
  Begin
   InternalCommand := ticAccessGranted;
   Delete(Command, InitStrPos, Length(cAccessGranted));
  End
 Else If Copy(Command, InitStrPos, Length(cGetMonitorCount))  = cGetMonitorCount      Then
  Begin
   InternalCommand := ticGetMonitorCount;
   Delete(Command, InitStrPos, Length(cGetMonitorCount));
  End
 Else If Copy(Command, InitStrPos, Length(cChangeMonitor))    = cChangeMonitor        Then
  Begin
   InternalCommand := ticChangeMonitor;
   Delete(Command, InitStrPos, Length(cChangeMonitor));
  End
 Else If Copy(Command, InitStrPos, Length(cRelation))         = cRelation             Then
  Begin
   InternalCommand := ticRelation;
   Delete(Command, InitStrPos, Length(cRelation));
  End
 Else If Copy(Command, InitStrPos, Length(cConnectedPeer))    = cConnectedPeer        Then
  Begin
   InternalCommand := ticConnectedPeer;
   Delete(Command, InitStrPos, Length(cConnectedPeer));
  End
 Else If Copy(Command, InitStrPos, Length(cDisconnectedPeer)) = cDisconnectedPeer     Then
  Begin
   InternalCommand := ticDisconnectedPeer;
   Delete(Command, InitStrPos, Length(cDisconnectedPeer));
  End
 Else If Copy(Command, InitStrPos, Length(cPing))             = cPing                 Then
  Begin
   InternalCommand := ticPing;
   Delete(Command, InitStrPos, Length(cPing));
  End
 Else If Copy(Command, InitStrPos, Length(cPong))             = cPong                 Then
  Begin
   InternalCommand := ticPong;
   Delete(Command, InitStrPos, Length(cPong));
  End
 Else If Copy(Command, InitStrPos, Length(cIncommingConnect)) = cIncommingConnect     Then
  Begin
   InternalCommand := ticIncommingConnect;
   Delete(Command, InitStrPos, Length(cIncommingConnect));
  End
 Else If Copy(Command, InitStrPos, Length(cDisconnectPeer)) = cDisconnectPeer         Then
  Begin
   InternalCommand := ticDisconnectPeer;
   Delete(Command, InitStrPos, Length(cDisconnectPeer));
  End
 Else If Copy(Command, InitStrPos, Length(cDisconnectAllPeers)) = cDisconnectAllPeers Then
  Begin
   InternalCommand := ticDisconnectAllPeers;
   Delete(Command, InitStrPos, Length(cDisconnectAllPeers));
  End
 Else If Copy(Command, InitStrPos, Length(cKickPeer))           = cKickPeer Then
  Begin
   InternalCommand := ticKick;
   Delete(Command, InitStrPos, Length(cKickPeer));
  End;
End;
Procedure ParseValues(Var Source : String;
                      Values     : TArrayOfPointer);
Var
 I : Integer;
 PString : ^String;
Begin
 For I := 0 To Length(Values)-1 Do
  Begin
   PString := Values[I];
   If Pos('&', Source) > 0 then
    Begin
     PString^ := Copy(Source, InitStrPos, Pos('&', Source) -1);
     Delete(Source, InitStrPos, Pos('&', Source));
    End
   Else
    Begin
     PString^ := Copy(Source, InitStrPos, Length(Source));
     Delete(Source, InitStrPos, Length(Source));
    End;
   If Source = '' Then
    Break;
  End;
End;

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
