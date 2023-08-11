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
  Function  EncodeStrings(Value               : String)         : String;
  Function  DecodeStrings(Value               : String)         : String;
  Function  EncodeStream (Value               : TStream)        : String;
  Function  DecodeStream (Value               : String)         : TMemoryStream;


Implementation

Uses uAegysConsts;

Function RemoveLineBreaks(aText : string) : String;
Begin
 { Retirando as quebras de linha em campos blob }
 Result := StringReplace(aText, #$D#$A, '', [rfReplaceAll]);
 { Retirando as quebras de linha em campos blob }
 Result := StringReplace(Result, sLineBreak, '', [rfReplaceAll]);
End;

Function restdwMin(Const AValueOne,
                   AValueTwo        : Int64) : Int64;
Begin
 If AValueOne > AValueTwo Then
  Result := AValueTwo
 Else
  Result := AValueOne;
End;

Function restdwMax(Const AValueOne,
                   AValueTwo        : Int64) : Int64;
Begin
 If AValueOne < AValueTwo Then
  Result := AValueTwo
 Else
  Result := AValueOne;
End;

Function restdwLength(Const ABuffer : String;
                      Const ALength : Integer = -1;
                      Const AIndex  : Integer = 1) : Integer;
Var
 LAvailable: Integer;
Begin
 Assert(AIndex >= 1);
 LAvailable := restdwMax(Length(ABuffer)-AIndex+1, 0);
 If ALength < 0 Then
  Result := LAvailable
 Else
  Result := restdwMin(LAvailable, ALength);
End;

Function RawToBytes(Const AValue : String;
                    Const ASize  : Integer) : TBytes;
Type
 PBytes = ^TBytes;
Var
 vSizeChar : Integer;
 vString   : String;
Begin
 vSizeChar := 1;
 If SizeOf(WideChar) > 1 Then
  vSizeChar := 2;
// SetLength(Result, ASize * vSizeChar);
 SetLength(Result, ASize);
 If ASize > 0 Then
  Move(Utf8String(AValue)[InitStrPos], PBytes(Result)^, Length(Result));
End;

Function ToBytes(Const AValue  : String;
                 Const ALength : Integer;
                 Const AIndex  : Integer = 1) : TBytes;Overload;
Var
 LLength: Integer;
 LBytes: TBytes;
Begin
 {$IFDEF STRING_IS_ANSI}
  LBytes := nil; // keep the compiler happy
 {$ENDIF}
 LLength := restdwLength(AValue, ALength, AIndex);
 If LLength > 0 Then
  Result := RawToBytes(AValue, LLength)
 Else
  SetLength(Result, 0);
End;

Function ToBytes(Const AValue : String) : TBytes;Overload;
Begin
 Result := ToBytes(AValue, -1, 1);
End;

Function EncodeStream (Value : TStream) : String;
 {$IFNDEF FPC}
   Function EncodeBase64(AValue : TStream) : String;
   Var
    StreamDecoded : TMemoryStream;
    StreamEncoded : TStringStream;
   Begin
    StreamDecoded := TMemoryStream.Create;
    StreamEncoded := TStringStream.Create('');
    Try
     StreamDecoded.CopyFrom(AValue, AValue.Size);
     StreamDecoded.Position := 0;
     EncdDecd.EncodeStream(StreamDecoded, StreamEncoded);
     Result := RemoveLineBreaks(StreamEncoded.DataString); //Gledston 03/12/2022
    Finally
     StreamEncoded.Free;
     StreamDecoded.Free;
    End;
   End;
 {$ELSE}
  Function EncodeBase64(AValue : TStream) : String;
  Var
   outstream : TStringStream;
  Begin
   outstream := TStringStream.Create('');
   Try
    outstream.CopyFrom(AValue, AValue.Size);
    outstream.Position := 0;
    Result := EncodeStrings(outstream.Datastring, csUndefined);
   Finally
    FreeAndNil(outstream);
   End;
  End;
 {$ENDIF}
Begin
 Result         := '';
 Value.Position := 0;
 If Value.Size > 0 Then
  Result := EncodeBase64(Value);
 Value.Position := 0;
End;

Function DecodeStream(Value : String) : TMemoryStream;
Var
 vRESTDWBytes : TBytes;
Begin
 If Trim(Value) = '' Then
  Exit;
 vRESTDWBytes := TBytes(TNetEncoding.Base64.DecodeStringToBytes(Value));;
 Result       := TMemoryStream.Create;
 Try
  Result.WriteBuffer(vRESTDWBytes[0], Length(vRESTDWBytes));
  Result.Position := 0;
 Except
 End;
End;

Function  EncodeStrings(Value : String) : String;
Var
 Ne : TBase64Encoding;
Begin
 Ne := TBase64Encoding.Create(-1, '');
 Try
  Result := Ne.Encode(Value);
 Finally
  FreeAndNil(Ne);
 End;
End;

Function  DecodeStrings(Value : String) : String;
Var
 Ne : TBase64Encoding;
Begin
 Ne     := TBase64Encoding.Create(-1, '');
 Try
  Result := Ne.Decode(Value);
 Finally
  FreeAndNil(Ne);
 End;
End;

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
{$IFNDEF FPC}
 {$IF Defined(ANDROID) or Defined(LINUX) or Defined(IOS)}
 Function abbintohexstring(stream: Tstream):string;
 Var
  s: TStream;
  i: Integer;
  b: Byte;
  hex: String;
 Begin
  s := stream;
  Try
   s.Seek(int64(0), word(soFromBeginning));
   For i:=1 to s.Size do
    Begin
     s.Read(b, 1);
     hex := IntToHex(b, 2);
     result:=result+hex;
    End;
  Finally
   s.Free;
  End;
 End;
 {$IFend}
{$ENDIF}
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
 If Copy(Command, 1, Length(cStatusDesc))              = cStatusDesc           Then
  Begin
   InternalCommand := ticDataStatus;
   Delete(Command, 1, Length(cStatusDesc));
  End
 Else If Copy(Command, 1, Length(cFindID))             = cFindID               Then
  Begin
   InternalCommand := ticFindID;
   Delete(Command, 1, Length(cFindID));
  End
 Else If Copy(Command, 1, Length(cIDExistsReqPass))    = cIDExistsReqPass      Then
  Begin
   InternalCommand := ticIDExistsReqPass;
   Delete(Command, 1, Length(cIDExistsReqPass));
  End
 Else If Copy(Command, 1, Length(cIDNotFound))         = cIDNotFound           Then
  Begin
   InternalCommand := ticIDNotFound;
   Delete(Command, 1, Length(cIDNotFound));
  End
 Else If Copy(Command, 1, Length(cPing))               = cPing                 Then
  Begin
   InternalCommand := ticPing;
   Delete(Command, 1, Length(cPing));
  End
 Else If Copy(Command, 1, Length(cCheckPass))          = cCheckPass            Then
  Begin
   InternalCommand := ticCheckPass;
   Delete(Command, 1, Length(cCheckPass));
  End
 Else If Copy(Command, 1, Length(cAccessGranted))      = cAccessGranted        Then
  Begin
   InternalCommand := ticAccessGranted;
   Delete(Command, 1, Length(cAccessGranted));
  End
 Else If Copy(Command, 1, Length(cAccessDenied))       = cAccessDenied         Then
  Begin
   InternalCommand := ticAccessDenied;
   Delete(Command, 1, Length(cAccessDenied));
  End
 Else If Copy(Command, 1, Length(cGetMonitorCount))    = cGetMonitorCount      Then
  Begin
   InternalCommand := ticGetMonitorCount;
   Delete(Command, 1, Length(cGetMonitorCount));
  End
 Else If Copy(Command, 1, Length(cChangeMonitor))      = cChangeMonitor        Then
  Begin
   InternalCommand := ticChangeMonitor;
   Delete(Command, 1, Length(cChangeMonitor));
  End
 Else If Copy(Command, 1, Length(cChangeImageQuality)) = cChangeImageQuality   Then
  Begin
   InternalCommand := ticChangeImageQuality;
   Delete(Command, 1, Length(cChangeImageQuality));
  End
 Else If Copy(Command, 1, Length(cRelation))           = cRelation             Then
  Begin
   InternalCommand := ticRelation;
   Delete(Command, 1, Length(cRelation));
  End
 Else If Copy(Command, 1, Length(cConnectedPeer))      = cConnectedPeer        Then
  Begin
   InternalCommand := ticConnectedPeer;
   Delete(Command, 1, Length(cConnectedPeer));
  End
 Else If Copy(Command, 1, Length(cDisconnectedPeer))   = cDisconnectedPeer     Then
  Begin
   InternalCommand := ticDisconnectedPeer;
   Delete(Command, 1, Length(cDisconnectedPeer));
  End
 Else If Copy(Command, 1, Length(cPing))               = cPing                 Then
  Begin
   InternalCommand := ticPing;
   Delete(Command, 1, Length(cPing));
  End
 Else If Copy(Command, 1, Length(cPong))               = cPong                 Then
  Begin
   InternalCommand := ticPong;
   Delete(Command, 1, Length(cPong));
  End
 Else If Copy(Command, 1, Length(cIncommingConnect))   = cIncommingConnect     Then
  Begin
   InternalCommand := ticIncommingConnect;
   Delete(Command, 1, Length(cIncommingConnect));
  End
 Else If Copy(Command, 1, Length(cDisconnectPeer))     = cDisconnectPeer       Then
  Begin
   InternalCommand := ticDisconnectPeer;
   Delete(Command, 1, Length(cDisconnectPeer));
  End
 Else If Copy(Command, 1, Length(cDisconnectAllPeers)) = cDisconnectAllPeers   Then
  Begin
   InternalCommand := ticDisconnectAllPeers;
   Delete(Command, 1, Length(cDisconnectAllPeers));
  End
 Else If Copy(Command, 1, Length(cKickPeer))           = cKickPeer             Then
  Begin
   InternalCommand := ticKick;
   Delete(Command, 1, Length(cKickPeer));
  End
 Else If Copy(Command, 1, Length(cNewID))              = cNewID                Then
  Begin
   InternalCommand := ticNewID;
   Delete(Command, 1, Length(cNewID));
  End
 Else If Copy(Command, 1, Length(cSetPortSend))        = cSetPortSend          Then
  Begin
   InternalCommand := ticSetPortSend;
   Delete(Command, 1, Length(cSetPortSend));
  End
 Else If Copy(Command, 1, Length(cSetPortRec))         = cSetPortRec           Then
  Begin
   InternalCommand := ticSetPortRec;
   Delete(Command, 1, Length(cSetPortRec));
  End
 Else If Copy(Command, 1, Length(cNewPeerList))        = cNewPeerList          Then
  Begin
   InternalCommand := ticPeerNewList;
   Delete(Command, 1, Length(cNewPeerList));
  End
 Else If Copy(Command, 1, Length(cPeerOn))             = cPeerOn               Then
  Begin
   InternalCommand := ticPeerOn;
   Delete(Command, 1, Length(cPeerOn));
  End
 Else If Copy(Command, 1, Length(cPeerOff))            = cPeerOff              Then
  Begin
   InternalCommand := ticPeerOff;
   Delete(Command, 1, Length(cPeerOff));
  End
 Else If Copy(Command, 1, Length(cPeerService))        = cPeerService          Then
  Begin
   InternalCommand := ticPeerService;
   Delete(Command, 1, Length(cPeerService));
  End
 Else If Copy(Command, 1, Length(cSendImage))          = cSendImage            Then
  Begin
   InternalCommand := ticPeerSendImage;
   Delete(Command, 1, Length(cSendImage));
  End
 Else If Copy(Command, 1, Length(cInsertPeer))         = cInsertPeer           Then
  Begin
   InternalCommand := ticNewPeerData;
   Delete(Command, 1, Length(cInsertPeer));
  End
 Else If Copy(Command, 1, Length(cEditPeer))           = cEditPeer             Then
  Begin
   InternalCommand := ticEditPeerData;
   Delete(Command, 1, Length(cEditPeer));
  End
 Else If Copy(Command, 1, Length(cDeletePeer))         = cDeletePeer           Then
  Begin
   InternalCommand := ticDeletePeerData;
   Delete(Command, 1, Length(cDeletePeer));
  End
 Else If Copy(Command, 1, Length(cMyConfigs))          = cMyConfigs            Then
  Begin
   InternalCommand := ticConnectionEdit;
   Delete(Command, 1, Length(cMyConfigs));
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
     PString^ := Copy(Source, 1, Pos('&', Source) -1);
     Delete(Source, 1, Pos('&', Source));
    End
   Else
    Begin
     PString^ := Copy(Source, 1, Length(Source));
     Delete(Source, 1, Length(Source));
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
 Delete(Value, 1, EndPos);
End;

Function StringToBytes(AStr: String) : TAegysBytes;
Begin
 SetLength(Result, 0);
 If AStr <> '' Then
  Begin
   {$IFDEF FPC}
    Result := TAegysBytes(TEncoding.ANSI.GetBytes(Astr));
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
   SetString(Result, PAnsiChar(LBytes), Length(LBytes));
  {$ELSE}
   {$IF CompilerVersion < 23}
    SetString(Result, PAnsiChar(LBytes), Length(LBytes));
   {$ELSE}
    {$IFDEF MSWINDOWS}
     Result := TEncoding.ANSI.GetString(TBytes(LBytes));
    {$ELSE}
     {$IF CompilerVersion > 32}
      Result := AnsiToUtf8(TEncoding.ANSI.GetString(TBytes(LBytes)));
     {$ELSE}
      Result := Utf8Decode(TEncoding.ANSI.GetString(TBytes(LBytes)));
     {$IFEND}
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
