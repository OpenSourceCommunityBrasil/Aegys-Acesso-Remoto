Unit uAegysBufferPack;

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
 SysUtils, Classes, Variants, uAegysConsts, uAegysDataTypes, uAegysZlib;

 Type
  TPackClass = Class;//DummyClass
  TPackError = Procedure(Pack : TPackClass) Of Object;//Event PackError
  PPackClass = ^TPackClass;
  TPackClass = Class
  Private
   vHeaderVersion : AeInteger;
   vPackError     : TPackError;
   vCompression,
   vChecked,
   vProxyMyList   : Boolean;
   vDataMode      : TDataMode;
   vDataType      : TDataType;
   vDataCheck     : TDataCheck;
   vCommandType   : TCommandType;
   vDelay,
   vRetryes       : AEInteger;
   vBufferSize,
   vPackNo,
   vPacks,
   vDataSize      : AEInt64;
   vOwner,
   vDest,
   vCommand,
   vBytesOptions  : AeString;
   vDataBytes     : TAegysBytes;
   Procedure    CleanBytes;
   Procedure    SetDataBytes  (Value       : TAegysBytes);
   Procedure    SetDataType   (Value       : TDataType);
   Function     GetDataBytes               : TAegysBytes;
   Procedure    SetCommand    (Value       : AeString);
  Public
   Constructor Create;
   Destructor  Destroy;Override;
   Procedure   FromBytes     (Value        : TAegysBytes);
   Function    ToBytes                     : TAegysBytes;
   Procedure   LoadFromStream(Value        : TStream);
   Procedure   SaveToStream  (Var Value    : TStream);
   Procedure   LoadFromFile  (Filename     : AeString);
   Procedure   SaveToFile    (Filename     : AeString);
   Property    BufferVersion               : AeInteger    Read vHeaderVersion;
   Property    ProxyToMyConnectionList     : Boolean      Read vProxyMyList    Write vProxyMyList;
   Property    Checked                     : Boolean      Read vChecked        Write vChecked;
   Property    Compression                 : Boolean      Read vCompression    Write vCompression;
   Property    DataBytes                   : TAegysBytes  Read GetDataBytes    Write SetDataBytes;
   Property    BytesOptions                : AeString     Read vBytesOptions   Write vBytesOptions;
   Property    DataMode                    : TDataMode    Read vDataMode       Write vDataMode;
   Property    DataType                    : TDataType    Read vDataType       Write SetDataType;
   Property    DataCheck                   : TDataCheck   Read vDataCheck      Write vDataCheck;
   Property    BufferSize                  : AEInt64      Read vBufferSize     Write vBufferSize;
   Property    DataSize                    : AEInt64      Read vDataSize       Write vDataSize;
   Property    Dest                        : AeString     Read vDest           Write vDest;
   Property    Command                     : AeString     Read vCommand        Write SetCommand;
   Property    CommandType                 : TCommandType Read vCommandType    Write vCommandType;
   Property    Delay                       : AEInteger    Read vDelay          Write vDelay;
   Property    Owner                       : AeString     Read vOwner          Write vOwner;
   Property    PackNo                      : AEInt64      Read vPackNo         Write vPackNo;
   Property    PacksGeral                  : AEInt64      Read vPacks          Write vPacks;
   Property    Retryes                     : AEInteger    Read vRetryes        Write vRetryes;
   Property    OnPackError                 : TPackError   Read vPackError      Write vPackError;
 End;
 Type
  PPackList = ^TPackList;
  TPackList = Class(TList)
  Private
   Function  GetRec         (Index         : Integer) : TPackClass;   Overload;
   Procedure PutRec         (Index         : Integer;
                             Item          : TPackClass);             Overload;
   Procedure ClearAll;
  Protected
  Public
   Constructor Create;
   Destructor Destroy;  Override;
   Procedure  Delete       (Index         : Integer);                Overload;
   Function   Add          (Item          : TPackClass)    : Integer;Overload;
   Function   Add          (aBytes        : TAegysBytes)   : Integer;Overload;
   Function   Add          (aStream       : TStream)       : Integer;Overload;
   Function   Add          (PackOwner,
                             PackDest      : AeString;
                             aDataMode     : TDataMode;
                             aDataCheck    : TDataCheck;
                             aCommandType  : TCommandType;
                             aCommand      : AeString;
                             aDestMylist   : Boolean   = False;
                             aBufferSize   : AEInt64   = 0;
                             aDelay        : AEInteger = 0) : Integer;Overload;
   Function   Add           (PackOwner,
                             PackDest      : AeString;
                             aDataMode     : TDataMode;
                             aDataCheck    : TDataCheck;
                             aCommand      : AeString;
                             aBufferSize   : AEInt64   = 0;
                             aDelay        : AEInteger = 0) : Integer;Overload;
   Function   Add           (PackOwner,
                             PackDest      : AeString;
                             aDataMode     : TDataMode;
                             aDataCheck    : TDataCheck;
                             aCommandType  : TCommandType;
                             aCommand      : AeString;
                             aBufferSize   : AEInt64   = 0;
                             aDelay        : AEInteger = 0) : Integer;Overload;
   Function   Add           (PackOwner,
                             PackDest      : AeString;
                             aDataMode     : TDataMode;
                             aDataCheck    : TDataCheck;
                             aCommandType  : TCommandType;
                             aDataBytes    : TAegysBytes;
                             aBytesOptions : AeString;
                             aDestMylist   : Boolean   = False;
                             aBufferSize   : AEInt64   = 0;
                             aDelay        : AEInteger = 0) : Integer;Overload;
   Function   Add           (PackOwner,
                             PackDest      : AeString;
                             aDataMode     : TDataMode;
                             aDataCheck    : TDataCheck;
                             aCommandType  : TCommandType;
                             aDataBytes    : TAegysBytes;
                             aBytesOptions : AeString;
                             aDestMylist,
                             aFullPack     : Boolean;
                             aPacks        : AEInt64   = 0) : Integer;Overload;
   Function   ReadPack      (Index         : Integer)  : TAegysBytes;
   Function   ReadBufferPack(PackNo        : Integer)  : TAegysBytes;
   Property   Items         [Index         : Integer]  : TPackClass Read GetRec Write PutRec; Default;
  End;

Implementation

Uses uAegysTools;

Function SizeOfHeader : AEInteger;
Begin
 Result := (SizeOf(Boolean)   * 2) +  SizeOf(AEInteger)      +
            SizeOf(AeDataTypeSize) +  SizeOf(AeDataTypeSize) +
            SizeOf(AeDataTypeSize) +  SizeOf(AeDataTypeSize) +
           (SizeOf(AEInteger) * 2) + (SizeOf(AEInt64) * 4);
End;

Procedure TPackClass.CleanBytes;
Begin
 SetLength(vDataBytes, 0);
End;

Constructor TPackClass.Create;
Begin
 vChecked       := False;
 vProxyMyList   := False;
 vCompression   := False;
 vOwner         := '';
 vDest          := '';
 vBytesOptions  := '';
 vPackError     := Nil;
 vDelay         := 0;
 vRetryes       := 0;
 vBufferSize    := 0;
 vPackNo        := 1;
 vPacks         := 1;
 vDataSize      := 0;
 vCommandType   := tctNone;
 vHeaderVersion := cAeBufferVersion;
End;

Destructor TPackClass.Destroy;
Begin
 vCommand     := '';
 CleanBytes;
 Inherited;
End;

Procedure TPackClass.LoadFromFile(Filename  : AeString);
Var
 aFileStream : TFileStream;
 aValue      : TAegysBytes;
Begin
 aFileStream := TFileStream.Create(Filename, fmOpenRead);
 Try
  If aFileStream.Size > 0 Then
   Begin
    aFileStream.Position := 0;
    SetLength  (aValue, aFileStream.Size);
    Try
     aFileStream.Read(aValue, aFileStream.Size);
     FromBytes(aValue);
    Finally
     SetLength (aValue, 0);
    End;
   End;
 Finally
  FreeAndNil(aFileStream);
 End;
End;

Procedure TPackClass.SaveToFile  (Filename  : AeString);
Var
 vFileStream : TFileStream;
 vStream     : TStream;
Begin
 vFileStream := TFileStream.Create(Filename, fmCreate{$IFNDEF FPC} or fmExclusive{$ENDIF});
 Try
  vStream    := TMemoryStream.Create;
  SaveToStream(vStream);
  vFileStream.CopyFrom(vStream, vStream.Size);
 Finally
  FreeAndNil(vStream);
  FreeAndNil(vFileStream);
 End;
End;

Procedure TPackClass.SaveToStream(Var Value : TStream);
Var
 aValue    : TAegysBytes;
Begin
 If Assigned(Value) Then
  Begin
   aValue := ToBytes;
   Value.Position := 0;
   Try
    Value.Write(aValue, Length(aValue));
    Value.Position := 0;
   Finally
    SetLength(aValue, 0);
   End;
  End;
End;

Procedure TPackClass.SetCommand(Value : AeString);
Begin
 vCommand := Utf8Decode(Value);
End;

Procedure TPackClass.SetDataBytes (Value    : TAegysBytes);
Var
 aStringStream,
 StringStream   : TStream;
Begin
 CleanBytes;
 aStringStream := TMemoryStream.Create;
 StringStream  := Nil;
 Try
  aStringStream.Write(Value[0], Length(Value));
  aStringStream.Position := 0;
  ZCompressStreamD(aStringStream, StringStream);
  StringStream.Position  := 0;
  SetLength(vDataBytes, StringStream.Size);
  StringStream.Read(vDataBytes[0], Length(vDataBytes));
//  Move(Value[0], vDataBytes[0], Length(Value));
 Finally
  FreeAndNil(aStringStream);
  If Assigned(StringStream) Then
   FreeAndNil(StringStream);
 End;
End;

Function TPackClass.GetDataBytes : TAegysBytes;
Var
 aStringStream,
 StringStream   : TStream;
Begin
 aStringStream := TMemoryStream.Create;
 aStringStream.Write(vDataBytes[0], Length(vDataBytes));
 Try
  Try
   StringStream := ZDecompressStreamNew(aStringStream);
   StringStream.Position  := 0;
   SetLength(Result, StringStream.Size);
   StringStream.Read(Result[0], Length(Result));
//  Move(Value[0], vDataBytes[0], Length(Value));
  Except

  End;
 Finally
  FreeAndNil(aStringStream);
  If Assigned(StringStream) Then
   FreeAndNil(StringStream);
 End;
End;

Procedure TPackClass.SetDataType  (Value    : TDataType);
Begin
 If Value = tdtString Then
  CleanBytes
 Else
  vCommand := '';
 vDataType := Value;
End;

Procedure TPackClass.FromBytes     (Value   : TAegysBytes);
Var
 aCommand,
 aOwnerBytes,
 aBytesOption,
 aResult,
 aDestBytes       : TAegysBytes;
 aSizeOf,
 aDestSize,
 aOwnerSize,
 aPosition        : AeInteger;
 aPackSize,
 aCommandSize,
 aDataSize,
 aBytesOptionSize : AEInt64;
Begin
 If vCompression Then
  Begin
   Move(Value[0],     aDataSize,      SizeOf(aDataSize));
   SetLength(aResult, aDataSize - SizeOf(aDataSize));
   Move(Value[SizeOf(aDataSize)], aResult[0], aDataSize - SizeOf(aDataSize));
   Try
    SetLength(Value, 0);
    ZDecompressBytes(aResult, Value);
   Finally
    SetLength(aResult, 0);
   End;
  End;
 aPackSize        := 0;
 aCommandSize     := 0;
 aDataSize        := 0;
 aBytesOptionSize := 0;
 aSizeOf          := 0;
 aDestSize        := 0;
 aOwnerSize       := 0;
 aPosition        := 0;
 If Length(Value) > 0 Then
  Begin
//   If Not vCompression Then
//    Begin
   Move(Value[0], aPackSize, SizeOf(aPackSize));                    //PackSize
   aPosition             :=  SizeOf(aPackSize);
//    End
//   Else
//    aPackSize := Length(Value);
   Move(Value[aPosition], vChecked,         SizeOf(vChecked));        //Checked
   aPosition            := aPosition +      SizeOf(vChecked);
   Move(Value[aPosition], vProxyMyList,     SizeOf(vProxyMyList));    //ProxyMyList
   aPosition            := aPosition +      SizeOf(vProxyMyList);
   Move(Value[aPosition], vHeaderVersion,   SizeOf(vHeaderVersion));  //Header Version
   aPosition            := aPosition +      SizeOf(vHeaderVersion);
   //Check HeaderVersion
   If vHeaderVersion = cAeBufferVersion Then
    Begin
     Try
      //LoadHeader
      Move(Value[aPosition],  vDataMode,     SizeOf(AeDataTypeSize));      //DataMode
      aPosition            := aPosition +    SizeOf(AeDataTypeSize);
      Move(Value[aPosition],  vDataType,     SizeOf(AeDataTypeSize));      //DataType
      aPosition            := aPosition +    SizeOf(AeDataTypeSize);
      Move(Value[aPosition],  vDataCheck,    SizeOf(AeDataTypeSize));     //DataCheck
      aPosition            := aPosition +    SizeOf(AeDataTypeSize);
      Move(Value[aPosition],  vCommandType,  SizeOf(AeDataTypeSize));   //CommandType
      aPosition            := aPosition +    SizeOf(AeDataTypeSize);
      Move(Value[aPosition],  vDelay,        SizeOf(vDelay));         //Delay
      aPosition            := aPosition +    SizeOf(vDelay);
      Move(Value[aPosition],  vRetryes,      SizeOf(vRetryes));       //Retryes
      aPosition            := aPosition +    SizeOf(vRetryes);
      Move(Value[aPosition],  vBufferSize,   SizeOf(vBufferSize));    //BufferSize
      aPosition            := aPosition +    SizeOf(vBufferSize);
      Move(Value[aPosition],  vPackNo,       SizeOf(vPackNo));        //PackNo
      aPosition            := aPosition +    SizeOf(vPackNo);
      Move(Value[aPosition],  vPacks,        SizeOf(vPacks));         //Packs
      aPosition            := aPosition +    SizeOf(vPacks);
      Move(Value[aPosition],  vDataSize,     SizeOf(vDataSize));      //DataSize
      aPosition            := aPosition +    SizeOf(vDataSize);
      Move(Value[aPosition],  aOwnerSize,    SizeOf(aOwnerSize));     //OwnerSize
      aPosition            := aPosition +    SizeOf(aOwnerSize);
      If aOwnerSize > 0 Then
       Begin
        SetLength(aOwnerBytes,  aOwnerSize);
        Move(Value[aPosition],  aOwnerBytes[0], aOwnerSize);            //Owner
       End;
      aPosition            := aPosition +     aOwnerSize;
      If aOwnerSize > 0 Then
       vOwner              := BytesToVar(aOwnerBytes, varString);
      SetLength(aOwnerBytes, 0);
      Move(Value[aPosition],  aDestSize,     SizeOf(aDestSize));      //DestSize
      aPosition            := aPosition +    SizeOf(aDestSize);
      If aDestSize > 0 Then
       Begin
        SetLength(aDestBytes,   aDestSize);
        Move(Value[aPosition],  aDestBytes[0], aDestSize);            //Dest
       End;
      aPosition            := aPosition +    aDestSize;
      If aDestSize > 0 Then
       vDest               := BytesToVar(aDestBytes,  varString);
      SetLength(aDestBytes, 0);
      SetLength(vDataBytes, 0);
      vCommand             := '';
      //Load DataPack
      If vDataMode <> tdmReply Then
       Begin
        If vDataType = tdtString Then //Pack For Strings
         Begin
          Move(Value[aPosition], Pointer(@aCommandSize)^, SizeOf(aCommandSize));
          aPosition := aPosition + SizeOf(aCommandSize);
          SetLength(aCommand, aCommandSize);
          Move(Value[aPosition], aCommand[0], aCommandSize);
          vCommand := BytesToVar(aCommand, varString);
         End
        Else //Pack For Bytes
         Begin
          Move(Value[aPosition], Pointer(@aDataSize)^, SizeOf(aDataSize));
          aPosition := aPosition + SizeOf(aDataSize);
          vDataSize := aDataSize;
          SetLength(vDataBytes, vDataSize);
          Move(Value[aPosition], vDataBytes[0], vDataSize);
          aPosition := aPosition + vDataSize;
          Move(Value[aPosition], Pointer(@aBytesOptionSize)^, SizeOf(aBytesOptionSize));
          aPosition := aPosition + SizeOf(aBytesOptionSize);
          If aBytesOptionSize > 0 Then
           Begin
            SetLength(aBytesOption, aBytesOptionSize);
            Move(Value[aPosition], aBytesOption[0], aBytesOptionSize);
            vBytesOptions := BytesToVar(aBytesOption, varString);
           End;
         End;
       End;
     Except
      Raise Exception.Create(cInvalidBufferData);
     End;
    End
   Else
    Raise Exception.Create(cInvalidHeaderData);
  End
 Else
  Raise Exception.Create(cInvalidBufferData);
End;

Procedure TPackClass.LoadFromStream(Value   : TStream);
Var
 aValue : TAegysBytes;
Begin
 If Assigned(Value) Then
  Begin
   If Value.Size > 0 Then
    Begin
     Value.Position := 0;
     SetLength  (aValue, Value.Size);
     Try
      Value.Read(aValue, Value.Size);
      FromBytes(aValue);
     Finally
      SetLength (aValue, 0);
     End;
    End;
  End;
End;

Function TPackClass.ToBytes                 : TAegysBytes;
Var
 aHeader,
 aOwner,
 aDest,
 aCommand,
 aBytesOption    : TAegysBytes;
 aHeaderSize,
 aDestSize,
 aOwnerSize,
 aPosition        : AeInteger;
 aPackSize,
 aDataSize,
 aBytesOptionSize : AEInt64;
 aResult          : TAegysBytes;
Begin
 aPackSize        := 0;
 aDataSize        := 0;
 aBytesOptionSize := 0;
 aHeaderSize      := 0;
 aDestSize        := 0;
 aOwnerSize       := 0;
 aPosition        := 0;
 If vDataType = tdtString Then
  Begin
   If Length(vCommand) = 0 Then
    Exit;
  End
 Else
  Begin
   If Length(vDataBytes) = 0 Then
    Exit;
  End;
 aPosition            := 0;
 aBytesOptionSize     := 0;
 SetLength(aHeader,      SizeOfHeader);
 Try
  Move(vChecked,          Pointer(@aHeader[aPosition])^, SizeOf(vChecked));       //Checked
  aPosition            := SizeOf(vChecked);
  Move(vProxyMyList,      Pointer(@aHeader[aPosition])^, SizeOf(vProxyMyList));   //ProxyMyList
  aPosition            := aPosition + SizeOf(vProxyMyList);
  Move(vHeaderVersion,    Pointer(@aHeader[aPosition])^, SizeOf(vHeaderVersion)); //DataMode
  aPosition            := aPosition +         SizeOf(vHeaderVersion);
  Move(vDataMode,         Pointer(@aHeader[aPosition])^, SizeOf(AeDataTypeSize));      //DataMode
  aPosition            := aPosition +         SizeOf(AeDataTypeSize);
  Move(vDataType,         Pointer(@aHeader[aPosition])^, SizeOf(AeDataTypeSize));      //DataType
  aPosition            := aPosition +         SizeOf(AeDataTypeSize);
  Move(vDataCheck,        Pointer(@aHeader[aPosition])^, SizeOf(AeDataTypeSize));     //DataCheck
  aPosition            := aPosition +         SizeOf(AeDataTypeSize);
  Move(vCommandType,      Pointer(@aHeader[aPosition])^, SizeOf(AeDataTypeSize));   //CommandType
  aPosition            := aPosition +         SizeOf(AeDataTypeSize);
  Move(vDelay,            Pointer(@aHeader[aPosition])^, SizeOf(vDelay));         //Delay
  aPosition            := aPosition +         SizeOf(vDelay);
  Move(vRetryes,          Pointer(@aHeader[aPosition])^, SizeOf(vRetryes));       //Retryes
  aPosition            := aPosition +         SizeOf(vRetryes);
  Move(vBufferSize,       Pointer(@aHeader[aPosition])^, SizeOf(vBufferSize));    //BufferSize
  aPosition            := aPosition +         SizeOf(vBufferSize);
  Move(vPackNo,           Pointer(@aHeader[aPosition])^, SizeOf(vPackNo));        //PackNo
  aPosition            := aPosition +         SizeOf(vPackNo);
  Move(vPacks,            Pointer(@aHeader[aPosition])^, SizeOf(vPacks));         //Packs
  aPosition            := aPosition +         SizeOf(vPacks);
  Move(vDataSize,         Pointer(@aHeader[aPosition])^, SizeOf(vDataSize));      //DataSize
  aOwner               := VarToBytes(vOwner, varString);
  aDest                := VarToBytes(vDest,  varString);
  aHeaderSize          := Length(aHeader);
  aOwnerSize           := Length(aOwner);
  aDestSize            := Length(aDest);
  If vDataMode <> tdmReply Then
   Begin
    If vDataType = tdtString Then
     Begin
      aPosition          := 0;
      aCommand           := VarToBytes(vCommand,  varString);
      aDataSize          := Length(aCommand);
      aPackSize          := SizeOf(aPackSize)  + aHeaderSize +
                            SizeOf(aOwnerSize) + aOwnerSize +
                            SizeOf(aDestSize)  + aDestSize  +
                            SizeOf(aDataSize)  + aDataSize;
      SetLength(Result, aPackSize);
      Move(aPackSize,     Pointer(@Result[aPosition])^, SizeOf(aPackSize));
      aPosition          := aPosition + SizeOf(aPackSize);
      Move(aHeader   [0], Pointer(@Result[aPosition])^, aHeaderSize);
      aPosition          := aPosition + aHeaderSize;
      Move(aOwnerSize,    Pointer(@Result[aPosition])^, SizeOf(aOwnerSize));
      If aOwnerSize > 0 Then
       Begin
        aPosition        := aPosition + SizeOf(aOwnerSize);
        Move(aOwner   [0], Pointer(@Result[aPosition])^, aOwnerSize);
       End;
      aPosition          := aPosition + aOwnerSize;
      Move(aDestSize,     Pointer(@Result[aPosition])^, SizeOf(aDestSize));
      If aDestSize > 0 Then
       Begin
        aPosition        := aPosition + SizeOf(aDestSize);
        Move(aDest    [0], Pointer(@Result[aPosition])^, aDestSize);
       End;
      aPosition          := aPosition + aDestSize;
      Move(aDataSize,     Pointer(@Result[aPosition])^, SizeOf(aDataSize));
      If aDataSize > 0 Then
       Begin
        aPosition        := aPosition + SizeOf(aDataSize);
        Move(aCommand  [0], Pointer(@Result[aPosition])^, aDataSize);
       End;
     End
    Else
     Begin
      aDataSize          := Length(vDataBytes);
      aBytesOptionSize   := Length(vBytesOptions);
      If aBytesOptionSize > 0 Then
       Begin
        aBytesOption     := VarToBytes(vBytesOptions,  varString);
        aBytesOptionSize := Length(aBytesOption);
        aPackSize        := SizeOf(aPackSize)        + aHeaderSize +
                            SizeOf(aOwnerSize)       + aOwnerSize  +
                            SizeOf(aDestSize)        + aDestSize   +
                            SizeOf(aDataSize)        + aDataSize   +
                            SizeOf(aBytesOptionSize) + aBytesOptionSize;
       End
      Else
       aPackSize         := SizeOf(aPackSize)        + aHeaderSize +
                            SizeOf(aOwnerSize)       + aOwnerSize  +
                            SizeOf(aDestSize)        + aDestSize   +
                            SizeOf(aDataSize)        + aDataSize;
      SetLength(Result,  aPackSize);
      Move(aPackSize,     Pointer(@Result[0])^, SizeOf(aPackSize));
      Move(aHeader   [0], Pointer(@Result[SizeOf(aPackSize)])^, aHeaderSize);
      Move(aOwnerSize,    Pointer(@Result[SizeOf(aPackSize) + aHeaderSize])^, SizeOf(aOwnerSize));
      If aOwnerSize > 0 Then
       Move(aOwner   [0], Pointer(@Result[SizeOf(aPackSize) + aHeaderSize + SizeOf(aOwnerSize)])^, aOwnerSize);
      Move(aDestSize,     Pointer(@Result[SizeOf(aPackSize) + aHeaderSize + SizeOf(aOwnerSize) + aOwnerSize])^, SizeOf(aDestSize));
      If aDestSize > 0 Then
       Move(aDest    [0], Pointer(@Result[SizeOf(aPackSize) + aHeaderSize + SizeOf(aOwnerSize) + aOwnerSize + SizeOf(aDestSize)])^, aDestSize);
      Move(aDataSize,     Pointer(@Result[SizeOf(aPackSize) + aHeaderSize + SizeOf(aOwnerSize) + aOwnerSize + SizeOf(aDestSize) + aDestSize])^, SizeOf(aDataSize));
      Move(vDataBytes[0], Pointer(@Result[SizeOf(aPackSize) + aHeaderSize + SizeOf(aOwnerSize) + aOwnerSize + SizeOf(aDestSize) + aDestSize + SizeOf(aDataSize)])^, aDataSize);
      If aBytesOptionSize > 0 Then
       Begin
        Move(aBytesOptionSize, Pointer(@Result[SizeOf(aPackSize) + aHeaderSize + SizeOf(aOwnerSize) + aOwnerSize + SizeOf(aDestSize) + aDestSize + SizeOf(aDataSize) + aDataSize])^, SizeOf(aBytesOptionSize));
        Move(aBytesOption[0],  Pointer(@Result[SizeOf(aPackSize) + aHeaderSize + SizeOf(aOwnerSize) + aOwnerSize + SizeOf(aDestSize) + aDestSize + SizeOf(aDataSize) + aDataSize + SizeOf(aBytesOptionSize)])^, aBytesOptionSize);
       End;
     End;
   End
  Else
   Begin
    aPackSize         := SizeOf(aPackSize)  + aHeaderSize +
                         SizeOf(aOwnerSize) + aOwnerSize  +
                         SizeOf(aDestSize)  + aDestSize;
    SetLength(Result,  aPackSize);
    Move(aPackSize,     Pointer(@Result[0])^, SizeOf(aPackSize));
    Move(aHeader   [0], Pointer(@Result[SizeOf(aPackSize)])^, aHeaderSize);
    Move(aOwnerSize,    Pointer(@Result[SizeOf(aPackSize) +   aHeaderSize])^, SizeOf(aOwnerSize));
    Move(aOwner    [0], Pointer(@Result[SizeOf(aPackSize) +   aHeaderSize + SizeOf(aOwnerSize)])^, aOwnerSize);
    Move(aDestSize,     Pointer(@Result[SizeOf(aPackSize) +   aHeaderSize + SizeOf(aOwnerSize) + aOwnerSize])^, SizeOf(aDestSize));
    Move(aDest     [0], Pointer(@Result[SizeOf(aPackSize) +   aHeaderSize + SizeOf(aOwnerSize) + aOwnerSize + SizeOf(aDestSize)])^, aDestSize);
   End;
  If vCompression Then
   Begin
    SetLength(aResult, Length(Result));
    Move(Result[0], aResult[0], Length(Result));
//    aResult := Result;
    Try
     SetLength(Result, 0);
     ZCompressBytes(aResult, Result);
    Finally
     SetLength(aResult, 0);
    End;
   End;
 Except
  Raise Exception.Create('Error Message');
 End;
End;

Function TPackList.Add    (aBytes         : TAegysBytes) : Integer;
Var
 aItem : TPackClass;
Begin
 aItem := TPackClass.Create;
 Try
  aItem.FromBytes(aBytes);
  Add(aItem);
 Except
  FreeAndNil(aItem);
 End;
End;

Function TPackList.Add    (aStream        : TStream)     : Integer;
Var
 aBytesValue : TAegysBytes;
Begin
 If Assigned(aStream) Then
  Begin
   If aStream.Size > 0 Then
    Begin
     SetLength(aBytesValue, aStream.Size);
     Try
      aStream.Read(aBytesValue, aStream.Size);
      Add(aBytesValue);
     Finally
      SetLength(aBytesValue, 0);
     End;
    End;
  End;
End;

Function TPackList.Add    (Item           : TPackClass)    : Integer;
Var
 vItem: PPackClass;
Begin
 New(vItem);
 Try
  vItem^        := Item;
  Result        := Inherited Add(vItem);
  vItem^.PackNo := 1;
 Finally
 End;
End;

Function TPackList.Add    (PackOwner,
                           PackDest       : AeString;
                           aDataMode      : TDataMode;
                           aDataCheck     : TDataCheck;
                           aCommand       : AeString;
                           aBufferSize    : AEInt64   = 0;
                           aDelay         : AEInteger = 0) : Integer;
Var
 aItem            : TPackClass;
 bBufferSize,
 aPacksGeralB,
 aPackNoB,
 aPackCount       : AEInt64;
 atempDataString,
 atempDataStringB : AeString;
Begin
 Result           := -1;
 aPacksGeralB     := Length(aCommand);
 Try
  If aPacksGeralB > 0 Then
   Begin
    If aBufferSize = 0 Then
     Begin
      aItem             := TPackClass.Create;
      aItem.DataCheck   := aDataCheck;
      aItem.DataSize    := aPacksGeralB;
      aItem.BufferSize  := aItem.DataSize;
      aItem.PacksGeral  := 1;
      aItem.PackNo      := 1;
      aItem.DataMode    := aDataMode;
      aItem.DataType    := tdtString;
      aItem.Command     := aCommand;
      aItem.Owner       := PackOwner;
      aItem.Dest        := PackDest;
      Result            := Add(aItem);
      aItem.Delay       := aDelay;
     End
    Else //Build MultiPack
     Begin
      aPackNoB          := 0;
      atempDataString   := aCommand;
      bBufferSize       := aBufferSize - SizeOfHeader - Length(PackOwner) - Length(PackDest);
      If bBufferSize > 0 Then
       Begin
        aPackCount        := (aPacksGeralB div bBufferSize);
        If (aPacksGeralB Mod aBufferSize) > 1 Then
         Inc(aPackCount)
        Else If aPackCount = 0 Then
         Inc(aPackCount);
        While (atempDataString <> '') Do
         Begin
          aItem             := TPackClass.Create;
          aItem.DataCheck   := aDataCheck;
          aItem.DataSize    := aPacksGeralB;
          aItem.PacksGeral  := aPackCount;
          aItem.PackNo      := aPackNoB;
          aItem.DataMode    := aDataMode;
          aItem.DataType    := tdtString;
          aItem.Owner       := PackOwner;
          aItem.Dest        := PackDest;
          atempDataStringB  := Copy(atempDataString, InitStrPos, bBufferSize);
          DeleteString(atempDataString, InitStrPos, bBufferSize);
          aItem.Command     := atempDataStringB;
          aItem.BufferSize  := Length(atempDataStringB);
          Result            := Add(aItem);
          aItem.Delay       := aDelay;
          Inc(aPackNoB);
         End;
       End
      Else
       Raise Exception.Create(cPackInvalidSize + ' Buffer is ' + IntToStr(bBufferSize));
     End;
   End;
 Finally
 End;
End;

Function TPackList.Add    (PackOwner,
                           PackDest      : AeString;
                           aDataMode     : TDataMode;
                           aDataCheck    : TDataCheck;
                           aCommandType  : TCommandType;
                           aCommand      : AeString;
                           aDestMylist   : Boolean   = False;
                           aBufferSize   : AEInt64   = 0;
                           aDelay        : AEInteger = 0) : Integer;
Var
 aItem            : TPackClass;
 bBufferSize,
 aPacksGeralB,
 aPackNoB,
 aPackCount       : AEInt64;
 atempDataString,
 atempDataStringB : AeString;
Begin
 Result           := -1;
 aPacksGeralB     := Length(aCommand);
 Try
  If aPacksGeralB > 0 Then
   Begin
    If aBufferSize = 0 Then
     Begin
      aItem              := TPackClass.Create;
      aItem.DataCheck    := aDataCheck;
      aItem.DataMode     := aDataMode;
      aItem.CommandType  := aCommandType;
      aItem.vProxyMyList := aDestMylist;
      aItem.DataSize     := aPacksGeralB;
      aItem.BufferSize   := aItem.DataSize;
      aItem.PacksGeral   := 1;
      aItem.PackNo       := 1;
      aItem.DataMode     := aDataMode;
      aItem.DataType     := tdtString;
      aItem.Command      := aCommand;
      aItem.Owner        := PackOwner;
      aItem.Dest         := PackDest;
      Result             := Add(aItem);
      aItem.Delay        := aDelay;
     End
    Else //Build MultiPack
     Begin
      aPackNoB          := 0;
      atempDataString   := aCommand;
      bBufferSize       := aBufferSize - SizeOfHeader - Length(PackOwner) - Length(PackDest);
      If bBufferSize > 0 Then
       Begin
        aPackCount        := (aPacksGeralB div bBufferSize);
        If (aPacksGeralB Mod aBufferSize) > 1 Then
         Inc(aPackCount)
        Else If aPackCount = 0 Then
         Inc(aPackCount);
        While (atempDataString <> '') Do
         Begin
          aItem              := TPackClass.Create;
          aItem.DataCheck    := aDataCheck;
          aItem.DataMode     := aDataMode;
          aItem.CommandType  := aCommandType;
          aItem.vProxyMyList := aDestMylist;
          aItem.DataSize     := aPacksGeralB;
          aItem.PacksGeral   := aPackCount;
          aItem.PackNo       := aPackNoB;
          aItem.DataMode     := aDataMode;
          aItem.DataType     := tdtString;
          aItem.Owner        := PackOwner;
          aItem.Dest         := PackDest;
          atempDataStringB   := Copy(atempDataString, InitStrPos, bBufferSize);
          DeleteString(atempDataString, InitStrPos, bBufferSize);
          aItem.Command      := atempDataStringB;
          aItem.BufferSize   := Length(atempDataStringB);
          Result             := Add(aItem);
          aItem.Delay        := aDelay;
          Inc(aPackNoB);
         End;
       End
      Else
       Raise Exception.Create(cPackInvalidSize + ' Buffer is ' + IntToStr(bBufferSize));
     End;
   End;
 Finally
 End;
End;

Function TPackList.Add    (PackOwner,
                           PackDest       : AeString;
                           aDataMode      : TDataMode;
                           aDataCheck     : TDataCheck;
                           aCommandType   : TCommandType;
                           aDataBytes     : TAegysBytes;
                           aBytesOptions  : AeString;
                           aDestMylist,
                           aFullPack      : Boolean;
                           aPacks         : AEInt64   = 0) : Integer;
Var
 aItem            : TPackClass;
 aInitPosition,
 bBufferSize,
 aPacksGeralB     : AEInt64;
Begin
 Result           := -1;
 aPacksGeralB     := Length(aDataBytes);
 Try
  If aPacksGeralB > 0 Then
   Begin
    If aFullPack Then
     Begin
      aItem              := TPackClass.Create;
      aItem.DataCheck    := aDataCheck;
      aItem.DataSize     := aPacksGeralB;
      aItem.vProxyMyList := aDestMylist;
      aItem.BufferSize   := aItem.DataSize;
      aItem.PacksGeral   := 1;
      aItem.PackNo       := 1;
      aItem.DataMode     := aDataMode;
      aItem.DataType     := tdtDataBytes;
      aItem.CommandType  := aCommandType;
      aItem.DataBytes    := aDataBytes;
      aItem.BytesOptions := aBytesOptions;
      aItem.Owner        := PackOwner;
      aItem.Dest         := PackDest;
      Result             := Add(aItem);
      aItem.Delay        := 0;
     End
    Else //Build MultiPack
     Begin
      bBufferSize       := Length(aDataBytes) - SizeOfHeader - Length(PackOwner) - Length(PackDest) - Length(aBytesOptions);
      If bBufferSize > 0 Then
       Begin
        aItem              := TPackClass.Create;
        aItem.DataCheck    := aDataCheck;
        aItem.DataSize     := aPacksGeralB;
        aItem.vProxyMyList := aDestMylist;
        aItem.PacksGeral   := aPacks;
        aItem.PackNo       := 1;
        aItem.DataMode     := aDataMode;
        aItem.DataType     := tdtDataBytes;
        aItem.CommandType  := aCommandType;
        aItem.Owner        := PackOwner;
        aItem.Dest         := PackDest;
        aItem.BytesOptions := aBytesOptions;
        aItem.DataBytes    := aDataBytes;
        aItem.BufferSize   := bBufferSize;
        Result             := Add(aItem);
        aItem.Delay        := 0;
       End
      Else
       Raise Exception.Create(cPackInvalidSize + ' Buffer is ' + IntToStr(bBufferSize));
     End;
   End;
 Finally
 End;
End;

Function TPackList.Add    (PackOwner,
                           PackDest       : AeString;
                           aDataMode      : TDataMode;
                           aDataCheck     : TDataCheck;
                           aCommandType   : TCommandType;
                           aDataBytes     : TAegysBytes;
                           aBytesOptions  : AeString;
                           aDestMylist    : Boolean   = False;
                           aBufferSize    : AEInt64   = 0;
                           aDelay         : AEInteger = 0) : Integer;
Var
 aItem            : TPackClass;
 aInitPosition,
 bBufferSize,
 aActualBufferSize,
 aPacksGeralB,
 aPackNoB,
 aPackCount       : AEInt64;
 atempDataBytes,
 atempDataBytesB  : TAegysBytes;
 aEOF             : Boolean;
Begin
 Result           := -1;
 aPacksGeralB     := Length(aDataBytes);
 Try
  If aPacksGeralB > 0 Then
   Begin
    If aBufferSize = 0 Then
     Begin
      aItem              := TPackClass.Create;
      aItem.DataCheck    := aDataCheck;
      aItem.DataSize     := aPacksGeralB;
      aItem.vProxyMyList := aDestMylist;
      aItem.BufferSize   := aItem.DataSize;
      aItem.PacksGeral   := 1;
      aItem.PackNo       := 1;
      aItem.DataMode     := aDataMode;
      aItem.DataType     := tdtDataBytes;
      aItem.CommandType  := aCommandType;
      aItem.DataBytes    := aDataBytes;
      aItem.BytesOptions := aBytesOptions;
      aItem.Owner        := PackOwner;
      aItem.Dest         := PackDest;
      Result             := Add(aItem);
      aItem.Delay        := aDelay;
     End
    Else //Build MultiPack
     Begin
      atempDataBytes    := aDataBytes;
      bBufferSize       := aBufferSize - SizeOfHeader - Length(PackOwner) - Length(PackDest) - Length(aBytesOptions);
      If bBufferSize > 0 Then
       Begin
        aPackNoB            := 0;
        aPackCount          := (aPacksGeralB div bBufferSize);
        If (aPacksGeralB Mod aBufferSize) > 0 Then
         Inc(aPackCount);
        aEOF := Length(atempDataBytes) = 0;
        aInitPosition := 0;
        While Not aEOF Do
         Begin
          aItem              := TPackClass.Create;
          aItem.DataCheck    := aDataCheck;
          aItem.DataSize     := aPacksGeralB;
          aItem.vProxyMyList := aDestMylist;
          aItem.PacksGeral   := aPackCount;
          aItem.PackNo       := aPackNoB;
          aItem.DataMode     := aDataMode;
          aItem.DataType     := tdtDataBytes;
          aItem.CommandType  := aCommandType;
          aItem.Owner        := PackOwner;
          aItem.Dest         := PackDest;
          aItem.BytesOptions := aBytesOptions;
          SetLength(atempDataBytesB, 0);
          If (aInitPosition + bBufferSize) < Length(atempDataBytes) Then
           aActualBufferSize := bBufferSize
          Else
           aActualBufferSize := Length(atempDataBytes) - aInitPosition;
          MoveBytes(atempDataBytes, atempDataBytesB, aInitPosition, aActualBufferSize);
          aInitPosition      := aInitPosition + Length(atempDataBytesB);
          aEOF               := aInitPosition >= aPacksGeralB;
          aItem.DataBytes    := atempDataBytesB;
          aItem.BufferSize   := Length(atempDataBytesB);
          Result             := Add(aItem);
          aItem.Delay        := aDelay;
          Inc(aPackNoB);
         End;
       End
      Else
       Raise Exception.Create(cPackInvalidSize + ' Buffer is ' + IntToStr(bBufferSize));
     End;
   End;
 Finally
 End;
End;

Function TPackList.Add(PackOwner,
                       PackDest     : AeString;
                       aDataMode    : TDataMode;
                       aDataCheck   : TDataCheck;
                       aCommandType : TCommandType;
                       aCommand     : AeString;
                       aBufferSize  : AEInt64;
                       aDelay       : AEInteger): Integer;
Var
 aItem            : TPackClass;
 bBufferSize,
 aPacksGeralB,
 aPackNoB,
 aPackCount       : AEInt64;
 atempDataString,
 atempDataStringB : AeString;
Begin
 Result           := -1;
 aPacksGeralB     := Length(aCommand);
 Try
  If aPacksGeralB > 0 Then
   Begin
    If aBufferSize = 0 Then
     Begin
      aItem             := TPackClass.Create;
      aItem.DataCheck   := aDataCheck;
      aItem.DataSize    := aPacksGeralB;
      aItem.BufferSize  := aItem.DataSize;
      aItem.PacksGeral  := 1;
      aItem.PackNo      := 1;
      aItem.DataMode    := aDataMode;
      aItem.DataType    := tdtString;
      aItem.CommandType := aCommandType;
      aItem.Command     := aCommand;
      aItem.Owner       := PackOwner;
      aItem.Dest        := PackDest;
      Result            := Add(aItem);
      aItem.Delay       := aDelay;
     End
    Else //Build MultiPack
     Begin
      aPackNoB          := 0;
      atempDataString   := aCommand;
      bBufferSize       := aBufferSize - SizeOfHeader - Length(PackOwner) - Length(PackDest);
      If bBufferSize > 0 Then
       Begin
        aPackCount        := (aPacksGeralB div bBufferSize);
        If (aPacksGeralB Mod aBufferSize) > 1 Then
         Inc(aPackCount)
        Else If aPackCount = 0 Then
         Inc(aPackCount);
        While (atempDataString <> '') Do
         Begin
          aItem             := TPackClass.Create;
          aItem.DataCheck   := aDataCheck;
          aItem.DataSize    := aPacksGeralB;
          aItem.PacksGeral  := aPackCount;
          aItem.PackNo      := aPackNoB;
          aItem.DataMode    := aDataMode;
          aItem.CommandType := aCommandType;
          aItem.DataType    := tdtString;
          aItem.Owner       := PackOwner;
          aItem.Dest        := PackDest;
          atempDataStringB  := Copy(atempDataString, InitStrPos, bBufferSize);
          DeleteString(atempDataString, InitStrPos, bBufferSize);
          aItem.Command     := atempDataStringB;
          aItem.BufferSize  := Length(atempDataStringB);
          Result            := Add(aItem);
          aItem.Delay       := aDelay;
          Inc(aPackNoB);
         End;
       End
      Else
       Raise Exception.Create(cPackInvalidSize + ' Buffer is ' + IntToStr(bBufferSize));
     End;
   End;
 Finally
 End;
End;

Procedure TPackList.ClearAll;
Var
 I : Integer;
Begin
 I := Count - 1;
 While I > -1 Do
  Begin
   Delete(I);
   Dec(I);
  End;
 Inherited Clear;
End;

Constructor TPackList.Create;
Begin
 Inherited;
End;

Procedure TPackList.Delete       (Index  : Integer);
Begin
 If Not Assigned(Self) Then
  Exit;
 If (Index > -1)        And
    (Index <= Count -1) Then
  Begin
   Try
    If Assigned(TList(Self).Items[Index]) Then
     Begin
      If Assigned(TPackClass(TList(Self).Items[Index]^)) Then
       Begin
       {$IFDEF FPC}
        FreeAndNil(TList(Self).Items[Index]^);
       {$ELSE}
        {$IF CompilerVersion > 33}
         FreeAndNil(TPackClass(TList(Self).Items[Index]^));
        {$ELSE}
         FreeAndNil(TList(Self).Items[Index]^);
        {$IFEND}
       {$ENDIF}
       End;
     End;
    {$IFDEF FPC}
     Dispose(PPackClass(TList(Self).Items[Index]));
    {$ELSE}
     Dispose(TList(Self).Items[Index]);
    {$ENDIF}
   Except
   End;
   TList(Self).Delete(Index);
  End;
End;

Destructor TPackList.Destroy;
Begin
 ClearAll;
 Inherited;
End;

Function TPackList.GetRec        (Index  : Integer) : TPackClass;
Begin
 Result := Nil;
 If (Index < Self.Count) And (Index > -1) Then
  Result := TPackClass(TList(Self).Items[Index]^);
End;

Procedure TPackList.PutRec       (Index  : Integer;
                                  Item   : TPackClass);
Begin
 If (Index < Self.Count) And (Index > -1) Then
  TPackClass(TList(Self).Items[Index]^) := Item;
End;

Function TPackList.ReadBufferPack(PackNo : Integer) : TAegysBytes;
Begin
 //For Read Complete Datapacks
 //Assync files and Etc...
End;

Function TPackList.ReadPack      (Index  : Integer) : TAegysBytes;
Begin
 SetLength(Result, 0);
 If (Index > -1)        And
    (Index <= Count -1) Then
  Begin
   Result := Items[Index].ToBytes;
   If Items[Index].vDataCheck = tdcAsync Then
    Delete(Index);
  End
 Else
  Raise Exception.Create(cOutOfRange);
End;

End.
