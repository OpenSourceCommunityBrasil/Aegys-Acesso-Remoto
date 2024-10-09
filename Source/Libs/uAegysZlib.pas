unit uAegysZlib;

Interface

Uses
{$IFDEF FPC}zstream, base64,{$ENDIF}
 FMX.Forms, SysUtils, Classes, zlib, uAegysDataTypes,
 uAegysTools;

Const
 CompressBuffer    = 1024 * 2;
 cCompressionLevel = clFastest;

// Fun��es de Compress�o e descompress�o de Stream com ZLib
Procedure ZCompressStream    (inStream,
                              outStream          : TStream;
                              aCompressionLevel  : TCompressionLevel = clDefault);
Procedure ZCompressStreamBytes(Var inStream      : TStream;
                               Var outBytes      : TAegysBytes;
                               aCompressionLevel : TCompressionLevel = clDefault);
Procedure ZCompressBytes     (Var inBytes,
                              outBytes           : TAegysBytes;
                              aCompressionLevel  : TCompressionLevel = clDefault);
Procedure ZDecompressBytes   (Var inBytes,
                              outBytes           : TAegysBytes);
Procedure ZDecompressBytesStream(Var inBytes     : TAegysBytes;
                                 Var OutStream   : TStream);
Procedure ZDecompressStream     (inStream,
                                 outStream       : TStream);
Function ZDecompressStreamNew   (Const S         : TStream)       : TStream;
Function ZDecompressStr         (Const S         : String;
                                 Var Value       : String)        : Boolean;
Function ZDecompressStreamD     (Const S         : TStringStream;
                                 Var Value       : TStringStream) : Boolean;
Function ZCompressStreamNew     (Const S         : String)        : TStream;
Function ZCompressStreamSS      (Const S         : String)        : TStringStream;
Function ZCompressStr           (Const S         : String;
                                 Var Value       : String)        : Boolean;
Function ZCompressStreamD       (S               : TStream;
                                 Var Value       : TStream)       : Boolean;

implementation

Procedure ZDecompressBytesStream(Var inBytes      : TAegysBytes;
                                 Var OutStream    : TStream);
Var
 D : TDecompressionstream;
 B : Array [1 .. CompressBuffer] of Byte;
 R : Integer;
 Size : AEInt64;
 inStream : TStream;
Begin
 If Not Assigned(outStream) Then
  Exit;
 inStream  := TStringStream.Create('');
 outStream.Position := 0;
 Try
  inStream.Write(inBytes[0], Length(inBytes));
  inStream.Position := 0;
  D := TDecompressionstream.Create(inStream);
  D.Read(Size, SizeOf(AEInt64));
  inStream.Position := SizeOf(Size);
 {$IFDEF FPC}
  While True Do
   Begin
    R := D.Read(B, SizeOf(B));
    If R <> 0 Then
      outStream.WriteBuffer(B, R)
    Else
      Break;
   End;
  outStream.Position := 0;
  FreeAndNil(D);
 {$ELSE}
  Try
    Repeat
      If ((Size - outStream.Size) > CompressBuffer) Then
        R := D.Read(B, SizeOf(B))
      Else
        R := D.Read(B, (Size - outStream.Size));
      If R > 0 then
        outStream.Write(B, R);
    Until R < SizeOf(B);
  Finally
    outStream.Position := 0;
    D.Free;
  End;
 {$ENDIF}
 Finally
  outStream.Position := 0;
  FreeAndNil(inStream);
 End;
End;

Procedure ZDecompressBytes(Var inBytes,
                           outBytes         : TAegysBytes);
Var
 D : TDecompressionstream;
 B : Array [1 .. CompressBuffer] of Byte;
 R : Integer;
 Size : AEInt64;
 inStream,
 outStream : TStream;
Begin
 inStream  := TStringStream.Create('');
 outStream := TMemoryStream.Create;
 Try
  inStream.Write(inBytes[0], Length(inBytes));
  inStream.Position := 0;
  D := TDecompressionstream.Create(inStream);
  D.Read(Size, SizeOf(AEInt64));
  inStream.Position := SizeOf(Size);
 {$IFDEF FPC}
  While True Do
   Begin
    R := D.Read(B, SizeOf(B));
    If R <> 0 Then
      outStream.WriteBuffer(B, R)
    Else
      Break;
   End;
  outStream.Position := 0;
  FreeAndNil(D);
 {$ELSE}
//  inStream.Position := 0;//SizeOf(Size);
  Try
    Repeat
      If ((Size - outStream.Size) > CompressBuffer) Then
        R := D.Read(B, SizeOf(B))
      Else
        R := D.Read(B, (Size - outStream.Size));
      If R > 0 then
        outStream.Write(B, R);
    Until R < SizeOf(B);
  Finally
    outStream.Position := 0;
    D.Free;
  End;
 {$ENDIF}
 Finally
  outStream.Position := 0;
  SetLength(outBytes, outStream.Size);
  outStream.Read(outBytes[0], outStream.Size);
  FreeAndNil(inStream);
  FreeAndNil(outStream);
 End;
End;

Procedure ZCompressBytes     (Var inBytes,
                              outBytes         : TAegysBytes;
                              aCompressionLevel : TCompressionLevel = clDefault);
Var
 DS        : TCompressionStream;
 Size      : AeInt64;
 inStream,
 outStream : TStream;
 bBytes    : TAegysBytes;
Begin
 inStream  := TStringStream.Create('');
 outStream := TStringStream.Create('');
 Try
  inStream.Write(inBytes[0], Length(inBytes));
  inStream.Position := 0; // Goto Start of input stream
  DS := TCompressionStream.Create(aCompressionLevel, outStream);
  Try
   Size := inStream.Size;
   inStream.Position := 0;
   DS.Write(Size, SizeOf(AEInt64));
   DS.CopyFrom(inStream, inStream.Size);
  Finally
   DS.Free;
   Size := outStream.Size;
   SetLength(outBytes, Size);
   outStream.Position := 0;
   outStream.Read(outBytes[0], Size);
//   SetLength(outBytes,   Size);
//   Move(Pointer(@Size)^, outBytes[0], SizeOf(AeInt64));
//   Move(bBytes[0],       outBytes[SizeOf(AeInt64)], Length(bBytes));
  End;
 Finally
  SetLength(bBytes, 0);
  FreeAndNil(inStream);
  FreeAndNil(outStream);
 End;
End;

Procedure ZCompressStreamBytes(Var inStream      : TStream;
                               Var outBytes      : TAegysBytes;
                               aCompressionLevel : TCompressionLevel = clDefault);
Var
  DS        : TCompressionStream;
  outStream : TMemoryStream;
  Size      : AeInt64;
Begin
 inStream.Position := 0; // Goto Start of input stream
 SetLength(outBytes, 0);
 outStream := TMemoryStream.Create;
 Try
  DS := TCompressionStream.Create(aCompressionLevel, outStream);
  Try
   Size := inStream.Size;
   inStream.Position := 0;
   DS.Write(Size, SizeOf(AEInt64));
   Application.Processmessages;
   DS.CopyFrom(inStream, inStream.Size);
  Finally
   DS.Free;
  End;
 Finally
  outStream.Position := 0;
  SetLength(outBytes, outStream.Size);
  outStream.Read(outBytes[0], outStream.Size);
  FreeANdNil(outStream);
  Application.Processmessages;
 End;
End;

Procedure ZCompressStream(inStream, outStream: TStream;
                          aCompressionLevel: TCompressionLevel = clDefault);
Var
  DS: TCompressionStream;
  Size: AeInt64;
Begin
  inStream.Position := 0; // Goto Start of input stream
  DS := TCompressionStream.Create(aCompressionLevel, outStream);
  Try
    Size := inStream.Size;
    inStream.Position := 0;
    DS.Write(Size, SizeOf(AEInt64));
    DS.CopyFrom(inStream, inStream.Size);
  Finally
    DS.Free;
  End;
End;

Procedure ZDecompressStream(inStream, outStream: TStream);
Var
  D: TDecompressionstream;
  B: Array [1 .. CompressBuffer] of Byte;
  R: Integer;
  Size: AEInt64;
Begin
  D := TDecompressionstream.Create(inStream);
  D.Read(Size, SizeOf(AEInt64));

{$IFDEF FPC}
  While True Do
  Begin
    R := D.Read(B, SizeOf(B));
    If R <> 0 Then
      outStream.WriteBuffer(B, R)
    Else
      Break;
  End;
  outStream.Position := 0;
  FreeAndNil(D);
{$ELSE}
  inStream.Position := SizeOf(Size);
  Try
    Repeat
      If ((Size - outStream.Size) > CompressBuffer) Then
        R := D.Read(B, SizeOf(B))
      Else
        R := D.Read(B, (Size - outStream.Size));
      If R > 0 then
        outStream.Write(B, R);
    Until R < SizeOf(B);
  Finally
    outStream.Position := 0;
    D.Free;
  End;
{$ENDIF}
End;

Function ZCompressStreamD(S: TStream; Var Value: TStream): Boolean;
Var
  Utf8Stream: TStream;
Begin
  Result := False;
  Try
    Utf8Stream := TMemoryStream.Create;
{$IFDEF FPC}
    Utf8Stream.CopyFrom(S, S.Size);
{$ELSE}
{$IF CompilerVersion > 24} // Delphi 2010 pra cima
    Utf8Stream.CopyFrom(S, S.Size);
{$ELSE} // Delphi 2010 pra cima
    Utf8Stream.Write(AnsiString(TStringStream(S).Datastring)
      [InitStrPos], S.Size);
{$IFEND} // Delphi 2010 pra cima
{$ENDIF}
    Value := TMemoryStream.Create;
    Try
      ZCompressStream(Utf8Stream, Value, cCompressionLevel);
      Value.Position := 0;
      Result := True;
    Finally

    End;
  Finally
{$IFNDEF FPC}Utf8Stream.Size := 0; {$ENDIF}
    Utf8Stream.Free;
    If Value.Size = 0 Then
    Begin
      Result := False;
      Value.Size := 0;
      FreeAndNil(Value);
    End;
  End;
End;

Function ZCompressStreamSS(Const S: String): TStringStream;
Var
  Utf8Stream: TStringStream;
Begin
  Try
{$IFDEF FPC}
    Utf8Stream := TStringStream.Create(S);
{$ELSE}
{$IF CompilerVersion > 24} // Delphi 2010 pra cima
    Utf8Stream := TStringStream.Create(S{$IF CompilerVersion > 21},
      TEncoding.UTF8{$IFEND});
{$ELSE} // Delphi 2010 pra cima
    Utf8Stream := TStringStream.Create('');
    Utf8Stream.Write(AnsiString(S)[1], Length(AnsiString(S)));
{$IFEND} // Delphi 2010 pra cima
{$ENDIF}
{$IFNDEF FPC}
    Result := TStringStream.Create('');
{$ELSE}
    Result := TStringStream.Create('');
{$ENDIF}
    Try
      ZCompressStream(Utf8Stream, Result, cCompressionLevel);
      Result.Position := 0;
    Finally

    End;
  Finally
{$IFNDEF FPC}Utf8Stream.Size := 0; {$ENDIF}
    Utf8Stream.Free;
    If Result.Size = 0 Then
      FreeAndNil(Result);
  End;
End;

Function ZCompressStreamNew(Const S: String): TStream;
Var
  Utf8Stream: TStream;
Begin
  Try
    Utf8Stream := TMemoryStream.Create;
{$IFDEF FPC}
    Utf8Stream.Write(AnsiString(S)[1], Length(AnsiString(S)));
{$ELSE}
{$IF CompilerVersion < 25} // Delphi 2010 pra cima
    Utf8Stream.Write(AnsiString(S)[1], Length(AnsiString(S)));
{$ELSE} // Delphi 2010 pra cima
{$IFDEF MSWINDOWS}
    Utf8Stream.Write(AnsiString(S)[1], Length(AnsiString(S)));
{$ELSE}
    Utf8Stream.Write(S[1], Length(S));
{$ENDIF}
{$IFEND} // Delphi 2010 pra cima
{$ENDIF}
    Result := TMemoryStream.Create;
    Try
      ZCompressStream(Utf8Stream, Result, cCompressionLevel);
      Result.Position := 0;
    Finally

    End;
  Finally
{$IFNDEF FPC}Utf8Stream.Size := 0; {$ENDIF}
    Utf8Stream.Free;
    If Result.Size = 0 Then
      FreeAndNil(Result);
  End;
End;

Function ZCompressStr(Const S: String; Var Value: String): Boolean;
Var
  Utf8Stream: TStringStream;
  Compressed: TMemoryStream;
Begin
{$IFDEF FPC}
  Result := False;
  Utf8Stream := TStringStream.Create(S);
{$ELSE}
{$IF CompilerVersion > 24} // Delphi 2010 pra cima
  Utf8Stream := TStringStream.Create(S{$IF CompilerVersion > 21},
    TEncoding.UTF8{$IFEND});
{$ELSE} // Delphi 2010 pra cima
  Utf8Stream := TStringStream.Create('');
  Utf8Stream.Write(AnsiString(S)[1], Length(AnsiString(S)));
{$IFEND} // Delphi 2010 pra cima
{$ENDIF}
  Try
    Compressed := TMemoryStream.Create;
    Try
      ZCompressStream(Utf8Stream, Compressed, cCompressionLevel);
      Compressed.Position := 0;
      Try
        Value := StreamToHex(Compressed, False);
        Result := True;
      Finally
      End;
    Finally
{$IFNDEF FPC}
{$IF CompilerVersion > 21}
{$IFDEF LINUXFMX}
      Compressed := Nil;
{$ELSE}
      Compressed.Clear;
{$ENDIF}
{$IFEND}
      FreeAndNil(Compressed);
{$ELSE}
      Compressed := Nil;
{$ENDIF}
    End;
  Finally
{$IFNDEF FPC}{$IF CompilerVersion > 21}Utf8Stream.Clear; {$IFEND}{$ENDIF}
    FreeAndNil(Utf8Stream);
  End;
End;

Function ZDecompressStreamD(Const S: TStringStream;
  Var Value: TStringStream): Boolean;
Var
  Utf8Stream, Base64Stream: TStringStream;
{$IFDEF FPC}
  Encoder: TBase64DecodingStream;
{$ENDIF}
Begin
{$IFDEF FPC}
  Base64Stream := TStringStream.Create('');
  S.Position := 0;
  Base64Stream.CopyFrom(S, 0);
  Base64Stream.Position := 0;
{$ELSE}
  Base64Stream := TStringStream.Create(''{$IF CompilerVersion > 21},
    TEncoding.UTF8{$IFEND});
  S.Position := 0;
  Base64Stream.CopyFrom(S, S.Size);
  Base64Stream.Position := 0;
{$ENDIF}
  Try
{$IFDEF FPC}
    Value := TStringStream.Create('');
{$ELSE}
    Value := TStringStream.Create('');
    // {$if CompilerVersion > 21}, TEncoding.UTF8{$IFEND});
{$ENDIF}
    Try
      Try
{$IFDEF FPC}
        Utf8Stream := TStringStream.Create('');
        HexToStream(Base64Stream.Datastring, Utf8Stream);
        Utf8Stream.Position := 0;
        ZDecompressStream(Utf8Stream, Value);
        Value.Position := 0;
{$ELSE}
        Utf8Stream := TStringStream.Create(''{$IF CompilerVersion > 21},
          TEncoding.UTF8{$IFEND});
        HexToStream(Base64Stream.Datastring, Utf8Stream);
        Utf8Stream.Position := 0;
        ZDecompressStream(Utf8Stream, Value);
        Value.Position := 0;
{$ENDIF}
        Result := True;
      Except
        Result := False;
      End;
    Finally
{$IFNDEF FPC}Utf8Stream.Size := 0; {$ENDIF}
      FreeAndNil(Utf8Stream);
    End;
  Finally
{$IFNDEF FPC}Base64Stream.Size := 0; {$ENDIF}
    FreeAndNil(Base64Stream);
  End;
End;

Function ZDecompressStreamNew(Const S: TStream): TStream;
Begin
  Result := TMemoryStream.Create;
  S.Position := 0;
  ZDecompressStream(S, Result);
  Result.Position := 0;
End;

Function ZDecompressStr(Const S: String; Var Value: String): Boolean;
Var
  Utf8Stream, Compressed, Base64Stream: TStringStream;
{$IFDEF FPC}
  Encoder: TBase64DecodingStream;
{$ENDIF}
Begin
{$IFDEF FPC}
  Result := False;
  Base64Stream := TStringStream.Create(S);
{$ELSE}
  Base64Stream := TStringStream.Create(S{$IF CompilerVersion > 22},
    TEncoding.ANSI{$IFEND});
{$ENDIF}
  Try
    Compressed := TStringStream.Create('');
    Try
{$IFDEF FPC}
      Utf8Stream := TStringStream.Create('');
      Encoder := TBase64DecodingStream.Create(Base64Stream);
      Utf8Stream.CopyFrom(Encoder, Encoder.Size);
      Utf8Stream.Position := 0;
      FreeAndNil(Encoder);
      Compressed.Position := 0;
      ZDecompressStream(Utf8Stream, Compressed);
{$ELSE}
      Utf8Stream := TStringStream.Create(''{$IF CompilerVersion > 21},
        TEncoding.UTF8{$IFEND});
      ZDecompressStream(Base64Stream, Compressed);
      Compressed.Position := 0;
{$ENDIF}
      Try
        Value := Compressed.Datastring;
        Result := True;
      Finally
{$IFNDEF FPC}Utf8Stream.Size := 0; {$ENDIF}
        FreeAndNil(Utf8Stream);
      End;
    Finally
{$IFNDEF FPC}Compressed.Size := 0; {$ENDIF}
      FreeAndNil(Compressed);
    End;
  Finally
{$IFNDEF FPC}Base64Stream.Size := 0; {$ENDIF}
    FreeAndNil(Base64Stream);
  End;
End;

end.
