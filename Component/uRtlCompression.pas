unit uRtlCompression;
//written by testest

interface

const
  COMPRESSION_ENGINE_STANDARD     = $00000000;
  COMPRESSION_ENGINE_MAXIMUM     = $00000100;

function Compress(const Source: Pointer; var Dest: Pointer; Count, Compression: Cardinal): Cardinal; overload;
function Compress(const Value: String; Compression: Cardinal = COMPRESSION_ENGINE_STANDARD): String; overload;
function Decompress(const Source: Pointer; var Dest: Pointer; Count: Cardinal): Cardinal; overload;
function Decompress(const Value: String): String; overload;

implementation

const
  ntdll = 'ntdll.dll';
  COMPRESSION_FORMAT_LZNT1      = $00000002;
  DECOMPRESSION_MULTIPLICATOR   = 150;

type
  PULONG = ^ULONG;
  ULONG = Cardinal;

function RtlGetCompressionWorkSpaceSize(CompressionFormatAndEngine: ULONG;
                CompressBufferWorkSpaceSize, CompressFragmentWorkSpaceSize: PULONG): Cardinal;
                  stdcall; external ntdll name 'RtlGetCompressionWorkSpaceSize';
function RtlCompressBuffer(CompressionFormatAndEngine: ULONG; UncompressedBuffer: Pointer;
                UncompressedBufferSize: ULONG; CompressedBuffer: Pointer; CompressedBufferSize: ULONG;
                UncompressedChunkSize: ULONG; FinalCompressedSize: PULONG; WorkSpace: Pointer): Cardinal;
                  stdcall; external ntdll name 'RtlCompressBuffer';
function RtlDecompressFragment(CompressionFormat:ULONG; UncompressedFragment: Pointer;
                UncompressedFragmentSize: ULONG; CompressedBuffer: Pointer; CompressedBufferSize: ULONG;
                FragmentOffset: ULONG; FinalUncompressedSize: PULONG; WorkSpace: Pointer): Cardinal;
                  stdcall; external ntdll name 'RtlDecompressFragment';

function Compress(const Source: Pointer; var Dest: Pointer; Count, Compression: Cardinal): Cardinal;
var
  WorkSpace: Pointer;
  WorkSpaceSize, ChunkSize: Cardinal;
begin
  Result := 0;
  Compression := COMPRESSION_FORMAT_LZNT1 or Compression;
  RtlGetCompressionWorkSpaceSize(Compression, @WorkSpaceSize, @ChunkSize);
  GetMem(Dest, Count);
  GetMem(WorkSpace, WorkSpaceSize);
  RtlCompressBuffer(Compression, Source, Count, Dest, Count, ChunkSize, @Result, WorkSpace);
  FreeMem(WorkSpace);
  if Result = 0 then
  begin
    Move(Source^, Dest^, Count);
    Result := Count;
  end
  else
    ReallocMem(Dest, Result);
end;

function Compress(const Value: String; Compression: Cardinal = COMPRESSION_ENGINE_STANDARD): String;
var
  Buffer: PChar;
  Size: Cardinal;
begin
  Size := Compress(@Value[1], Pointer(Buffer), Length(Value), Compression);
  SetString(Result, Buffer, Size);
  FreeMem(Buffer);
end;

function Decompress(const Source: Pointer; var Dest: Pointer; Count: Cardinal): Cardinal;
var
  WorkSpace: Pointer;
  WorkSpaceSize, ChunkSize, BytesDecompressed: Cardinal;
begin
  Result := 0;
  BytesDecompressed := 0;
  RtlGetCompressionWorkSpaceSize(COMPRESSION_FORMAT_LZNT1, @WorkSpaceSize, @ChunkSize);
  GetMem(WorkSpace, WorkSpaceSize);
  ChunkSize := Count * DECOMPRESSION_MULTIPLICATOR div 100;
  New(Dest);
  repeat
    ReallocMem(Dest, Result + ChunkSize);
    RtlDecompressFragment(COMPRESSION_FORMAT_LZNT1, Pointer(Cardinal(Dest) + Result), ChunkSize,
                          Source, Count, Result, @BytesDecompressed, WorkSpace);
    if BytesDecompressed <= ChunkSize then
      Inc(Result, BytesDecompressed);
  until BytesDecompressed <> ChunkSize;
  FreeMem(WorkSpace);
  if Result = 0 then
  begin
    Move(Source^, Dest^, Count);
    Result := Count;
  end
  else
    ReallocMem(Dest, Result);
end;

function Decompress(const Value: String): String;
var
  Buffer: PChar;
  Size: Cardinal;
begin
  Size := Decompress(@Value[1], Pointer(Buffer), Length(Value));
  SetString(Result, Buffer, Size);
  FreeMem(Buffer);
end;

end.
