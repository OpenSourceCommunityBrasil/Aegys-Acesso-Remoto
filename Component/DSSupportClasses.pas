unit DSSupportClasses;

interface

uses System.Classes, System.ZLib, System.SysUtils;

type
  TDSSupportZLib = class(TObject)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
    class function ZCompressString(aText: string): TBytes;
    class function ZDecompressString(aText: TBytes): string;
  end;

implementation

{ TDSSupport }

class function TDSSupportZLib.ZCompressString(aText: string): TBytes;
var
  strInput: TBytesStream;
  strOutput: TBytesStream;
  Zipper: TZCompressionStream;
begin
  SetLength(Result, 0);
  strInput := TBytesStream.Create(TEncoding.UTF8.GetBytes(aText));
  strOutput := TBytesStream.Create;
  try
    Zipper := TZCompressionStream.Create(TCompressionLevel.clMax, strOutput);
    try
      Zipper.CopyFrom(strInput, strInput.size);
    finally
      Zipper.Free;
    end;
    Result := Copy(strOutput.Bytes, 0, strOutput.size);
  finally
    strInput.Free;
    strOutput.Free;
  end;
end;

class function TDSSupportZLib.ZDecompressString(aText: TBytes): string;
var
  strInput: TBytesStream;
  strOutput: TBytesStream;
  UnZipper: TZDecompressionStream;
begin
  Result := '';
  strInput := TBytesStream.Create(aText);
  strOutput := TBytesStream.Create;
  try
    UnZipper := TZDecompressionStream.Create(strInput);
    try
      try
        strOutput.CopyFrom(UnZipper, 0);
      except
        on E: Exception do
        begin
          raise Exception.Create('Error Message: ' + E.Message);
        end;
      end;
    finally
      UnZipper.Free;
    end;
    Result := TEncoding.UTF8.GetString(strOutput.Bytes, 0, strOutput.size);
  finally
    strInput.Free;
    strOutput.Free;
  end;
end;

end.
