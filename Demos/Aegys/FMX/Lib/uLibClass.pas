unit uLibClass;

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
  System.Classes;

type
  TRDLib = class
  public
    class function ColarTexto: string;
    class function CompressStreamWithZLib(SrcStream: TMemoryStream): Boolean;
    class function DeCompressStreamWithZLib(SrcStream: TMemoryStream): Boolean;
    class function GetAppVersionStr: string;
    class function GetSize(bytes: single): string;
    class function GetWallpaperDirectory: string;
    class function ListFiles(FileName, Ext: string): string;
    class function ListFolders(Directory: string): string;
    class function MemoryStreamToString(M: TMemoryStream): AnsiString;
    class procedure ChangeWallpaper(Directory: string);
    class procedure CopiarTexto(ATexto: string);
  end;

implementation

uses
  Winapi.Windows, System.SysUtils, FMX.Forms, Registry, System.Rtti, FMX.Platform,
  FMX.Surfaces, System.ZLib;

{ TRDLib }

class procedure TRDLib.ChangeWallpaper(Directory: string);
var
  Reg: TRegistry;
begin
  Reg := nil;
  try
    Reg := TRegistry.Create;
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.OpenKey('Control Panel\Desktop', False);
    Reg.WriteString('Wallpaper', Directory);
    SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, nil, SPIF_SENDWININICHANGE);
  finally
    FreeAndNil(Reg);
  end;
end;

class function TRDLib.ColarTexto: string;
var
  Svc: IFMXClipboardService;
  vValue: TValue;
begin
  Result := '';
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc) then
  begin
    vValue := Svc.GetClipboard;
    if (not vValue.IsEmpty) and (vValue.IsType<string>) then
      Result := vValue.ToString;
  end;
end;

class function TRDLib.CompressStreamWithZLib(SrcStream: TMemoryStream): Boolean;
var
  InputStream: TMemoryStream;
  inbuffer: Pointer;
  outbuffer: Pointer;
  count, outcount: longint;
begin
  Result := False;
  InputStream := TMemoryStream.Create;

  try
    InputStream.LoadFromStream(SrcStream);
    count := InputStream.Size;
    getmem(inbuffer, count);
    InputStream.ReadBuffer(inbuffer^, count);
    zcompress(inbuffer, count, outbuffer, outcount, zcDefault);
    SrcStream.Clear;
    SrcStream.Write(outbuffer^, outcount);
    Result := true;
  finally
    FreeAndNil(InputStream);
    FreeMem(inbuffer, count);
    FreeMem(outbuffer, outcount);
  end;
end;

class procedure TRDLib.CopiarTexto(ATexto: string);
var
  Svc: IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc) then
    Svc.SetClipboard(ATexto);
end;

class function TRDLib.DeCompressStreamWithZLib(SrcStream: TMemoryStream): Boolean;
var
  InputStream: TMemoryStream;
  inbuffer: Pointer;
  outbuffer: Pointer;
  count: longint;
  outcount: longint;
begin
  Result := False;
  InputStream := TMemoryStream.Create;

  try
    InputStream.LoadFromStream(SrcStream);
    count := InputStream.Size;
    getmem(inbuffer, count);
    InputStream.ReadBuffer(inbuffer^, count);
    zdecompress(inbuffer, count, outbuffer, outcount);
    SrcStream.Clear;
    SrcStream.Write(outbuffer^, outcount);
    Result := true;
  finally
    FreeAndNil(InputStream);
    FreeMem(inbuffer, count);
    FreeMem(outbuffer, outcount);
  end;
end;

class function TRDLib.GetAppVersionStr: string;
var
  Exe: string;
  Size, Handle: DWORD;
  Buffer: TBytes;
  FixedPtr: PVSFixedFileInfo;
begin
  Exe := ParamStr(0);
  Size := GetFileVersionInfoSize(PChar(Exe), Handle);
  if Size = 0 then
    RaiseLastOSError;
  SetLength(Buffer, Size);
  if not GetFileVersionInfo(PChar(Exe), Handle, Size, Buffer) then
    RaiseLastOSError;
  if not VerQueryValue(Buffer, '\', Pointer(FixedPtr), Size) then
    RaiseLastOSError;
  Result := Format('%d.%d.%d.%d', [LongRec(FixedPtr.dwFileVersionMS).Hi, // major
    LongRec(FixedPtr.dwFileVersionMS).Lo, // minor
    LongRec(FixedPtr.dwFileVersionLS).Hi, // release
    LongRec(FixedPtr.dwFileVersionLS).Lo]) // build
end;

class function TRDLib.GetSize(bytes: single): string;
begin
  if bytes < 1024 then
    Result := bytes.ToString + ' B'
  else if bytes < 1048576 then
    Result := FloatToStrF(bytes / 1024, ffFixed, 10, 1) + ' KB'
  else if bytes < 1073741824 then
    Result := FloatToStrF(bytes / 1048576, ffFixed, 10, 1) + ' MB'
  else if bytes > 1073741824 then
    Result := FloatToStrF(bytes / 1073741824, ffFixed, 10, 1) + ' GB';
end;

class function TRDLib.GetWallpaperDirectory: string;
var
  Reg: TRegistry;
begin
  Reg := nil;
  try
    Reg := TRegistry.Create;
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.OpenKey('Control Panel\Desktop', False);
    Result := Reg.ReadString('Wallpaper');
  finally
    FreeAndNil(Reg);
  end;
end;

class function TRDLib.ListFiles(FileName, Ext: string): string;
var
  SearchFile: TSearchRec;
  FindResult: Integer;
  Arc: TStrings;
begin
  Arc := TStringList.Create;
  FindResult := FindFirst(FileName + Ext, faArchive, SearchFile);
  try
    while FindResult = 0 do
    begin
      Application.ProcessMessages;
      Arc.Add(SearchFile.Name);
      FindResult := FindNext(SearchFile);
    end;
  finally
    FindClose(SearchFile);
  end;
  Result := Arc.Text;
end;

class function TRDLib.ListFolders(Directory: string): string;
var
  FileName: string;
  Filelist: string;
  Dirlist: string;
  Searchrec: TWin32FindData;
  FindHandle: THandle;
  ReturnStr: string;
begin
  ReturnStr := '';
  try
    FindHandle := FindFirstFile(PChar(Directory + '*.*'), Searchrec);
    if FindHandle <> INVALID_HANDLE_VALUE then
      repeat
        FileName := Searchrec.cFileName;
        if (FileName = '.') then
          Continue;
        if ((Searchrec.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0) then
        begin
          Dirlist := Dirlist + (FileName + #13);
        end
        else
        begin
          Filelist := Filelist + (FileName + #13);
        end;
      until FindNextFile(FindHandle, Searchrec) = False;
  finally
    Winapi.Windows.FindClose(FindHandle);
  end;
  ReturnStr := (Dirlist);
  Result := ReturnStr;
end;

class function TRDLib.MemoryStreamToString(M: TMemoryStream): AnsiString;
begin
  SetString(Result, PAnsiChar(M.Memory), M.Size);
end;

end.
