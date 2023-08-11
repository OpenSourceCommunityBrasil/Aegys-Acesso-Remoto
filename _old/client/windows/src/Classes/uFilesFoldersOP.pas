unit uFilesFoldersOP;

interface

uses
  System.Classes, Registry, ShellAPI, System.SysUtils, Winapi.Windows, VCL.Forms,
  Variants,
  System.Generics.Collections;

Type
  TFileProps = (fpDir, fpFile, fpShortcut, fpDriver);

Type
  TAfterChangeDir = Procedure of Object;

Type
  TFileDesc = Packed Record
    FileName, FileTypeDesc: AnsiString;
    FileType: TFileProps;
    FileSize: Int64;
    Creation, lastAccess, LastWrite: TDatetime;
  End;

Type
  TFileList = TList;

Type
  TShellProps = Class(TObject)
  Private
    vFileList: TFileList;
    vListFolder: String;
    vDriversList: TStringList;
    vAfterChangeDir: TAfterChangeDir;
    Function GetFile(Value: Integer): TFileDesc;
    Procedure SetFolder(Value: String);
    Function GetFileDateTime(FileName: String;
      Var Creation, lastAccess, LastWrite: TDatetime): Boolean;
    Procedure ListarArquivos(DiretorioInicial, Mascara: String;
      ListTotalDir: Boolean = False; Recursive: Boolean = False);
    Function GetFilesCount: Integer;
    Function GetFileTypeRegKey(FileName: String; out Key: HKEY): Integer;
    Function GetDrivers: TStringList;
    Procedure GetDriveLetters(AList: TStrings);
    Function GetPCName: String;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Property Files[I: Integer]: TFileDesc Read GetFile;
    Property FilesCount: Integer Read GetFilesCount;
  Published
    Property Folder: String Read vListFolder Write SetFolder;
    Property OnAfterChangeDir: TAfterChangeDir Read vAfterChangeDir
      Write vAfterChangeDir;
    Property Drivers: TStringList Read GetDrivers;
    Property LocalStation: String Read GetPCName;
  End;

  Function GetFileTypeDescription(FileName: String): AnsiString;

implementation

Function TShellProps.GetPCName: String;
Var
  Computer: PChar;
  CSize: DWORD;
Begin
  Computer := #0;
  Result := '';
  CSize := MAX_COMPUTERNAME_LENGTH + 1;
  Try
    GetMem(Computer, CSize);
    If GetComputerName(Computer, CSize) Then
      Result := Computer;
  Finally
    FreeMem(Computer);
  End;
End;

Function TShellProps.GetDrivers: TStringList;
Begin
  Result := TStringList.Create;
  GetDriveLetters(Result);
End;

Function GetFileTypeDescription(FileName: String): AnsiString;
Var
  SHFileInfo: TSHFileInfo;
  SearchRec: TSearchRec;
Begin
  Result := '';
  If Trim(FileName) = '' Then
    Exit;
  ShGetFileInfo(PChar(FileName), 0, SHFileInfo, SizeOf(TSHFileInfo),
    SHGFI_TYPENAME or SHGFI_DISPLAYNAME or SHGFI_SYSICONINDEX or SHGFI_ICON);
  Result := SHFileInfo.szTypeName;
End;

Function TShellProps.GetFileTypeRegKey(FileName: String; out Key: HKEY)
  : Integer;
Var
  hkExt: HKEY;
  lpszTypeKey: Array [0 .. 1024] of Char;
  dwSize: DWORD;
  szExt: String;
Begin
  // Get the file extension
  szExt := ExtractFileExt(FileName);
  // Attempt to open the registry key
  Result := RegOpenKeyEx(HKEY_CLASSES_ROOT, PChar(szExt), 0, KEY_READ, hkExt);
  // Check result of open
  If (Result = ERROR_SUCCESS) Then
  Begin
    // Resource protection
    Try
      // Set size of buffer
      dwSize := SizeOf(lpszTypeKey);
      // Query for the default value which is the redirect to the extension type key
      Result := RegQueryValueEx(hkExt, nil, nil, nil, @lpszTypeKey, @dwSize);
      // Check result
      If (Result = ERROR_SUCCESS) Then
      Begin
        // Open the redirect to the extension type key
        Result := RegOpenKeyEx(HKEY_CLASSES_ROOT, @lpszTypeKey, 0,
          KEY_READ, Key);
      End;
    Finally
      // Close the opened key
      RegCloseKey(hkExt);
    End;
  End;
End;

Function FileSize(Const aFilename: String): Int64;
Var
  info: TWin32FileAttributeData;
Begin
  Result := -1;
  If Not GetFileAttributesEx(PWideChar(aFilename), GetFileExInfoStandard,
    @info) Then
    Exit;
  Result := Int64(info.nFileSizeLow) or Int64(info.nFileSizeHigh shl 32);
End;

Procedure TShellProps.GetDriveLetters(AList: TStrings);
Var
  vDrivesSize: Cardinal;
  vDrives: Array [0 .. 128] of Char;
  vDrive: PChar;
Begin
  AList.BeginUpdate;
  Try
    // clear the list from possible leftover from prior operations
    AList.Clear;
    Try
      vDrivesSize := GetLogicalDriveStrings(SizeOf(vDrives), vDrives);
      If vDrivesSize = 0 Then
        Exit; // no drive found, no further processing needed
      vDrive := vDrives;
      While vDrive^ <> #0 Do
      Begin
        AList.Add(StrPas(vDrive));
        Inc(vDrive, SizeOf(vDrive));
      End;
    Except

    End;
  Finally
    AList.EndUpdate;
  End;
End;

Function TShellProps.GetFilesCount: Integer;
Begin
  Result := vFileList.Count;
End;

Function TShellProps.GetFileDateTime(FileName: String;
  Var Creation, lastAccess, LastWrite: TDatetime): Boolean;
Var
  Handle: THandle;
  FindData: TWin32FindData;
  LocalFileTime: TFileTime;
  LikeInt: Integer;
Begin
  Handle := FindFirstFile(PChar(FileName), FindData);
  If Handle <> INVALID_HANDLE_VALUE Then
  Begin
    FindClose(Handle);
    If (FindData.dwFileAttributes And FILE_ATTRIBUTE_DIRECTORY) = 0 Then
    Begin
      FileTimeToLocalFileTime(FindData.ftCreaTionTime, LocalFileTime);
      If FileTimeToDosDateTime(LocalFileTime, LongRec(LikeInt).Hi,
        LongRec(LikeInt).Lo) Then
        Creation := FileDatetoDateTime(LikeInt);
      FileTimeToLocalFileTime(FindData.ftLastAccessTime, LocalFileTime);
      If FileTimeToDosDateTime(LocalFileTime, LongRec(LikeInt).Hi,
        LongRec(LikeInt).Lo) Then
        lastAccess := FileDatetoDateTime(LikeInt);
      FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
      If FileTimeToDosDateTime(LocalFileTime, LongRec(LikeInt).Hi,
        LongRec(LikeInt).Lo) Then
        LastWrite := FileDatetoDateTime(LikeInt);
      Result := True;
      Exit;
    End;
  End;
  Result := False;
End;

Procedure TShellProps.ListarArquivos(DiretorioInicial, Mascara: String;
  ListTotalDir: Boolean = False; Recursive: Boolean = False);
Var
  Attributes, I: Integer;
  listatemp: TStrings;
  FileDesc: ^TFileDesc;
  Procedure ListarDiretorios(Folder: string; lista: TStrings);
  Var
    Rec: TSearchRec;
    I: Integer;
    temps: String;
  Begin
    lista.Clear;
    If FindFirst(Folder + '*', faDirectory, Rec) = 0 Then
      Try
        Repeat
          lista.Add(Rec.Name);
        Until FindNext(Rec) <> 0;
      Finally
        If lista.Count <> 0 Then
        Begin
          // deleta o diretorio ..
          lista.Delete(1);
          // deleta o diretorio .
          lista.Delete(0);
          I := 0;
          // deleta os arquivos isto e fica apenas os diretorios
          If lista.Count <> 0 Then
          Begin
            Repeat
              temps := lista.Strings[I];
              temps := ExtractFileExt(temps);
              If temps <> '' Then
                lista.Delete(I)
              Else
                Inc(I);
            Until I >= lista.Count;
          End;
        End;
      End;
  End;
  Procedure ListarDiretoriosP(Directory: String; ReturnData: TStrings);
  Var
    Rec: TSearchRec;
  Begin
    ReturnData.Clear;
    If FindFirst(Directory + '*', faDirectory, Rec) = 0 Then
      Try
        Repeat
          If (Rec.Name <> '.rnd') And (ExtractFileExt(Rec.Name) = '') Then
            ReturnData.Add(Rec.Name);
        Until FindNext(Rec) <> 0;
      Finally
        System.SysUtils.FindClose(Rec);
      End;
  End;
  Procedure ListarArquivos(Folder: String; lista: TStrings);
  Var
    Rec: TSearchRec;
  Begin
    If FindFirst(IncludeTrailingPathDelimiter(Folder) + '*.*', faArchive, Rec) = 0 Then
      Try
        Repeat
          If lista.IndexOf(Rec.Name) = -1 Then
            lista.Add(Rec.Name);
        Until FindNext(Rec) <> 0;
      Finally
        System.SysUtils.FindClose(Rec);
      End;
  End;
  Procedure ListarAtalhos(Folder, mask: String; lista: TStrings);
  Var
    Rec: TSearchRec;
  Begin
    lista.Clear;
    If FindFirst(Folder + mask, faAnyFile, Rec) = 0 Then
      Try
        Repeat
          lista.Add(Rec.Name);
        Until FindNext(Rec) <> 0;
      Finally
        System.SysUtils.FindClose(Rec);
      End;
  End;
  Procedure AddListInOther(ListSource, ListDestino: TStrings);
  Var
    f: Integer;
  Begin
    For f := 0 To ListSource.Count - 1 Do
      ListDestino.Add(ListSource.Strings[f]);
  End;

Begin
  listatemp := TStringList.Create;
  {
    If recursive Then
    Begin
    ListarDiretorios(diretorioInicial, listatemp);
    For i := 0 To listatemp.Count - 1 Do
    ListarArquivos(diretorioInicial + listatemp.Strings[i] + '', mascara, listtotaldir, recursive);
    End;
  }
  Try
   ListarDiretoriosP(DiretorioInicial, listatemp);
  Except
  End;
  Try
   ListarArquivos(DiretorioInicial, listatemp);
  Except
  End;
  {
    ListarAtalhos(diretorioInicial, mascara, listatemp);
    If listtotaldir Then
    Begin
    For i := 0 To listatemp.Count - 1 Do
    listatemp.Strings[i] := diretorioInicial + listatemp.Strings[i];
    End;
  }
  vFileList.Clear;
  For I := 0 To listatemp.Count - 1 Do
  Begin
    Attributes := FileGetAttr(IncludeTrailingPathDelimiter(DiretorioInicial) + listatemp[I]);
    If (Attributes <> faSysFile) And (Attributes <> faDirectory) And
      (Attributes <> faArchive) And (Attributes <> faSymLink) Then
      Continue;
    New(FileDesc);
    FileDesc^.FileName := listatemp[I];
    Case Attributes Of
      faSysFile, faDirectory:
        FileDesc^.FileType := fpDir;
      faArchive:
        FileDesc^.FileType := fpFile;
      faSymLink:
        FileDesc^.FileType := fpShortcut;
    Else
      FileDesc^.FileType := fpDriver;
    End;
    FileDesc^.FileSize := FileSize(DiretorioInicial + FileDesc^.FileName);
    FileDesc^.FileTypeDesc := GetFileTypeDescription
      (DiretorioInicial + FileDesc^.FileName);
    FileDesc^.FileTypeDesc :=
      StringReplace(StringReplace(FileDesc^.FileTypeDesc, #0, '', [rfReplaceAll]
      ), '?', '', [rfReplaceAll]);
    If FileDesc^.FileTypeDesc = '' Then
      FileDesc^.FileTypeDesc := 'Tipo desconhecido';
    GetFileDateTime(DiretorioInicial + FileDesc^.FileName, FileDesc^.Creation,
      FileDesc^.lastAccess, FileDesc^.LastWrite);
    vFileList.Add(FileDesc);
  End;
  FreeAndNil(listatemp);
  If Assigned(vAfterChangeDir) Then
    vAfterChangeDir;
End;

Procedure TShellProps.SetFolder(Value: String);
Begin
 Try
  vListFolder := IncludeTrailingPathDelimiter(Value);
  ListarArquivos(vListFolder, '*.*');
 Except
  vListFolder := '';
 End;
End;

Function TShellProps.GetFile(Value: Integer): TFileDesc;
Begin
  If (Value > -1) And (Value <= vFileList.Count - 1) Then
    Result := TFileDesc(vFileList[Value]^);
End;

Constructor TShellProps.Create;
Begin
  vFileList := TFileList.Create;
  vDriversList := TStringList.Create;
  GetDriveLetters(vDriversList);
End;

Destructor TShellProps.Destroy;
Begin
  vFileList.Clear;
  FreeAndNil(vFileList);
  vDriversList.Clear;
  FreeAndNil(vDriversList);
End;

end.
