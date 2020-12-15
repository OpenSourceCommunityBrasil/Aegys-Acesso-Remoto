unit uIconsAssoc;

interface

uses
  Registry, ShellAPI, System.SysUtils, Winapi.Windows, Forms, Variants;

Type
  PHICON = ^HICON;

Procedure GetAssociatedIcon(FileName: TFilename;
  PLargeIcon, PSmallIcon: PHICON); Overload;
Procedure GetAssociatedIcon(FileName: TFilename; PSmallIcon: PHICON); Overload;

implementation

Procedure GetAssociatedIcon(FileName: TFilename;
  PLargeIcon, PSmallIcon: PHICON);
Var
  IconIndex: SmallInt; // Position of the icon in the file
  Icono: PHICON; // The LargeIcon parameter of ExtractIconEx
  FileExt, FileType: String;
  Reg: TRegistry;
  p: Integer;
  p1, p2: PChar;
  buffer: Array [0 .. 255] of Char;
Label
  noassoc,
  NoSHELL; // ugly! but I use it, to not modify to much the original code :(
Begin
  If Assigned(PLargeIcon) Then
    PLargeIcon^ := 0;
  If Assigned(PSmallIcon) then
    PSmallIcon^ := 0;
  IconIndex := 0;
  Icono := Nil;
  // Get the extension of the file
  FileExt := UpperCase(ExtractFileExt(FileName));
  If ((FileExt <> '.EXE') And (FileExt <> '.ICO')) Or
    Not FileExists(FileName) Then
  Begin
    // If the file is an EXE or ICO and exists, then we can
    // extract the icon from that file. Otherwise here we try
    // to find the icon in the Windows Registry.
    Reg := nil;
    Try
      Reg := TRegistry.Create;
      Reg.RootKey := HKEY_CLASSES_ROOT;
      If FileExt = '.EXE' Then
        FileExt := '.COM';
      If Reg.OpenKeyReadOnly(FileExt) Then
      Begin
        Try
          FileType := Reg.ReadString('');
        Finally
          Reg.CloseKey;
        End;
      End;
      If (FileType <> '') And Reg.OpenKeyReadOnly
        (FileType + '\DefaultIcon') Then
      Begin
        Try
          FileName := Reg.ReadString('');
        Finally
          Reg.CloseKey;
        End;
      End;
    Finally
      Reg.Free;
    End;
    // If there is not association then lets try to
    // get the default icon
    If FileName = '' Then
      Goto noassoc;
    // Get file name and icon index from the association
    // ('"File\Name",IconIndex')
    p1 := PChar(FileName);
    p2 := StrRScan(p1, ',');
    If (p2 <> Nil) Then
    Begin
      p := p2 - p1 + 1; // Position de la coma
      IconIndex := StrToInt(Copy(FileName, p + 1, Length(FileName) - p));
      SetLength(FileName, p - 1);
    End;
  End; // if ((FileExt  '.EX ...
  // Try to extract the small icon
  If ExtractIconEx(PChar(FileName), IconIndex, Icono^, PSmallIcon^, 1) <> 1 Then
  Begin
  noassoc:
    // That code is executed only if the ExtractIconEx return a value but 1
    // There is not associated icon
    // try to get the default icon from SHELL32.DLL
    FileName := 'C:\Windows\System\SHELL32.DLL';
    If Not FileExists(FileName) Then
    Begin // If SHELL32.DLL is not in Windows\System then
      GetWindowsDirectory(buffer, SizeOf(buffer));
      // Search in the current directory and in the windows directory
      FileName := FileSearch('SHELL32.DLL', GetCurrentDir + ';' + buffer);
      If FileName = '' Then
        Goto NoSHELL; // the file SHELL32.DLL is not in the system
    End;
    // Determine the default icon for the file extension
    If (FileExt = '.DOC') Then
      IconIndex := 1
    Else If (FileExt = '.EXE') Or (FileExt = '.COM') Then
      IconIndex := 2
    Else If (FileExt = '.HLP') Then
      IconIndex := 23
    Else If (FileExt = '.INI') Or (FileExt = '.INF') Then
      IconIndex := 63
    Else If (FileExt = '.TXT') Then
      IconIndex := 64
    Else If (FileExt = '.BAT') Then
      IconIndex := 65
    Else If (FileExt = '.DLL') Or (FileExt = '.SYS') Or (FileExt = '.VBX') Or
      (FileExt = '.OCX') Or (FileExt = '.VXD') Then
      IconIndex := 66
    Else If (FileExt = '.FON') Then
      IconIndex := 67
    Else If (FileExt = '.TTF') Then
      IconIndex := 68
    Else If (FileExt = '.FOT') Then
      IconIndex := 69
    Else
      IconIndex := 0;
    // Try to extract the small icon
    If ExtractIconEx(PChar(FileName), IconIndex, Icono^, PSmallIcon^, 1)
      <> 1 Then
    Begin
      // That code is executed only if the ExtractIconEx return a value but 1
      // Fallo encontrar el icono. Solo "regresar" ceros.
    NoSHELL:
      If PLargeIcon = Nil Then
        PLargeIcon^ := 0;
      If PSmallIcon = Nil Then
        PSmallIcon^ := 0;
    End;
  End; // if ExtractIconEx
  If PSmallIcon^ = 0 Then
  Begin // If there is an small icon then extract the large icon.
    If Assigned(PLargeIcon) Then
    Begin
      PLargeIcon^ := ExtractIcon(Application.Handle, PChar(FileName),
        IconIndex);
      If PLargeIcon^ = Null Then
        PLargeIcon^ := 0;
    End;
  End;
End;

Procedure GetAssociatedIcon(FileName: TFilename; PSmallIcon: PHICON);
Var
  IconIndex: SmallInt;
  Icono: PHICON; // The LargeIcon parameter of ExtractIconEx
  FileExt, FileType: String;
  Reg: TRegistry;
  p: Integer;
  p1, p2: PChar;
  buffer: Array [0 .. 255] of Char;
Label
  noassoc,
  NoSHELL; // ugly! but I use it, to not modify to much the original code :(
Begin
  If Assigned(PSmallIcon) then
    PSmallIcon^ := 0;
  IconIndex := 0;
  Icono := Nil;
  // Get the extension of the file
  FileExt := UpperCase(ExtractFileExt(FileName));
  If ((FileExt <> '.EXE') And (FileExt <> '.ICO')) Or
    Not FileExists(FileName) Then
  Begin
    // If the file is an EXE or ICO and exists, then we can
    // extract the icon from that file. Otherwise here we try
    // to find the icon in the Windows Registry.
    Reg := nil;
    Try
      Reg := TRegistry.Create;
      Reg.RootKey := HKEY_CLASSES_ROOT;
      If FileExt = '.EXE' Then
        FileExt := '.COM';
      If Reg.OpenKeyReadOnly(FileExt) Then
      Begin
        Try
          FileType := Reg.ReadString('');
        Finally
          Reg.CloseKey;
        End;
      End;
      If (FileType <> '') And Reg.OpenKeyReadOnly
        (FileType + '\DefaultIcon') Then
      Begin
        Try
          FileName := Reg.ReadString('');
        Finally
          Reg.CloseKey;
        End;
      End;
    Finally
      Reg.Free;
    End;
    // If there is not association then lets try to
    // get the default icon
    If FileName = '' Then
      Goto noassoc;
    // Get file name and icon index from the association
    // ('"File\Name",IconIndex')
    p1 := PChar(FileName);
    p2 := StrRScan(p1, ',');
    If (p2 <> Nil) Then
    Begin
      p := p2 - p1 + 1; // Position de la coma
      IconIndex := StrToInt(Copy(FileName, p + 1, Length(FileName) - p));
      SetLength(FileName, p - 1);
    End;
  End; // if ((FileExt  '.EX ...
  // Try to extract the small icon
  If ExtractIconEx(PChar(FileName), IconIndex, Icono^, PSmallIcon^, 1) <> 1 Then
  Begin
  noassoc:
    // That code is executed only if the ExtractIconEx return a value but 1
    // There is not associated icon
    // try to get the default icon from SHELL32.DLL
    FileName := 'C:\Windows\System\SHELL32.DLL';
    If Not FileExists(FileName) Then
    Begin // If SHELL32.DLL is not in Windows\System then
      GetWindowsDirectory(buffer, SizeOf(buffer));
      // Search in the current directory and in the windows directory
      FileName := FileSearch('SHELL32.DLL', GetCurrentDir + ';' + buffer);
      If FileName = '' Then
        Goto NoSHELL; // the file SHELL32.DLL is not in the system
    End;
    // Determine the default icon for the file extension
    If (FileExt = '.DOC') Then
      IconIndex := 1
    Else If (FileExt = '.EXE') Or (FileExt = '.COM') Then
      IconIndex := 2
    Else If (FileExt = '.HLP') Then
      IconIndex := 23
    Else If (FileExt = '.INI') Or (FileExt = '.INF') Then
      IconIndex := 63
    Else If (FileExt = '.TXT') Then
      IconIndex := 64
    Else If (FileExt = '.BAT') Then
      IconIndex := 65
    Else If (FileExt = '.DLL') Or (FileExt = '.SYS') Or (FileExt = '.VBX') Or
      (FileExt = '.OCX') Or (FileExt = '.VXD') Then
      IconIndex := 66
    Else If (FileExt = '.FON') Then
      IconIndex := 67
    Else If (FileExt = '.TTF') Then
      IconIndex := 68
    Else If (FileExt = '.FOT') Then
      IconIndex := 69
    Else
      IconIndex := 0;
    // Try to extract the small icon
    If ExtractIconEx(PChar(FileName), IconIndex, Icono^, PSmallIcon^, 1)
      <> 1 Then
    Begin
      // That code is executed only if the ExtractIconEx return a value but 1
      // Fallo encontrar el icono. Solo "regresar" ceros.
    NoSHELL:
      If PSmallIcon = Nil Then
        PSmallIcon^ := 0;
    End;
  End; // if ExtractIconEx
End;

end.
