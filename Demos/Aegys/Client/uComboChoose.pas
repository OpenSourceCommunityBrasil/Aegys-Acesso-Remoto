unit uComboChoose;

interface

uses
  System.SysUtils, System.Variants, System.Generics.Collections, System.Classes;

Type
  TComboChooseRec = Packed Record
    Combo: String[10];
    LastAcess: Boolean;
  End;

Type
  TComboOptions = TList;

Type
  TComboList = Class(TComponent)
  Private
    vFileName: String;
    vElements: TStringList;
    vComboOptions: TComboOptions;
    vComboChooseFile: File of TComboChooseRec;
    Procedure CleanComboOptions;
    Procedure DeleteComboOption(Value: String);
    Procedure RebuildList(Value: String = '');
    Procedure OpenDat;
    Procedure ReadList;
    Procedure FileClose;
    Function NodeExists(Value: String): Boolean;
  Public
    Procedure AddElement(Value: String; Rebuild: Boolean = True;
      LastAccess: Boolean = False);
    Procedure ClearList;
    Procedure DeleteElement(Value: String);
    Function GetLastAccess: String;
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    Property Elements: TStringList Read vElements;
    Property FileName: String Read vFileName Write vFileName;
  End;

implementation

Function TComboList.NodeExists(Value: String): Boolean;
Var
  I: Integer;
Begin
  Result := False;
  For I := vComboOptions.Count - 1 DownTo 0 Do
  Begin
    Result := Trim(TComboChooseRec(vComboOptions[I]^).Combo) = Trim(Value);
    If Result Then
      Break;
  End;
End;

Procedure TComboList.ReadList;
Var
  vValue: TComboChooseRec;
Begin
  Try
    Reset(vComboChooseFile);
    Seek(vComboChooseFile, 0);
    While Not EOF(vComboChooseFile) Do
    Begin
      Read(vComboChooseFile, vValue);
      AddElement(vValue.Combo, False, vValue.LastAcess);
    End;
  Except
    RebuildList;
  End;
End;

Procedure TComboList.FileClose;
Begin
  RebuildList;
  CloseFile(vComboChooseFile);
End;

Procedure TComboList.OpenDat;
Begin
  Assignfile(vComboChooseFile, vFileName);
  Try
    Filemode := 2;
    If Not FileExists(vFileName) Then
      Rewrite(vComboChooseFile)
    Else
    Begin
      ReadList;
      RebuildList;
    End;
  Except

  End;
End;

Procedure TComboList.RebuildList(Value: String = '');
Var
  I: Integer;
  vValue: TComboChooseRec;
Begin
  Try
    Seek(vComboChooseFile, 0);
    Rewrite(vComboChooseFile);
    vElements.Clear;
    For I := 0 to vComboOptions.Count - 1 Do
    Begin
      vValue.Combo := TComboChooseRec(vComboOptions[I]^).Combo;
      vValue.LastAcess := TComboChooseRec(vComboOptions[I]^).LastAcess;
      If Value <> '' Then
      Begin
        TComboChooseRec(vComboOptions[I]^).LastAcess :=
          Trim(TComboChooseRec(vComboOptions[I]^).Combo) = Trim(Value);
        vValue.LastAcess := TComboChooseRec(vComboOptions[I]^).LastAcess;
      End;
      Write(vComboChooseFile, vValue);
      vElements.Add(vValue.Combo);
    End;
  Except
  End;
End;

Procedure TComboList.DeleteComboOption(Value: String);
Begin
  RebuildList;
End;

Procedure TComboList.CleanComboOptions;
Var
  I: Integer;
Begin
  If vComboOptions <> Nil Then
  Begin
    For I := vComboOptions.Count - 1 DownTo 0 Do
    Begin
      Dispose(vComboOptions[I]);
      vComboOptions.Delete(I);
      vComboOptions.Capacity := vComboOptions.Count;
      vComboOptions.Pack;
    End;
    // RebuildList;
  End;
End;

Function TComboList.GetLastAccess: String;
Var
  I: Integer;
Begin
  Result := '';
  For I := 0 to vComboOptions.Count - 1 Do
  Begin
    If TComboChooseRec(vComboOptions[I]^).LastAcess Then
    Begin
      Result := TComboChooseRec(vComboOptions[I]^).Combo;
      Break;
    End;
  End;
End;

Procedure TComboList.AddElement(Value: String; Rebuild: Boolean = True;
  LastAccess: Boolean = False);
Var
  vComboChooseRec: ^TComboChooseRec;
Begin
  If Not NodeExists(Value) Then
  Begin
    New(vComboChooseRec);
    vComboChooseRec^.Combo := Value;
    vComboChooseRec^.LastAcess := LastAccess;
    vComboOptions.Add(vComboChooseRec);
  End;
  If Rebuild Then
    RebuildList(Value);
End;

Procedure TComboList.ClearList;
Begin

  CleanComboOptions;
End;

Procedure TComboList.DeleteElement(Value: String);
Begin
  RebuildList;
End;

Destructor TComboList.Destroy;
Begin
  FileClose;
  CleanComboOptions;
  FreeAndNil(vElements);
  FreeAndNil(vComboOptions);
  Inherited;
End;

Constructor TComboList.Create(AOwner: TComponent);
Begin
  Inherited;
  vFileName := ExtractFilePath(ParamStr(0)) + 'Aegys.lst';
  vElements := TStringList.Create;
  vComboOptions := TComboOptions.Create;
  OpenDat;
End;

end.
