unit uSQLiteConfig;

interface

uses
  FireDAC.Comp.Client,
  FireDAC.phys.SQLite,
  FireDAC.Stan.Def, FireDAC.DApt, FireDAC.FMXUI.Wait, FireDAC.Stan.Async,
  FireDAC.Stan.StorageJSON, FireDAC.Stan.StorageBin,FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.Intf, FireDAC.Phys,
  System.JSON, System.SysUtils,
  uConstants;

 Type
  TSQLiteConfig = Class
  Private
   FConn      : TFDConnection;
   Procedure  Validate;
  Public
   Constructor Create;
   Destructor  Destroy; Override;
   Function    getValue    (pKey  : String) : String;
   Procedure   UpdateConfig(aJSON : TJSONObject);
   Function    LoadConfig         : TJSONObject;
  End;

Var
 DriverLink : TFDPhysSQLiteDriverLink;

Implementation

{ TCfg }

Constructor TSQLiteConfig.Create;
Begin
 FConn            := TFDConnection.Create(Nil);
 FConn.Params.Clear;
 FConn.DriverName := 'SQLite';
 FConn.Params.Add('Database=' + ExtractFilePath(ParamStr(0)) + 'config.db');
 FConn.Params.Add('LockingMode=normal');
 Validate;
End;

Destructor TSQLiteConfig.Destroy;
Begin
 FreeAndNil(FConn);
 Inherited;
End;

Function TSQLiteConfig.getValue(pKey : String) : String;
Var
 FDataSet : TFDQuery;
Begin
 Result := '';
 FDataSet            := TFDQuery.Create(Nil);
 FDataSet.Connection := FConn;
 Try
  With FDataSet do
   Begin
    Close;
    SQL.Clear;
    SQL.Add('SELECT CFG_Value');
    SQL.Add('  FROM Config');
    SQL.Add(' WHERE CFG_Key = :CFG_Key');
    ParamByName('CFG_Key').Value := pKey;
    Open;
    Result := Fields.Fields[0].AsString;
    Close;
   End;
 Finally
  FreeAndNil(FDataSet);
 End;
End;

Function TSQLiteConfig.LoadConfig: TJSONObject;
Var
 FDataSet : TFDQuery;
Begin
 FDataSet            := TFDQuery.Create(Nil);
 FDataSet.Connection := FConn;
 Try
  Result := TJSONObject.Create;
  With FDataSet Do
   Begin
    Close;
    SQL.Clear;
    SQL.Add('SELECT CFG_Key, CFG_Value');
    SQL.Add('  FROM Config');
    Open;
    While Not Eof Do
     Begin
      Result.AddPair(Fields.Fields[0].AsString, Fields.Fields[1].AsString);
      Next;
     End;
    Close;
   End;
 Finally
  FreeAndNil(FDataSet);
 End;
End;

Procedure TSQLiteConfig.UpdateConfig(aJSON: TJSONObject);
Var
 FDataSet : TFDQuery;
 JSONVal  : TJSONValue;
 i        : Integer;
Begin
 FDataSet            := TFDQuery.Create(Nil);
 FDataSet.Connection := FConn;
 Try
  For i := 0 To aJSON.Count -1 Do
   Begin
    With FDataSet do
     Begin
      Close;
      SQL.Clear;
      SQL.Add('SELECT CFG_Key, CFG_Value');
      SQL.Add('  FROM Config');
      SQL.Add(' WHERE CFG_Key = :CFG_Key');
      ParamByName('CFG_Key').Value := aJSON.Pairs[i].JsonString.ToString.Replace('"', '', [rfReplaceAll]);
      Open;
      Edit;
      Fields.Fields[0].Value := aJSON.Pairs[i].JsonString.ToString.Replace('"', '', [rfReplaceAll]);
      Fields.Fields[1].Value := aJSON.Pairs[i].JsonValue.ToString.Replace ('"', '', [rfReplaceAll]);
      Post;
      If FDataSet.CachedUpdates Then
       ApplyUpdates;
      Close;
     End;
   End;
 Finally
  FreeAndNil(FDataSet);
 End;
End;

Procedure TSQLiteConfig.Validate;
Var
 FDataSet : TFDQuery;
Begin
 FDataSet            := TFDQuery.Create(Nil);
 FDataSet.Connection := FConn;
 Try
  With FDataSet do
   Begin
    Close;
    Try
     Open('PRAGMA table_info("Config")');
    Except
     ExecSQL('PRAGMA table_info("Config")');
    End;
    If IsEmpty then
     Begin
      Close;
      SQL.Clear;
      SQL.Add('CREATE TABLE "Config"(');
      SQL.Add('  "CFG_ID" integer primary key');
      SQL.Add(', "CFG_Key" varchar');
      SQL.Add(', "CFG_Value" varchar');
      SQL.Add(');');
      ExecSQL;
     End;
   End;
 Finally
  FreeAndNil(FDataSet);
 End;
End;

Initialization
 DriverLink := TFDPhysSQLiteDriverLink.Create(Nil);

Finalization
 FreeAndNil(DriverLink);
End.
