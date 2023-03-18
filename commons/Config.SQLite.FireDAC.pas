unit Config.SQLite.FireDAC;

interface

uses
  {$IF CompilerVersion > 33.0}
  FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.Intf, FireDAC.Phys, FireDAC.Phys.SQLite,
  {$IFEND}
  {$IFDEF Android}
  System.IOUtils,
  {$ENDIF}
  FireDAC.Comp.Client, System.JSON, System.SysUtils

    ;

type
  TSQLiteConfig = class
  private
    FConn: TFDConnection;
    FDataSet: TFDQuery;
    procedure Validate;
  public
    constructor Create;
    destructor Destroy; override;
    function getValue(pKey: string): string;
    procedure UpdateConfig(aJSON: TJSONObject); overload;
    procedure UpdateConfig(aKey, aValue: string); overload;
    function LoadConfig: TJSONObject;
  end;

var
  aCFG: TSQLiteConfig;

implementation

{ TSQLiteConfig }

constructor TSQLiteConfig.Create;
begin
  FConn := TFDConnection.Create(nil);
  FConn.Params.Clear;
  FConn.DriverName := 'SQLite';
  {$IFDEF Android}
  FConn.Params.Add('Database=' + TPath.Combine(TPath.GetDocumentsPath,
    'config.db'));
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  FConn.Params.Add('Database=' + ExtractFilePath(ParamStr(0)) + 'config.db');
  {$ENDIF}
  FConn.Params.Add('LockingMode=normal');

  FDataSet := TFDQuery.Create(nil);
  FDataSet.Connection := FConn;
  FDataSet.ResourceOptions.SilentMode := true;

  Validate;
end;

destructor TSQLiteConfig.Destroy;
begin
  FDataSet.Free;
  FConn.Free;
  inherited;
end;

function TSQLiteConfig.getValue(pKey: string): string;
begin
  Result := '';
  with FDataSet do
  begin
    Close;
    SQL.Clear;
    SQL.Add('SELECT CFG_Value');
    SQL.Add('  FROM Config');
    SQL.Add(' WHERE CFG_Key = :CFG_Key');
    ParamByName('CFG_Key').Value := pKey;
    Open;
    Result := Fields.Fields[0].AsString;
    Close;
  end;
end;

function TSQLiteConfig.LoadConfig: TJSONObject;
begin
  Result := TJSONObject.Create;
  with FDataSet do
  begin
    Close;
    SQL.Clear;
    SQL.Add('SELECT CFG_Key, CFG_Value');
    SQL.Add('  FROM Config');
    Open;
    while not Eof do
    begin
      Result.AddPair(Fields.Fields[0].AsString, Fields.Fields[1].AsString);
      Next;
    end;
    Close;
  end;
end;

procedure TSQLiteConfig.UpdateConfig(aJSON: TJSONObject);
var
  JSONVal: TJSONValue;
  i: integer;
begin
  // exemplo entrada
  // {"key1":"value1", "key2":"value2", "key3":"value3", "key4":"value4", "key5":"value5"}
  // aJSON.Pairs[i].JSONString.tostring = "key1",
  // aJSON.Pairs[i].JSONValue.tostring = "value1";
  for i := 0 to aJSON.Count - 1 do
    with FDataSet do
    begin
      Close;
      SQL.Clear;
      SQL.Add('SELECT CFG_Key, CFG_Value');
      SQL.Add('  FROM Config');
      SQL.Add(' WHERE CFG_Key = :CFG_Key');
      ParamByName('CFG_Key').Value := aJSON.Pairs[i].JsonString.ToString.Replace
        ('"', '', [rfReplaceAll]);
      Open;
      Edit;
      Fields.Fields[0].Value := aJSON.Pairs[i].JsonString.ToString.Replace('"',
        '', [rfReplaceAll]);
      Fields.Fields[1].Value := aJSON.Pairs[i].JsonValue.ToString.Replace('"',
        '', [rfReplaceAll]);
      Post;
      if FDataSet.CachedUpdates then
        ApplyUpdates;
      Close;
    end;
end;

procedure TSQLiteConfig.UpdateConfig(aKey, aValue: string);
begin
  with FDataSet do
  begin
    Close;
    SQL.Clear;
    SQL.Add('SELECT CFG_Key, CFG_Value');
    SQL.Add('  FROM Config');
    SQL.Add(' WHERE CFG_Key = :CFG_Key');
    ParamByName('CFG_Key').Value := aKey;
    Open;
    Edit;
    Fields.Fields[0].Value := aKey;
    Fields.Fields[1].Value := aValue;
    Post;
    if FDataSet.CachedUpdates then
      ApplyUpdates;
    Close;
  end;
end;

procedure TSQLiteConfig.Validate;
begin
  with FDataSet do
  begin
    Close;
    Open('PRAGMA table_info("Config")');
    if isEmpty then
    begin
      Close;
      SQL.Clear;
      SQL.Add('CREATE TABLE Config(');
      SQL.Add('  CFG_ID integer primary key');
      SQL.Add(', CFG_Key varchar');
      SQL.Add(', CFG_Value varchar');
      SQL.Add(');');
      ExecSQL;
    end;
  end;
end;

end.
