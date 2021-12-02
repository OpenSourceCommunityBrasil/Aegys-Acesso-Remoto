unit uSQLiteConfig;

interface

uses
  FireDAC.Comp.Client,
{$IF CompilerVersion > 33}
  FireDAC.phys.SQLite,
{$ENDIF}
{$IF CompilerVersion > 34}
  FireDAC.Stan.Def, FireDAC.DApt, FireDAC.FMXUI.Wait, FireDAC.Stan.Async,
{$ENDIF}
  System.JSON, System.SysUtils,
  uConstants;

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
    procedure UpdateConfig(aJSON: TJSONObject);
    function LoadConfig: TJSONObject;
  end;

var
  aCFG: TSQLiteConfig;

implementation

{ TCfg }

constructor TSQLiteConfig.Create;
begin
  FConn := TFDConnection.Create(nil);
  FConn.Params.Clear;
  FConn.DriverName := 'SQLite';
  FConn.Params.Add('Database=' + ExtractFilePath(ParamStr(0)) + 'config.db');
  FConn.Params.Add('LockingMode=normal');

  FDataSet := TFDQuery.Create(nil);
  FDataSet.Connection := FConn;

  Validate;
end;

destructor TSQLiteConfig.Destroy;
begin
  FDataSet.DisposeOf;
  FConn.DisposeOf;
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

procedure TSQLiteConfig.Validate;
begin
  with FDataSet do
  begin
    Close;
{$IF CompilerVersion > 34.0}
    try
      Open('PRAGMA table_info("Config")');
    except
      ExecSQL('PRAGMA table_info("Config")');
    end;
{$ELSE}
    SQL.Text := 'PRAGMA table_info("Config")';
    OpenOrExecute;
{$ENDIF}
    if IsEmpty then
    begin
      Close;
      SQL.Clear;
      SQL.Add('CREATE TABLE "Config"(');
      SQL.Add('  "CFG_ID" integer primary key');
      SQL.Add(', "CFG_Key" varchar');
      SQL.Add(', "CFG_Value" varchar');
      SQL.Add(');');
      ExecSQL;
    end;
  end;
end;

end.
