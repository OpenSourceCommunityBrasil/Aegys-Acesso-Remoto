unit uLocaleFunctions;

interface

type
  TLocale = class
  private
  public
    function GetLocale(section, variable: string): string;
    function GetLocaleDlg(section, variable: string): PWideChar;
  end;

var
  Locale: TLocale;

implementation

uses
  IniFiles, SysUtils;

{ TLocale }

function TLocale.GetLocale(section, variable: string): string;
var
  Locale: TIniFile;
begin
{$IFDEF Debug}
  Locale := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'locale.ini');
{$ELSE}
  Locale := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'locale.dat');
{$IFEND}
  try
    Result := Locale.ReadString(section, variable, '');
    Result := StringReplace(Result, '|n', #13#10, [rfReplaceAll]);
  finally
    Locale.DisposeOf;
  end;
end;

function TLocale.GetLocaleDlg(section, variable: string): PWideChar;
begin
  Result := PWideChar(GetLocale(section, variable));
end;

end.
