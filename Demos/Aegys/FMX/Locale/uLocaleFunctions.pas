unit uLocaleFunctions;

{
 Project Aegys Remote Support.

   Created by Gilberto Rocha da Silva in 04/05/2017 based on project Allakore, has by objective to promote remote access
 and other resources freely to all those who need it, today maintained by a beautiful community. Listing below our
 higly esteemed collaborators:

  Gilberto Rocha da Silva (XyberX) (Creator of Aegys Project/Main Developer/Admin)
  Wendel Rodrigues Fassarella (wendelfassarella) (Creator of Aegys FMX/CORE Developer)
  Rai Duarte Jales (Raí Duarte) (Aegys Server Developer)
  Roniery Santos Cardoso (Aegys Developer)
  Alexandre Carlos Silva Abade (Aegys Developer)
  Mobius One (Aegys Developer)
}

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
