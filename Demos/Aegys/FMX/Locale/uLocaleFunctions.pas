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

Uses System.SysUtils, System.Types, System.Classes;

type
  TLocale = class
  private
    FLocaleFileName: string;
    procedure SetLocaleFileName(const Value: string);
  public
    function GetLocale(section, variable: string): string;
    function GetLocaleDlg(section, variable: string): PWideChar;
    procedure Initialize;
    constructor Create;
    property LocaleFileName: string read FLocaleFileName
      write SetLocaleFileName;
  end;

implementation

uses
  IniFiles;

{ TLocale }

constructor TLocale.Create;
begin
{$IFDEF Debug}
  SetLocaleFileName(ExtractFilePath(ParamStr(0)) + 'locale.ini');
{$ELSE}
  SetLocaleFileName(ExtractFilePath(ParamStr(0)) + 'locale.dat');
{$IFEND}
  Initialize;
end;

Function TLocale.GetLocale(section, variable: string): string;
Var
  Locale: TIniFile;
Begin
  Locale := TIniFile.Create(FLocaleFileName);
  Try
    Result := Locale.ReadString(section, variable, '');
    Result := StringReplace(Result, '|n', sLineBreak, [rfReplaceAll]);
  Finally
    Locale.DisposeOf;
  End;
End;

function TLocale.GetLocaleDlg(section, variable: string): PWideChar;
begin
  Result := PWideChar(GetLocale(section, variable));
end;

procedure TLocale.Initialize;
var
  Res: TResourceStream;
  teste: integer;
begin
  if not FileExists(FLocaleFileName) then
  begin
{$IFDEF MSWindows}
    case SysLocale.DefaultLCID of
      3081, 10249, 4105, 9225, 2057, 16393, 6153, 8201, 5129, 13321, 7177,
        11273, 12297, 1033:
        Res := TResourceStream.Create(HInstance, 'EN_US', RT_RCDATA);
      1046:
        Res := TResourceStream.Create(HInstance, 'PT_BR', RT_RCDATA);
      11274, 16394, 13322, 9226, 5130, 7178, 12298, 17418, 4106, 18442, 2058,
        19466, 6154, 15370, 10250, 20490, 14346, 8202, 1034:
        Res := TResourceStream.Create(HInstance, 'ES_ES', RT_RCDATA);
    else
      Res := TResourceStream.Create(HInstance, 'EN_US', RT_RCDATA);
    end;
{$ENDIF}
    try
      Res.SaveToFile(FLocaleFileName);
    finally
      Res.DisposeOf;
    end;
  end;
end;

procedure TLocale.SetLocaleFileName(const Value: string);
begin
  FLocaleFileName := Value;
end;

end.
