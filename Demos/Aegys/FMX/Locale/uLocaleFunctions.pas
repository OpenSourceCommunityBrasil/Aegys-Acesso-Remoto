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

Uses System.SysUtils, System.Types, System.Classes, uLocaloPadrao;

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
  IniFiles;

{ TLocale }

Function TLocale.GetLocale(section, variable: string): string;
Var
 Locale      : TIniFile;
 vLocaleName : String;
 vStringStream : TStringStream;
Begin
 {$IFDEF Debug}
  vLocaleName := ExtractFilePath(ParamStr(0)) + 'locale.ini';
 {$ELSE}
  vLocaleName := ExtractFilePath(ParamStr(0)) + 'locale.dat';
 {$IFEND}
 If Not FileExists(vLocaleName) Then
  Begin
   vStringStream := TStringStream.Create(TDefaultLocale);
   Try
    vStringStream.SaveToFile(vLocaleName);
   Finally
    FreeAndNil(vStringStream);
   End;
  End;
 Locale := TIniFile.Create(vLocaleName);
 Try
  Result := Locale.ReadString(section, variable, '');
  Result := StringReplace(Result, '|n', sLineBreak, [rfReplaceAll]);
 Finally
  Locale.DisposeOf;
 End;
End;

function TLocale.GetLocaleDlg(section, variable: string) : PWideChar;
begin
  Result := PWideChar(GetLocale(section, variable));
end;

end.
