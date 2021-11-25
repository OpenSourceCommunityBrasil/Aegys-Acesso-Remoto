unit UFuncoes;

interface
uses
  System.SysUtils, System.Classes,IniFiles;

  function lercfg(Arquivo, Ext, Secao, Campo: String; Criptografado:boolean): String;
  procedure salvarcfg(Arquivo, Ext, Secao, Campo, Valor: String; Criptografado:boolean);

implementation

function lercfg(Arquivo, Ext, Secao, Campo: String;
  Criptografado: boolean): String;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(ExtractFilePath(ParamStr(0))+Arquivo+'.'+Ext);
  if Criptografado = true then
  Result := Ini.ReadString(Secao, Campo, '')
  else
  Result := Ini.ReadString(Secao, Campo, '');
  Ini.Free;
end;

procedure salvarcfg(Arquivo, Ext, Secao, Campo, Valor: String;
  Criptografado: boolean);
Var
   Ini: TIniFile;
begin
    Ini := TIniFile.Create(ExtractFilePath(ParamStr(0))+Arquivo+'.'+Ext);
    if Criptografado = true then
    Ini.WriteString(Secao, Campo, Valor)
    else
    Ini.WriteString(Secao, Campo, Valor);
    Ini.Free;
end;

end.
