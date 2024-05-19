unit uFunctions;

{
   Aegys Remote Access Project.
  Criado por XyberX (Gilbero Rocha da Silva), o Aegys Remote Access Project tem como objetivo o uso de Acesso remoto
  Gratuito para utilização de pessoas em geral.
   O Aegys Remote Access Project tem como desenvolvedores e mantedores hoje

  Membros do Grupo :

  XyberX (Gilberto Rocha)    - Admin - Criador e Administrador  do pacote.
  Wendel Fassarela           - Devel and Admin
  Mobius One                 - Devel, Tester and Admin.
  Gustavo                    - Devel and Admin.
  Roniery                    - Devel and Admin.
  Alexandre Abbade           - Devel and Admin.
  e Outros como você, venha participar também.
}

interface

Uses
  System.SysUtils, System.Types, System.Classes, System.TypInfo,
  FMX.ListBox, FMX.Layouts,
  IniFiles, uConstants, Registry, Winapi.windows;

type
  TLanguageImage = (AR_SA, DE_DE, EN_US, ES_AR, ES_ES, FR_FR, IT_IT, JA_JA,
    KO_KO, PT_BR, RU_RU, ZH_CN, ZH_TW);
  TComboTypes = (tcbQuality, tcbLanguage);

  TVertScrollBoxHelper = class helper for TVertScrollBox
  public
    procedure Clear;
  end;

function iif(bcondicao: boolean; vtrue, vfalse: variant): variant;
function RunOnStartup(sProgTitle, sCmdLine: string; bRunOnce: boolean): boolean;

implementation

function RunOnStartup(sProgTitle, sCmdLine: string; bRunOnce: boolean): boolean;
var
  sKey: string;
  reg: TRegIniFile;
begin
  if not bRunOnce then
    sKey := ''
  else
    sKey := 'Once';
  reg := TRegIniFile.Create('');
  reg.RootKey := HKEY_LOCAL_MACHINE;
  reg.WriteString('Software\Microsoft\Windows\CurrentVersion\Run' + sKey + #0,
    sProgTitle, sCmdLine);
  reg.Free;
end;

function iif(bcondicao: boolean; vtrue, vfalse: variant): variant;
begin
  if bcondicao then
    Result := vtrue
  else
    Result := vfalse;
end;

{ TVertScrollBoxHelper }

procedure TVertScrollBoxHelper.Clear;
var
  i: integer;
begin
  for i := pred(self.Content.ChildrenCount) downto 0 do
    self.Content.Children.Items[i].DisposeOf;
end;

end.
