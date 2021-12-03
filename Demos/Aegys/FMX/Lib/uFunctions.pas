unit uFunctions;

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

Uses
  System.SysUtils, System.Types, System.Classes, System.TypInfo,
  FMX.ListBox, FMX.Layouts,
  IniFiles, uConstants, Registry, Winapi.windows;

type
  TLanguageImage = (AR_SA, DE_DE, EN_US, ES_AR, ES_ES, FR_FR, IT_IT, JA_JA,
    KO_KO, PT_BR, RU_RU, ZH_CN, ZH_TW);
  TComboTypes = (tcbQuality, tcbLanguage);

type
  TLocale = class
  private
    Locale: TIniFile;
    FLocaleFileName: string;
    procedure SetLocaleFileName(const Value: string);
  public
    function GetLocale(section, variable: string): string; overload;
    procedure GetLocale(aCombo: TComboBox; comboType: TComboTypes); overload;
    function GetLocaleDlg(section, variable: string): PWideChar;
    procedure Initialize;
    constructor Create;
    property LocaleFileName: string read FLocaleFileName
      write SetLocaleFileName;
  end;

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

{ TLocale }

constructor TLocale.Create;
begin
{$IFDEF DEBUG}
  SetLocaleFileName(ExtractFilePath(ParamStr(0)) + 'locale.ini');
{$ELSE}
  SetLocaleFileName(ExtractFilePath(ParamStr(0)) + 'locale.dat');
{$IFEND}
  Initialize;
end;

Function TLocale.GetLocale(section, variable: string): string;
Begin
  Locale := TIniFile.Create(FLocaleFileName);
  Try
    Result := UTF8Decode(Locale.ReadString(section, variable, ''));
    Result := StringReplace(Result, '|n', sLineBreak, [rfReplaceAll]);
  Finally
    Locale.DisposeOf;
  End;
End;

procedure TLocale.GetLocale(aCombo: TComboBox; comboType: TComboTypes);
var
  Items: TStringList;
  currIndex, i, j: integer;
begin
  Items := TStringList.Create;
  Locale := TIniFile.Create(FLocaleFileName);
  currIndex := aCombo.ItemIndex;
  try
    aCombo.Items.Clear;
    case comboType of
      tcbQuality:
        begin
          Locale.ReadSectionValues(CBQUAL, Items);
          for i := 0 to pred(Items.count) do
          begin
            aCombo.ListBox.Items.Add('');
            aCombo.ListItems[i].ItemData.Detail := Items.KeyNames[i];
            aCombo.ListItems[i].ItemData.Text :=
              UTF8Decode(Items.ValueFromIndex[i]);
          end;
        end;
      tcbLanguage:
        begin
          Locale.ReadSectionValues(CBLANG, Items);
          j := 0;
          for i := 0 to pred(Items.count) do
            if GetEnumValue(TypeInfo(TLanguageImage), Items.KeyNames[i]) > -1
            then
            begin
              aCombo.ListBox.Items.Add('');
              aCombo.ListItems[j].ItemData.Detail := Items.KeyNames[i];
              aCombo.ListItems[j].ItemData.Text :=
                UTF8Decode(Items.ValueFromIndex[i]);
              aCombo.ListItems[j].ImageIndex :=
                GetEnumValue(TypeInfo(TLanguageImage), Items.KeyNames[i]);
              inc(j);
            end;
        end;
    end;
    aCombo.ItemIndex := currIndex;
  finally
    Items.DisposeOf;
    Locale.DisposeOf;
  end;
end;

function TLocale.GetLocaleDlg(section, variable: string): PWideChar;
begin
  Result := PWideChar(GetLocale(section, variable));
end;

procedure TLocale.Initialize;
var
  Res: TResourceStream;
  teste: integer;
  test: string;
begin
  if not FileExists(FLocaleFileName) then
  begin
{$IFDEF MSWindows}
    try
      Res := TResourceStream.Create(HInstance,
        Languages.LocaleName[Languages.IndexOf(Languages.UserDefaultLocale)
        ].ToUpper.Replace('-', '_'), RT_RCDATA);
    except
      Res := TResourceStream.Create(HInstance, 'EN-US', RT_RCDATA);
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

{ TVertScrollBoxHelper }

procedure TVertScrollBoxHelper.Clear;
var
  i: integer;
begin
  for i := pred(self.Content.ChildrenCount) downto 0 do
    self.Content.Children.Items[i].DisposeOf;
end;

end.
