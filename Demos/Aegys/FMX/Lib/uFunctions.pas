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
  IniFiles, uConstants,Registry,Winapi.windows ;

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
  function iif(bcondicao:boolean;vtrue,vfalse:variant):variant;
  function RunOnStartup(sProgTitle, sCmdLine: string; bRunOnce: boolean):boolean;
implementation

function RunOnStartup(sProgTitle, sCmdLine: string; bRunOnce: boolean):boolean;
var
  sKey: string;
  reg : TRegIniFile;
begin
  if not bRunOnce then
      sKey := ''
  else
    sKey := 'Once';
  reg := TRegIniFile.Create('');
  reg.RootKey := HKEY_LOCAL_MACHINE;
  reg.WriteString('Software\Microsoft\Windows\CurrentVersion\Run'
+ sKey + #0, sProgTitle, sCmdLine);
  reg.Free;
end;

function iif(bcondicao:boolean;vtrue,vfalse:variant):variant;
begin
  if bcondicao then
  Result := vtrue else result :=vfalse;
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
  currIndex, i: integer;
begin
  Items := TStringList.Create;
  Locale := TIniFile.Create(FLocaleFileName);
  currIndex := aCombo.ItemIndex;
  try
    case comboType of
      tcbQuality:
        Locale.ReadSectionValues(CBQUAL, Items);
      tcbLanguage:
        Locale.ReadSectionValues(CBLANG, Items);
    end;

    aCombo.Items.Clear;
    for i := 0 to pred(Items.count) do
    begin
      aCombo.ListBox.Items.Add('');
      aCombo.ListItems[i].ItemData.Detail := Items.KeyNames[i];
      aCombo.ListItems[i].ItemData.Text := UTF8Decode(Items.ValueFromIndex[i]);
      aCombo.ListItems[i].ImageIndex := GetEnumValue(TypeInfo(TLanguageImage),
        Items.KeyNames[i]);
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
      1028:
        Res := TResourceStream.Create(HInstance, 'ZH_TW', RT_RCDATA);
      2052:
        Res := TResourceStream.Create(HInstance, 'ZH_CN', RT_RCDATA);
      1025:
        Res := TResourceStream.Create(HInstance, 'AR_SA', RT_RCDATA);
      1031:
        Res := TResourceStream.Create(HInstance, 'DE_DE', RT_RCDATA);
      1036:
        Res := TResourceStream.Create(HInstance, 'FR_FR', RT_RCDATA);
      1040:
        Res := TResourceStream.Create(HInstance, 'IT_IT', RT_RCDATA);
      1041:
        Res := TResourceStream.Create(HInstance, 'JA_JA', RT_RCDATA);
      1042:
        Res := TResourceStream.Create(HInstance, 'KO_KO', RT_RCDATA);
      1049:
        Res := TResourceStream.Create(HInstance, 'RU_RU', RT_RCDATA);
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

{ TVertScrollBoxHelper }

procedure TVertScrollBoxHelper.Clear;
var
  I: integer;
begin
  for I := pred(self.Content.ChildrenCount) downto 0 do
    self.Content.Children.Items[I].DisposeOf;
end;

end.
