unit uLocale;
(*
  Delphi localization unit functions for windows that uses .ini files
*)

{$DEFINE HAS_FMX}

interface

uses
{$IFDEF HAS_FMX}
  FMX.StdCtrls, FMX.ListBox, FMX.ExtCtrls,
{$ELSE}Vcl.StdCtrls, {$IFEND}
  Windows,
  IniFiles, SysUtils, Classes, TypInfo;

Const
  // ini sections
  locSYSTEMINFO = 'SYSTEMINFO';
  locMESSAGES = 'MESSAGES';
  locFORMS = 'FORMS';
  locDIALOGS = 'DIALOGS';
  locCHAT = 'CHAT';
  locCBQUALITY = 'CBQUALITY';
  locCBLANGUAGE = 'CBLANGUAGE';

  // ini message keys
  iMsgAwaitingAuth = 'AwaitingAuth';
  iMsgBusyGuest = 'BusyGuest';
  iMsgCanceled = 'Canceled';
  iMsgConnected = 'Connected';
  iMsgConnecting = 'Connecting';
  iMsgConnectionError = 'ConnectionError';
  iMsgDisconnected = 'Disconnected';
  iMsgDownloadSuccess = 'DownloadSuccess';
  iMsgGranted = 'Granted';
  iMsgIDNoExist = 'IDNoExist';
  iMsgLostConnection = 'LostConnection';
  iMsgRemoteConnected = 'RemoteConnected';
  iMsgSearchingID = 'SearchingID';
  iMsgSendSuccess = 'SendSuccess';
  iMsgServerError = 'ServerError';
  iMsgWrongPassword = 'WrongPassword';
  // ini form keys
  iFrmConfigApplyButton = 'ConfigApplyButton';
  iFrmConfigBackButton = 'ConfigBackButton';
  iFrmConfigLanguage = 'ConfigLanguage';
  iFrmConfigPassword = 'ConfigPassword';
  iFrmConfigQuickSupport = 'ConfigQuickSupport';
  iFrmConfigServer = 'ConfigServer';
  iFrmConfigStartup = 'ConfigStartup';
  iFrmConfigSystemTray = 'ConfigSystemTray';
  iFrmConfigTitle = 'ConfigTitle';
  iFrmFileDownloadButton = 'FileDownloadButton';
  iFrmFileDownloadProgress = 'FileDownloadProgress';
  iFrmFileFolder = 'FileFolder';
  iFrmFileSubTitle = 'FileSubTitle';
  iFrmFileTitle = 'FileTitle';
  iFrmFileUploadButton = 'FileUploadButton';
  iFrmFileUploadProgress = 'FileUploadProgress';
  iFrmMainConnectButton = 'MainConnectButton';
  iFrmMainGuestID = 'MainGuestID';
  iFrmMainMachineID = 'MainMachineID';
  iFrmMainPassword = 'MainPassword';
  iFrmMainResolution = 'MainResolution';
  iFrmMainSubTitle = 'MainSubTitle';
  iFrmMainTitle = 'MainTitle';
  iFrmRemoteBlock = 'RemoteBlock';
  iFrmRemoteChat = 'RemoteChat';
  iFrmRemoteFile = 'RemoteFile';
  iFrmRemoteMouse = 'RemoteMouse';
  iFrmRemoteRelease = 'RemoteRelease';
  iFrmRemoteTitle = 'RemoteTitle';
  // ini dialog keys
  iDlgErrorSelfConnect = 'ErrorSelfConnect';
  iDlgRemoteSupport = 'RemoteSupport';
  // ini system keys
  iSysSize = 'Size';
  iSysTitle = 'Title';
  iSysVersion = 'Version';
  // ini chat keys
  iChtThey = 'They';
  iChtYou = 'You';

  QualityValues: array of string = ['Grey Scale', 'Low', 'High', 'Real Time'];

type
  TLanguageImage = (AR_SA, DE_DE, EN_US, ES_AR, ES_ES, FR_FR, IT_IT, JA_JA,
    KO_KO, PT_BR, RU_RU, ZH_CN, ZH_TW);
  TComboTypes = (tcbQuality, tcbLanguage);

  TLocale = class
  private
    Locale: TIniFile;
    FLocaleFileName: string;
    procedure SetLocaleFileName(const Value: string);
  public
    constructor Create;
    function GetLocale(section, variable: string): string; overload;
    {$IFDEF HAS_FMX}
    procedure GetLocale(aCombo: TComboBox; comboType: TComboTypes); overload;
    {$ENDIF}
    function GetLocaleDlg(section, variable: string): PWideChar;
    procedure Initialize;
    procedure MakeTemplateIniFile;

    property LocaleFileName: string read FLocaleFileName
      write SetLocaleFileName;
  end;

implementation

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
    Locale.Free;
  End;
End;

{$IFDEF HAS_FMX}

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
          Locale.ReadSectionValues(locCBQUALITY, Items);
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
          Locale.ReadSectionValues(locCBLANGUAGE, Items);
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
{$ENDIF}

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

procedure TLocale.MakeTemplateIniFile;
begin

end;

procedure TLocale.SetLocaleFileName(const Value: string);
begin
  FLocaleFileName := Value;
end;

end.
