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
  lsSYSTEMINFO = 'SYSTEMINFO';
  lsMESSAGES = 'MESSAGES';
  lsFORMS = 'FORMS';
  lsDIALOGS = 'DIALOGS';
  lsCHAT = 'CHAT';
  lsCBQUALITY = 'CBQUALITY';
  lsCBLANGUAGE = 'CBLANGUAGE';

  // ini message keys
  lvMsgAwaitingAuth = 'AwaitingAuth';
  lvMsgBusyGuest = 'BusyGuest';
  lvMsgCanceled = 'Canceled';
  lvMsgConnected = 'Connected';
  lvMsgConnecting = 'Connecting';
  lvMsgConnectionError = 'ConnectionError';
  lvMsgDisconnected = 'Disconnected';
  lvMsgDownloadSuccess = 'DownloadSuccess';
  lvMsgGranted = 'Granted';
  lvMsgIDNoExist = 'IDNoExist';
  lvMsgIncomming = 'Incomming';
  lvMsgLostConnection = 'LostConnection';
  lvMsgOffline = 'Offline';
  lvMsgOnline = 'Online';
  lvMsgPeerDisconnected = 'PeerDisconnected';
  lvMsgRemoteConnected = 'RemoteConnected';
  lvMsgSearchingID = 'SearchingID';
  lvMsgSendSuccess = 'SendSuccess';
  lvMsgServerError = 'ServerError';
  lvMsgWrongPassword = 'WrongPassword';
  // ini form keys
  lvFrmConfigApplyButton = 'ConfigApplyButton';
  lvFrmConfigBackButton = 'ConfigBackButton';
  lvFrmConfigLanguage = 'ConfigLanguage';
  lvFrmConfigPassword = 'ConfigPassword';
  lvFrmConfigQuickSupport = 'ConfigQuickSupport';
  lvFrmConfigServer = 'ConfigServer';
  lvFrmConfigStartup = 'ConfigStartup';
  lvFrmConfigSystemTray = 'ConfigSystemTray';
  lvFrmConfigTitle = 'ConfigTitle';
  lvFrmFileDownloadButton = 'FileDownloadButton';
  lvFrmFileDownloadProgress = 'FileDownloadProgress';
  lvFrmFileFolder = 'FileFolder';
  lvFrmFileSubTitle = 'FileSubTitle';
  lvFrmFileTitle = 'FileTitle';
  lvFrmFileUploadButton = 'FileUploadButton';
  lvFrmFileUploadProgress = 'FileUploadProgress';
  lvFrmMainConnectButton = 'MainConnectButton';
  lvFrmMainGuestID = 'MainGuestID';
  lvFrmMainMachineID = 'MainMachineID';
  lvFrmMainPassword = 'MainPassword';
  lvFrmMainResolution = 'MainResolution';
  lvFrmMainSubTitle = 'MainSubTitle';
  lvFrmMainTitle = 'MainTitle';
  lvFrmRemoteBlock = 'RemoteBlock';
  lvFrmRemoteChat = 'RemoteChat';
  lvFrmRemoteFile = 'RemoteFile';
  lvFrmRemoteMouse = 'RemoteMouse';
  lvFrmRemoteRelease = 'RemoteRelease';
  lvFrmRemoteTitle = 'RemoteTitle';
  // ini dialog keys
  lvDlgErrorSelfConnect = 'ErrorSelfConnect';
  lvDlgRemoteSupport = 'RemoteSupport';
  // ini system keys
  lvSysSize = 'Size';
  lvSysTitle = 'Title';
  lvSysVersion = 'Version';
  // ini chat keys
  lvChtThey = 'They';
  lvChtYou = 'You';

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
    destructor Destroy; override;
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
  Locale := TIniFile.Create(FLocaleFileName);
  Initialize;
end;

Function TLocale.GetLocale(section, variable: string): string;
Begin
  Result := UTF8Decode(Locale.ReadString(section, variable, ''));
  Result := StringReplace(Result, '|n', sLineBreak, [rfReplaceAll]);
End;

destructor TLocale.Destroy;
begin
  Locale.Free;
  inherited;
end;

{$IFDEF HAS_FMX}
procedure TLocale.GetLocale(aCombo: TComboBox; comboType: TComboTypes);
var
  Items: TStringList;
  currIndex, i, j: integer;
begin
  Items := TStringList.Create;
  currIndex := aCombo.ItemIndex;
  try
    aCombo.Items.Clear;
    case comboType of
      tcbQuality:
        begin
          Locale.ReadSectionValues(lsCBQUALITY, Items);
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
          Locale.ReadSectionValues(lsCBLANGUAGE, Items);
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
    Items.Free;
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
      Res.Free;
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
