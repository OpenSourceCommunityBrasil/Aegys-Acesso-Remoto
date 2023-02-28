program AegysSuporteCliente;



{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  ShellAPI,
  Windows,
  classes,
  SysUtils,
  // forms
  uDM in 'src\Views\uDM.pas' {DM: TDataModule},
  uFormConexao in 'src\Views\uFormConexao.pas' {FormConexao},
  uFormTelaRemota in 'src\Views\uFormTelaRemota.pas' {FormTelaRemota},
  uFormChat in 'src\Views\uFormChat.pas' {FormChat},
  uFormConfig in 'src\Views\uFormConfig.pas' {fConfig},
  uFormSenha in 'src\Views\uFormSenha.pas' {FormSenha},
  uFileTransfer in 'src\Views\uFileTransfer.pas' {fFileTransfer},
  uFrameMensagemChat in 'src\Views\Frame\uFrameMensagemChat.pas' {FrameMensagemChat: TFrame},
  ufrMonitorItem in 'src\Views\Frame\ufrMonitorItem.pas' {frMonitorItem: TFrame},
  // units
  uLibClass in 'src\Classes\uLibClass.pas',
  uSendKeyClass in 'src\Classes\uSendKeyClass.pas',
  StreamManager in 'src\Classes\StreamManager.pas',
  Bcrypt in 'src\Classes\Bcrypt.pas',
  uFunctions in 'src\Classes\uFunctions.pas',
  uFilesFoldersOP in 'src\Classes\uFilesFoldersOP.pas',
  uIconsAssoc in 'src\Classes\uIconsAssoc.pas',
  // common units
  uConstants in '..\..\commons\uConstants.pas',
  uSQLiteConfig in '..\..\commons\uSQLiteConfig.pas',
  // clipboard
  CCR.Clipboard.Apple.Helpers in 'src\Classes\ClipBoard\CCR.Clipboard.Apple.Helpers.pas',
  CCR.Clipboard.Apple in 'src\Classes\ClipBoard\CCR.Clipboard.Apple.pas',
  CCR.Clipboard.Consts in 'src\Classes\ClipBoard\CCR.Clipboard.Consts.pas',
  CCR.Clipboard.FMX.iOS in 'src\Classes\ClipBoard\CCR.Clipboard.FMX.iOS.pas',
  CCR.Clipboard.FMX.Mac in 'src\Classes\ClipBoard\CCR.Clipboard.FMX.Mac.pas',
  CCR.Clipboard.FMX in 'src\Classes\ClipBoard\CCR.Clipboard.FMX.pas',
  CCR.Clipboard.FMX.Win in 'src\Classes\ClipBoard\CCR.Clipboard.FMX.Win.pas',
  CCR.Clipboard in 'src\Classes\ClipBoard\CCR.Clipboard.pas',
  CCR.Clipboard.VCL in 'src\Classes\ClipBoard\CCR.Clipboard.VCL.pas',
  CCR.Clipboard.Win in 'src\Classes\ClipBoard\CCR.Clipboard.Win.pas',
  // basecomponent
  uAegysBase in '..\..\Component\Base\uAegysBase.pas',
  uAegysBufferPack in '..\..\Component\Base\uAegysBufferPack.pas',
  uAegysConsts in '..\..\Component\Base\uAegysConsts.pas',
  uAegysDataTypes in '..\..\Component\Base\uAegysDataTypes.pas',
  uAegysThreads in '..\..\Component\Base\uAegysThreads.pas',
  uAegysTools in '..\..\Component\Base\uAegysTools.pas',
  uAegysClientMotor in '..\..\Component\Base\uAegysClientMotor.pas',
  uAegysZlib in '..\..\Component\Utils\uAegysZlib.pas';

{$R *.res}

procedure ExtractRunAsSystem;
var
  resource: TResourceStream;
begin
  resource := TResourceStream.Create(HInstance, 'RUN_AS_SYSTEM', RT_RCDATA);
  try
    resource.SaveToFile(ExtractFilePath(ParamStr(0)) + '\RunAsSystem.exe');
  finally
    FreeAndNil(resource);
  end;
end;

procedure ExtractCommonFiles;
var
  resource: TResourceStream;
begin
{$IFDEF WIN32}
  resource := TResourceStream.Create(HInstance, 'SQLITE32', RT_RCDATA);
  resource.SaveToFile(ExtractFilePath(ParamStr(0)) + '\sqlite3.dll');
  FreeAndNil(resource);
{$ENDIF}
{$IFDEF WIN64}
  resource := TResourceStream.Create(HInstance, 'SQLITE64', RT_RCDATA);
  resource.SaveToFile(ExtractFilePath(ParamStr(0)) + '\sqlite3.dll');
  FreeAndNil(resource);
{$ENDIF}
end;

function IsAccountSystem: Boolean;
var
  hToken: THandle;
  pTokenUser: ^TTokenUser;
  dwInfoBufferSize: DWORD;
  pSystemSid: PSID;
const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_LOCAL_SYSTEM_RID = $00000012;
begin
  if not OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hToken) then
  begin
    Result := False;
    Exit;
  end;

  GetMem(pTokenUser, 1024);
  if not GetTokenInformation(hToken, TokenUser, pTokenUser, 1024,
    dwInfoBufferSize) then
  begin
    CloseHandle(hToken);
    Result := False;
    Exit;
  end;

  CloseHandle(hToken);

  if not AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 1,
    SECURITY_LOCAL_SYSTEM_RID, 0, 0, 0, 0, 0, 0, 0, pSystemSid) then
  begin
    Result := False;
    Exit;
  end;

  Result := EqualSid(pTokenUser.User.Sid, pSystemSid);
  FreeSid(pSystemSid);
end;

var
  Locale: TLocale;

begin
  Application.Initialize;
  Locale := TLocale.Create;
  // Workaround to run on SYSTEM account. This is necessary in order to be able to interact with UAC.
{$IFNDEF DEBUG}
  if not IsAccountSystem then
  begin
    ExtractRunAsSystem;
    ShellExecute(0, 'open', PChar(ExtractFilePath(ParamStr(0)) +
      '\RunAsSystem.exe'), PChar('"' + ParamStr(0) + '"'), nil, SW_HIDE);
    Application.Terminate;
  end
  else
  begin
    Sleep(1000);
    DeleteFile(ExtractFilePath(ParamStr(0)) + '\RunAsSystem.exe');
  end;
{$ENDIF}
  ExtractCommonFiles;
  Application.Title := Locale.GetLocale(MAIN, 'Title');
  Application.CreateForm(TFormConexao, FormConexao);
  Application.CreateForm(TDM, DM);
  Application.Run;

  if Application.Terminated then
    Locale.DisposeOf;

end.
