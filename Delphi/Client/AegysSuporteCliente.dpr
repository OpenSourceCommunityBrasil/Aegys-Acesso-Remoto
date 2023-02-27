program AegysSuporteCliente;



{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  ShellAPI,
  Windows,
  classes,
  SysUtils,
  uFormConexao in '..\..\Source\View\uFormConexao.pas' {FormConexao},
  uFormTelaRemota in '..\..\Source\View\uFormTelaRemota.pas' {FormTelaRemota},
  uFormChat in '..\..\Source\View\uFormChat.pas' {FormChat},
  uDM in '..\..\Source\Styles\uDM.pas' {DM: TDataModule},
  uLibClass in '..\..\Source\Lib\uLibClass.pas',
  uSendKeyClass in '..\..\Source\Lib\uSendKeyClass.pas',
  StreamManager in '..\..\Source\Lib\StreamManager.pas',
  uFormSenha in '..\..\Source\View\uFormSenha.pas' {FormSenha},
  Bcrypt in '..\..\Source\Lib\Bcrypt.pas',
  uFormConfig in '..\..\Source\View\uFormConfig.pas' {fConfig},
  uFrameMensagemChat in '..\..\Source\View\Frame\uFrameMensagemChat.pas' {FrameMensagemChat: TFrame},
  uConstants in '..\..\Source\Lib\uConstants.pas',
  uFunctions in '..\..\Source\Lib\uFunctions.pas',
  CCR.Clipboard.Apple.Helpers in '..\..\Source\Lib\ClipBoard\CCR.Clipboard.Apple.Helpers.pas',
  CCR.Clipboard.Apple in '..\..\Source\Lib\ClipBoard\CCR.Clipboard.Apple.pas',
  CCR.Clipboard.Consts in '..\..\Source\Lib\ClipBoard\CCR.Clipboard.Consts.pas',
  CCR.Clipboard.FMX.iOS in '..\..\Source\Lib\ClipBoard\CCR.Clipboard.FMX.iOS.pas',
  CCR.Clipboard.FMX.Mac in '..\..\Source\Lib\ClipBoard\CCR.Clipboard.FMX.Mac.pas',
  CCR.Clipboard.FMX in '..\..\Source\Lib\ClipBoard\CCR.Clipboard.FMX.pas',
  CCR.Clipboard.FMX.Win in '..\..\Source\Lib\ClipBoard\CCR.Clipboard.FMX.Win.pas',
  CCR.Clipboard in '..\..\Source\Lib\ClipBoard\CCR.Clipboard.pas',
  CCR.Clipboard.VCL in '..\..\Source\Lib\ClipBoard\CCR.Clipboard.VCL.pas',
  CCR.Clipboard.Win in '..\..\Source\Lib\ClipBoard\CCR.Clipboard.Win.pas',
  ufrMonitorItem in '..\..\Source\View\Frame\ufrMonitorItem.pas' {frMonitorItem: TFrame},
  uSQLiteConfig in '..\..\Source\Lib\uSQLiteConfig.pas',
  uFilesFoldersOP in '..\..\Source\Lib\uFilesFoldersOP.pas',
  uFileTransfer in '..\..\Source\View\uFileTransfer.pas' {fFileTransfer},
  uIconsAssoc in '..\..\Source\Lib\uIconsAssoc.pas',
  uAegysBase in '..\..\Source\Base\uAegysBase.pas',
  uAegysBufferPack in '..\..\Source\Base\uAegysBufferPack.pas',
  uAegysConsts in '..\..\Source\Base\uAegysConsts.pas',
  uAegysDataTypes in '..\..\Source\Base\uAegysDataTypes.pas',
  uAegysThreads in '..\..\Source\Base\uAegysThreads.pas',
  uAegysTools in '..\..\Source\Base\uAegysTools.pas',
  uAegysClientMotor in '..\..\Source\Lib\uAegysClientMotor.pas',
  uAegysZlib in '..\..\Source\Utils\uAegysZlib.pas';

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
