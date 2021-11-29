program AegysSuporteCliente;

{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  ShellAPI,
  Windows,
  classes,
  SysUtils,
  uFormConexao in 'View\uFormConexao.pas' {FormConexao},
  uFormTelaRemota in 'View\uFormTelaRemota.pas' {FormTelaRemota},
  uFormChat in 'View\uFormChat.pas' {FormChat},
  uDM_Styles in 'Styles\uDM_Styles.pas' {DM_Styles: TDataModule},
  uFormArquivos in 'View\uFormArquivos.pas' {FormArquivos},
  uCtrl_Threads in 'Control\uCtrl_Threads.pas',
  uLibClass in 'Lib\uLibClass.pas',
  uSendKeyClass in 'Lib\uSendKeyClass.pas',
  StreamManager in 'Lib\StreamManager.pas',
  uFormSenha in 'View\uFormSenha.pas' {FormSenha},
  uCtrl_Conexao in 'Control\uCtrl_Conexao.pas',
  uHttpClass in 'Lib\uHttpClass.pas',
  Bcrypt in 'Lib\Bcrypt.pas',
  uFormConfig in 'View\uFormConfig.pas' {fConfig},
  uFrameArquivo in 'View\Frame\uFrameArquivo.pas' {FrameArquivo: TFrame},
  uFrameMensagemChat in 'View\Frame\uFrameMensagemChat.pas' {FrameMensagemChat: TFrame},
  uConstants in 'Lib\uConstants.pas',
  uFunctions in 'Lib\uFunctions.pas',
  CCR.Clipboard.Apple.Helpers in 'Lib\ClipBoard\CCR.Clipboard.Apple.Helpers.pas',
  CCR.Clipboard.Apple in 'Lib\ClipBoard\CCR.Clipboard.Apple.pas',
  CCR.Clipboard.Consts in 'Lib\ClipBoard\CCR.Clipboard.Consts.pas',
  CCR.Clipboard.FMX.iOS in 'Lib\ClipBoard\CCR.Clipboard.FMX.iOS.pas',
  CCR.Clipboard.FMX.Mac in 'Lib\ClipBoard\CCR.Clipboard.FMX.Mac.pas',
  CCR.Clipboard.FMX in 'Lib\ClipBoard\CCR.Clipboard.FMX.pas',
  CCR.Clipboard.FMX.Win in 'Lib\ClipBoard\CCR.Clipboard.FMX.Win.pas',
  CCR.Clipboard in 'Lib\ClipBoard\CCR.Clipboard.pas',
  CCR.Clipboard.VCL in 'Lib\ClipBoard\CCR.Clipboard.VCL.pas',
  CCR.Clipboard.Win in 'Lib\ClipBoard\CCR.Clipboard.Win.pas',
  ufrMonitorItem in 'View\Frame\ufrMonitorItem.pas' {frMonitorItem: TFrame};

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
  Application.Title := Locale.GetLocale(MAIN, 'Title');
  Application.CreateForm(TFormConexao, FormConexao);
  Application.CreateForm(TDM_Styles, DM_Styles);
  Application.CreateForm(TFormChat, FormChat);
  Application.CreateForm(TFormTelaRemota, FormTelaRemota);
  Application.CreateForm(TFormArquivos, FormArquivos);
  Application.CreateForm(TFormSenha, FormSenha);
  Application.Run;

  if Application.Terminated then
    Locale.DisposeOf;

end.
