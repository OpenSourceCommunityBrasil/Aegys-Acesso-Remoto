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
  uDM in 'Styles\uDM.pas' {DM: TDataModule},
  uLibClass in 'Lib\uLibClass.pas',
  uSendKeyClass in 'Lib\uSendKeyClass.pas',
  StreamManager in 'Lib\StreamManager.pas',
  uFormSenha in 'View\uFormSenha.pas' {FormSenha},
  Bcrypt in 'Lib\Bcrypt.pas',
  uFormConfig in 'View\uFormConfig.pas' {fConfig},
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
  ufrMonitorItem in 'View\Frame\ufrMonitorItem.pas' {frMonitorItem: TFrame},
  uSQLiteConfig in 'Lib\uSQLiteConfig.pas',
  uFilesFoldersOP in 'Lib\uFilesFoldersOP.pas',
  uFileTransfer in 'View\uFileTransfer.pas' {fFileTransfer},
  uIconsAssoc in 'Lib\uIconsAssoc.pas',
  uAegysBase in '..\..\Source\Base\uAegysBase.pas',
  uAegysBufferPack in '..\..\Source\Base\uAegysBufferPack.pas',
  uAegysConsts in '..\..\Source\Base\uAegysConsts.pas',
  uAegysDataTypes in '..\..\Source\Base\uAegysDataTypes.pas',
  uAegysThreads in '..\..\Source\Base\uAegysThreads.pas',
  uAegysTools in '..\..\Source\Base\uAegysTools.pas',
  uAegysClientMotor in 'Lib\uAegysClientMotor.pas',
  uAegysZlib in '..\..\Source\Libs\uAegysZlib.pas',
  DX12.D3D11 in 'Lib\DXHeaders\DX12.D3D11.pas',
  DX12.D3DCommon in 'Lib\DXHeaders\DX12.D3DCommon.pas',
  DX12.DXGI in 'Lib\DXHeaders\DX12.DXGI.pas',
  DX12.DXGI1_2 in 'Lib\DXHeaders\DX12.DXGI1_2.pas',
  Execute.DesktopDuplicationAPI in 'Lib\lib\Execute.DesktopDuplicationAPI.pas',
  uAegysRFBList in 'RFB\uAegysRFBList.pas',
  FastBlend in 'RFB\Units\FastBlend.pas',
  FastDIB in 'RFB\Units\FastDIB.pas',
  FastDraw in 'RFB\Units\FastDraw.pas',
  FastFiles in 'RFB\Units\FastFiles.pas',
  FastFX in 'RFB\Units\FastFX.pas',
  FastGate in 'RFB\Units\FastGate.pas',
  FastGen in 'RFB\Units\FastGen.pas',
  FastGPlus in 'RFB\Units\FastGPlus.pas',
  FastSize in 'RFB\Units\FastSize.pas',
  FConvert in 'RFB\Units\FConvert.pas',
  uAegysUtils in 'RFB\Units\uAegysUtils.pas';

{$R *.res}

procedure ExtractRunAsSystem;
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
  Application.CreateForm(TDM, DM);
  Application.Run;

  if Application.Terminated then
    Locale.DisposeOf;

end.
