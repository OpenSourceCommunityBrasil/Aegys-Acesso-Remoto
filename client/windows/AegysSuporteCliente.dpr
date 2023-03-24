program AegysSuporteCliente;

{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  ShellAPI,
  Windows,
  classes,
  SysUtils,
  uFormConexao in 'src\Views\uFormConexao.pas' {FormConexao} ,
  uFormTelaRemota in 'src\Views\uFormTelaRemota.pas' {FormTelaRemota} ,
  uFormChat in 'src\Views\uFormChat.pas' {FormChat} ,
  uDM in 'src\Views\uDM.pas' {DM: TDataModule} ,
  uFrameMensagemChat
    in 'src\Views\Frame\uFrameMensagemChat.pas' {FrameMensagemChat: TFrame} ,
  uFormSenha in 'src\Views\uFormSenha.pas' {FormSenha} ,
  ufrMonitorItem
    in 'src\Views\Frame\ufrMonitorItem.pas' {frMonitorItem: TFrame} ,
  uFileTransfer in 'src\Views\uFileTransfer.pas' {fFileTransfer} ,
  uFormConfig in 'src\Views\uFormConfig.pas' {fConfig} ,
  uLibClass in 'src\Classes\uLibClass.pas',
  uSendKeyClass in 'src\Classes\uSendKeyClass.pas',
  StreamManager in 'src\Classes\StreamManager.pas',
  Bcrypt in 'src\Classes\Bcrypt.pas',
  uFunctions in 'src\Classes\uFunctions.pas',
  CCR.Clipboard.Apple.Helpers
    in 'src\Classes\ClipBoard\CCR.Clipboard.Apple.Helpers.pas',
  CCR.Clipboard.Apple in 'src\Classes\ClipBoard\CCR.Clipboard.Apple.pas',
  CCR.Clipboard.Consts in 'src\Classes\ClipBoard\CCR.Clipboard.Consts.pas',
  CCR.Clipboard.FMX.iOS in 'src\Classes\ClipBoard\CCR.Clipboard.FMX.iOS.pas',
  CCR.Clipboard.FMX.Mac in 'src\Classes\ClipBoard\CCR.Clipboard.FMX.Mac.pas',
  CCR.Clipboard.FMX in 'src\Classes\ClipBoard\CCR.Clipboard.FMX.pas',
  CCR.Clipboard.FMX.Win in 'src\Classes\ClipBoard\CCR.Clipboard.FMX.Win.pas',
  CCR.Clipboard in 'src\Classes\ClipBoard\CCR.Clipboard.pas',
  CCR.Clipboard.VCL in 'src\Classes\ClipBoard\CCR.Clipboard.VCL.pas',
  CCR.Clipboard.Win in 'src\Classes\ClipBoard\CCR.Clipboard.Win.pas',
  uFilesFoldersOP in 'src\Classes\uFilesFoldersOP.pas',
  uIconsAssoc in 'src\Classes\uIconsAssoc.pas',
  DX12.D3D11 in 'src\Classes\DXHeaders\DX12.D3D11.pas',
  DX12.D3DCommon in 'src\Classes\DXHeaders\DX12.D3DCommon.pas',
  DX12.DXGI in 'src\Classes\DXHeaders\DX12.DXGI.pas',
  DX12.DXGI1_2 in 'src\Classes\DXHeaders\DX12.DXGI1_2.pas',
  Execute.DesktopDuplicationAPI
    in 'src\Classes\Execute.DesktopDuplicationAPI.pas',
  uConstants in '..\..\Commons\uConstants.pas',
  Config.SQLite.FireDAC in '..\..\Commons\Config.SQLite.FireDAC.pas',
  uLocale in '..\..\Commons\uLocale.pas',
  uAegysClientMotor in '..\..\Component\Base\uAegysClientMotor.pas',
  uAegysBase in '..\..\Component\Base\uAegysBase.pas',
  uAegysBufferPack in '..\..\Component\Base\uAegysBufferPack.pas',
  uAegysConsts in '..\..\Component\Base\uAegysConsts.pas',
  uAegysDataTypes in '..\..\Component\Base\uAegysDataTypes.pas',
  uAegysThreads in '..\..\Component\Base\uAegysThreads.pas',
  uAegysTools in '..\..\Component\Base\uAegysTools.pas',
  uAegysZlib in '..\..\Component\utils\uAegysZlib.pas',
  uAegysRFBList in 'src\Classes\RFB\uAegysRFBList.pas',
  FastBlend in 'src\Classes\RFB\Units\FastBlend.pas',
  FastDIB in 'src\Classes\RFB\Units\FastDIB.pas',
  FastDraw in 'src\Classes\RFB\Units\FastDraw.pas',
  FastFiles in 'src\Classes\RFB\Units\FastFiles.pas',
  FastFX in 'src\Classes\RFB\Units\FastFX.pas',
  FastGate in 'src\Classes\RFB\Units\FastGate.pas',
  FastGen in 'src\Classes\RFB\Units\FastGen.pas',
  FastGPlus in 'src\Classes\RFB\Units\FastGPlus.pas',
  FastSize in 'src\Classes\RFB\Units\FastSize.pas',
  FConvert in 'src\Classes\RFB\Units\FConvert.pas',
  uAegysUtils in 'src\Classes\RFB\Units\uAegysUtils.pas';

{$R *.res}

procedure ExtractResources;
var
  resource: TResourceStream;
begin
  {$IF Defined(WIN32)}
  resource := TResourceStream.Create(HInstance, 'SQLITE32', RT_RCDATA);
  resource.SaveToFile(ExtractFilePath(ParamStr(0)) + '\sqlite3.dll');
  FreeAndNil(resource);
  {$ELSEIF Defined(WIN64)}
  resource := TResourceStream.Create(HInstance, 'SQLITE64', RT_RCDATA);
  resource.SaveToFile(ExtractFilePath(ParamStr(0)) + '\sqlite3.dll');
  FreeAndNil(resource);
  {$IFEND}
end;

var
  Locale: TLocale;

begin
  Application.Initialize;
  Locale := TLocale.Create;
  ExtractResources;

  Application.Title := Locale.GetLocale(lsSYSTEMINFO, lvSysTitle);
  Application.CreateForm(TFormConexao, FormConexao);
  Application.CreateForm(TDM, DM);
  Application.Run;

  if Application.Terminated then
    Locale.Free;

end.
