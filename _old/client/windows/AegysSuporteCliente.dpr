program AegysSuporteCliente;



{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  ShellAPI,
  Windows,
  classes,
  SysUtils,
  uFormConexao in 'src\Views\uFormConexao.pas' {FormConexao},
  uFormTelaRemota in 'src\Views\uFormTelaRemota.pas' {FormTelaRemota},
  uFormChat in 'src\Views\uFormChat.pas' {FormChat},
  uDM in 'src\Views\uDM.pas' {DM: TDataModule},
  uFormConfig in 'src\Views\uFormConfig.pas' {fConfig},
  uFrameMensagemChat in 'src\Views\Frame\uFrameMensagemChat.pas' {FrameMensagemChat: TFrame},
  uFileTransfer in 'src\Views\uFileTransfer.pas' {fFileTransfer},
  ufrMonitorItem in 'src\Views\Frame\ufrMonitorItem.pas' {frMonitorItem: TFrame},
  uFormSenha in 'src\Views\uFormSenha.pas' {FormSenha},
  uLibClass in 'src\Classes\uLibClass.pas',
  uSendKeyClass in 'src\Classes\uSendKeyClass.pas',
  StreamManager in 'src\Classes\StreamManager.pas',
  Bcrypt in 'src\Classes\Bcrypt.pas',
  uFunctions in 'src\Classes\uFunctions.pas',
  uFilesFoldersOP in 'src\Classes\uFilesFoldersOP.pas',
  uIconsAssoc in 'src\Classes\uIconsAssoc.pas',
  uLocale in '..\..\commons\uLocale.pas',
  uConstants in '..\..\commons\uConstants.pas',
  Config.SQLite.FireDAC in '..\..\commons\Config.SQLite.FireDAC.pas',
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
  uAegysBase in '..\..\Component\Base\uAegysBase.pas',
  uAegysBufferPack in '..\..\Component\Base\uAegysBufferPack.pas',
  uAegysConsts in '..\..\Component\Base\uAegysConsts.pas',
  uAegysDataTypes in '..\..\Component\Base\uAegysDataTypes.pas',
  uAegysThreads in '..\..\Component\Base\uAegysThreads.pas',
  uAegysTools in '..\..\Component\Base\uAegysTools.pas',
  uAegysClientMotor in '..\..\Component\Base\uAegysClientMotor.pas',
  uAegysZlib in '..\..\Component\Utils\uAegysZlib.pas';

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
