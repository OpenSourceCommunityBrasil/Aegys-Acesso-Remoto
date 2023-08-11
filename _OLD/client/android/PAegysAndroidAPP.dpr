program PAegysAndroidAPP;

uses
  System.StartUpCopy,
  FMX.Forms,
  UPrincipal in 'src\UPrincipal.pas' {frmPrincipal},
  uAegysBase in '..\..\Component\Base\uAegysBase.pas',
  uConstants in '..\..\commons\uConstants.pas',
  uSQLiteConfig in '..\..\commons\uSQLiteConfig.pas',
  uAegysBufferPack in '..\..\component\base\uAegysBufferPack.pas',
  uAegysConsts in '..\..\component\base\uAegysConsts.pas',
  uAegysDataTypes in '..\..\component\base\uAegysDataTypes.pas',
  uAegysThreads in '..\..\component\base\uAegysThreads.pas',
  uAegysTools in '..\..\component\base\uAegysTools.pas',
  uAegysZlib in '..\..\component\utils\uAegysZlib.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.Run;
end.
