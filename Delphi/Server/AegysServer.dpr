program AegysServer;

uses
  Vcl.Forms,
  uServer in '..\..\Source\View\uServer.pas' {fServer},
  uAegysBufferPack in '..\..\Source\Base\uAegysBufferPack.pas',
  uAegysConsts in '..\..\Source\Base\uAegysConsts.pas',
  uAegysDataTypes in '..\..\Source\Base\uAegysDataTypes.pas',
  uAegysTools in '..\..\Source\Base\uAegysTools.pas',
  uAegysBase in '..\..\Source\Base\uAegysBase.pas',
  uAegysThreads in '..\..\Source\Base\uAegysThreads.pas',
  uConstants in '..\..\Source\Lib\uConstants.pas',
  uAegysZlib in '..\..\Source\Utils\uAegysZlib.pas',
  uSQLiteConfig in '..\..\Source\Lib\uSQLiteConfig.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfServer, fServer);
  Application.Run;
end.
