program AegysServer;

uses
  Vcl.Forms,
  uServer in 'src\uServer.pas' {fServer},
  uAegysBufferPack in '..\..\Component\Base\uAegysBufferPack.pas',
  uAegysConsts in '..\..\Component\Base\uAegysConsts.pas',
  uAegysDataTypes in '..\..\Component\Base\uAegysDataTypes.pas',
  uAegysTools in '..\..\Component\Base\uAegysTools.pas',
  uAegysBase in '..\..\Component\Base\uAegysBase.pas',
  uAegysThreads in '..\..\Component\Base\uAegysThreads.pas',
  uAegysZlib in '..\..\Component\Utils\uAegysZlib.pas',
  uConstants in '..\..\commons\uConstants.pas',
  uSQLiteConfig in '..\..\commons\uSQLiteConfig.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfServer, fServer);
  Application.Run;
end.
