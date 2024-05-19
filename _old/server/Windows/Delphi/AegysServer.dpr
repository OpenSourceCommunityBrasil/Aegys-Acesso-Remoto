program AegysServer;

uses
  Vcl.Forms,
  uServer in 'src\uServer.pas' {fServer},
  uConstants in '..\..\..\commons\uConstants.pas',
  uAegysBase in '..\..\..\Component\Base\uAegysBase.pas',
  uAegysBufferPack in '..\..\..\Component\Base\uAegysBufferPack.pas',
  uAegysConsts in '..\..\..\Component\Base\uAegysConsts.pas',
  uAegysDataTypes in '..\..\..\Component\Base\uAegysDataTypes.pas',
  uAegysZlib in '..\..\..\Component\Utils\uAegysZlib.pas',
  uAegysTools in '..\..\..\Component\Base\uAegysTools.pas',
  uAegysThreads in '..\..\..\Component\Base\uAegysThreads.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfServer, fServer);
  Application.Run;
end.
