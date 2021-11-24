program AegysSuporteServiceServer;

uses
  Vcl.SvcMgr,
  uFormRemotoServer in 'View\uFormRemotoServer.pas' {FormRemotoServer: TService},
  uCtrl_ThreadsService in 'Control\uCtrl_ThreadsService.pas',
  uCtrl_Conexoes in 'Control\uCtrl_Conexoes.pas',
  uDMServer in 'View\uDMServer.pas' {DMServer: TDataModule},
  uConstants in 'Lib\uConstants.pas';

{$R *.RES}

begin
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TFormRemotoServer, FormRemotoServer);
  Application.CreateForm(TDMServer, DMServer);
  Application.Run;
end.
