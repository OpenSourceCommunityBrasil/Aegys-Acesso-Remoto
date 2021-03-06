program AegysSuporteServer;

uses
  Vcl.Forms,
  uFormServidor in 'View\uFormServidor.pas' {FormServidor} ,
  uDMServer in 'View\uDMServer.pas' {DMServer: TDataModule} ,
  uCtrl_Conexoes in 'Control\uCtrl_Conexoes.pas',
  uCtrl_ThreadsService in 'Control\uCtrl_ThreadsService.pas',
  uConstants in 'Lib\uConstants.pas';

{$R *.res}


begin
  Application.Initialize;
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormServidor, FormServidor);
  Application.Run;

end.
