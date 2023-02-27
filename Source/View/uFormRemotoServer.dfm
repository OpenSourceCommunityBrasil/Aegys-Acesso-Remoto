object FormRemotoServer: TFormRemotoServer
  OldCreateOrder = True
  OnCreate = ServiceCreate
  DisplayName = 'Servidor RedRemoto'
  BeforeInstall = ServiceBeforeInstall
  AfterInstall = ServiceAfterInstall
  BeforeUninstall = ServiceBeforeUninstall
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 240
  Width = 408
end
