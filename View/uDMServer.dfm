object DMServer: TDMServer
  OldCreateOrder = True
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 266
  Width = 341
  object tLatencia: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = tLatenciaTimer
    Left = 80
    Top = 62
  end
end
