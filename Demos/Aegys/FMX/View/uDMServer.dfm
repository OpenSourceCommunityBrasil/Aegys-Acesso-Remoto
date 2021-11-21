object DMServer: TDMServer
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 150
  Width = 215
  PixelsPerInch = 96
  object tLatencia: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = tLatenciaTimer
    Left = 80
    Top = 62
  end
end
