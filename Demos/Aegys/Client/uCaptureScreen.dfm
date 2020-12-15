object dmCaptureScreen: TdmCaptureScreen
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 150
  Width = 215
  object FastDesktop: TJDRMDesktop
    Compression = 100
    Active = False
    VerticalCount = 4
    HorizontalCount = 4
    PixelFormat = pf8bit
    DrawCursor = False
    OnNewBlock = FastDesktopNewBlock
    Left = 48
    Top = 38
  end
end
