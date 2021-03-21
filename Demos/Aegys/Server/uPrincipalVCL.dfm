object fServerControl: TfServerControl
  Left = 0
  Top = 0
  Caption = 'Servidor de Controle Remoto'
  ClientHeight = 381
  ClientWidth = 985
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object sbAcesso: TStatusBar
    Left = 0
    Top = 362
    Width = 985
    Height = 19
    BiDiMode = bdLeftToRight
    Panels = <>
    ParentBiDiMode = False
    SimplePanel = True
    SimpleText = #9#9
  end
  object Connections_ListView: TListView
    Left = 0
    Top = 0
    Width = 985
    Height = 362
    Align = alClient
    Columns = <
      item
        Caption = 'HandleConnection'
        Width = 100
      end
      item
        Caption = 'IP'
        Width = 170
      end
      item
        Caption = 'ID'
        Width = 100
      end
      item
        Caption = 'Password'
        Width = 100
      end
      item
        Caption = 'Target ID'
        Width = 100
      end
      item
        Caption = 'Ping'
        Width = 80
      end>
    GridLines = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
  end
  object MainMenu1: TMainMenu
    Left = 145
    Top = 47
    object Arquivo1: TMenuItem
      Caption = 'Arquivo'
      object ConfigurarServidor1: TMenuItem
        Caption = 'Configurar Servidor'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object LigarServidor1: TMenuItem
        Caption = 'Ligar Servidor'
        OnClick = LigarServidor1Click
      end
      object DesligarServidor1: TMenuItem
        Caption = 'Desligar Servidor'
        Enabled = False
        OnClick = DesligarServidor1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Sair1: TMenuItem
        Caption = 'Sair'
        OnClick = Sair1Click
      end
    end
  end
  object idTCPMain: TIdTCPServer
    Bindings = <>
    DefaultPort = 0
    Scheduler = IdSTDTCPMain
    Left = 117
    Top = 93
  end
  object IdSTDTCPMain: TIdSchedulerOfThreadDefault
    MaxThreads = 0
    Left = 145
    Top = 93
  end
  object idAFTCPMain: TIdAntiFreeze
    OnlyWhenIdle = False
    Left = 173
    Top = 93
  end
  object ipPSDeskTop: TUDPSuperServer
    Active = False
    Port = 8082
    TCPPort = 0
    BufferSize = 8192
    IPVersion = Id_IPv4
    TextEncoding = encUTF8
    Left = 344
    Top = 72
  end
  object ipPSFiles: TUDPSuperServer
    Active = False
    Port = 8081
    TCPPort = 0
    BufferSize = 8192
    IPVersion = Id_IPv4
    TextEncoding = encUTF8
    Left = 264
    Top = 48
  end
  object ipCommandsServer: TUDPSuperServer
    Active = False
    Port = 8079
    TCPPort = 8084
    BufferSize = 8192
    IPVersion = Id_IPv4
    TextEncoding = encUTF8
    Left = 264
    Top = 104
  end
  object Ping_Timer: TTimer
    Interval = 5000
    OnTimer = Ping_TimerTimer
    Left = 360
    Top = 128
  end
end
