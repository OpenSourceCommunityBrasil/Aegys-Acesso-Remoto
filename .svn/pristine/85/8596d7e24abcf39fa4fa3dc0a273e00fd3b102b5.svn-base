object FormPrincipal: TFormPrincipal
  Left = 192
  Top = 125
  Caption = 'Servidor Suporte Remoto'
  ClientHeight = 550
  ClientWidth = 742
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 17
  object Splitter1: TSplitter
    Left = 0
    Top = 384
    Width = 742
    Height = 3
    Cursor = crVSplit
    Align = alBottom
  end
  object memLog: TMemo
    Left = 0
    Top = 387
    Width = 742
    Height = 163
    Align = alBottom
    Lines.Strings = (
      'Exceptions Log:'
      '')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object lstConexoes: TListView
    Left = 0
    Top = 0
    Width = 742
    Height = 384
    Align = alClient
    Columns = <
      item
        Caption = 'Conex'#227'o'
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
        Caption = 'Senha'
        Width = 100
      end
      item
        Caption = 'ID do parceiro'
        Width = 100
      end
      item
        Caption = 'Lat'#234'ncia'
        Width = 80
      end>
    GridLines = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
  end
  object tLatencia: TTimer
    Interval = 5000
    OnTimer = tLatenciaTimer
    Left = 360
    Top = 128
  end
end
