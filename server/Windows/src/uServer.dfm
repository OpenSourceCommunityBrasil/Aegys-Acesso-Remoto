object fServer: TfServer
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Aegys Support Server'
  ClientHeight = 458
  ClientWidth = 707
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 0
    Top = 0
    Width = 707
    Height = 458
    Align = alClient
    DataSource = DataSource1
    DrawingStyle = gdsGradient
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'PROTOCOLO'
        Title.Caption = 'Connection'
        Width = 163
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ID'
        Width = 140
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'SENHA'
        Title.Caption = 'Senha'
        Width = 116
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'SENHA2'
        Title.Caption = 'Senha Gerada'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'LATENCIA'
        Title.Caption = 'Lat'#234'ncia'
        Width = 60
        Visible = True
      end>
  end
  object DataSource1: TDataSource
    DataSet = QryConexoes
    Left = 64
    Top = 160
  end
  object QryConexoes: TFDMemTable
    FieldDefs = <>
    IndexDefs = <>
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    StoreDefs = True
    Left = 64
    Top = 96
    object QryConexoesPROTOCOLO: TStringField
      FieldName = 'PROTOCOLO'
      Size = 50
    end
    object QryConexoesID: TStringField
      FieldName = 'ID'
      Size = 11
    end
    object QryConexoesSENHA: TStringField
      FieldName = 'SENHA'
    end
    object QryConexoesSENHA2: TStringField
      FieldName = 'SENHA2'
    end
    object QryConexoesLATENCIA: TStringField
      FieldName = 'LATENCIA'
      Size = 10
    end
  end
  object tReload: TTimer
    Interval = 5000
    OnTimer = tReloadTimer
    Left = 320
    Top = 112
  end
end
