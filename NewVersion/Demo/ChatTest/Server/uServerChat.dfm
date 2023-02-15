object Form3: TForm3
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Server'
  ClientHeight = 96
  ClientWidth = 167
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 38
    Top = 27
    Width = 20
    Height = 13
    Caption = 'Port'
  end
  object ePort: TEdit
    Left = 61
    Top = 24
    Width = 47
    Height = 21
    TabOrder = 0
    Text = '9092'
  end
  object bConnect: TButton
    Left = 38
    Top = 51
    Width = 91
    Height = 25
    Caption = 'Connect'
    TabOrder = 1
    OnClick = bConnectClick
  end
end
