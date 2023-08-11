object Form3: TForm3
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Aegys Support Server'
  ClientHeight = 153
  ClientWidth = 304
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
  object lClientsConnect: TLabel
    Left = 128
    Top = 52
    Width = 50
    Height = 23
    Caption = '00000'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 80
    Top = 23
    Width = 149
    Height = 23
    Caption = 'Clients Connected'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object bActive: TButton
    Left = 80
    Top = 96
    Width = 145
    Height = 25
    Caption = 'Active Server'
    TabOrder = 0
    OnClick = bActiveClick
  end
  object FDPhysFBDriverLink1: TFDPhysFBDriverLink
    Left = 48
    Top = 48
  end
end
