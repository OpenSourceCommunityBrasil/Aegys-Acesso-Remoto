object Form2: TForm2
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Chat'
  ClientHeight = 325
  ClientWidth = 661
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
  object Label1: TLabel
    Left = 76
    Top = 18
    Width = 22
    Height = 13
    Caption = 'Host'
  end
  object Label2: TLabel
    Left = 78
    Top = 43
    Width = 20
    Height = 13
    Caption = 'Port'
  end
  object Label3: TLabel
    Left = 51
    Top = 68
    Width = 47
    Height = 13
    Caption = 'SessionID'
  end
  object Label4: TLabel
    Left = 39
    Top = 93
    Width = 59
    Height = 13
    Caption = 'SessionPWD'
  end
  object Label5: TLabel
    Left = 49
    Top = 118
    Width = 49
    Height = 13
    Caption = 'FixedPWD'
  end
  object Label6: TLabel
    Left = 18
    Top = 143
    Width = 80
    Height = 13
    Caption = 'ConnectTimeOut'
  end
  object Label7: TLabel
    Left = 18
    Top = 168
    Width = 80
    Height = 13
    Caption = 'RequestTimeOut'
  end
  object Label8: TLabel
    Left = 16
    Top = 196
    Width = 82
    Height = 13
    Caption = 'Peers Connected'
  end
  object Label9: TLabel
    Left = 314
    Top = 43
    Width = 19
    Height = 13
    Caption = 'Msg'
  end
  object Label10: TLabel
    Left = 286
    Top = 69
    Width = 47
    Height = 13
    Caption = 'Response'
  end
  object iImgSend: TImage
    Left = 337
    Top = 192
    Width = 276
    Height = 121
    Stretch = True
  end
  object sbSendMSG: TSpeedButton
    Left = 616
    Top = 39
    Width = 23
    Height = 22
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333333333333333333FFFFFFFFFFFFFFF000000000000
      000077777777777777770FFFFFFFFFFFFFF07F3333FFF33333370FFFF777FFFF
      FFF07F333777333333370FFFFFFFFFFFFFF07F3333FFFFFF33370FFFF777777F
      FFF07F33377777733FF70FFFFFFFFFFF99907F3FFF33333377770F777FFFFFFF
      9CA07F77733333337F370FFFFFFFFFFF9A907FFFFFFFFFFF7FF7000000000000
      0000777777777777777733333333333333333333333333333333333333333333
      3333333333333333333333333333333333333333333333333333333333333333
      3333333333333333333333333333333333333333333333333333}
    NumGlyphs = 2
  end
  object sbSendIMG: TSpeedButton
    Left = 616
    Top = 192
    Width = 23
    Height = 22
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00300000000000
      0000377777777777777707FFFFFFFFFFFF70773FF33333333F770F77FFFFFFFF
      77F07F773FFFFFFF77F70FFF7700000000007F337777777777770FFFFF0BBBBB
      BBB07F333F7F3FF33FF70FFF700B00BB00B07F3F777F77F377370F707F0BB0B0
      0BB07F77337F37F77337007EEE0BB0B0BBB077FFFF7F37F7F3370777770EE000
      EEE07777777F3777F3F7307EEE0E0E00E0E03773FF7F7377F73733707F0EE000
      0EE03337737F377773373333700EEE00EEE03333377F3377FF373333330EEEE0
      0EE03333337F33377F373333330EEEE00EE03333337F333773373333330EEEEE
      EEE03333337FFFFFFFF733333300000000003333337777777777}
    NumGlyphs = 2
  end
  object Label11: TLabel
    Left = 322
    Top = 18
    Width = 11
    Height = 13
    Caption = 'ID'
  end
  object sbIDConn: TSpeedButton
    Left = 616
    Top = 15
    Width = 23
    Height = 21
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00300000333300
      0000377777FFFF7777770FFFF099990FFFF07FFFF777777FFFF7000000333300
      00007777773333777777307703333330770337FF7F333337FF7F300003333330
      0003377773333337777333993333333399333F7FFFF3333FF7FF000000333300
      0000777777F3337777770FFFF033330FFFF07FFFF7F3337FFFF7000000333300
      00007777773333777777307703333330770337FF7F333337FF7F300003333330
      0003377773FFFFF777733393300000033933337F3777777F37F3339990FFFF09
      99333373F7FFFF7FF73333399000000993333337777777777333333333077033
      33333333337FF7F3333333333300003333333333337777333333}
    NumGlyphs = 2
    OnClick = sbIDConnClick
  end
  object Label12: TLabel
    Left = 462
    Top = 18
    Width = 46
    Height = 13
    Caption = 'Password'
  end
  object eHost: TEdit
    Left = 101
    Top = 15
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '192.168.15.10'
  end
  object ePort: TEdit
    Left = 101
    Top = 40
    Width = 47
    Height = 21
    TabOrder = 1
    Text = '9092'
  end
  object eSessionID: TEdit
    Left = 101
    Top = 65
    Width = 121
    Height = 21
    ReadOnly = True
    TabOrder = 2
  end
  object eSessionPWD: TEdit
    Left = 101
    Top = 90
    Width = 121
    Height = 21
    ReadOnly = True
    TabOrder = 3
  end
  object eFixedPWD: TEdit
    Left = 101
    Top = 115
    Width = 75
    Height = 21
    TabOrder = 4
    Text = '1234'
  end
  object cbAcceptUnAssist: TCheckBox
    Left = 181
    Top = 118
    Width = 97
    Height = 17
    Caption = 'AcceptUnAssist'
    TabOrder = 5
  end
  object eConnectTimeOut: TEdit
    Left = 101
    Top = 140
    Width = 75
    Height = 21
    TabOrder = 6
    Text = '10000'
  end
  object ERequestTimeOut: TEdit
    Left = 101
    Top = 165
    Width = 75
    Height = 21
    TabOrder = 7
    Text = '30000'
  end
  object bConnect: TButton
    Left = 181
    Top = 161
    Width = 91
    Height = 25
    Caption = 'Connect'
    TabOrder = 8
    OnClick = bConnectClick
  end
  object lbPeersConnected: TListBox
    Left = 101
    Top = 192
    Width = 171
    Height = 122
    ItemHeight = 13
    TabOrder = 9
  end
  object eMessage: TEdit
    Left = 337
    Top = 40
    Width = 276
    Height = 21
    TabOrder = 10
  end
  object mReply: TMemo
    Left = 337
    Top = 67
    Width = 276
    Height = 114
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 11
  end
  object eIDConn: TEdit
    Left = 337
    Top = 15
    Width = 121
    Height = 21
    TabOrder = 12
  end
  object eLoginPass: TEdit
    Left = 512
    Top = 15
    Width = 101
    Height = 21
    TabOrder = 13
  end
end
