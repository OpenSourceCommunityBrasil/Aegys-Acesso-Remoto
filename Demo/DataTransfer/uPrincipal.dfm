object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 440
  ClientWidth = 772
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 27
    Top = 51
    Width = 32
    Height = 13
    Caption = 'Owner'
  end
  object Label2: TLabel
    Left = 37
    Top = 78
    Width = 22
    Height = 13
    Caption = 'Dest'
  end
  object Label3: TLabel
    Left = 9
    Top = 303
    Width = 49
    Height = 13
    Caption = 'BufferSize'
  end
  object Label4: TLabel
    Left = 395
    Top = 51
    Width = 32
    Height = 13
    Caption = 'Owner'
  end
  object Label5: TLabel
    Left = 405
    Top = 78
    Width = 22
    Height = 13
    Caption = 'Dest'
  end
  object Label6: TLabel
    Left = 377
    Top = 303
    Width = 49
    Height = 13
    Caption = 'BufferSize'
  end
  object Button1: TButton
    Left = 64
    Top = 342
    Width = 75
    Height = 25
    Caption = 'Build'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 440
    Top = 342
    Width = 75
    Height = 25
    Caption = 'Read'
    TabOrder = 1
    OnClick = Button2Click
  end
  object eOwner: TEdit
    Left = 64
    Top = 48
    Width = 121
    Height = 21
    TabOrder = 2
    Text = '0.0.0.0'
  end
  object eDest: TEdit
    Left = 64
    Top = 75
    Width = 121
    Height = 21
    TabOrder = 3
    Text = '1.1.1.1'
  end
  object rgPackDest: TRadioGroup
    Left = 64
    Top = 99
    Width = 121
    Height = 61
    Caption = 'PackDest'
    ItemIndex = 0
    Items.Strings = (
      'Direct'
      'Proxy')
    TabOrder = 4
  end
  object rgDataCheck: TRadioGroup
    Left = 64
    Top = 166
    Width = 121
    Height = 61
    Caption = 'DataCheck'
    ItemIndex = 0
    Items.Strings = (
      'Sync'
      'Async')
    TabOrder = 5
  end
  object rgCommandType: TRadioGroup
    Left = 191
    Top = 237
    Width = 121
    Height = 128
    Caption = 'Command Type'
    ItemIndex = 0
    Items.Strings = (
      'ScreenCapture'
      'Audio'
      'Video'
      'Keyboard'
      'Mouse'
      'FileTransfer')
    TabOrder = 6
    Visible = False
  end
  object rgDataType: TRadioGroup
    Left = 64
    Top = 233
    Width = 121
    Height = 61
    Caption = 'DataType'
    ItemIndex = 0
    Items.Strings = (
      'String'
      'Bytes')
    TabOrder = 7
    OnClick = rgDataTypeClick
  end
  object eValue: TEdit
    Left = 191
    Top = 240
    Width = 177
    Height = 21
    TabOrder = 8
  end
  object eBufferSize: TEdit
    Left = 64
    Top = 300
    Width = 65
    Height = 21
    TabOrder = 9
    Text = '0'
  end
  object eBytesOptions: TEdit
    Left = 191
    Top = 371
    Width = 177
    Height = 21
    TabOrder = 10
    Visible = False
  end
  object eLoadOwner: TEdit
    Left = 432
    Top = 48
    Width = 121
    Height = 21
    TabOrder = 11
  end
  object eLoadDest: TEdit
    Left = 432
    Top = 75
    Width = 121
    Height = 21
    TabOrder = 12
  end
  object rgLoadPackDest: TRadioGroup
    Left = 432
    Top = 99
    Width = 121
    Height = 61
    Caption = 'PackDest'
    ItemIndex = 0
    Items.Strings = (
      'Direct'
      'Proxy')
    TabOrder = 13
  end
  object rgLoadDataCheck: TRadioGroup
    Left = 432
    Top = 166
    Width = 121
    Height = 61
    Caption = 'DataCheck'
    ItemIndex = 0
    Items.Strings = (
      'Sync'
      'Async')
    TabOrder = 14
  end
  object rgLoadCommandType: TRadioGroup
    Left = 559
    Top = 237
    Width = 121
    Height = 128
    Caption = 'Command Type'
    ItemIndex = 0
    Items.Strings = (
      'ScreenCapture'
      'Audio'
      'Video'
      'Keyboard'
      'Mouse'
      'FileTransfer')
    TabOrder = 15
    Visible = False
  end
  object rgLoadDataType: TRadioGroup
    Left = 432
    Top = 233
    Width = 121
    Height = 61
    Caption = 'DataType'
    ItemIndex = 0
    Items.Strings = (
      'String'
      'Bytes')
    TabOrder = 16
    OnClick = rgLoadDataTypeClick
  end
  object eLoadValue: TEdit
    Left = 559
    Top = 240
    Width = 177
    Height = 21
    TabOrder = 17
  end
  object eLoadBufferSize: TEdit
    Left = 432
    Top = 300
    Width = 65
    Height = 21
    TabOrder = 18
    Text = '0'
  end
  object eLoadBytesOptions: TEdit
    Left = 559
    Top = 371
    Width = 177
    Height = 21
    TabOrder = 19
    Visible = False
  end
  object odFile: TOpenDialog
    Left = 200
    Top = 168
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '*.buf'
    Filter = 'Arquivos de Buffer|*.buf'
    Title = 'Arquivos de Buffer'
    Left = 264
    Top = 104
  end
  object odbuf: TOpenDialog
    DefaultExt = '*.buf'
    Filter = 'Arquivos de Buffer|*.buf'
    Left = 496
    Top = 184
  end
end
