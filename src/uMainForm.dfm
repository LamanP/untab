object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Untab'
  ClientHeight = 336
  ClientWidth = 1126
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 244
    Width = 1126
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 177
    ExplicitWidth = 159
  end
  object lbFiles: TListBox
    Left = 0
    Top = 0
    Width = 1126
    Height = 184
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
  end
  object memoLog: TMemo
    Left = 0
    Top = 247
    Width = 1126
    Height = 89
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    WordWrap = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 184
    Width = 1126
    Height = 60
    Align = alBottom
    BevelEdges = []
    TabOrder = 2
    object Label1: TLabel
      Left = 8
      Top = 13
      Width = 325
      Height = 13
      Caption = 
        'Drag files into this window and press '#39'Go'#39' to convert tabs to sp' +
        'aces.'
    end
    object Label2: TLabel
      Left = 352
      Top = 13
      Width = 43
      Height = 13
      Caption = 'Tab size:'
    end
    object editTabsize: TEdit
      Left = 408
      Top = 10
      Width = 121
      Height = 21
      TabOrder = 0
      Text = '4'
    end
    object btnGo: TButton
      Left = 544
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Go'
      TabOrder = 1
      OnClick = btnGoClick
    end
    object CBBackup: TCheckBox
      Left = 89
      Top = 37
      Width = 89
      Height = 17
      Caption = 'Make backup'
      TabOrder = 2
    end
    object BtnBrowse: TButton
      Left = 8
      Top = 32
      Width = 75
      Height = 25
      Caption = 'Browse'
      TabOrder = 3
      OnClick = BtnBrowseClick
    end
    object CbTrim: TCheckBox
      Left = 352
      Top = 37
      Width = 186
      Height = 17
      Caption = 'Remove trailing spaces from lines'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
  end
  object OpenDialog: TOpenDialog
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 104
    Top = 96
  end
end
