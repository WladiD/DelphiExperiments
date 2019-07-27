object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 535
  ClientWidth = 543
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object FilterLabel: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 418
    Width = 537
    Height = 13
    Align = alBottom
    Caption = 'Filter by window style'
    ExplicitTop = 426
    ExplicitWidth = 104
  end
  object MainListBox: TListBox
    Left = 0
    Top = 0
    Width = 543
    Height = 415
    Align = alClient
    DoubleBuffered = True
    ItemHeight = 13
    ParentDoubleBuffered = False
    TabOrder = 0
    ExplicitWidth = 503
    ExplicitHeight = 423
  end
  object IncludeFilterPanel: TFlowPanel
    AlignWithMargins = True
    Left = 10
    Top = 466
    Width = 523
    Height = 17
    Margins.Left = 10
    Margins.Top = 5
    Margins.Right = 10
    Margins.Bottom = 5
    Align = alBottom
    Alignment = taLeftJustify
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 474
    ExplicitWidth = 483
  end
  object ExcludeFilterPanel: TFlowPanel
    AlignWithMargins = True
    Left = 10
    Top = 439
    Width = 523
    Height = 17
    Margins.Left = 10
    Margins.Top = 5
    Margins.Right = 10
    Margins.Bottom = 5
    Align = alBottom
    Alignment = taLeftJustify
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 447
    ExplicitWidth = 483
  end
  object EnumeratePanel: TPanel
    Left = 0
    Top = 488
    Width = 543
    Height = 47
    Align = alBottom
    BevelEdges = [beTop]
    BevelKind = bkSoft
    BevelOuter = bvNone
    TabOrder = 3
    object EnumerateButton: TButton
      Left = 113
      Top = 0
      Width = 430
      Height = 45
      Align = alClient
      Caption = 'Enumerate windows'
      Default = True
      TabOrder = 0
      OnClick = EnumerateButtonClick
      ExplicitLeft = 0
      ExplicitTop = 449
      ExplicitWidth = 543
      ExplicitHeight = 39
    end
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 113
      Height = 45
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
      object AutoUpdateCheckBox: TCheckBox
        Left = 3
        Top = 16
        Width = 97
        Height = 17
        Caption = 'Auto update'
        TabOrder = 0
        OnClick = AutoUpdateCheckBoxClick
      end
    end
  end
  object AutoUpdateTimer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = AutoUpdateTimerTimer
    Left = 88
    Top = 488
  end
end
