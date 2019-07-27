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
    Top = 426
    Width = 537
    Height = 13
    Align = alBottom
    Caption = 'Filter by window style'
    ExplicitWidth = 104
  end
  object MainListBox: TListBox
    Left = 0
    Top = 0
    Width = 543
    Height = 423
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
    ExplicitWidth = 503
  end
  object EnumerateButton: TButton
    Left = 0
    Top = 496
    Width = 543
    Height = 39
    Align = alBottom
    Caption = 'Enumerate windows'
    Default = True
    TabOrder = 1
    OnClick = EnumerateButtonClick
    ExplicitWidth = 503
  end
  object IncludeFilterPanel: TFlowPanel
    AlignWithMargins = True
    Left = 10
    Top = 474
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
    ExplicitWidth = 483
  end
  object ExcludeFilterPanel: TFlowPanel
    AlignWithMargins = True
    Left = 10
    Top = 447
    Width = 523
    Height = 17
    Margins.Left = 10
    Margins.Top = 5
    Margins.Right = 10
    Margins.Bottom = 5
    Align = alBottom
    Alignment = taLeftJustify
    BevelOuter = bvNone
    TabOrder = 3
    ExplicitWidth = 483
  end
end
