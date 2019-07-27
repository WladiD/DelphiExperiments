unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  TMainForm = class(TForm)
    MainListBox: TListBox;
    EnumerateButton: TButton;
    IncludeFilterPanel: TFlowPanel;
    FilterLabel: TLabel;
    ExcludeFilterPanel: TFlowPanel;
    EnumeratePanel: TPanel;
    Panel1: TPanel;
    AutoUpdateCheckBox: TCheckBox;
    AutoUpdateTimer: TTimer;
    procedure EnumerateButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AutoUpdateTimerTimer(Sender: TObject);
    procedure AutoUpdateCheckBoxClick(Sender: TObject);

  private
    WinList: TStrings;

  public
    procedure AutoSizeFilterPanels;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

function AddWindowToList(Window: THandle; Target: Pointer): Boolean; stdcall;
var
  MainForm: TMainForm absolute Target;

  function GetWindowText: string;
  var
    TextLength: Integer;
  begin
    TextLength := Winapi.Windows.GetWindowTextLength(Window);
    if TextLength > 0 then
    begin
      SetLength(Result, TextLength);
      Winapi.Windows.GetWindowText(Window, PChar(Result), TextLength + 1);
    end
    else
      Result := '';
  end;

  function GetWindowRect: string;
  var
    Rect: TRect;
  begin
    if Winapi.Windows.GetWindowRect(Window, Rect) then
    begin
      if Rect.IsEmpty then
        Result := '(IsEmpty)'
      else
        Result := Format('%d, %d, %d, %d', [Rect.Left, Rect.Top, Rect.Right, Rect.Bottom]);
    end;
  end;

var
  WindowStyle: NativeInt;

  function HasStyle(CheckMask: FixedUInt): Boolean;
  begin
    Result := (WindowStyle and CheckMask) = CheckMask;
  end;

  function GetCheckedMask(ContainerPanel: TWinControl): NativeInt;
  var
    FilterControl: TControl;
    FilterCheckBox: TCheckBox absolute FilterControl;
    cc: Integer;
  begin
    Result := 0;
    for cc := 0 to ContainerPanel.ControlCount - 1 do
    begin
      FilterControl := ContainerPanel.Controls[cc];
      if (FilterControl is TCheckBox) and FilterCheckBox.Checked and (FilterCheckBox.Tag > 0) then
        Result := Result or FilterCheckBox.Tag;
    end;
  end;

var
  IncludeMask, ExcludeMask: NativeInt;
begin
  Result := True;
  WindowStyle := GetWindowLong(Window, GWL_STYLE);

  ExcludeMask := GetCheckedMask(MainForm.ExcludeFilterPanel);

  if (ExcludeMask > 0) and HasStyle(ExcludeMask) then
    Exit;

  IncludeMask := GetCheckedMask(MainForm.IncludeFilterPanel);

  if (IncludeMask = 0) or HasStyle(IncludeMask) then
    MainForm.WinList.AddObject(
      Format('Handle: %d; Rect: %s; Text: %s', [Window, GetWindowRect, GetWindowText]),
      TObject(Window));
end;

procedure TMainForm.FormCreate(Sender: TObject);

  procedure AddFilterCheckBox(ConstantName: string; Mask: NativeInt;
    CheckInclude: Boolean = False; CheckExclude: Boolean = False);

    function AddCheckBox(Parent: TWinControl; Tag: NativeInt): TCheckBox;
    begin
      Result := TCheckBox.Create(Self);
      Result.Parent := Parent;
      Result.Name := 'CB_' + Parent.Name + IntToStr(Parent.ControlCount);
      Result.Tag := Tag;
      Result.SetBounds(0, 0, MulDiv(165, Screen.PixelsPerInch, PixelsPerInch),
        MulDiv(17, Screen.PixelsPerInch, PixelsPerInch));
    end;

  var
    CB: TCheckBox;
  begin
    CB := AddCheckBox(IncludeFilterPanel, Mask);
    CB.Caption := 'Include ' + ConstantName;
    CB.Checked := CheckInclude;

    CB := AddCheckBox(ExcludeFilterPanel, Mask);
    CB.Caption := 'Exclude ' + ConstantName;
    CB.Checked := CheckExclude;
  end;

begin
  AddFilterCheckBox('WS_CAPTION', WS_CAPTION);
  AddFilterCheckBox('WS_CHILD', WS_CHILD);
  AddFilterCheckBox('WS_DISABLED', WS_DISABLED, False, True);
  AddFilterCheckBox('WS_POPUP', WS_POPUP);
  AddFilterCheckBox('WS_SYSMENU', WS_SYSMENU, True);
  AddFilterCheckBox('WS_TILEDWINDOW', WS_TILEDWINDOW);
  AddFilterCheckBox('WS_VISIBLE', WS_VISIBLE, True);

  AutoSizeFilterPanels;
end;

procedure TMainForm.AutoUpdateCheckBoxClick(Sender: TObject);
begin
  AutoUpdateTimer.Enabled := AutoUpdateCheckBox.Checked;
end;

procedure TMainForm.AutoUpdateTimerTimer(Sender: TObject);
begin
  EnumerateButton.Click;
end;

procedure TMainForm.EnumerateButtonClick(Sender: TObject);
var
  SelHandle: TObject;
begin
  WinList := TStringList.Create;
  try
    EnumWindows(@AddWindowToList, NativeInt(Self));

    if WinList.Equals(MainListBox.Items) then
      Exit;

    if MainListBox.ItemIndex >= 0 then
      SelHandle := MainListBox.Items.Objects[MainListBox.ItemIndex]
    else
      SelHandle := nil;

    MainListBox.Items.Assign(WinList);

    if Assigned(SelHandle) then
      MainListBox.ItemIndex := MainListBox.Items.IndexOfObject(SelHandle);

    Caption := Format('Matched windows: %d', [WinList.Count]);
  finally
    FreeAndNil(WinList);
  end;
end;

procedure TMainForm.AutoSizeFilterPanels;
begin
  ExcludeFilterPanel.AutoSize := True;
  ExcludeFilterPanel.AutoSize := False;
  IncludeFilterPanel.AutoSize := True;
  IncludeFilterPanel.AutoSize := False;

  // The bottom order can be jumbled after AutoSize, so we must correct it
  FilterLabel.Top := MainListBox.Top + MainListBox.Height + 5;
  ExcludeFilterPanel.Top := FilterLabel.Top + FilterLabel.Height + 5;
  IncludeFilterPanel.Top := ExcludeFilterPanel.Top + ExcludeFilterPanel.Height + 5;
  EnumeratePanel.Top := IncludeFilterPanel.Top + IncludeFilterPanel.Height + 5;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  AutoSizeFilterPanels;
end;

end.
