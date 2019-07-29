unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ActiveX,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Menus;

const
  CLSID_VirtualDesktopManager: TGUID = '{AA509086-5CA9-4C25-8F95-589D3C07B48A}';
  IID_VirtualDesktopManager: TGUID = '{A5CD92FF-29BE-454C-8D04-D82879FB3F1B}';

type
  IVirtualDesktopManager = interface(IUnknown)
    ['{A5CD92FF-29BE-454C-8D04-D82879FB3F1B}']
    function IsWindowOnCurrentVirtualDesktop(Wnd: HWND; pIsTrue: PBOOL): HResult; stdcall;
    function GetWindowDesktopId(Wnd: HWND; pDesktopID: PGUID): HResult; stdcall;
    function MoveWindowToDesktop(Wnd: HWND; const DesktopID: TGUID): HResult; stdcall;
  end;

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
    OptionsPanel: TPanel;
    FilterOverlappedWindowsCheckBox: TCheckBox;
    OnlyCurrendVDCheckBox: TCheckBox;
    Label1: TLabel;
    procedure EnumerateButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AutoUpdateTimerTimer(Sender: TObject);
    procedure AutoUpdateCheckBoxClick(Sender: TObject);

  private
    FWinList: TStrings;
    FVirtualDesktopAvailable: Boolean;
    FVirtualDesktopManager: IVirtualDesktopManager;

    procedure AutoSizeFilterPanels;
    procedure FilterHiddenWindows(List: TStrings);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

function AddWindowToList(Window: THandle; Target: Pointer): Boolean; stdcall;
var
  MainForm: TMainForm absolute Target;
  WindowStyle: NativeInt;
  IncludeMask, ExcludeMask: NativeInt;
  WindowMatch, WinOnCurrentDesktop: Boolean;

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

begin
  Result := True;
  WindowStyle := GetWindowLong(Window, GWL_STYLE);

  ExcludeMask := GetCheckedMask(MainForm.ExcludeFilterPanel);

  if (ExcludeMask > 0) and HasStyle(ExcludeMask) then
    Exit;

  IncludeMask := GetCheckedMask(MainForm.IncludeFilterPanel);
  WindowMatch := (IncludeMask = 0) or HasStyle(IncludeMask);

  if WindowMatch then
  begin
    if MainForm.FVirtualDesktopAvailable and MainForm.OnlyCurrendVDCheckBox.Checked and
      Succeeded(MainForm.FVirtualDesktopManager.IsWindowOnCurrentVirtualDesktop(Window, @WinOnCurrentDesktop)) and
      not WinOnCurrentDesktop then
      Exit;

    MainForm.FWinList.AddObject(
      Format('Handle: %d; Rect: %s; Text: %s', [Window, GetWindowRect, GetWindowText]),
      TObject(Window));
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);

  procedure AddFilterCheckBox(ConstantName: string; Mask: NativeUInt;
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
  AddFilterCheckBox('WS_CAPTION', WS_CAPTION, True);
  AddFilterCheckBox('WS_CHILD', WS_CHILD);
  AddFilterCheckBox('WS_DISABLED', WS_DISABLED, False, True);
  AddFilterCheckBox('WS_POPUP', WS_POPUP);
  AddFilterCheckBox('WS_SYSMENU', WS_SYSMENU);
  AddFilterCheckBox('WS_TILEDWINDOW', WS_TILEDWINDOW);
  AddFilterCheckBox('WS_VISIBLE', WS_VISIBLE, True);

  AutoSizeFilterPanels;

  FVirtualDesktopAvailable := TOSVersion.Check(6, 3) and // Windows 10
    Succeeded(CoCreateInstance(CLSID_VirtualDesktopManager, nil, CLSCTX_INPROC_SERVER, IID_VirtualDesktopManager, FVirtualDesktopManager));

  AutoUpdateCheckBox.Checked := True;
end;

procedure TMainForm.AutoUpdateCheckBoxClick(Sender: TObject);
begin
  AutoUpdateTimer.Enabled := AutoUpdateCheckBox.Checked;
end;

procedure TMainForm.AutoUpdateTimerTimer(Sender: TObject);
begin
  EnumerateButton.Click;
end;

procedure TMainForm.FilterHiddenWindows(List: TStrings);

  function IsWindowEffectiveVisible(Index: Integer): Boolean;
  var
    cc: Integer;
    RefHandle, TestHandle: HWND;
    RefRect, TestRect: TRect;
    RefRegion, TestRegion: HRGN;
    CombineResult: Integer;
  begin
    Result := False;
    RefHandle := HWND(List.Objects[Index]);

    if not Winapi.Windows.GetWindowRect(RefHandle, RefRect) and RefRect.IsEmpty then
      Exit;

    RefRegion := CreateRectRgnIndirect(RefRect);
    try
      for cc := Index - 1 downto 0 do
      begin
        TestHandle := HWND(List.Objects[cc]);
        if not IsIconic(TestHandle) and Winapi.Windows.GetWindowRect(TestHandle, TestRect) and
          not TestRect.IsEmpty then
        begin
          TestRegion := CreateRectRgnIndirect(TestRect);
          try
            CombineResult := CombineRgn(RefRegion, RefRegion, TestRegion, RGN_DIFF);
            if CombineResult = NULLREGION then
              Exit;
          finally
            DeleteObject(TestRegion);
          end;
        end;
      end;
    finally
      DeleteObject(RefRegion);
    end;

    Result := True;
  end;

var
  cc: Integer;
begin
  for cc := List.Count - 1 downto 1 do
    if not IsWindowEffectiveVisible(cc) then
      List.Delete(cc);
end;

procedure TMainForm.EnumerateButtonClick(Sender: TObject);
var
  SelHandle: TObject;
begin
  FWinList := TStringList.Create;
  try
    EnumWindows(@AddWindowToList, NativeInt(Self));

    if MainListBox.ItemIndex >= 0 then
      SelHandle := MainListBox.Items.Objects[MainListBox.ItemIndex]
    else
      SelHandle := nil;

    if FilterOverlappedWindowsCheckBox.Checked then
      FilterHiddenWindows(FWinList);

    if FWinList.Equals(MainListBox.Items) then
      Exit;

    MainListBox.Items.Assign(FWinList);

    if Assigned(SelHandle) then
      MainListBox.ItemIndex := MainListBox.Items.IndexOfObject(SelHandle);

    Caption := Format('Matched windows: %d', [FWinList.Count]);
  finally
    FreeAndNil(FWinList);
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
  OptionsPanel.Top := IncludeFilterPanel.Top + IncludeFilterPanel.Height + 5;
  EnumeratePanel.Top := OptionsPanel.Top + OptionsPanel.Height + 5;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  AutoSizeFilterPanels;
end;

end.
