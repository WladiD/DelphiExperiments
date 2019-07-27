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
  Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TMainForm = class(TForm)
    MainListBox: TListBox;
    Button1: TButton;
    IncludeFilterPanel: TFlowPanel;
    Label1: TLabel;
    ExcludeFilterPanel: TFlowPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  public
    procedure AutoSizeFilterPanels;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

function AddWindowToList(Window: THandle; Target: Pointer): Boolean; stdcall;
var
  SL: TStrings absolute Target;

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
    SL.Add(Format('Handle: %d; Rect: %s; Text: %s', [Window, GetWindowRect, GetWindowText]));
end;

procedure TMainForm.FormCreate(Sender: TObject);

  procedure AddFilterCheckBox(ConstantName: string; Mask: NativeInt; CheckInclude: Boolean = False; CheckExclude: Boolean = False);

    function AddCheckBox(Parent: TWinControl; Tag: NativeInt): TCheckBox;
    begin
      Result := TCheckBox.Create(Self);
      Result.Parent := Parent;
      Result.Name := 'CB_' + Parent.Name + IntToStr(Parent.ControlCount);
      Result.Tag := Tag;
      Result.SetBounds(0, 0, MulDiv(160, Screen.PixelsPerInch, PixelsPerInch), MulDiv(17, Screen.PixelsPerInch, PixelsPerInch));
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

procedure TMainForm.Button1Click(Sender: TObject);
var
  SL: TStrings;
begin

  SL := MainListBox.Items;
  SL.BeginUpdate;
  try
    SL.Clear;
    EnumWindows(@AddWindowToList, NativeInt(SL));

    Caption := Format('Matched windows: %d', [SL.Count]);
  finally
    SL.EndUpdate;
  end;
end;

procedure TMainForm.AutoSizeFilterPanels;
begin
  ExcludeFilterPanel.AutoSize := True;
  ExcludeFilterPanel.AutoSize := False;
  IncludeFilterPanel.AutoSize := True;
  IncludeFilterPanel.AutoSize := False;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  AutoSizeFilterPanels;
end;

end.
