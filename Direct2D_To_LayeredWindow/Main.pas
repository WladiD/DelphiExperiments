unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Direct2D, Winapi.D2D1, Vcl.ExtCtrls, Winapi.Wincodec,
  Winapi.ActiveX, Winapi.DxgiFormat, System.Win.ComObj, System.Types;

type
  TMainForm = class(TForm)
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    RenderTarget: ID2D1RenderTarget;
    InteropRenderTarget: ID2D1GdiInteropRenderTarget;
    D2DFactory: ID2D1Factory;
    WICFactory: IWICImagingFactory;
    WICBitmap: IWICBitmap;
    EllipsePos: TD2D1Point2F;
    XDelta: Integer;
    YDelta: Integer;
    DeviceResourcesValid: Boolean;

    Blend: TBlendFunction;
    SourcePosition: TPoint;
    WindowPosition: TPoint;
    Size: TSize;

    procedure UpdateWindow(SourceDC: HDC);
    procedure PaintToRenderTarget(const RT: ID2D1RenderTarget);
    procedure WMSize(var Message: TWMSize); message WM_SIZE;

    procedure CreateDeviceResources;
    procedure InvalidateDeviceResources;
  end;

  TUpdateLayeredWindowInfo = record
    cbSize: DWORD;
    hdcDst: HDC;
    pptDst: PPoint;
    psize: PSize;
    hdcSrc: HDC;
    pptSrc: PPoint;
    crKey: TColorRef;
    pblend: PBlendFunction;
    dwFlags: DWORD;
    prcDirty: PRect;
  end;
  PUpdateLayeredWindowInfo = ^TUpdateLayeredWindowInfo;

function UpdateLayeredWindowIndirect(Handle: THandle; Info: PUpdateLayeredWindowInfo): Boolean; stdcall;
  external user32;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
var
  ExStyle: DWORD;
  WorkRect: TRect;
begin
  WorkRect := Screen.MonitorFromWindow(Handle).WorkareaRect;
  SetBounds(WorkRect.Left, WorkRect.Top, WorkRect.Width, WorkRect.Height);

  ExStyle := GetWindowLong(Handle, GWL_EXSTYLE);
  if (ExStyle and WS_EX_LAYERED) = 0 then
    SetWindowLong(Handle, GWL_EXSTYLE, ExStyle or WS_EX_LAYERED);

  EllipsePos := D2D1PointF(100, 100);
  XDelta := 5;
  YDelta := 5;

  D2DFactory := Vcl.Direct2D.D2DFactory;
  WICFactory := CreateComObject(CLSID_WICImagingFactory) as IWICImagingFactory;

  Blend.BlendOp := AC_SRC_OVER;
  Blend.BlendFlags := 0;
  Blend.SourceConstantAlpha := 255;
  Blend.AlphaFormat := AC_SRC_ALPHA;

  SourcePosition := Point(0, 0);
  WindowPosition := Point(0, 0);
  Size.cx := Width;
  Size.cy := Height;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_SPACE then
    Timer1.Enabled := not Timer1.Enabled;
end;

procedure TMainForm.CreateDeviceResources;
var
  PF: TD2D1PixelFormat;
  RTP: TD2D1RenderTargetProperties;
begin
  if DeviceResourcesValid then
    Exit;

  WICFactory.CreateBitmap(Width, Height, @GUID_WICPixelFormat32bppPBGRA, WICBitmapCacheOnLoad,
    WICBitmap);

  PF.format := DXGI_FORMAT_B8G8R8A8_UNORM;
  PF.alphaMode := D2D1_ALPHA_MODE_PREMULTIPLIED;

  RTP := D2D1RenderTargetProperties(D2D1_RENDER_TARGET_TYPE_DEFAULT, PF, 0, 0,
    D2D1_RENDER_TARGET_USAGE_GDI_COMPATIBLE);

  D2DFactory.CreateWicBitmapRenderTarget(WICBitmap, RTP, RenderTarget);
  InteropRenderTarget := RenderTarget as ID2D1GdiInteropRenderTarget;

  DeviceResourcesValid := True;
end;

procedure TMainForm.InvalidateDeviceResources;
begin
  DeviceResourcesValid := False;
end;

procedure TMainForm.UpdateWindow(SourceDC: HDC);
var
  Info: TUpdateLayeredWindowInfo;
begin
  ZeroMemory(@Info, SizeOf(Info));
  Info.cbSize := SizeOf(TUpdateLayeredWindowInfo);
  Info.pptSrc := @SourcePosition;
  Info.pptDst := @WindowPosition;
  Info.psize  := @Size;
  Info.pblend := @Blend;
  Info.dwFlags := ULW_ALPHA;
  Info.hdcSrc := SourceDC;

  if not UpdateLayeredWindowIndirect(Handle, @Info) then
    RaiseLastOSError();
end;

procedure TMainForm.PaintToRenderTarget(const RT: ID2D1RenderTarget);
var
  CanvasSize: TD2D1SizeF;
  EllipseBrush: ID2D1SolidColorBrush;
  YOne: Single;
  DC: HDC;
begin
  CreateDeviceResources;

  RT.BeginDraw;
  try
    RT.GetSize(CanvasSize);
    YOne := EllipsePos.y / CanvasSize.height;

    RT.CreateSolidColorBrush(D2D1ColorF(clBlack, 1 - YOne), nil, EllipseBrush);

    RT.Clear(D2D1ColorF(clBlack, 0));
    RT.FillEllipse(D2D1Ellipse(EllipsePos, 100, 100), EllipseBrush);

    InteropRenderTarget.GetDC(D2D1_DC_INITIALIZE_MODE_COPY, DC);
    UpdateWindow(DC);
    InteropRenderTarget.ReleaseDC(TRect.Empty);
  finally
    RT.EndDraw;
  end;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);

  procedure Move(var Dimension: Single; var Delta: Integer; Border: Single);
  begin
    if (Delta > 0) and ((Dimension + 100 + Delta) > Border) then
    begin
      Delta := -Delta;
      Dimension := Border - 100;
    end
    else if (Delta < 0) and ((Dimension - 100 + Delta) < 0)  then
    begin
      Delta := -Delta;
      Dimension := 100;
    end
    else
      Dimension := Dimension + Delta;
  end;

var
  CanvasSize: TD2D1SizeF;
begin
  if not DeviceResourcesValid then
    CreateDeviceResources;

  RenderTarget.GetSize(CanvasSize);

  Move(EllipsePos.x, XDelta, CanvasSize.width);
  Move(EllipsePos.y, YDelta, CanvasSize.height);

  PaintToRenderTarget(RenderTarget);
end;

procedure TMainForm.WMSize(var Message: TWMSize);
begin
  InvalidateDeviceResources;

  inherited;
end;

end.
