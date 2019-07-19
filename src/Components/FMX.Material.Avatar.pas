unit FMX.Material.Avatar;

interface

uses
  FMX.Material.Paper, System.UITypes, FMX.Graphics, FMX.ImgList, System.Classes;

type
  TMaterialAvatar = class(TMaterialPaper)
  private
    procedure SetText(const Value: String);
  protected
    FFill: TBrush;
    FText: string;
    function GetFill: TBrush;
    procedure SetFill(const Value: TBrush);
    procedure Paint; override;
    procedure FillChanged(Sender: TObject); virtual;
    procedure SetWidth(const Value: Single); override;
    procedure SetHeight(const Value: Single); override;
    function GetInitials: string;
    procedure GenerateNameColor;
  public
    constructor Create(AOwner: TComponent); override;
    property Elevation;
  published
    property Fill: TBrush read GetFill write SetFill;
    property Align;
    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
    property HitTest default True;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property Visible default True;
    property Width;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
    property OnResized;
    property Text: String read FText write SetText;
  end;

implementation

uses
  FMX.Objects, System.Types, FMX.Types, System.Sysutils, FMX.Controls;

{ TMaterialAvatar }

function GenerateRandomColor(const AMix: TAlphaColor = TAlphaColorRec.Azure): TAlphaColor;
var
  RGB, Red, Green, Blue: Integer;
begin
  Red := Random(256);
  Green := Random(256);
  Blue := Random(256);
  RGB := TAlphaColorRec.ColorToRGB(AMix);

  Red := (Red + RGB) div 2;
  Green := (Green + RGB) div 2;
  Blue := (Blue + RGB) div 2;
  Result := TAlphaColorF.Create(Red, Green, Blue).ToAlphaColor;
end;

constructor TMaterialAvatar.Create(AOwner: TComponent);
begin
  inherited;
  Self.Height := 72;
  Self.Width := 72;
  Self.CanFocus := False;
  Self.TabStop := False;
  Self.Elevation := 0;

  FFill := TBrush.Create(TBrushKind.Solid, TAlphaColorRec.White);
  FFill.OnChanged := FillChanged;
  FFill.Color := GenerateRandomColor;
end;

procedure TMaterialAvatar.FillChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TMaterialAvatar.GenerateNameColor;
const
  COLORS: array[0..9] of integer  = ($FF4A148C, $FFF44336, $FF303F9F, $FF1565C0, $FF388E3C, $FF827717,
    $FFFFA000, $FF4E342E, $FF616161, $FF37474F);
var
 LSum: Integer;
 LInitials: string;
 LIndex: Integer;
begin
  LInitials := GetInitials;

  if LInitials.IsEmpty then
  begin
    FFill.Color := COLORS[0];
    Exit;
  end;

  LSum := 0;
  {$ZEROBASEDSTRINGS OFF}
  for LIndex := 1 to Length(LInitials) do
  begin
    LSum := LSum + Ord(LInitials[LIndex]);
  end;
  {$ZEROBASEDSTRINGS ON}

  FFill.Color := COLORS[LSum mod 9];
end;

function TMaterialAvatar.GetFill: TBrush;
begin
  Result := FFill;
end;

function TMaterialAvatar.GetInitials: string;
var
  LStrings: TArray<string>;
begin
  if FText.IsEmpty then
    Exit('');

  LStrings := Text.Trim.Split([' ']);
  Result := LStrings[0].Chars[0];

  if Length(LStrings) > 1 then
    Result := Result + LStrings[1].Chars[0];

end;

procedure TMaterialAvatar.Paint;
var
  LRect: TRectF;
begin
  inherited;
  LRect := TRectF.Create(0, 0, Self.Width, Self.Height);
  LRect := TRectF.Create(0, 0, 1, 1).FitInto(LRect);

  Canvas.BeginScene;
  try
    Canvas.FillEllipse(LRect, AbsoluteOpacity, FFill);
    if Self.Width > Self.Height then
      Canvas.Font.Size := Self.Height / 2
    else
      Canvas.Font.Size := Self.Width / 2;
    Canvas.Font.Style := [TFontStyle.fsBold];
    Canvas.Fill.Color := TAlphaColorRec.White;

    Canvas.FillText(LRect, GetInitials.ToUpper, False, AbsoluteOpacity, [], TTextAlign.Center);
  finally
    Canvas.EndScene;
  end;
end;

procedure TMaterialAvatar.SetFill(const Value: TBrush);
begin
  FFill.Assign(Value);
end;

procedure TMaterialAvatar.SetHeight(const Value: Single);
begin
  inherited;
  if Width <> Value then
    Width := Value;
end;

procedure TMaterialAvatar.SetText(const Value: String);
begin
  FText := Value;
  GenerateNameColor;
  Repaint;
end;

procedure TMaterialAvatar.SetWidth(const Value: Single);
begin
  inherited;
  if Height <> Value then
    Height := Value;
end;

end.
