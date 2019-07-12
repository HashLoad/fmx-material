unit FMX.Material.Chip;

interface

uses
  FMX.Material.Paper, FMX.Graphics, System.Classes, System.UITypes, System.SysUtils;

const
  DEFAULT_OUTLINE_SIZE = 2;
  TEXT_MARGING_HEIGHT = 10;
  TEXT_MARGING_WIDTH = 20;

type
  TMaterialChipVariant = (vDefault, vOutlined);
  TMaterialChipDeleteType = (dtNone, dtDefault, dtCustom);

  TMaterialChip = class(TMaterialPaper)
  private
    FFill: TBrush;
    FVariant: TMaterialChipVariant;
    FOutlinedSize: Integer;
    FText: string;
    FOldText: string;
    FFont: TFont;
    FFontColor: TAlphaColor;
    FMinHeight: Single;
    FMinWidth: Single;
    FInRecalcSize: Boolean;
    FDeleteIcon: TMaterialChipDeleteType;
    FOnDelete: TProc<TMaterialChip>;
    function GetFill: TBrush;
    procedure SetFill(const Value: TBrush);
    procedure SetVariant(const Value: TMaterialChipVariant);
    procedure SetText(const Value: string);
    procedure SetFont(const Value: TFont);
    procedure SetFontColor(const Value: TAlphaColor);
    procedure SetMinHeight(const Value: Single);
    procedure SetMinWidth(const Value: Single);
    procedure RecalculeSize;
    procedure SetOutlinedSize(const Value: Integer);
    procedure SetDeleteIcon(const Value: TMaterialChipDeleteType);
    procedure DoDrawDeleteButton;
  protected
    procedure Resize; override;
    property HitTest default True;
    property CanFocus default False;
    procedure Paint; override;
    procedure FillChanged(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Fill: TBrush read GetFill write SetFill;
    property Font: TFont read FFont write SetFont;
    property FontColor: TAlphaColor read FFontColor write SetFontColor default TAlphaColorRec.Black;
    property Variant: TMaterialChipVariant read FVariant write SetVariant;
    property Text: string read FText write SetText;
    property OutlinedSize: Integer read FOutlinedSize write SetOutlinedSize default DEFAULT_OUTLINE_SIZE;
    property DeleteIcon: TMaterialChipDeleteType read FDeleteIcon write SetDeleteIcon
      default TMaterialChipDeleteType.dtNone;

    property MinHeight: Single read FMinHeight write SetMinHeight;
    property MinWidth: Single read FMinWidth write SetMinWidth;
    property OnDelete: TProc<TMaterialChip> read FOnDelete write FOnDelete;

    property Align;
    property Anchors;
    property ClipParent;
    property Cursor;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Locked;
    property Height;
    property Padding;
    property Opacity;
    property Margins;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    { Events }
    property OnPainting;
    property OnPaint;
    property OnResize;
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

    { Material }
    property Elevation;
  end;

implementation

uses
  System.Types, FMX.Types, FMX.Controls;

{ TMaterialChip }

constructor TMaterialChip.Create(AOwner: TComponent);
begin
  inherited;
  Height := 28;
  Width := 100;
  MinHeight := Height;
  MinWidth := Width;
  CanFocus := False;
  TabStop := False;
  Elevation := 0;
  FOutlinedSize := DEFAULT_OUTLINE_SIZE;
  FDeleteIcon := TMaterialChipDeleteType.dtNone;
  FInRecalcSize := False;

  FFill := TBrush.Create(TBrushKind.Solid, $FFE0E0E0);
  FFill.OnChanged := FillChanged;
  FFont := TFont.Create;
  FFont.Size := 12;
  FFontColor := TAlphaColorRec.Black;
end;

destructor TMaterialChip.Destroy;
begin
  FFill.DisposeOf;
  inherited;
end;

procedure TMaterialChip.DoDrawDeleteButton;
var
  LRect: TRectF;
begin
  inherited;
  LRect := TRectF.Create(0, Self.Width - Self.Height, Self.Width - 3, Self.Height - 3);
  LRect := TRectF.Create(0, 0, 1, 1).FitInto(LRect);

  Canvas.BeginScene;
  try
    Canvas.FillEllipse(LRect, AbsoluteOpacity, FFill);
  finally
    Canvas.EndScene;
  end;
end;

procedure TMaterialChip.FillChanged(Sender: TObject);
begin
  Repaint;
end;

function TMaterialChip.GetFill: TBrush;
begin
  Result := FFill;
end;

procedure TMaterialChip.Paint;
var
  LRect: TRectF;
  LCorners: TCorners;
  LSides: TSides;
  LOutlinedBrush: TStrokeBrush;
  LBorderRadious: Single;
begin
  LRect := TRectF.Create(0, 0, Self.Width, Self.Height);
  LCorners := [TCorner.TopLeft, TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight];
  LSides := [TSide.Top, TSide.Left, TSide.Bottom, TSide.Right];
  LBorderRadious := LRect.Height / 2;

  Canvas.BeginScene;

  Canvas.Font.Assign(FFont);

  case FVariant of
    vDefault:
      begin
        Canvas.FillRect(LRect, LBorderRadious, LBorderRadious, LCorners, AbsoluteOpacity, FFill, TCornerType.Round);
        Canvas.Fill.Color := FontColor;
      end;
    vOutlined:
      begin
        LOutlinedBrush := TStrokeBrush.Create(FFill.Kind, FFill.Color);
        LOutlinedBrush.Kind := TBrushKind.Solid;
        LOutlinedBrush.Thickness := FOutlinedSize;
        Canvas.DrawRectSides(LRect, LBorderRadious, LBorderRadious, LCorners, AbsoluteOpacity, LSides, LOutlinedBrush);
        Canvas.Fill.Color := FFill.Color;
      end;
  end;
  DoDrawDeleteButton;

  Canvas.FillText(LRect, Text, False, AbsoluteOpacity, [], TTextAlign.Center);

  Canvas.EndScene;
  inherited;
end;

procedure TMaterialChip.RecalculeSize;
var
  LHeight: Single;
  LWidth: Single;
  LOldTextWidth: Single;
begin
  if FInRecalcSize then
    Exit;

  LHeight := 0;
  LWidth := 0;

  FInRecalcSize := True;
  try
    if not FOldText.IsEmpty then
    begin
      LOldTextWidth := Canvas.TextWidth(FOldText) + TEXT_MARGING_WIDTH;
      if LOldTextWidth = Width then
      begin
        Width := 0;
        Height := 0;
      end;
    end;

    if DeleteIcon <> TMaterialChipDeleteType.dtNone then
    begin
      LWidth := Canvas.TextHeight(FText) + TEXT_MARGING_HEIGHT;
    end;

    if not FText.IsEmpty then
    begin
      LHeight := Canvas.TextHeight(FText) + TEXT_MARGING_HEIGHT;
      LWidth := LWidth + Canvas.TextWidth(FText) + TEXT_MARGING_WIDTH;
    end;

    if LHeight < FMinHeight then
      LHeight := FMinHeight;

    if LWidth < FMinWidth then
      LWidth := FMinWidth;

    if Width < LWidth then
      Width := LWidth;

    if Height < LHeight then
      Height := LHeight;

  finally
    FInRecalcSize := False;
  end;
end;

procedure TMaterialChip.Resize;
begin
  inherited;
  RecalculeSize;
end;

procedure TMaterialChip.SetDeleteIcon(const Value: TMaterialChipDeleteType);
begin
  FDeleteIcon := Value;
  if FDeleteIcon <> Value then
  begin

    if FDeleteIcon <> TMaterialChipDeleteType.dtNone then
    begin
      Width := Width - Height;
    end;

    RecalculeSize;
    Repaint;
  end;
end;

procedure TMaterialChip.SetFill(const Value: TBrush);
begin
  FFill.Assign(Value);
end;

procedure TMaterialChip.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Repaint;
end;

procedure TMaterialChip.SetFontColor(const Value: TAlphaColor);
begin
  FFontColor := Value;
  Repaint;
end;

procedure TMaterialChip.SetMinHeight(const Value: Single);
begin
  if FMinHeight <> Value then
  begin
    FMinHeight := Value;
    RecalculeSize;
    Repaint;
  end;
end;

procedure TMaterialChip.SetMinWidth(const Value: Single);
begin
  if FMinWidth <> Value then
  begin
    FMinWidth := Value;
    RecalculeSize;
    Repaint;
  end;
end;

procedure TMaterialChip.SetOutlinedSize(const Value: Integer);
begin
  FOutlinedSize := Value;
  Repaint;
end;

procedure TMaterialChip.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FOldText := FText;
    FText := Value;
    RecalculeSize;
    Repaint;
  end;
end;

procedure TMaterialChip.SetVariant(const Value: TMaterialChipVariant);
begin
  FVariant := Value;
  Repaint;
end;

end.
