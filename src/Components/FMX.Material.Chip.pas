unit FMX.Material.Chip;

interface

uses
  FMX.Material.Paper, FMX.Graphics, System.Classes, System.UITypes, System.SysUtils, FMX.Objects;

const
  DEFAULT_OUTLINE_SIZE = 2;
  TEXT_MARGING_HEIGHT = 10;
  TEXT_MARGING_WIDTH = 20;
  DEFAULT_PATH_DELETE = 'M12 2C6.47 2 2 6.47 2 12s4.47 10 10 10 10-4.47 10-10S17.53 2 12 2zm5 ' +
    '13.59L15.59 17 12 13.41 8.41 17 7 15.59 10.59 12 7 8.41 8.41 7 12 10.59 15.59 7 17 8.41 13.41 12 17 15.59z';

type
  TMaterialChip = class;

  TMaterialChipVariant = (vDefault, vOutlined);
  TMaterialChipDeleteType = (dtNone, dtDefault, dtCustom);
  TDeleteChip = procedure(Sender: TMaterialChip) of object;

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
    FDeletePath: TPath;
    FDeleteIcon: TMaterialChipDeleteType;
    FOnDelete: TDeleteChip;
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
    function GetDeletePath: TPathData;
    function PathIsStored: Boolean;
    procedure SetDeletePath(const Value: TPathData);

    procedure InternalDeleteClick(ASender: TObject);

  protected
    procedure Resize; override;
    procedure HitTestChanged; override;
    property CanFocus default False;
    procedure Paint; override;
    procedure DrawDeleteIcon;
    procedure FillChanged(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

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
    property OnDelete: TDeleteChip read FOnDelete write FOnDelete;
    property DeletePath: TPathData read GetDeletePath write SetDeletePath stored PathIsStored;

    property HitTest default True;
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
  System.Types, FMX.Types, FMX.Controls, FMX.Ani;

{ TMaterialChip }

procedure TMaterialChip.Assign(Source: TPersistent);
var
  LSource: TMaterialChip;
begin
  LSource := Source as TMaterialChip;

  Self.Elevation := LSource.Elevation;
  Self.Margins := LSource.Margins;
  Self.Fill := LSource.Fill;
  Self.Font := LSource.Font;
  Self.FontColor := LSource.FontColor;
  Self.Variant := LSource.Variant;
  Self.Text := LSource.Text;
  Self.DeleteIcon := LSource.DeleteIcon;
  Self.MinHeight := LSource.MinHeight;
  Self.MinWidth := LSource.MinWidth;
  Self.DeletePath := LSource.DeletePath;
  Self.OutlinedSize := LSource.OutlinedSize;
  Self.OnDelete := LSource.OnDelete;
end;

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

  FDeletePath := TPath.Create(nil);

  FDeletePath.SetSubComponent(True);
  FDeletePath.Stored := False;

  FDeletePath.Data.Data := DEFAULT_PATH_DELETE;
  FDeletePath.Align := TAlignLayout.Right;
  FDeletePath.Margins.Top := 4;
  FDeletePath.Margins.Bottom := 4;
  FDeletePath.Margins.Right := 5;
  FDeletePath.OnClick := InternalDeleteClick;

  FFill := TBrush.Create(TBrushKind.Solid, $FFE0E0E0);
  FFill.OnChanged := FillChanged;
  FFont := TFont.Create;
  FFont.Size := 12;
  FFontColor := TAlphaColorRec.Black;
end;

destructor TMaterialChip.Destroy;
begin
  FFill.DisposeOf;
  FFont.DisposeOf;
  FDeletePath.DisposeOf;
  inherited;
end;

procedure TMaterialChip.DrawDeleteIcon;
begin
  case DeleteIcon of
    dtNone: FDeletePath.Parent := nil;
    dtDefault,
    dtCustom:
      begin
        case FVariant of
          vDefault:
          begin
            FDeletePath.Fill.Color := TAlphaColorRec.White;
            FDeletePath.Opacity := 0.6;
          end;
          vOutlined:
          begin
            FDeletePath.Fill.Color := FFill.Color;
            FDeletePath.Opacity := 1;
          end;
        end;

        FDeletePath.Stroke.Kind := TBrushKind.None;
        FDeletePath.Height := Height;
        FDeletePath.Width := FDeletePath.Height;
        FDeletePath.Parent := Self;
      end;
  end;
end;

procedure TMaterialChip.FillChanged(Sender: TObject);
begin
  Repaint;
end;

function TMaterialChip.GetDeletePath: TPathData;
begin
  Result := FDeletePath.Data;
end;

function TMaterialChip.GetFill: TBrush;
begin
  Result := FFill;
end;

procedure TMaterialChip.HitTestChanged;
begin
  inherited;
  FDeletePath.HitTest := HitTest;
end;

procedure TMaterialChip.InternalDeleteClick(ASender: TObject);
begin
  if Assigned(FOnDelete) then
    FOnDelete(Self);

  Self.Parent := nil;
  Self.DisposeOf;
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

  if DeleteIcon <> dtNone then
    //To square button marge
    LRect.Right := LRect.Right - Height;

  DrawDeleteIcon;

  Canvas.FillText(LRect, Text, False, AbsoluteOpacity, [], TTextAlign.Center);

  Canvas.EndScene;
  inherited;
end;

function TMaterialChip.PathIsStored: Boolean;
begin
  Result := FDeletePath.Data.Data <> DEFAULT_PATH_DELETE;
end;

procedure TMaterialChip.RecalculeSize;
var
  LHeight: Single;
  LWidth: Single;
  LOldTextWidth: Single;
begin
  if FInRecalcSize or not Assigned(Canvas) then
    Exit;

  Canvas.Font.Assign(FFont);

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
  RecalculeSize;
  Repaint;
end;

procedure TMaterialChip.SetDeletePath(const Value: TPathData);
begin
  FDeletePath.Data.Assign(Value);
  Repaint;
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
