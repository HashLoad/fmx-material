unit FMX.Material.Card;

interface

uses
  FMX.Material.Paper, FMX.Graphics, System.Classes, FMX.Types;

const
  CORNERS_ALL = [TCorner.TopLeft, TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight];

type

  TMaterialCard = class(TMaterialPaper)
  private
    FFill: TBrush;
    FCorners: TCorners;
    function GetFill: TBrush;
    procedure SetFill(const Value: TBrush);
    procedure SetCorners(const Value: TCorners);
  protected
    property HitTest default True;
    property CanFocus default False;
    procedure Paint; override;
    procedure FillChanged(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Fill: TBrush read GetFill write SetFill;
    property Corners: TCorners read FCorners write SetCorners default CORNERS_ALL;
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
  System.UITypes, System.Types, FMX.Controls;

{ TMaterialCard }

constructor TMaterialCard.Create(AOwner: TComponent);
begin
  inherited;
  Self.Height := 250;
  Self.Width := 200;
  Self.CanFocus := False;
  Self.TabStop := False;

  FCorners := [TCorner.TopLeft, TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight];
  FFill := TBrush.Create(TBrushKind.Solid, TAlphaColorRec.White);
  FFill.OnChanged := FillChanged;
end;

destructor TMaterialCard.Destroy;
begin
  FFill.DisposeOf;
  inherited;
end;

procedure TMaterialCard.FillChanged(Sender: TObject);
begin
  Repaint;
end;

function TMaterialCard.GetFill: TBrush;
begin
  Result := FFill;
end;

procedure TMaterialCard.Paint;
begin
  Canvas.BeginScene;
  Canvas.FillRect(TRectF.Create(0, 0, Self.Width, Self.Height), 5, 5, FCorners, AbsoluteOpacity, FFill,
    TCornerType.Round);
  Canvas.EndScene;
  inherited;
end;

procedure TMaterialCard.SetCorners(const Value: TCorners);
begin
  FCorners := Value;
  Repaint;
end;

procedure TMaterialCard.SetFill(const Value: TBrush);
begin
  FFill.Assign(Value);
end;

end.
