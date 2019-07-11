unit FMX.Material.Badge;

interface

uses
  FMX.Material.Paper, FMX.Graphics, System.Classes, FMX.Controls;

type
  TBadgeVariant = (standard, dot);

  TMaterialBadge = class(TMaterialPaper)
  private
    FMax: Integer;
    FVariant: TBadgeVariant;
    FShowZero: Boolean;
    FInvisible: Boolean;
    FValue: Integer;
    procedure SetText(const Value: String);
    procedure SetMax(const Value: Integer);
    procedure SetShowZero(const Value: Boolean);
    procedure SetVariant(const Value: TBadgeVariant);
    procedure SetInvisible(const Value: Boolean);
    procedure SetValue(const Value: Integer);
  protected
    FFill: TBrush;
    FText: string;
    function GetFill: TBrush;
    procedure SetFill(const Value: TBrush);
    procedure Paint; override;
    procedure FillChanged(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property Elevation;
  published
    property Fill: TBrush read GetFill write SetFill;
    property Text: String read FText write SetText;
    property Max: Integer read FMax write SetMax default 99;
    property ShowZero: Boolean read FShowZero write SetShowZero;
    property Variant: TBadgeVariant read FVariant write SetVariant default TBadgeVariant.standard;
    property Invisible: Boolean read FInvisible write SetInvisible default True;
    property Value: Integer read FValue write SetValue default 0;
  end;

implementation

uses
  System.Types, System.UITypes;

{ TMaterialBadge }

constructor TMaterialBadge.Create(AOwner: TComponent);
begin
  inherited;
  Self.Height := 8;
  Self.Width := 8;
  Self.CanFocus := False;
  Self.TabStop := False;
  Self.Elevation := 0;

  FFill := TBrush.Create(TBrushKind.Solid, TAlphaColorRec.White);
  FFill.OnChanged := FillChanged;
  FFill.Color := TAlphaColorRec.Red;
end;

procedure TMaterialBadge.FillChanged(Sender: TObject);
begin
  Repaint;
end;

function TMaterialBadge.GetFill: TBrush;
begin
  Result := FFill;
end;

procedure TMaterialBadge.Paint;
var
  LRect: TRectF;
begin
  inherited;
  LRect := TRectF.Create(Self.Width - 5, -10, Self.Width + 10, 5);

  Canvas.BeginScene;
  try
    Canvas.FillEllipse(LRect, AbsoluteOpacity, FFill);
  finally
    Canvas.EndScene;
  end;
end;

procedure TMaterialBadge.SetFill(const Value: TBrush);
begin
  FFill.Assign(Value);
end;

procedure TMaterialBadge.SetInvisible(const Value: Boolean);
begin
  FInvisible := Value;
end;

procedure TMaterialBadge.SetMax(const Value: Integer);
begin
  FMax := Value;
end;

procedure TMaterialBadge.SetShowZero(const Value: Boolean);
begin
  FShowZero := Value;
end;

procedure TMaterialBadge.SetText(const Value: String);
begin

end;

procedure TMaterialBadge.SetValue(const Value: Integer);
begin
  FValue := Value;
end;

procedure TMaterialBadge.SetVariant(const Value: TBadgeVariant);
begin
  FVariant := Value;
end;

end.
