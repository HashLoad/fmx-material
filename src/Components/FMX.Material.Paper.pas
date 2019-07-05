unit FMX.Material.Paper;

interface

uses
  FMX.Controls, FMX.Objects, FMX.Layouts, FMX.Material.ZIndex, FMX.Effects, System.Classes;

type
  TMaterialPaper = class(TControl)
  private
    FElevation: TElevation;
    procedure SetElevation(const Value: TElevation);
    procedure ProvideShadow(var Shadow: TShadowEffect);
    procedure ProvideShadowTop(var Shadow: TShadowEffect);
  protected
    FShadowEffect: TShadowEffect;
    FShadowEffectTop: TShadowEffect;

    function GetElevationPx: Single;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Elevation: TElevation read FElevation write SetElevation;

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
  end;

  TMaterialPaperShadowDefaults = class
    const
    OPACITY = 0.5;
    SOFTNESS = 0.6;
    DIRECTION = 90;
  end;

implementation

uses
  System.UITypes, {$IFDEF ANDROID} Androidapi.Helpers, {$ENDIF} FMX.Platform, System.Math;

{ TMaterialPaper }

constructor TMaterialPaper.Create(AOwner: TComponent);
begin
  inherited;
  ProvideShadow(FShadowEffect);
  ProvideShadowTop(FShadowEffectTop);

  Self.Elevation := 1;
end;

function TMaterialPaper.GetElevationPx: Single;
var
  LDpi: Float32;
begin
{$IFDEF ANDROID}
  LDpi := SharedActivityContext.getResources.getDisplayMetrics.densityDpi / 160;
{$ELSE}
  LDpi := 1;
{$ENDIF}
  Result := RoundTo(Elevation * LDpi, -2);
end;

procedure TMaterialPaper.ProvideShadow(var Shadow: TShadowEffect);
begin
  Shadow := TShadowEffect.Create(Self);
  Shadow.Direction := TMaterialPaperShadowDefaults.DIRECTION;
  Shadow.Softness := TMaterialPaperShadowDefaults.SOFTNESS;
  Shadow.Opacity := TMaterialPaperShadowDefaults.OPACITY;
  Shadow.ShadowColor := TAlphaColorRec.Black;
  Shadow.Parent := Self;
  Shadow.SetSubComponent(True);
  Shadow.Stored := False;
end;

procedure TMaterialPaper.ProvideShadowTop(var Shadow: TShadowEffect);
begin
  Shadow := TShadowEffect.Create(Self);
  Shadow.Direction := -TMaterialPaperShadowDefaults.DIRECTION;
  Shadow.Softness := TMaterialPaperShadowDefaults.SOFTNESS;
  Shadow.Opacity := TMaterialPaperShadowDefaults.OPACITY;
  Shadow.ShadowColor := TAlphaColorRec.Black;
  Shadow.Parent := FShadowEffect;
  Shadow.SetSubComponent(True);
  Shadow.Stored := False;
end;

procedure TMaterialPaper.SetElevation(const Value: TElevation);
begin
  FElevation := Value;
  FShadowEffect.Enabled := Value > 0;
  FShadowEffectTop.Enabled := Value > 0;

  FShadowEffect.Distance := GetElevationPx / 2;
  FShadowEffectTop.Distance := GetElevationPx / 5;
end;

end.
