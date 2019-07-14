unit FMX.Material.Paper;

interface

uses
  FMX.Controls, FMX.Objects, FMX.Layouts, FMX.Material.ZIndex, FMX.Effects, System.Classes, FMX.Material.Theme;

type
  TMaterialPaper = class(TControl, IMaterialThemed)
  private
    FElevation: TElevation;
    procedure SetElevation(const Value: TElevation);
    procedure ProvideShadow(var Shadow: TShadowEffect);
  protected
    FShadowEffect: TShadowEffect;

    function GetElevationPx: Single;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ThemeUpdated(ATheme: TMaterialThemeType); virtual;
    destructor Destroy; override;
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
    Opacity = 0.5;
    SOFTNESS = 0.1;
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
  MaterialTheme.SetObserved([TMaterialThemeType.mtElevation], Self);
  Self.Elevation := 1;
end;

destructor TMaterialPaper.Destroy;
begin
  MaterialTheme.RemoveObserved(Self);
  inherited;
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
  Shadow.DIRECTION := TMaterialPaperShadowDefaults.DIRECTION;
  Shadow.SOFTNESS := TMaterialPaperShadowDefaults.SOFTNESS;
  Shadow.Opacity := TMaterialPaperShadowDefaults.Opacity;
  Shadow.ShadowColor := TAlphaColorRec.Black;
  Shadow.Parent := Self;
  Shadow.SetSubComponent(True);
  Shadow.Stored := False;
end;

procedure TMaterialPaper.SetElevation(const Value: TElevation);
begin
  Value.AdjustValue;

  FElevation := Value;
  FShadowEffect.Enabled := Value > 0;

  FShadowEffect.Distance := GetElevationPx / 1.3;
  FShadowEffect.Opacity := TMaterialPaperShadowDefaults.Opacity - (Elevation * 0.007);

  FShadowEffect.SOFTNESS := TMaterialPaperShadowDefaults.SOFTNESS + (Elevation * 0.0354);

end;

procedure TMaterialPaper.ThemeUpdated(ATheme: TMaterialThemeType);
begin

end;

end.
