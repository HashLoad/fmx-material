unit FMX.Material.Theme;

interface

uses
  System.Classes, FMX.Material.ZIndex, System.Generics.Collections, FMX.Material.Theme.Manager;

type
  IMaterialThemed = FMX.Material.Theme.Manager.IMaterialThemed;
  TMaterialThemeType = FMX.Material.Theme.Manager.TMaterialThemeType;
  TMaterialThemeTypes = FMX.Material.Theme.Manager.TMaterialThemeTypes;

  TMaterialZIndex = class(TPersistent)
  private
    FMobileStepper: TElevation;
    FTooltip: TElevation;
    FAppBar: TElevation;
    FModal: TElevation;
    FDrawer: TElevation;
    FSnackbar: TElevation;
    FManager: TMaterialThemeManager;
    procedure SetAppBar(const Value: TElevation);
    procedure SetDrawer(const Value: TElevation);
    procedure SetMobileStepper(const Value: TElevation);
    procedure SetModal(const Value: TElevation);
    procedure SetSnackbar(const Value: TElevation);
    procedure SetTooltip(const Value: TElevation);
  published
    property MobileStepper: TElevation read FMobileStepper write SetMobileStepper default 0;
    property AppBar: TElevation read FAppBar write SetAppBar default 4;
    property Drawer: TElevation read FDrawer write SetDrawer default 16;
    property Modal: TElevation read FModal write SetModal default 24;
    property Snackbar: TElevation read FSnackbar write SetSnackbar default 1;
    property Tooltip: TElevation read FTooltip write SetTooltip default 1;
    constructor Create;
  end;

  TMaterialThemeCore = class(TComponent)
  private
    FZIndex: TMaterialZIndex;

    FManager: TMaterialThemeManager;
    procedure SetZIndex(const Value: TMaterialZIndex);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetObserved(ATriggers: TMaterialThemeTypes; AObject: IMaterialThemed);
    procedure RemoveObserved(AObject: IMaterialThemed);
  published
    property ZIndex: TMaterialZIndex read FZIndex write SetZIndex;
  end;

  TMaterialTheme = class(TComponent)
  private
    function GetZIndex: TMaterialZIndex;
    procedure SetZIndex(const Value: TMaterialZIndex);
  published
    property ZIndex: TMaterialZIndex read GetZIndex write SetZIndex;
  end;

var
  MaterialTheme: TMaterialThemeCore;

implementation

{ TMaterialThemeCore }

constructor TMaterialThemeCore.Create(AOwner: TComponent);
begin
  inherited;
  FManager := TMaterialThemeManager.Create;
  FZIndex := TMaterialZIndex.Create;

  FZIndex.FManager := FManager;
end;

destructor TMaterialThemeCore.Destroy;
begin
  FManager.DisposeOf;
  FZIndex.DisposeOf;
  inherited;
end;

procedure TMaterialThemeCore.RemoveObserved(AObject: IMaterialThemed);
begin
  FManager.RemoveObserved(AObject);
end;

procedure TMaterialThemeCore.SetObserved(ATriggers: TMaterialThemeTypes; AObject: IMaterialThemed);
begin
  FManager.SetObserved(ATriggers, AObject);
end;

procedure TMaterialThemeCore.SetZIndex(const Value: TMaterialZIndex);
begin
  FZIndex := Value;
end;

{ TMaterialZIndex }

constructor TMaterialZIndex.Create;
begin
  FMobileStepper := 0;
  FAppBar := 4;
  FDrawer := 16;
  FModal := 24;
  FSnackbar := 1;
  FTooltip := 1;
end;

procedure TMaterialZIndex.SetAppBar(const Value: TElevation);
begin
  FAppBar := Value;
  FManager.DoNotify(mtElevation);
end;

procedure TMaterialZIndex.SetDrawer(const Value: TElevation);
begin
  FDrawer := Value;
  FManager.DoNotify(mtElevation);
end;

procedure TMaterialZIndex.SetMobileStepper(const Value: TElevation);
begin
  FMobileStepper := Value;
  FManager.DoNotify(mtElevation);
end;

procedure TMaterialZIndex.SetModal(const Value: TElevation);
begin
  FModal := Value;
  FManager.DoNotify(mtElevation);
end;

procedure TMaterialZIndex.SetSnackbar(const Value: TElevation);
begin
  FSnackbar := Value;
  FManager.DoNotify(mtElevation);
end;

procedure TMaterialZIndex.SetTooltip(const Value: TElevation);
begin
  FTooltip := Value;
  FManager.DoNotify(mtElevation);
end;

{ TMaterialTheme }

function TMaterialTheme.GetZIndex: TMaterialZIndex;
begin
  Result := MaterialTheme.ZIndex;
end;

procedure TMaterialTheme.SetZIndex(const Value: TMaterialZIndex);
begin
  MaterialTheme.ZIndex.Assign(Value);
end;

initialization

MaterialTheme := TMaterialThemeCore.Create(nil);

finalization

MaterialTheme.DisposeOf;
MaterialTheme.Loaded

end.
