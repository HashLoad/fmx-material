unit FMX.Material.Theme.Manager;

interface

uses
  System.Generics.Collections;

type
  TMaterialThemeType = (mtElevation);
  TMaterialThemeTypes = set of TMaterialThemeType;

  IMaterialThemed = interface
    procedure ThemeUpdated(ATheme: TMaterialThemeType);
  end;

  TMaterialThemeManager = class
  private
    FContext: TDictionary<IMaterialThemed, TMaterialThemeTypes>;
  public
    procedure SetObserved(ATriggers: TMaterialThemeTypes; AObject: IMaterialThemed);
    procedure RemoveObserved(AObject: IMaterialThemed);
    procedure DoNotify(AType: TMaterialThemeType);
    constructor Create;
    destructor Destroy; override;

  end;

implementation

{ TMaterialThemeManager }

constructor TMaterialThemeManager.Create;
begin
  FContext := TDictionary<IMaterialThemed, TMaterialThemeTypes>.Create;
end;

destructor TMaterialThemeManager.Destroy;
begin
  FContext.DisposeOf;
  inherited;
end;

procedure TMaterialThemeManager.DoNotify(AType: TMaterialThemeType);
var
  LListener: TPair<IMaterialThemed, TMaterialThemeTypes>;
begin
  for LListener in FContext do
  begin
    if AType in LListener.Value then
      LListener.Key.ThemeUpdated(AType);
  end;
end;

procedure TMaterialThemeManager.RemoveObserved(AObject: IMaterialThemed);
begin
  FContext.Remove(AObject);
end;

procedure TMaterialThemeManager.SetObserved(ATriggers: TMaterialThemeTypes; AObject: IMaterialThemed);
begin
  FContext.Add(AObject, ATriggers);
end;

end.
