unit FMX.RegisterComponents;

interface

procedure Register;

implementation

uses
  System.Classes, FMX.Material.Paper, FMX.Material.ZIndex, FMX.Material.ZIndex.Editor, DesignIntf, FMX.Types,
  FMX.Material.Card, FMX.Material.Avatar, FMX.Material.Badge, FMX.Material.Chip, FMX.Material.ChipList, FMX.Material.Theme;

procedure Register;
begin
  RegisterComponents('HashLoad - Material', [
    TMaterialTheme,
    TMaterialPaper,
    TMaterialCard,
    TMaterialAvatar,
    TMaterialBadge,
    TMaterialChip,
    TMaterialChipList
  ]);

  RegisterPropertyEditor(TypeInfo(TElevation), System.Classes.TPersistent, '', TElevationProperty);
  RegisterPropertyEditor(TypeInfo(TElevation), FMX.Types.TFmxObject, '', TElevationProperty);
end;

end.
