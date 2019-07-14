unit FMX.Material.ChipList;

interface

uses
  FMX.Material.Card, System.Classes, FMX.Edit;

type
  TMaterialChipList = class(TCustomEdit)


  public
    constructor Create(AOwner: TComponent); override;
  end;



implementation

{ TMaterialChipList }

constructor TMaterialChipList.Create(AOwner: TComponent);
begin
  inherited;
//  Elevation := 0;
end;

end.
