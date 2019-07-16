unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Material.Paper, FMX.Objects, FMX.Effects,
  FMX.Controls.Presentation, FMX.Edit, FMX.EditBox, FMX.NumberBox, FMX.Material.Card, Data.Bind.EngExt,
  FMX.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs, FMX.Bind.Editors, Data.Bind.Components, FMX.Ani,
  FMX.StdCtrls,
  FMX.Material.Avatar, FMX.Material.Badge, FMX.Material.Chip, FMX.Layouts, FMX.ScrollBox, FMX.Material.ChipList,
  System.Generics.Collections;

type
  TForm3 = class(TForm)
    MaterialChipList1: TMaterialChipList;
    Button1: TButton;
    Edit1: TEdit;
    procedure MaterialChipList1TMaterialChipListBaseDelete(Sender: TMaterialChip);
    procedure Button1Click(Sender: TObject);
    function MaterialChipList1Validate(AValue: string): Boolean;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  FMX.Material.ZIndex;

{$R *.fmx}
{ TTestFlowLayout }

procedure TForm3.Button1Click(Sender: TObject);
var
  Lindex: Integer;
begin
  for Lindex := 0 to 1000 do
    MaterialChipList1.Add(Lindex.ToString);
end;

procedure TForm3.MaterialChipList1TMaterialChipListBaseDelete(Sender: TMaterialChip);
begin
  Sender.DisposeOf;
end;

function TForm3.MaterialChipList1Validate(AValue: string): Boolean;
begin
  Result := not AValue.IsEmpty;

end;

end.
