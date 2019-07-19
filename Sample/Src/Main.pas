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
    Edit1: TEdit;
    MaterialAvatar1: TMaterialAvatar;
    procedure Edit1Change(Sender: TObject);
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

procedure TForm3.Edit1Change(Sender: TObject);
begin
 MaterialAvatar1.Text := Edit1.Text;
end;

end.
