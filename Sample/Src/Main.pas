unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Material.Paper, FMX.Objects, FMX.Effects,
  FMX.Controls.Presentation, FMX.Edit, FMX.EditBox, FMX.NumberBox, FMX.Material.Card, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.Components, FMX.Ani, FMX.StdCtrls,
  FMX.Material.Avatar, FMX.Material.Badge, FMX.Material.Chip;

type
  TForm3 = class(TForm)
    NumberBox1: TNumberBox;
    MaterialCard1: TMaterialCard;
    MaterialPaper1: TMaterialPaper;
    Circle1: TCircle;
    FloatAnimation1: TFloatAnimation;
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    MaterialAvatar1: TMaterialAvatar;
    Edit2: TEdit;
    MaterialChip1: TMaterialChip;
    Path1: TPath;
    procedure FloatAnimation1Process(Sender: TObject);
    procedure NumberBox1ChangeTracking(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure MaterialChip1Delete(Sender: TMaterialChip);
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

procedure TForm3.CheckBox1Change(Sender: TObject);
begin
FloatAnimation1.Enabled := CheckBox1.IsChecked;
end;

procedure TForm3.Edit2Change(Sender: TObject);
begin
  MaterialChip1.Text := Edit2.Text;
end;

procedure TForm3.FloatAnimation1Process(Sender: TObject);
begin
  Edit1.Text := ElevationToString(MaterialCard1.Elevation);
end;

procedure TForm3.MaterialChip1Delete(Sender: TMaterialChip);
begin
  ShowMessage(Sender.Text);
end;

procedure TForm3.NumberBox1ChangeTracking(Sender: TObject);
begin
  MaterialCard1.Elevation := TElevation(NumberBox1.Text.ToInteger);
  MaterialPaper1.Elevation := TElevation(NumberBox1.Text.ToInteger);
end;

end.
