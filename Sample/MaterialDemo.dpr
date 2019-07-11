program MaterialDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Src\Main.pas' {Form3},
  FMX.Material.Avatar in '..\src\Components\FMX.Material.Avatar.pas',
  FMX.Material.Badge in '..\src\Components\FMX.Material.Badge.pas',
  FMX.Material.Card in '..\src\Components\FMX.Material.Card.pas',
  FMX.Material.Chip in '..\src\Components\FMX.Material.Chip.pas',
  FMX.Material.ListView in '..\src\Components\FMX.Material.ListView.pas',
  FMX.Material.Paper in '..\src\Components\FMX.Material.Paper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
