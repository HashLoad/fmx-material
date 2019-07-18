program MaterialDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Src\Main.pas' {Form3},
  FMX.Material.Theme.Manager in '..\src\Theme\FMX.Material.Theme.Manager.pas',
  FMX.Material.Theme in '..\src\Theme\FMX.Material.Theme.pas',
  FMX.Material.AppBar in '..\src\Components\FMX.Material.AppBar.pas',
  FMX.Material.Avatar in '..\src\Components\FMX.Material.Avatar.pas',
  FMX.Material.Badge in '..\src\Components\FMX.Material.Badge.pas',
  FMX.Material.Card in '..\src\Components\FMX.Material.Card.pas',
  FMX.Material.Chip in '..\src\Components\FMX.Material.Chip.pas',
  FMX.Material.ChipList in '..\src\Components\FMX.Material.ChipList.pas',
  FMX.Material.ListView in '..\src\Components\FMX.Material.ListView.pas',
  FMX.Material.Paper in '..\src\Components\FMX.Material.Paper.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
