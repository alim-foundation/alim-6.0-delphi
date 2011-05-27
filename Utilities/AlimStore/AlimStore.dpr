program AlimStore;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  QuranStruct in '..\..\Application\QuranStruct.pas',
  Builders in 'Builders.pas',
  IndexStruct in '..\..\Application\IndexStruct.pas',
  IslUtils in '..\..\Application\IslUtils.pas',
  SearchExpr in '..\..\Application\SearchExpr.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
