program exprtst;

uses
  Forms,
  exprtest in 'exprtest.pas' {Form1},
  SearchExpr in '..\..\Application\SearchExpr.pas',
  IslUtils in '..\..\Application\IslUtils.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
