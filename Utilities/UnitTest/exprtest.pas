unit exprtest;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SearchExpr, StdCtrls;

type
  TForm1 = class(TForm, ISchExprValueManager)
    Edit1: TEdit;
    Button1: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    function CreateValue(var T : TSchExprToken) : TSchExprValue; virtual;
    function GetUnaryOpResult(var T : TSchExprToken; const X : TSchExprValue) : TSchExprValue; virtual;
    function GetBinaryOpResult(var T : TSchExprToken; const X, Y : TSchExprValue) : TSchExprValue; virtual;
    procedure DisposeValue(const AValue : TSchExprValue); virtual;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

function TForm1.CreateValue(var T : TSchExprToken) : TSchExprValue;
begin
  Result := NewStr(T.Text);
end;

function TForm1.GetUnaryOpResult(var T : TSchExprToken; const X : TSchExprValue) : TSchExprValue;
begin
  Result := NewStr('(Unary: ' + T.Text + ' ' + PString(X)^ + ' ['+IntToStr(T.Flags) + '])');
end;

function TForm1.GetBinaryOpResult(var T : TSchExprToken; const X, Y : TSchExprValue) : TSchExprValue;
var
  Data : String;
begin
  case T.Code of
    tcAnd : Data := '(Binary: ' + PString(X)^ + ' ' + T.Text + ' ' + PString(Y)^ + ' ['+IntToStr(T.Flags) + '])';
    tcOr : Data := '(Binary: ' + PString(X)^ + ' ' + T.Text + ' ' + PString(Y)^ + ' ['+IntToStr(T.Flags) + '])';
    tcNear : Data := '(Binary: ' + PString(X)^ + ' ' + T.Text + ' ' + PString(Y)^ + ' ['+IntToStr(T.Flags) + '])';
  end;
  Result := NewStr(Data);
end;

procedure TForm1.DisposeValue(const AValue : TSchExprValue);
begin
  if AValue <> Nil then
    DisposeStr(AValue);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  E : TSearchExpression;
  I : Integer;
begin
  E := TSearchExpression.Create;
  E.Expression := Edit1.Text;
  E.ValueManager := Self;
  E.Execute;

  Memo1.Lines.Clear;
  for I := 0 to High(E.Infix) do begin
    with E.Infix[I] do begin
      Memo1.Lines.Add(Format('%d: %s [%d]', [Ord(Kind), Text, Flags]));
    end;
  end;

  Memo2.Lines.Clear;
  for I := 0 to High(E.Postfix) do begin
    with E.Postfix[I] do begin
      Memo2.Lines.Add(Format('%d: %s [%d]', [Ord(Kind), Text, Flags]));
    end;
  end;

  Memo3.Lines.Clear;
  Memo3.WordWrap := True;
  Memo3.Lines.Add(PString(E.Result)^);

  E.DisposeResult;
  E.Free;
end;

end.
