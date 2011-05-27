unit MessageWin;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RXCtrls, ExtCtrls, StdCtrls;

type
  TMessageWindow = class(TForm)
    Messages: TMemo;
    ToolPanel: TPanel;
    ClearBtn: TRxSpeedButton;
    SaveBtn: TRxSpeedButton;
    SaveDialog: TSaveDialog;
    procedure ClearBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Add(const AType, AMessage: String); overload;
    procedure Add(const AType : String; AMessages: TStrings); overload;
    procedure Add(const AType, AFormat : String; AParams : array of const); overload;
  end;

var
  MessageWindow: TMessageWindow;

implementation

uses Main;

{$R *.DFM}

procedure TMessageWindow.ClearBtnClick(Sender: TObject);
begin
  Messages.Lines.Clear;
end;

procedure TMessageWindow.SaveBtnClick(Sender: TObject);
begin
  if SaveDialog.Execute then begin
    Messages.Lines.SaveToFile(SaveDialog.FileName);
    ShowMessage('Messages saved to ' + SaveDialog.FileName + '.');
  end;
end;

procedure TMessageWindow.Add(const AType, AMessage: String);
begin
  Messages.Lines.Insert(0, Format('[%s] %s', [AType, AMessage]));
end;

procedure TMessageWindow.Add(const AType : String; AMessages: TStrings);
var
  I : Integer;
begin
  if AMessages.Count > 0 then
    for I := 0 to AMessages.Count-1 do
      Add(AType, AMessages[I]);
end;

procedure TMessageWindow.Add(const AType, AFormat : String; AParams : array of const);
begin
  Messages.Lines.Insert(0, '[' + AType + '] ' + Format(AFormat, AParams));
end;

end.
