unit AuthenticForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Htmlview, StdCtrls, ExtCtrls, StrHlder;

type
  TAuthenticDlg = class(TForm)
    AuthenticHtml: TStrHolder;
    Panel3: TPanel;
    Label1: TLabel;
    Bevel1: TBevel;
    Panel1: TPanel;
    Button1: TButton;
    Button3: TButton;
    Panel2: TPanel;
    InfoViewer: THTMLViewer;
    Image1: TImage;
    Panel4: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TAuthenticDlg.FormCreate(Sender: TObject);
begin
  InfoViewer.LoadStrings(AuthenticHtml.Strings);
end;

end.
