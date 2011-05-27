unit TextView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Htmlview, StdCtrls, AmlView;

type
  TTextViewer = class(TAmlViewer)
    HtmlViewer: THTMLViewer;
  private
    procedure SetHeading(const AHeading : String);
    procedure SetHTML(const AHtml : String);
  public
    constructor Create(AOwner : TComponent); override;
    property Heading : String write SetHeading;
    property Html : String write SetHTML;
  end;

  function ShowText(const AHeading, AText : String;
                     AWidth, AHeight : Integer) : TTextViewer;

implementation

uses Main;

{$R *.DFM}

constructor TTextViewer.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  SetupHtmlViewerDefaults(HtmlViewer);
end;

procedure TTextViewer.SetHeading(const AHeading : String);
begin
  Caption := AHeading;
end;

procedure TTextViewer.SetHTML(const AHtml : String);
begin
  HtmlViewer.LoadFromBuffer(PChar(AHtml), Length(AHtml));
  if HtmlViewer.DocumentTitle <> '' then
    Caption := HtmlViewer.DocumentTitle;
end;

function ShowText(const AHeading, AText : String; AWidth, AHeight : Integer) : TTextViewer;
begin
  Result := TTextViewer.Create(Application);
  Result.AddressMgr := MainForm;
  Result.DataMgr := MainForm.DataMgr;
  Result.StatusDisplay := MainForm;
  Result.Html := AText;
  if AHeading <> '' then
    Result.Heading := AHeading;
  Result.Width := AWidth;
  Result.Height := AHeight;
  Result.Show;
end;

end.
