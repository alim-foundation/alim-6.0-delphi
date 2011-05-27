unit WebView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OleCtrls, SHDocVw_TLB, AmlView, ExtCtrls, islctrls, RXCtrls, StdCtrls,
  ShellAPI;

type
  TWebViewer = class(TForm)
    WebBrowser: TWebBrowser;
    ToolsPanel: TPanel;
    BtnBack: TRxSpeedButton;
    BtnForward: TRxSpeedButton;
    BtnStop: TRxSpeedButton;
    BtnRefresh: TRxSpeedButton;
    HeadingLabel: TLabel;
    BtnWebSearch: TRxSpeedButton;
    procedure WebBrowserStatusTextChange(Sender: TObject;
      const Text: WideString);
    procedure WebBrowserTitleChange(Sender: TObject;
      const Text: WideString);
    procedure BtnBackClick(Sender: TObject);
    procedure BtnForwardClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure BtnRefreshClick(Sender: TObject);
    procedure WebBrowserCommandStateChange(Sender: TObject;
      Command: Integer; Enable: WordBool);
    procedure BtnWebSearchClick(Sender: TObject);
  private
    FAddrMgr : IAddressManager;
    FStatus : IStatusDisplay;
    FActiveTitle : String;
    procedure SetURL(const AURL : String);
  public
    property AddressMgr : IAddressManager read FAddrMgr write FAddrMgr;
    property StatusMgr : IStatusDisplay read FStatus write FStatus;
    property URL : String write SetURL;
  end;

implementation

{$R *.DFM}

procedure TWebViewer.SetURL(const AURL : String);
var
  Null : OleVariant;
begin
  FAddrMgr.SetQuickLinks('', 0, Nil, '', tpNone);
  FillChar(Null, SizeOf(Null), 0);
  WebBrowser.Navigate(AUrl, Null, Null, Null, Null);
  WebBrowser.Toolbar := 0;
end;

procedure TWebViewer.WebBrowserStatusTextChange(Sender: TObject;
  const Text: WideString);
begin
  if Assigned(FStatus) then
    FStatus.StatusBegin(Text, False);
end;

procedure TWebViewer.WebBrowserTitleChange(Sender: TObject;
  const Text: WideString);
begin
  FActiveTitle := Text;
  HeadingLabel.Caption := FActiveTitle;
end;

procedure TWebViewer.BtnBackClick(Sender: TObject);
begin
  WebBrowser.GoBack;
end;

procedure TWebViewer.BtnForwardClick(Sender: TObject);
begin
  WebBrowser.GoForward;
end;

procedure TWebViewer.BtnStopClick(Sender: TObject);
begin
  WebBrowser.Stop;
end;

procedure TWebViewer.BtnRefreshClick(Sender: TObject);
begin
  WebBrowser.Refresh;
end;

procedure TWebViewer.WebBrowserCommandStateChange(Sender: TObject;
  Command: Integer; Enable: WordBool);
begin
  case Command of
    1 : BtnForward.Enabled := Enable;
    2 : BtnBack.Enabled := Enable;
  end;
end;

procedure TWebViewer.BtnWebSearchClick(Sender: TObject);
begin
  WebBrowser.GoSearch;
end;

end.
