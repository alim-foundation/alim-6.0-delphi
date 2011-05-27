unit HomeView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, AmlView, ComCtrls, islctrls, moneyctrls, StdCtrls, RXCtrls,
  CSPC, CSTCBase, StrHlder, TextView;

type
  THomeViewer = class(TAmlViewer)
    StatusPanel: TWallpaperPanel;
    HomeImage: TImage;
    TabPanel: TPanel;
    ShortcutTabs: TMoneyShortcutBar;
    StandardPanel: TPanel;
    Image1: TImage;
    Label2: TLabel;
    Bevel1: TBevel;
    QuranImage: TImage;
    QuranInfoLabel: TLabel;
    QuranBevel: TBevel;
    Image3: TImage;
    Bevel4: TBevel;
    Label4: TLabel;
    MoneyLabel1: TMoneyLabel;
    QuranLabel: TMoneyLabel;
    MoneyLabel3: TMoneyLabel;
    AdvancedPanel: TWallpaperPanel;
    AlimToolsPanel: TWallpaperPanel;
    AlimToolsPanelInside: TWallpaperPanel;
    AlimToolsBorder: TShape;
    OwnershipPanel: TPanel;
    AuthenticLabel: TMoneyLabel;
    AlimFAQLabel: TMoneyLabel;
    AuthenticHtml: TStrHolder;
    DownloadLabel: TMoneyLabel;
    procedure AdvancedPanelResize(Sender: TObject);
    procedure AdvancedPanelClick(Sender: TObject);
    procedure StandardPanelResize(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure QuranImageClick(Sender: TObject);
    procedure Image3Click(Sender: TObject);
    procedure ShortcutTabsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AuthenticLabelClick(Sender: TObject);
    procedure OwnershipPanelClick(Sender: TObject);
    procedure AlimFAQLabelClick(Sender: TObject);
    procedure DownloadLabelClick(Sender: TObject);
  protected
    FShortcuts : TBookmarks;

    procedure PositionShortcuts;
    procedure SetAddress(const AAddress : TAddress); override;
    procedure SetShortcuts(const AShortcuts : TBookmarks);
  public
    procedure ShowStatus(const AMsg : String);
    property Shortcuts : TBookmarks read FShortcuts write SetShortcuts;
  end;

implementation

{$R *.DFM}

uses Math, AuthenticForm;

procedure THomeViewer.SetAddress(const AAddress : TAddress);
begin
  FAddrMgr.SetQuickLinks('', clWhite, Nil, '', tpNone);
end;

procedure THomeViewer.PositionShortcuts;
const
  BorderX = 4;
  BorderY = 4;
var
  X, Col, ColCount, ColWidth, LineHeight, HighestY : Integer;

  procedure PlaceColumn(const AItems : TBookmarks);
  var
    I, Y : Integer;
    Node : TBookmarkNode;
  begin
    Y := BorderY;
    for I := 0 to AItems.Count-1 do begin
      Node := AItems[I];
      with TMoneyLabel(Node.ExtraData) do begin
        Left := X;
        Top := Y;
        Width := ColWidth - BorderX;
      end;
      Inc(Y, LineHeight);
      HighestY := Max(Y, HighestY);
    end;
  end;

begin
  // remember, column 0 is the "special" column

  ColCount := FShortcuts.Count;
  ColWidth := (AdvancedPanel.ClientWidth - AlimToolsPanel.Width - (BorderX*2)) div (ColCount-1);
  LineHeight := Canvas.TextHeight('A') + BorderY;

  HighestY := 0;
  X := AlimToolsPanel.Width + (BorderX*2);
  for Col := 1 to ColCount-1 do begin
    PlaceColumn(FShortcuts[Col].Children);
    Inc(X, ColWidth);
  end;

  AdvancedPanel.ClientHeight := Max(HighestY, AlimToolsPanel.Height);
end;

procedure THomeViewer.SetShortcuts(const AShortcuts : TBookmarks);

  procedure CreateSpecialLabels(const AItems : TBookmarks);
  var
    HotLabel : TMoneyLabel;
    Bullet : TLabel;
    Node : TBookmarkNode;
    I, Y, TextHt, Widest, HighestY : Integer;
  begin
    TextHt := Canvas.TextHeight('A') + 4;
    Widest := 0;
    HighestY := 0;

    Y := 6;
    for I := 0 to AItems.Count-1 do begin
      Node := AItems[I];
      Bullet := TLabel.Create(Self);
      with Bullet do begin
        Parent := AlimToolsPanelInside;
        Caption := '4';
        Left := 4;
        Top := Y-2;
        ParentFont := False;
        Font.Charset := SYMBOL_CHARSET;
        Font.Color := clYellow;
        Font.Height := -11;
        Font.Name := 'Webdings';
        //Font.Style = [];
        Transparent := True;
      end;

      HotLabel := TMoneyLabel.Create(Self);
      Node.ExtraData := HotLabel;
      with HotLabel do begin
        Parent := AlimToolsPanelInside;
        Left := 18;
        Top := Y;
        Width := Canvas.TextWidth(Node.Caption)+8;
        Alignment := maLeft;
        Caption := Node.Caption;
        Font.Color := $7fdfdf;
        OverTextColor := clWhite;
        Tag := LongInt(Node);
        OnClick := AdvancedPanelClick;

        Inc(Y, TextHt);
        Widest := Max(Widest, Left+Width+18);
        HighestY := Max(Y, HighestY);
      end;
    end;

    Inc(HighestY, 2);
    AdvancedPanel.Height := HighestY + (AlimToolsPanel.BorderWidth * 2);
    AlimToolsPanel.Width := Widest + (AlimToolsPanel.BorderWidth * 2);
    AlimToolsPanel.Height := HighestY + (AlimToolsPanel.BorderWidth * 2);
    AlimToolsPanelInside.Height := HighestY;
  end;

  procedure CreateLabels(const AItems : TBookmarks);
  var
    HotLabel : TMoneyLabel;
    Node : TBookmarkNode;
    I : Integer;
  begin
    for I := 0 to AItems.Count-1 do begin
      Node := AItems[I];
      HotLabel := TMoneyLabel.Create(Self);
      Node.ExtraData := HotLabel;
      with HotLabel do begin
        Parent := AdvancedPanel;
        Alignment := maLeft;
        Caption := '• ' + Node.Caption;
        Font.Color := clSilver;
        OverTextColor := clWhite;
        OnClick := AdvancedPanelClick;
        Tag := LongInt(Node);
      end;
    end;
  end;

var
  Col : Integer;
begin
  FShortcuts := AShortcuts;

  CreateSpecialLabels(FShortcuts[0].Children);
  for Col := 1 to FShortcuts.Count-1 do
    CreateLabels(FShortcuts[Col].Children);
  PositionShortcuts;
end;

procedure THomeViewer.ShowStatus(const AMsg : String);
begin
  if AMsg <> '' then begin
    StatusPanel.Visible := True;
    StatusPanel.Caption := AMsg;
  end else
    StatusPanel.Visible := False;
  Application.ProcessMessages;
end;

procedure THomeViewer.AdvancedPanelResize(Sender: TObject);
begin
  if FShortcuts <> Nil then
    PositionShortcuts;
end;

procedure THomeViewer.AdvancedPanelClick(Sender: TObject);
var
  Node : TBookmarkNode;
begin
  if (Sender as TWinControl).Tag > 0 then begin
    Node := TBookmarkNode((Sender as TWinControl).Tag);
    if Node.Address <> '' then
      GlobalSetAddress(Node.Address);
  end;
end;

procedure THomeViewer.StandardPanelResize(Sender: TObject);
const
  SpaceBetween = 10;
var
  LeftX, TotlWid : Integer;
begin
  TotlWid := QuranImage.Width + SpaceBetween + QuranLabel.Width;
  LeftX := (StandardPanel.Width div 2) - (TotlWid div 2);
  QuranImage.Left := LeftX;
  QuranLabel.Left := QuranImage.Left + QuranImage.Width + SpaceBetween;
  QuranBevel.Left := QuranLabel.Left;
  QuranInfoLabel.Left := QuranLabel.Left;
end;

procedure THomeViewer.Image1Click(Sender: TObject);
begin
  GlobalSetAddress('woi:');
end;

procedure THomeViewer.QuranImageClick(Sender: TObject);
begin
  GlobalSetAddress(CreateQuranAddress(qatMultiQuran, 1));
end;

procedure THomeViewer.Image3Click(Sender: TObject);
begin
  GlobalSetAddress('shb:');
end;

procedure THomeViewer.ShortcutTabsChange(Sender: TObject);
begin
  case ShortcutTabs.TabIndex of
    0 : begin
          StandardPanel.Visible := True;
          StandardPanel.Align := alClient;
          AdvancedPanel.Visible := False;
        end;
    1 : begin
          StandardPanel.Visible := False;
          AdvancedPanel.Visible := True;
          AdvancedPanel.Align := alClient;
        end;
  end;
end;

procedure THomeViewer.FormCreate(Sender: TObject);
begin
  TabPanel.Height := 90;
end;

procedure THomeViewer.AuthenticLabelClick(Sender: TObject);
var
  AuthDlg : TAuthenticDlg;
  AuthResult : Integer;
begin
  AuthDlg := TAuthenticDlg.Create(Application);
  AuthResult := AuthDlg.ShowModal;
  AuthDlg.Free;
  if AuthResult = mrNo then
    ShowText('Verify that your CD is Authentic', AuthenticHtml.Strings.Text, 600, 400);
end;

procedure THomeViewer.OwnershipPanelClick(Sender: TObject);
begin
  AuthenticLabelClick(Sender);
end;

procedure THomeViewer.AlimFAQLabelClick(Sender: TObject);
begin
  GlobalSetAddress('alimfaq:');
end;

procedure THomeViewer.DownloadLabelClick(Sender: TObject);
begin
  GlobalSetAddress('news:');
end;

end.
