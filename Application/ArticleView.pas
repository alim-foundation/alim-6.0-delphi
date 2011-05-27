unit ArticleView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ToolWin, ComCtrls, ExtCtrls, Htmlview, XmlObjModel, IslUtils,
  RXCtrls, islctrls, SpeedBar, jpeg, RXSpin, StdCtrls, AmlView, CompDoc,
  QuranStruct, ImgList, DFSSplitter, sCombos, moneyctrls, Spin, Menus,
  RxMenus;

const
  NavgPanelDefWidth = 175;
  IconIdxPurpleBookClosed = 0;
  IconIdxPurpleBookOpen = 1;
  IconIdxGreenBookClosed = 2;
  IconIdxGreenBookOpen = 3;
  IconIdxPage = 4;
  IconIdxArrow = 7;
  IconIdxBullet = 6;
  IconIdxPageParent = 8;

type
  (*
  Outline Structures
  - none (unknown)
  - book is a regular book with next/prev page navigation and all pages in Outline
  - page is a regular book with next/prev page navigation and all sections of page in Outline
  - article is a collection of pages in a book with no next/page navigation and all sections of article in Outline
  *)
  TOutlineStructure = (osNone, osBook, osPage, osArticle);
  TOutlineViewer = (ovNone, ovHtml, ovHtmlCombo, ovList, ovListCombo, ovTree);
  TArticleData = class(TAmlData)
  protected
    FViewerType : String;
    FOutline : TOutlineStructure;
    FOutlViewer : TOutlineViewer;
    FShowNavgPageMenus : Boolean;
    FShowNavgItemMenus : Boolean;
    FLibName : String;
    FCatalogNavg : TXmlElement;
    FContentStruct : TXmlElement;
    FContentPage : TXmlElement;
    FContentItem : TXmlElement;
    FTOC : TStringList;
    FSuraArticles : Boolean;
    FTabsShortcuts : TBookmarks;
    FSplitSections : Boolean;
    FBrowsePages : Boolean;

    function GetLibName : String; override;
    function CreateViewer : TAmlViewer; override;
  public
    function InitializeLoad(const AFileName : String;
                            const ACatalog : TXmlElement) : Boolean; override;
    procedure FinalizeLoad; override;
    procedure Close; override;
    function GetAddressCaption(const AAddress : TAddress; const AIncludeName : Boolean) : String; override;

    property TableOfContents : TStringList read FTOC;
    property Structure : TXmlElement read FContentStruct;
    property SuraArticles : Boolean read FSuraArticles;
    property Outline : TOutlineStructure read FOutline;
    property OutlViewer : TOutlineViewer read FOutlViewer;
    property TabsShortcuts : TBookmarks read FTabsShortcuts;
    property PageInfo : TXmlElement read FContentPage;
    property ItemInfo : TXmlElement read FContentItem;
    property SplitSections : Boolean read FSplitSections;
    property BrowsePages : Boolean read FBrowsePages;
  end;

  TTreeNodeData =   // needs to fit inside a Pointer (four bytes)
    packed record
      PageIdx : Word;
      SectIdx : Word;
    end;

  TArticleViewScope = (avPage, avSection);
  TArticleNavigation = (anNone, anHtml, anHtmlCombo, anList, anListCombo, anTree);
  TArticleNavgManipulation = (nmHide, nmMinMax, nmShow);
  TArticleViewer = class(TAmlViewer)
    HeadingPanel: TWallpaperPanel;
    DataPanel: TPanel;
    OutlSplitter: TSplitter;
    ContentsPanel: TPanel;
    HtmlViewer: THTMLViewer;
    SubheadingPanel: TWallpaperPanel;
    Subheading: TRxLabel;
    NavgPanel: TPanel;
    OutlHtmlViewer: THTMLViewer;
    ComboPanel: TPanel;
    CntOutlineCombo: TsComboBox;
    ArticleHeadingLabel: TLabel;
    BookHeadingLabel: TLabel;
    FillerPanel: TWallpaperPanel;
    SectionSpinBtn: TMoneySpinButton;
    SectMenuButton: TRxSpeedButton;
    SectMenu: TRxPopupMenu;
    ArticlePrevLabel: TMoneyLabel;
    ArticleNextLabel: TMoneyLabel;
    NavgExpandLabel: TMoneyLabel;
    NavgCloseLabel: TMoneyLabel;
    OpenNavgLabel: TMoneyLabel;
    OutlTreeViewer: TTreeView;
    SmallImages: TImageList;
    procedure ShortcutsListClick(Sender: TObject);
    procedure OutlSplitterMaximize(Sender: TObject);
    procedure CntOutlineComboChange(Sender: TObject);
    procedure OutlListViewerResize(Sender: TObject);
    procedure OutlTreeViewerClick(Sender: TObject);
    procedure SectionSpinBtnUpClick(Sender: TObject);
    procedure SectionSpinBtnDownClick(Sender: TObject);
    procedure SectMenuGetItemParams(Sender: TMenu; Item: TMenuItem;
      State: TMenuOwnerDrawState; AFont: TFont; var Color: TColor;
      var Graphic: TGraphic; var NumGlyphs: Integer);
    procedure SectMenuPopup(Sender: TObject);
    procedure ArticlePrevLabelClick(Sender: TObject);
    procedure ArticleNextLabelClick(Sender: TObject);
    procedure FillerPanelDblClick(Sender: TObject);
    procedure NavgCloseLabelClick(Sender: TObject);
    procedure OpenNavgLabelClick(Sender: TObject);
  protected
    FBook : TArticleData;
    FAmlData : TXmlElement;
    FViewType : TArticleViewScope;
    FNavgType : TArticleNavigation;
    FNavgDataChanged : Boolean;
    FPageTitle : String;
    FPageIdx : Integer;
    FSections : TXmlNodeList;
    FActiveSection : TXmlElement;
    FAllDataOfThisType : TDataList;
    FExtraLinks : TCaptionedAddressList;
    FNavgHiddenByUser : Boolean;

    function SectListToHtml(const ASectList : TXmlNodeList;
                            const AExtraLinks : TCaptionedAddressList;
                            const AAddSelAnchor : String = 'SELECTED_SECT';
                            const ATableAttrs : String = '') : String;
    procedure SectListToListViewer(const ASectList : TXmlNodeList; const AExtraLinks : TCaptionedAddressList);
    function ContentsToHtml(const AExtraLinks : TCaptionedAddressList) : String;
    procedure ContentsToListViewer(const AExtraLinks : TCaptionedAddressList);
    procedure ContentsToTreeViewer;

    function GetActiveData : TAmlData; override;
    procedure InternalHtmlHotSpotClick(Sender: TObject; const APrimarySrc, ASecondarySrc : String); override;

    procedure ClearContents; virtual;
    procedure UpdateContents; virtual;
    procedure UpdateNavigation; virtual;
    procedure SetActiveBook(const ABook : TArticleData); virtual;
    procedure SetPageIndex(const APageIdx : Integer); virtual;
    procedure SetActivePage(const APageTitle : String); virtual;
    procedure SetActiveSection(const ASection : TXmlElement); overload; virtual;
    procedure SetActiveSection(const ASectHead : String); overload; virtual;
    procedure SetAddress(const AAddress : TAddress); override;
    procedure SetNavgType(const AType : TArticleNavigation); virtual;
    procedure SetViewType(const AType : TArticleViewScope); virtual;
    procedure SectMenuClick(Sender : TObject);

    procedure ManipulateNavg(const AWhat : TArticleNavgManipulation);

    function GetEnableCopy : Boolean; override;
    function GetEnablePrint : Boolean; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure Navigate(const AType : TNavigate; const ADirection : TNavigateDirection; const AAddress : TCaptionedAddress = Nil); override;
    function SelectSura(const ASura : TSuraNum) : Boolean; override;

    procedure Print(const ADialog : TPrintDialog); override;
    procedure CopyToClipboard; override;

    property ViewType : TArticleViewScope read FViewType write SetViewType;
  end;

implementation

{$R *.DFM}

uses StStrS, QuranView, Math;

const
  SubSectDelim = '/';

function TArticleData.InitializeLoad(const AFileName : String;
                                     const ACatalog : TXmlElement) : Boolean;
begin
  Result := inherited InitializeLoad(AFileName, ACatalog);
  if not Result then
    Exit;

  FCatalogNavg := FCatalog.FindElement('navigation');
  FContentStruct := FCatalogContent.FindElement('structure');
  FContentPage := FCatalogContent.FindElement('page');
  FContentItem := FCatalogContent.FindElement('item');

  FSplitSections := FContentItem.GetAttribute('split') <> 'no';

  FLibName := FCatalogContent.GetAttribute('library');
  if FCatalogNavg <> Nil then begin
    FOutline := TOutlineStructure(FindString(FCatalogNavg.GetAttribute('style'), ['none', 'book', 'page', 'article'], 1));
    FOutlViewer := TOutlineViewer(FindString(FCatalogNavg.GetAttribute('viewer'), ['none', 'html', 'htmlcombo', 'list', 'listcombo', 'tree'], 2));
  end else begin
    FOutline := osNone;
    FOutlViewer := ovNone;
  end;
  //FOutlViewer := ovTree;

  if FTabsShortcuts = Nil then
    FTabsShortcuts := TBookmarks.Create
  else
    FTabsShortcuts.Clear;

  FBrowsePages := (FOutline <> osArticle) and (FContentPage.GetAttribute('browse') <> 'no');
  SetFlag(adfShareViewer);
end;

procedure TArticleData.FinalizeLoad;
var
  I, SL : Integer;
  Node : TXmlElement;
  FillTabs : Boolean;
  PageTitleAttr : String;
begin
  inherited FinalizeLoad;

  FTOC := TStringList.Create;
  if FContentSubtype = 'suras' then begin
    FSuraArticles := True;
    if FQuranStruct <> Nil then begin
      for I := Low(TSuraNum) to High(TSuraNum) do
         FTOC.Add(Format('Sura %d. %s', [I, FQuranStruct.SuraName[I]]));
    end else begin
      for I := Low(TSuraNum) to High(TSuraNum) do
        FTOC.Add(Format('Sura %d', [I]));
    end;
    FOutline := osBook;
    FOutlViewer := ovHtmlCombo;
  end else begin
    FSuraArticles := False;
    PageTitleAttr := FContentPage.GetAttribute('attr');
    SL := FContentStruct.ChildNodes.Length;
    FillTabs := FContentPage.GetAttribute('tabs') = 'yes';
    if SL > 0 then begin
      Dec(SL);
      for I := 0 to SL do begin
        Node := FContentStruct.ChildNodes.Item(I) as TXmlElement;
        FTOC.Add(Node.GetAttribute(PageTitleAttr));
        if FillTabs then
          FTabsShortcuts.AddChild('', Format('%s:%s', [FId, Node.GetAttribute(PageTitleAttr)]), Node.GetAttribute('tab'), Format('%.4d', [I]));
      end;
    end;
  end;

  if FTOC.Count = 0 then
    ShowMessage(Format('Warning: %s has no Table Of Contents', [FName]));
end;

procedure TArticleData.Close;
begin
  if FTOC <> Nil then begin
    FTOC.Free;
    FTOC := Nil;
  end;
  if FTabsShortcuts <> Nil then begin
    FTabsShortcuts.Free;
    FTabsShortcuts := Nil;
  end;
  inherited Close;
end;

function TArticleData.GetAddressCaption(const AAddress : TAddress; const AIncludeName : Boolean) : String;
var
  PageRef, PageTitle : String;
  PageIdx, Sura : Word;
begin
  Assert(FTOC <> Nil);

  PageRef := AAddress.BookPage;
  if PageRef <> '' then begin
    if (FSuraArticles) and
       (PageRef[1] = 'S') and Str2WordS(Copy(PageRef, 2, 3), Sura) then
      PageTitle := FTOC[Sura-1]
    else if Str2WordS(PageRef, PageIdx) then
      PageTitle := FTOC[PageIdx]
    else
      PageTitle := PageRef;
  end else begin
    if (FTOC.Count > 0) then
      PageTitle := FTOC[0];
  end;

  if AIncludeName then
    Result := Format('%s (%s)', [Name, PageTitle])
  else
    Result := PageTitle;
end;

function TArticleData.GetLibName : String;
begin
  Result := FLibName;
end;

function TArticleData.CreateViewer : TAmlViewer;
begin
  Result := TArticleViewer.Create(Application);
end;

{------------------------------------------------------------------------------}

constructor TArticleViewer.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  SetupHtmlViewerDefaults(HtmlViewer);
  SetupHtmlViewerDefaults(OutlHtmlViewer);
  ManipulateNavg(nmShow);
end;

destructor TArticleViewer.Destroy;
begin
  if FAmlData <> Nil then
    FAmlData.Free;
  if FSections <> Nil then
    FSections.Free;
  if FExtraLinks <> Nil then
    FExtraLinks.Free;
  if FAllDataOfThisType <> Nil then
    FAllDataOfThisType.Free;
  inherited Destroy;
end;

function SectListToBookmarks(const AMarks : TBookmarks;
                             const ANodes : TXmlNodeList;
                             const AddrPrefix : String = '';
                             const AActiveSect : TXmlElement = Nil) : TBookmarkNode;
var
  I : Integer;
  Elem : TXmlElement;
  Addr : String;
  NewNode : TBookmarkNode;
  ChildSects : TXmlNodeList;
begin
  Result := Nil;
  if (ANodes = Nil) or (ANodes.Length <= 0) then
    Exit;

  for I := 0 to ANodes.Length-1 do begin
    Elem := ANodes.Item(I) as TXmlElement;
    if AddrPrefix = '' then
      Addr := IntToStr(I)
    else
      Addr := AddrPrefix + SubSectDelim + IntToStr(I);
    NewNode := AMarks.AddChild('', Addr, Elem.GetAttribute('heading'), Format('%.4d', [I]));
    if Elem = AActiveSect then
      Result := NewNode;

    ChildSects := Elem.GetChildElementsByTagName('section');
    if ChildSects.Length > 0 then begin
      NewNode.Children.AddChild('', Addr, NewNode.Caption, '00000').BreakAfter := True;
      SectListToBookmarks(NewNode.Children, ChildSects, Addr);
    end;
  end;
end;

function TArticleViewer.SectListToHtml(const ASectList : TXmlNodeList;
                                       const AExtraLinks : TCaptionedAddressList;
                                       const AAddSelAnchor : String;
                                       const ATableAttrs : String) : String;
const
  Bullet = '§';
  SelBullet = 'F';
  InnerTableFmt = '<table border=0 cellspacing=0 cellpadding=2 %1:s>%0:s</table>';
  RowFmt = '<tr valign=top><td align=center><font face="wingdings">%2:s</font></td><td><a href="%1:s">%0:s</a></td></td></tr>';
  SelRowFmt = '<tr valign=top><td align=center><font face="wingdings"><font color="MAROON"><a name="%2:s">%1:s</a></font></font></td><td><font color="MAROON"><b>%0:s</b></font></td></td></tr>';
  ExtraSep = '<p><hr size=1 noshade color=silver width=100%>';
var
  N : Cardinal;
  Node : TXmlElement;
  ExtraRows, Rows, Heading : String;
  CA : TCaptionedAddress;
begin
  Result := '';
  if (ASectList = Nil) or (ASectList.Length <= 0) then
    Exit;

  Rows := '';
  for N := 0 to ASectList.Length-1 do begin
    Node := ASectList.Item(N) as TXmlElement;
    Heading := Node.GetAttribute('heading');
    if Heading = '' then
      Heading := 'Heading Missing';
    if Node = FActiveSection then
      Rows := Rows + Format(SelRowFmt, [Heading, SelBullet, AAddSelAnchor])
    else
      Rows := Rows + Format(RowFmt, [Heading, AddrInternalHotspotPrefix+FPageTitle+AddrPathDelim+Heading, Bullet]);
  end;

  ExtraRows := '';
  if (AExtraLinks <> Nil) and (AExtraLinks.Count > 0) then begin
    for N := 0 to AExtraLinks.Count-1 do begin
      CA := AExtraLinks[N];
      ExtraRows := ExtraRows + Format(RowFmt, [CA.Caption, CA.Address, Bullet]);
    end;
  end;

  Result := Format(InnerTableFmt, [Rows, ATableAttrs]);
  if ExtraRows <> '' then
    Result := Result + ExtraSep + Format(InnerTableFmt, [ExtraRows, ATableAttrs]);
end;

procedure TArticleViewer.SectListToListViewer(const ASectList : TXmlNodeList; const AExtraLinks : TCaptionedAddressList);
var
  N : Cardinal;
  Node : TXmlElement;
  CA : TCaptionedAddress;
  LI : TTreeNode;
  Heading : String;
begin
  Assert(ASectList <> Nil);
  if ASectList.Length <= 0 then
    Exit;

  for N := 0 to ASectList.Length-1 do begin
    Node := ASectList.Item(N) as TXmlElement;
    Heading := Node.GetAttribute('heading');
    if Heading = '' then
      Heading := 'Heading Missing';
    LI := OutlTreeViewer.Items.Add(Nil, Heading);
    LI.Data := Node;
    LI.ImageIndex := IconIdxPage;
    LI.StateIndex := IconIdxPage;
    LI.SelectedIndex := IconIdxPage;
    if Node = FActiveSection then
      LI.Selected := True;
  end;

  if (AExtraLinks <> Nil) and (AExtraLinks.Count > 0) then begin
    for N := 0 to AExtraLinks.Count-1 do begin
      CA := AExtraLinks[N];
      LI := OutlTreeViewer.Items.Add(Nil, CA.Caption);
      LI.Data := CA;
      LI.ImageIndex := IconIdxPage;
    end;
  end;
end;

function TArticleViewer.ContentsToHtml(const AExtraLinks : TCaptionedAddressList) : String;
const
  Bullet = 'n';
  SelBullet = 'F';
  InnerTableFmt = '<table border=0 cellspacing=0 cellpadding=2>%s</table>';
  RowFmt = '<tr valign=top><td align=center><font face="wingdings" color="silver">%2:s</font></td><td><a href="%1:s">%0:s</a></td></td></tr>';
  SelRowNoSectFmt = '<tr valign=top bgcolor="navy"><td align=center><font face="wingdings" color="yellow"><a name="SELECTED">%1:s</a></font></td><td><font color="yellow"><b>%0:s</b></font></td></td></tr>';
  ExtraSep = '<p><hr size=1 noshade color=silver width=100%>';
  SelRowWithSectFmt =
    '<tr valign=top bgcolor="F1EEF7">'+
    '  <td align=center bgcolor="navy">'+
    '    <font face="wingdings" color="yellow">'+
    '    <a name="SELECTED">%1:s</a>'+
    '    </font>'+
    '  </td>'+
    '  <td bgcolor="navy">'+
    '    <font color="yellow"><b>%0:s</b></font>'+
    '  </td>'+
    '</tr>'+
    '<tr bgcolor="FFEEF1">'+
    '    <td>&nbsp</td><td>%2:s</td>'+
    '</tr>';
var
  N : Cardinal;
  ExtraRows, Rows, PageTitle, Outline : String;
  CA : TCaptionedAddress;
begin
  Result := '';
  if FBook.TableOfContents.Count <= 0 then
    Exit;

  Rows := '';
  for N := 0 to FBook.TableOfContents.Count-1 do begin
    PageTitle := FBook.TableOfContents[N];
    if CompareText(PageTitle, FPageTitle) = 0 then begin
      Outline := SectListToHtml(FSections, Nil, '');
      if Outline = '' then
        Rows := Rows + Format(SelRowNoSectFmt, [PageTitle, SelBullet])
      else
        Rows := Rows + Format(SelRowWithSectFmt, [PageTitle, SelBullet, Outline]);
    end else
      Rows := Rows + Format(RowFmt, [PageTitle, AddrInternalHotspotPrefix+PageTitle, Bullet]);
  end;

  ExtraRows := '';
  if (AExtraLinks <> Nil) and (AExtraLinks.Count > 0) then begin
    for N := 0 to AExtraLinks.Count-1 do begin
      CA := AExtraLinks[N];
      ExtraRows := ExtraRows + Format(RowFmt, [CA.Caption, CA.Address, Bullet]);
    end;
  end;

  Result := Format(InnerTableFmt, [Rows]);
  if ExtraRows <> '' then
    Result := Result + ExtraSep + Format(InnerTableFmt, [ExtraRows]);
end;

procedure TArticleViewer.ContentsToListViewer(const AExtraLinks : TCaptionedAddressList);
var
  N : Cardinal;
  LI : TTreeNode;
  PageTitle : String;
  CA : TCaptionedAddress;
begin
  for N := 0 to FBook.TableOfContents.Count-1 do begin
    PageTitle := FBook.TableOfContents[N];
    LI := OutlTreeViewer.Items.Add(Nil, PageTitle);
    LI.ImageIndex := IconIdxPage;
    LI.StateIndex := IconIdxPage;
    LI.SelectedIndex := IconIdxPage;
    if CompareText(PageTitle, FPageTitle) = 0 then
      LI.Selected := True;
  end;

  if (AExtraLinks <> Nil) and (AExtraLinks.Count > 0) then begin
    for N := 0 to AExtraLinks.Count-1 do begin
      CA := AExtraLinks[N];
      LI := OutlTreeViewer.Items.Add(Nil, CA.Caption);
      LI.Data := CA;
      LI.ImageIndex := IconIdxPage;
    end;
  end;
end;

procedure TArticleViewer.ContentsToTreeViewer;
var
  NodeData : TTreeNodeData;
  P, ChildCount : Integer;
  PageNode : TXmlNode;
  PageElem : TXmlElement;
  PageTreeNode : TTreeNode;
  PageTag, PageHdAttr, SectTag, SectHdAttr : String;

  procedure ProcessSections(const AXmlParent : TXmlElement;
                            const ATreeParent : TTreeNode;
                            const ANodeData : Pointer;
                            const ALevel : Integer);
  var
    S, SectChildCount : Integer;
    SectNode : TXmlNode;
    SectElem : TXmlElement;
    SectTreeNode : TTreeNode;
  begin
    SectChildCount := AXmlParent.ChildNodes.Length-1;
    if SectChildCount >= 0 then begin
      for S := 0 to SectChildCount do begin
        SectNode := AXmlParent.ChildNodes.Item(S);
        if SectNode.NodeName = SectTag then begin
          SectElem := SectNode as TXmlElement;
          SectTreeNode := OutlTreeViewer.Items.AddChild(ATreeParent, SectElem.GetAttribute(SectHdAttr));
          if ANodeData = Nil then begin
            NodeData.SectIdx := S;
            SectTreeNode.Data := Pointer(NodeData);
          end else
            SectTreeNode.Data := ANodeData;
          if ALevel < 1 then begin
            SectTreeNode.ImageIndex := IconIdxBullet;
            SectTreeNode.StateIndex := IconIdxBullet;
            SectTreeNode.SelectedIndex := IconIdxBullet;
          end else begin
            SectTreeNode.ImageIndex := IconIdxBullet;
            SectTreeNode.StateIndex := IconIdxBullet;
            SectTreeNode.SelectedIndex := IconIdxBullet;
          end;
          ProcessSections(SectElem, SectTreeNode, SectTreeNode.Data, Succ(ALevel));
        end;
      end;
    end;
  end;

begin
  PageTag     := FBook.PageInfo.GetAttribute('tag');
  PageHdAttr  := FBook.PageInfo.GetAttribute('attr');
  SectTag     := FBook.ItemInfo.GetAttribute('tag');
  SectHdAttr  := FBook.ItemInfo.GetAttribute('attr');

  ChildCount := FBook.Structure.ChildNodes.Length-1;
  for P := 0 to ChildCount do begin
    PageNode := FBook.Structure.ChildNodes.Item(P);
    if PageNode.NodeName = PageTag then begin
      PageElem := PageNode as TXmlElement;
      PageTreeNode := OutlTreeViewer.Items.Add(Nil, PageElem.GetAttribute(PageHdAttr));
      FBook.TableOfContents.Objects[P] := PageTreeNode;
      if PageElem.ChildNodes.Length > 0 then begin
        PageTreeNode.ImageIndex := IconIdxPageParent;
        PageTreeNode.SelectedIndex := IconIdxPageParent;
      end else begin
        PageTreeNode.ImageIndex := IconIdxPage;
        PageTreeNode.SelectedIndex := IconIdxPage;
      end;
      NodeData.PageIdx := P;
      ProcessSections(PageElem, PageTreeNode, Nil, 0);
    end;
  end;
end;

procedure TArticleViewer.Navigate(const AType : TNavigate; const ADirection : TNavigateDirection; const AAddress : TCaptionedAddress);
var
  I : Integer;
begin
  case AType of
    nvPage :
      case ADirection of
        nvdPrevious, nvdNext :
          if (FBook.TableOfContents <> Nil) and (FBook.TableOfContents.Count > 0) then begin
            I := FPageIdx;
            case ADirection of
              nvdPrevious : if FPageIdx > 0 then I := Pred(FPageIdx);
              nvdNext : if FPageIdx < FBook.TableOfContents.Count-1 then I := Succ(FPageIdx);
            end;
            if I <> FPageIdx then
              SetActivePage(FBook.TableOfContents[I]);
          end;
        nvdCaptionedAddr :
          begin
            Assert(AAddress <> Nil);
            SetActivePage(AAddress.Caption);
          end;
      end;
    nvItem :
      case ADirection of
        nvdPrevious, nvdNext :
          if (FActiveSection <> Nil) and (FSections.Length > 0) then begin
            I := FSections.IndexOf(FActiveSection);
            case ADirection of
              nvdPrevious : if I > 0 then SetActiveSection(FSections.Item(Pred(I)) as TXmlElement);
              nvdNext : if I < FSections.Length-1 then SetActiveSection(FSections.Item(Succ(I)) as TXmlElement);
            end;
          end;
        nvdCaptionedAddr :
          begin
            Assert(AAddress <> Nil);
            SetActiveSection(AAddress.Caption);
          end;
      end;
  end;
end;

procedure TArticleViewer.ClearContents;
begin
  FPageIdx := -1;
  FPageTitle := '';
  FActiveSection := Nil;
  FNavgDataChanged := True;

  HeadingPanel.Caption := '';
  ArticleHeadingLabel.Caption := '';
  BookHeadingLabel.Caption := '';
  Subheading.Caption := '';
  HtmlViewer.LoadFromBuffer('', 0);
  OutlHtmlViewer.LoadFromBuffer('', 0);
  OutlTreeViewer.Items.BeginUpdate;
  OutlTreeViewer.Items.Clear;
  OutlTreeViewer.Items.EndUpdate;

  CntOutlineCombo.Items.BeginUpdate;
  CntOutlineCombo.Items.Clear;
  CntOutlineCombo.Items.EndUpdate;

  if FAllDataOfThisType <> Nil then begin
    FAllDataOfThisType.Free;
    FAllDataOfThisType := Nil;
  end;
  if FExtraLinks <> Nil then
    FExtraLinks.Clear;
end;

procedure TArticleViewer.SetActiveBook(const ABook : TArticleData);
var
  GroupMembers : TBookmarks;
  HaveGroups : Boolean;
begin
  if ABook = Nil then begin
    ClearContents;
    Exit;
  end;

  if ABook <> FBook then begin
    ClearContents;
    FBook := ABook;

    Assert(FDataMgr <> Nil);
    if FAllDataOfThisType <> Nil then
      FAllDataOfThisType.Free;
    FAllDataOfThisType := FDataMgr.GetAll(dslDataType, FBook.ContentTypeKey);
    HtmlViewer.NoSelect := not FBook.FlagIsSet(adfCanCopyText);

    CntOutlineCombo.Items.AddStrings(FBook.TableOfContents);
    if (FBook.Outline = osBook) and (FBook.OutlViewer = ovList) then begin
      SetNavgType(anList);
      OutlTreeViewer.Items.BeginUpdate;
      OutlTreeViewer.Items.Clear;
      ContentsToListViewer(FExtraLinks);
      OutlTreeViewer.Items.EndUpdate;
    end else if (FBook.Outline = osBook) and (FBook.OutlViewer = ovTree) then begin
      SetNavgType(anTree);
      OutlTreeViewer.Items.BeginUpdate;
      OutlTreeViewer.Items.Clear;
      ContentsToTreeViewer;
      OutlTreeViewer.Items.EndUpdate;
    end;
    FPrintHeader.BookName := FBook.Name;
  end;

  if (FBook.TabsShortcuts = Nil) or (FBook.TabsShortcuts.Count = 0) then begin
    HaveGroups := False;
    if FBook.GroupName <> '' then begin
      GroupMembers := FDataMgr.GroupMembers[FBook.GroupName];
      if GroupMembers.Count > 1 then begin
        FAddrMgr.SetQuickLinks(FBook.GroupName, FillerPanel.Color, GroupMembers, FBook.ShortName, tpNone);
        HaveGroups := True;
      end;
    end;
    if not HaveGroups then FAddrMgr.SetQuickLinks('', 0, Nil, '', tpNone);
  end else
    FAddrMgr.SetQuickLinks(' ', FillerPanel.Color, FBook.TabsShortcuts, FBook.TabsShortcuts.Bookmarks[0].Caption, tpNone);
end;

procedure TArticleViewer.SetPageIndex(const APageIdx : Integer);
var
  T : Integer;
  OtherData : TAmlData;
begin
  if (FBook = Nil) then begin
    ClearContents;
    Exit;
  end;

  if (APageIdx <> FPageIdx) or (APageIdx < 0) then begin
    FStatus.StatusBegin('Loading page...', True);
    if APageIdx < 0 then
      FPageIdx := 0
    else
      FPageIdx := APageIdx;

    if FAmlData <> Nil then
      FAmlData.Free;
    FAmlData := FBook.ReadXML(IntToStr(FPageIdx));

    if FSections <> Nil then begin
      FSections.Free;
      FSections := Nil;
    end;
    FSections := FAmlData.GetChildElementsByTagName('section');

    if (FBook.Outline = osPage) or (FBook.Outline = osArticle) then
      FNavgDataChanged := True;
    if FBook.TableOfContents.Objects[APageIdx] is TTreeNode then
      (FBook.TableOfContents.Objects[APageIdx] as TTreeNode).Selected := True;

    FStatus.StatusEnd;
  end;

  SectionSpinBtn.Visible := (FSections.Length > 0) and FBook.SplitSections;
  if FSections.Length > 0 then begin
    SectMenuButton.Visible := True;
    Subheading.Left := SectMenuButton.Left + SectMenuButton.Width + 2;
  end else begin
    SectMenuButton.Visible := False;
    Subheading.Left := SectMenuButton.Left;
  end;

  if (FSections.Length > 0) and FBook.SplitSections then begin
    FActiveSection := FSections.Item(0) as TXmlElement;
    FViewType := avSection;
  end else begin
    FViewType := avPage;
    FActiveSection := Nil;
  end;

  if FExtraLinks <> Nil then
    FExtraLinks.Clear;
  if FBook.SuraArticles then begin
    if FExtraLinks = Nil then
      FExtraLinks := TCaptionedAddressList.Create;

    FExtraLinks.AppendAddr('Goto Sura in Quran', CreateQuranAddress(qatMultiQuran, Succ(FPageIdx), 1));
    if FAllDataOfThisType.Count > 0 then begin
      for T := 0 to FAllDataOfThisType.Count-1 do begin
        OtherData := FAllDataOfThisType[T];
        if (OtherData.Id <> FBook.Id) and (OtherData is TArticleData) and ((OtherData as TArticleData).SuraArticles) then
          FExtraLinks.AppendAddr('View ' + FAllDataOfThisType[T].Name, Format('%s:%d', [FAllDataOfThisType[T].Id, FPageIdx]));
      end;
    end;
  end;

  if FActiveSection <> Nil then
    SetActiveSection(FActiveSection)
  else
    UpdateContents;

  HtmlViewer.SetFocus;
end;

procedure TArticleViewer.SetActivePage(const APageTitle : String);
var
  PageIdx : Integer;
  Sura : Word;
begin
  if FBook = Nil then begin
    ClearContents;
    Exit;
  end;

  // if APageTitle is Sxxx then xxx is the sura number
  // if APageTitle is numeric, then goto the numbered page
  // if APageTitle is alphanumeric, then goto the named page

  try
    if (APageTitle <> '') and (FBook.SuraArticles) and
       (APageTitle[1] = 'S') and Str2WordS(Copy(APageTitle, 2, 3), Sura) then
      FPageTitle := FBook.TableOfContents[Sura-1]
    else if Str2LongS(APageTitle, PageIdx) then
      FPageTitle := FBook.TableOfContents[PageIdx]
    else
      FPageTitle := APageTitle;
    PageIdx := FBook.TableOfContents.IndexOf(FPageTitle);
    if PageIdx >= 0 then
      SetPageIndex(PageIdx)
    else begin
      FPageTitle := FBook.TableOfContents[0];
      SetPageIndex(0);
      if APageTitle <> '' then
        FStatus.StatusBegin(Format('Page "%s" not found in %s', [APageTitle, FBook.Name]), False);
    end;
  except
    FPageTitle := FBook.TableOfContents[0];
    SetPageIndex(0);
    FStatus.StatusBegin(Format('Page "%s" not found in %s', [APageTitle, FBook.Name]), False);
  end;
end;

procedure TArticleViewer.SetActiveSection(const ASection : TXmlElement);
begin
  if ASection = Nil then
    Exit;

  FActiveSection := ASection;
  if FViewType = avPage then
    HtmlViewer.PositionTo(Format('Sect1_%d', [FSections.IndexOf(FActiveSection)]))
  else
    UpdateContents;
end;

procedure TArticleViewer.SetActiveSection(const ASectHead : String);
var
  SectIdx : Integer;
  Section : TXmlElement;
begin
  if (FSections = Nil) or (FSections.Length = 0) then
    Exit;

  if Str2LongS(ASectHead, SectIdx) then begin
    if (SectIdx >= 0) and (SectIdx < FSections.Length) then
      SetActiveSection(FSections.Item(SectIdx) as TXmlElement)
    else
      FStatus.StatusBegin(Format('Section %s not found.', [ASectHead]), False);
  end else begin
    for SectIdx := 0 to FSections.Length-1 do begin
      Section := FSections.Item(SectIdx) as TXmlElement;
      if CompareText(TrimS(Section.GetAttribute('heading')), TrimS(ASectHead)) = 0 then begin
        SetActiveSection(Section);
        Exit;
      end;
    end;
    FStatus.StatusBegin(Format('Section "%s" not found.', [ASectHead]), False);
  end;
end;

procedure TArticleViewer.SetAddress(const AAddress : TAddress);
var
  Book : TAmlData;
begin
  Book := FDataMgr.DataIds[AAddress.BookId];
  if Book is TArticleData then begin
    SetActiveBook(Book as TArticleData);
    SetActivePage(AAddress.BookPage);
    FSearchHits := AAddress.SearchHits;
    if AAddress.Path <> '' then
      SetActiveSection(AAddress.Path)
    else
      UpdateContents;
  end;
end;

procedure TArticleViewer.SetNavgType(const AType : TArticleNavigation);
begin
  FNavgType := AType;
  if not FNavgHiddenByUser then begin
    NavgPanel.Visible := FNavgType <> anNone;
    OutlSplitter.Visible := FNavgType <> anNone;
    OutlSplitter.Left := NavgPanel.Left + NavgPanel.Width + 1;
    OpenNavgLabel.Hide;
    BookHeadingLabel.Left := 4;

    case FNavgType of
      anNone : ;
      anHtml, anHtmlCombo : begin
          ComboPanel.Visible := FNavgType = anHtmlCombo;
          OutlHtmlViewer.Align := alClient;
          OutlHtmlViewer.Visible := True;
          OutlTreeViewer.Visible := False;
          OutlSplitter.Visible := True;
          OutlSplitter.Left := OutlHtmlViewer.Left + OutlHtmlViewer.Width + 1;
        end;
      anList, anListCombo, anTree : begin
          ComboPanel.Visible := FNavgType = anListCombo;
          OutlTreeViewer.Align := alClient;
          OutlTreeViewer.Visible := True;
          OutlHtmlViewer.Visible := False;
          OutlSplitter.Visible := True;
          OutlSplitter.Left := OutlTreeViewer.Left + OutlTreeViewer.Width + 1;
        end;
    end;
  end else begin
    NavgPanel.Visible := False;
    OutlSplitter.Visible := False;
    if FNavgType <> anNone then begin
      OpenNavgLabel.Visible := FNavgType <> anNone;
      OpenNavgLabel.Left := 2;
      BookHeadingLabel.Left := OpenNavgLabel.Left + OpenNavgLabel.Width + 2;
    end else begin
      OpenNavgLabel.Visible := False;
      BookHeadingLabel.Left := 4;
    end;
  end;
end;

procedure TArticleViewer.SetViewType(const AType : TArticleViewScope);
begin
  FViewType := AType;
  UpdateContents;
end;

function TArticleViewer.SelectSura(const ASura : TSuraNum) : Boolean;
begin
  if (FBook <> Nil) and (FBook.SuraArticles) then begin
    SetActivePage(FBook.TableOfContents[Pred(ASura)]);
    Result := True;
  end else
    Result := False;
end;

procedure TArticleViewer.UpdateContents;
var
  Html : String;
begin
  FBook.GetFont('default').AssignInHtmlViewer(HtmlViewer);
  FBook.GetFont('outline').AssignInHtmlViewer(OutlHtmlViewer);
  if (FViewType = avPage) and (FAmlData <> Nil) then begin
    Html := FBook.AsHtml(FAmlData, FSearchHits);
    HtmlViewer.LoadFromBuffer(PChar(Html), Length(Html));
    Subheading.Caption := FPageTitle;
    if FActiveSection <> Nil then
      HtmlViewer.PositionTo(Format('Sect1_%d', [FSections.IndexOf(FActiveSection)]));
    UpdateNavigation;
  end else if (FViewType = avSection) and (FActiveSection <> Nil) then begin
    Html := FBook.AsHtml(FActiveSection, FSearchHits);
    HtmlViewer.LoadFromBuffer(PChar(Html), Length(Html));
    Subheading.Caption := FActiveSection.GetAttribute('heading');
    UpdateNavigation;
  end else begin
    ClearContents;
  end;
  FSearchHits := '';    // clear them so later updates don't show them
end;

procedure TArticleViewer.UpdateNavigation;
var
  Html : String;

  procedure SetHeading(const ABook : String; const AArticle : String = '');
  begin
    if (ABook <> '') and (AArticle <> '') then begin
      HeadingPanel.Caption := '';
      ArticleHeadingLabel.Visible := True;
      BookHeadingLabel.Visible := True;
      ArticleHeadingLabel.Caption := AArticle;
      BookHeadingLabel.Caption := ABook;
      SubheadingPanel.Visible := (Subheading.Caption <> '') and (Subheading.Caption <> ArticleHeadingLabel.Caption);

      FPrintHeader.BookPage := AArticle;
      if SubheadingPanel.Visible then
        FPrintHeader.BookSection := ArticleHeadingLabel.Caption
      else
        FPrintHeader.BookSection := '';
    end else begin
      HeadingPanel.Caption := ABook;
      ArticleHeadingLabel.Visible := False;
      BookHeadingLabel.Visible := False;
      SubheadingPanel.Visible := (Subheading.Caption <> '') and (Subheading.Caption <> HeadingPanel.Caption);

      FPrintHeader.BookPage := ABook;
      FPrintHeader.BookSection := '';
    end;
    ArticlePrevLabel.Visible := FBook.BrowsePages;
    ArticleNextLabel.Visible := FBook.BrowsePages;
    if (not SubheadingPanel.Visible) and (FSections.Length > 0) then
      SubheadingPanel.Visible := True;
  end;

begin
  case FBook.Outline of
    osNone : SetNavgType(anNone);

    osBook : begin
        if FSections.Length > 0 then
          SetHeading(FBook.Name, FPageTitle)
        else
          SetHeading(FBook.Name);
        case FBook.OutlViewer of
          ovNone : SetNavgType(anNone);
          ovHtmlCombo : begin
              if FSections.Length > 0 then begin
                SetNavgType(anHtmlCombo);
                Html := '<p>' + SectListToHtml(FSections, FExtraLinks);
                OutlHtmlViewer.LoadFromBuffer(PChar(Html), Length(Html));
                OutlHtmlViewer.PositionTo('SELECTED_SECT');
                CntOutlineCombo.ItemIndex := FPageIdx;
              end else begin
                SetNavgType(anHtml);
                Html := ContentsToHtml(FExtraLinks);
                OutlHtmlViewer.LoadFromBuffer(PChar(Html), Length(Html));
                OutlHtmlViewer.PositionTo('SELECTED');
                OutlHtmlViewer.PositionTo('SELECTED_SECT');
              end;
            end;
          ovHtml : begin
              SetNavgType(anHtml);
              Html := ContentsToHtml(FExtraLinks);
              OutlHtmlViewer.LoadFromBuffer(PChar(Html), Length(Html));
              OutlHtmlViewer.PositionTo('SELECTED');
              OutlHtmlViewer.PositionTo('SELECTED_SECT');
            end;

          ovListCombo : begin
              if FSections.Length > 0 then begin
                SetNavgType(anListCombo);
                OutlTreeViewer.Items.BeginUpdate;
                OutlTreeViewer.Items.Clear;
                SectListToListViewer(FSections, FExtraLinks);
                OutlTreeViewer.Items.EndUpdate;
                CntOutlineCombo.ItemIndex := FPageIdx;
              end else begin
                SetNavgType(anList);
                OutlTreeViewer.Items.BeginUpdate;
                OutlTreeViewer.Items.Clear;
                ContentsToListViewer(FExtraLinks);
                OutlTreeViewer.Items.EndUpdate;
              end;
            end;

          //ovList, ovTree : OutlTreeViewer.Invalidate; // the rest is already taken care of in SetAddress
        end;
      end;

    osPage, osArticle : begin
        if FBook.TabsShortcuts.Count > 0 then
          FAddrMgr.SetQuickLinks(' ', FillerPanel.Color, FBook.TabsShortcuts, FAmlData.GetAttribute('tab'), tpNone);
        SetHeading(FPageTitle);
        case FBook.OutlViewer of
          ovHtml, ovHtmlCombo : begin
              if FSections.Length > 0 then begin
                SetNavgType(anHtml);
                Html := SectListToHtml(FSections, FExtraLinks);
                OutlHtmlViewer.LoadFromBuffer(PChar(Html), Length(Html));
                OutlHtmlViewer.PositionTo('SELECTED_SECT');
              end else
                SetNavgType(anNone);
            end;
        else
          SetNavgType(anNone);
        end;
      end;
  end;
end;

procedure TArticleViewer.ManipulateNavg(const AWhat : TArticleNavgManipulation);
begin
  case AWhat of
    nmHide :
      begin
        FNavgHiddenByUser := True;
        SetNavgType(FNavgType);
      end;
    nmMinMax :
      begin
        if NavgPanel.Width > NavgPanelDefWidth then
          NavgPanel.Width := NavgPanelDefWidth
        else
          NavgPanel.Width := Self.Width div 2;
      end;
    nmShow   :
      begin
        FNavgHiddenByUser := False;
        SetNavgType(FNavgType);
      end;
  end;
end;

function TArticleViewer.GetActiveData : TAmlData;
begin
  Result := FBook;
end;

procedure TArticleViewer.InternalHtmlHotSpotClick(Sender: TObject; const APrimarySrc, ASecondarySrc : String);
begin
  if (Sender = OutlHtmlViewer) then begin
    if (APrimarySrc <> '') and (ASecondarySrc <> '') then begin
      Assert(APrimarySrc = FPageTitle);
      SetActiveSection(ASecondarySrc);
    end else if (APrimarySrc <> '') then
      SetActivePage(APrimarySrc);
  end;
end;

procedure TArticleViewer.ShortcutsListClick(Sender: TObject);
var
  Shortcut : TBookmarkNode;
begin
  if (Sender as TListView).Selected <> Nil then begin
    Shortcut := TBookmarkNode((Sender as TListView).Selected.Data);
    SetActivePage(Shortcut.Caption);
  end;
end;

procedure TArticleViewer.OutlSplitterMaximize(Sender: TObject);
begin
(*
  case FNavgType of
    anNone : ;
    anHtml, anHtmlCombo : OutlHtmlViewer.Visible := OutlSplitter.Maximized;
    anList, anListCombo : OutlListViewer.Visible := OutlSplitter.Maximized;
  end;
*)
end;

procedure TArticleViewer.CntOutlineComboChange(Sender: TObject);
begin
  Assert(FBook <> Nil);
  if (CntOutlineCombo.ItemIndex >= 0) and (CntOutlineCombo.ItemIndex <> FPageIdx) then
    SetActivePage(FBook.TableOfContents[CntOutlineCombo.ItemIndex]);
end;

procedure TArticleViewer.OutlListViewerResize(Sender: TObject);
begin
  //OutlListViewer.Columns[0].Width := OutlListViewer.Width - GetSystemMetrics(SM_CXVSCROLL) - 4;
end;

procedure TArticleViewer.OutlTreeViewerClick(Sender: TObject);
var
  Item : TTreeNode;
begin
  Item := OutlTreeViewer.Selected;
  if Item <> Nil then begin
    if Item.Data <> Nil then begin
      with TTreeNodeData(Item.Data) do begin
        SetPageIndex(PageIdx);
        SetActiveSection(FSections.Item(SectIdx) as TXmlElement);
      end;
      //if TObject(Item.Data) is TXmlElement then
      //  SetActiveSection(TXmlElement(Item.Data));
      //if TObject(Item.Data) is TCaptionedAddress then
      //  GlobalSetAddress(TCaptionedAddress(Item.Data).Address);
    end else
      SetActivePage(Item.Text);
  end;
end;

procedure TArticleViewer.ArticlePrevLabelClick(Sender: TObject);
begin
  Navigate(nvPage, nvdPrevious);
end;

procedure TArticleViewer.ArticleNextLabelClick(Sender: TObject);
begin
  Navigate(nvPage, nvdNext);
end;

procedure TArticleViewer.SectionSpinBtnUpClick(Sender: TObject);
begin
  Navigate(nvItem, nvdPrevious);
end;

procedure TArticleViewer.SectionSpinBtnDownClick(Sender: TObject);
begin
  Navigate(nvItem, nvdNext);
end;

const
  ACTIVE_ITEM_TAG = 0;

procedure TArticleViewer.SectMenuGetItemParams(Sender: TMenu;
  Item: TMenuItem; State: TMenuOwnerDrawState; AFont: TFont;
  var Color: TColor; var Graphic: TGraphic; var NumGlyphs: Integer);
var
  Bookmark : TBookmarkNode;
begin
  AFont.Name := 'Tahoma';
  AFont.Size := 8;
  if Item.Tag = ACTIVE_ITEM_TAG then begin
    AFont.Style := AFont.Style + [fsBold];
    if State = [] then
      AFont.Color := clNavy;
  end else begin
    Bookmark := TBookmarkNode(Item.Tag);
    // a menu "header" that is itself a section marker has a breakafter
    if Bookmark.BreakAfter then begin
      //if (State = []) then
      //  AFont.Color := clGray;
      AFont.Style := AFont.Style + [fsBold];
    end;
  end;
end;

procedure TArticleViewer.SectMenuPopup(Sender: TObject);
var
  ActiveNode : TBookmarkNode;
begin
  if FContextMenuBmks <> Nil then
    FContextMenuBmks.Clear
  else
    FContextMenuBmks := TBookmarks.Create;
  ActiveNode := SectListToBookmarks(FContextMenuBmks, FSections, '', FActiveSection);
  ClearMenu(SectMenu);
  if FContextMenuBmks.Count > 0 then
    FContextMenuBmks.Populate(SectMenu.Items, SectMenuClick, Nil, -1, -1, '', ActiveNode, ACTIVE_ITEM_TAG);
end;

procedure TArticleViewer.SectMenuClick(Sender : TObject);
var
  Bookmark : TBookmarkNode;
  MainSect, SubSect : String;
  P : Integer;
begin
  if Sender is TMenuItem then begin
    if (Sender as TMenuItem).Tag <> ACTIVE_ITEM_TAG then begin
      Bookmark := TBookmarkNode((Sender as TMenuItem).Tag);
      if Bookmark.Children.Count <= 0 then begin
        P := Pos(SubSectDelim, Bookmark.Address);
        if P > 0 then begin
          MainSect := Copy(Bookmark.Address, 1, Pred(P));
          SubSect := Copy(Bookmark.Address, Succ(P), Length(Bookmark.Address));
          SetActiveSection(FSections.Item(StrToInt(MainSect)) as TXmlElement);
          if FViewType = avSection then
            HtmlViewer.PositionTo('Sect1_'+SubSect);
        end else begin
          SetActiveSection(FSections.Item(StrToInt(Bookmark.Address)) as TXmlElement);
        end;
      end;
    end;
  end;
end;

function TArticleViewer.GetEnableCopy : Boolean;
begin
  Result := FBook.FlagIsSet(adfCanCopyText);
end;

function TArticleViewer.GetEnablePrint : Boolean;
begin
  Result := FBook.FlagIsSet(adfCanPrintText);
end;

procedure TArticleViewer.Print(const ADialog : TPrintDialog);
begin
  if not ADialog.Execute then
    Exit;

  if ADialog.PrintRange = prAllPages then
    HtmlViewer.Print(1, 9999)
  else
    HtmlViewer.Print(ADialog.FromPage, ADialog.ToPage);
end;

procedure TArticleViewer.CopyToClipboard;
begin
  if HtmlViewer.SelLength > 0 then
    HtmlViewer.CopyToClipboard
  else if OutlHtmlViewer.SelLength > 0 then
    OutlHtmlViewer.CopyToClipboard
  else begin
    HtmlViewer.SelectAll;
    HtmlViewer.CopyToClipboard;
    HtmlViewer.SelLength := 0;
  end;
end;

procedure TArticleViewer.FillerPanelDblClick(Sender: TObject);
begin
  ManipulateNavg(nmMinMax);
end;

procedure TArticleViewer.NavgCloseLabelClick(Sender: TObject);
begin
  ManipulateNavg(nmHide);
end;

procedure TArticleViewer.OpenNavgLabelClick(Sender: TObject);
begin
  ManipulateNavg(nmShow);
end;

end.
