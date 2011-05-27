unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ActiveX,
  Dialogs, ImgList, ExtCtrls, Menus, CompDoc, IslUtils,
  Htmlview, XmlObjModel, DFSSplitter, CSFrmPnl, AmlView,
  RXCtrls, islctrls, ComCtrls, RxMenus, QuranStruct, IndexStruct,
  HtmlSubs, ToolWin, SearchView, ReciteView, WebView, HomeView,
  StdCtrls, RXSpin, ActnList, MultiIndexView, CSTCBase, CSTC32,
  StdActns, sEdits, BookmarksView, Arabic, moneyctrls, HtmlUn2,
  ConcordView, StRegIni, MediaPlayerWin, AppEvent, ShopCart,
  StStrL, AuthenticForm, MessageWin, ALSTDlg, Ftpcli, TextView,
  ShellAPI;

const
  DefaultNavgWidth = 165;
  MinNavgWidth = 90;
  MinQuranToolsWidth = 125;
  MinAlimToolsWidth = 125;

type
  TUpdateReceive = (urNone, urMasterFile, urBook);
  TMainForm = class(TForm, IAddressManager, IStatusDisplay)
    StatusBar: TStatusBar;
    AppMenus: TMainMenu;
    MainActionList: TActionList;
    ActionQuranSelectSura: TAction;
    ActionQuranRecite: TAction;
    ActionAppSearch: TAction;
    MenuAlim: TMenuItem;
    MenuItemAlimExit: TMenuItem;
    ActionAppHistoryBack: TAction;
    ActionAppHistoryForward: TAction;
    ActionAppBookmarks: TAction;
    ImagesSmall: TImageList;
    N1: TMenuItem;
    ActionEditCopy: TEditCopy;
    SearchTheAlim1: TMenuItem;
    ActionAppIndexes: TAction;
    OpenaQuranSuraorAyah1: TMenuItem;
    RecitetheQuran1: TMenuItem;
    N2: TMenuItem;
    Indexes1: TMenuItem;
    N3: TMenuItem;
    Home1: TMenuItem;
    ActionAppHome: TAction;
    MainPanel: TPanel;
    NavgSplitter: TSplitter;
    NavgPanel: TPanel;
    NavgTitlePanel: TWallpaperPanel;
    ActiveNavigatorPanel: TcsFormPanel;
    ContentsPanel: TPanel;
    ActiveViewerPanel: TcsFormPanel;
    QuickLinksPanel: TPanel;
    ShortcutsBar: TMoneyShortcutBar;
    QuickLinksHeaderPanel: TWallpaperPanel;
    QuranToolsPanel: TWallpaperPanel;
    QuranToolsPanelMain: TWallpaperPanel;
    QuranToolsPanelBorder: TShape;
    QuranToolsBevel1: TBevel;
    QuranToolsBevel2: TBevel;
    Label1: TLabel;
    QuranToolViewIntroLabel: TMoneyLabel;
    QuranToolAraSchLabel: TMoneyLabel;
    QuranToolCustomizeLabel: TMoneyLabel;
    QuranToolNextSuraLabel: TMoneyLabel;
    QuranToolPrevSuraLabel: TMoneyLabel;
    QuranToolChooseSuraLabel: TMoneyLabel;
    QuranToolReciteLabel: TMoneyLabel;
    GotoSuraAyahEdit: TVolElemEdit;
    ViewBookmarks1: TMenuItem;
    NavgCloseLabel: TMoneyLabel;
    NavgExpandLabel: TMoneyLabel;
    MainToolsPanel: TWallpaperPanel;
    RxSpeedButton5: TRxSpeedButton;
    RxSpeedButton6: TRxSpeedButton;
    RxSpeedButton7: TRxSpeedButton;
    BtnHistoryBack: TRxSpeedButton;
    RxSpeedButton9: TRxSpeedButton;
    ActionAppConcordances: TAction;
    ViewConcordances1: TMenuItem;
    FontDialog: TFontDialog;
    N4: TMenuItem;
    ActionAppPrint: TAction;
    ActionAppPrint1: TMenuItem;
    PageSetup1: TMenuItem;
    PrinterSetupDialog: TPrinterSetupDialog;
    PrintDialog: TPrintDialog;
    EditMenu: TMenuItem;
    Copy1: TMenuItem;
    XmlParser: TXmlObjModel;
    AppEvents: TAppEvents;
    ActionAppFAQ: TAction;
    CopyrightsAndFAQ: TMenuItem;
    AlimViewMessages: TMenuItem;
    AppTips: TALSTipDlg;
    AlimViewTips: TMenuItem;
    FTPClient: TFtpClient;
    FTPUpdateParser: TXmlObjModel;
    AlimDownloadNews: TMenuItem;
    ViewShoppingCart: TMenuItem;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AlimMenuViewInstalledBooksClick(Sender: TObject);
    procedure AlimMenuToolBtnClick(Sender: TObject);
    procedure ActionQuranSelectSuraExecute(Sender: TObject);
    procedure ActionQuranReciteExecute(Sender: TObject);
    procedure ActionAppSearchExecute(Sender: TObject);
    procedure NavgCloseLabelClick(Sender: TObject);
    procedure ActionAppBookmarksExecute(Sender: TObject);
    procedure ActionAppIndexesExecute(Sender: TObject);
    procedure GotoSuraAyahEditEnterKey(Sender: TObject);
    procedure ActionAppHomeExecute(Sender: TObject);
    procedure ActionAppCrossRefsExecute(Sender: TObject);
    procedure ShortcutsBarClick(Sender: TObject);
    procedure QuranToolLabelClick(Sender: TObject);
    procedure MenuItemAlimExitClick(Sender: TObject);
    procedure NavgExpandLabelClick(Sender: TObject);
    procedure ActionAppConcordancesExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure PageSetup1Click(Sender: TObject);
    procedure ActionAppPrintUpdate(Sender: TObject);
    procedure ActionEditCopyUpdate(Sender: TObject);
    procedure ActionEditCopyExecute(Sender: TObject);
    procedure ActionAppPrintExecute(Sender: TObject);
    procedure AppEventsIdle(Sender: TObject; var Done: Boolean);
    procedure ActionAppFAQExecute(Sender: TObject);
    procedure AlimViewMessagesClick(Sender: TObject);
    procedure AlimViewTipsClick(Sender: TObject);
    procedure FTPClientRequestDone(Sender: TObject; RqType: TFtpRequest;
      Error: Word);
    procedure AlimDownloadNewsClick(Sender: TObject);
    procedure ViewShoppingCartClick(Sender: TObject);
  private
    FHome : THomeViewer;         { the home viewer }
    FWebBrowser : TWebViewer;    { the web viewer }
    FShortcuts : TBookmarks;     { the menus, and other lists }
    FBookmarks : TBookmarks;     { the actual bookmarks }
    FHomeShortcuts : TBookmarks; { the shortcuts on the home screen (not tools) }
    FIndexes : TBookmarks;       { bookmarks for indexes }
    FBookPaths : String;         { the paths to search }
    FAppDataPath : String;       { the path of the run-time application data}
    FDataMgr : TDataManager;     { all of the books/data }
    FActiveQuickLinks : TBookmarks; { not owned by this object }
    FSearchViewer : TSearchViewer;
    FConcordViewer : TConcordanceViewer;
    FMultiIndexViewer : TMultiIndexViewer;
    FBookmarksViewer : TBookmarksViewer;
    FMediaPlayerWin : TMediaPlayerForm;
    FAraEncodings : TArabicEncodings;
    FQuranStruct : TQuranStructure;
    FRecite : TRecitationViewer; { the recitation window }
    FInHistoryMove : Boolean;    { are we setting an address inside a history method? }
    FHistory : TCaptionedAddressList;    { the history list }
    FCurIdxInHistory : Cardinal; { current address's place in the history }
    FMenuBookmarks : TMenuItem;
    FMenuIndexes : TMenuItem;
    FRegistry : TStRegIni;
    FErrorClose : Boolean;
    FSaveStatusCursor : TCursor;
    FRunCount : Cardinal;
    FUpdateMasterFName : String;
    FUpdateReceive : TUpdateReceive;
    FUpdateData : TXmlElement;
    FForceNewsShow : Boolean;

    function VerifyPrerequisites : Boolean;

    procedure ReadDataFiles(const APaths : String);
    procedure ReadShortcuts;
    procedure ReadBookmarks;
    procedure ReadArabicEncodings;
    procedure ReadQuranStruct;

    procedure PopulateAppMenus;

    procedure HistoryAdd(const ACaption, AAddress : String);
    procedure HistoryMove(const CA : TCaptionedAddress);
    procedure HistoryPopulate;
    procedure HistoryMenuItemClick(Sender: TObject);

    procedure OnBookmarksAddr(const AAddress : TAddress); virtual;
    procedure OnHomeAddr(const AAddress : TAddress); virtual;
    procedure OnIndexAddr(const AAddress : TAddress); virtual;
    procedure OnQuranSuraAddr(const AAddress : TAddress); virtual;
    procedure OnQuranAyahAddr(const AAddress : TAddress); virtual;
    procedure OnReciteQuranAddr(const AAddress : TAddress); virtual;
    procedure OnSearchAddr(const AAddress : TAddress); virtual;
    procedure OnCrossRefAddr(const AAddress : TAddress); virtual;
    procedure OnConcordAddr(const AAddress : TAddress); virtual;
    procedure OnMediaPlayerAddr(const AAddress : TAddress); virtual;
    procedure OnCartAddr(const AAddress : TAddress); virtual;
    procedure OnCartSendAddr(const AAddress : TAddress); virtual;
    procedure OnWebAddr(const AAddress : String); virtual;

    procedure SetActiveNavigator(const ANavigator : TForm); virtual;
    procedure SetActiveViewer(const AViewer : TForm); virtual;
  public
    function AddShortcut(const ALibrary, AAddress, ACaption, ASortKey : String) : TBookmarkNode;
    procedure SetQuickLinks(const AHeader : String; const AColor : TColor;
                            const ALinks : TBookmarks; const AActiveCaption : String;
                            const AShowTools : TToolsPanels);

    function AddBook(const AFileName : String) : TAmlData; overload;
    function AddBook(const AFileName : String; const ACatalog : TXmlElement) : TAmlData; overload;
    function AddSpecialBook(const AName : String) : TAmlData;
    procedure BookmarkNodeMenuItemClick(Sender : TObject);

    procedure SetAddress(const AAddress : TAddress); overload;
    procedure SetAddress(const AAddress : String); overload;
    function GetAddressCaption(const AAddress : TAddress; const AIncludeName : Boolean) : String;

    procedure SetActiveSuraAyah(const ALoc : TSuraAyah);
    procedure ShowBookmarks(const ABookmarks : TBookmarks; const ATitle : String; const ABecomeOwner : Boolean);

    function GetDirectoryPath(const APathId : String; var AStyle : TDirPathStyle) : String; overload;
    function GetDirectoryPath(const APathId : String) : String; overload;
    function ForceIntoPath(const APathId, AFileOrRelPath : String) : String;

    procedure StatusBegin(const AMessage : String; AWaitCursor : Boolean; AMax : LongInt = 0);
    procedure StatusUpdate(const AProgress : LongInt);
    procedure StatusEnd;

    procedure NewsReceiveStart;
    procedure NewsReceiveShow;

    property DataMgr : TDataManager read FDataMgr;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

uses StStrS, BooksView, MultiQuranView, QuranView, QuranImgView, SuraWin,
     ArticleView, HadithView, Math, StBits, RemindersDlg;

const
  imgIdxHistoryPrev = 5;
  imdIdxHistroyNext = 6;
  imgIdxPagePrev = 7;
  imgIdxPageNext = 8;
  imgIdxSectPrev = 9;
  imgIdxSectNext = 10;

resourcestring
  rsErrRegistryKeyMissing = 'Registry entry %s in %s is missing. Please reinstall the application.';
  rsErrMultiplePathFoundWhereSingleExpected  = 'Registry entry %s in %s is missing. Please reinstall the application.';

  // application data file constants
  adAppTipsFile = 'Alim.tip';
  adArabicEncodingDataFile = 'Arabic.aml';
  adQuranStructDataFile = 'Quran Structure.aml';
  adHomeDataFile = 'Home.aml';

  // application cache (runtime) data file constants
  cadDataCatalogCacheFile = 'AlimData.ccf';
  cadQuranStructCacheFile = 'QStruct.qsc';

  PrimaryRegistryKey = 'SOFTWARE\ISL Software Corporation\The Alim';
  MessagesRegistryKey = 'SOFTWARE\ISL Software Corporation\The Alim\Messages';
  PathsRegistryKey = 'SOFTWARE\ISL Software Corporation\The Alim\Paths';

procedure FitToolsInBar(const AToolbar : TToolBar);
var
  BtnIdx, MaxWidth : Cardinal;
begin
  MaxWidth := 0;
  for BtnIdx := 0 to AToolbar.ButtonCount-1 do begin
    Inc(MaxWidth, AToolbar.Buttons[BtnIdx].Width);
  end;
  AToolbar.Width := MaxWidth;
end;

function TMainForm.AddShortcut(const ALibrary, AAddress, ACaption, ASortKey : String) : TBookmarkNode;
begin
  Result := FShortcuts.AddChild(ALibrary, AAddress, ACaption, ASortKey);
end;

procedure TMainForm.SetQuickLinks(const AHeader : String; const AColor : TColor;
                                  const ALinks : TBookmarks; const AActiveCaption : String;
                                  const AShowTools : TToolsPanels);
var
  I, MaxCaptionWidth, RealWidth : Integer;
  Shortcut : TBookmarkNode;
begin
  QuickLinksHeaderPanel.Visible := AHeader <> '';
  QuickLinksHeaderPanel.Caption := AHeader;
  QuickLinksHeaderPanel.Color := AColor;

  QuranToolsPanel.Visible := AShowTools = tpQuran;
  QuickLinksPanel.Visible := (AShowTools <> tpNone) or ((ALinks <> Nil) and (ALinks.Count > 0));

  ShortcutsBar.Tabs.Clear;
  FActiveQuickLinks := ALinks;
  if ALinks <> Nil then begin
    if ALinks.Count > 0 then begin
      MaxCaptionWidth := 0;
      for I := 0 to ALinks.Count-1 do begin
        Shortcut := ALinks[I];
        MaxCaptionWidth := Max(MaxCaptionWidth, Canvas.TextExtent(Shortcut.Caption).cx);
        ShortcutsBar.Tabs.AddObject(Shortcut.Caption, Shortcut);
      end;
      RealWidth := MaxCaptionWidth + ShortcutsBar.LeftOffset + ShortcutsBar.RightOffset + 15;
    end else
      RealWidth := 0;

    case AShowTools of
      tpNone : QuickLinksPanel.Width := RealWidth;
      tpQuran : QuickLinksPanel.Width := Max(RealWidth, MinQuranToolsWidth);
    end;

    if ALinks.Count > 0 then
      ShortcutsBar.TabIndex := ShortcutsBar.Tabs.IndexOf(AActiveCaption);
  end;
end;

function TMainForm.AddBook(const AFileName : String; const ACatalog : TXmlElement) : TAmlData;
var
  CatalogContent : TXmlElement;
  ContentType, ContentSubtype : String;
begin
  CatalogContent := ACatalog.FindElement('content');
  Result := Nil;

  if CatalogContent <> Nil then begin
    ContentType := CatalogContent.GetAttribute('type');
    ContentSubtype := CatalogContent.GetAttribute('subtype');

    case FindString(ContentType, ['quran', 'index', 'article', 'hadith', 'fiqh']) of
     -1 : {not found};
      0 : begin
            case FindString(ContentSubType, ['text', 'images', 'themes']) of
             -1 : {not found};
              0 : Result := TQuranData.Create(FDataMgr);
              1 : Result := TQuranImgData.Create(FDataMgr);
              2 : Result := TQuranThemesData.Create(FDataMgr);
            end;
          end;
      1 : Result := TIndexData.Create(FDataMgr);
      2 : Result := TArticleData.Create(FDataMgr);
      3 : Result := THadithFiqhData.Create(FDataMgr);
      4 : Result := THadithFiqhData.Create(FDataMgr);
    end;
  end;

  if Result <> Nil then begin
    Result.XmlParser := Self.XmlParser;
    Result.AddressMgr := Self;
    Result.StatusDisplay := Self;
    Result.AraEncodings := FAraEncodings;
    Result.QuranStruct := FQuranStruct;
    if Result.InitializeLoad(AFileName, ACatalog) then begin
      FDataMgr.AddData(Result);
      if Result.Links[alExternal] <> Nil then
        FShortcuts.Merge(Result.Links[alExternal]);
    end else begin
      ACatalog.Free;
      Result := Nil;
    end;
  end else
    ShowMessage(Format('No viewer found for %s (%s)', [AFileName, ContentType]));
end;

function TMainForm.AddBook(const AFileName : String) : TAmlData;
var
  RootStorage : TRootStorage;
  DataStream : TStorageStream;
  SourceXml, CatalogXml : String;
  Catalog : TXmlElement;
begin
  Result := FDataMgr.DataFiles[AFileName];
  if Result <> Nil then
    Exit;

  try
    RootStorage := FDataMgr.OpenRootStorage(AFileName);
    try
      DataStream := TStorageStream.Create('Catalog', RootStorage, amRead, False);
      SetLength(SourceXml, DataStream.Size);
      DataStream.Read(PChar(SourceXml)^, DataStream.Size);
      DataStream.Free;
      if AmlDataEncryptKey <> '' then
        CatalogXml := ScrambleL(SourceXml, AmlDataEncryptKey)
      else
        CatalogXml := SourceXml;

      XmlParser.LoadMemory(PChar(CatalogXml));
      Catalog := XmlParser.Document.DocumentElement.CloneNode as TXmlElement;
      XmlParser.ClearDocument;
    except
      FDataMgr.CloseRootStorage(AFileName);
      ShowMessage(Format('No catalog found in %s.', [AFileName]));
      Exit;
    end;
  except
    ShowMessage(Format('Unable to open %s.', [AFileName]));
    Exit;
  end;

  Catalog.SetAttribute('filename', AFileName);
  Catalog.SetAttribute('timestamp', IntToStr(FileAge(AFileName)));

  Result := AddBook(AFileName, Catalog);
  FDataMgr.CloseRootStorage(AFileName);
end;

function TMainForm.AddSpecialBook(const AName : String) : TAmlData;
begin
  if AName <> '_quran_multi' then
    raise Exception.CreateFmt('Unknown special book %s', [AName]);

  Result := TMultiQuranData.Create(FDataMgr);
  Result.XmlParser := Self.XmlParser;
  Result.AddressMgr := Self;
  //Result.DataMgr := FDataMgr;
  Result.StatusDisplay := Self;
  if Result.InitializeLoad(AName, Nil) then
    FDataMgr.AddData(Result);
end;

procedure TMainForm.BookmarkNodeMenuItemClick(Sender : TObject);
var
  Item : TMenuItem;
  Bookmark : TBookmarkNode;
begin
  Item := Sender as TMenuItem;
  if Item.Tag <> 0 then begin
    Bookmark := TBookmarkNode(Item.Tag);
    if Bookmark.Address <> '' then
      SetAddress(Bookmark.Address);
  end;
end;

procedure TMainForm.PopulateAppMenus;
const
  AccelsUsed = 'aix';  // the "Alim", "Bookmarks", and "Indexes" menus
var
  I : Integer;
  IndexName : String;
  Node : TBookmarkNode;
begin
  if FShortcuts.Count > 0 then begin
    FShortcuts.Populate(AppMenus.Items, BookmarkNodeMenuItemClick,
                        ImagesSmall, -1, //DefaultBookIconIdxSmallCyan,
                        DefaultBookIconIdxSmallCyan, AccelsUsed);
  end;

  if FBookmarks <> Nil then begin
    if FMenuBookmarks = Nil then begin
      FMenuBookmarks := TMenuItem.Create(Self);
      FMenuBookmarks.Caption := '&Internet';
      AppMenus.Items.Add(FMenuBookmarks);
    end else
      while FMenuBookmarks.Count > 0 do FMenuBookmarks[0].Free;

    FBookmarks.Populate(FMenuBookmarks, BookmarkNodeMenuItemClick,
                        ImagesSmall, DefaultBmkCatgIdxSmall, DefaultBmkItemIdxSmall);
  end;

  if FDataMgr.PageIndexesDict.Count > 0 then begin
    if FMenuIndexes = Nil then begin
      FMenuIndexes := TMenuItem.Create(Self);
      FMenuIndexes.Caption := 'Inde&xes';
      AppMenus.Items.Add(FMenuIndexes);
    end else
      while FMenuIndexes.Count > 0 do FMenuIndexes[0].Free;

    for I := 0 to FDataMgr.PageIndexesDict.Count-1 do begin
      IndexName := FDataMgr.PageIndexesDict[I];
      Node := FIndexes.AddChild('', Format('index:%s', [IndexName]), IndexName, IndexName);
      Node.SmallIconIdx := DefaultIndexIconIdxSmall;
      Node.Shortcut := 'Ctrl+'+IntToStr(Succ(I));
    end;
    FIndexes.Populate(FMenuIndexes, BookmarkNodeMenuItemClick,
                      ImagesSmall, DefaultBmkCatgIdxSmall, DefaultBmkItemIdxSmall);
  end;
end;

procedure TMainForm.HistoryAdd(const ACaption, AAddress : String);
var
  I : Integer;
begin
  if FInHistoryMove then
    Exit;

  // if we have any "forward" items, then clear them because we can't track them
  if FCurIdxInHistory > 0 then begin
    for I := 0 to FCurIdxInHistory-1 do begin
      TObject(FHistory[0]).Free;
      FHistory.Delete(0);
    end;
    FCurIdxInHistory := 0;
  end;

  FHistory.PrependAddr(ACaption, AAddress);
  HistoryPopulate;
end;

procedure TMainForm.HistoryMove(const CA : TCaptionedAddress);
begin
  FInHistoryMove := True;

  FCurIdxInHistory := FHistory.IndexOf(CA);
  HistoryPopulate;

  SetAddress(CA.Address);
  FInHistoryMove := False;
end;

procedure TMainForm.HistoryPopulate;
begin
  if FHistory.Count = 0 then begin
    BtnHistoryBack.Enabled := False;
    //ToolBtnHistoryNext.Enabled := False;
    Exit;
  end;

  //ClearMenu(HistoryMenuBack);
  //ClearMenu(HistoryMenuForward);
  (*

  if Cardinal(FHistory.Count) > FCurIdxInHistory then begin
    FirstBack := True;
    for I := Succ(FCurIdxInHistory) to FHistory.Count-1 do begin
      Addr := FHistory.Addresses[I];
      if FirstBack then begin
        ToolBtnHistoryPrev.Tag := LongInt(Addr);
        ToolBtnHistoryPrev.OnClick := HistoryMenuItemClick;
        FirstBack := False;
      end;
      MI := TMenuItem.Create(Application);
      MI.Caption := Addr.Caption;
      MI.Tag := LongInt(Addr);
      MI.OnClick := HistoryMenuItemClick;
      HistoryMenuBack.Items.Add(MI);
    end;
  end;

  if FCurIdxInHistory > 0 then begin
    FirstForward := True;
    for I := Pred(FCurIdxInHistory) downto 0 do begin
      Addr := FHistory.Addresses[I];
      if FirstForward then begin
        ToolBtnHistoryNext.Tag := LongInt(Addr);
        ToolBtnHistoryNext.OnClick := HistoryMenuItemClick;
        FirstForward := False;
      end;
      MI := TMenuItem.Create(Application);
      MI.Caption := Addr.Caption;
      MI.Tag := LongInt(Addr);
      MI.OnClick := HistoryMenuItemClick;
      HistoryMenuForward.Items.Add(MI);
    end;
  end;

  ToolBtnHistoryPrev.DropdownMenu := HistoryMenuBack;
  ToolBtnHistoryNext.DropdownMenu := HistoryMenuForward;
  ToolBtnHistoryNext.Enabled := HistoryMenuForward.Items.Count > 0;
  *)
  if Cardinal(FHistory.Count) > FCurIdxInHistory then begin
    BtnHistoryBack.Enabled := True;
    if Succ(FCurIdxInHistory) < Cardinal(FHistory.Count) then
      BtnHistoryBack.Tag := LongInt(FHistory.Addresses[Succ(FCurIdxInHistory)]);
    BtnHistoryBack.OnClick := HistoryMenuItemClick;
  end else
    BtnHistoryBack.Enabled := False;
end;

procedure TMainForm.HistoryMenuItemClick(Sender: TObject);
begin
  if Sender is TMenuItem then
    HistoryMove(TCaptionedAddress((Sender as TMenuItem).Tag))
  else
    HistoryMove(TCaptionedAddress((Sender as TRxSpeedButton).Tag));
end;

procedure TMainForm.SetActiveNavigator(const ANavigator : TForm);
begin
  if ANavigator = Nil then begin
    NavgSplitter.Hide;
    NavgPanel.Hide;
  end else begin
    NavgTitlePanel.Caption := ANavigator.Caption;
    ActiveNavigatorPanel.Form := ANavigator;
    ActiveNavigatorPanel.Form.SetFocus;
    if NavgPanel.Width < MinNavgWidth then
      NavgPanel.Width := DefaultNavgWidth;

    NavgPanel.Show;
    NavgSplitter.Show;
    NavgSplitter.Left := NavgPanel.Left + NavgPanel.Width + 1;
  end;
end;

procedure TMainForm.SetActiveViewer(const AViewer : TForm);
begin
  ActiveViewerPanel.Form := AViewer;
  ActiveViewerPanel.Form.SetFocus;
end;

procedure TMainForm.ShowBookmarks(const ABookmarks : TBookmarks; const ATitle : String; const ABecomeOwner : Boolean);
begin
  if FBookmarksViewer = Nil then begin
    FBookmarksViewer := TBookmarksViewer.Create(Self);
    FBookmarksViewer.AddressMgr := Self;
    FBookmarksViewer.Status := Self;
  end;
  FBookmarksViewer.Caption := ATitle;
  FBookmarksViewer.Bookmarks := ABookmarks;
  FBookmarksViewer.OwnTheData := ABecomeOwner;
  SetActiveNavigator(FBookmarksViewer);
end;

procedure TMainForm.OnBookmarksAddr(const AAddress : TAddress);
begin
  ShowBookmarks(FBookmarks, 'Alim Bookmarks', False);
end;

procedure TMainForm.OnHomeAddr(const AAddress : TAddress);
begin
  FHome.Address := AAddress;
  HistoryAdd('Alim Home', AAddress.Address);
  SetActiveViewer(FHome);
end;

procedure TMainForm.OnIndexAddr(const AAddress : TAddress);
begin
  if FMultiIndexViewer = Nil then begin
    FMultiIndexViewer := TMultiIndexViewer.Create(Self);
    FMultiIndexViewer.AddressMgr := Self;
    FMultiIndexViewer.Status := Self;
    FMultiIndexViewer.DataMgr := FDataMgr;
  end;
  SetActiveNavigator(FMultiIndexViewer);
  FMultiIndexViewer.ActiveIndexName := AAddress.BookPage;
  if AAddress.Path <> '' then
    FMultiIndexViewer.Entry := AAddress.Path;
end;

procedure TMainForm.OnQuranSuraAddr(const AAddress : TAddress);
var
  ActiveViewer : TAmlViewer;
  Sura : TSuraNum;
begin
  if AAddress.BookPage = '' then begin
    SuraListForm.Populate(FQuranStruct);
    SuraListForm.Show;
    Exit;
  end;

  Sura := StrToInt(AAddress.BookPage);
  if ActiveViewerPanel.Form is TAmlViewer then begin
    ActiveViewer := ActiveViewerPanel.Form as TAmlViewer;
    if not ActiveViewer.SelectSura(Sura) then
      SetAddress(CreateQuranAddress(qatGeneric, Sura));
  end else
    SetAddress(CreateQuranAddress(qatGeneric, Sura));
end;

procedure TMainForm.OnQuranAyahAddr(const AAddress : TAddress);
var
  ActiveViewer, NewViewer : TAmlViewer;
  NewAddr : TAddress;
  AmlData : TAmlData;
begin
  if AAddress.BookPage = '' then begin
    SuraListForm.Populate(FQuranStruct);
    SuraListForm.Show;
    Exit;
  end;

  if ActiveViewerPanel.Form is TAmlViewer then
    ActiveViewer := ActiveViewerPanel.Form as TAmlViewer
  else
    ActiveViewer := Nil;
  NewAddr := TAddress.Create;
  NewAddr.Address := AAddress.Address;

  // if a quran viewer is already open (any type), show the ayah in it
  // if no quran viewer of any type is open, show the ayah in a MultiQuranViewer
  if ActiveViewer is TQuranViewer then
    AmlData := (ActiveViewer as TQuranViewer).Book
  else if ActiveViewer is TMultiQuranViewer then
    AmlData := (ActiveViewer as TMultiQuranViewer).Book
  else if ActiveViewer is TQuranImgViewer then
    AmlData := (ActiveViewer as TQuranImgViewer).Book
  else
    AmlData := FDataMgr.DataIds[MultiQuranId];

  if AmlData <> Nil then begin
    NewAddr.BookId := AmlData.Id;
    HistoryAdd(GetAddressCaption(NewAddr, True), NewAddr.Address);
    NewViewer := AmlData.GetViewer(vsDefault);
    SetActiveViewer(NewViewer);
    NewViewer.Address := NewAddr;
  end;
  NewAddr.Free;
end;

procedure TMainForm.OnReciteQuranAddr(const AAddress : TAddress);
var
  Sura : TSuraNum;
  Ayah : TAyahNum;
begin
  if FRecite = Nil then begin
    FRecite := TRecitationViewer.Create(Application);
    FRecite.Constraints.MinWidth := 165;
    FRecite.Constraints.MaxWidth := 165;
    FRecite.AddressMgr := Self;
    FRecite.DataMgr := FDataMgr;
    FRecite.QuranStruct := FQuranStruct;

    FRegistry.CurSubKey := PrimaryRegistryKey;
    FRecite.ShowDebugPanel := FRegistry.ReadBoolean('ShowReciteDebugPanel', False);
    FRecite.SelStartEndSetCount := FRegistry.ReadInteger('SelStartEndSetCount', DefaultSelStartEndSetCount);
  end;
  SetActiveNavigator(FRecite);
  if AAddress.BookPage <> '' then begin
    try
      Sura := StrToInt(AAddress.Volume);
      Ayah := StrToInt(AAddress.Element);
      if AAddress.Path = '+' then begin
        FRecite.Sura := Sura;
        FRecite.Ayah := Ayah;
        FRecite.ReciteContinuous;
      end else
        FRecite.ReciteRange(Sura, Ayah, Ayah);
    except
    end;
  end;
end;

procedure TMainForm.OnSearchAddr(const AAddress : TAddress);
begin
  if FSearchViewer = Nil then begin
    FSearchViewer := TSearchViewer.Create(Self);
    FSearchViewer.AddressMgr := Self;
    FSearchViewer.StatusMgr := Self;
    FSearchViewer.DataMgr := FDataMgr;
  end;
  FSearchViewer.Caption := 'Search The Alim';
  FSearchViewer.IndexNames := SpecialIdxFullText;
  FSearchViewer.InverseAddr := False;
  SetActiveNavigator(FSearchViewer);
  Application.ProcessMessages;
  if AAddress.BookPage <> '' then begin
    if (AAddress.BookPage <> '') and (AAddress.Path <> '') then
      FSearchViewer.SearchSingle(AAddress.BookPage, AAddress.Path)
    else
      FSearchViewer.Search(AAddress.BookPage);
  end;
end;

procedure TMainForm.OnCrossRefAddr(const AAddress : TAddress);
begin
  FDataMgr.ShowAddrReferences(AAddress.BookPage, Self, AAddress.Path);
end;

procedure TMainForm.OnConcordAddr(const AAddress : TAddress);
begin
  if FConcordViewer = Nil then begin
    FConcordViewer := TConcordanceViewer.Create(Self);
    FConcordViewer.AddressMgr := Self;
    FConcordViewer.StatusMgr := Self;
    FConcordViewer.DataMgr := FDataMgr;
  end;
  SetActiveNavigator(FConcordViewer);
end;

procedure TMainForm.OnMediaPlayerAddr(const AAddress : TAddress);
var
  MediaPath : String;
  Src, Src0 : String;
  P : Integer;
begin
  MediaPath := GetDirectoryPath(MediaPathId);
  if CompareText(Copy(AAddress.BookPage, 1, 5), 'http:') = 0 then
    Src := AAddress.AfterBookID
  else begin
    Src := AddBackSlashS(GetDirectoryPath(MediaPathId))+'Video\'+AAddress.BookPage;
    if not FileExists(Src) then begin
      Src0 := Src;
      Src := AddBackSlashS(GetDirectoryPath(MediaSrcPathId))+'Video\'+AAddress.BookPage;
      if not FileExists(Src) then begin
        ShowMessage('Unable to find Media file '+Src0+' or '+Src);
        Exit;
      end;
    end;
  end;

  if FMediaPlayerWin = Nil then
    FMediaPlayerWin := TMediaPlayerForm.Create(Application);
  FMediaPlayerWin.Show;

  if AAddress.Path = '' then
    FMediaPlayerWin.Play(Src)
  else begin
    P := Pos('-', AAddress.Path);
    if P > 0 then
      FMediaPlayerWin.Play(Src, StrToInt(Copy(AAddress.Path, 1, P-1)), StrToInt(Copy(AAddress.Path, P+1, 255)))
    else
      FMediaPlayerWin.Play(Src, StrToInt(AAddress.Path))
  end;
end;

procedure TMainForm.OnCartAddr(const AAddress : TAddress);
begin
  ShoppingCart.AddItem(AAddress.AfterBookID);
end;

procedure TMainForm.OnCartSendAddr(const AAddress : TAddress);
const
  CRLF = #13#10;
var
  ShopCartData : String;
  PostURL, Flags, TargetFrame, PostData, Headers : OleVariant;
  I : Integer;
begin
  if FWebBrowser = Nil then begin
    try
      FWebBrowser := TWebViewer.Create(Self);
      FWebBrowser.StatusMgr := Self;
      FWebBrowser.AddressMgr := Self;
    except
      FWebBrowser := Nil;
      ShowMessage(Format('Unable to open web browser to send shopping cart information', [AAddress]));
    end;
  end;
  if FWebBrowser <> Nil then begin
    SetActiveViewer(FWebBrowser);
    FRegistry.CurSubKey := PrimaryRegistryKey;
    PostURL := FRegistry.ReadString('SCPost', 'http://shop.alim.org/alim6sc/order.ppl');
    Flags := NULL;
    TargetFrame := NULL;

    ShopCartData := ShoppingCart.GetProductsPostData;
    PostData := VarArrayCreate([0, Length(ShopCartData)-1], VT_UI1);
    for I := 1 to Length(ShopCartData) do PostData[I-1] := Ord(ShopCartData[I]);

    Headers := 'Content-type: application/x-www-form-urlencoded'+CRLF;
    FWebBrowser.WebBrowser.Navigate2(PostURL, Flags, TargetFrame, PostData, Headers);
  end else
    ShowMessage(Format('Unable to send shopping cart information', [AAddress]));
end;

procedure TMainForm.OnWebAddr(const AAddress : String);
begin
  HistoryAdd('Internet - ' + AAddress, AAddress);
  if FWebBrowser = Nil then begin
    try
      FWebBrowser := TWebViewer.Create(Self);
      FWebBrowser.StatusMgr := Self;
      FWebBrowser.AddressMgr := Self;
    except
      FWebBrowser := Nil;
      ShowMessage(Format('Unable to open web browser to browse Internet address %s', [AAddress]));
    end;
  end;
  if FWebBrowser <> Nil then begin
    SetActiveViewer(FWebBrowser);
    FWebBrowser.Url := AAddress;
  end else
    ShowMessage(Format('Unable to open web browser to browse Internet address %s', [AAddress]));
end;

procedure SetActionProperties(const AAction : TAction;
                              const AToolBtn : TToolButton;
                              const AEnabled : Boolean;
                              const AImageIndex : Integer;
                              const AHint : String = '';
                              const AAddrMenu : TPopupMenu = Nil;
                              const AAddrList : TCaptionedAddressList = Nil;
                              const AOnActionExec : TNotifyEvent = Nil;
                              const AOnMenuItemClick : TNotifyEvent = Nil);
var
  I : Cardinal;
  MenuItem : TMenuItem;
  CA : TCaptionedAddress;
begin
  if AEnabled then begin
    AAction.Enabled := True;
    AAction.Hint := AHint;
    AAction.OnExecute := AOnActionExec;
    if AAddrList.Count > 0 then begin
      ClearMenu(AAddrMenu);
      for I := 0 to AAddrList.Count-1 do begin
        CA := AAddrList[I];
        MenuItem := TMenuItem.Create(Application);
        MenuItem.Caption := CA.Caption;
        MenuItem.Tag := LongInt(CA);
        MenuItem.OnClick := AOnMenuItemClick;
        AAddrMenu.Items.Add(MenuItem);
      end;
      AToolBtn.DropdownMenu := AAddrMenu;
    end else begin
      AToolBtn.DropdownMenu := Nil;
    end;
  end else begin
    AAction.Enabled := False;
    AAction.OnExecute := Nil;
  end;
end;

procedure TMainForm.SetAddress(const AAddress : TAddress);
var
  BookId : String;
  AmlData : TAmlData;
  NewViewer : TAmlViewer;
begin
  BookId := AAddress.BookId;

  if CompareText(BookId, GenericQuranAyahId) = 0 then begin
    OnQuranAyahAddr(AAddress)
  end else if CompareText(BookId, GenericQuranSuraId) = 0 then begin
    OnQuranSuraAddr(AAddress)
  end else if CompareText(BookId, 'index') = 0 then
    OnIndexAddr(AAddress)
  else if CompareText(BookId, 'recite') = 0 then
    OnReciteQuranAddr(AAddress)
  else if CompareText(BookId, 'search') = 0 then
    OnSearchAddr(AAddress)
  else if CompareText(BookId, 'crossref') = 0 then
    OnCrossRefAddr(AAddress)
  else if CompareText(BookId, 'bookmark') = 0 then
    OnBookmarksAddr(AAddress)
  else if CompareText(BookId, 'concordance') = 0 then
    OnConcordAddr(AAddress)
  else if CompareText(BookId, 'news') = 0 then begin
    FForceNewsShow := True;
    NewsReceiveStart;
  end else if CompareText(BookId, 'home') = 0 then
    OnHomeAddr(AAddress)
  else if CompareText(BookId, 'cart') = 0 then
    OnCartAddr(AAddress)
  else if CompareText(BookId, 'mailto') = 0 then
    ShellExecute(0, Nil, PChar(AAddress.Address), Nil, Nil, SW_SHOW)
  else if CompareText(BookId, 'cartsend') = 0 then
    OnCartSendAddr(AAddress)
  else if CompareText(BookId, 'mediaplayer') = 0 then
    OnMediaPlayerAddr(AAddress)
  else if AAddress.WebAddress then
    OnWebAddr(AAddress.Address)
  else begin
    HistoryAdd(GetAddressCaption(AAddress, True), AAddress.Address);
    AmlData := FDataMgr.DataIds[BookId];
    if AmlData <> Nil then begin
      NewViewer := AmlData.GetViewer(vsDefault);
      if NewViewer <> Nil then begin
        SetActiveViewer(NewViewer);
        NewViewer.Address := AAddress;
      end else
        ShowMessage(Format('No viewer available for %s (%s)', [AmlData.Name, AAddress.Address]));
    end else
      ShowMessage(Format('Unable to find %s', [AAddress.Address]));
  end;
end;

procedure TMainForm.SetAddress(const AAddress : String);
var
  Addr : TAddress;
begin
  Addr := TAddress.Create;
  Addr.Address := AAddress;
  SetAddress(Addr);
  Addr.Free;
end;

function TMainForm.GetAddressCaption(const AAddress : TAddress; const AIncludeName : Boolean) : String;
var
  AmlData : TAmlData;
begin
  if CompareText(AAddress.BookId, GenericQuranAyahId) = 0 then begin
    Result := Format('Sura %s Ayah %s', [AAddress.Volume, AAddress.Element]);
  end else if CompareText(AAddress.BookId, GenericQuranSuraId) = 0 then begin
    Result := Format('Sura %s', [AAddress.BookPage]);
  end else if CompareText(AAddress.BookId, 'index') = 0 then begin
    Result := Format('Show all locations for "%s"', [AAddress.Path])
  end else if CompareText(AAddress.BookId, DefaultReciteId) = 0 then begin
    Result := Format('Recite Sura %s Ayah %s', [AAddress.Volume, AAddress.Element]);
  end else if CompareText(AAddress.BookId, MediaPlayerId) = 0 then begin
    Result := Format('Play audio or video media', [AAddress.Volume, AAddress.Element]);
  end else if CompareText(AAddress.BookId, 'cart') = 0 then begin
    Result := 'Add product to Shopping Cart'
  end else if CompareText(AAddress.BookId, 'cartsend') = 0 then begin
    Result := 'Place an order for all products in the current Shopping Cart'
  end else if CompareText(AAddress.BookId, 'mailto') = 0 then begin
    Result := 'Send mail to ' + AAddress.AfterBookID;
  end else begin
    AmlData := FDataMgr.DataIds[AAddress.BookId];
    if AmlData <> Nil then begin
      Result := AmlData.GetAddressCaption(AAddress, AIncludeName)
    end else if FindString(AAddress.BookId, ['http', 'gopher', 'ftp']) >= 0 then
      Result := Format('Connect to the Internet (%s)', [AAddress.Address])
    else
      Result := Format('Unknown Address (%s)', [AAddress.Address]);
  end;
end;

procedure TMainForm.SetActiveSuraAyah(const ALoc : TSuraAyah);
begin
  GotoSuraAyahEdit.VolumeDelim := SuraAyahStrDelim;
  GotoSuraAyahEdit.Volume := IntToStr(ALoc.suraNum);
  GotoSuraAyahEdit.Element := IntToStr(ALoc.ayahNum);
end;

procedure TMainForm.ReadDataFiles(const APaths : String);
var
  LoadedFromCache : TStBits;
  Updated : TStringList;
  NewData : TAmlData;
  DataFiles : TFileNamesList;
  Node : TXmlNode;
  CatalogsCache, Catalog : TXmlElement;
  FullFileName, CacheFile : String;
  FileInfo : PSearchRec;
  I, FileIdx, CacheSize, CacheTime : Integer;
  EncryptedData, CacheData : String;
  CacheStream : TFileStream;
begin
  StatusBegin('Looking for data files...', True);
  DataFiles := ReadFilesInDirTrees(APaths, '*.*');
  if DataFiles.Count > 0 then begin
    for I := 0 to DataFiles.Count-1 do
      MessageWindow.Add('FS', '''%s'' (time %d)', [DataFiles[I], DataFiles.FileInfo[I].Time]);

    Updated := TStringList.Create;
    LoadedFromCache := TStBits.Create(DataFiles.Count);

    CacheFile := ForceIntoPath(CacheDataPathId, cadDataCatalogCacheFile);
    if FileExists(CacheFile) then begin
      try
        CacheStream := TFileStream.Create(CacheFile, fmOpenRead);
        SetLength(EncryptedData, CacheStream.Size);
        CacheStream.Read(PChar(EncryptedData)^, CacheStream.Size);
        CacheStream.Free;
      except
      end;
      CacheData := ScrambleL(EncryptedData, 'cache');
      if XmlParser.LoadMemory(PChar(CacheData)) then begin
        CatalogsCache := XmlParser.Document.DocumentElement;
        CacheSize := CatalogsCache.ChildNodes.Length-1;
        if CacheSize >= 0 then begin
          for I := 0 to CacheSize do begin
            Node := CatalogsCache.ChildNodes.Item(I);
            if Node.NodeType = ELEMENT_NODE then begin
              Catalog := Node as TXmlElement;
              FullFileName := Catalog.GetAttribute('filename');
              if DataFiles.Find(FullFileName, FileIdx) then begin
                FileInfo := DataFiles.FileInfo[FileIdx];
                try
                  CacheTime := StrToInt(Catalog.GetAttribute('timestamp'));
                  MessageWindow.Add('CACHEHIT', '%d - ''%s'' (time %d ? %d)', [FileIdx, FullFileName, CacheTime, FileInfo.Time]);
                  if CacheTime = FileInfo.Time then begin
                    AddBook(FullFileName, Catalog.CloneNode as TXmlElement);
                    LoadedFromCache.SetBit(FileIdx);
                  end;
                except
                  LoadedFromCache.ClearBit(FileIdx);
                end;
              end else
                MessageWindow.Add('CACHEMISS', '''%s''', [FullFileName]);
            end;
          end;
        end;
        XmlParser.ClearDocument;
      end;
    end;

    for I := 0 to DataFiles.Count-1 do begin
      if not LoadedFromCache.BitIsSet(I) then begin
        FullFileName := DataFiles[I];
        try
          FHome.ShowStatus(Format('Reading book %d of %d. Please wait...', [Succ(I), DataFiles.Count]));
          if FileIsCompoundDoc(FullFileName) then begin
            NewData := AddBook(FullFileName);
            if NewData <> Nil then
              Updated.Add(NewData.Name);
          end;
        except
        end;
        Application.ProcessMessages;
      end;
    end;

    if ((Updated.Count > 0) and (Updated.Count < DataFiles.Count)) then
      ShowMessage('Some new or updated books have been loaded:'#13#13+Updated.Text);

    LoadedFromCache.Free;
    Updated.Free;
  end;
  DataFiles.Free;
  FHome.ShowStatus('');
  StatusEnd;
end;

procedure TMainForm.ReadArabicEncodings;
var
  I : Integer;
  FoundAFont : Boolean;
begin
  StatusBegin('Reading Arabic Encodings...', True);
  try
    XmlParser.LoadDataSource(ForceIntoPath(AppDataPathId, adArabicEncodingDataFile));
    FAraEncodings.LoadFromAML(XmlParser.Document.DocumentElement);
  except
  end;

  FoundAFont := False;
  if FAraEncodings.FontEncodings.Count > 0 then begin
    if HtmlSubs.RtlFontNames = Nil then begin
      HtmlSubs.RtlFontNames := TStringList.Create;
      HtmlSubs.RtlFontNames.Sorted := True;
      HtmlSubs.RtlFontNames.Duplicates := dupIgnore;
    end;
    HtmlSubs.RtlFontDefault := FAraEncodings.DefaultFontName;
    for I := 0 to FAraEncodings.FontEncodings.Count-1 do
      HtmlSubs.RtlFontNames.Add(FAraEncodings.FontEncodings.Names[I]);

    if Screen.Fonts.IndexOf(HtmlSubs.RtlFontDefault) = -1 then begin
      for I := 0 to FAraEncodings.FontEncodings.Count-1 do begin
        if Screen.Fonts.IndexOf(FAraEncodings.FontEncodings.Names[I]) >= 0 then begin
          FoundAFont := True;
          HtmlSubs.RtlFontDefault := FAraEncodings.FontEncodings.Names[I];
          break;
        end;
      end;
    end else
      FoundAFont := True;

    if not FoundAFont then
      ShowMessage('No Arabic Fonts are installed. Please run Setup again.');
  end else
    ShowMessage('No Arabic Fonts found in the Encoding scheme');
  StatusEnd;
end;

procedure TMainForm.ReadQuranStruct;
var
  CacheFile : String;
  CacheStream : TFileStream;
begin
  StatusBegin('Read Quran Structure...', True);
  CacheFile := ForceIntoPath(CacheDataPathId, cadQuranStructCacheFile);
  if FileExists(CacheFile) then begin
    CacheStream := TFileStream.Create(CacheFile, fmOpenRead);
    FQuranStruct.LoadFromStream(CacheStream);
    CacheStream.Free;
  end else begin
    try
      XmlParser.LoadDataSource(ForceIntoPath(AppDataPathId, adQuranStructDataFile));
      FQuranStruct.LoadFromAML(XmlParser.Document.DocumentElement);
    except
      raise Exception.Create('Very serious error in Quran Structure file. Please exit now.');
    end;
    CacheStream := TFileStream.Create(CacheFile, fmCreate);
    FQuranStruct.SaveToStream(CacheStream);
    CacheStream.Free;
  end;
  StatusEnd;
end;

procedure TMainForm.ReadShortcuts;
var
  ScFiles : TFileNamesList;
  I : Integer;
begin
  StatusBegin('Reading bookmarks...', True);
  try
    ScFiles := ReadFilesInDirTrees(GetDirectoryPath(BookmarksPathId), '*.sml');
    if ScFiles.Count > 0 then begin
      for I := 0 to ScFiles.Count-1 do begin
        try
          XmlParser.LoadDataSource(ScFiles[I]);
          FShortcuts.Merge(XmlParser.Document.DocumentElement);
        except       
        end;
      end;
    end;
    ScFiles.Free;
  except
  end;
  StatusEnd;
end;

procedure TMainForm.ReadBookmarks;
var
  BmkFiles : TFileNamesList;
  Node : TBookmarkNode;
  I : Integer;
begin
  StatusBegin('Reading bookmarks...', True);
  FBookmarks.AddChild('', 'http://www.alim.org', 'The Alim Online', '000001').SmallIconIdx := 26;
  FBookmarks.AddChild('', 'http://www.islsoftware.com', 'ISL Software Corporation Online (Company Site)', '000002').SmallIconIdx := 26;
  FBookmarks.AddChild('', 'http://shop.alim.org', 'Shop for Islamic Products Online', '000003').SmallIconIdx := 26;
  FBookmarks.AddChild('', 'http://www.sadaqa.com', 'Donate to Islamic Causes Online', '000004').SmallIconIdx := 26;
  FBookmarks.AddChild('', 'http://www.alim.org/quality', 'Report Alim Discrepancies or Bugs Online', '000005').SmallIconIdx := 26;
  Node := FBookmarks.AddChild('', 'mailto:alim@islsoftware.com', 'Send E-mail to The Alim''s Developers', '000006');
  Node.SmallIconIdx := 26;
  Node.BreakAfter := True;
  try
    BmkFiles := ReadFilesInDirTrees(GetDirectoryPath(BookmarksPathId), '*.bml');
    if BmkFiles.Count > 0 then begin
      for I := 0 to BmkFiles.Count-1 do begin
        try
          XmlParser.LoadDataSource(BmkFiles[I]);
          FBookmarks.Merge(XmlParser.Document.DocumentElement);
        except
        end;
      end;
    end;
    BmkFiles.Free;
  except
  end;
  StatusEnd;
end;

procedure TMainForm.StatusBegin(const AMessage : String; AWaitCursor : Boolean; AMax : LongInt);
begin
  FSaveStatusCursor := Screen.Cursor;
  if AWaitCursor then Screen.Cursor := crHourglass;
  StatusBar.SimpleText := AMessage;
end;

procedure TMainForm.StatusUpdate(const AProgress : LongInt);
begin
end;

procedure TMainForm.StatusEnd;
begin
  Screen.Cursor := FSaveStatusCursor;
  StatusBar.SimpleText := '';
end;

function TMainForm.GetDirectoryPath(const APathId : String; var AStyle : TDirPathStyle) : String;
const
  TempPath : array[0..255] of char = '';
begin
  if APathId = AppPathId then begin
    Result := JustPathNameS(Application.ExeName);
    AStyle := dpsSingle;
    if Result[Length(Result)] = '\' then
      Delete(Result, Length(Result), 1);
    Exit;
  end;

  FRegistry.CurSubKey := PathsRegistryKey;
  Result := FRegistry.ReadString(APathId, 'c:\temp');
  if Pos(MultiDirSeparator, Result) > 0 then
    AStyle := dpsMultiple
  else begin
    AStyle := dpsSingle;
    if Result[Length(Result)] = '\' then
      Delete(Result, Length(Result), 1);
  end;
end;

function TMainForm.GetDirectoryPath(const APathId : String) : String;
var
  AStyle : TDirPathStyle;
begin
  Result := GetDirectoryPath(APathId, AStyle);
end;

function TMainForm.ForceIntoPath(const APathId, AFileOrRelPath : String) : String;
var
  AStyle : TDirPathStyle;
  InitialPath : String;
begin
  InitialPath := GetDirectoryPath(APathId, AStyle);
  if AStyle = dpsSingle then
    Result := AddBackSlashS(InitialPath) + AFileOrRelPath
  else
    raise Exception.CreateFmt(rsErrMultiplePathFoundWhereSingleExpected, [APathId, PathsRegistryKey]);
end;

function TMainForm.VerifyPrerequisites : Boolean;
const
  RequiredFonts : array[0..2] of String[20] = ('Tahoma', 'Verdana', 'Marlett');
  RequiredCDFileBase = '_setiacd.dll';
var
  I : Integer;
  Issues : TStringList;
  ColorDepth : Integer;
  CDPath : String;
  RequiredCDFile : String;
  Quit, CDFound : Boolean;
begin
  Result := True;
  FErrorClose := False;

  Issues := TStringList.Create;
  for I := Low(RequiredFonts) to High(RequiredFonts) do
    if Screen.Fonts.IndexOf(RequiredFonts[I]) = -1 then
      Issues.Add(Format('Font %s not found. Please install it.', [RequiredFonts[I]]));

  if not FRegistry.KeyExists(PrimaryRegistryKey) then begin
    Issues.Add('The Registry settings are invalid. Please reinstall the application.');
    Result := False;
  end;

  FRegistry.CurSubKey := MessagesRegistryKey;
  FRunCount := FRegistry.ReadInteger('RunCount', 0);
  FRegistry.WriteInteger('RunCount', Succ(FRunCount));

  ColorDepth := GetDeviceCaps(Canvas.Handle, BitsPixel);
  if ColorDepth < 8 then
    Issues.Add('Your graphics card is set to '+IntToStr(ColorDepth)+' bit color. You need to display at least 256 colors to effectively use The Alim');
  if (ColorDepth = 8) and (FRunCount < 1) then
    Issues.Add('Your graphics card is set to diplay 256 colors. The Alim looks best in 16 bit or higher colors.');

  if not FRegistry.KeyExists(PathsRegistryKey) then begin
    Issues.Add('The Paths Registry settings are invalid. Please reinstall the application.');
    Result := False;
  end;

  FRegistry.CurSubKey := PrimaryRegistryKey;
  WindowState := TWindowState(FRegistry.ReadInteger('WindowState', 2));

  // we put a "secret" file on the cd and then check to see if that file is found
  // if the file is not found, then that means it's not our CD
  CDPath := GetDirectoryPath('Source');
  RequiredCDFile := ForceIntoPath('Source', RequiredCDFileBase);
  Quit := False;
  repeat
    CDFound := FileExists(RequiredCDFile);
    if not CDFound then
      Quit := MessageDlg(Format('The Alim CD was not found in %s.'+CRLF+CRLF+'Please insert the authorized CD and press Ok to try again.'+CRLF+'Or, you can press Cancel to Quit The Alim.', [CDPath]), mtError, [mbOK, mbCancel], 0) = mrCancel;
  until Quit or CDFound;
  Result := Result and not Quit;

  if Issues.Count > 0 then
    ShowMessage(Issues.Text);

  Issues.Free;
end;

{------------------------------------------------------------------------------}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FSaveStatusCursor := Screen.Cursor;
  FDataMgr := TDataManager.Create;
  FDataMgr.SmallImages := ImagesSmall;

  FRegistry := TStRegIni.Create(RIMachine, False);

  FBookPaths := GetDirectoryPath(BooksPathId) + MultiDirSeparator + GetDirectoryPath(BooksSrcPathId);
  FAppDataPath := GetDirectoryPath(AppDataPathId);
  FShortcuts := TBookmarks.Create;
  FBookmarks := TBookmarks.Create;
  FHomeShortcuts := TBookmarks.Create;
  FIndexes := TBookmarks.Create;
  FHistory := TCaptionedAddressList.Create;
  FAraEncodings := TArabicEncodings.Create;
  FQuranStruct := TQuranStructure.Create;
  GotoSuraAyahEdit.VolumeDelim := SuraAyahStrDelim;
  //ToolBtnHistoryPrev.ImageIndex := imgIdxHistoryPrev;

  FHome := THomeViewer.Create(Self);
  FHome.AddressMgr := Self;
  FHome.DataMgr := FDataMgr;
  FHome.StatusDisplay := Self;

  FHome.OwnershipPanel.Caption := 'Release ' + GetAppVersionInfo + '. ' + FHome.OwnershipPanel.Caption;
end;

procedure TMainForm.FormActivate(Sender: TObject);
const
  FirstTime : Boolean = True;
var
  I : Integer;
  AuthDlg : TAuthenticDlg;
  AuthResult : Integer;
begin
  if FirstTime then begin
    if not VerifyPrerequisites then begin
      FErrorClose := True;
      Close;
      Exit;
    end;

    NewsReceiveStart;

    try
      XmlParser.LoadDataSource(ForceIntoPath(BookmarksPathId, adHomeDataFile));
      FHomeShortcuts.Merge(XmlParser.Document.DocumentElement);
      FHome.Shortcuts := FHomeShortcuts;
    except
    end;
    SetAddress('home:');
    Application.ProcessMessages;

    FirstTime := False;
    FShortcuts.AddCategory('Islam', '001');
    FShortcuts.AddCategory('Quran', '002');
    FShortcuts.AddCategory('Hadith', '003');

    ReadDataFiles(FBookPaths);
    AddSpecialBook('_quran_multi');
    ReadShortcuts;
    ReadBookmarks;
    ReadArabicEncodings;
    ReadQuranStruct;

    if FDataMgr.Count > 0 then begin
      for I := 0 to FDataMgr.Count-1 do begin
        FDataMgr[I].QuranStruct := FQuranStruct;
        FDataMgr[I].FinalizeLoad;
      end;
    end else
      ShowMessage(Format('No books found in %s', [FBookPaths]));

    PopulateAppMenus;

    if (FRunCount = 0) then begin
      AuthDlg := TAuthenticDlg.Create(Application);
      AuthResult := AuthDlg.ShowModal;
      AuthDlg.Free;
      if AuthResult = mrNo then begin
        Application.Terminate;
        Exit;
      end;
    end;

    CheckForReminders(FRunCount, ForceIntoPath(RemindersPathId, ''));

    FRegistry.CurSubKey := PrimaryRegistryKey;
    if FRegistry.ReadBoolean('ShowTipsOnStartup', True) and (FRunCount > 0) then
      AlimViewTipsClick(Self);

    ShoppingCart.SetPrimaryPath(ForceIntoPath(ShoppingPathId, ''));
    ShoppingCart.AddressMgr := Self;

    //SetAddress('cart:item_id=test|item_name=the alim for windows|vendor=isl software corp|quantity=1|unit_cost=89.10|item_address=recite:');
    //SetAddress('cart:item_id=test2|item_name=arabic playhouse|vendor=isl software corp|quantity=1|unit_cost=89.10|item_address=http://www.islsoftware.come');
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FRegistry.Free;
  FShortcuts.Free;
  FBookmarks.Free;
  FHomeShortcuts.Free;
  FIndexes.Free;
  FHistory.Free;
  FAraEncodings.Free;
  FQuranStruct.Free;
  FDataMgr.Free;
end;

procedure TMainForm.AlimMenuViewInstalledBooksClick(Sender: TObject);
begin
  //InstalledBooksForm.Populate(FBookPaths, FBooksByFName, FBooksById, FBooksByType);
  InstalledBooksForm.Show;
end;

procedure TMainForm.AlimMenuToolBtnClick(Sender: TObject);
begin
  SetAddress('home:');
end;

procedure TMainForm.ActionQuranSelectSuraExecute(Sender: TObject);
begin
  SetAddress(GenericQuranAyahId + ':');
end;

procedure TMainForm.ActionQuranReciteExecute(Sender: TObject);
begin
  SetAddress('recite:');
end;

procedure TMainForm.ActionAppSearchExecute(Sender: TObject);
begin
  SetAddress('search:');
end;

procedure TMainForm.ActionAppIndexesExecute(Sender: TObject);
begin
  SetAddress('index:');
end;

procedure TMainForm.ActionAppBookmarksExecute(Sender: TObject);
begin
  SetAddress('bookmark:');
end;

procedure TMainForm.ActionAppHomeExecute(Sender: TObject);
begin
  SetAddress('home:');
end;

procedure TMainForm.ActionAppCrossRefsExecute(Sender: TObject);
begin
  SetAddress('crossref:');
end;

procedure TMainForm.ActionAppConcordancesExecute(Sender: TObject);
begin
  SetAddress('concordance:');
end;

procedure TMainForm.NavgCloseLabelClick(Sender: TObject);
begin
  SetActiveNavigator(Nil);
end;

procedure TMainForm.NavgExpandLabelClick(Sender: TObject);
begin
  if NavgPanel.Width > DefaultNavgWidth then
    NavgPanel.Width := DefaultNavgWidth
  else
    NavgPanel.Width := Self.Width div 2;
end;

procedure TMainForm.GotoSuraAyahEditEnterKey(Sender: TObject);
var
  Sura, Ayah : Word;
begin
  if Str2WordS(GotoSuraAyahEdit.Volume, Sura) and Str2WordS(GotoSuraAyahEdit.Element, Ayah) then
    SetAddress(CreateQuranAddress(qatGeneric, Sura, Ayah));
end;

procedure TMainForm.ShortcutsBarClick(Sender: TObject);
var
  Data : TObject;
begin
  if ShortcutsBar.Tabs.Count > 0 then begin
    Data := ShortcutsBar.Tabs.Objects[ShortcutsBar.TabIndex];
    if Data is TBookmarkNode then begin
      if (Data as TBookmarkNode).Address <> '' then
        SetAddress((Data as TBookmarkNode).Address);
    end;
  end;
end;

procedure TMainForm.QuranToolLabelClick(Sender: TObject);
var
  ActiveViewer : TAmlViewer;
begin
  if Sender is TComponent then begin
    if ActiveViewerPanel.Form is TAmlViewer then
      ActiveViewer := ActiveViewerPanel.Form as TAmlViewer
    else
      ActiveViewer := Nil;
    case (Sender as TComponent).Tag of
      1 : if ActiveViewer <> Nil then ActiveViewer.Navigate(nvPage, nvdPrevious);
      2 : if ActiveViewer <> Nil then ActiveViewer.Navigate(nvPage, nvdNext);
      3 : if ActiveViewer <> Nil then ActiveViewer.ExecuteQuranCmd(qcGotoSura);
      4 : if ActiveViewer <> Nil then ActiveViewer.ExecuteQuranCmd(qcArabicSearch);
      5 : if ActiveViewer <> Nil then ActiveViewer.ExecuteQuranCmd(qcViewSuraIntro);
      6 : if ActiveViewer <> Nil then ActiveViewer.ExecuteQuranCmd(qcRecite);
      7 : if ActiveViewer <> Nil then ActiveViewer.Customize;
    end;
  end;
end;

procedure TMainForm.MenuItemAlimExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Viewer : TAmlViewer;
begin
  if ActiveViewerPanel.Form is TAmlViewer then
    Viewer := (ActiveViewerPanel.Form as TAmlViewer)
  else
    Viewer := Nil;

  case Key of
    VK_ESCAPE :
      if ssShift in Shift then
        SetActiveNavigator(Nil)
      else begin
        if FSearchViewer <> Nil then
          FSearchViewer.CancelSearch := True;
      end;
    VK_F5 :
      if not (ssShift in Shift) then begin
        if Viewer <> Nil then Viewer.Navigate(nvPage, nvdPrevious)
      end else begin
        if FSearchViewer <> Nil then
          FSearchViewer.Navigate(nvdPrevious);
      end;
    VK_F6 :
      if not (ssShift in Shift) then begin
        if Viewer <> Nil then Viewer.Navigate(nvPage, nvdNext)
      end else begin
        if FSearchViewer <> Nil then
          FSearchViewer.Navigate(nvdNext);
      end;
    VK_F7 :
      if not (ssShift in Shift) then begin
        if Viewer <> Nil then Viewer.Navigate(nvItem, nvdPrevious)
      end else begin
        if FSearchViewer <> Nil then
          FSearchViewer.Navigate(nvdPrevious);
      end;
    VK_F8 :
      if not (ssShift in Shift) then begin
        if Viewer <> Nil then Viewer.Navigate(nvItem, nvdNext)
      end else begin
        if FSearchViewer <> Nil then
          FSearchViewer.Navigate(nvdNext);
      end;
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if not FErrorClose then begin
    StatusBegin('Storing data cache for instant startup next time...', True);
    FDataMgr.StoreToFile(ForceIntoPath(CacheDataPathId, cadDataCatalogCacheFile), 'cache');
    StatusEnd;
  end;
end;

procedure TMainForm.PageSetup1Click(Sender: TObject);
begin
  PrinterSetupDialog.Execute;
end;

procedure TMainForm.ActionAppPrintUpdate(Sender: TObject);
var
  ActiveViewer : TAmlViewer;
begin
  if ActiveViewerPanel.Form is TAmlViewer then begin
    ActiveViewer := ActiveViewerPanel.Form as TAmlViewer;
    ActionAppPrint.Enabled := ActiveViewer.EnablePrint;
  end;
end;

procedure TMainForm.ActionEditCopyUpdate(Sender: TObject);
var
  ActiveViewer : TAmlViewer;
begin
  if ActiveViewerPanel.Form is TAmlViewer then begin
    ActiveViewer := ActiveViewerPanel.Form as TAmlViewer;
    ActionEditCopy.Enabled := ActiveViewer.EnableCopy;
  end;
end;

procedure TMainForm.ActionEditCopyExecute(Sender: TObject);
var
  ActiveViewer : TAmlViewer;
begin
  if ActiveViewerPanel.Form is TAmlViewer then begin
    ActiveViewer := ActiveViewerPanel.Form as TAmlViewer;
    ActiveViewer.CopyToClipboard;
  end;
end;

procedure TMainForm.ActionAppPrintExecute(Sender: TObject);
var
  ActiveViewer : TAmlViewer;
begin
  if ActiveViewerPanel.Form is TAmlViewer then begin
    ActiveViewer := ActiveViewerPanel.Form as TAmlViewer;
    ActiveViewer.Print(PrintDialog);
  end;
end;

procedure TMainForm.AppEventsIdle(Sender: TObject; var Done: Boolean);
begin
  Screen.Cursor := crDefault;
end;

procedure TMainForm.ActionAppFAQExecute(Sender: TObject);
begin
  SetAddress('alimfaq:');
end;

procedure TMainForm.AlimViewMessagesClick(Sender: TObject);
begin
  MessageWindow.Show;
end;

procedure TMainForm.AlimViewTipsClick(Sender: TObject);
begin
  AppTips.TipsLocation := locIniFile;
  AppTips.IniFile := ForceIntoPath(AppDataPathId, adAppTipsFile);
  AppTips.IniSection := 'Alim';
  AppTips.Execute;
  
  FRegistry.CurSubKey := PrimaryRegistryKey;
  FRegistry.WriteBoolean('ShowTipsOnStartup', AppTips.ShowAtStartup);
end;

procedure TMainForm.NewsReceiveStart;
var
  UpdateFileName : String;
begin
  with FTPClient do begin
    UpdateFileName := FRegistry.ReadString('UpdateFTPHostFile', 'update.aml');
    FUpdateMasterFName := ForceIntoPath(CacheDataPathId, UpdateFileName);
    DeleteFile(FUpdateMasterFName);

    FRegistry.CurSubKey := PrimaryRegistryKey;
    Binary := True;
    HostDirName := FRegistry.ReadString('UpdateFTPHostPath', 'Alim6');
    HostFileName := UpdateFileName;
    HostName := FRegistry.ReadString('UpdateFTPHostName', 'ftp.alim.org');
    LocalFileName := FUpdateMasterFName;

    MultiThreaded := True;
    UserName := FRegistry.ReadString('UpdateFTPUserName', 'anonymous');
    PassWord := FRegistry.ReadString('UpdateFTPPassword', 'alim6@alim.org');

    MessageWindow.Add('FTP', 'Receive of ftp://%s/%s/%s to %s started', [HostName, HostDirName, HostFileName, LocalFileName]);
    FUpdateReceive := urMasterFile;

    try
      ReceiveAsync;
    except
      on E : Exception do
        ShowMessage('Error downloading news from the Internet. Are you connected?'#13#10#13#10'Detail: '+E.Message);
    end;
  end;
end;

procedure TMainForm.NewsReceiveShow;
var
  I : Integer;
  NewsViewer : TTextViewer;
  DateNode, NewsNode : TXmlElement;
  LastShown, ThisFileUpdate : TDateTime;
begin
  if not FileExists(FUpdateMasterFName) then begin
    if FForceNewsShow then
      ShowMessage('Unable to download the Alim News. Are you connected to the Internet?');
    Exit;
  end;

  if FUpdateData <> Nil then begin
    FUpdateData.Free;
    FUpdateData := Nil;
  end;

  try
    FTPUpdateParser.LoadDataSource(FUpdateMasterFName);
    if FTPUpdateParser.ErrorCount > 0 then begin
      for I := 0 to FTPUpdateParser.ErrorCount-1 do
        MessageWindow.Add('UPDATE', '[%s] AML Parser error %s', [FUpdateMasterFName, FTPUpdateParser.Errors[I]]);
    end else begin
      try
        FUpdateData := FTPUpdateParser.Document.DocumentElement.CloneNode as TXmlElement;
        DateNode := FUpdateData.FindElement('lastupdate');
        if DateNode <> Nil then begin
          ThisFileUpdate := StrToDate(DateNode.ElementText);
          FRegistry.CurSubKey := MessagesRegistryKey;
          LastShown := StrToDate(FRegistry.ReadString('LastNewsUpdate', '1/1/1990'));
          if (ThisFileUpdate > LastShown) or FForceNewsShow then begin
            FForceNewsShow := False;
            NewsNode := FUpdateData.FindElement('news');
            if NewsNode <> Nil then begin
              NewsViewer := ShowText('', NewsNode.XMLDocument, 350, 200);
              NewsViewer.Left := NewsViewer.Left + 100;
              NewsViewer.Top := NewsViewer.Top + 100;
            end;
            FRegistry.WriteString('LastNewsUpdate', DateNode.ElementText);
          end;
        end else
          MessageWindow.Add('UPDATE', 'Last update field <lastupdate> not found in %s', [FUpdateMasterFName]);
      except
        on E : Exception do
          MessageWindow.Add('UPDATE', 'Error displaying update html: %s', [E.Message]);
      end;
    end;
  except
    MessageWindow.Add('FTP', 'Update file %s could not be parsed', [FUpdateMasterFName]);
  end;
end;

procedure TMainForm.FTPClientRequestDone(Sender: TObject;
  RqType: TFtpRequest; Error: Word);
var
  Code : Integer;
begin
  Code := Ord(FUpdateReceive);
  MessageWindow.Add('FTP', 'Request Done [%d]', [Code]);

  if (FUpdateReceive = urMasterFile) then
    NewsReceiveShow;
  FUpdateReceive := urNone;
end;

procedure TMainForm.AlimDownloadNewsClick(Sender: TObject);
begin
  FForceNewsShow := True;
  NewsReceiveStart;
end;

procedure TMainForm.ViewShoppingCartClick(Sender: TObject);
begin
  ShoppingCart.Show;
end;

initialization
  MoneyCtrls.ThePalette := HtmlUn2.ThePalette;
  IslCtrls.DefaultPalette := HtmlUn2.ThePalette;
end.
