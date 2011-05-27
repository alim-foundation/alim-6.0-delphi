unit AmlView;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     Menus, XmlObjModel, CompDoc, IslUtils, QuranStruct, IndexStruct, RxGif, Math,
     Arabic, HtmlView, RxMenus;
                                    
const
  AppPathId = '_app';
  AppDataPathId = 'AppData';
  CacheDataPathId = 'Cache';
  BooksPathId = 'Books';
  BooksSrcPathId = 'BooksSrc';
  MediaPathId = 'Media';
  MediaSrcPathId = 'MediaSrc';
  BookmarksPathId = 'Bookmarks';
  ReciteMapPathId = 'ReciteMap';
  ReciteMapSrcPathId = 'ReciteMapSrc';
  RemindersPathId = 'Reminders';
  ShoppingPathId = 'Shop';
  AmlDataEncryptKey = 'aml';
  CRLF = #13#10;

  AddrVolumeElemDelim = '.';
  AddrPathDelim = '/';
  AddrSearchDelim = '?';
  AddrInternalHotspotPrefix = '!';
  AddrFootnotePrefix = 'fn:';
  ImageFromStorePrefix = '$';        // if an image name starts with this, it's inside a TRootStorage
  ImageFromImageListPrefix = '#';    // if an image name starts with this, it's a smallImages index
  MultiDirSeparator = ';';

  PageIdxSubject = 'SubjectsIndex';
  SpecialIdxFullText = 'WordsIndex';
  SpecialIdxInverseSubjects = 'InverseSubjectsIndex';
  SpecialIdxHRefs = 'HRefsIndex';

  MaxCopyCharsToClipboard = 512;

  { AmlData flags }
  adfShareViewer         = $00000001;
  adfHasFullTextIndex    = $00000002;
  adfHasInverseSubjIndex = $00000004;
  adfHasHRefIndex        = $00000008;
  adfCanCopyText         = $00000010;
  adfCanPrintText        = $00000020;
  adfIsQuranData         = $00000100;

type
  TDirPathStyle = (dpsSingle, dpsMultiple);
  TDirPath = String;

  TAmlData = class;
  TAddress = class;
  TAmlViewer = class;
  TBookmarks = class;
  TBookmarkNode = class;
  TDataManager = class;

  TCaptionedAddress = class(TObject)
  protected
    FCaption : String;
    FAddress : String;
  public
    property Address : String read FAddress write FAddress;
    property Caption : String read FCaption write FCaption;
  end;

  TCaptionedAddressList = class(TObjList)
  protected
    function GetAddress(Index : Cardinal) : TCaptionedAddress; virtual;
  public
    procedure AppendAddr(const ACaption, AAddress : String); virtual;
    procedure PrependAddr(const ACaption, AAddress : String); virtual;
    property Addresses[Index : Cardinal] : TCaptionedAddress read GetAddress; default;
  end;

  TToolsPanels = (tpNone, tpQuran);
  IAddressManager = interface
    function AddShortcut(const ALibrary, AAddress, ACaption, ASortKey : String) : TBookmarkNode;
    procedure SetQuickLinks(const AHeader : String; const AColor : TColor;
                            const ALinks : TBookmarks; const AActiveCaption : String;
                            const AShowTools : TToolsPanels);

    procedure SetAddress(const AAddress : TAddress); overload;
    procedure SetAddress(const AAddress : String); overload;
    function GetAddressCaption(const AAddress : TAddress; const AIncludeName : Boolean) : String;
    procedure HistoryAdd(const ACaption, AAddress : String);

    procedure SetActiveSuraAyah(const ALoc : TSuraAyah);
    procedure ShowBookmarks(const ABookmarks : TBookmarks; const ATitle : String; const ABecomeOwner : Boolean);

    function GetDirectoryPath(const APathId : String; var AStyle : TDirPathStyle) : String; overload;
    function GetDirectoryPath(const APathId : String) : String; overload;
    function ForceIntoPath(const APathId, AFileOrRelPath : String) : String;
  end;

  IStatusDisplay = interface
    procedure StatusBegin(const AMessage : String; AWaitCursor : Boolean; AMax : LongInt = 0);
    procedure StatusUpdate(const AProgress : LongInt);
    procedure StatusEnd;
  end;

  TViewerShare = (vsDefault, vsShare, vsCreate);

  TAmlLinks = (alExternal, alBookmarks);
  TAmlLinksArray = array[TAmlLinks] of TXmlElement;


  TAmlDataFont = class(TObject)
  protected
    FName : String;
    FPointSize : Cardinal;
    FHtmlSize : Cardinal;
    FColor : TColor;

    function GetHtmlColor : String;
  public
    procedure AssignInHtmlViewer(const AHtmlView : THtmlViewer);

    property Name : String read FName write FName;
    property PointSize : Cardinal read FPointSize write FPointSize;
    property HtmlSize : Cardinal read FHtmlSize write FHtmlSize;
    property Color : TColor read FColor write FColor;
    property HtmlColor : String read GetHtmlColor;
  end;

  TAmlData = class(TObject)
  protected
    FOwner : TDataManager;
    FFileName : String;
    FXMLParser : TXmlObjModel;
    FCatalog : TXmlElement;
    FCatalogContent : TXmlElement;
    FCatalogNames : TXmlElement;
    FCatalogIndexes : TXmlElement;
    FCatalogLinks : TAmlLinksArray;
    FCatalogLanguage : TXmlElement;
    FCatalogHome : TXmlElement;
    FCatalogCustom : TXmlElement;
    FFlags : LongInt;
    FFonts : TObjDict;
    FId : String;
    FGroupName : String;
    FContentType : String;
    FContentSubtype : String;
    FShortName : String;
    FLanguageId : String;
    FName : String;
    FViewer : TAmlViewer;
    FAddrMgr : IAddressManager;
    FStatus : IStatusDisplay;
    FIndexes : TObjDict;
    FIndexNames : TStringList;
    FAraEncodings : TArabicEncodings;
    FQuranStruct : TQuranStructure;
    FOpenConcordances : TObjDict;

    function GetLibName : String; virtual;
    function CreateViewer : TAmlViewer; virtual;
    function GetMainStorage : TRootStorage; virtual;
    function GetContentTypeKey : String; virtual;

    function GetIndex(const AIndexName : String) : TPageIndex; virtual;
    procedure SetupDefaultFlags; virtual;
    function CreateDefaultFont(const AFontId : String) : TAmlDataFont; virtual;
  public
    constructor Create(const AOwner : TDataManager); virtual;
    destructor Destroy; override;

    function InitializeLoad(const AFileName : String;
                            const ACatalog : TXmlElement) : Boolean; virtual;
      {-called during initial creation (when books are scanned) }
    procedure FinalizeLoad; virtual;
      {-called once after all books are loaded }
    procedure Close; virtual;
      {-closes the file, releases all data items }

    function AsHtml(const AElement : TXmlElement; const ASearchHits : String) : String; virtual;

    function GetAddressCaption(const AAddress : TAddress; const AIncludeName : Boolean) : String; virtual;
    function GetViewer(const AShare : TViewerShare) : TAmlViewer; virtual;

    function OpenStream(const AName : String) : TStream; virtual;
    function ReadStream(const AName : String) : TMemoryStream;
    function ReadImage(const Key : String) : TBitmap; virtual;
    function ReadStringList(const AKey : String) : TStringList; virtual;
    function ReadString(const AKey : String) : String; virtual;
    function ReadXML(const AKey : String) : TXmlElement; virtual;

    function OpenConcordance(const AName : String) : TStaticIndex;
    procedure CloseConcordance(const AName : String);

    procedure SetFlag(const AFlag : LongInt; const AValue : Boolean = True);
    procedure ClearFlag(const AFlag : LongInt);
    function FlagIsSet(const AFlag : LongInt) : Boolean;
    function ChangeFont(const AFontId : String; const AOnlyColor : Boolean = False) : Boolean;
    function GetFont(const AFontId : String) : TAmlDataFont;

    property Catalog : TXmlElement read FCatalog;
    property MainStorage : TRootStorage read GetMainStorage;
    property ContentType : String read FContentType;
    property ContentSubtype : String read FContentSubtype;
    property ContentTypeKey : String read GetContentTypeKey;
    property AraEncodings : TArabicEncodings read FAraEncodings write FAraEncodings;
    property QuranStruct : TQuranStructure read FQuranStruct write FQuranStruct;
    property Owner : TDataManager read FOwner write FOwner;
    property FileName : String read FFileName;
    property Id : String read FId;
    property GroupName : String read FGroupName;
    property Home : TXmlElement read FCatalogHome;
    property LanguageId : String read FLanguageId;
    property LibName : String read GetLibName;
    property Links : TAmlLinksArray read FCatalogLinks;
    property Index[const AName : String] : TPageIndex read GetIndex;
    property IndexNames : TStringList read FIndexNames;
    property AddressMgr : IAddressManager read FAddrMgr write FAddrMgr;
    property Name : String read FName;
    property ShortName : String read FShortName;
    property StatusDisplay : IStatusDisplay read FStatus write FStatus;
    property XmlParser : TXmlObjModel read FXMLParser write FXMLParser;
  end;

  // address format:
  //   bookId:bookPage/path
  //   bookPage can be abc.defg format, where abc is Volume, defg is Element
  TAddress = class(TObject)
  protected
    FBookID : String;
    FAfterBookID : String;
    FBookPage : String;
    FPath : String;
    FSubPath : String;
    FVolDelim : Char;
    FPathDelim : Char;
    FSearchDelim : Char;
    FWebAddress : Boolean;
    FSearchHits : String;

    function GetAddress : String; virtual;
    procedure SetAddress(const AAddress : String); virtual;
    function GetElement : String;
    procedure SetElement(AValue : String);
    function GetVolume : String;
    procedure SetVolume(AValue : String);
  public
    constructor Create;
    property BookID : String read FBookID write FBookID;
    property AfterBookID : String read FAfterBookID write FAfterBookID;
    property BookPage : String read FBookPage write FBookPage;
    property Path : String read FPath write FPath;
    property SubPath : String read FSubPath write FSubPath;
    property Address : String read GetAddress write SetAddress;
    property Volume : String read GetVolume write SetElement;
    property Element : String read GetElement write SetElement;
    property SearchHits : String read FSearchHits write FSearchHits;
    property WebAddress : Boolean read FWebAddress;
  end;

  TBookmarkNode = class(TObject)
  protected
    FCaption : String;       // what the user sees for the address
    FSortKey : String;       // the way the address is sorted
    FAddress : String;       // the actual address (blank if it's just a container)
    FShortcut : String;      // a keyboard accelarator, if any
    FOwnTheData : Boolean;   // true if icons are owned by this object, false if not (default)
    FSmallIconSrc : String;  // the file or storage name of the small icon
    FLargeIconSrc : String;  // the file or storage name of the large icon
    FSmallIcon : TBitmap;    // small icon (owned by this object if non-null)
    FLargeIcon : TBitmap;    // large icon (owned by this object if non-null)
    FLargeIconIdx : Integer; // if > 0, then index into largeImages ImageList
    FSmallIconIdx : Integer; // if > 0, then index into smallImages ImageList
    FChildren : TBookmarks;  // any children (shouln't be null)
    FBreakBefore : Boolean;  // break before this bookmark (in a menu)?
    FBreakAfter : Boolean;   // break after this bookmark (in a menu)?
    FExtraData : Pointer;    // any extra data
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property OwnTheData : Boolean read FOwnTheData write FOwnTheData;
    property Address : String read FAddress write FAddress;
    property Caption : String read FCaption write FCaption;
    property Children : TBookmarks read FChildren write FChildren;
    property Shortcut : String read FShortcut write FShortcut;
    property SortKey : String read FSortKey write FSortKey;
    property SmallIconSrc : String read FSmallIconSrc write FSmallIconSrc;
    property LargeIconSrc : String read FLargeIconSrc write FLargeIconSrc;
    property SmallIcon : TBitmap read FSmallIcon write FSmallIcon;
    property LargeIcon : TBitmap read FLargeIcon write FLargeIcon;
    property LargeIconIdx : Integer read FLargeIconIdx write FLargeIconIdx;
    property SmallIconIdx : Integer read FSmallIconIdx write FSmallIconIdx;
    property BreakBefore : Boolean read FBreakBefore write FBreakBefore;
    property BreakAfter : Boolean read FBreakAfter write FBreakAfter;
    property ExtraData : Pointer read FExtraData write FExtraData;
  end;

  TBookmarks = class(TObject)
  protected
    FId : String;
    FNodes : TObjDict;

    function GetBookmark(Index : Cardinal) : TBookmarkNode;
    function GetCount : Cardinal;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure AddNode(const AAddress : TBookmarkNode); overload; virtual;
      {-add the given node to this list}
    function AddCategory(const ACaption, ASortKey : String) : TBookmarkNode; overload; virtual;
      {-add a 'parent' node to the list}
    function AddChild(const AParent, AAddress, ACaption, ASortKey : String) : TBookMarkNode; virtual;
      {-if node 'ACategory' is found, add the address; otherwise, create the node}
    function Find(const ACaption : String) : TBookmarkNode; virtual;
      {-find the node with the given caption}
    procedure Clear; virtual;
      {-remove all of the bookmarks}

    procedure Merge(const AXmlElement : TXmlElement); overload; virtual;
      {-merge bookmarks from a element that contains <bookmark> or <shortcut> tags}

    procedure Populate(const AParentMenu : TMenuItem; const AOnClick : TNotifyEvent;
                       const AImageList : TImageList;
                       const ADefCatgImgIdx, ADefBmkImgIdx : Integer;
                       const AMainAccelsUsed : String = '';
                       const ATagNodeMenuItem : TBookmarkNode = Nil;
                       const ATagNodeMenuItemValue : LongInt = 0); virtual;
      {-put all the bookmarks into the given menu}

    property Id : String read FId write FId;
    property Bookmarks[Index : Cardinal] : TBookmarkNode read GetBookmark; default;
    property Count : Cardinal read GetCount;
  end;

  TDataList = class(TList)
  protected
    FName : String;
    function GetData(Index : Cardinal) : TAmlData; virtual;
  public
    property Name : String read FName write FName;
    property Data[Index : Cardinal] : TAmlData read GetData; default;
  end;

  TRootStoreInfo = class(TObject)
  protected
    FFileName : String;
    FStorage : TRootStorage;
    FUsage : Cardinal;

    procedure SetFileName(const AFileName : String);
  public
    destructor Destroy; override;
    procedure IncUsage;
    procedure DecUsage;

    property FileName : String read FFileName write SetFileName;
    property Storage : TRootStorage read FStorage;
    property Usage : Cardinal read FUsage;
  end;

  TDataLookup = (dslDataId, dslFileName, dslDataType, dslLibrary);
  TDataManager = class(TDataList)
  protected
    FById : TStringList;           // string is id, object is TAmlData
    FByFName : TStringList;        // string is filename, object is TAmlData
    FByIsQuran : TStringList;      // string is full name, value is aml data that answered IsQuranData to true
    FByLib : TObjDict;             // key is library name, value is TDataList
    FByType : TObjDict;            // key is dataType, value is TDataList
    FByIndex : TObjDict;           // key is index name, value is TDataList
    FByGroup : TObjDict;           // key is group name, value is TBookmarks
    FSpecialIndexes : TObjDict;    // key is index name, value is TDataList
    FOwnTheData : Boolean;         // own the data passed into AddData (TRUE by default)?
    FRootStorages : TObjDict;      // storages currently open, key is filename value is TRootStoreInfo
    FImages : TObjDict;            // key id name of bitmap, value is a TBitmap
    FSmallImages : TImageList;     // the small images list (NOT owned by this object)

    function GetDataId(Index : String) : TAmlData;
    function GetDataFile(Index : String) : TAmlData;
    function GetDataType(Index : String) : TDataList;
    function GetIndex(Index : String) : TDataList;
    function GetSpecialIndex(Index : String) : TDataList;
    function GetLibrary(Index : String) : TDataList;
    function GetGroupMembers(Index : String) : TBookmarks;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Clear; override;
      {-dispose all dataStores and remove them from this manager}

    procedure AddData(const AData : TAmlData); virtual;
      {-add the given data to the list and populate sub-lists}
    function Get(const ALookup : TDataLookup; const AKey : String) : TAmlData; virtual;
      {-return either a single dataStore or nil if Key is not found}
    procedure FillAll(const ALookup : TDataLookup; const AKey : String; const AList : TDataList; const AClear : Boolean = False); virtual;
      {-given a dataList (owned by caller), fill in all stores of AKey}
    function GetAll(const ALookup : TDataLookup; const AKey : String) : TDataList; virtual;
      {-return a newly created dataList (owned by caller), populated by zero or more dataStores}

    function OpenRootStorage(const AFileName : String) : TRootStorage;
      {-return a pointer to a newly created root storage or an existing one (with incremented usage count)}
    procedure CloseRootStorage(const AFileName : String);
      {-close the root storage pointed to by AFileName if usage count is 0}
    procedure ShowAddrReferences(const ASpecialIndex : String; const AAddrMgr : IAddressManager; const AAddress : String);
      {-given an address, see if it's referenced in any special indexes (inverse subjects, hrefs, etc)}

    function GetImageStream(const AFileName : String) : TMemoryStream; overload; virtual;
      {-read and cache an image from the file system, put into a memory stream}
    function GetImageStream(const AStorage : TStorage; AStorageName, AStreamName : String) : TMemoryStream; overload; virtual;
      {-read and cache an image from the file system, put into a memory stream}
    function GetImageStream(const AImageListIdx : Integer; const ASmallImages : Boolean = True) : TMemoryStream; overload; virtual;
      {-read and cache an image from the file system, put into a memory stream}
    function GetImage(const AFileName : String) : TBitmap; overload; virtual;
      {-read and cache an image from the file system}
    function GetImage(const AStorage : TStorage; AStorageName, AStreamName : String) : TBitmap; overload; virtual;
      {-read and cache an image from within a storage}
    function GetImage(const AImageListIdx : Integer; const ASmallImages : Boolean = True) : TBitmap; overload; virtual;
      {-read an image from the global image list}

    procedure StoreToFile(const AFileName : String; const AEncryptKey : String);
      {-store all the catalogs into a file}

    property SpecialIndexes[Index : String] : TDataList read GetSpecialIndex;
    property PageIndexes[Index : String] : TDataList read GetIndex;
    property DataIds[Index : String] : TAmlData read GetDataId;
    property DataFiles[Index : String] : TAmlData read GetDataFile;
    property DataTypes[Index : String] : TDataList read GetDataType;
    property GroupMembers[Index : String] : TBookmarks read GetGroupMembers;
    property Libraries[Index : String] : TDataList read GetLibrary;
    property QuranDataList : TStringList read FByIsQuran;

    property PageIndexesDict : TObjDict read FByIndex write FByIndex;
    property SmallImages : TImageList read FSmallImages write FSmallImages;
  end;

  TNavigate = (nvPage, nvItem);
  TNavigateDirection = (nvdPrevious, nvdNext, nvdCaptionedAddr);
  TQuranCommand = (qcGotoSura, qcArabicSearch, qcViewSuraIntro, qcRecite);

  TPrintHeaderInfo =
    record
      AppTitle : String;
      BookName : String;
      BookPage : String;
      BookSection : String;
    end;

  TAmlViewer = class(TForm)
  protected
    FAddrMgr : IAddressManager;
    FDataMgr : TDataManager;
    FStatus : IStatusDisplay;
    FQuickLinks : TBookmarks;
    FContextMenu : TRxPopupMenu;      // valid only during context menu popup
    FContextMenuBmks : TBookmarks;    // valid only during context menu popup
    FSearchHits : String;             // any search hits that should be highlighted
    FPrintHeader : TPrintHeaderInfo;

    function GetActiveData : TAmlData; virtual;
    procedure SetAddress(const AAddress : TAddress); virtual;
    procedure GlobalSetAddress(const AAddress : String); virtual;
    procedure HoverOverAddress(const AAddress : String); virtual;

    procedure SetupHtmlViewerDefaults(const AViewer : THtmlViewer); virtual;

    procedure DefaultHtmlHotSpotCovered(Sender: TObject; const SRC : String); virtual;
    procedure DefaultHtmlHotSpotClick(Sender: TObject; const SRC: String; var Handled: Boolean); virtual;
    procedure InternalHtmlHotSpotClick(Sender: TObject; const APrimarySrc, ASecondarySrc : String); virtual;
    procedure InternalHtmlFootnoteCovered(Sender : TObject; const ANoteId : String); virtual;
    procedure InternalHtmlFootnoteClick(Sender: TObject; const ANoteId : String); virtual;
    procedure DefaultHtmlImageRequest(Sender: TObject; const SRC: String; var Stream: TMemoryStream); virtual;
    procedure DefaultHtmlRightClick(Sender: TObject; Parameters: TRightClickParameters); virtual;
    procedure InternalHtmlRightClick(Sender: TObject; Parameters: TRightClickParameters); virtual;
    procedure DefaultHtmlPrintHeader(Sender: TObject; Canvas: TCanvas; NumPage, W, H: Integer; var StopPrinting: Boolean); virtual;

    procedure PopupContextMenu(const ABookmarks : TBookmarks; const ATitle : String); overload; virtual;
    procedure PopupContextMenu; overload; virtual;
    function AddContextMenuItem(const ACaption: string; ATag : LongInt = 0; AChecked : Boolean = False; AParent : TMenu = Nil): TMenuItem;
    procedure DefaultContextMenuClick(Sender : TObject); virtual;
    procedure ContextMenuCommand(const ATag : LongInt); virtual;
    procedure DefaultContextMenuItemParams(Sender: TMenu; Item: TMenuItem;
                                           State: TMenuOwnerDrawState; AFont: TFont; var Color: TColor;
                                           var Graphic: TGraphic; var NumGlyphs: Integer); virtual;

    procedure PageNext(Sender : TObject);
    procedure PagePrev(Sender : TObject);
    procedure ItemNext(Sender : TObject);
    procedure ItemPrev(Sender : TObject);

    function GetEnableCopy : Boolean; virtual;
    function GetEnablePrint : Boolean; virtual;

    procedure FillQuickLinksWithQurans(const AActiveCaption : String; const AActiveSura, AActiveAyah : Word; const AColor : TColor);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure Customize; virtual;
    procedure ExecuteQuranCmd(const ACommand : TQuranCommand); virtual;
    procedure Navigate(const AType : TNavigate; const ADirection : TNavigateDirection; const AAddress : TCaptionedAddress = Nil); virtual;
    function SelectSura(const ASura : TSuraNum) : Boolean; virtual;

    property Address : TAddress write SetAddress;
    property AddressMgr : IAddressManager read FAddrMgr write FAddrMgr;
    property DataMgr : TDataManager read FDataMgr write FDataMgr;
    property StatusDisplay : IStatusDisplay read FStatus write FStatus;
    property QuickLinks : TBookmarks read FQuickLinks;

    procedure Print(const ADialog : TPrintDialog); virtual;
    procedure CopyToClipboard; virtual;

    property EnableCopy : Boolean read GetEnableCopy;
    property EnablePrint : Boolean read GetEnablePrint;
  end;

const
  GenericQuranAyahId = 'QA';
  GenericQuranSuraId = 'QS';
  MultiQuranId = 'AMQ';
  ArabicQuranPagesId = 'QAP';
  ArabicQuranTextId = 'QAT';
  DefaultSuraIntroId = 'QSI';
  DefaultReciteId = 'recite';
  DefaultCrossRefId = 'crossref';
  QuranStructType = 'quran_struct';
  MediaPlayerId = 'mediaplayer';

  DefaultIconNone = -1;
  DefaultBookIconLarge = -1;
  DefaultIndexIconIdxSmall = 1;
  DefaultBookIconIdxSmallBlue = 12;
  DefaultBookIconIdxSmallBrown = 13;
  DefaultBookIconIdxSmallCyan = 14;
  DefaultBookIconIdxSmallGreen = 15;
  DefaultBookIconIdxSmallPurple = 16;
  DefaultBookIconIdxSmallRed = 17;
  DefaultBookIconIdxMultiBook = 19;
  DefaultPageIconIdxSmall = 11;
  DefaultBmkCatgIdxSmall = 23;
  DefaultBmkItemIdxSmall = 24;
  DefaultAudioImgIdxSmall = 28;
  DefaultNoteIdxSmall = 30;
  DefaultVideoImgIdxSmall = 31;

  BookIconSmallIdxQuran = DefaultBookIconIdxSmallBlue;
  BookIconSmallIdxQuranTranslation = DefaultBookIconIdxSmallGreen;
  BookIconSmallIdxQuranInfo = DefaultBookIconIdxSmallPurple;
  BookIconSmallIdxHadith = DefaultBookIconIdxSmallBrown;
  ImageSmallIdxQuranAraPage = 18;

  DefaultArticleIconLarge = -1;
  DefaultArticleIconSmall = 11;

type
  TQuranAddressTarget =
    (qatAny,
     qatMultiQuran,
     qatArabicImages,
     qatGeneric,
     qatSuraIntro,
     qatCrossRefSubj,
     qatCrossRef,
     qatReciteOne,
     qatReciteContinue);

var
  SharedViewers : TStringList;   // indexed by Class name, object shared viewer

resourcestring
  PrintHeaderAppName = 'The Alim';

function CreateQuranAddress(const ATarget : TQuranAddressTarget;
                            const ASura : TSuraNum;
                            const AAyah : TAyahNum = 1) : String;

function IsQuranAddress(const AAddress : String; ATarget : TQuranAddressTarget;
                        var Id : String; var Sura, Ayah : Word) : Boolean;
  {-sura/ayah can be 0, that's why they're words and not TAyahNum, TSuraNum}

procedure HighlightWords(var AStr : String; const ASearchHits : String);

function FootnoteToHtml(const AElement : TXmlElement; const AIncludeId : Boolean;
                        const ASearchHits : String) : String;

procedure PrintStandardHeader(HeaderInfo : TPrintHeaderInfo; Canvas: TCanvas; NumPage, W, H: Integer);

implementation

uses StrUtils, StStrL, StStrS, StBase, QuranView, StUtils;

function CreateQuranAddress(const ATarget : TQuranAddressTarget;
                            const ASura : TSuraNum;
                            const AAyah : TAyahNum) : String;
begin
  Result := '';
  case ATarget of
    qatMultiQuran : Result := Format('%s:%d.%d', [MultiQuranId, ASura, AAyah]);
    qatArabicImages : Result := Format('%s:%d.%d', [ArabicQuranPagesId, ASura, AAyah]);
    qatAny, qatGeneric : Result := Format('%s:%d.%d', [GenericQuranAyahId, ASura, AAyah]);
    qatSuraIntro : Result := Format('%s:%d', [DefaultSuraIntroId, ASura]);
    qatCrossRefSubj : Result := Format('%s:%s/%s:%d.%d', [DefaultCrossRefId, SpecialIdxInverseSubjects, GenericQuranAyahId, ASura, AAyah]);
    qatCrossRef : Result := Format('%s:%s/%s:%d.%d', [DefaultCrossRefId, SpecialIdxHRefs, GenericQuranAyahId, ASura, AAyah]);
    qatReciteOne : Result := Format('%s:%d.%d', [DefaultReciteId, ASura, AAyah]);
    qatReciteContinue : Result := Format('%s:%d.%d/+', [DefaultReciteId, ASura, AAyah]);
  end;
end;

function IsQuranAddress(const AAddress : String; ATarget : TQuranAddressTarget;
                        var Id : String; var Sura, Ayah : Word) : Boolean;
var
  P, D : Integer;
  Data : String;
  DataOk : Boolean;
begin
  Result := False;
  P := Pos(':', AAddress);
  if P > 0 then begin
    Id := Copy(AAddress, 1, P-1);
    Data := Copy(AAddress, P+1, Length(AAddress));
    D := Pos(SuraAyahStrDelim, Data);
    if D > 0 then begin
      DataOk := Str2WordS(Copy(Data, 1, D-1), Sura);
      DataOk := DataOk and Str2Words(Copy(Data, D+1, Length(Data)), Ayah);
    end else begin
      DataOk := Str2WordS(Data, Sura);
    end;
  end else begin
    Id := '';
    DataOk := False;
  end;

  case ATarget of
    qatAny : Result := True;
    qatMultiQuran : Result := CompareText(Id, MultiQuranId) = 0;
    qatArabicImages : Result := CompareText(Id, ArabicQuranPagesId) = 0;
    qatGeneric : Result := CompareText(Id, GenericQuranAyahId) = 0;
    qatSuraIntro : Result := CompareText(Id, DefaultSuraIntroId) = 0;
    qatCrossRefSubj, qatCrossRef : Result := CompareText(Id, DefaultCrossRefId) = 0;
    qatReciteOne, qatReciteContinue : Result := CompareText(Id, DefaultReciteId) = 0;
  end;
  Result := Result and DataOk;
end;

function Highlight(const S, Srch : string): string;
const
  ReplPrefix = '<font color="maroon" size=+1><b>';
  ReplSuffix = '</b></font>';
var
  I: Cardinal;
  Source: string;
  Found : Boolean;
begin
  Source := S;
  Result := '';
  repeat
    Found := SearchUC(PChar(Source)^, Length(Source), PChar(Srch)^, Length(Srch), I);
    if Found then begin
      Inc(I);
      Result := Result + Copy(Source, 1, I - 1) + ReplPrefix + Copy(Source, I, Length(Srch)) + ReplSuffix;
      Source := Copy(Source, I + Cardinal(Length(Srch)), MaxInt);
    end
    else Result := Result + Source;
  until not Found;
end;

procedure HighlightWords(var AStr : String; const ASearchHits : String);
var
  HighlightWords : TStringList;
  HighlIdx : Integer;
  HighlWord : String;
begin
  if ASearchHits <> '' then begin
    HighlightWords := TStringList.Create;
    HighlightWords.Sorted := True;
    HighlightWords.Duplicates := dupIgnore;
    HighlightWords.CommaText := ASearchHits;

    for HighlIdx := 0 to HighlightWords.Count-1 do begin
      HighlWord := HighlightWords[HighlIdx];
      AStr := Highlight(AStr, HighlWord);
    end;

    HighlightWords.Free;
  end;
end;

function AmlToHtml(const AData : TAmlData;
                   const AElement : TXmlElement;
                   const ASearchHits : String) : String;
const
  Sect1Fmt =
  '<tr><td colspan=2></td></tr><tr valign=top><td><font color=navy><b><a name="Sect1_%2:d">%0:s</a></b></font></td><td>%1:s</td></tr>';
  Sect2Fmt =
  '<font size="+1" color=green><b>%s</b></font><hr size=1 width="100%%" color=silver noshade>%s';
  Sect3Fmt =
  '<font color=green><b>%s</b></font><br>%s';
var
  DataMgr : TDataManager;
  MultimediaRows : String;

  function Convert(AParentElem : TXmlElement; Depth : Cardinal = 0) : String;
  var
    E, ELen, SectIdx, ImgIdx, CartAttrIdx : Cardinal;
    ChildElem : TXmlNode;
    SectHead, SectHTML, MediaType, MediaSrc, BookId, AyahText,
      MediaStartPos, MediaEndPos, MediaCaption, MediaNarr,
      Anchor, CartCaption, CartImageSrc, CartData : String;
    Sura : TSuraNum;
    AIdx, Ayah, LastAyah : TAyahNum;
    TableStarted : Boolean;
    GenericData : TAmlData;
  begin
    Result := '';
    ELen := AParentElem.ChildNodes.Length;
    if ELen <= 0 then
      Exit;

    TableStarted := False;
    Dec(ELen);
    SectIdx := 0;
    for E := 0 to ELen do begin
      ChildElem := AParentElem.ChildNodes.Item(E);
      if (ChildElem.NodeName = 'section') or (ChildElem.NodeName = 'year') then begin
        if ChildElem.NodeName = 'year' then
          SectHead := (ChildElem as TXmlElement).GetAttribute('num')
        else
          SectHead := (ChildElem as TXmlElement).GetAttribute('heading');
        SectHTML := Convert(ChildElem as TXmlElement, Succ(Depth));
        if Depth = 0 then begin
          if not TableStarted then begin
            Result := Result + '<table>';
            TableStarted := True;
          end;

          Result := Result + Format(Sect1Fmt, [SectHead, SectHTML, SectIdx]);
        end else if Depth = 1 then
          Result := Result + Format(Sect2Fmt, [SectHead, SectHTML, SectIdx])
        else
          Result := Result + Format(Sect3Fmt, [SectHead, SectHTML, SectIdx]);
        Inc(SectIdx);
      end else if (ChildElem.NodeName = 'ayah') and (DataMgr <> Nil) then begin
        BookId := GetElemAttr(ChildElem as TXmlElement, 'id', ArabicQuranTextId);
        GenericData := DataMgr.DataIds[BookId];
        if GenericData is TQuranData then begin
          Sura := GetElemAttr(ChildElem as TXmlElement, 'sura', 1);
          Ayah := GetElemAttr(ChildElem as TXmlElement, 'num', 1);
          AyahText := (GenericData as TQuranData).ReadAyahText(Sura, Ayah);
          if (GenericData as TQuranData).FlagIsSet(qdfIsArabic) then
            Anchor := Format('<a href="%s">%d.%d</a><br><a href="%s"><img src="#18" border=0></a><a href="%s"><img src="#28" border=0></a>', [CreateQuranAddress(qatMultiQuran, Sura, Ayah), Sura, Ayah, CreateQuranAddress(qatArabicImages, Sura, Ayah), CreateQuranAddress(qatReciteOne, Sura, Ayah)])
          else
            Anchor := Format('<a href="%s">%d.%d</a>', [CreateQuranAddress(qatMultiQuran, Sura, Ayah), Sura, Ayah]);
          if (GenericData as TQuranData).FlagIsSet(qdfIsArabic) then
            Result := Result + Format('<p align=right><table cellspacing=15><tr valign=top><td align="right"><font face="%s" size=6>%s</font></td><td>%s</td></tr></table></p>', [AData.AraEncodings.DefaultFontName, Reverse(AyahText), Anchor])
          else
            Result := Result + Format('<table><tr valign=top><td>%s</td><td>%s</td></tr></table>', [Anchor, AyahText]);
        end;
      end else if (ChildElem.NodeName = 'ayahs') and (DataMgr <> Nil) then begin
        BookId := GetElemAttr(ChildElem as TXmlElement, 'id', ArabicQuranTextId);
        GenericData := DataMgr.DataIds[BookId];
        if GenericData is TQuranData then begin
          Sura := GetElemAttr(ChildElem as TXmlElement, 'sura', 1);
          Ayah := GetElemAttr(ChildElem as TXmlElement, 'first', 1);
          LastAyah := GetElemAttr(ChildElem as TXmlElement, 'last', 1);
          if LastAyah < Ayah then LastAyah := Ayah;
          Result := Result + '<p align=right><table cellspacing=15>';
          for AIdx := Ayah to LastAyah do begin
            AyahText := (GenericData as TQuranData).ReadAyahText(Sura, AIdx);
            if (GenericData as TQuranData).FlagIsSet(qdfIsArabic) then
              Anchor := Format('<a href="%s">%d.%d</a><br><a href="%s"><img src="#18" border=0></a><a href="%s"><img src="#28" border=0></a>', [CreateQuranAddress(qatMultiQuran, Sura, AIdx), Sura, AIdx, CreateQuranAddress(qatArabicImages, Sura, AIdx), CreateQuranAddress(qatReciteOne, Sura, AIdx)])
            else
              Anchor := Format('<a href="%s">%d.%d</a>', [CreateQuranAddress(qatMultiQuran, Sura, AIdx), Sura, AIdx]);
            if (GenericData as TQuranData).FlagIsSet(qdfIsArabic) then
              Result := Result + Format('<tr valign=top><td align="right"><font face="%s" size=6>%s</font></td><td>%s</td></tr></p>', [AData.AraEncodings.DefaultFontName, Reverse(AyahText), Anchor])
            else
              Result := Result + Format('<tr valign=top><td>%s</td><td>%s</td></tr>', [Anchor, AyahText]);
          end;
          Result := Result + '</table>';
        end;
      end else if (ChildElem.NodeName = 'media') then begin
        MediaType := (ChildElem as TXmlElement).GetAttribute('type');
        if MediaType = '' then MediaType := 'image';
        MediaSrc := (ChildElem as TXmlElement).GetAttribute('src');
        if CompareText(MediaType, 'image') = 0 then
          Result := Format('<center><img src="%s"></center>%s', [MediaSrc, Convert(ChildElem as TXmlElement, Depth)])
        else if (CompareText(MediaType, 'video') = 0) or (CompareText(MediaType, 'audio') = 0) then begin
          if CompareText(MediaType, 'video') = 0 then
            ImgIdx := DefaultVideoImgIdxSmall
          else
            ImgIdx := DefaultAudioImgIdxSmall;
          MediaCaption := (ChildElem as TXmlElement).GetAttribute('caption');
          MediaStartPos := (ChildElem as TXmlElement).GetAttribute('start');
          MediaEndPos := (ChildElem as TXmlElement).GetAttribute('finish');
          MediaNarr := (ChildElem as TXmlElement).GetAttribute('narration');
          if (MediaStartPos <> '') and (MediaEndPos <> '') then
            MultimediaRows := MultimediaRows + Format('<tr valign=top><td><img src="#%d"></td><td><font size=2 face="Arial"><a href="mediaplayer:%s/%s-%s">%s</a></font></td></tr>', [ImgIdx, MediaSrc, MediaStartPos, MediaEndPos, MediaCaption])
          else
            MultimediaRows := MultimediaRows + Format('<tr valign=top><td><img src="#%d"></td><td><font size=2 face="Arial"><a href="mediaplayer:%s">%s</a></font></td></tr>', [ImgIdx, MediaSrc, MediaCaption])
        end else
          Result := Format('<center><b>Unknown Media Type %s<b><p></center>%s', [MediaType, Convert(ChildElem as TXmlElement, Depth)]);
      end else if (ChildElem.NodeName = 'cart') then begin
        CartData := '';
        CartCaption := 'Add to Shopping Cart';
        CartImageSrc := '#32';
        if ChildElem.Attributes.Length > 0 then
          for CartAttrIdx := 0 to ChildElem.Attributes.Length - 1 do begin
            with ChildElem.Attributes.Item(CartAttrIdx) as TXmlAttribute do begin
              if Name = 'caption' then begin
                CartCaption := Value;
                continue;
              end;
              if Name = 'imagesrc' then begin
                CartImageSrc := Value;
                continue;
              end;
              if CartData <> '' then CartData := CartData + '|';
              CartData := CartData + Format('%s=%s', [Name, Value]);
            end;
          end;
        if (CartData <> '') and (CartImageSrc <> '') then
          Result := Result + Format('<a href="cart:%s"><img src="%s" border=0></a>', [CartData, CartImageSrc]);
        if (CartData <> '') and (CartCaption <> '') then
          Result := Result + Format(' <a href="cart:%s">%s</a>', [CartData, CartCaption]);
      end else if (ChildElem.NodeName <> 'index') then
        Result := Result + ' ' + ChildElem.XmlDocument;
    end;
    if TableStarted then
      Result := Result + '</table>';
  end;

begin
  Assert(AElement <> Nil);
  if AData <> Nil then
    DataMgr := AData.Owner
  else
    DataMgr := Nil;

  Result := '';
  MultimediaRows := '';
  if AElement.ChildNodes.Length > 0 then
    Result := Convert(AElement);

  if MultimediaRows <> '' then begin
     Result := Format('<table><tr valign=top><td><table>%s</table></td><td>%s</td></tr></table>', [MultimediaRows, Result]);
  end;

  HighlightWords(Result, ASearchHits);
end;

function FootnoteToHtml(const AElement : TXmlElement; const AIncludeId : Boolean;
                        const ASearchHits : String) : String;
const
  NoteDataFmt = '<table cellspacing=5><tr valign=top><td align=center><img src="%0:s" transp><br><font color="navy"><b>%1:s</b></font></td><td>%2:s</td></tr></table>';
var
  NoteId : String;
begin
  Assert(AElement <> Nil);

  if AIncludeId then begin
    NoteId := AElement.GetAttribute('id');
    Result := Format(NoteDataFmt, [Format('%s%d', [ImageFromImageListPrefix, DefaultNoteIdxSmall]), NoteId, AmlToHtml(Nil, AElement, ASearchHits)]);
  end else
    Result := AmlToHtml(Nil, AElement, ASearchHits);
end;

procedure PrintStandardHeader(HeaderInfo : TPrintHeaderInfo; Canvas: TCanvas; NumPage, W, H: Integer);
const
  StartX = 50;
  StartY = 40;
  LineHeight = 15;
var
  AFont: TFont;
begin
  AFont := TFont.Create;
  AFont.Name := 'Arial';
  AFont.Size := 8;
  with Canvas do begin
    Font.Assign(AFont);
    SetTextAlign(Handle, TA_Top or TA_Left);
    TextOut(StartX, StartY, HeaderInfo.AppTitle);
    TextOut(StartX, StartY+LineHeight, Format('Page %d', [NumPage]));
    SetTextAlign(Handle, TA_Top or TA_Right);
    Font.Style := [fsBold, fsUnderline];
    TextOut(W-StartX, StartY, HeaderInfo.BookName);
    Font.Style := [fsBold];
    TextOut(W-StartX, StartY+LineHeight, HeaderInfo.BookPage);
    Font.Style := [fsItalic];
    TextOut(W-StartX, StartY+(LineHeight*2), HeaderInfo.BookSection);
    MoveTo(StartX, StartY+(LineHeight*3));
    LineTo(W-StartX, StartY+(LineHeight*3));
  end;
  AFont.Free;
end;

{------------------------------------------------------------------------------}

procedure TCaptionedAddressList.AppendAddr(const ACaption, AAddress : String);
var
  CA : TCaptionedAddress;
begin
  CA := TCaptionedAddress.Create;
  CA.Caption := ACaption;
  CA.Address := AAddress;
  Add(CA);
end;

function TCaptionedAddressList.GetAddress(Index : Cardinal) : TCaptionedAddress;
begin
  Result := TCaptionedAddress(Items[Integer(Index)]);
end;

procedure TCaptionedAddressList.PrependAddr(const ACaption, AAddress : String);
var
  CA : TCaptionedAddress;
begin
  CA := TCaptionedAddress.Create;
  CA.Caption := ACaption;
  CA.Address := AAddress;
  Insert(0, CA);
end;

{------------------------------------------------------------------------------}

constructor TAmlData.Create(const AOwner : TDataManager);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TAmlData.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TAmlData.AsHtml(const AElement : TXmlElement; const ASearchHits : String) : String;
begin
  Result := AmlToHtml(Self, AElement, ASearchHits);
end;

function TAmlData.GetLibName : String;
begin
  Result := '_none';
end;

function TAmlData.CreateViewer : TAmlViewer;
begin
  Result := Nil;
end;

function TAmlData.GetViewer(const AShare : TViewerShare) : TAmlViewer;
var
  Share : Boolean;
  ViewerIdx : Integer;
begin
  case AShare of
    vsDefault : Share := FlagIsSet(adfShareViewer);
    vsShare   : Share := True;
    vsCreate  : Share := False;
  else
    Share := True;
  end;

  if FViewer <> Nil then
    Result := FViewer
  else begin
    if Share then begin
      if SharedViewers.Find(Self.ClassName, ViewerIdx) then begin
        FViewer := SharedViewers.Objects[ViewerIdx] as TAmlViewer;
      end else begin
        FViewer := CreateViewer;
        SharedViewers.AddObject(Self.ClassName, FViewer);
      end;
    end else begin
      FViewer := CreateViewer;
    end;
    Result := FViewer;
  end;
  if Result <> Nil then begin
    Result.AddressMgr := FAddrMgr;
    Result.DataMgr := FOwner;
    Result.StatusDisplay := FStatus;
  end;
end;

function TAmlData.GetContentTypeKey : String;
begin
  if FContentSubType <> '' then
    Result := FContentType + '_' + FContentSubType
  else
    Result := FContentType;
end;

function TAmlData.GetMainStorage : TRootStorage;
begin
  Result := FOwner.OpenRootStorage(FFileName);
end;

function TAmlData.GetAddressCaption(const AAddress : TAddress; const AIncludeName : Boolean) : String;
begin
  Result := AAddress.Address;
end;

function TAmlData.InitializeLoad(const AFileName : String;
                                 const ACatalog : TXmlElement) : Boolean;
var
  Permissions : TXmlElement;
  Links : TXmlNodeList;
begin
  // don't forget, TMultiQuranData does NOT call this inherited method
  // so if you change it, be sure to look at that overridden method

  Result := False;

  Close;
  if ACatalog <> Nil then begin
    FFileName := AFileName;
    FCatalog := ACatalog;
    FCatalogContent := ACatalog.FindElement('content');
    FCatalogNames := ACatalog.FindElement('names');
    FCatalogIndexes := ACatalog.FindElement('indexes');
    FCatalogLanguage := ACatalog.FindElement('language');
    FCatalogHome := ACatalog.FindElement('home');
    FCatalogCustom := ACatalog.FindElement('customize');

    if FCatalogLanguage <> Nil then
      FLanguageId := FCatalogLanguage.GetAttribute('id');

    FContentType := FCatalogContent.GetAttribute('type');
    FContentSubtype := FCatalogContent.GetAttribute('subtype');
    FId := FCatalog.GetAttribute('id');
    FName := FCatalogNames.GetAttribute('full');
    FShortName := FCatalogNames.GetAttribute('short');
    FGroupName := FCatalogNames.GetAttribute('group');

    FFonts := TObjDict.Create;
    FIndexes := TObjDict.Create;
    FIndexNames := TStringList.Create;
    FOpenConcordances := TObjDict.Create;

    if FCatalogIndexes <> Nil then begin
      SetFlag(adfHasFullTextIndex, FCatalogIndexes.FindElement('fulltext') <> Nil);
      SetFlag(adfHasInverseSubjIndex, FCatalogIndexes.FindElement('inversesubjects') <> Nil);
      SetFlag(adfHasHRefIndex, FCatalogIndexes.FindElement('hrefs') <> Nil);
      if FCatalogIndexes.FindElement('subjects') <> Nil then
        FIndexNames.Values['Subjects'] := 'SubjectsIndex';
    end;

    Permissions := FCatalog.FindElement('permissions');
    if Permissions <> Nil then begin
      SetFlag(adfCanCopyText, Permissions.GetAttribute('copy') <> 'no');
      SetFlag(adfCanPrintText, Permissions.GetAttribute('print') <> 'no');
    end else begin
      SetFlag(adfCanCopyText);
      SetFlag(adfCanPrintText);
    end;

    Links := FCatalog.GetElementsByTagNameWithAttribute('shortcuts', 'type', 'external');
    if Links.Length = 1 then
      FCatalogLinks[alExternal] := Links.Item(0) as TXmlElement;
    Links.Free;

    Links := FCatalog.GetElementsByTagNameWithAttribute('shortcuts', 'type', 'bookmarks');
    if Links.Length = 1 then
      FCatalogLinks[alBookmarks] := Links.Item(0) as TXmlElement;
    Links.Free;

    if FCatalogCustom = Nil then begin
      FCatalogCustom := ACatalog.OwnerDocument.CreateElement('customize');
      FCatalog.AppendChild(FCatalogCustom);
    end else begin

    end;

    Result := True;
  end;
end;

procedure TAmlData.FinalizeLoad;
begin
end;

procedure TAmlData.Close;
begin
  if FFonts <> Nil then begin
    FFonts.Free;
    FFonts := Nil;
  end;
  if FIndexes <> Nil then begin
    FIndexes.Free;
    FIndexes := Nil;
  end;
  if FOpenConcordances <> Nil then begin
    FOpenConcordances.Free;
    FOpenConcordances := Nil;
  end;

  FOwner.CloseRootStorage(FFileName);
  FId := '';
  FName := '';
  FShortName := '';
  FLanguageId := '';
  FGroupName := '';
  FillChar(FCatalogLinks, SizeOf(FCatalogLinks), 0);
  FCatalogContent := Nil;
  FCatalogIndexes := Nil;
  FCatalogNames := Nil;
  FCatalogLanguage := Nil;
  FCatalogHome := Nil;
  if FCatalog <> Nil then begin
    FCatalog.Free;
    FCatalog := Nil;
  end;
  if FIndexNames <> NIl then begin
    FIndexNames.Free;
    FIndexNames := Nil;
  end;

  FFlags := 0;
  SetupDefaultFlags;
end;

function TAmlData.OpenStream(const AName : String) : TStream;
begin
  Result := TStorageStream.Create(AName, GetMainStorage, amRead, False);
end;

function TAmlData.ReadStream(const AName : String) : TMemoryStream;
var
  StorageStream : TStream;
begin
  Result := TMemoryStream.Create;
  try
    StorageStream := OpenStream(AName);
  except
    Result.Free;
    Result := Nil;
    Exit;
  end;

  try
    Result.LoadFromStream(StorageStream);
  except
    Result.Free;
    Result := Nil;
  end;
  StorageStream.Free;
end;

function TAmlData.ReadImage(const Key : String) : TBitmap;
begin
  Result := FOwner.GetImage(GetMainStorage, FFileName, Key);
end;

function TAmlData.ReadStringList(const AKey : String) : TStringList;
var
  S : TStream;
begin
  try
    S := OpenStream(AKey);
    Result := TStringList.Create;
    Result.LoadFromStream(S);
    S.Free;
  except
    Result := Nil;
  end;
end;

function TAmlData.ReadString(const AKey : String) : String;
var
  S : TStream;
begin
  try
    S := OpenStream(AKey);
    SetLength(Result, S.Size);
    S.Read(PChar(Result)^, S.Size);
    S.Free;
  except
    Result := ''
  end;
end;

function TAmlData.ReadXML(const AKey : String) : TXmlElement;
var
  Xml : String;
begin
  Assert(FXmlParser <> Nil);

  Xml := ScrambleL(ReadString(AKey), AmlDataEncryptKey);
  if Xml <> '' then begin
    FXmlParser.LoadMemory(PChar(Xml));
    Result := FXmlParser.Document.DocumentElement.CloneNode as TXmlElement;
  end else
    Result := Nil;
end;

function TAmlData.OpenConcordance(const AName : String) : TStaticIndex;
var
  I : Integer;
  Store : TStorage;
begin
  if FOpenConcordances.Find(AName, I) then begin
    Result := FOpenConcordances.Objects[I] as TStaticIndex;
  end else begin
    Store := TStorage.Create(AName, MainStorage, amRead, tmDirect, False);
    Result := TStaticIndex.Create;
    Result.Storage := Store;
    FOpenConcordances.AddObject(AName, Result);
  end;
  Result.RefCount := Result.RefCount + 1;
end;

procedure TAmlData.CloseConcordance(const AName : String);
var
  I : Integer;
  Index : TStaticIndex;
begin
  if FOpenConcordances.Find(AName, I) then begin
    Index := FOpenConcordances.Objects[I] as TStaticIndex;
    Index.RefCount := Index.RefCount - 1;
    if Index.RefCount <= 0 then
      FOpenConcordances.Delete(I);
  end;
end;

procedure TAmlDataFont.AssignInHtmlViewer(const AHtmlView : THtmlViewer);
begin
  AHtmlView.DefFontName := FName;
  AHtmlView.DefFontSize := FPointSize;
  AHtmlView.DefFontColor := FColor;
end;

function TAmlDataFont.GetHtmlColor : String;
begin
  Result := ColorToHtmlColor(FColor);
end;

procedure TAmlData.SetFlag(const AFlag : LongInt; const AValue : Boolean);
begin
  if AValue then
    StUtils.SetLongFlag(FFlags, AFlag)
  else
    StUtils.ClearLongFlag(FFlags, AFlag);
end;

procedure TAmlData.ClearFlag(const AFlag : LongInt);
begin
  StUtils.ClearLongFlag(FFlags, AFlag);
end;

function TAmlData.FlagIsSet(const AFlag : LongInt) : Boolean;
begin
  Result := StUtils.LongFlagIsSet(FFlags, AFlag);
end;

function TAmlData.ChangeFont(const AFontId : String; const AOnlyColor : Boolean = False) : Boolean;
var
  F : TAmlDataFont;
  FontDlg : TFontDialog;
  ColorDlg : TColorDialog;
begin
  Result := False;

  F := GetFont(AFontId);
  if AOnlyColor then begin
    ColorDlg := TColorDialog.Create(Application);
    ColorDlg.Color := F.Color;
    try
      if ColorDlg.Execute then begin
        F.Color := ColorDlg.Color;
        Result := True;
      end;
    finally
      ColorDlg.Free;
    end;
  end else begin
    FontDlg := TFontDialog.Create(Application);
    FontDlg.Font.Name := F.Name;
    FontDlg.Font.Size := F.PointSize;
    FontDlg.Font.Color := F.Color;
    try
      if FontDlg.Execute then begin
        F.Name := FontDlg.Font.Name;
        F.PointSize := FontDlg.Font.Size;
        F.Color := FontDlg.Font.Color;
        Result := True;
      end;
    finally
      FontDlg.Free;
    end;
  end;
end;

function TAmlData.GetFont(const AFontId : String) : TAmlDataFont;
var
  I : Integer;
begin
  if FFonts.Find(AFontId, I) then
    Result := FFonts.Objects[I] as TAmlDataFont
  else begin
    Result := CreateDefaultFont(AFontId);
    FFonts.AddObject(AFontId, Result);
  end;
end;

procedure TAmlData.SetupDefaultFlags;
begin
end;

function TAmlData.CreateDefaultFont(const AFontId : String) : TAmlDataFont;
begin
  Result := TAmlDataFont.Create;
  if AFontId = 'outline' then begin
    Result.Name := 'Tahoma';
    Result.PointSize := 8;
    Result.HtmlSize := 2;
  end else begin
    Result.Name := 'Verdana';
    Result.PointSize := 9;
    Result.HtmlSize := 2;
  end;
  Result.Color := clBlack;
end;

function TAmlData.GetIndex(const AIndexName : String) : TPageIndex;
var
  I : Integer;
  IndexStorageName : String;
begin
  if FIndexes.Find(AIndexName, I) then begin
    Result := FIndexes.Objects[I] as TPageIndex;
    Exit;
  end;

  IndexStorageName := FIndexNames.Values[AIndexName];
  Result := TPageIndex.Create;
  try
    Result.Load(IndexStorageName, GetMainStorage);
    FIndexes.AddObject(AIndexName, Result);
  except
    Result.Free;
    Result := Nil;
  end;
end;

{------------------------------------------------------------------------------}

constructor TAddress.Create;
begin
  inherited Create;
  FVolDelim := AddrVolumeElemDelim;
  FPathDelim := AddrPathDelim;
  FSearchDelim := AddrSearchDelim;
end;

function TAddress.GetAddress : String;
begin
  Result := Format('%s:%s', [FBookId, FBookPage]);
  if FPath <> '' then
    Result := Result + FPathDelim + FPath;
  if FSubPath <> '' then
    Result := Result + FPathDelim + FSubPath;
  if FSearchHits <> '' then
    Result := Result + FSearchDelim + FSearchHits;
end;

procedure TAddress.SetAddress(const AAddress : String);
var
  P : Cardinal;
  TmpPage : String;
begin
  P := Pos(':', AAddress);
  if P > 0 then begin
    FBookId := Copy(AAddress, 1, P-1);
    FBookPage := Copy(AAddress, P+1, Length(AAddress));
  end else begin
    FBookId := '';
    FBookPage := AAddress;
  end;
  FAfterBookID := FBookPage;

  if (CompareText(FBookId, 'http') <> 0) and ((CompareText(FBookId, 'ftp') <> 0)) then begin
    FWebAddress := False;
    P := Pos(FSearchDelim, FBookPage);
    if P > 0 then begin
      TmpPage := FBookPage;
      FBookPage := Copy(TmpPage, 1, P-1);
      FSearchHits := Copy(TmpPage, P+1, Length(TmpPage));
    end else
      FSearchHits := '';

    P := Pos(FPathDelim, FBookPage);
    if P > 0 then begin
      TmpPage := FBookPage;
      FBookPage := Copy(TmpPage, 1, P-1);
      FPath := Copy(TmpPage, P+1, Length(TmpPage));
      P := Pos(FPathDelim, FPath);
      if P > 0 then begin
        FPath := Copy(FPath, 1, P-1);
        FSubPath := Copy(FPath, P+1, Length(FPath));
      end else
        FSubPath := '';
    end else begin
      FPath := '';
      FSubPath := '';
    end;
  end else
    FWebAddress := True;
end;

function TAddress.GetElement : String;
var
  Elem : String;
  VolDelimPos : Integer;
begin
  Elem := FBookPage;
  VolDelimPos := Pos(FVolDelim, Elem);
  if VolDelimPos > 0 then
    Result := Copy(Elem, VolDelimPos+1, 255)
  else
    Result := Elem;
  Result := TrimS(Result);
end;

procedure TAddress.SetElement(AValue : String);
var
  Vol : String;
begin
  if FVolDelim <> #0 then begin
    Vol := Volume;
    FBookPage := ' '+Vol + FVolDelim + AValue;
  end else
    FBookPage := ' '+AValue;
end;

function TAddress.GetVolume : String;
var
  Elem : String;
  VolDelimPos : Integer;
begin
  Elem := FBookPage;
  VolDelimPos := Pos(FVolDelim, FBookPage);
  if VolDelimPos > 0 then
    Result := TrimS(Copy(Elem, 1, VolDelimPos-1))
  else
    Result := '';
end;

procedure TAddress.SetVolume(AValue : String);
var
  Elem : String;
begin
  if FVolDelim <> #0 then begin
    Elem := Element;
    FBookPage := ' '+AValue + FVolDelim + Elem;
  end;
end;

{------------------------------------------------------------------------------}

//const
//  NoBookmarkCategory = '_none';

constructor TBookmarkNode.Create;
begin
  inherited Create;
  FChildren := TBookmarks.Create;
  FLargeIconIdx := -1;
  FSmallIconIdx := -1;
end;

destructor TBookmarkNode.Destroy;
begin
  FChildren.Free;
  if FOwnTheData then begin
    if FSmallIcon <> Nil then
      FSmallIcon.Free;
    if FLargeIcon <> Nil then
      FLargeIcon.Free;
  end;
  inherited Destroy;
end;

constructor TBookmarks.Create;
begin
  inherited Create;
  FNodes := TObjDict.Create;
  FNodes.OwnTheData := True;
end;

destructor TBookmarks.Destroy;
begin
  FNodes.Free;
  inherited Destroy;
end;

procedure TBookmarks.AddNode(const AAddress : TBookmarkNode);
begin
  Assert(AAddress <> Nil);
  FNodes.AddObject(AAddress.SortKey + AAddress.Caption, AAddress);
end;

function TBookmarks.AddCategory(const ACaption, ASortKey : String) : TBookmarkNode;
begin
  Result := Nil;
  if ACaption <> '' then begin
    Result := Find(ACaption);
    if Result = Nil then begin
      Result := TBookmarkNode.Create;
      Result.Caption := ACaption;
      Result.SortKey := ASortKey;
      AddNode(Result);
    end else
      Result.SortKey := ASortKey;
  end;
end;

function TBookmarks.AddChild(const AParent, AAddress, ACaption, ASortKey : String) : TBookmarkNode;
var
  ParentNode : TBookmarkNode;
begin
  Result := TBookmarkNode.Create;
  with Result do begin
    Address := AAddress;
    Caption := ACaption;
    if ASortKey = '' then
      SortKey := ACaption
    else
      SortKey := ASortKey;
  end;

  if AParent <> '' then begin
    ParentNode := Find(AParent);
    if ParentNode = Nil then begin
      ParentNode := TBookmarkNode.Create;
      ParentNode.Caption := AParent;
      AddNode(ParentNode);
    end;
    ParentNode.Children.AddNode(Result);
  end else begin
    //RealParent := NoBookmarkCategory;
    AddNode(Result);
  end;
end;

procedure TBookmarks.Clear;
begin
  FNodes.Clear;
end;

function TBookmarks.Find(const ACaption : String) : TBookmarkNode;
var
  I : Cardinal;
  Node : TBookmarkNode;
begin
  Result := Nil;
  if FNodes.Count <= 0 then
    Exit;

  for I := 0 to FNodes.Count-1 do begin
    Node := FNodes.Objects[I] as TBookmarkNode;
    Assert(Node <> Nil);
    if CompareText(Node.Caption, ACaption) = 0 then begin
      Result := Node;
      Exit;
    end;
  end;
end;

function TBookmarks.GetBookmark(Index : Cardinal) : TBookmarkNode;
begin
  Result := FNodes.Objects[Index] as TBookmarkNode;
end;

function TBookmarks.GetCount : Cardinal;
begin
  Result := FNodes.Count;
end;

procedure TBookmarks.Merge(const AXmlElement : TXmlElement);
type
  TSortKind = (skAlphaCaption, skPhysicalOrder);

  procedure ProcessNode(const ACategory : String;
                        const AParent : TBookmarks;
                        const ANode : TXmlElement;
                        const ADefaultSort : TSortKind);
  var
    I : Integer;
    ChildNode : TXmlNode;
    ChildElem : TXmlElement;
    NewNode : TBookmarkNode;
    Caption, SortKindS, SortKey : String;
    SortKind : TSortKind;
  begin
    if ANode.ChildNodes.Length <= 0 then
      Exit;

    SortKindS := ANode.GetAttribute('sort');
    SortKind := skAlphaCaption;
    if SortKindS = '' then
      SortKind := ADefaultSort
    else begin
      case FindString(SortKindS, ['caption', 'physical']) of
        -1, 0 : SortKind := skAlphaCaption;
        1 : SortKind := skPhysicalOrder;
      end;
    end;

    for I := 0 to ANode.ChildNodes.Length-1 do begin
      ChildNode := ANode.ChildNodes.Item(I) as TXmlNode;
      if (ChildNode.NodeType = ELEMENT_NODE) then begin
        ChildElem := ChildNode as TXmlElement;
        SortKey := ChildElem.GetAttribute('sortkey');
        if SortKind = skPhysicalOrder then
          SortKey := Format('%.5d', [I]);

        if (ChildElem.NodeName = 'bookmark') or (ChildElem.NodeName = 'shortcut') then begin
          Caption := ChildElem.GetAttribute('caption');
          NewNode := AParent.AddChild(ACategory, ChildElem.GetAttribute('href'), Caption, SortKey);
          NewNode.BreakBefore := ChildElem.GetAttribute('breakbefore') = 'yes';
          NewNode.BreakAfter := ChildElem.GetAttribute('break') = 'yes';
          NewNode.LargeIconIdx := GetElemAttr(ChildElem, 'iconidx', -1);
          NewNode.SmallIconIdx := GetElemAttr(ChildElem, 'smalliconidx', -1);
          NewNode.LargeIconSrc := GetElemAttr(ChildElem, 'iconsrc', '');
          NewNode.SmallIconSrc := GetElemAttr(ChildElem, 'smalliconsrc', '');
          NewNode.Shortcut := GetElemAttr(ChildElem, 'keyaccel', '');
        end else if ChildElem.NodeName = 'category' then begin
          Caption := ChildElem.GetAttribute('name');
          NewNode := AParent.AddCategory(Caption, SortKey);
          NewNode.BreakBefore := ChildElem.GetAttribute('breakbefore') = 'yes';
          NewNode.BreakAfter := ChildElem.GetAttribute('break') = 'yes';
          NewNode.LargeIconIdx := GetElemAttr(ChildElem, 'iconidx', -1);
          NewNode.SmallIconIdx := GetElemAttr(ChildElem, 'smalliconidx', -1);
          NewNode.LargeIconSrc := GetElemAttr(ChildElem, 'iconsrc', '');
          NewNode.SmallIconSrc := GetElemAttr(ChildElem, 'smalliconsrc', '');
          ProcessNode('', NewNode.Children, ChildElem, SortKind);
        end;
      end;
    end;
  end;

begin
  Assert(AXmlElement <> Nil);
  ProcessNode('', Self, AXmlElement, skAlphaCaption);
end;

procedure TBookmarks.Populate(const AParentMenu : TMenuItem; const AOnClick : TNotifyEvent;
                              const AImageList : TImageList;
                              const ADefCatgImgIdx, ADefBmkImgIdx : Integer;
                              const AMainAccelsUsed : String;
                              const ATagNodeMenuItem : TBookmarkNode;
                              const ATagNodeMenuItemValue : LongInt);
const
  AccelChar = '&';

  procedure AddNodes(const AParent : TMenuItem; const ANodes : TBookmarks; const AAccelsUsed : String);
  var
    Accelerators : ShortString;
    LastNodeIdx, NodeIdx,
    LastBreakBeforeIdx, LastBreakAfterIdx : Cardinal;

    function CreateAccel(const AName : String) : String;
    var
      C : Integer;
      Ch : Char;
    begin
      Result := AName;
      if Result = '' then Exit;
      for C := 1 to Length(AName) do begin
        Ch := UpCase(AName[C]);
        if not (Ch in ['A'..'Z', '0'..'9']) then
          continue;
        if Pos(Ch, Accelerators) = 0 then begin
          Insert(AccelChar, Result, C);
          Accelerators := Accelerators + Ch;
          Exit;
        end;
      end;
    end;

    procedure AddNode(const AParent : TMenuItem; const ANode : TBookmarkNode);
    var
      NewItem, NewBreak : TMenuItem;
    begin
      Assert(ANode <> Nil);

      // if we're not the first node and we didn't have a break right before us, add a break before
      if ANode.BreakBefore and (NodeIdx > 0) and (Pred(NodeIdx) <> LastBreakAfterIdx) then begin
        NewBreak := TMenuItem.Create(Application);
        NewBreak.Caption := '-';
        AParent.Add(NewBreak);
        LastBreakBeforeIdx := NodeIdx;
      end;

      NewItem := TMenuItem.Create(Application);
      NewItem.Caption := CreateAccel(ANode.Caption);
      NewItem.Tag := LongInt(ANode);
      NewItem.OnClick := AOnClick;
      if ANode.Shortcut <> '' then
        NewItem.Shortcut := TextToShortCut(ANode.Shortcut);
      if ANode = ATagNodeMenuItem then
        NewItem.Tag := ATagNodeMenuItemValue;

      if (AImageList <> Nil) and (ANode.SmallIcon <> Nil) then
        NewItem.ImageIndex := AImageList.AddMasked(ANode.SmallIcon, clWhite)
      else if ANode.SmallIconIdx >= 0 then
        NewItem.ImageIndex := ANode.SmallIconIdx
      else begin
        if ANode.Children.Count > 0 then
          NewItem.ImageIndex := ADefCatgImgIdx
        else
          NewItem.ImageIndex := ADefBmkImgIdx;
      end;
      AParent.Add(NewItem);

      if ANode.BreakAfter and (NodeIdx < LastNodeIdx) then begin
        NewBreak := TMenuItem.Create(Application);
        NewBreak.Caption := '-';
        AParent.Add(NewBreak);
        LastBreakAfterIdx := NodeIdx;
      end;

      AddNodes(NewItem, ANode.Children, '');
    end;

  begin
    Accelerators := Uppercase(AAccelsUsed);

    LastNodeIdx := ANodes.Count;
    if LastNodeIdx > 0 then begin
      Dec(LastNodeIdx);
      for NodeIdx := 0 to LastNodeIdx do
        AddNode(AParent, ANodes.Bookmarks[NodeIdx]);
    end;
  end;

begin
  AddNodes(AParentMenu, Self, AMainAccelsUsed);
end;

{------------------------------------------------------------------------------}

function TDataList.GetData(Index : Cardinal) : TAmlData;
begin
  Result := TAmlData(Items[Index]);
end;

{------------------------------------------------------------------------------}

destructor TRootStoreInfo.Destroy;
begin
  if FStorage <> Nil then
    FStorage.Free;
  inherited Destroy;
end;

procedure TRootStoreInfo.IncUsage;
begin
  Inc(FUsage);
end;

procedure TRootStoreInfo.DecUsage;
begin
  if FUsage > 1 then
    Dec(FUsage)
  else begin
    FStorage.Free;
    FStorage := Nil;
    FUsage := 0;
  end;
end;

procedure TRootStoreInfo.SetFileName(const AFileName : String);
begin
  if FStorage <> Nil then
    FStorage.Free;

  FFileName := AFileName;
  FStorage := TRootStorage.Create(AFileName, amRead, smDenyWrite, tmDirect, False);
  FUsage := 1;
end;

{------------------------------------------------------------------------------}

constructor TDataManager.Create;
begin
  inherited Create;
  FOwnTheData := True;

  FByIsQuran := TStringList.Create;
  FByIsQuran.Sorted := True;
  FByIsQuran.Duplicates := dupIgnore;

  FById := TStringList.Create;
  FById.Sorted := True;
  FById.Duplicates := dupIgnore;

  FByFName := TStringList.Create;
  FByFName.Sorted := True;
  FByFName.Duplicates := dupIgnore;

  FByLib := TObjDict.Create;
  FByLib.OwnTheData := True;

  FByType := TObjDict.Create;
  FByType.OwnTheData := True;

  FByIndex := TObjDict.Create;
  FByIndex.OwnTheData := True;

  FByGroup := TObjDict.Create;
  FByGroup.OwnTheData := True;

  FSpecialIndexes := TObjDict.Create;
  FSpecialIndexes.OwnTheData := True;
  FSpecialIndexes.AddObject(SpecialIdxFullText, TDataList.Create);
  FSpecialIndexes.AddObject(SpecialIdxInverseSubjects, TDataList.Create);
  FSpecialIndexes.AddObject(SpecialIdxHRefs, TDataList.Create);

  FImages := TObjDict.Create;
  FImages.OwnTheData := True;

  FRootStorages := TObjDict.Create;
  FRootStorages.Sorted := True;
  FRootStorages.Duplicates := dupError;
  FRootStorages.OwnTheData := True;
end;

destructor TDataManager.Destroy;
var
  I : Integer;
begin
  if (Count > 0) and FOwnTheData then begin
    for I := 0 to Count-1 do
      if Items[I] <> Nil then begin
        Data[I].Free;
        Items[I] := Nil;
      end;
  end;
  if FByIsQuran <> Nil then FByIsQuran.Free;
  if FImages <> Nil then FImages.Free;
  if FById <> Nil then FById.Free;
  if FByFName <> Nil then FByFName.Free;
  if FByLib <> Nil then FByLib.Free;
  if FByType <> Nil then FByType.Free;
  if FSpecialIndexes <> Nil then FSpecialIndexes.Free;  // *** need to free the CONTENTS, too (MemorySleuth)
  if FByIndex <> Nil then FByIndex.Free;
  if FRootStorages <> Nil then FRootStorages.Free;
  if FByGroup <> Nil then FByGroup.Free;

  FImages := Nil;
  FById := Nil;
  FByFName := Nil;
  FByLib := Nil;
  FByType := Nil;
  FSpecialIndexes := Nil;
  FByIndex := Nil;
  FRootStorages := Nil;
  FByIsQuran := Nil;
  FByGroup := Nil;

  inherited Destroy;
end;

procedure TDataManager.Clear;
var
  I : Integer;
begin
  if (Count > 0) and FOwnTheData then begin
    for I := 0 to Count-1 do
      if Items[I] <> Nil then begin
        Data[I].Free;
        Items[I] := Nil;
      end;
  end;

  if FByIsQuran <> Nil then FByIsQuran.Free;
  if FImages <> Nil then FImages.Free;
  if FById <> Nil then FById.Free;
  if FByFName <> Nil then FByFName.Free;
  if FByLib <> Nil then FByLib.Free;
  if FByType <> Nil then FByType.Free;
  if FSpecialIndexes <> Nil then FSpecialIndexes.Free;  // *** need to free the CONTENTS, too (MemorySleuth)
  if FByIndex <> Nil then FByIndex.Free;
  if FRootStorages <> Nil then FRootStorages.Free;
  if FByGroup <> Nil then FByGroup.Free;

  FByIsQuran := Nil;
  FImages := Nil;
  FById := Nil;
  FByFName := Nil;
  FByLib := Nil;
  FByType := Nil;
  FSpecialIndexes := Nil;
  FByIndex := Nil;
  FRootStorages := Nil;
  FByGroup := Nil;

  inherited Clear;
end;

procedure TDataManager.AddData(const AData : TAmlData);

  procedure AddToDict(const ADict : TObjDict; const AKey : String);
  var
    Idx : Integer;
    DataList : TDataList;
  begin
    if not ADict.Find(AKey, Idx) then begin
      DataList := TDataList.Create;
      DataList.Name := AKey;
      ADict.AddObject(AKey, DataList);
    end else
      DataList := ADict.Objects[Idx] as TDataList;
    DataList.Add(AData);
  end;

  procedure AddToBookmarks(const ADict : TObjDict; const AKey : String);
  var
    Idx : Integer;
    Bookmarks : TBookmarks;
  begin
    if not ADict.Find(AKey, Idx) then begin
      Bookmarks := TBookmarks.Create;
      Bookmarks.Id := AKey;
      ADict.AddObject(AKey, Bookmarks);
    end else
      Bookmarks := ADict.Objects[Idx] as TBookmarks;
    Bookmarks.AddChild('', AData.Id+':', AData.ShortName, AData.ShortName);
  end;

var
  I : Cardinal;
begin
  inherited Add(AData);

  FById.AddObject(AData.Id, AData);
  FByFName.AddObject(AData.FileName, AData);
  if AData.FlagIsSet(adfIsQuranData) then
    FByIsQuran.AddObject(AData.Name, AData);
  AddToDict(FByLib, AData.LibName);
  AddToDict(FByType, AData.ContentTypeKey);
  if AData.GroupName <> '' then
    AddToBookmarks(FByGroup, AData.GroupName);
  if (AData.IndexNames <> Nil) and (AData.IndexNames.Count > 0) then begin
    for I := 0 to AData.IndexNames.Count-1 do
      AddToDict(FByIndex, AData.IndexNames.Names[I]);
  end;

  if AData.FlagIsSet(adfHasFullTextIndex) then
    (FSpecialIndexes.Objects[FSpecialIndexes.IndexOf(SpecialIdxFullText)] as TDataList).Add(AData);
  if AData.FlagIsSet(adfHasInverseSubjIndex) then
    (FSpecialIndexes.Objects[FSpecialIndexes.IndexOf(SpecialIdxInverseSubjects)] as TDataList).Add(AData);
  if AData.FlagIsSet(adfHasHRefIndex) then
    (FSpecialIndexes.Objects[FSpecialIndexes.IndexOf(SpecialIdxHRefs)] as TDataList).Add(AData);
end;

function TDataManager.GetDataId(Index : String) : TAmlData;
var
  Idx : Integer;
begin
  Result := Nil;
  if FById.Find(Index, Idx) then
    Result := FById.Objects[Idx] as TAmlData;
end;

function TDataManager.GetDataFile(Index : String) : TAmlData;
var
  Idx : Integer;
begin
  Result := Nil;
  if FByFName.Find(Index, Idx) then
    Result := FByFName.Objects[Idx] as TAmlData;
end;

function TDataManager.GetDataType(Index : String) : TDataList;
var
  Idx : Integer;
begin
  Result := Nil;
  if FByType.Find(Index, Idx) then
    Result := FByType.Objects[Idx] as TDataList;
end;

function TDataManager.GetIndex(Index : String) : TDataList;
var
  Idx : Integer;
begin
  Result := Nil;
  if FByIndex.Find(Index, Idx) then
    Result := FByIndex.Objects[Idx] as TDataList;
end;

function TDataManager.GetSpecialIndex(Index : String) : TDataList;
var
  Idx : Integer;
begin
  Result := Nil;
  if FSpecialIndexes.Find(Index, Idx) then
    Result := FSpecialIndexes.Objects[Idx] as TDataList;
end;

function TDataManager.GetLibrary(Index : String) : TDataList;
var
  Idx : Integer;
begin
  Result := Nil;
  if FByLib.Find(Index, Idx) then
    Result := FByLib.Objects[Idx] as TDataList;
end;

function TDataManager.GetGroupMembers(Index : String) : TBookmarks;
var
  Idx : Integer;
begin
  Result := Nil;
  if FByGroup.Find(Index, Idx) then
    Result := FByGroup.Objects[Idx] as TBookmarks;
end;

function TDataManager.Get(const ALookup : TDataLookup; const AKey : String) : TAmlData;
var
  Idx : Integer;
  DataList : TDataList;
  Dict : TObjDict;
begin
  Result := Nil;
  case ALookup of
    dslDataId :
      if FById.Find(AKey, Idx) then
        Result := FById.Objects[Idx] as TAmlData;
    dslFileName :
      if FByFName.Find(AKey, Idx) then
        Result := FByFName.Objects[Idx] as TAmlData;
    dslDataType, dslLibrary :
      begin
        if ALookup = dslDataType then
          Dict := FByType
        else
          Dict := FByLib;
        if Dict.Find(AKey, Idx) then begin
          DataList := Dict.Objects[Idx] as TDataList;
          Assert(DataList <> Nil);
          if DataList.Count > 0 then
            Result := DataList[0];
        end;
      end;
  end;
end;

procedure TDataManager.FillAll(const ALookup : TDataLookup; const AKey : String; const AList : TDataList; const AClear : Boolean);
var
  Idx, I : Integer;
  Dict : TObjDict;
  DataList : TDataList;
begin
  if AClear then
    AList.Clear;

  case ALookup of
    dslDataId :
      if FById.Find(AKey, Idx) then
        AList.Add(FById.Objects[Idx] as TAmlData);
    dslFileName :
      if FByFName.Find(AKey, Idx) then
        AList.Add(FByFName.Objects[Idx] as TAmlData);
    dslDataType, dslLibrary :
      begin
        if ALookup = dslDataType then
          Dict := FByType
        else
          Dict := FByLib;
        if Dict.Find(AKey, Idx) then begin
          DataList := Dict.Objects[Idx] as TDataList;
          Assert(DataList <> Nil);
          if DataList.Count > 0 then begin
            for I := 0 to DataList.Count-1 do
              AList.Add(DataList.Items[I]);
          end;
        end;
      end;
  end;
end;

function TDataManager.GetAll(const ALookup : TDataLookup; const AKey : String) : TDataList;
begin
  Result := TDataList.Create;
  FillAll(ALookup, AKey, Result);
end;

function TDataManager.OpenRootStorage(const AFileName : String) : TRootStorage;
var
  I : Integer;
  RSI : TRootStoreInfo;
begin
  if FRootStorages.Find(AFileName, I) then
    RSI := FRootStorages.Objects[I] as TRootStoreInfo
  else begin
    RSI := TRootStoreInfo.Create;
    RSI.FileName := AFileName;
    FRootStorages.AddObject(AFileName, RSI);
  end;

  Result := RSI.Storage;
end;

procedure TDataManager.CloseRootStorage(const AFileName : String);
var
  I : Integer;
  RSI : TRootStoreInfo;
begin
  if FRootStorages.Find(AFileName, I) then begin
    RSI := FRootStorages.Objects[I] as TRootStoreInfo;
    RSI.DecUsage;
    if RSI.Usage = 0 then
      FRootStorages.Delete(I);
  end;
end;

function TDataManager.GetImageStream(const AFileName : String) : TMemoryStream;
var
  ImageIndex : Integer;
  ImageKey : String;
begin
  ImageKey := '**' + AFileName;
  if FImages.Find(ImageKey, ImageIndex) then
    Result := FImages.Objects[ImageIndex] as TMemoryStream
  else begin
    Result := TMemoryStream.Create;
    try
      Result.LoadFromFile(AFileName);
    except
      Result.Free;
      Result := Nil;
      ShowMessage(Format('Error reading Image %s.', [AFileName]));
    end;
    if Result <> Nil then
      FImages.AddObject(ImageKey, Result);
  end;
end;

function TDataManager.GetImageStream(const AStorage : TStorage; AStorageName, AStreamName : String) : TMemoryStream;
var
  ImageIndex : Integer;
  ImageKey : String;
  SrcStream : TStream;
begin
  ImageKey := '**' + AStorageName + '$' + AStreamName;
  if FImages.Find(ImageKey, ImageIndex) then
    Result := FImages.Objects[ImageIndex] as TMemoryStream
  else begin
    try
      SrcStream := TStorageStream.Create(AStreamName, AStorage, amRead, False);
    except
      Result := Nil;
      Exit;
    end;
    Result := TMemoryStream.Create;
    try
      Result.LoadFromStream(SrcStream);
    except
      Result.Free;
      Result := Nil;
      ShowMessage(Format('Error reading Image %s from %s.', [AStreamName, AStorageName]));
    end;
    if Result <> Nil then
      FImages.AddObject(ImageKey, Result);
    SrcStream.Free;
  end;
end;

function TDataManager.GetImageStream(const AImageListIdx : Integer; const ASmallImages : Boolean = True) : TMemoryStream;
var
  I : Integer;
  ImageKey : String;
  SrcBmp : TBitmap;
begin
  if FSmallImages <> Nil then begin
    ImageKey := '__**' + IntToStr(AImageListIdx);
    if FImages.Find(ImageKey, I) then
      Result := FImages.Objects[I] as TMemoryStream
    else begin
      SrcBmp := TBitmap.Create;
      FSmallImages.GetBitmap(AImageListIdx, SrcBmp);
      Result := TMemoryStream.Create;
      SrcBmp.SaveToStream(Result);
      SrcBmp.Free;
      FImages.AddObject(ImageKey, Result);
    end;
  end else
    Result := Nil;
end;

function TDataManager.GetImage(const AFileName : String) : TBitmap;
var
  I : Integer;
  Extn : ShortString;
  Gif : TGifImage;
begin
  if FImages.Find(AFileName, I) then
    Result := FImages.Objects[I] as TBitmap
  else begin
    Extn := JustExtensionS(AFileName);
    Result := TBitmap.Create;
    try
      if CompareText(Extn, 'gif') = 0 then begin
        Gif := TGifImage.Create;
        try
          Gif.LoadFromFile(AFileName);
          Result.Assign(Gif);
        finally
          Gif.Free;
        end;
      end else if CompareText(Extn, 'bmp') = 0 then
        Result.LoadFromFile(AFileName);
    except
      Result.Free;
      Result := Nil;
    end;
    if Result <> Nil then
      FImages.AddObject(AFileName, Result);
  end;
end;

function TDataManager.GetImage(const AStorage : TStorage; AStorageName, AStreamName : String) : TBitmap;
var
  I : Integer;
  Extn, ImageKey : ShortString;
  Gif : TGifImage;
  Stream : TStream;
begin
  ImageKey := AStorageName + '$' + AStreamName;
  if FImages.Find(ImageKey, I) then
    Result := FImages.Objects[I] as TBitmap
  else begin
    try
      Stream := TStorageStream.Create(AStreamName, AStorage, amRead, False);
    except
      Result := Nil;
      Exit;
    end;
    Extn := JustExtensionS(AStreamName);
    Result := TBitmap.Create;
    try
      if CompareText(Extn, 'gif') = 0 then begin
        Gif := TGifImage.Create;
        try
          Gif.LoadFromStream(Stream);
          Result.Assign(Gif);
        finally
          Gif.Free;
        end;
      end else if CompareText(Extn, 'bmp') = 0 then
        Result.LoadFromStream(Stream);
    except
      Result.Free;
      Result := Nil;
    end;
    Stream.Free;
    if Result <> Nil then
      FImages.AddObject(ImageKey, Result);
  end;
end;

function TDataManager.GetImage(const AImageListIdx : Integer; const ASmallImages : Boolean) : TBitmap;
var
  I : Integer;
  ImageKey : String;
begin
  if FSmallImages <> Nil then begin
    ImageKey := '__' + IntToStr(AImageListIdx);
    if FImages.Find(ImageKey, I) then
      Result := FImages.Objects[I] as TBitmap
    else begin
      Result := TBitmap.Create;
      FSmallImages.GetBitmap(AImageListIdx, Result);
      FImages.AddObject(ImageKey, Result);
    end;
  end else
    Result := Nil;
end;

procedure TDataManager.StoreToFile(const AFileName : String; const AEncryptKey : String);
var
  I : Integer;
  SingleData : TAmlData;
  AllData, SaveData : String;
  Stream : TFileStream;
begin
  AllData := '<?xml version="1.0"?>'#13'<datamanager>'#13;
  for I := 0 to FByFName.Count-1 do begin
    SingleData := FByFName.Objects[I] as TAmlData;
    if SingleData.Catalog <> Nil then
      AllData := AllData + SingleData.Catalog.XmlDocument + #13;
  end;
  AllData := AllData + '</datamanager>'#13;

  try
    Stream := TFileStream.Create(AFileName, fmCreate);
    if AEncryptKey <> '' then
      SaveData := ScrambleL(AllData, AEncryptKey)
    else
      SaveData := AllData;
    Stream.Write(PChar(SaveData)^, Length(SaveData));
    Stream.Free;
  except
    on E : Exception do
      ShowMessage('Error storing dataMgrCache: '+E.Message);
  end;
end;

procedure TDataManager.ShowAddrReferences(const ASpecialIndex : String; const AAddrMgr : IAddressManager; const AAddress : String);
type
  TSpecialAddrType = (satInverseSubject, satHRef);
var
  Addresses : TBookmarks;

  procedure SearchAddrIndex(const AType : TSpecialAddrType);
  var
    DataList : TDataList;
    Data : TAmlData;
    IndexName : String;
    Index : TStaticIndex;
    Locs : TEntryLocs;
    CA : TCaptionedAddress;
    I, L : Integer;
    Addr : TAddress;
  begin
    case AType of
      satInverseSubject : IndexName := SpecialIdxInverseSubjects;
      satHRef : IndexName := SpecialIdxHRefs;
    else
      IndexName := '';
    end;
    DataList := SpecialIndexes[IndexName];
    if (DataList = Nil) or (DataList.Count <= 0) then
      Exit;

    for I := 0 to DataList.Count-1 do begin
      Data := DataList[I];
      Index := TStaticIndex.Create;
      Index.Storage := TStorage.Create(IndexName, Data.MainStorage, amRead, tmDirect, False);
      Locs := Index.GetEntryLocs(AAddress);
      Index.Free;

      if Locs <> Nil then begin
        for L := 0 to Locs.Count-1 do begin
          CA := TCaptionedAddress.Create;
          case AType of
            satInverseSubject : begin
                CA.Caption := Locs[L] + ' (Subject)';
                CA.Address := Format('index:Subjects/%s', [Locs[L]]);
              end;
            satHRef : begin
                Addr := TAddress.Create;
                Addr.Address := Locs[L];
                CA.Caption := Data.GetAddressCaption(Addr, True);
                CA.Address := Locs[L];
                Addr.Free;
              end;
          end;
          Addresses.AddChild('', CA.Address, CA.Caption, Format('%05d', [L]));
        end;
        Locs.Free;
      end;
    end;
  end;

var
  Addr : TAddress;
begin
  Addr := TAddress.Create;
  Addr.Address := AAddress;

  Addresses := TBookmarks.Create;
  if ASpecialIndex = SpecialIdxInverseSubjects then begin
    SearchAddrIndex(satInverseSubject);
    AAddrMgr.ShowBookmarks(Addresses, AAddrMgr.GetAddressCaption(Addr, False)+' Subjects', True);
  end;
  if ASpecialIndex = SpecialIdxHRefs then begin
    SearchAddrIndex(satHRef);
    AAddrMgr.ShowBookmarks(Addresses, AAddrMgr.GetAddressCaption(Addr, False)+' References', True);
  end;

  Addr.Free;
end;

{------------------------------------------------------------------------------}

constructor TAmlViewer.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FQuickLinks := TBookmarks.Create;
  KeyPreview := True;
end;

destructor TAmlViewer.Destroy;
begin
  FQuickLinks.Free;
  if FContextMenuBmks <> Nil then
    FContextMenuBmks.Free;
  inherited Destroy;
end;

procedure TAmlViewer.GlobalSetAddress(const AAddress : String);
begin
  Assert(FAddrMgr <> Nil);
  FAddrMgr.SetAddress(AAddress);
end;

procedure TAmlViewer.HoverOverAddress(const AAddress : String);
var
  Addr : TAddress;
begin
  Assert(FStatus <> Nil);
  Assert(FAddrMgr <> Nil);

  if AAddress <> '' then begin
    Addr := TAddress.Create;
    Addr.Address := AAddress;
    FStatus.StatusBegin(FAddrMgr.GetAddressCaption(Addr, True), False);
    Addr.Free;
  end else
    FStatus.StatusEnd;
end;

procedure TAmlViewer.SetupHtmlViewerDefaults(const AViewer : THtmlViewer);
begin
  with AViewer do begin
    OnHotSpotCovered := DefaultHtmlHotSpotCovered;
    OnHotSpotClick := DefaultHtmlHotspotClick;
    OnImageRequest := DefaultHtmlImageRequest;
    OnRightClick := DefaultHtmlRightClick;
    OnPrintHeader := DefaultHtmlPrintHeader;
    PrintMarginTop := 3.5;
    htOptions := htOptions + [htOverLinksActive];
    DefoverLinkColor := clRed;
  end;

  with FPrintHeader do begin
    AppTitle := PrintHeaderAppName;
    BookName := '';
    BookPage := '';
    BookSection := '';
  end;
end;

procedure TAmlViewer.DefaultHtmlHotSpotCovered(Sender: TObject; const SRC : String);
var
  NoteId : String;
begin
  if (SRC <> '') then begin
    if CompareText(Copy(SRC, 1, Length(AddrFootnotePrefix)), AddrFootnotePrefix) = 0 then begin
      NoteId := Copy(SRC, Length(AddrFootnotePrefix)+1, Length(SRC));
      InternalHtmlFootnoteCovered(Sender, NoteId);
    end else if (SRC[1] <> AddrInternalHotspotPrefix) then
      HoverOverAddress(SRC);
  end else
    FStatus.StatusEnd;
end;

procedure TAmlViewer.DefaultHtmlHotSpotClick(Sender: TObject; const SRC: String; var Handled: Boolean);
var
  P : Cardinal;
  URL, Page, Sect, NoteId : String;
begin
  if SRC <> '' then begin
    if SRC[1] = AddrInternalHotspotPrefix then begin
      URL := Copy(SRC, 2, Length(SRC));
      P := Pos(AddrPathDelim, URL);
      if P > 0 then begin
        Page := Copy(URL, 1, P-1);
        Sect := Copy(URL, P+1, Length(SRC));
        InternalHtmlHotspotClick(Sender, Page, Sect);
        Handled := True;
      end else begin
        InternalHtmlHotspotClick(Sender, URL, '');
        Handled := True;
      end;
    end else if CompareText(Copy(SRC, 1, Length(AddrFootnotePrefix)), AddrFootnotePrefix) = 0 then begin
      NoteId := Copy(SRC, Length(AddrFootnotePrefix)+1, Length(SRC));
      InternalHtmlFootnoteClick(Sender, NoteId);
      Handled := True;
    end else begin
      GlobalSetAddress(SRC);
      Handled := True;
    end;
  end;
end;

procedure TAmlViewer.InternalHtmlFootnoteCovered(Sender: TObject; const ANoteId : String);
begin
  ShowMessage(Format('Internal Footnote %s Hover not handled', [ANoteId]));
end;

procedure TAmlViewer.InternalHtmlFootnoteClick(Sender: TObject; const ANoteId : String);
begin
  ShowMessage(Format('Internal Footnote %s Click not handled', [ANoteId]));
end;

procedure TAmlViewer.InternalHtmlHotSpotClick(Sender: TObject; const APrimarySrc, ASecondarySrc : String);
begin
  ShowMessage(Format('Internal Address %s Click not handled', [APrimarySrc]));
end;

procedure TAmlViewer.DefaultHtmlImageRequest(Sender: TObject; const SRC: String; var Stream: TMemoryStream);
var
  RealSrc : String;
  ImageIdx : Integer;
  ActiveData : TAmlData;
begin
  FStatus.StatusBegin('Loading image...', True);
  try
    if SRC <> '' then begin
      if SRC[1] in [ImageFromStorePrefix, ImageFromImageListPrefix] then
        RealSrc := Copy(SRC, 2, Length(SRC))
      else
        RealSrc := SRC;

      case SRC[1] of
        ImageFromStorePrefix :
          begin
            ActiveData := GetActiveData;
            if ActiveData <> Nil then
              Stream := FDataMgr.GetImageStream(ActiveData.GetMainStorage, ActiveData.FileName, RealSrc);
          end;
        ImageFromImageListPrefix :
          begin
            try
              ImageIdx := StrToInt(RealSrc);
              Stream := FDataMgr.GetImageStream(ImageIdx);
            except
              Stream := Nil;
            end;
          end;
      else
        Stream := FDataMgr.GetImageStream(RealSrc);
      end;
    end;
  finally
    FStatus.StatusEnd;
  end;
end;

procedure TAmlViewer.PopupContextMenu(const ABookmarks : TBookmarks; const ATitle : String);
var
  ActivePt : TPoint;
  TitleItem : TMenuItem;
begin
  if FContextMenu = Nil then
    FContextMenu := TRxPopupMenu.Create(Self)
  else
    ClearMenu(FContextMenu);
  FContextMenu.Style := msOwnerDraw;
  FContextMenu.OnGetItemParams := DefaultContextMenuItemParams;
  ABookmarks.Populate(FContextMenu.Items, DefaultContextMenuClick, Nil, -1, -1);
  if ATitle <> '' then begin
    TitleItem := TMenuItem.Create(Self);
    TitleItem.Caption := ATitle;
    TitleItem.Tag := -1;
    FContextMenu.Items.Insert(0, TitleItem);
  end;
  GetCursorPos(ActivePt);
  FContextMenu.Popup(ActivePt.X, ActivePt.Y);
end;

procedure TAmlViewer.PopupContextMenu;
var
  ActivePt : TPoint;
begin
  GetCursorPos(ActivePt);
  FContextMenu.Popup(ActivePt.X, ActivePt.Y);
end;

function TAmlViewer.AddContextMenuItem(const ACaption: string;
                                       ATag : LongInt = 0;
                                       AChecked : Boolean = False;
                                       AParent : TMenu = Nil): TMenuItem;
begin
  Result := TMenuItem.Create(Self);
  with Result do begin
    Caption := ACaption;
    OnClick := DefaultContextMenuClick;
    HelpContext := 0;
    Checked := AChecked;
    Enabled := True;
    Tag := ATag;
  end;
  if AParent = Nil then
    FContextMenu.Items.Add(Result)
  else
    AParent.Items.Add(Result);
end;

procedure TAmlViewer.DefaultContextMenuItemParams(Sender: TMenu; Item: TMenuItem;
                                                  State: TMenuOwnerDrawState; AFont: TFont; var Color: TColor;
                                                  var Graphic: TGraphic; var NumGlyphs: Integer);
begin
  if (Item.Tag = -1) then begin
    AFont.Style := AFont.Style + [fsBold];
    AFont.Color := clWhite;
    Color := clBlack;
  end;
end;

procedure TAmlViewer.DefaultContextMenuClick(Sender : TObject);
var
  MenuItem : TMenuItem;
  BmkNode : TBookmarkNode;
begin
  if Sender is TMenuItem then begin
    MenuItem := Sender as TMenuItem;
    if (MenuItem.Tag <> -1) and (MenuItem.Tag > 0) then begin
      BmkNode := TBookmarkNode(MenuItem.Tag);
      if BmkNode.Address <> '' then
        GlobalSetAddress(BmkNode.Address);
    end else
      ContextMenuCommand(MenuItem.Tag);
  end;
end;

procedure TAmlViewer.ContextMenuCommand(const ATag : LongInt);
begin
end;

procedure TAmlViewer.DefaultHtmlRightClick(Sender: TObject; Parameters: TRightClickParameters);

  function PopupAyahMenu : Boolean;
  var
    LinkData, OtherQuran : TAmlData;
    OtherQurans : TStringList;
    MatchId : String;
    Sura, Ayah, I : Word;
    ViewInNode : TBookmarkNode;

    function MakeAddr(const AAddrType : TQuranAddressTarget) : String;
    begin
      Result := CreateQuranAddress(AAddrType, Sura, Ayah);
    end;

  begin
    Result := False;
    if IsQuranAddress(Parameters.Url, qatAny, MatchId, Sura, Ayah) then begin
      if CompareText(MatchId, GenericQuranAyahId) <> 0 then begin
        LinkData := FDataMgr.DataIds[MatchId];
        if (LinkData = Nil) or (not LinkData.FlagIsSet(adfIsQuranData)) then
          Exit;
      end else
        LinkData := FDataMgr.DataIds[MultiQuranId];

      if LinkData = Nil then
        Exit;
      OtherQurans := FDataMgr.QuranDataList;

      if FContextMenuBmks <> Nil then
        FContextMenuBmks.Clear
      else
        FContextMenuBmks := TBookmarks.Create;
      with FContextMenuBmks do begin
        AddChild('', MakeAddr(qatReciteOne), 'Recite this Ayah', '01');
        AddChild('', MakeAddr(qatReciteContinue), 'Recite from this Ayah', '02');
        if OtherQurans.Count > 0 then begin
          ViewInNode := AddCategory('View in another Quran', '03');
          for I := 0 to OtherQurans.Count-1 do begin
            OtherQuran := OtherQurans.Objects[I] as TAmlData;
            ViewInNode.Children.AddChild('', Format('%s:%d.%d', [OtherQuran.Id, Sura, Ayah]), OtherQuran.Name, '03');
          end;
        end;
        AddChild('', MakeAddr(qatSuraIntro), 'View Sura Introduction', '04');
        AddChild('', MakeAddr(qatCrossRefSubj), 'Show Subjects Discussed in this Ayah', '05').BreakBefore := True;
        AddChild('', MakeAddr(qatCrossRef), 'Show References to this Ayah', '06');
      end;
      PopupContextMenu(FContextMenuBmks, Format('Sura %s Ayah %d', [LinkData.QuranStruct.SuraName[Sura], Ayah]));
      Result := True;
    end;
  end;

begin
  if not PopupAyahMenu then
    InternalHtmlRightClick(Sender, Parameters);
end;

procedure TAmlViewer.InternalHtmlRightClick(Sender: TObject; Parameters: TRightClickParameters);
begin
  // ignore by default
end;

procedure TAmlViewer.DefaultHtmlPrintHeader(Sender: TObject; Canvas: TCanvas; NumPage, W, H: Integer; var StopPrinting: Boolean);
begin
  PrintStandardHeader(FPrintHeader, Canvas, NumPage, W, H);
end;

procedure TAmlViewer.ExecuteQuranCmd(const ACommand : TQuranCommand);
begin
  case ACommand of
    qcGotoSura : GlobalSetAddress(GenericQuranAyahId+':');
  end;
end;

procedure TAmlViewer.Customize;
begin
  ShowMessage('This feature has not been implemented. Please contact technical support - code #7213');
end;

procedure TAmlViewer.Navigate(const AType : TNavigate; const ADirection : TNavigateDirection; const AAddress : TCaptionedAddress);
begin
  // ignore by default
end;

procedure TAmlViewer.FillQuickLinksWithQurans(const AActiveCaption : String; const AActiveSura, AActiveAyah : Word; const AColor : TColor);
var
  I : Integer;
  QuranData : TAmlData;
  QuranDataList : TStringList;
  QuickLinksId : String;
begin
  if (AActiveAyah > 0) then
    QuickLinksId := Format('QuranDataList%d_%d', [AActiveSura, AActiveAyah])
  else
    QuickLinksId := Format('QuranDataList%d_0', [AActiveSura]);

  if FQuickLinks.Id <> QuickLinksId then begin
    FQuickLinks.Clear;
    FQuickLinks.Id := QuickLinksId;
    QuranDataList := FDataMgr.QuranDataList;
    if QuranDataList.Count > 1 then begin
      for I := 0 to QuranDataList.Count-1 do begin
        QuranData := QuranDataList.Objects[I] as TAmlData;
        FQuickLinks.AddChild('', Format('%s:%d.%d', [QuranData.Id, AActiveSura, AActiveAyah]), QuranData.ShortName, QuranData.ShortName);
      end;
    end;
  end;
  FAddrMgr.SetQuickLinks('Quran', AColor, FQuickLinks, AActiveCaption, tpQuran);
end;

procedure TAmlViewer.PageNext(Sender : TObject);
var
  CA : TCaptionedAddress;
begin
  if Sender is TMenuItem then begin
    CA := TCaptionedAddress((Sender as TMenuItem).Tag);
    Navigate(nvPage, nvdCaptionedAddr, CA);
  end else begin
    Navigate(nvPage, nvdNext);
  end;
end;

procedure TAmlViewer.PagePrev(Sender : TObject);
var
  CA : TCaptionedAddress;
begin
  if Sender is TMenuItem then begin
    CA := TCaptionedAddress((Sender as TMenuItem).Tag);
    Navigate(nvPage, nvdCaptionedAddr, CA);
  end else begin
    Navigate(nvPage, nvdPrevious);
  end;
end;

procedure TAmlViewer.ItemNext(Sender : TObject);
var
  CA : TCaptionedAddress;
begin
  if Sender is TMenuItem then begin
    CA := TCaptionedAddress((Sender as TMenuItem).Tag);
    Navigate(nvItem, nvdCaptionedAddr, CA);
  end else begin
    Navigate(nvItem, nvdNext);
  end;
end;

procedure TAmlViewer.ItemPrev(Sender : TObject);
var
  CA : TCaptionedAddress;
begin
  if Sender is TMenuItem then begin
    CA := TCaptionedAddress((Sender as TMenuItem).Tag);
    Navigate(nvItem, nvdCaptionedAddr, CA);
  end else begin
    Navigate(nvItem, nvdPrevious);
  end;
end;

function TAmlViewer.GetActiveData : TAmlData;
begin
  ShowMessage('Active Data Requested, GetActiveData not overridden');
  Result := Nil;
end;

procedure TAmlViewer.SetAddress(const AAddress : TAddress);
begin
  // ignore by default
end;

function TAmlViewer.SelectSura(const ASura : TSuraNum) : Boolean;
begin
  Result := False;
end;

function TAmlViewer.GetEnableCopy : Boolean;
begin
  Result := False;
end;

function TAmlViewer.GetEnablePrint : Boolean;
begin
  Result := False;
end;

procedure TAmlViewer.Print(const ADialog : TPrintDialog);
begin
  // ignore by default
end;

procedure TAmlViewer.CopyToClipboard;
begin
  // ignore by default
end;

initialization
  SharedViewers := TStringList.Create;
  SharedViewers.Sorted := True;
  SharedViewers.Duplicates := dupError;
finalization
  SharedViewers.Free;
end.
