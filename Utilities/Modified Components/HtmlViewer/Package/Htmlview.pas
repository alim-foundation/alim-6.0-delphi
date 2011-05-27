{Version 7.01}
{*********************************************************}
{*                     HTMLVIEW.PAS                      *}
{*                Copyright (c) 1995-9 by                *}
{*                   L. David Baldwin                    *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$i htmlcons.inc}

unit Htmlview;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls, StdCtrls,
  {$ifdef Win32}
    vwPrint, MetafilePrinter,
  {$endif}
  HTMLUn2, Forms, Dialogs, ExtCtrls, ReadHTML, HTMLSubs, Printers, Menus,
  dsgnintf;

const
  wm_FormSubmit = wm_User+100;
  wm_MouseScroll = wm_User+102;

type
  THTMLBorderStyle = (htFocused, htNone, htSingle);
  TRightClickParameters = Class(TObject)
    URL, Target: string;
    Image: TImageObj;
    ImageX, ImageY: integer;
    ClickWord: string;   
    end;
  TRightClickEvent = procedure(Sender: TObject; Parameters: TRightClickParameters) of Object; 
  THotSpotEvent = procedure(Sender: TObject; const SRC: string) of Object;
  THotSpotClickEvent = procedure(Sender: TObject; const SRC: string;
                     var Handled: boolean) of Object;
  TProcessingEvent = procedure(Sender: TObject; ProcessingOn: boolean) of Object;
  TPagePrinted = procedure( Sender: TObject;
                              Canvas : TCanvas ;
                              NumPage, W, H: Integer ;
                              var StopPrinting : Boolean) of Object;
  {$ifdef Windows}   
  TPageEvent = procedure( Sender: TObject; NumPage: Integer ;
                              var StopPrinting : Boolean) of Object;
  {$endif}
  TImageClickEvent = procedure(Sender, Obj: TObject; Button: TMouseButton;
                       Shift: TShiftState; X, Y: Integer) of Object;   
  TImageOverEvent = procedure(Sender, Obj: TObject; Shift: TShiftState;
                       X, Y: Integer) of Object;
  TMetaRefreshType = procedure(Sender: TObject; Delay: integer; const URL: string) of Object;

  htOptionEnum = (htOverLinksActive,htNoLinkUnderline,htPrintTableBackground);
  ThtmlViewerOptions = set of htOptionEnum;

  TPaintPanel = class(TCustomPanel)
  private
    FOnPaint: TNotifyEvent;
    FViewer: TComponent;
    Canvas2: TCanvas;
    Sizing: boolean;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_EraseBkgnd;
    procedure WMLButtonDblClk(var Message: TWMMouse); message WM_LButtonDblClk; 
    procedure DoBackground(ACanvas: TCanvas; WmErase: boolean);
    constructor CreateIt(AOwner: TComponent; Viewer: TComponent);
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  public
    procedure Paint; override;    
  end;

{$ifdef Win32}
  T32ScrollBar = Class(TScrollBar)   {a 32 bit scrollbar}
  private
    FPosition: integer;
    FMin, FMax, FPage: integer;
    procedure SetPosition(Value: integer);
    procedure SetMin(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure CNVScroll(var Message: TWMVScroll); message CN_VSCROLL;
  public
    property Position: integer read FPosition write SetPosition;
    property Min: integer read FMin write SetMin;
    property Max: integer read FMax write SetMax;
    procedure SetParams(APosition, APage, AMin, AMax: Integer);
  end;
{$endif}

  TFileType = (HTMLType, TextType, ImgType, OtherType);

  THTMLViewer = class(TWinControl)
  protected
    { Private declarations }
    DontDraw: boolean;
    FTitle: PString;
    FURL: PString;
    FTarget: PString;
    FBase, FBaseEx: PString;
    FBaseTarget: PString;
    FCurrentFile: PString;
    FNameList: TStringList;
    FCurrentFileType: TFileType;
    FOnHotSpotCovered: THotSpotEvent;
    FOnHotSpotClick: THotSpotClickEvent;
    FOnBitmapRequest: TGetBitmapEvent;
    FOnImageRequest: TGetImageEvent;
    FOnScript: TScriptEvent;
    FOnFormSubmit: TFormSubmitEvent;
    FOnHistoryChange: TNotifyEvent;
    FOnProcessing: TProcessingEvent;
    FOnInclude: TIncludeType;
    FOnSoundRequest: TSoundType;
    FOnMeta: TMetaType;
    FOnMetaRefresh: TMetaRefreshType;
    FRefreshURL: string;
    FRefreshDelay: Integer;
    FOnRightClick: TRightClickEvent;
    FOnImageClick: TImageClickEvent;
    FOnImageOver: TImageOverEvent;
    FOnObjectClick: TObjectClickEvent;
    FHistory, FTitleHistory: TStrings;
    FPositionHistory: TFreeList;
    FHistoryIndex: integer;
    FHistoryMaxCount: integer;
    FFontName: PString;
    FPreFontName: PString;
    FFontColor: TColor;
    FHotSpotColor, FVisitedColor, FOverColor: TColor; 
    FVisitedMaxCount: integer;
    FBackGround: TColor;
    FFontSize: integer;
    FProcessing: boolean;
    FAction, FFormTarget, FEncType, FMethod: PString;
    FStringList: TStringList;
    FOldWidth: integer;
    FImageCacheCount: integer;
    FNoSelect: boolean;
    FScrollBars: TScrollStyle;
    FBorderStyle: THTMLBorderStyle;
    FDither: boolean;
    FCaretPos: LongInt;
    FOptions: ThtmlViewerOptions;   
    sbWidth: integer;
    ScrollWidth: integer;
    MaxVertical: LongInt;
    MouseScrolling: boolean;
    LeftButtonDown: boolean;
    Hiliting: boolean;
    FPrintMarginLeft,
    FPrintMarginRight,
    FPrintMarginTop,
    FPrintMarginBottom: double;
    {$ifdef Delphi3_4_CppBuilder3_4}  {Delphi 3, C++Builder 3, 4}
    FCharset: TFontCharset;
    {$endif}
    FOnPrintHeader, FOnPrintFooter: TPagePrinted;
    FPage: integer;
    FOnPageEvent: TPageEvent;   
    FOnMouseDouble: TMouseEvent;   
    HotSpotAction: boolean;
    FMarginHeight, FMarginWidth: integer;
    FServerRoot: string;
    FSectionList: TSectionList;
    FImageStream: TMemoryStream;
    FOnExpandName: TExpandNameEvent;

    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure ScrollTo(Y: LongInt);
    procedure Scroll(Sender: TObject; ScrollCode: TScrollCode;
           var ScrollPos: Integer);
    procedure Layout;
    procedure SetViewImages(Value: boolean);
    function GetViewImages: boolean;
    procedure SetColor(Value: TColor);
    function GetBase: string;
    procedure SetBase(Value: string);
    function GetBaseTarget: string;
    function GetFURL: string;
    function GetTitle: string;
    function GetCurrentFile: string;
    procedure SetBorderStyle(Value: THTMLBorderStyle);
    function GetPosition: LongInt;
    procedure SetPosition(Value: LongInt);
    function GetScrollPos: integer;
    procedure SetScrollPos(Value: integer);
    function GetScrollBarRange: integer;
    procedure SetHistoryIndex(Value: integer);
    function GetFontName: TFontName;
    procedure SetFontName(Value: TFontName);
    function GetPreFontName: TFontName;
    procedure SetPreFontName(Value: TFontName);
    procedure SetFontSize(Value: integer);
    procedure SetFontColor(Value: TColor);
    procedure SetHotSpotColor(Value: TColor);
    procedure SetActiveColor(Value: TColor);   
    procedure SetVisitedColor(Value: TColor);   
    procedure SetVisitedMaxCount(Value: integer);  
    procedure SetOnBitmapRequest(Handler: TGetBitmapEvent);
    procedure SetOnImageRequest(Handler: TGetImageEvent);
    procedure SetOnScript(Handler: TScriptEvent);
    procedure SetOnFormSubmit(Handler: TFormSubmitEvent);
    function GetOurPalette: HPalette;
    procedure SetOurPalette(Value: HPalette);
    procedure SetDither(Value: boolean);
    procedure SetCaretPos(Value: LongInt);
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure BackgroundChange(Sender: TObject);
    procedure SubmitForm(Sender: TObject; const Action, Target, EncType, Method: string;
                Results: TStringList);
    procedure SetImageCacheCount(Value: integer);
    procedure WMFormSubmit(var Message: TMessage); message WM_FormSubmit;
    procedure WMMouseScroll(var Message: TMessage); message WM_MouseScroll;
    procedure SetSelLength(Value: LongInt);
    function GetSelLength: LongInt;
    function GetSelText: string;
    procedure SetNoSelect(Value: boolean);
    procedure SetHistoryMaxCount(Value: integer);
    procedure DrawBorder;
    procedure DoHilite(X, Y: integer);
    procedure SetScrollBars(Value: TScrollStyle);
    procedure SetProcessing(Value: boolean);
    function GetTarget: String;
    {$ifdef Delphi3_4_CppBuilder3_4}  {Delphi 3, C++Builder 3, 4}
    procedure SetCharset(Value: TFontCharset);
    {$endif}
    function GetFormControlList: TList;
    function GetNameList: TStringList;
    function GetLinkList: TList;
    procedure SetMarginWidth(Value: integer);
    procedure SetMarginHeight(Value: integer);
    procedure SetServerRoot(Value: string);
    procedure SetOnObjectClick(Handler: TObjectClickEvent);
    procedure FormControlEnterEvent(Sender: TObject);
    procedure HandleMeta(Sender: TObject; const HttpEq, Name, Content: string);
    procedure SetOptions(Value: ThtmlViewerOptions);
    procedure DoImage(Sender: TObject; const SRC: string; var Stream: TMemoryStream);
    procedure SetOnExpandName(Handler: TExpandNameEvent); 
    function GetWordAtCursor(X, Y: LongInt; var St, En: LongInt;
                                            var AWord: string): boolean;
  protected
    { Protected declarations }
    PaintPanel: TPaintPanel;
    PaintBox: TPaintBox;
    {$ifdef Win32}
    VScrollBar: T32ScrollBar;
    {$else}
    VScrollBar: TScrollBar;
    {$endif}
    HScrollBar: TScrollBar;
    Sel1: LongInt;
    Visited: TStringList;     {visited URLs}  

    procedure DoLogic;
    procedure DoScrollBars;
    procedure SetupAndLogic;
    function GetURL(X, Y: integer; var UrlTarg: TUrlTarget;
             var FormControl: TImageFormControlObj): boolean;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function GetPalette: HPALETTE; override;
    procedure HTMLPaint(Sender: TObject); virtual;
    procedure HTMLMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure HTMLMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer); virtual;
    procedure HTMLMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure HTMLMouseDblClk(Message: TWMMouse);      
    procedure URLAction; virtual;
    function HotSpotClickHandled: boolean; dynamic;
    procedure LoadFile(const FileName: string; ft: TFileType);
    procedure PaintWindow(DC: HDC); override;
    procedure UpdateImageCache;
    procedure LoadTheStrings(Strings: TStrings; ft: TFileType);
    procedure AddVisitedLink(const S: string);
    procedure CheckVisitedLinks;

  public
    { Public declarations }
    FrameOwner: TObject;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function HTMLExpandFilename(const Filename: string): string; virtual;
    procedure LoadFromFile(const FileName: string);
    procedure LoadTextFile(const FileName: string);
    procedure LoadImageFile(const FileName: string);
    procedure LoadStrings(Strings: TStrings);
    procedure LoadTextStrings(Strings: TStrings);
    procedure LoadFromBuffer(Buffer: PChar; BufSize: LongInt);
    procedure LoadFromStream(AStream: TStream);
    procedure LoadStream(const URL: string; AStream: TMemoryStream; ft: TFileType);  
    procedure Print(FromPage, ToPage: integer);
    function NumPrinterPages: integer;
    {$ifdef Win32}
    function PrintPreview(MFPrinter: TMetaFilePrinter): integer;  
    {$endif}
    function PositionTo(Dest: string): boolean;
    function Find(const S: String; MatchCase: boolean): boolean;
    procedure Clear; virtual;
    procedure CopyToClipboard;
    procedure SelectAll;
    procedure ClearHistory;
    procedure Reload;
    procedure BumpHistory(const FileName, Title: string;
                 OldPos: LongInt; ft: TFileType);
    function GetSelTextBuf(Buffer: PChar; BufSize: LongInt): LongInt;
    function InsertImage(const Src: string; Stream: TMemoryStream): boolean;
    procedure DoEnter; override;    
    procedure DoExit; override;

    property DocumentTitle: string read GetTitle;
    property URL: string read GetFURL;
    property Base: string read GetBase write SetBase;
    property BaseTarget: string read GetBaseTarget;
    property Position: LongInt read GetPosition write SetPosition;
    property VScrollBarPosition: integer read GetScrollPos write SetScrollPos;
    property VScrollBarRange: integer read GetScrollBarRange;
    property CurrentFile: string read GetCurrentFile;
    property History: TStrings read FHistory;
    property TitleHistory: TStrings read FTitleHistory;
    property HistoryIndex: integer read FHistoryIndex write SetHistoryIndex;
    property Processing: boolean read FProcessing;
    property SelLength: LongInt read GetSelLength write SetSelLength;
    property SelText: string read GetSelText;
    property Target: string read GetTarget;
    property Palette: HPalette read GetOurPalette write SetOurPalette;
    property Dither: boolean read FDither write SetDither default True;
    property CaretPos: LongInt read FCaretPos write SetCaretPos;
    property FormControlList: TList read GetFormControlList;
    property NameList: TStringList read GetNameList;
    property LinkList: TList read GetLinkList;
    property SectionList: TSectionList read FSectionList;
    property OnPageEvent: TPageEvent read FOnPageEvent write FOnPageEvent;
    property OnExpandName: TExpandNameEvent read FOnExpandName write SetOnExpandName;
    property VertPixels : LongInt read MaxVertical;

  published
    { Published declarations }
    property OnHotSpotCovered: THotSpotEvent read FOnHotSpotCovered
             write FOnHotSpotCovered;
    property OnHotSpotClick: THotSpotClickEvent read FOnHotSpotClick
             write FOnHotSpotClick;
    property OnBitmapRequest: TGetBitmapEvent read FOnBitmapRequest
             write SetOnBitmapRequest;
    property OnImageRequest: TGetImageEvent read FOnImageRequest
             write SetOnImageRequest;
    property OnScript: TScriptEvent read FOnScript
             write SetOnScript;
    property OnFormSubmit: TFormSubmitEvent read FOnFormSubmit
             write SetOnFormSubmit;
    property OnHistoryChange: TNotifyEvent read FOnHistoryChange
             write FOnHistoryChange;
    property ViewImages: boolean read GetViewImages write SetViewImages;
    property Enabled;
    property TabStop;
    property TabOrder;
    property Align;
    property Name;
    property Tag;
    property PopupMenu;
    property ShowHint;
    property Height default 150;
    property Width default 150;
    property DefBackground: TColor read FBackground write SetColor default clBtnFace;
    property BorderStyle: THTMLBorderStyle read FBorderStyle write SetBorderStyle;
    property Visible;
    property HistoryMaxCount: integer read FHistoryMaxCount write SetHistoryMaxCount;
    property DefFontName: TFontName read GetFontName write SetFontName;
    property DefPreFontName: TFontName read GetPreFontName write SetPreFontName;
    property DefFontSize: integer read FFontSize write SetFontSize default 12;
    property DefFontColor: TColor read FFontColor write SetFontColor
             default clBtnText;
    property DefHotSpotColor: TColor read FHotSpotColor write SetHotSpotColor
             default clBlue;
    property DefVisitedLinkColor: TColor read FVisitedColor write SetVisitedColor
             default clPurple;       
    property DefOverLinkColor: TColor read FOverColor write SetActiveColor
             default clBlue;        
    property VisitedMaxCount: integer read FVisitedMaxCount write SetVisitedMaxCount default 50;
    property ImageCacheCount: integer read FImageCacheCount
                write SetImageCacheCount default 5;
    property NoSelect: boolean read FNoSelect write SetNoSelect;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    {$ifdef Delphi3_4_CppBuilder3_4}  {Delphi 3, C++Builder 3, 4}
    property CharSet: TFontCharset read FCharSet write SetCharset;
    {$endif}
    property MarginHeight: integer read FMarginHeight write SetMarginHeight default 5;
    property MarginWidth: integer read FMarginWidth write SetMarginWidth default 10;
    property ServerRoot: string read FServerRoot write SetServerRoot;
    property PrintMarginLeft: double read FPrintMarginLeft write FPrintMarginLeft;
    property PrintMarginRight: double read FPrintMarginRight write FPrintMarginRight;
    property PrintMarginTop: double read FPrintMarginTop write FPrintMarginTop;
    property PrintMarginBottom: double read FPrintMarginBottom write FPrintMarginBottom;
    property htOptions: ThtmlViewerOptions read FOptions write SetOptions;

    property OnMouseMove;
    property OnMouseUp;
    property OnMouseDown;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnEnter;
    property OnProcessing: TProcessingEvent read FOnProcessing write FOnProcessing;
    property OnPrintHeader: TPagePrinted read FOnPrintHeader write FOnPrintHeader;
    property OnPrintFooter: TPagePrinted read FOnPrintFooter write FOnPrintFooter;
    property OnInclude: TIncludeType read FOnInclude write FOnInclude;
    property OnSoundRequest: TSoundType read FOnSoundRequest write FOnSoundRequest;
    property OnMeta: TMetaType read FOnMeta write FOnMeta;
    property OnMetaRefresh: TMetaRefreshType read FOnMetaRefresh write FOnMetaRefresh;
    property OnImageClick: TImageClickEvent read FOnImageClick write FOnImageClick;
    property OnImageOver: TImageOverEvent read FOnImageOver write FOnImageOver;
    property OnObjectClick: TObjectClickEvent read FOnObjectClick write SetOnObjectClick;
    property OnRightClick:  TRightClickEvent read FOnRightClick write FOnRightClick;
    property OnMouseDouble: TMouseEvent read FOnMouseDouble write FOnMouseDouble;
    end;

  THTMLEditor = class(TComponentEditor)
    function GetVerbCount: Integer; Override;
    function GetVerb(index: Integer): String; Override;
    procedure ExecuteVerb(index: Integer); Override;
    end;

procedure Register;

implementation

const
  MaxHScroll = 2000;  {max horizontal display in pixels}
  {$ifdef Win32}
  VScale = 1;
  {$else}
  VScale = 16;
  {$endif}

type
  PositionObj = class(TObject)
    Pos: LongInt;
    FileType: TFileType;
    end;

constructor THTMLViewer.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
  csSetCaption, csDoubleClicks]; 
Height := 150;
Width := 150;
FPrintMarginLeft := 2.0;
FPrintMarginRight := 2.0;
FPrintMarginTop := 2.0;
FPrintMarginBottom := 2.0;
{$ifdef Delphi3_4_CppBuilder3_4}  {Delphi 3, C++Builder 3, 4}
FCharset := DEFAULT_CHARSET;
{$endif}
FMarginHeight := 5;
FMarginWidth := 10;

PaintBox := TPaintBox.Create(Self);
PaintBox.Align := alClient;
InsertControl(PaintBox);

PaintPanel := TPaintPanel.CreateIt(Self, Self);
PaintPanel.ParentFont := False;    
InsertControl(PaintPanel);
PaintPanel.Top := 1;
PaintPanel.Left := 1;
Cursor := ThickIBeamCursor;
PaintPanel.Cursor := Cursor;

PaintPanel.OnPaint := HTMLPaint;
PaintPanel.OnMouseDown := HTMLMouseDown;
PaintPanel.OnMouseMove := HTMLMouseMove;
PaintPanel.OnMouseUp := HTMLMouseUp;

{$ifdef Win32}
  VScrollBar := T32ScrollBar.Create(Self);
  VScrollBar.Kind := sbVertical;
  VScrollBar.SmallChange := 16;
{$else}
  VScrollBar := TScrollBar.Create(Self);
  VScrollBar.Kind := sbVertical;
  VScrollBar.SmallChange := 1;
  VScrollBar.OnScroll := Scroll;
{$endif}
VScrollBar.Visible := False;
VScrollBar.TabStop := False;
sbWidth := VScrollBar.Width;
InsertControl(VScrollBar);

HScrollBar := TScrollBar.Create(Self);
HScrollBar.Kind := sbHorizontal;
HScrollBar.SmallChange := 15;
HScrollBar.OnScroll := Scroll;
HScrollBar.Visible := False;
HScrollBar.TabStop := False;
InsertControl(HScrollBar);

FScrollBars := ssBoth;

PaintBox.BringToFront;

FSectionList := TSectionList.Create(Self, PaintPanel);
FSectionList.ControlEnterEvent := FormControlEnterEvent;
FSectionList.OnBackgroundChange := BackgroundChange;

FNameList := TStringList.Create;
FNameList.Sorted := True;
DefBackground := clBtnFace;
DefFontColor := clBtnText;
DefHotSpotColor := clBlue;
DefOverLinkColor := clBlue;   
DefVisitedLinkColor := clPurple;   
FVisitedMaxCount := 50;   
DefFontSize := 12;
DefFontName := 'Times New Roman';
DefPreFontName := 'Courier New';
SetImageCacheCount(5);

FBase := NullStr;
FBaseEx := NullStr;
FBaseTarget := NullStr;
FCurrentFile := NullStr;
FTitle := NullStr;
FURL := NullStr;
FTarget := NullStr;

FHistory := TStringList.Create;
FPositionHistory := TFreeList.Create;
FTitleHistory := TStringList.Create;
FDither := True;

Visited := TStringList.Create;
end;

destructor ThtmlViewer.Destroy;
begin
FSectionList.Free;
FNameList.Free;
DisposeStr(FBase);
DisposeStr(FBaseEx);
DisposeStr(FBaseTarget);
DisposeStr(FCurrentFile);
DisposeStr(FTitle);
DisposeStr(FURL);
DisposeStr(FTarget);
DisposeStr(FFontName);
DisposeStr(FPreFontName);
FHistory.Free;
FPositionHistory.Free;
FTitleHistory.Free;
Visited.Free;    
inherited Destroy;
end;

procedure THtmlViewer.SetupAndLogic;
begin
AssignStr(FTitle, ReadHTML.Title);
if ReadHTML.Base <> '' then
  AssignStr(FBase, ReadHTML.Base)
else AssignStr(FBase, FBaseEx^);
AssignStr(FBaseTarget, ReadHTML.BaseTarget);
try
  DontDraw := True;
  {Load the background bitmap if any and if ViewImages set}
  FSectionList.GetBackgroundBitmap;

DoLogic;

finally
  DontDraw := False;
  end;
end;

procedure ThtmlViewer.LoadFile(const FileName: string; ft: TFileType);
var
  I: integer;
  Dest, FName, OldFile: string;
  SBuffer: string[255];
  OldCursor: TCursor;
begin
with Screen do
  begin
  OldCursor := Cursor;
  Cursor := crHourGlass;
  end;
{$ifdef Windows}
I :=
{$endif}
IOResult;   {eat up any pending errors}
FName := FileName;
I := Pos('#', FName);
if I > 0 then
  begin
  Dest := copy(FName, I+1, 255);  {positioning information}
  System.Delete(FName, I, 255);
  end
else Dest := '';
FRefreshDelay := 0;
try
  SetProcessing(True);
  if not FileExists(FName) then
    Raise(EInOutError.Create('Can''t locate file: '+FName));
  FSectionList.Clear;
  UpdateImageCache;
  FSectionList.SetFonts(FFontName^, FPreFontName^, FFontSize, FFontColor,
                        FHotSpotColor, FVisitedColor, FOverColor, FBackground,
                        htOverLinksActive in FOptions);
  FNameList.Clear;
  CaretPos := 0;
  Sel1 := -1;
  try
    OldFile := FCurrentFile^;  
    AssignStr(FCurrentFile, ExpandFileName(FName));  
    FCurrentFileType := ft;    
    if ft = HTMLType then
      begin
      if Assigned(FOnSoundRequest) then
        FOnSoundRequest(Self, '', 0, True);
      HtmlParseFile(FSectionList, FNameList, FName, FOnInclude, FOnSoundRequest, HandleMeta);
      end
    else if ft = TextType then
      HtmlParseTextFile(FSectionList, FNameList, FName)
    else
      begin
      SBuffer := '<img src="'+FName+'">';
      HTMLParseBuffer(FSectionList, FNameList, @SBuffer[1], Length(SBuffer), FOnInclude, Nil, Nil);
      end;
  finally
    SetupAndLogic;
    CheckVisitedLinks;     
    if Dest <> '' then
      PositionTo(Dest)   {change position, if applicable}
    else if FCurrentFile^ <> OldFile then
       begin
       ScrollTo(0);
       HScrollBar.Position := 0;
       end;
    {else if same file leave position alone}
    PaintPanel.Invalidate;
  end;
finally
  Screen.Cursor := OldCursor;
  SetProcessing(False);
  end;
if (FRefreshDelay > 0) and Assigned(FOnMetaRefresh) then
  FOnMetaRefresh(Self, FRefreshDelay, FRefreshURL);
end;

procedure ThtmlViewer.LoadFromFile(const FileName: string);
var
  OldFile, OldTitle: string;
  OldPos: LongInt;
  OldType: TFileType;
begin
if FProcessing then Exit;
if Filename <> '' then
  begin
  OldFile := FCurrentFile^;
  OldTitle := FTitle^;
  OldPos := Position;
  OldType := FCurrentFileType;
  LoadFile(FileName, HTMLType);
  if (OldFile <> FCurrentFile^) or (OldType <> FCurrentFileType) then
    BumpHistory(OldFile, OldTitle, OldPos, OldType);
  end;
end;

{----------------ThtmlViewer.LoadTextFile}
procedure ThtmlViewer.LoadTextFile(const FileName: string);
var
  OldFile, OldTitle: string;
  OldPos: LongInt;
  OldType: TFileType;
begin
if FProcessing then Exit;
if Filename <> '' then
  begin
  OldFile := FCurrentFile^;
  OldTitle := FTitle^;
  OldPos := Position;
  OldType := FCurrentFileType;
  LoadFile(FileName, TextType);
  if (OldFile <> FCurrentFile^) or (OldType <> FCurrentFileType) then
    BumpHistory(OldFile, OldTitle, OldPos, OldType);
  end;
end;

{----------------ThtmlViewer.LoadImageFile}
procedure ThtmlViewer.LoadImageFile(const FileName: string);
var
  OldFile, OldTitle: string;
  OldPos: LongInt;
  OldType: TFileType;

begin
if FProcessing then Exit;
if Filename <> '' then
  begin
  OldFile := FCurrentFile^;
  OldTitle := FTitle^;
  OldPos := Position;
  OldType := FCurrentFileType;
  LoadFile(FileName, ImgType);
  if (OldFile <> FCurrentFile^) or (OldType <> FCurrentFileType) then
    BumpHistory(OldFile, OldTitle, OldPos, OldType);
  end;
end;

procedure THtmlViewer.LoadTheStrings(Strings: TStrings; ft: TFileType);
begin
if FProcessing then Exit;
SetProcessing(True);
try
  FSectionList.Clear;
  UpdateImageCache;
  FSectionList.SetFonts(FFontName^, FPreFontName^, FFontSize, FFontColor,
                        FHotSpotColor, FVisitedColor, FOverColor, FBackground,
                        htOverLinksActive in FOptions);
  FNameList.Clear;
  FCaretPos := 0;
  Sel1 := -1;
  if ft = HTMLType then
    begin
    if Assigned(FOnSoundRequest) then   
      FOnSoundRequest(Self, '', 0, True);
    HtmlParseStrings(FSectionList, FNameList, Strings, FOnInclude, FOnSoundRequest, HandleMeta); 
    end
  else if ft = TextType then
    HtmlParseTextStrings(FSectionList, FNameList, Strings);
  SetupAndLogic;
  ScrollTo(0);
  HScrollBar.Position := 0;
  PaintPanel.Invalidate;
  AssignStr(FCurrentFile, '');
finally
  SetProcessing(False);
  end;
end;

{----------------THtmlViewer.LoadStrings}
procedure THtmlViewer.LoadStrings(Strings: TStrings);
begin
FRefreshDelay := 0;
LoadTheStrings(Strings, HTMLType);
if (FRefreshDelay > 0) and Assigned(FOnMetaRefresh) then
  FOnMetaRefresh(Self, FRefreshDelay, FRefreshURL);
end;

{----------------THtmlViewer.LoadTextStrings}
procedure THtmlViewer.LoadTextStrings(Strings: TStrings);
begin
LoadTheStrings(Strings, TextType);
end;

{----------------ThtmlViewer.LoadFromBuffer}
procedure ThtmlViewer.LoadFromBuffer(Buffer: PChar; BufSize: LongInt);
begin
if FProcessing then Exit;
SetProcessing(True);
FRefreshDelay := 0;
try
  FSectionList.Clear;
  UpdateImageCache;
  FSectionList.SetFonts(FFontName^, FPreFontName^, FFontSize, FFontColor,
                        FHotSpotColor, FVisitedColor, FOverColor, FBackground,
                        htOverLinksActive in FOptions);
  FNameList.Clear;
  CaretPos := 0;
  Sel1 := -1;
  if Assigned(FOnSoundRequest) then   
    FOnSoundRequest(Self, '', 0, True);
  HtmlParseBuffer(FSectionList, FNameList, Buffer, BufSize, FOnInclude, FOnSoundRequest, HandleMeta); 
  SetupAndLogic;
  ScrollTo(0);
  HScrollBar.Position := 0;
  PaintPanel.Invalidate;
  AssignStr(FCurrentFile, '');
finally
  SetProcessing(False);
  end;
if (FRefreshDelay > 0) and Assigned(FOnMetaRefresh) then
  FOnMetaRefresh(Self, FRefreshDelay, FRefreshURL);
end;

{----------------ThtmlViewer.LoadFromStream}
procedure ThtmlViewer.LoadFromStream(AStream: TStream);
begin
if FProcessing then Exit;
SetProcessing(True);
FRefreshDelay := 0;
try
  FSectionList.Clear;
  UpdateImageCache;
  FSectionList.SetFonts(FFontName^, FPreFontName^, FFontSize, FFontColor,
                        FHotSpotColor, FVisitedColor, FOverColor, FBackground,
                        htOverLinksActive in FOptions);
  FNameList.Clear;
  CaretPos := 0;
  Sel1 := -1;
  if Assigned(FOnSoundRequest) then        
    FOnSoundRequest(Self, '', 0, True);
  HtmlParseStream(FSectionList, FNameList, AStream, FOnInclude, FOnSoundRequest, HandleMeta);
  SetupAndLogic;
  ScrollTo(0);
  HScrollBar.Position := 0;
  PaintPanel.Invalidate;
  AssignStr(FCurrentFile, '');
finally
  SetProcessing(False);
  end;
if (FRefreshDelay > 0) and Assigned(FOnMetaRefresh) then
  FOnMetaRefresh(Self, FRefreshDelay, FRefreshURL);
end;

procedure ThtmlViewer.DoImage(Sender: TObject; const SRC: string; var Stream: TMemoryStream);
begin
Stream := FImageStream;
end;

{----------------ThtmlViewer.LoadStream}
procedure ThtmlViewer.LoadStream(const URL: string; AStream: TMemoryStream; ft: TFileType);
var
  SaveOnImageRequest: TGetImageEvent;
  SBuffer: string;
begin
if FProcessing then Exit;
SetProcessing(True);
FRefreshDelay := 0;
try
  FSectionList.Clear;
  UpdateImageCache;
  FSectionList.SetFonts(FFontName^, FPreFontName^, FFontSize, FFontColor,
                        FHotSpotColor, FVisitedColor, FOverColor, FBackground,
                        htOverLinksActive in FOptions);
  FNameList.Clear;
  CaretPos := 0;
  Sel1 := -1;

  if ft = HTMLType then
    begin
    if Assigned(FOnSoundRequest) then
      FOnSoundRequest(Self, '', 0, True);
    HtmlParseStream(FSectionList, FNameList, AStream, FOnInclude, FOnSoundRequest, HandleMeta);
    SetupAndLogic;
    end
  else if ft = TextType then
    begin
    HtmlParseTextStream(FSectionList, FNameList, AStream);
    SetupAndLogic;
    end
  else
    begin
    SaveOnImageRequest := FOnImageRequest;
    SetOnImageRequest(DoImage);
    FImageStream := AStream;
    SBuffer := '<img src="'+URL+'">';
    try
      HTMLParseBuffer(FSectionList, FNameList, @SBuffer[1], Length(SBuffer), FOnInclude, Nil, Nil);
      SetupAndLogic;
    finally
      SetOnImageRequest(SaveOnImageRequest);
      end;
    end;
  ScrollTo(0);
  HScrollBar.Position := 0;
  PaintPanel.Invalidate;
  AssignStr(FCurrentFile, URL);
finally
  SetProcessing(False);
  end;
if (FRefreshDelay > 0) and Assigned(FOnMetaRefresh) then
  FOnMetaRefresh(Self, FRefreshDelay, FRefreshURL);
end;

{----------------ThtmlViewer.DoScrollBars}
procedure ThtmlViewer.DoScrollBars;
var
  VBar, HBar: boolean;
  Wid, HWidth, WFactor: integer;
{$ifdef Win32}
  ScrollInfo :TScrollInfo;
{$endif}

begin
VBar := False;
ScrollWidth := IntMin(ScrollWidth, MaxHScroll);
if FBorderStyle = htNone then
  begin
  WFactor := 0;
  PaintPanel.Top := 0;
  PaintPanel.Left := 0;
  PaintBox.Visible := False;
  end
else
  begin
  WFactor := 1;
  PaintPanel.Top := 1;
  PaintPanel.Left := 1;
  PaintBox.Visible := False;
  PaintBox.Visible := True; 
  end;
if FScrollBars in [ssBoth, ssVertical] then
  begin  {assume a vertical scrollbar}
  VBar := (MaxVertical >= Height-2) or
          ((FScrollBars in [ssBoth, ssHorizontal]) and
           (MaxVertical >= Height-2-sbWidth) and
           (ScrollWidth+2*FMarginWidth > Width-sbWidth));
  HBar := (FScrollBars in [ssBoth, ssHorizontal]) and
          ((ScrollWidth+2*FMarginWidth > Width) or
           (VBar and (ScrollWidth+2*FMarginWidth > Width-sbWidth)));
  end
else
  begin  {there is no vertical scrollbar}
  HBar := (FScrollBars in [ssBoth, ssHorizontal]) and
          (ScrollWidth+2*FMarginWidth > Width);
  end;
if VBar then
  Wid := Width - sbWidth
else
  Wid := Width;
PaintPanel.Width := Wid - 2*WFactor;
if HBar then
  PaintPanel.Height := Height - 2*WFactor - sbWidth
else
  PaintPanel.Height := Height - 2*WFactor;
HWidth := IntMax(ScrollWidth+2*FMarginWidth, Wid-2*WFactor);
HScrollBar.Visible := HBar;
HScrollBar.LargeChange := IntMax(1, Wid - 20);
HScrollBar.SetBounds(WFactor, Height-sbWidth-WFactor, Wid -WFactor, sbWidth);
VScrollBar.SetBounds(Width-sbWidth-WFactor, WFactor, sbWidth, Height - 2*WFactor);  
VScrollBar.LargeChange := PaintPanel.Height div VScale - VScrollBar.SmallChange;
VScrollBar.Visible := VBar;

{$ifndef Win32}
VScrollBar.SetParams(VScrollBar.Position, 0,
          IntMax(0, (MaxVertical-PaintPanel.Height) div VScale + 2));
HScrollBar.Max := IntMax(0, HWidth  - Wid);
{$else}
HScrollBar.Max := IntMax(0, HWidth);
VScrollBar.SetParams(VScrollBar.Position, PaintPanel.Height+1, 0, MaxVertical);
ScrollInfo.cbSize := SizeOf(ScrollInfo);
ScrollInfo.fMask := SIF_PAGE;
ScrollInfo.nPage := Wid;
SetScrollInfo(HScrollBar.Handle,SB_CTL,ScrollInfo,TRUE);
{$endif}
end;

{----------------ThtmlViewer.DoLogic}
procedure ThtmlViewer.DoLogic;
var
  Curs: LongInt;
  Wid, WFactor: integer;
begin
ScrollWidth := 0;
Curs := 0;
try

  DontDraw := True;
  if FBorderStyle = htNone then WFactor := 1   
    else WFactor := 3;
  if FScrollBars in [ssBoth, ssVertical] then
    begin  {assume a vertical scrollbar}
    Wid := Width - sbWidth - WFactor;   
    MaxVertical := FSectionList.DoLogic(PaintPanel.Canvas, FMarginHeight,
            Wid-2*FMarginWidth, ScrollWidth, Curs) + 2*FMarginHeight;
    DoScrollBars;
    end
  else
    begin  {there is no vertical scrollbar}
    Wid := Width-WFactor;
    MaxVertical := FSectionList.DoLogic(PaintPanel.Canvas, FMarginHeight,
            Wid-2*FMarginWidth, ScrollWidth, Curs) + FMarginHeight;
    DoScrollBars;
    end;
finally

  DontDraw := False;
  end;
end;

procedure ThtmlViewer.HTMLPaint(Sender: TObject);
var
  ARect: TRect;
begin
if not DontDraw then
  begin
  ARect := Rect(FMarginWidth, 1, PaintPanel.Width, PaintPanel.Height);
  FSectionList.Draw(PaintPanel.Canvas2, ARect, MaxHScroll,
                        FMarginWidth - HScrollBar.Position, FMarginHeight);
  end;
end;

procedure ThtmlViewer.WMSize(var Message: TWMSize);
begin
inherited;
if (Width <> FOldWidth) and not FProcessing then
  begin
  PaintPanel.Sizing := True;
  Layout;
  FOldWidth := Width;
  end
else
  DoScrollBars;
if MaxVertical < PaintPanel.Height then
  Position := 0
else ScrollTo(VScrollBar.Position * LongInt(VScale));   {keep aligned to limits}
with HScrollBar do
  {$ifdef Win32}
  Position := IntMin(Position, Max - PaintPanel.Width);
  {$else}
  Position := IntMin(Position, Max);
  {$endif}
end;

{$ifdef Win32}
procedure ThtmlViewer.Scroll(Sender: TObject; ScrollCode: TScrollCode;
       var ScrollPos: Integer);
{only the 32 bit horizontal scrollbar comes here}
begin
SetFocus;
ScrollPos := IntMin(ScrollPos, HScrollBar.Max - PaintPanel.Width);
PaintPanel.Invalidate;
end;

{$else}
procedure ThtmlViewer.Scroll(Sender: TObject; ScrollCode: TScrollCode;
       var ScrollPos: Integer);
begin
SetFocus;
if Sender = VScrollBar then
  FSectionList.SetYOffset(ScrollPos * LongInt(VScale));
PaintPanel.Invalidate;
end;
{$endif}

procedure ThtmlViewer.ScrollTo(Y: LongInt);
{$ifndef Win32}
var
  Tmp: LongInt;
{$endif}
begin
{$ifdef Win32}
  Y := IntMin(Y, MaxVertical - PaintPanel.Height);
  Y := IntMax(Y, 0);
  VScrollBar.Position := Y;
{$else}
  Tmp := VScrollBar.Max * LongInt(VScale);
  if Y > Tmp then
    Y := Tmp;
  if Y < 0 then Y := 0;
  VScrollBar.Position := Y div VScale;
{$endif}
FSectionList.SetYOffset(Y);
Invalidate;
end;

procedure ThtmlViewer.Layout;
var
  OldPos: LongInt;
begin
if FProcessing then Exit;
SetProcessing(True);
try
  OldPos := Position;
  DoLogic;
  Position := OldPos;   {return to old position after width change}
finally
  SetProcessing(False);
  end;
end;

function ThtmlViewer.HotSpotClickHandled: boolean;
var
  Handled: boolean;
begin
Handled := False;
if Assigned(FOnHotSpotClick) then
  FOnHotSpotClick(Self, URL, Handled);
Result := Handled;
end;

procedure ThtmlViewer.URLAction;
var
  S, Dest: string;
  Ext: string[5];
  I: integer;
  OldPos: LongInt;

begin
if not HotSpotClickHandled then
  begin
  OldPos := Position;
  S := URL;
  I := Pos('#', S);  {# indicates a position within the document}
  if I = 1 then
    begin
    if PositionTo(S) then    {no filename with this one}
      begin
      BumpHistory(FCurrentFile^, FTitle^, OldPos, FCurrentFileType);
      AddVisitedLink(FCurrentFile^+S);    
      end;
    end
  else
    begin
    if I >= 1 then
      begin
      Dest := System.Copy(S, I, 255);  {local destination}
      S := System.Copy(S, 1, I-1);     {the file name}
      end
    else
      Dest := '';    {no local destination}
    S := HTMLExpandFileName(S);
    Ext := Uppercase(ExtractFileExt(S));
{$ifdef Win32}
    if (Ext = '.HTM') or (Ext = '.HTML')  then
{$else}
    if Ext = '.HTM' then
{$endif}
      begin              {an html file}
      if S <> FCurrentFile^ then
        begin
        LoadFromFile(S + Dest);
        AddVisitedLink(S+Dest);
        end
      else
        if PositionTo(Dest) then   {file already loaded, change position}
          begin
          BumpHistory(FCurrentFile^, FTitle^, OldPos, HTMLType);
          AddVisitedLink(S+Dest);
          end;
      end
    else if (Ext = '.BMP') or (Ext = '.GIF') or (Ext = '.JPG') or (Ext = '.JPEG')
                or (Ext = '.PNG') then
      LoadImageFile(S);
    end;
    {Note: Self may not be valid here}
  end;
end;

{----------------ThtmlViewer.AddVisitedLink}
procedure ThtmlViewer.AddVisitedLink(const S: string);    
var
  I, J: integer;
  S1, UrlTmp: string;
begin
if Assigned(FrameOwner) or (FVisitedMaxCount = 0) then
  Exit;      {TFrameViewer will take care of visited links}
I := Visited.IndexOf(S);
if I = 0 then Exit
else if I < 0 then
  begin
  for J := 0 to SectionList.LinkList.Count-1 do
    with TFontObj(SectionList.LinkList[J]) do
      begin
      UrlTmp := Url;
      if Length(UrlTmp) > 0 then
        begin
        if Url[1] = '#' then
          S1 := FCurrentFile^+UrlTmp
        else
          S1 := HTMLExpandFilename(UrlTmp);
        if CompareText(S, S1) = 0 then
          Visited := True;
        end;
      end;
  end
else Visited.Delete(I);   {thus moving it to the top}
Visited.Insert(0, S);
for I :=  Visited.Count-1 downto FVisitedMaxCount do
  Visited.Delete(I);
end;

{----------------ThtmlViewer.CheckVisitedLinks}
procedure ThtmlViewer.CheckVisitedLinks;
var
  I, J: integer;
  S, S1: string;
begin
if FVisitedMaxCount = 0 then
  Exit;
for I := 0 to Visited.Count-1 do
  begin
  S := Visited[I];
  for J := 0 to SectionList.LinkList.Count-1 do
    with TFontObj(SectionList.LinkList[J]) do
      begin
      if Url[1] = '#' then
        S1 := FCurrentFile^+Url
      else
        S1 := HTMLExpandFilename(Url);
      if CompareText(S, S1) = 0 then
        Visited := True;
      end;
  end;
end;

procedure ThtmlViewer.HTMLMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  XR, CaretHt: integer;
  YR: LongInt;
  Cell1: TObject;
  InText: boolean;
begin
inherited MouseDown(Button, Shift, X, Y);

SetFocus;
HotSpotAction := False;
if (Button <> mbLeft) then Exit;
LeftButtonDown := True;
HiLiting := True;    
with FSectionList do
  begin
  Sel1 := FindCursor(PaintPanel.Canvas, X, Y+YOff-FMarginHeight, XR, YR, CaretHt, Cell1, InText);
  if Sel1 > -1 then
    begin
    if SelB <> SelE then
      InvalidateRect(PaintPanel.Handle, Nil, True);
    SelB := Sel1;
    SelE := Sel1;
    CaretPos := Sel1;
    end;
  end;
end;

procedure ThtmlViewer.HTMLMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  UrlTarget : TUrlTarget;
  Url, Target: string;
  FormControl: TImageFormControlObj;
  Obj: TObject;
  IX, IY: integer;
  XR, CaretHt: integer;
  YR: LongInt;
  Cell1: TObject;
  InText: boolean;
  NextCursor: TCursor;  
begin
Inherited MouseMove(Shift,X,Y);

UrlTarget := Nil;
URL := '';
NextCursor := crArrow;  
if GetURL(X, Y, UrlTarget, FormControl) then
  begin
  NextCursor := HandCursor;
  if not Assigned(FormControl) then
    begin
    Url := UrlTarget.Url^;
    Target := UrlTarget.Target^;
    UrlTarget.Free;
    end;
  end;
if (Assigned(FOnImageClick) or Assigned(FOnImageOver)) and      
     FSectionList.PtInObject(X, Y+FSectionList.YOff-FMarginHeight, Obj, IX, IY) then
  begin
  if NextCursor <> HandCursor then  {in case it's also a Link}  
    NextCursor := crArrow;
  if Assigned(FOnImageOver) then FOnImageOver(Self, Obj, Shift, IX, IY);
  end
else if (FSectionList.FindCursor(PaintPanel.Canvas, X, Y+FSectionList.YOff-FMarginHeight, XR, YR, CaretHt, Cell1, InText) >= 0)
          and InText and (NextCursor <> HandCursor) then
  NextCursor := Cursor;

PaintPanel.Cursor := NextCursor;
if NextCursor = HandCursor then   
  SetCapture(PaintPanel.Handle)
else
  ReleaseCapture;

if URL <> FURL^ then
  begin
  AssignStr(FURL, URL);
  AssignStr(FTarget, Target);
  if Assigned(FOnHotSpotCovered) then FOnHotSpotCovered(Self, URL);
  end;
if (ssLeft in Shift) and not MouseScrolling and ((Y <= 0) or (Y > Self.Height)) then
  begin
  MouseScrolling := True;
  PostMessage(Handle, wm_MouseScroll, 0, 0);
  end;
if (ssLeft in Shift) and not FNoSelect then
  DoHilite(X, Y);
inherited MouseMove(Shift, X, Y);
end;

procedure ThtmlViewer.HTMLMouseUp(Sender: TObject; Button: TMouseButton;
            Shift: TShiftState; X, Y: Integer);
var
  UrlTarget: TUrlTarget;
  FormControl: TImageFormControlObj;
  Obj: TObject;
  IX, IY: integer;
  InImage: boolean;
  Parameters: TRightClickParameters;
  AWord: string;
  St, En: LongInt; 
begin
inherited MouseUp(Button, Shift, X, Y);

if Assigned(FOnImageClick) or Assigned(FOnRightClick) then
  begin
  InImage := FSectionList.PtInObject(X, Y+FSectionList.YOff-FMarginHeight, Obj, IX, IY);
  if Assigned(FOnImageClick) and InImage then
    FOnImageClick(Self, Obj, Button, Shift, IX, IY);
  if (Button = mbRight) and Assigned(FOnRightClick) then
    begin
    Parameters := TRightClickParameters.Create;
    try
      if InImage then
        begin
        Parameters.Image := Obj as TImageObj;
        Parameters.ImageX := IX;
        Parameters.ImageY := IY;
        end;
      if GetURL(X, Y, UrlTarget, FormControl) and (UrlTarget <> Nil) then
        begin
        Parameters.URL := UrlTarget.URL^;
        Parameters.Target := UrlTarget.Target^;
        UrlTarget.Free;
        end;
      if GetWordAtCursor(X, Y, St, En, AWord) then
        Parameters.ClickWord := AWord;
      FOnRightClick(Self, Parameters);
    finally
      Parameters.Free;
      end;
    end;
  end;

if Button <> mbLeft then Exit;
DoHilite(X, Y);
Hiliting := False;
MouseScrolling := False;
if LeftButtonDown and (FSectionList.SelE <= FSectionList.SelB)
       and GetURL(X, Y, UrlTarget, FormControl) then
  begin
  LeftButtonDown := False;
  if Assigned(FormControl) then
    FormControl.ImageClick
  else
    begin
    AssignStr(FURL, UrlTarget.URL^);
    AssignStr(FTarget, UrlTarget.Target^);
    UrlTarget.Free;
    HotSpotAction := True;   {prevent double click action}
    URLAction;
    {Note:  Self pointer may not be valid after URLAction call (TFrameViewer, HistoryMaxCount=0)}
    end;
  end
else LeftButtonDown := False;
end;

{----------------ThtmlViewer.GetWordAtCursor}
function ThtmlViewer.GetWordAtCursor(X, Y: LongInt; var St, En: LongInt;   
                                        var AWord: string): boolean;
const
  AlphNum = ['a'..'z', 'A'..'Z', '0'..'9', #192..#214, #216..#246, #248..#255];
var
  XR, X1, CaretHt: integer;
  YR, Y1: LongInt;
  Cell1: TObject;
  Obj: TObject;
  Ch: char;
  InText: boolean;

  function GetCh(Pos: integer): char;
  var
    Ch: char;
    Obj1: TObject;
  begin
  Result := ' ';
  if not FSectionList.GetChAtPos(Pos, Ch, Obj1) or (Obj1 <> Obj) then Exit;
  Result := Ch;
  end;

begin
Result := False;
AWord := '';
with FSectionList do
  begin
  CaretPos := FindCursor(PaintPanel.Canvas, X,
         Y+YOff-FMarginHeight, XR, YR, CaretHt, Cell1, InText);
  CursorToXY(PaintPanel.Canvas, CaretPos, X1, Y1);
  if Y1-FMarginHeight = YR then   {else cursor is past end of row}
    begin
    en := CaretPos;
    st := en-1;
    if GetChAtPos(en, Ch, Obj) and (Ch in AlphNum) then
      begin
      AWord := Ch;
      Result := True;
      Inc(en);
      Ch := GetCh(en);
      while Ch in AlphNum do
        begin
        AWord := AWord + Ch;
        Inc(en);
        Ch := GetCh(en);
        end;
      if St >= 0 then
        begin
        Ch := GetCh(st);
        while (st >= 0) and (Ch in AlphNum) do
          begin
          System.Insert(Ch, AWord, 1);
          Dec(st);
          if St >= 0 then
            Ch := GetCh(St);
          end;
        end;
      end;
    end;
  end;
end;

{----------------ThtmlViewer.HTMLMouseDblClk}
procedure ThtmlViewer.HTMLMouseDblClk(Message: TWMMouse);   
var
  st, en: LongInt;
  AWord: string;
begin
if FProcessing or HotSpotAction then Exit;
if not FNoSelect and GetWordAtCursor(Message.XPos, Message.YPos, St, En, AWord) then
  begin
  FSectionList.SelB := st+1;
  FSectionList.SelE := en;
  InvalidateRect(PaintPanel.Handle, Nil, True);
  end;
if Assigned(FOnMouseDouble) then
  with Message do
    FOnMouseDouble(Self, mbLeft, KeysToShiftState(Keys), XPos, YPos);
end;

procedure ThtmlViewer.DoHilite(X, Y: integer);
var
  Curs, YR, YWin: LongInt;
  CursCell: TObject;
  XR, CaretHt: integer;
  InText: boolean;
begin
if Hiliting and (Sel1 >= 0) then
  with FSectionList do
    begin
    CursCell := Nil;
    YWin := IntMin(IntMax(0, Y), Height);
    Curs := FindCursor(PaintPanel.Canvas, X, YWin+YOff-FMarginHeight, XR, YR, CaretHt, CursCell, InText);
    if (Curs <> CaretPos) and (Curs >= 0) and not FNoSelect then
      begin
      if Curs > Sel1 then
        begin
        SelE := Curs;
        SelB := Sel1;
        end
      else
        begin
        SelB := Curs;
        SelE := Sel1;
        end;
      InvalidateRect(PaintPanel.Handle, Nil, True);
      end;
    CaretPos := Curs;
    end;
end;

procedure ThtmlViewer.WMMouseScroll(var Message: TMessage);
const
 {$ifdef Win32}
    Ticks: DWord = 0;
 {$else}
    Ticks: LongInt = 0;
 {$endif}
var
  Pos: integer;
  Pt: TPoint;
begin
GetCursorPos(Pt);
Ticks := 0;
with VScrollBar do
  begin
  Pt := PaintPanel.ScreenToClient(Pt);
  while MouseScrolling and ((Pt.Y <= 0) or (Pt.Y > Self.Height)) do
    begin
    if GetTickCount > Ticks +100 then
      begin
      Ticks := GetTickCount;
      if Pt.Y < -15 then
        Pos := Position - SmallChange * 8
      else if Pt.Y <= 0 then
        Pos := Position - SmallChange
      else if Pt.Y > Self.Height+15 then
        Pos := Position + SmallChange * 8
      else
        Pos := Position + SmallChange;
{$ifdef Win32}
      Pos := IntMax(0, IntMin(Pos, MaxVertical - PaintPanel.Height));
      FSectionList.SetYOffset(Pos * LongInt(VScale));
      SetPosition(Pos);
{$else}
      Pos := IntMax(0, IntMin(Pos, Max));
      Position := Pos;
      Self.Scroll(VScrollBar, scLineUp, Pos);
{$endif}
      DoHilite(Pt.X, Pt.Y);
      PaintPanel.Invalidate;
      GetCursorPos(Pt);
      Pt := PaintPanel.ScreenToClient(Pt);
      end;
    Application.ProcessMessages;
    end;
  end;
MouseScrolling := False;
end;

function ThtmlViewer.PositionTo(Dest: string): boolean;
var
  I: integer;
begin
Result := False;
If Dest = '' then Exit;
if Dest[1] = '#' then
  System.Delete(Dest, 1, 1);
I := FNameList.IndexOf(UpperCase(Dest));
if I > -1 then
  begin
  ScrollTo(TSectionBase(FNameList.Objects[I]).YValue);
  HScrollBar.Position := 0;
  Result := True;
  AddVisitedLink(FCurrentFile^+'#'+Dest);   
  end;
end;

function ThtmlViewer.GetURL(X, Y: integer; var UrlTarg: TUrlTarget;
          var FormControl: TImageFormControlObj): boolean;
begin
Result := FSectionList.GetURL(PaintPanel.Canvas, X, Y+FSectionList.YOff-FMarginHeight,   
          UrlTarg, FormControl);
end;

procedure THTMLViewer.SetViewImages(Value: boolean);
var
  OldPos: LongInt;
  OldCursor: TCursor;
begin
if (Value <> FSectionList.ShowImages) and not FProcessing then
  begin
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    SetProcessing(True);
    FSectionList.ShowImages := Value;
    if FSectionList.Count > 0 then
      begin
      FSectionList.GetBackgroundBitmap;    {load any background bitmap}
      OldPos := Position;
      DoLogic;
      Position := OldPos;
      Invalidate;
      end;
  finally
    Screen.Cursor := OldCursor;
    SetProcessing(False);
    end;
  end;
end;

{----------------ThtmlViewer.InsertImage}
function ThtmlViewer.InsertImage(const Src: string; Stream: TMemoryStream): boolean;
var
  OldPos: LongInt;
  ReFormat: boolean;
begin
Result := False;
if FProcessing then Exit;
try
  SetProcessing(True);
  FSectionList.InsertImage(Src, Stream, Reformat);
  FSectionList.GetBackgroundBitmap;     {in case it's the one placed}
  if Reformat then
    if FSectionList.Count > 0 then
      begin
      FSectionList.GetBackgroundBitmap;    {load any background bitmap}
      OldPos := Position;
      DoLogic;
      Position := OldPos;
      end;
  Invalidate;
finally
  SetProcessing(False);
  Result := True;
  end;
end;

function THTMLViewer.GetBase: string;
begin
Result := FBase^;
end;

procedure THTMLViewer.SetBase(Value: string);
begin
AssignStr(FBase, Value);
AssignStr(FBaseEx, Value);   
end;

function THTMLViewer.GetBaseTarget: string;
begin
Result := FBaseTarget^;
end;

function THTMLViewer.GetTitle: string;
begin
Result := FTitle^;
end;

function THTMLViewer.GetCurrentFile: string;
begin
Result := FCurrentFile^;
end;

function THTMLViewer.GetFURL: string;
begin
Result := FURL^;
end;

function THTMLViewer.GetTarget: string;
begin
Result := FTarget^;
end;

function THTMLViewer.GetViewImages: boolean;
begin
Result := FSectionList.ShowImages;
end;

procedure THTMLViewer.SetColor(Value: TColor);
begin
if FProcessing then Exit;
FBackground := Value;
FSectionList.Background:= Value;
PaintPanel.Color := Value;
Invalidate;
end;

procedure THTMLViewer.SetBorderStyle(Value: THTMLBorderStyle);
begin
if Value <> FBorderStyle then
  begin
  FBorderStyle := Value;
  DrawBorder;
  end;
end;

procedure ThtmlViewer.KeyDown(var Key: Word; Shift: TShiftState);
var
  Pos: integer;
  OrigPos:    integer;
  TheChange: LongInt;
begin
inherited KeyDown(Key, Shift);
with VScrollBar do
  if Key in [VK_PRIOR, VK_NEXT, VK_UP, VK_DOWN, VK_HOME, VK_END] then
    begin
    Pos := Position;
    OrigPos := Pos;
    case Key of
      VK_PRIOR : Dec(Pos, LargeChange);
      VK_NEXT  : Inc(Pos, LargeChange);
      VK_UP    : Dec(Pos, SmallChange);
      VK_DOWN  : Inc(Pos, SmallChange);
      VK_Home  : Pos := 0;
      VK_End   : Pos := MaxVertical div VScale;
      end;
    if Pos < 0 then Pos := 0;
    {$ifdef Win32}
    Pos := IntMax(0, IntMin(Pos, MaxVertical - PaintPanel.Height));
    {$else}
    Pos := IntMin(Pos, Max);
    {$endif}

    Position := Pos;
    FSectionList.SetYOffset(Pos * LongInt(VScale));
    TheChange := OrigPos-Pos;
    if abs(TheChange) = SmallChange then begin
       ScrollWindow(PaintPanel.Handle, 0, TheChange*VScale, NIL, NIL);
       PaintPanel.Update;
    end else
       PaintPanel.Invalidate;
    end;
with HScrollBar do
  if Key in [VK_LEFT, VK_RIGHT] then
    begin
    Pos := Position;
    case Key of
      VK_LEFT  : Dec(Pos, SmallChange);
      VK_RIGHT : Inc(Pos, SmallChange);
      end;
    if Pos < 0 then Pos := 0;
    {$ifdef Win32}
    Pos := IntMin(Pos, Max - PaintPanel.Width);
    {$else}
    Pos := IntMin(Pos, Max);
    {$endif}
    Position := Pos;
    PaintPanel.Invalidate;
    end;
end;

procedure ThtmlViewer.WMGetDlgCode(var Message: TMessage);
begin
Message.Result := DLGC_WantArrows;  {else don't get the arrow keys}
end;

function ThtmlViewer.GetPosition: LongInt;
var
  Index: integer;
  TopPos, Pos: LongInt;
  S: TSectionBase;
begin
Pos := LongInt(VScrollBar.Position) * VScale;
S:= FSectionList.FindSectionAtPosition(Pos, TopPos, Index);
if Assigned(S) then
  Result := LongInt(Index) shl 16 + ((Pos - TopPos) and $FFFF)
else Result := 0;
{Hiword is section #, Loword is displacement from top of section}
end;

procedure ThtmlViewer.SetPosition(Value: LongInt);
var
  TopPos: LongInt;
begin
if (Value >= 0) and (Hiword(Value) < FSectionList.Count) then
  begin
  TopPos := FSectionList.FindPositionByIndex(HiWord(Value));
  ScrollTo(TopPos + LoWord(Value));
  end;
end;

function ThtmlViewer.GetScrollPos: integer;
begin
Result := VScrollBar.Position;
end;

procedure ThtmlViewer.SetScrollPos(Value: integer);
begin
    if Value < 0 then Value := 0;
    {$ifdef Win32}
    Value := IntMin(Value, MaxVertical - PaintPanel.Height);
    {$else}
    Value := IntMin(Value, VScrollBar.Max);
    {$endif}
if Value <> GetScrollPos then
  ScrollTo(LongInt(Value) * VScale);
end;

function ThtmlViewer.GetScrollBarRange: integer;
begin
{$ifdef Win32}
Result := MaxVertical - PaintPanel.Height;  
{$else}
Result := VScrollBar.Max;
{$endif}
end;

function ThtmlViewer.GetPalette: HPALETTE;
begin
if ThePalette <> 0 then
  Result := ThePalette
else Result := inherited GetPalette;
Invalidate;
end;

function ThtmlViewer.HTMLExpandFilename(const Filename: string): string;
{$ifdef Windows} 
var
  Dest: string;
  I: integer;
{$endif}
begin
Result := HTMLServerToDos(Trim(Filename), FServerRoot);
{$ifdef Windows}
I := Pos('#', Result);
if I > 0 then
  begin
  Dest := Copy(Result, I, Length(Result)+1);
  Result := Copy(Result, 1, I-1);
  end
else Dest := '';
{$endif}

if Pos('\', Result) = 1 then
  Result := ExpandFilename(Result)
else if (Pos(':', Result)<> 2) and (Pos('\\', Result) <> 1) then
  if CompareText(FBase^, 'DosPath') = 0 then  {let Dos find the path}
  else if FBase^ <> '' then
    Result := ExpandFilename(HTMLToDos(FBase^) + Result)
  else
    Result := ExpandFilename(ExtractFilePath(FCurrentFile^) + Result);
{$ifdef Windows}
Result := Result + Dest;
{$endif}
end;

{----------------ThtmlViewer.BumpHistory}
procedure ThtmlViewer.BumpHistory(const FileName, Title: string;
              OldPos: LongInt; ft: TFileType);
var
  I: integer;
  PO: PositionObj;
begin
if (FHistoryMaxCount > 0) and  (FCurrentFile^ <> '') and
         ((FileName <> FCurrentFile^) or (FCurrentFileType <> ft)
         or (OldPos <> Position)) then
  with FHistory do
    begin
    if (Count > 0) and (Filename <> '') then
      begin
      Strings[FHistoryIndex] := Filename;
      with PositionObj(FPositionHistory[FHistoryIndex]) do
        begin
        Pos := OldPos;
        FileType := ft;
        end;
      FTitleHistory[FHistoryIndex] := Title;
      for I := 0 to FHistoryIndex-1 do
        begin
        Delete(0);
        FTitleHistory.Delete(0);
        PositionObj(FPositionHistory[0]).Free;
        FPositionHistory.Delete(0);
        end;
      end;
    FHistoryIndex := 0;
    Insert(0, FCurrentFile^);
    PO := PositionObj.Create;
    PO.Pos := Position;
    PO.FileType := FCurrentFileType;
    FPositionHistory.Insert(0, PO);
    FTitleHistory.Insert(0, FTitle^);
    if Count > FHistoryMaxCount then
      begin
      Delete(FHistoryMaxCount);
      FTitleHistory.Delete(FHistoryMaxCount);
      PositionObj(FPositionHistory[FHistoryMaxCount]).Free;
      FPositionHistory.Delete(FHistoryMaxCount);
      end;
    if Assigned(FOnHistoryChange) then FOnHistoryChange(Self);
    end;
end;

procedure ThtmlViewer.SetHistoryIndex(Value: integer);
begin
with FHistory do
  if (Value <> FHistoryIndex) and (Value >= 0) and (Value < Count)
            and not FProcessing then
    begin
    if FCurrentFile^ <> '' then
      begin
      Strings[FHistoryIndex] := FCurrentFile^;
      with PositionObj(FPositionHistory[FHistoryIndex]) do
        begin
        Pos := Position;
        FileType := FCurrentFileType;
        end;
      FTitleHistory[FHistoryIndex] := FTitle^;
      end;
    with PositionObj(FPositionHistory[Value]) do
      begin
      if (FCurrentFile^ <> Strings[Value]) or (FCurrentFileType <> FileType) then
        Self.LoadFile(Strings[Value], FileType);
      Position := Pos;
      end;
    FHistoryIndex := Value;
    if Assigned(FOnHistoryChange) then FOnHistoryChange(Self);
    end;
end;

procedure ThtmlViewer.SetHistoryMaxCount(Value: integer);
begin
if (Value = FHistoryMaxCount) or (Value < 0) then Exit;
if Value < FHistoryMaxCount then
  ClearHistory;
FHistoryMaxCount := Value;
end;

procedure ThtmlViewer.ClearHistory;
var
  CountWas: integer;
begin
CountWas := FHistory.Count;
FHistory.Clear;
FTitleHistory.Clear;
FPositionHistory.Clear;
FHistoryIndex := 0;
if (CountWas > 0) and Assigned(FOnHistoryChange) then
  FOnHistoryChange(Self);
end;

function ThtmlViewer.GetFontName: TFontName;
begin
Result := FFontName^;
end;

procedure ThtmlViewer.SetFontName(Value: TFontName);
begin
if  CompareText(Value, FSectionList.FontName) <> 0 then
  begin
  DisposeStr(FFontName);
  FFontName := NewStr(Value);
  FSectionList.FontName := Value;
  FSectionList.UpdateFonts;
  if FSectionList.Count > 0 then
    Layout;
  Invalidate;
  end;
end;

function ThtmlViewer.GetPreFontName: TFontName;
begin
Result := FPreFontName^;
end;

procedure ThtmlViewer.SetPreFontName(Value: TFontName);
begin
if  CompareText(Value, FSectionList.PreFontName) <> 0 then
  begin
  DisposeStr(FPreFontName);
  FPreFontName := NewStr(Value);
  FSectionList.PreFontName := Value;
  FSectionList.UpdateFonts;
  if FSectionList.Count > 0 then
    Layout;
  Invalidate;
  end;
end;

procedure ThtmlViewer.SetFontSize(Value: integer);
begin
Value := IntMax(Value, 6);  {minimum value of 6 pts}
FFontSize := Value;
FSectionList.FontSize := Value;
FSectionList.UpdateFonts;
if FSectionList.Count > 0 then
  Layout;
Invalidate; 
end;

{$ifdef Delphi3_4_CppBuilder3_4}  {Delphi 3, C++Builder 3, 4}
procedure ThtmlViewer.SetCharset(Value: TFontCharset);
begin
FCharset := Value;
FSectionList.Charset := Value;
FSectionList.UpdateFonts;
if FSectionList.Count > 0 then
  Layout;
Invalidate;
end;
{$endif}

function ThtmlViewer.GetFormControlList: TList;
begin
Result := FSectionList.FormControlList;
end;

function ThtmlViewer.GetNameList: TStringList;
begin
Result := FNameList;
end;

function ThtmlViewer.GetLinkList: TList;
begin
Result := FSectionList.LinkList;
end;

procedure ThtmlViewer.SetFontColor(Value: TColor);
begin
FFontColor := Value;
FSectionList.FontColor := Value;
FSectionList.UpdateFonts;
Invalidate;
end;

procedure ThtmlViewer.SetHotSpotColor(Value: TColor);
begin
FHotSpotColor := Value;
FSectionList.HotSpotColor := Value;
FSectionList.UpdateFonts;
Invalidate;
end;

procedure ThtmlViewer.SetVisitedColor(Value: TColor);
begin
FVisitedColor := Value;
FSectionList.LinkVisitedColor := Value;
FSectionList.UpdateFonts;
Invalidate;
end;

procedure ThtmlViewer.SetActiveColor(Value: TColor);
begin
FOverColor := Value;
FSectionList.LinkActiveColor := Value;
FSectionList.UpdateFonts;
Invalidate;
end;

procedure ThtmlViewer.SetVisitedMaxCount(Value: integer);
var
  I: integer;
begin
Value := IntMax(Value, 0);
if Value <> FVisitedMaxCount then
  begin
  FVisitedMaxCount := Value;
  if FVisitedMaxCount = 0 then
    begin
    Visited.Clear;
    for I := 0 to SectionList.LinkList.Count-1 do
      TFontObj(LinkList[I]).Visited := False;
    Invalidate;
    end
  else
    begin
    FVisitedMaxCount := Value;
    for I := Visited.Count-1 downto FVisitedMaxCount do
      Visited.Delete(I);
    end;
  end;
end;

{$ifdef Windows}  {the Delphi 1 version}
procedure ThtmlViewer.Print(FromPage, ToPage: integer);
var
  ARect: TRect;
  PrintList: TSectionList;
  P1, P2, P3, W, H, Dummy: integer;
  HTop, HBot: LongInt;
  Curs: LongInt;
  Done: boolean;
  DC : HDC;
  SaveFont: TFont;
  SaveSize: integer;

  { added to query the printer for printable area }
  QEsc: integer;

  UpperLeftPagePoint, { these will contain Top/Left and Bottom/Right
                        unprintable area}
  LowerRightPagePoint: TPoint;

  { added for "local" coordinate system }
  MLeft: integer;

  MLeftPrn: integer;
  MRightPrn: integer;
  MTopPrn: integer;
  MBottomPrn: integer;
  TopPixels, TopPixelsPrn, HPrn, WPrn: integer;
  hrgnClip: THandle;
  savedFont : TFont ;
  savedPen : TPen ;
  savedBrush : TBrush ;
  Align, ScaledPgHt, ScaledPgWid: integer;
  VPixels: LongInt;    

begin
FPage := 0;
if FProcessing or (FSectionList.Count = 0) then Exit;     
PrintList := TSectionList.CreateCopy(FSectionList);
PrintList.SetYOffset(0);
try
  savedFont := TFont.Create ;
  savedPen := TPen.Create ;
  savedBrush := TBrush.Create ;
  try
    PrintList.Printing := True;
    PrintList.SetBackground(clWhite);
    FPage := 1;
    try
      with Printer, Canvas do
        begin
        hRgnClip := 0;
        if (Printer.Title = '') and (DocumentTitle <> '') then
            Printer.Title := DocumentTitle ;
        BeginDoc;
        DC := Canvas.Handle;
        SaveFont := TFont.Create;
        SaveFont.Assign(Canvas.Font);
        P3 := GetDeviceCaps(DC, LOGPIXELSY);
        P2 := Screen.PixelsPerInch;
        Canvas.Font.PixelsPerInch := P2;  {use screen pixels for ThtmlViewer}
        SetMapMode(DC, mm_AnIsotropic);
        P1 := GetDeviceCaps(DC, LOGPIXELSX);
        SetWindowExtEx(DC, P2, P2, Nil);
        SetViewPortExtEx(DC, P1,P3, Nil);

        { calculate the amount of space that is non-printable }

        { get PHYSICAL page width }
        QEsc := GetPhysPageSize;
        if Escape(Printer.Handle, QueryEscSupport, sizeof(Integer),
                                  @QEsc, NIL)>0 then
          Escape(Printer.Handle, GetPhysPageSize, 0, NIL, @LowerRightPagePoint)
        else { command NOT supported !}
          with LowerRightPagePoint do
          begin
            x := -1; y := -1; { signal that these values are not good at all...}
          end;

        { now compute a complete unprintable area rectangle (composed of 2*width,
          2*height) in pixels...}
        with LowerRightPagePoint do
        begin
          y := y - GetDeviceCaps(DC, VertRes);
          x := x - GetDeviceCaps(DC, HorzRes);
        end;

        { get upper left physical offset for the printer... ->
          printable area <> paper size }
        QEsc := GetPrintingOffset;
        if Escape(Printer.Handle, QueryEscSupport, sizeof(Integer),
                 @QEsc, NIL)>0 then
          Escape(Printer.Handle, GetPrintingOffset, 0, NIL, @UpperLeftPagePoint)
        else
          with UpperLeftPagePoint do
          begin
            x := 0; y := 0; { assume there is no physical offset ?! }
          end;

        { now that we know the TOP and LEFT offset we finally can
          compute the BOTTOM and RIGHT offset: }
        with LowerRightPagePoint do
        begin
          x := x - UpperLeftPagePoint.x;
          { we don't want to have negative values}
          if x < 0 then
            x := 0; { assume no right printing offset }

          y := y - UpperLeftPagePoint.x;
          { we don't want to have negative values}
          if y < 0 then
            y := 0; { assume no bottom printing offset }
        end;
        { which results in LowerRightPoint containing the BOTTOM
          and RIGHT unprintable
          area offset; using these we modify the (logical, true)
          borders...}

        MLeftPrn := trunc(FPrintMarginLeft/2.54 * P1);
        MLeftPrn := MLeftPrn - UpperLeftPagePoint.x; { subtract physical offset }
        MLeft := MulDiv(MLeftPrn, P2, P1);

        MRightPrn := trunc(FPrintMarginRight/2.54 * P1);
        MRightPrn := MRightPrn - LowerRightPagePoint.x; { subtract physical offset }

        WPrn := PageWidth - (MLeftPrn + MRightPrn);
        W := MulDiv(WPrn, P2, P1);

        Curs := 0;
        VPixels := PrintList.DoLogic(Canvas, 0, W, Dummy, Curs);  

        MTopPrn := trunc(FPrintMarginTop/2.54 * P3);
        MTopPrn := MTopPrn - UpperLeftPagePoint.y; { subtract physical offset }

        MBottomPrn := trunc(FPrintMarginBottom/2.54 * P3);
        MBottomPrn := MBottomPrn - LowerRightPagePoint.y; { subtract physical offset }

        TopPixelsPrn := MTopPrn; 
        TopPixels := MulDiv(TopPixelsPrn, P2, P3);

        HPrn := PageHeight-(MTopPrn+MBottomPrn);
        H := MulDiv(HPrn, P2, P3);    {scaled pageHeight}

        Done := False;
        HTop := 0;
        ScaledPgHt := MulDiv(PageHeight, P2, P3);
        ScaledPgWid := MulDiv(PageWidth, P2, P3);
        hrgnClip := CreateRectRgn(0, TopPixelsPrn-1, WPrn + MLeftPrn+2,  
                    TopPixelsPrn + HPrn + 2);
        ARect := Rect(MLeft, TopPixels, W + MLeft, TopPixels + H);
        while (FPage <= ToPage) and not Done do
          begin
          PrintList.SetYOffset(HTop-TopPixels);
          HBot := HTop + H;
          SetMapMode(DC, mm_AnIsotropic);
          SetWindowExtEx(DC, P2, P2, Nil);
          SetViewPortExtEx(DC, P1,P3, Nil);
          SetWindowOrgEx(DC, 0, 0, Nil);
          SelectClipRgn(DC, hrgnClip);

          if FPage >= FromPage then
            begin
            PrintList.Draw(Canvas, ARect, W, MLeft, 0);
            { preserve current settings of the Canvas, in case user
                would make changes and not restore them back }
            savedFont.Assign (Canvas.Font);
            savedPen.Assign (Canvas.Pen);
            savedBrush.Assign (Canvas.Brush);
            SelectClipRgn(DC, 0);

            {White out excess printing}
            Canvas.Brush.Color := clWhite;
            Canvas.Brush.Style := bsSolid;
            Canvas.Pen.Style := psClear;
            Canvas.Rectangle(MLeft, 0, W + MLeft, TopPixels-1);   
            Canvas.Rectangle(MLeft, PrintList.PageBottom-HTop+TopPixels,
                             W + MLeft, TopPixels+H);
            Canvas.Pen.Assign(savedPen);

            if Assigned(FOnPrintHeader) or Assigned(FOnPrintFooter) then
              begin
              Align := SetTextAlign(DC, TA_Top or TA_Left or TA_NOUPDATECP);
              if Assigned(FOnPrintHeader) then
                begin
                SetWindowOrgEx(DC, 0, 0, Nil);
                FOnPrintHeader(Self, Canvas, FPage, ScaledPgWid, TopPixels, Done);
                end;
              if Assigned(FOnPrintFooter) then
                begin
                SetWindowOrgEx(DC, 0, -(TopPixels+H), Nil);
                FOnPrintFooter(Self, Canvas, FPage, ScaledPgWid,
                    ScaledPgHt-(TopPixels+H), Done);
                end;
              SetTextAlign(DC, Align);
              end;

            { restore initial Canvas settings }
            Canvas.Font.Assign(savedFont);
            Canvas.Pen.Assign(savedPen);
            Canvas.Brush.Assign(savedBrush);
            end
          else PrintList.Draw(Canvas, ARect, W, MLeft+3*W, 0); {off page}

          HTop := PrintList.PageBottom;
          if HTop > VPixels then   
            Done := True;
          if not Done and (FPage >= FromPage) and (FPage < ToPage) then
            NewPage;
          Inc(FPage);
          end;
        end;
    finally
      if (FromPage > FPage) then
        Printer.Abort;
      Printer.EndDoc;
      Dec(FPage);
      if hRgnClip <> 0 then DeleteObject(hrgnClip);
      Printer.Canvas.Font.Assign(SaveFont);  {restore font for others}
      SaveSize := Printer.Canvas.Font.Size;
      Printer.Canvas.Font.PixelsPerInch := P3;
      Printer.Canvas.Font.Size := SaveSize;
      SaveFont.Free;
      Printer.Title := '';
      end;
  finally
    savedFont.Free;
    savedPen.Free;
    savedBrush.Free;
    end;
finally
  PrintList.Free;
  end;
end;

{$else)   {Versions for other than Delphi 1}

procedure ThtmlViewer.Print(FromPage, ToPage: integer);
var
  ARect: TRect;
  PrintList: TSectionList;
  P1, P2, P3, W, H, HTop, HBot, Dummy: integer;
  Curs: LongInt;
  Done: boolean;
  DC : HDC;
  vwP, OldPrinter: TvwPrinter;
  QEsc: integer;

  UpperLeftPagePoint, { these will contain Top/Left and Bottom/Right unprintable area}
  LowerRightPagePoint: TPoint;
  MLeft: integer;
  MLeftPrn: integer;
  MRightPrn: integer;
  MTopPrn: integer;
  MBottomPrn: integer;
  TopPixels, TopPixelsPrn, HPrn, WPrn: integer;
  hrgnClip: THandle;
  savedFont : TFont ;
  savedPen : TPen ;
  savedBrush : TBrush ;
  Align, ScaledPgHt, ScaledPgWid, VPixels: integer;

begin
Done := False;
if Assigned(FOnPageEvent) then
  FOnPageEvent(Self, 0, Done);
FPage := 0;
if FProcessing or (FSectionList.Count = 0) then Exit;
PrintList := TSectionList.CreateCopy(FSectionList);
PrintList.SetYOffset(0);
try
  savedFont := TFont.Create ;
  savedPen := TPen.Create ;
  savedBrush := TBrush.Create ;
  try
    PrintList.Printing := True;
    PrintList.SetBackground(clWhite);
    vwP := TvwPrinter.Create;
    OldPrinter := vwSetPrinter(vwP);
    FPage := 1;
    hrgnClip := 0;
    try
      with vwP do
        begin
        if (DocumentTitle <> '') then   
            vwP.Title := DocumentTitle ;
        BeginDoc;
        DC := Canvas.Handle;
        P3 := GetDeviceCaps(DC, LOGPIXELSY);
        P2 := Screen.PixelsPerInch;
        SetMapMode(DC, mm_AnIsotropic);
        P1 := GetDeviceCaps(DC, LOGPIXELSX);
        SetWindowExtEx(DC, P2, P2, Nil);
        SetViewPortExtEx(DC, P1,P3, Nil);


        { calculate the amount of space that is non-printable }

        { get PHYSICAL page width }
              LowerRightPagePoint.X := GetDeviceCaps(Printer.Handle, PhysicalWidth);
              LowerRightPagePoint.Y := GetDeviceCaps(Printer.Handle, PhysicalHeight);

              { now compute a complete unprintable area rectangle
               (composed of 2*width, 2*height) in pixels...}
              with LowerRightPagePoint do
                 begin
                   Y := Y - Printer.PageHeight;
                   X := X - Printer.PageWidth;
                 end;

        { get upper left physical offset for the printer... ->
          printable area <> paper size }
              UpperLeftPagePoint.X := GetDeviceCaps(Printer.Handle, PhysicalOffsetX);
              UpperLeftPagePoint.Y := GetDeviceCaps(Printer.Handle, PhysicalOffsetY);

        { now that we know the TOP and LEFT offset we finally can
          compute the BOTTOM and RIGHT offset: }
        with LowerRightPagePoint do
        begin
          x := x - UpperLeftPagePoint.x;
          { we don't want to have negative values}
          if x < 0 then
            x := 0; { assume no right printing offset }

          y := y - UpperLeftPagePoint.y;        
          { we don't want to have negative values}
          if y < 0 then
            y := 0; { assume no bottom printing offset }
        end;
        { which results in LowerRightPoint containing the BOTTOM
          and RIGHT unprintable
          area offset; using these we modify the (logical, true)
          borders...}

        MLeftPrn := trunc(FPrintMarginLeft/2.54 * P1);
        MLeftPrn := MLeftPrn - UpperLeftPagePoint.x; { subtract physical offset }
        MLeft := MulDiv(MLeftPrn, P2, P1);

        MRightPrn := trunc(FPrintMarginRight/2.54 * P1);
        MRightPrn := MRightPrn - LowerRightPagePoint.x; { subtract physical offset }

        WPrn := PageWidth - (MLeftPrn + MRightPrn);

        W := MulDiv(WPrn, P2, P1);
        Curs := 0;
        VPixels := PrintList.DoLogic(Canvas, 0, W, Dummy, Curs);     

        MTopPrn := trunc(FPrintMarginTop/2.54 * P3);
        MTopPrn := MTopPrn - UpperLeftPagePoint.y; { subtract physical offset }

        MBottomPrn := trunc(FPrintMarginBottom/2.54 * P3);
        MBottomPrn := MBottomPrn - LowerRightPagePoint.y; { subtract physical offset }

        TopPixelsPrn := MTopPrn; 
        TopPixels := MulDiv(TopPixelsPrn, P2, P3);

        HPrn := PageHeight-(MTopPrn+MBottomPrn);
        H := MulDiv(HPrn, P2, P3);  {scaled pageHeight}
        Done := False;
        HTop := 0;
        ScaledPgHt := MulDiv(PageHeight, P2, P3);
        ScaledPgWid := MulDiv(PageWidth, P2, P3);
        hrgnClip := CreateRectRgn(0, TopPixelsPrn-1, WPrn + MLeftPrn+2,
                  TopPixelsPrn + HPrn+2);
        Application.ProcessMessages;
        if Assigned(FOnPageEvent) then
          FOnPageEvent(Self, FPage, Done);

        while (FPage <= ToPage) and not Done do
          begin
          HBot := HTop + H;
          ARect := Rect(MLeft, HTop, W + MLeft, HBot);
          SetMapMode(DC, mm_AnIsotropic);
          SetWindowExtEx(DC, P2, P2, Nil);
          SetViewPortExtEx(DC, P1,P3, Nil);
          SetWindowOrgEx(DC, 0, HTop-TopPixels, Nil);
          SelectClipRgn(DC, hrgnClip);

          if FPage >= FromPage then
            begin
            PrintList.Draw(Canvas, ARect, W, MLeft, 0);
            { preserve current settings of the Canvas, in case user
                would make changes and not restore them back }
            savedFont.Assign (Canvas.Font);    
            savedPen.Assign (Canvas.Pen);
            savedBrush.Assign (Canvas.Brush);
            SelectClipRgn(DC, 0);
            {White out excess printing}
            Canvas.Brush.Color := clWhite;
            Canvas.Brush.Style := bsSolid;
            Canvas.Pen.Style := psClear;
            Canvas.Rectangle(MLeft, PrintList.PageBottom, W + MLeft, HBot);
            Canvas.Rectangle(MLeft, HTop-TopPixels, W + MLeft, HTop);
            Canvas.Pen.Assign(savedPen);

            Align := SetTextAlign(DC, TA_Top or TA_Left or TA_NOUPDATECP);
            if Assigned(FOnPrintHeader) then
              begin
              SetWindowOrgEx(DC, 0, 0, Nil);
              FOnPrintHeader(Self, Canvas, FPage, ScaledPgWid, TopPixels, Done);
              end;
            if Assigned(FOnPrintFooter) then
              begin
              SetWindowOrgEx(DC, 0, -(TopPixels+H), Nil);
              FOnPrintFooter(Self, Canvas, FPage, ScaledPgWid,
                  ScaledPgHt-(TopPixels+H), Done);
              end;
            SetTextAlign(DC, Align);
            { restore initial Canvas settings }
            Canvas.Font.Assign(savedFont);
            Canvas.Pen.Assign(savedPen);
            Canvas.Brush.Assign(savedBrush);
            end
          else PrintList.Draw(Canvas, ARect, W, MLeft+3*W, 0);  {off page}
          HTop := PrintList.PageBottom;

          Application.ProcessMessages;
          if Assigned(FOnPageEvent) then
            FOnPageEvent(Self, FPage, Done);
          if HTop > VPixels then
            Done := True;
          if not Done and (FPage >= FromPage) and (FPage < ToPage) then
            NewPage;
          Inc(FPage);
          end;
        end;
    finally
      if hRgnClip <> 0 then DeleteObject(hrgnClip);
      if (FromPage > FPage) then
        vwPrinter.Abort
      else
        vwPrinter.EndDoc;
      Dec(FPage);
      vwSetPrinter(OldPrinter);
      vwP.Free;
      end;
  finally
    savedFont.Free ;
    savedPen.Free ;
    savedBrush.Free ;
    end;
finally
  PrintList.Free;
  end;
end;

function ThtmlViewer.PrintPreview(MFPrinter: TMetaFilePrinter): integer;
var
  ARect         : TRect;
  PrintList     : TSectionList;
  P1, P2, P3    : integer;
  W, H          : integer;
  HTop, HBot    : integer;
  Dummy         : integer;
  Curs          : LongInt;
  Done          : boolean;
  DC            : HDC;
  PrnDC         : HDC; {metafile printer's DC}

  UpperLeftPagePoint, { these will contain Top/Left and Bottom/Right unprintable area}
  LowerRightPagePoint: TPoint;

  MLeft         : integer;
  MLeftPrn      : integer;
  MRightPrn     : integer;
  MTopPrn       : integer;
  MBottomPrn    : integer;
  TopPixels     : integer;
  TopPixelsPrn  : integer;
  HPrn, WPrn    : integer;
  hrgnClip      : THandle;
  hrgnClip2     : THandle;
  SavedFont     : TFont;
  SavedPen      : TPen;
  SavedBrush    : TBrush;
  Align         : integer;
  ScaledPgHt    : integer;
  ScaledPgWid   : integer;
  VPixels       : integer;

begin
   Done := False;
   if Assigned(FOnPageEvent) then
     FOnPageEvent(Self, 0, Done);
   FPage := 0;
   Result := 0;
   if FProcessing or (SectionList.Count = 0) then Exit;
   PrintList := TSectionList.CreateCopy(SectionList);
   PrintList.SetYOffset(0);
   try
     SavedPen := TPen.Create;
     SavedFont := TFont.Create;
     SavedBrush := TBrush.Create;
     try
       PrintList.Printing := True;
       PrintList.SetBackground(clWhite);

       FPage := 1;
       hrgnClip := 0;
       hrgnClip2 := 0;
       try
         with MFPrinter do
           begin
              if DocumentTitle <> '' then
                 Title := DocumentTitle;

              BeginDoc;
              DC := Canvas.Handle;
              PrnDC := PrinterDC;

              P3 := GetDeviceCaps(PrnDC, LOGPIXELSY);
              P2 := Screen.PixelsPerInch;
              SetMapMode(DC, mm_AnIsotropic);
              P1 := GetDeviceCaps(PrnDC, LOGPIXELSX);
              SetWindowExtEx(DC, P2, P2, nil);
              SetViewPortExtEx(DC, P1, P3, nil);

              { calculate the amount of space that is non-printable }

              { get PHYSICAL page width }
              LowerRightPagePoint.X := GetDeviceCaps(PrnDC, PhysicalWidth);
              LowerRightPagePoint.Y := GetDeviceCaps(PrnDC, PhysicalHeight);

              { now compute a complete unprintable area rectangle
               (composed of 2*width, 2*height) in pixels...}
              with LowerRightPagePoint do
                 begin
                   Y := Y - Printer.PageHeight;
                   X := X - Printer.PageWidth;
                 end;

              { get upper left physical offset for the printer... ->
                printable area <> paper size }
              UpperLeftPagePoint.X := GetDeviceCaps(PrnDC, PhysicalOffsetX);
              UpperLeftPagePoint.Y := GetDeviceCaps(PrnDC, PhysicalOffsetY);

              { now that we know the TOP and LEFT offset we finally can
                compute the BOTTOM and RIGHT offset: }
              with LowerRightPagePoint do
                 begin
                   X := X - UpperLeftPagePoint.X;
                   { we don't want to have negative values}
                   if X < 0 then
                     X := 0; { assume no right printing offset }

                   Y := Y - UpperLeftPagePoint.Y;
                   { we don't want to have negative values}
                   if Y < 0 then
                     Y := 0; { assume no bottom printing offset }
                 end;

              { which results in LowerRightPoint containing the BOTTOM
                and RIGHT unprintable area offset; using these we modify
                the (logical, true) borders...}

              MLeftPrn := Trunc(FPrintMarginLeft/2.54 * P1);
              MLeftPrn := MLeftPrn - UpperLeftPagePoint.X;  { subtract physical offset }
              MLeft := MulDiv(MLeftPrn, P2, P1);

              MRightPrn := Trunc(FPrintMarginRight/2.54 * P1);
              MRightPrn := MRightPrn - LowerRightPagePoint.X;  { subtract physical offset }

              WPrn := PageWidth - (MLeftPrn + MRightPrn);

              W := MulDiv(WPrn, P2, P1);
              Curs := 0;
              VPixels := PrintList.DoLogic(Canvas, 0, W, Dummy, Curs);

              MTopPrn := Trunc(FPrintMarginTop/2.54 * P3);
              MTopPrn := MTopPrn - UpperLeftPagePoint.Y;  { subtract physical offset }

              MBottomPrn := Trunc(FPrintMarginBottom/2.54 * P3);
              MBottomPrn := MBottomPrn - LowerRightPagePoint.Y;  { subtract physical offset }

              TopPixelsPrn := MTopPrn;
              TopPixels    := MulDiv(TopPixelsPrn, P2, P3);

              HPrn := PageHeight-(MTopPrn+MBottomPrn);
              H    := MulDiv(HPrn, P2, P3);  {scaled pageHeight}
              HTop := 0;

              ScaledPgHt  := MulDiv(PageHeight, P2, P3);
              ScaledPgWid := MulDiv(PageWidth,  P2, P3);

              {This one clips to the allowable print region so that the preview is
               limited to that region also}
              hrgnClip2 := CreateRectRgn(0, 0, Printer.PageWidth, Printer.PageHeight);
              {This one is primarily used to clip the top and bottom margins to insure
               nothing is output there.  It's also constrained to the print region
               in case the margins are misadjusted.}
              hrgnClip := CreateRectRgn(0, IntMax(0, TopPixelsPrn-1),
                     IntMin(Printer.PageWidth, WPrn+MLeftPrn+2),
                     IntMin(Printer.PageHeight, TopPixelsPrn+HPrn+2));
              Application.ProcessMessages;
              if Assigned(FOnPageEvent) then
                 FOnPageEvent(Self, FPage, Done);

              while not Done do
                begin
                   HBot := HTop + H;
                   ARect := Rect(MLeft, HTop, W+MLeft, HBot);

                   {next line is necessary because the canvas changes with each new page }
                   DC := Canvas.Handle;

                   SetMapMode(DC, mm_AnIsotropic);
                   SetWindowExtEx(DC, P2, P2, nil);
                   SetViewPortExtEx(DC, P1, P3, nil);
                   SetWindowOrgEx(DC, 0, HTop-TopPixels, nil);
                   SelectClipRgn(DC, hrgnClip);

                   PrintList.Draw(Canvas, ARect, W, MLeft, 0);
                   if Assigned(FOnPrintHeader) or
                      Assigned(FOnPrintFooter) then
                     begin
                        { preserve current settings of the Canvas, in case }
                        { the user makes changes and doesn't restore them. }
                        SavedPen.Assign(Canvas.Pen);
                        SavedFont.Assign(Canvas.Font);
                        SavedBrush.Assign(Canvas.Brush);
                        SelectClipRgn(DC, 0);

                        {White out excess printing}
                        Canvas.Brush.Color := clWhite;
                        Canvas.Brush.Style := bsSolid;
                        Canvas.Pen.Style   := psClear;
                        Canvas.Pen.Color   := clWhite;
                        Canvas.Rectangle(MLeft, PrintList.PageBottom, W+MLeft, HBot);
                        Canvas.Rectangle(MLeft, HTop-TopPixels, W+MLeft, HTop);

                        Align := SetTextAlign(DC, TA_Top or TA_Left or TA_NOUPDATECP);
                        SelectClipRgn(DC, hrgnClip2);

                        if Assigned(FOnPrintHeader) then
                          begin
                             SetWindowOrgEx(DC, 0, 0, Nil);
                             FOnPrintHeader(Self, Canvas, FPage, ScaledPgWid, TopPixels, Done);
                          end;

                        if Assigned(FOnPrintFooter) then
                          begin
                             SetWindowOrgEx(DC, 0, -(TopPixels+H), nil);
                             FOnPrintFooter(Self, Canvas, FPage, ScaledPgWid,
                                            ScaledPgHt-(TopPixels+H), Done);
                          end;

                        { restore initial Canvas settings }
                        Canvas.Pen.Assign(SavedPen);
                        Canvas.Font.Assign(SavedFont);
                        Canvas.Brush.Assign(SavedBrush);
                        SetTextAlign(DC, Align);
                        SelectClipRgn(DC, 0);
                     end;

                   HTop := PrintList.PageBottom;
                   Application.ProcessMessages;
                   if Assigned(FOnPageEvent) then
                       FOnPageEvent(Self, FPage, Done);
                   if HTop > VPixels then
                     Done := True;

                   if not Done then
                     NewPage;

                   Inc(FPage);
                end;
              EndDoc;
           end;
       finally
         if hRgnClip <> 0 then
           DeleteObject(hrgnClip);
         if hRgnClip2 <> 0 then
           DeleteObject(hrgnClip2);
         Dec(FPage);
       end;
     finally
       SavedPen.Free;
       SavedFont.Free;
       SavedBrush.Free;
     end;
   finally
     PrintList.Free;
     Result := FPage;
   end;
end;          
{$endif}

{$ifdef Windows}  {Delphi 1}
function ThtmlViewer.NumPrinterPages: integer;
begin
Print(9998, 9999);
Result := FPage;
end;

{$else}

function ThtmlViewer.NumPrinterPages: integer;
var
  MFPrinter: TMetaFilePrinter;
begin
MFPrinter := TMetaFilePrinter.Create(Nil);
try
  PrintPreview(MFPrinter);
  Result := MFPrinter.LastAvailablePage;
finally
  MFPrinter.Free;
  end;
end;
{$endif}

procedure ThtmlViewer.BackgroundChange(Sender: TObject);
begin
PaintPanel.Color := (Sender as TSectionList).Background or $2000000;
end;

procedure ThtmlViewer.SetOnBitmapRequest(Handler: TGetBitmapEvent);
begin
FOnBitmapRequest := Handler;
FSectionList.GetBitmap := Handler;
end;

procedure ThtmlViewer.SetOnImageRequest(Handler: TGetImageEvent);
begin
FOnImageRequest := Handler;
FSectionList.GetImage := Handler;
end;

procedure ThtmlViewer.SetOnExpandName(Handler: TExpandNameEvent);  
begin
FOnExpandName := Handler;
FSectionList.ExpandName := Handler;
end;

procedure ThtmlViewer.SetOnScript(Handler: TScriptEvent);
begin
FOnScript := Handler;
FSectionList.ScriptEvent := Handler;
end;

procedure ThtmlViewer.SetOnObjectClick(Handler: TObjectClickEvent);
begin
FOnObjectClick := Handler;
FSectionList.ObjectClick := Handler;
end;

procedure ThtmlViewer.SetOnFormSubmit(Handler: TFormSubmitEvent);
begin
FOnFormSubmit := Handler;
if Assigned(Handler) then
  FSectionList.SubmitForm := SubmitForm
else FSectionList.SubmitForm := Nil;
end;

procedure ThtmlViewer.SubmitForm(Sender: TObject; const Action, Target, EncType, Method: string;
                    Results: TStringList);
begin
if Assigned(FOnFormSubmit) then
  begin
  FAction := NewStr(Action);
  FMethod := NewStr(Method);
  FFormTarget := NewStr(Target);
  FEncType:= NewStr(EncType);
  FStringList := Results;
  PostMessage(Handle, wm_FormSubmit, 0, 0);
  end;
end;

procedure ThtmlViewer.WMFormSubmit(var Message: TMessage);
begin
FOnFormSubmit(Self, FAction^, FFormTarget^, FEncType^, FMethod^, FStringList); 
DisposeStr(FAction);
DisposeStr(FMethod);
DisposeStr(FFormTarget);
DisposeStr(FEncType);
end;     {user disposes of the TStringList}

function ThtmlViewer.Find(const S: String; MatchCase: boolean): boolean;
var
  ChArray: array[0..256] of char;
  Curs: LongInt;
  X: integer;
  Y, Pos: LongInt;
begin
Result := False;
if S = '' then Exit;
StrPCopy(ChArray, S);
with FSectionList do
  begin
  Curs := FindString(CaretPos, ChArray, MatchCase);
  if Curs >= 0 then
    begin
    Result := True;
    SelB := Curs;
    SelE := Curs+Length(S);
    CaretPos := SelE;
    if CursorToXY(PaintPanel.Canvas, Curs, X, Y) then
      begin
      Pos := VScrollBarPosition * LongInt(VScale);
      if (Y < Pos) or
             (Y > Pos +ClientHeight-20) then
        VScrollBarPosition := (Y - ClientHeight div 2) div VScale;
      Invalidate;
      end;
    end;
  end;
end;

procedure ThtmlViewer.FormControlEnterEvent(Sender: TObject);
var
  Y, Pos: LongInt;
begin
if Sender is TFormControlObj then
  begin
  Y := TFormControlObj(Sender).YValue;
  Pos := VScrollBarPosition * LongInt(VScale);
  if (Y < Pos) or (Y > Pos +ClientHeight-20) then
    begin
    VScrollBarPosition := (Y - ClientHeight div 2) div VScale;
    Invalidate;
    end;
  end;
end;

procedure ThtmlViewer.SelectAll;
begin
with FSectionList do
  if (Count > 0) and not FNoSelect then
    begin
    SelB := 0;
    with TSectionBase(Items[Count-1]) do
      SelE := StartCurs + Len;
    Invalidate;
    end;
end;

procedure ThtmlViewer.Clear;
{Note: because of Frames do not clear history list here}
begin
if FProcessing then Exit;
FSectionList.Clear;
FSectionList.SetFonts(FFontName^, FPreFontName^, FFontSize, FFontColor,
                      FHotSpotColor, FVisitedColor, FOverColor, FBackground,
                        htOverLinksActive in FOptions);
FNameList.Clear;
DisposeStr(FBase); FBase := NullStr;
AssignStr(FBaseEx, '');
DisposeStr(FBaseTarget); FBaseTarget := NullStr;
DisposeStr(FTitle); FTitle := NullStr;
VScrollBar.Max := 0;
VScrollBar.Visible := False;
VScrollBar.Height := PaintPanel.Height;
HScrollBar.Visible := False;
CaretPos := 0;
Sel1 := -1;
if Assigned(FOnSoundRequest) then
  FOnSoundRequest(Self, '', 0, True);
Invalidate;
end;

procedure ThtmlViewer.PaintWindow(DC: HDC);
begin
PaintPanel.RePaint;
VScrollbar.RePaint;   
HScrollbar.RePaint;  
end;

procedure ThtmlViewer.CopyToClipboard;
begin
FSectionList.CopyToClipboard;
end;

function ThtmlViewer.GetSelTextBuf(Buffer: PChar; BufSize: LongInt): LongInt;
begin
if BufSize <= 0 then Result := 0
else Result := FSectionList.GetSelTextBuf(Buffer, BufSize);
end;

{$ifdef Windows} {Delphi 1}
function ThtmlViewer.GetSelText: string;
{only reads the first 255 chars of selected text}
var
  Buf: array[0..255] of char;
  Len: integer;
begin
Len := FSectionList.GetSelTextBuf(@Buf, 256);
Dec(Len);     {the #0}
if Len > 0 then
  begin
  Move(Buf, Result[1], Len);
  Result[0] := chr(Len);
  end
else Result := '';
end;

{$else}  {Delphi 2,3}
function ThtmlViewer.GetSelText: string;
var
  Len: LongInt;
begin
Len := GetSelLength;
if Len > 0 then
  begin
  SetString(Result, Nil, Len);
  FSectionList.GetSelTextBuf(Pointer(Result), Len+1);
  end
else Result := '';
end;
{$endif}

function ThtmlViewer.GetSelLength: LongInt;
begin
Result := FSectionList.GetSelLength;
end;

procedure ThtmlViewer.SetSelLength(Value: LongInt);
begin
if Value = 0 then
  with FSectionList do
    begin
    SelE := SelB;
    Invalidate;
    end;
end;

procedure ThtmlViewer.SetNoSelect(Value: boolean);
begin
if Value <> FNoSelect then
  begin
  FNoSelect := Value;
  if Value = True then
    begin
    FSectionList.SelB := -1;
    FSectionList.SelE := -1;
    RePaint;
    end;
  end;
end;

procedure ThtmlViewer.UpdateImageCache;
begin
BitmapList.BumpAndCheck;
end;

procedure ThtmlViewer.SetImageCacheCount(Value: integer);
begin
Value := IntMax(0, Value);
Value := IntMin(20, Value);
if Value <> FImageCacheCount then
  begin
  FImageCacheCount := Value;
  BitmapList.SetCacheCount(FImageCacheCount);
  end;
end;

procedure ThtmlViewer.DrawBorder;
begin
if (Focused and (FBorderStyle = htFocused)) or (FBorderStyle = htSingle)
       or (csDesigning in ComponentState) then
  begin
  PaintBox.Canvas.Pen.color := clBtnText;
  end
else
  begin
  PaintBox.Canvas.Pen.Color := PaintPanel.Color or $2000000;
  end;
if Showing then
  PaintBox.Canvas.Rectangle(0,0,Width, Height);
end;

procedure ThtmlViewer.DoEnter;
begin
inherited DoEnter;
DrawBorder;
end;

procedure ThtmlViewer.DoExit;
begin
inherited DoExit;
DrawBorder;
end;

procedure ThtmlViewer.SetScrollBars(Value: TScrollStyle);
begin
if (Value <> FScrollBars) then
  begin
  FScrollBars := Value;
  if not (csLoading in ComponentState) and HandleAllocated then
    begin
    SetProcessing(True);
    try
      DoLogic;
    finally
      SetProcessing(False);
      end;
    Invalidate;
    end;
  end;
end;

{----------------ThtmlViewer.Reload}
procedure ThtmlViewer.Reload;     {reload the last file}
var
  Pos: LongInt;
begin
if FCurrentFile^ <> '' then
  begin
  Pos := Position;
  if FCurrentFileType = HTMLType then
    LoadFromFile(FCurrentFile^)
  else if FCurrentFileType = TextType then
    LoadTextFile(FCurrentFile^)
  else LoadImageFile(FCurrentFile^);
  Position := Pos;
  end;
end;

{----------------ThtmlViewer.GetOurPalette:}
function ThtmlViewer.GetOurPalette: HPalette;
begin
if ColorBits = 8 then
  Result := CopyPalette(ThePalette)
else Result := 0;
end;

{----------------ThtmlViewer.SetOurPalette}
procedure ThtmlViewer.SetOurPalette(Value: HPalette);
var
  NewPalette: HPalette;
begin
if (Value <> 0) and (ColorBits = 8) then
  begin
  NewPalette := CopyPalette(Value);
  if NewPalette <> 0 then
    begin
    if ThePalette <> 0 then
      DeleteObject(ThePalette);
    ThePalette := NewPalette;
    if FDither then SetGlobalPalette(ThePalette);
    end;
  end;
end;

{----------------ThtmlViewer.SetDither}
procedure ThtmlViewer.SetDither(Value: boolean);
begin
if (Value <> FDither) and (ColorBits = 8) then
  begin
  FDither := Value;
  if Value then SetGlobalPalette(ThePalette)
  else SetGLobalPalette(0);
  end;
end;

procedure ThtmlViewer.SetCaretPos(Value: LongInt);
begin
if Value >= 0 then
  FCaretPos := Value;
end;

{----------------ThtmlViewer.SetProcessing}
procedure ThtmlViewer.SetProcessing(Value: boolean);
begin
if FProcessing <> Value then
  begin
  FProcessing := Value;
  if Assigned(FOnProcessing) and not (csLoading in ComponentState) then  
        FOnProcessing(Self, FProcessing);
  end;
end;

{----------------THTMLViewer.SetMarginWidth}
procedure THTMLViewer.SetMarginWidth(Value: integer);
var
  OldPos: LongInt;
  OldCursor: TCursor;
begin
if (Value <> FMarginWidth) and not FProcessing and (Value >= 0) and (Value <= 50) then
  begin
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    SetProcessing(True);
    FMarginWidth := Value;
    if FSectionList.Count > 0 then
      begin
      OldPos := Position;
      DoLogic;
      Position := OldPos;
      Invalidate;
      end;
  finally
    Screen.Cursor := OldCursor;
    SetProcessing(False);
    end;
  end;
end;

{----------------THTMLViewer.SetMarginHeight}
procedure THTMLViewer.SetMarginHeight(Value: integer);
var
  OldPos: LongInt;
  OldCursor: TCursor;
begin
if (Value <> FMarginHeight) and not FProcessing and (Value >= 0) and (Value <= 50) then
  begin
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    SetProcessing(True);
    FMarginHeight := Value;
    if FSectionList.Count > 0 then
      begin
      OldPos := Position;
      DoLogic;
      Position := OldPos;
      Invalidate;
      end;
  finally
    Screen.Cursor := OldCursor;
    SetProcessing(False);
    end;
  end;
end;

procedure THTMLViewer.SetServerRoot(Value: string);
begin
Value := Trim(Value);
if (Length(Value) >= 1) and (Value[Length(Value)] = '\') then
  SetLength(Value, Length(Value)-1);
FServerRoot := Value;
end;

procedure THTMLViewer.HandleMeta(Sender: TObject; const HttpEq, Name, Content: string);
var
  DelTime, I: integer;
begin
if Assigned(FOnMeta) then FOnMeta(Self, HttpEq, Name, Content);
if Assigned(FOnMetaRefresh) then
  if CompareText(Lowercase(HttpEq), 'refresh') = 0 then
    begin
    I := Pos(';', Content);
    if I > 0 then
      DelTime := StrToIntDef(copy(Content, 1, I-1), -1)
    else DelTime := StrToIntDef(Content, -1);        
    if DelTime < 0 then Exit
    else if DelTime = 0 then DelTime := 1;
    I := Pos('url=', Lowercase(Content));
    if I > 0 then   
      FRefreshURL := Copy(Content, I+4, Length(Content)-I-3)
    else FRefreshURL := '';
    FRefreshDelay := DelTime;   
    end;
end;

procedure THTMLViewer.SetOptions(Value: ThtmlViewerOptions); 
begin
if Value <> FOptions then
  begin
  FOptions := Value;
  if Assigned(FSectionList) then
    with FSectionList do
      begin
      LinksActive := htOverLinksActive in FOptions;
      if htNoLinkUnderline in FOptions then
        UnLine := []
      else UnLine := [fsUnderline];
      PrintTableBackground := htPrintTableBackground in FOptions;
      end;
  end;
end;

{----------------TPaintPanel.CreateIt}
constructor TPaintPanel.CreateIt(AOwner: TComponent; Viewer: TComponent);

begin
  inherited Create(AOwner);
  FViewer := Viewer;
end;

{----------------TPaintPanel.Paint}
procedure TPaintPanel.Paint;
var
  MemDC: HDC;
  ABitmap: HBitmap;
  ARect: TRect;
  OldPal: HPalette;
begin
if (FViewer as ThtmlViewer).DontDraw then Exit;
ThtmlViewer(FViewer).DrawBorder;
OldPal := 0;
Canvas.Font := Font;
Canvas.Brush.Color := Color;
ARect := Canvas.ClipRect;
Canvas2 := TCanvas.Create;   {paint on a memory DC}
try
  MemDC := CreateCompatibleDC(Canvas.Handle);
  ABitmap := 0;
  try
    with ARect do
      begin
      ABitmap := CreateCompatibleBitmap(Canvas.Handle, Right-Left, Bottom-Top);
      if ABitmap = 0 then   raise EOutOfResources.Create('Out of Resources');
      try
        SelectObject(MemDC, ABitmap);
        SetWindowOrgEx(memDC, Left, Top, Nil);
        Canvas2.Handle := MemDC;
        DoBackground(Canvas2, False);
        if Assigned(FOnPaint) then FOnPaint(Self);
        OldPal := SelectPalette(Canvas.Handle, ThePalette, False);
        RealizePalette(Canvas.Handle);
        BitBlt(Canvas.Handle, Left, Top, Right-Left, Bottom-Top,
                              MemDC, Left, Top, SrcCopy);
     finally
        if OldPal <> 0 then SelectPalette(MemDC, OldPal, False);
        Canvas2.Handle := 0;
        end;
      end;
  finally
    DeleteDC(MemDC);
    DeleteObject(ABitmap);
  end;
finally
  Canvas2.Free;
  end;
end;

procedure TPaintPanel.DoBackground(ACanvas: TCanvas; WmErase: boolean);
var
  Bitmap: TBitmap;
  H, W, WW, HH, HPos, BW, BH, DCx, DCy: integer;
  Pos: LongInt;
  OldBrush: HBrush;
  OldPal: HPalette;
  ARect: TRect;
  DC: HDC;
  CopyFromDC: boolean;
begin
DC := ACanvas.handle;
if DC <> 0 then
  begin
  Pos := (FViewer as ThtmlViewer).VScrollBarPosition * LongInt(VScale);
  ARect := Canvas.ClipRect;

  OldPal := SelectPalette(DC, ThePalette, False);
  RealizePalette(DC);
  ACanvas.Brush.Color := Color or $2000000;
  OldBrush := SelectObject(DC, ACanvas.Brush.Handle);
  try
    with FViewer as ThtmlViewer do
      begin
      if True then   {note:  this code for later use for Watermarks}
        HPos := HScrollBar.Position
      else
        begin
        HPos := 0;
        Pos := 0;
        end;
      Bitmap := FSectionList.BackgroundBitmap;
      if FSectionList.ShowImages and not FSectionList.Printing
                                 and Assigned(Bitmap) then
        try
          DCx := 0;  DCy := 0;
          BW := Bitmap.Width;
          BH := Bitmap.Height;
          HH := (Pos div BH) * BH - Pos;
          WW := (HPos div BW) * BW - HPos;
          while HH < ARect.Top do
            Inc(HH, BH);
          while WW < ARect.Left do
            Inc(WW, BW);
          {to use the fast method, the bitmap must be drawn entirely within the
           viewable area}
          if not WmErase and ((HH+BH <= ARect.Bottom) and ((WW+BW <= ARect.Right)
                  or ((WW = ARect.Left) and (BW >= ARect.Right-ARect.Left)))) then
            begin
            CopyFromDC := True;   {fast}
            DCx := WW;
            DCy := HH;
            end
          else CopyFromDC := False;
          Dec(HH, BH);
          Dec(WW, BW);
          H := HH;
          if CopyFromDC then
            BitBlt(DC, DCx, DCy, BW, BH, Bitmap.Canvas.Handle, 0, 0, SRCCOPY);  
          repeat
            W := WW;
            repeat
              if CopyFromDC then
                BitBlt(DC, W, H, BW, BH, DC, DCx, DCy, SRCCOPY)   
              else
                BitBlt(DC, W, H, BW, BH, Bitmap.Canvas.Handle, 0, 0, SRCCOPY);   
              Inc(W, BW);
            until W >= ARect.Right;
            Inc(H, BH);
          until H >= ARect.Bottom;
        except
          ACanvas.FillRect(ARect);
        end
      else
        begin
        ACanvas.FillRect(ARect);
        end;
      end;
  finally
    SelectObject(DC, OldBrush);
    SelectPalette(DC, OldPal, False);
    RealizePalette(DC);
    end;
  end;

end;

procedure TPaintPanel.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
{Only do background now if resizing the window}
if Sizing and (Message.DC <> 0) then
  begin
  Canvas.Handle := Message.DC;
  try
    DoBackground(Canvas, True);  
  finally
    Canvas.Handle := 0;
    Sizing := False;
    end;
  end;
Message.Result := 1;   {it's erased}
end;

{----------------TPaintPanel.WMLButtonDblClk} 
procedure TPaintPanel.WMLButtonDblClk(var Message: TWMMouse);
begin
if Message.Keys and MK_LButton <> 0 then
  ThtmlViewer(FViewer).HTMLMouseDblClk(Message);
end;

{$ifdef Win32}
{----------------T32ScrollBar.SetParams}
procedure T32ScrollBar.SetParams(APosition, APage, AMin, AMax: Integer);
var
  ScrollInfo: TScrollInfo;
begin
if (APosition <> FPosition) or (APage <> FPage) or (AMin <> FMin)
              or (AMax <> FMax) then
  with ScrollInfo do
    begin
    cbSize := SizeOf(ScrollInfo);
    fMask := SIF_ALL;
    nPos := APosition;
    nPage := APage;
    nMin := AMin;
    nMax := AMax;
    SetScrollInfo(Handle, SB_CTL, ScrollInfo, True);
    FPosition := APosition;
    FPage := APage;
    FMin := AMin;
    FMax := AMax;
    end;
end;

procedure T32ScrollBar.SetPosition(Value: integer);
begin
SetParams(Value, FPage, FMin, FMax);
end;

procedure T32ScrollBar.SetMin(Value: Integer);
begin
  SetParams(FPosition, FPage, Value, FMax);
end;

procedure T32ScrollBar.SetMax(Value: Integer);
begin
  SetParams(FPosition, FPage, FMin, Value);
end;

procedure T32ScrollBar.CNVScroll(var Message: TWMVScroll);
var
  SPos: LongInt;
  ScrollInfo: TScrollInfo;
  OrigPos: LongInt;
  TheChange: LongInt;
begin
Parent.SetFocus;
with ThtmlViewer(Parent) do
  begin
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_ALL;
  GetScrollInfo(Self.Handle, SB_CTL, ScrollInfo);
  if TScrollCode(Message.ScrollCode) = scTrack then
    begin
    OrigPos := ScrollInfo.nPos;
    SPos := ScrollInfo.nTrackPos;
    end
  else
    begin
    SPos := ScrollInfo.nPos;
    OrigPos := SPos;
    case TScrollCode(Message.ScrollCode) of
      scLineUp:
        Dec(SPos, SmallChange);
      scLineDown:
        Inc(SPos, SmallChange);
      scPageUp:
        Dec(SPos, LargeChange);
      scPageDown:
        Inc(SPos, LargeChange);
      scTop:
        SPos := 0;
      scBottom:
        SPos := (MaxVertical - PaintPanel.Height) div VScale;
      end;
    end;
  if SPos < 0 then SPos := 0;
  SPos := IntMin(SPos, (MaxVertical - PaintPanel.Height) div VScale);

  Self.SetPosition(SPos);

  FSectionList.SetYOffset(SPos * VScale);
  TheChange := OrigPos-SPos;

  ScrollWindow(PaintPanel.Handle,0,TheChange,NIL,NIL);
  PaintPanel.Update;
  end;
end;
{$endif}

function THTMLEditor.GetVerbCount: integer;
begin
  Result := 1;
end;

function THTMLEditor.GetVerb(index: Integer): string;
begin
  Result := 'About..';
end;

procedure THTMLEditor.ExecuteVerb(index:integer);
begin
  MessageDlg('ThtmlViewer'+#13#13+
             'Version     : '+VersionNo+#13#13+
             'Copyright  : 1995-9 by L. David Baldwin, All Rights Reserved'+#13#13+
             'Support    : dbaldwin@pbear.com'+#13#13+
             'Web Site : http://www.pbear.com/ '
             ,mtInformation,[mbOk],0)
end;

procedure Register;
begin
RegisterComponents('Samples', [THTMLViewer]);
RegisterComponentEditor(THTMLViewer, THTMLEditor);     
end;

end.

