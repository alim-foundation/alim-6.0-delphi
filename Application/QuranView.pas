unit QuranView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  AmlView, StdCtrls, islctrls, RXCtrls, ExtCtrls, Htmlview, HtmlSubs, QuranStruct,
  XmlObjModel, DFSSplitter, StDict, StBits, CompDoc, IslUtils, TextView,
  Menus, RxMenus, RXCombos, RXSpin, AyahComposer;

const
  { QuranData flags, added to Flags in TAmlData -- beware of conflicts }
  qdfIsArabic           = $00100000;
  qdfShowInCompare      = $00200000;
  qdfAutoRevealNotes    = $00400000;
  qdfShowAyahSep        = $00800000;
  qdfShowThemes         = $01000000;
  qdfReciteOnClick      = $02000000;  // if not set, then goes to MultiQuran on click
  qdfViewArabicIcons    = $04000000;
  qdfMarkedForCopyPrint = $00800000;

type
  TSuraMapData =
    record
      SuraNum : Word;
      AyahCount : Word;
      AyahMapOffs : LongInt;
    end;
  TSuraMap = array[TSuraNum] of TSuraMapData;

  TAyahMapData =
    record
      AyahNum : Word;
      AyahTextStart : LongInt;
      AyahTextLen : Word;
    end;
  TAyahMap = array[TAyahNum] of TAyahMapData;

  TAyahAsHtmlOption = (ahoIsolated, ahoIncludeNums);
  TAyahAsHtmlOptions = set of TAyahAsHtmlOption;
  TQuranData = class(TAmlData)
  protected
    FSura : Cardinal;
    FSuraData : TXmlElement;
    FSuraMap : TSuraMap;
    FSuraMapLoaded : Boolean;
    FAyahMapStream : TStream;
    FAyahMapLoaded : Integer;  // 0 or < 1 if no Sura's ayah map loaded, >= 1 if loaded
    FAyahMap : TAyahMap;

    function GetLibName : String; override;
    function CreateViewer : TAmlViewer; override;
    procedure ClearActiveSura; virtual;
    procedure SetupDefaultFlags; override;
    function CreateDefaultFont(const AFontId : String) : TAmlDataFont; override;
  public
    destructor Destroy; override;
    procedure Close; override;
    function InitializeLoad(const AFileName : String;
                            const ACatalog : TXmlElement) : Boolean; override;
    function GetAddressCaption(const AAddress : TAddress; const AIncludeName : Boolean) : String; override;
    function ReadAyahMarkup(const ASura : TSuraNum; const AAyah : TAyahNum) : String;
    function ReadAyah(const ASura : TSuraNum; const AAyah : TAyahNum) : TXmlElement;
    function ReadAyahText(const ASura : TSuraNum; const AAyah : TAyahNum) : String;
    function ReadSura(const ASura : TSuraNum; const ACacheIt : Boolean) : TXmlElement;
    function AyahAsHtml(const ASura : TSuraNum; const AAyah : TAyahNum; const AOptions : TAyahAsHtmlOptions) : String;
    procedure CopyAyahToClipboard(const ALoc : TSuraAyah);
    procedure BruteSearch(const AText : String; const ASuraStart, ASuraEnd : TSuraNum; const AList : TBookmarks);
    procedure ArabicSearch;
  end;

  TThemeInfo = class(TObject)
  public
    Theme : String;
    StartAyah : TAyahNum;
    EndAyah : TAyahNum;
    AyahCount : Cardinal;
  end;
  TAyahFlagArray = array[TAyahNum] of Boolean;
  TQuranThemesData = class(TAmlData)
  protected
    FSura : Cardinal;
    FSuraThemes : TStringList;
    FThemesDict : TStDictionary;
    FAyahHasTheme : TAyahFlagArray;

    procedure ClearActiveSura; virtual;
  public
    destructor Destroy; override;

    procedure ReadThemes(const ASura : TSuraNum);
    property ThemesDict : TStDictionary read FThemesDict;
    property AyahHasTheme : TAyahFlagArray read FAyahHasTheme;
  end;

  TQuranViewer = class(TAmlViewer)
    LocationPanel: TWallpaperPanel;
    Location: TRxLabel;
    HeadingPanel: TWallpaperPanel;
    MainPanel: TPanel;
    ContentsPanel: TPanel;
    NotesSplitter: TDFSSplitter;
    HtmlViewer: THTMLViewer;
    NotesViewer: THTMLViewer;
    ColorDialog: TColorDialog;
  protected
    FThemes : TQuranThemesData;
    FBook : TQuranData;
    FActiveLoc : TSuraAyah;
    FSearchLoc : TSuraAyah;
    FActiveNotes : TStDictionary;

    procedure ClearContents; virtual;
    procedure UpdateContents; virtual;
    procedure UpdateNavigation; virtual;
    procedure SetAddress(const AAddress : TAddress); override;
    function CreateRtlAyahTable(const ASura : TXmlElement;
                                const AStartAyah : TAyahNum;
                                const AEndAyah : TAyahNum) : String;
    function CreateAyahTable(const ASura : TXmlElement;
                             const AStartAyah : TAyahNum;
                             const AEndAyah : TAyahNum) : String;
    function SuraToHtml(const ASura : TXmlElement;
                        const AStartAyah : TAyahNum = Low(TAyahNum);
                        const AEndAyah : TAyahNum = High(TAyahNum)) : String;

    function GetActiveData : TAmlData; override;
    procedure InternalHtmlFootnoteCovered(Sender : TObject; const ANoteId : String); override;
    procedure InternalHtmlFootnoteClick(Sender: TObject; const ANoteId : String); override;
    procedure InternalHtmlRightClick(Sender: TObject; Parameters: TRightClickParameters); override;
    procedure ContextMenuCommand(const ATag : LongInt); override;
    function GetEnableCopy : Boolean; override;
    function GetEnablePrint : Boolean; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure Customize; override;
    procedure Navigate(const AType : TNavigate; const ADirection : TNavigateDirection; const AAddress : TCaptionedAddress = Nil); override;
    procedure ExecuteQuranCmd(const ACommand : TQuranCommand); override;
    function SelectSura(const ASura : TSuraNum) : Boolean; override;
    procedure Print(const ADialog : TPrintDialog); override;
    procedure CopyToClipboard; override;

    property Book : TQuranData read FBook;
  end;

implementation

{$R *.DFM}

uses StStrS, StStrL, ArabicKeyboardForm, ClipBrd, Main;

destructor TQuranData.Destroy;
begin
  ClearActiveSura;
  inherited Destroy;
end;

procedure TQuranData.ClearActiveSura;
begin
  if FSuraData <> Nil then
    FSuraData.Free;
  FSuraData := Nil;
  FSura := 0;
  FAyahMapLoaded := -1;
end;

procedure TQuranData.SetupDefaultFlags;
begin
  SetFlag(qdfShowInCompare);
  SetFlag(qdfAutoRevealNotes);
  SetFlag(qdfShowAyahSep);
  SetFlag(qdfShowThemes);
  SetFlag(qdfMarkedForCopyPrint);
end;

function TQuranData.CreateDefaultFont(const AFontId : String) : TAmlDataFont;
var
  FontIdx : Integer;
begin
  Result := inherited CreateDefaultFont(AFontId);
  FontIdx := FindString(AFontId, ['ayah', 'altayah', 'compare', 'theme', 'note']);
  case FontIdx of
    0, 1, 2 :
      begin
        if FlagIsSet(qdfIsArabic) then begin
          if FAraEncodings.AvailableFonts.IndexOf(Result.Name) = -1 then begin
            Result.Name := HtmlSubs.RtlFontDefault;
            Result.PointSize := HtmlSubs.RtlFontDefaultSize;
           end;
        end;
        case FontIdx of
          0 : Result.Color := clGreen;
          1 : Result.Color := clNavy;
          2 : if FlagIsSet(qdfIsArabic) then Result.Color := clGreen;
        end;
      end;
    3 : begin Result.Color := clTeal; Result.Name := 'Arial Narrow'; end;
    4 : begin Result.Name := 'Tahoma'; Result.PointSize := 8; end;
  end;
end;

procedure TQuranData.Close;
begin
  if FSuraData <> Nil then
    FSuraData.Free;
  if FAyahMapStream <> Nil then
    FAyahMapStream.Free;
  FAyahMapStream := Nil;
  FSuraData := Nil;
  FSura := 0;
  FAyahMapLoaded := -1;
  inherited Close;
end;

function TQuranData.InitializeLoad(const AFileName : String;
                                   const ACatalog : TXmlElement) : Boolean;
begin
  Result := inherited InitializeLoad(AFileName, ACatalog);
  if Result then
    SetFlag(qdfIsArabic, FLanguageId = 'arabic');

  ClearFlag(adfShareViewer);
  SetFlag(adfIsQuranData);
end;

function TQuranData.GetAddressCaption(const AAddress : TAddress; const AIncludeName : Boolean) : String;
begin
  if AIncludeName then
    Result := Format('%s (Sura %s Ayah %s)', [Name, AAddress.Volume, AAddress.Element])
  else
    Result := Format('Sura %s Ayah %s', [AAddress.Volume, AAddress.Element]);
end;

function TQuranData.GetLibName : String;
begin
  Result := 'Quran';
end;

function TQuranData.CreateViewer : TAmlViewer;
begin
  Result := TQuranViewer.Create(Application);
end;

function TQuranData.ReadAyahMarkup(const ASura : TSuraNum; const AAyah : TAyahNum) : String;
var
  SuraMapStream, SuraTextStream : TStream;
  SourceXml : String;
  AyahMapData : TAyahMapData;
begin
  Result := '';
  if (AAyah < 1) or (AAyah > FQuranStruct.Suras[ASura].ayahCount) then
    Exit;

  if not FSuraMapLoaded then begin
    try
      SuraMapStream := OpenStream('SuraMap');
      SuraMapStream.Read(FSuraMap, SizeOf(FSuraMap));
      SuraMapStream.Free;
      FSuraMapLoaded := True;
    except
      Exit;
    end;
  end;

  if FAyahMapStream = Nil then begin
    try
      FAyahMapStream := OpenStream('AyahMap');
    except
      FAyahMapStream := Nil;
    end;
  end;

  if (FAyahMapStream = Nil) or (not FSuraMapLoaded) then
    Exit;

  if FSuraMap[ASura].SuraNum <> ASura then
    raise Exception.CreateFmt('In SuraMap, Sura %d has invalid data (%d <> %d)', [ASura, FSuraMap[ASura].SuraNum, ASura]);

  if FAyahMapLoaded <> ASura then begin
    FAyahMapStream.Position := FSuraMap[ASura].AyahMapOffs;
    FAyahMapStream.Read(FAyahMap, FSuraMap[ASura].AyahCount * SizeOf(TAyahMapData));
    FAyahMapLoaded := ASura;
  end;
  AyahMapData := FAyahMap[AAyah];
  if AyahMapData.AyahNum <> AAyah then
    raise Exception.CreateFmt('In AyahMap, Ayah %d has invalid data (%d <> %d)', [AAyah, AyahMapData.AyahNum, AAyah]);

  SetLength(SourceXml, AyahMapData.AyahTextLen);
  try
    SuraTextStream := OpenStream(IntToStr(Pred(ASura)));
    SuraTextStream.Position := AyahMapData.AyahTextStart;
    SuraTextStream.Read(PChar(SourceXml)^, AyahMapData.AyahTextLen);
    SuraTextStream.Free;
  except
    Exit;
  end;

  if AmlDataEncryptKey <> '' then
    Result := ScrambleL(SourceXml, AmlDataEncryptKey)
  else
    Result := SourceXml;
end;

function TQuranData.ReadAyah(const ASura : TSuraNum; const AAyah : TAyahNum) : TXmlElement;
var
  AyahMarkup : String;
begin
  try
    AyahMarkup := ReadAyahMarkup(ASura, AAyah);
  except
    AyahMarkup := '';
  end;
  if AyahMarkup = '' then
    AyahMarkup := Format('<ayah num="%d">Sura %d Ayah %d not found</ayah>', [AAyah, ASura, AAyah]);

  try
    FXmlParser.LoadMemory(PChar(AyahMarkup));
    Result := FXmlParser.Document.DocumentElement.CloneNode as TXmlElement;
  except
    Result := Nil;
  end;
end;

function TQuranData.ReadAyahText(const ASura : TSuraNum; const AAyah : TAyahNum) : String;
var
  Node : TXmlNode;
  Elem : TXmlElement;
  I, ChildCount : Integer;
begin
  try
    Elem := ReadAyah(ASura, AAyah);
  except
    Elem := Nil;
  end;
  if (Elem <> Nil) then begin
    Result := '';
    ChildCount := Elem.ChildNodes.Length;
    if ChildCount > 0 then begin
      Dec(ChildCount);
      for I := 0 to ChildCount do begin
        Node := Elem.ChildNodes.Item(I);
        if Node.NodeType = TEXT_NODE then
          Result := Result + (Node as TXmlText).Data;
      end;
    end;
  end else
    Result := Format('Sura %d Ayah %d not found in %s', [ASura, AAyah, Name]);
end;

function TQuranData.ReadSura(const ASura : TSuraNum; const ACacheIt : Boolean) : TXmlElement;
var
  A : TAyahNum;
  SuraMarkup : String;
begin
  if FSura = ASura then begin
    Result := FSuraData;
    Exit;
  end;

  ClearActiveSura;
  with FQuranStruct.Suras[ASura] do begin
    FStatus.StatusBegin(Format('Loading Sura %d...', [ASura]), True);
    SuraMarkup := Format('<sura num="%d">', [ASura]);
    for A := 1 to AyahCount do
      SuraMarkup := SuraMarkup + ReadAyahMarkup(ASura, A);
    SuraMarkup := SuraMarkup + '</sura>';
    try
      FXmlParser.LoadMemory(PChar(SuraMarkup));
      Result := FXmlParser.Document.DocumentElement.CloneNode as TXmlElement;
    except
      Result := Nil;
    end;
    FStatus.StatusEnd;
  end;
  if ACacheIt then begin
    FSuraData := Result;
    if Result <> Nil then
      FSura := ASura
    else
      FSura := 0;
  end;
end;

procedure TQuranData.CopyAyahToClipboard(const ALoc : TSuraAyah);
var
  Ayah : TXmlElement;
begin
  if not FlagIsSet(adfCanCopyText) then begin
    ShowMessage(Format('The publishers of %s do not permit copying text to the clipboard.', [FName]));
    Exit;
  end;

  Ayah := ReadAyah(ALoc.suraNum, ALoc.ayahNum);
  Clipboard.AsText := Ayah.Text;
  Ayah.Free;
end;

procedure TQuranData.BruteSearch(const AText : String; const ASuraStart, ASuraEnd : TSuraNum;
                                 const AList : TBookmarks);
var
  A : Integer;
  AyahElem : TXmlElement;
  SuraNum : TSuraNum;
  AyahNum : TAyahNum;
  TmpSuraData : TXmlElement;
  SuraHd : String;
  SuraBmk, AyahBmk : TBookmarkNode;
begin
  Assert(FQuranStruct <> Nil);
  Assert(ASuraStart <= ASuraEnd);

  for SuraNum := ASuraStart to ASuraEnd do begin
    SuraBmk := Nil;

    FStatus.StatusBegin(Format('Searching Sura %d...', [SuraNum]), True);
    try
      TmpSuraData := ReadSura(SuraNum, False);
      Application.ProcessMessages;
      for A := 0 to TmpSuraData.ChildNodes.Length-1 do begin
        if TmpSuraData.ChildNodes.Item(A).NodeType <> ELEMENT_NODE then
          continue;

        AyahElem := TmpSuraData.ChildNodes.Item(A) as TXmlElement;
        if Pos(AText, AyahElem.Text) > 0 then begin
          if SuraBmk = Nil then begin
            SuraHd := Format('Sura %d. %s', [SuraNum, FQuranStruct.SuraName[SuraNum]]);
            SuraBmk := AList.AddChild('', '', SuraHd, Format('%03d', [SuraNum]));
            SuraBmk.SmallIconIdx := 3;
          end;

          AyahNum := StrToInt(AyahElem.GetAttribute('num'));
          AyahBmk := SuraBmk.Children.AddChild('', Format('%s:%d.%d', [Id, SuraNum, AyahNum]), Format('Ayah %d', [AyahNum]), Format('%03d', [AyahNum]));
          AyahBmk.SmallIconIdx := 18;
        end;
        Application.ProcessMessages;
      end;
    except
    end;
    FStatus.StatusEnd;
  end;
end;

procedure TQuranData.ArabicSearch;
var
  SearchResults : TBookmarks;
begin
  if not FlagIsSet(qdfIsArabic) then
    Exit;

  ArabicKeyboard.Encodings := FAraEncodings;
  if ArabicKeyboard.ShowModal <> mrOk then
    Exit;

  SearchResults := TBookmarks.Create;
  try
    BruteSearch(ArabicKeyboard.ArabicText, 1, 114, SearchResults);
    FAddrMgr.ShowBookmarks(SearchResults, Format('Search Results (%d)', [SearchResults.Count]), True);
  except
  end;
end;

function TQuranData.AyahAsHtml(const ASura : TSuraNum; const AAyah : TAyahNum; const AOptions : TAyahAsHtmlOptions) : String;
begin
  Result := ReadAyahText(ASura, AAyah);
  if ahoIsolated in AOptions then begin
    if FlagIsSet(qdfIsArabic) then
      Result := Format('<p align="right"><font face="%s" size=5>%s</font></p>', [AraEncodings.DefaultFontName, Reverse(Result)])
    else
      Result := Format('<p>%s</p>', [Result]);
  end;
end;

{------------------------------------------------------------------------------}

destructor TQuranThemesData.Destroy;
begin
  ClearActiveSura;
  inherited Destroy;
end;

procedure TQuranThemesData.ClearActiveSura;
begin
  if FSuraThemes <> Nil then
    FSuraThemes.Free;
  FSuraThemes := Nil;
  if FThemesDict <> Nil then
    FThemesDict.Free;
  FThemesDict := Nil;
  FSura := 0;
end;

procedure TQuranThemesData.ReadThemes(const ASura : TSuraNum);
var
  P, I, J : Integer;
  Range : String;
  ThemeRec : TThemeInfo;
begin
  if (FSuraThemes <> Nil) and (FSura = ASura) then
    Exit;

  ClearActiveSura;
  FStatus.StatusBegin(Format('Loading Sura %d themes...', [ASura]), True);
  try
    FSuraThemes := ReadStringList(Format('S%d', [ASura]));
    FSura := ASura;
  except
    FSuraThemes := Nil;
    FSura := 0;
  end;

  FThemesDict := TStDictionary.Create(High(TSuraNum));
  FThemesDict.DisposeData := DisposeObject;
  FillChar(FAyahHasTheme, SizeOf(FAyahHasTheme), 0);

  if (FSuraThemes <> Nil) and (FSuraThemes.Count > 0) then begin
    for I := 0 to FSuraThemes.Count-1 do begin
      ThemeRec := TThemeInfo.Create;
      Range := FSuraThemes.Names[I];
      with ThemeRec do begin
        Theme := FSuraThemes.Values[Range];
        P := Pos('-', Range);
        if P = 0 then begin
          StartAyah := StrToInt(Range);
          EndAyah := StartAyah;
          FAyahHasTheme[StartAyah] := True;
          AyahCount := 1;
        end else begin
          StartAyah := StrToInt(Copy(Range, 1, P-1));
          EndAyah :=  StrToInt(Copy(Range, P+1, Length(Range)));
          AyahCount := EndAyah-StartAyah+1;
          for J := StartAyah to EndAyah do
            FAyahHasTheme[J] := True;
        end;
        FThemesDict.Add(IntToStr(StartAyah), ThemeRec);
      end;
    end;
  end;

  FStatus.StatusEnd;
end;

{------------------------------------------------------------------------------}

constructor TQuranViewer.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FActiveLoc.suraNum := 1;
  FActiveLoc.ayahNum := 1;

  HtmlViewer.OnHotSpotCovered := DefaultHtmlHotSpotCovered;
  HtmlViewer.OnHotSpotClick := DefaultHtmlHotspotClick;
  HtmlViewer.OnImageRequest := DefaultHtmlImageRequest;
  HtmlViewer.OnRightClick := DefaultHtmlRightClick;
  NotesViewer.OnHotSpotCovered := DefaultHtmlHotSpotCovered;
  NotesViewer.OnHotSpotClick := DefaultHtmlHotspotClick;
  NotesViewer.OnImageRequest := DefaultHtmlImageRequest;
  NotesViewer.OnRightClick := DefaultHtmlRightClick;
end;

destructor TQuranViewer.Destroy;
begin
  if FActiveNotes <> Nil then
    FActiveNotes.Free;
  inherited Destroy;
end;

procedure TQuranViewer.Navigate(const AType : TNavigate; const ADirection : TNavigateDirection; const AAddress : TCaptionedAddress);
begin
  Assert(FBook <> Nil);
  if (FBook.QuranStruct = Nil) or (ADirection = nvdCaptionedAddr) then
    Exit;

  case AType of
    nvPage :
      begin
        case ADirection of
          nvdPrevious : FActiveLoc.SuraNum := FBook.QuranStruct.MoveSura(FActiveLoc.SuraNum, mdPrev);
          nvdNext : FActiveLoc.SuraNum := FBook.QuranStruct.MoveSura(FActiveLoc.SuraNum, mdNext);
        end;
        FActiveLoc.AyahNum := 1;
      end;
    nvItem :
      case ADirection of
        nvdPrevious : FActiveLoc := FBook.QuranStruct.MoveAyah(FActiveLoc, mdPrev);
        nvdNext : FActiveLoc := FBook.QuranStruct.MoveAyah(FActiveLoc, mdNext);
      end;
  end;
  UpdateContents;
  UpdateNavigation;
end;

procedure TQuranViewer.ClearContents;
begin
  FActiveLoc.suraNum := 1;
  FActiveLoc.ayahNum := 1;
  HeadingPanel.Caption := '';
  HtmlViewer.LoadFromBuffer(PChar(''), 0);
end;

const
  AyahSepHtml = '<hr width=100% color=silver size=1>';

function TQuranViewer.CreateRtlAyahTable(const ASura : TXmlElement;
                                         const AStartAyah : TAyahNum;
                                         const AEndAyah : TAyahNum) : String;
const
  AyahRowFmt = '<tr valign=top>'+
               '<td align="right"><font color="%2:s">%1:s</font>%4:s</td>'+
               '<td width=5></td><td><font color="silver"><b><a name="A%0:d" href="%3:s">%0:d</a></b></font></td>'+
               '%5:s'+
               '</tr>';
  SpecialIconsFmt = '<td><a href="%0:s"><img src="#18" border=0></a><a href="%1:s"><img src="#28" border=0></a></td>';
var
  A, AC : Cardinal;
  SuraNum : TSuraNum;
  AyahChild : TXmlNode;
  AyahElem : TXmlElement;
  AyahText, AyahSep, SuraNumStr, FontColor : String;
  CompareAnchor, ReciteAnchor, AraImgAnchor, SpecialIcons : String;
  AyahNum : TAyahNum;
  Color1, Color2 : String;
begin
  if FBook.FlagIsSet(qdfShowAyahSep) then
    AyahSep := AyahSepHtml
  else
    AyahSep := '';

  SuraNumStr := ASura.GetAttribute('num');
  Result := '';
  Color1 := FBook.GetFont('ayah').HtmlColor;
  Color2 := FBook.GetFont('ayahalt').HtmlColor;
  FontColor := Color1;
  for A := 0 to ASura.ChildNodes.Length-1 do begin
    if ASura.ChildNodes.Item(A).NodeType <> ELEMENT_NODE then
      continue;
    AyahElem := ASura.ChildNodes.Item(A) as TXmlElement;
    AyahNum := StrToInt(AyahElem.GetAttribute('num'));
    if (AyahNum < AStartAyah) or (AyahNum > AEndAyah) then
      continue;

    SuraNum := StrToInt(SuraNumStr);
    if FBook.FlagIsSet(qdfReciteOnClick) then
      CompareAnchor := CreateQuranAddress(qatReciteOne, SuraNum, AyahNum)
    else
      CompareAnchor := CreateQuranAddress(qatMultiQuran, SuraNum, AyahNum);

    if FBook.FlagIsSet(qdfViewArabicIcons) then begin
      ReciteAnchor := CreateQuranAddress(qatReciteOne, SuraNum, AyahNum);
      AraImgAnchor := CreateQuranAddress(qatArabicImages, SuraNum, AyahNum);
      SpecialIcons := Format(SpecialIconsFmt, [AraImgAnchor, ReciteAnchor]);
    end else
      SpecialIcons := '';

    AyahText := '';
    for AC := 0 to AyahElem.ChildNodes.Length-1 do begin
      AyahChild := AyahElem.ChildNodes.Item(AC);
      if AyahChild.NodeName = '' then
        AyahText := AyahText + AyahChild.NodeValue
    end;

    AyahText := Reverse(AyahText);
    Result := Result + Format(AyahRowFmt, [AyahNum, AyahText, FontColor, CompareAnchor, AyahSep, SpecialIcons]);

    if FontColor = Color1 then FontColor := Color2 else FontColor := Color1;
  end;

  Result := '<table cellspacing=3 cellpadding=5 width=100%>' + Result + '</table>';
end;

function TQuranViewer.CreateAyahTable(const ASura : TXmlElement;
                                      const AStartAyah : TAyahNum;
                                      const AEndAyah : TAyahNum) : String;
const
  NoteRefFmt = ' <sup><a href="fn:%0:s">%0:s</a></sup> ';
  AyahRowFmt = '<tr valign=top><td align=right><font color="silver"><b><a name="A%0:d" href="%3:s">%0:d</a></b></font></td><td><font color="%4:s">%1:s %2:s</td></tr>';
  AyahRowWithThemeFmt = '<tr valign=top><td align=right rowspan=%3:d><font color=%9:s size=%7:d face="%6:s">%4:s</font></td><td align=right><font color="silver"><b><a name="A%0:d" href="%5:s">%0:d</a></b></font></td><td><font color="%8:s">%1:s</font> %2:s</td></tr>';
var
  A, AC : Cardinal;
  AyahChild : TXmlNode;
  AyahElem : TXmlElement;
  AyahText, AyahSep, NoteId, SuraNumStr, CompareAnchor : String;
  Data : Pointer;
  AyahNum : TAyahNum;
  ThemeInfo : TThemeInfo;
  ThemeFont : TAmlDataFont;
  Color1, Color2 : String;
  FontColor : String;
begin
  if FBook.FlagIsSet(qdfShowAyahSep) then
    AyahSep := AyahSepHtml
  else
    AyahSep := '';

  SuraNumStr := ASura.GetAttribute('num');
  Result := '';
  Color1 := FBook.GetFont('ayah').HtmlColor;
  Color2 := FBook.GetFont('altayah').HtmlColor;
  ThemeFont := FBook.GetFont('theme');

  FontColor := Color1;
  for A := 0 to ASura.ChildNodes.Length-1 do begin
    if ASura.ChildNodes.Item(A).NodeType <> ELEMENT_NODE then
      continue;
    AyahElem := ASura.ChildNodes.Item(A) as TXmlElement;
    AyahNum := StrToInt(AyahElem.GetAttribute('num'));
    if (AyahNum < AStartAyah) or (AyahNum > AEndAyah) then
      continue;

    if FBook.FlagIsSet(qdfReciteOnClick) then
      CompareAnchor := CreateQuranAddress(qatReciteOne, StrToInt(SuraNumStr), AyahNum)
    else
      CompareAnchor := CreateQuranAddress(qatMultiQuran, StrToInt(SuraNumStr), AyahNum);

    AyahText := '';
    for AC := 0 to AyahElem.ChildNodes.Length-1 do begin
      AyahChild := AyahElem.ChildNodes.Item(AC);
      if AyahChild.NodeName = '' then begin
        AyahText := AyahText + AyahChild.NodeValue
      end else if AyahChild.NodeName = 'fn' then begin
        AyahText := AyahText + Format(NoteRefFmt, [(AyahChild as TXmlElement).Text]);
      end else if AyahChild.NodeName = 'note' then begin
        NoteId := (AyahChild as TXmlElement).GetAttribute('id');
        if not FActiveNotes.Exists(NoteId, Data) then
          FActiveNotes.Add(NoteId, AyahChild as TXmlElement);
      end;
    end;

    //if (FSearchHits <> '') and (AyahNum = FSearchLoc.ayahNum) then
    HighlightWords(AyahText, FSearchHits);

    if FBook.FlagIsSet(qdfShowThemes) and (FThemes <> Nil) and (FThemes.ThemesDict <> Nil) then begin
      if FThemes.ThemesDict.Exists(IntToStr(AyahNum), Data) then begin
        ThemeInfo := TThemeInfo(Data);
        Result := Result + Format(AyahRowWithThemeFmt, [AyahNum, AyahText, AyahSep, ThemeInfo.AyahCount, ThemeInfo.Theme, CompareAnchor, ThemeFont.Name, ThemeFont.HtmlSize, FontColor, ThemeFont.HtmlColor]);
      end else begin
        if FThemes.AyahHasTheme[AyahNum] then
          Result := Result + Format(AyahRowFmt, [AyahNum, AyahText, AyahSep, CompareAnchor, FontColor])
        else
          Result := Result + Format(AyahRowWithThemeFmt, [AyahNum, AyahText, AyahSep, 1, '', CompareAnchor, ThemeFont.Name, ThemeFont.HtmlSize, FontColor, ThemeFont.HtmlColor]);
      end;
    end else
      Result := Result + Format(AyahRowFmt, [AyahNum, AyahText, AyahSep, CompareAnchor, FontColor]);

    if FontColor = Color1 then FontColor := Color2 else FontColor :=Color1;
  end;

  Result := '<table cellspacing=3 cellpadding=1>' + Result + '</table>';
end;

function TQuranViewer.SuraToHtml(const ASura : TXmlElement;
                                 const AStartAyah : TAyahNum;
                                 const AEndAyah : TAyahNum) : String;
begin
  Assert(ASura <> Nil);
  Assert(AEndAyah >= AStartAyah);

  if FActiveNotes = Nil then
    FActiveNotes := TStDictionary.Create(256)
  else
    FActiveNotes.Clear;

  if FBook.FlagIsSet(qdfShowThemes) and (FThemes <> Nil) then
    FThemes.ReadThemes(StrToInt(ASura.GetAttribute('num')));

  if FBook.FlagIsSet(qdfIsArabic) then
    Result := CreateRtlAyahTable(ASura, AStartAyah, AEndAyah)
  else
    Result := CreateAyahTable(ASura, AStartAyah, AEndAyah);

  if FActiveNotes.Count > 0 then begin
    NotesSplitter.Visible := True;
    NotesViewer.Visible := True;
    NotesSplitter.Top := NotesViewer.Top - NotesSplitter.Height;
    NotesSplitter.Maximized := True;
  end else begin
    NotesSplitter.Visible := False;
    NotesViewer.Visible := False;
  end;
end;

procedure TQuranViewer.UpdateContents;
var
  Html : String;
begin
  Assert(FBook <> Nil);

  FBook.ReadSura(FActiveLoc.suraNum, True);
  if FBook.FSuraData = Nil then begin
    ClearContents;
    Exit;
  end;

  FBook.GetFont('ayah').AssignInHtmlViewer(HtmlViewer);
  FBook.GetFont('note').AssignInHtmlViewer(NotesViewer);

  FStatus.StatusBegin('Rendering...', True);
  Html := SuraToHtml(FBook.FSuraData);
  HtmlViewer.LoadFromBuffer(PChar(Html), Length(Html));
  NotesViewer.LoadFromBuffer(PChar(''), 0);
  FStatus.StatusEnd;
end;

procedure TQuranViewer.UpdateNavigation;
begin
  Assert(FBook <> Nil);
  Assert(FBook.FSuraData <> Nil);

  FillQuickLinksWithQurans(FBook.ShortName, FActiveLoc.suraNum, FActiveLoc.ayahNum, LocationPanel.Color);
  FAddrMgr.SetActiveSuraAyah(FActiveLoc);

  if FBook.QuranStruct <> Nil then begin
    with FBook.QuranStruct.Sura[FActiveLoc.suraNum] do begin
      Location.Caption := Format('Sura %d. %s', [suraNum, suraName, revealedInCityName, ayahCount]);
    end;
  end else begin
    Location.Caption := Format('Sura %d', [FActiveLoc.suraNum]);
  end;
end;

procedure TQuranViewer.SetAddress(const AAddress : TAddress);
var
  BookChanged : Boolean;
  NewBook : TAmlData;
  NewLoc : TSuraAyah;
  S, A : Integer;
begin
  NewBook := FDataMgr.DataIds[AAddress.BookId];
  if not (NewBook is TQuranData) then begin
    ClearContents;
    Exit;
  end;

  BookChanged := NewBook <> FBook;
  if BookChanged then begin
    FThemes := FDataMgr.Get(dslDataType, 'quran_themes') as TQuranThemesData;
    FBook := NewBook as TQuranData;
    HeadingPanel.Caption := FBook.Name;
    HtmlViewer.LoadFromBuffer(PChar(''), 0);

    HtmlViewer.NoSelect := FBook.FlagIsSet(qdfIsArabic) or (not FBook.FlagIsSet(adfCanCopyText));
    NotesViewer.NoSelect := HtmlViewer.NoSelect;
  end;

  Assert(FBook <> Nil);
  Assert(FBook.QuranStruct <> Nil);

  if AAddress.BookPage <> '' then begin
    try
      S := StrToInt(AAddress.Volume);
      A := StrToInt(AAddress.Element);
      if ((S >= Low(TSuraNum)) and (S <= High(TSuraNum))) and
         ((A >= Low(TAyahNum)) and (A <= FBook.QuranStruct.Sura[S].ayahCount)) then begin
        NewLoc.suraNum := S;
        NewLoc.ayahNum := A;
      end else
        NewLoc := FActiveLoc;
    except
      ShowMessage(Format('%s is not a valid Quran address', [AAddress.BookPage]));
      NewLoc := FActiveLoc;
    end;
    if AAddress.SearchHits <> '' then begin
      FSearchHits := AAddress.SearchHits;
      FSearchLoc := NewLoc;
    end else
      FSearchHits := '';
  end else
    NewLoc := FActiveLoc;

  if BookChanged or (NewLoc.suraNum <> FActiveLoc.suraNum) then begin
    FActiveLoc := NewLoc;
    UpdateContents;
  end else
    FActiveLoc.ayahNum := NewLoc.ayahNum;

  HtmlViewer.PositionTo('A'+IntToStr(FActiveLoc.ayahNum));
  UpdateNavigation;
end;

procedure TQuranViewer.ExecuteQuranCmd(const ACommand : TQuranCommand);
begin
  if FBook = Nil then
    Exit;

  case ACommand of
    qcArabicSearch  :
      if FBook.FlagIsSet(qdfIsArabic) then
        FBook.ArabicSearch
      else
        ShowMessage('Arabic Searching is performed in the Arabic Quran only. Please open the Arabic Quran and try again.');
    qcViewSuraIntro : GlobalSetAddress(CreateQuranAddress(qatSuraIntro, FActiveLoc.suraNum, 1));
    qcRecite : GlobalSetAddress(CreateQuranAddress(qatReciteContinue, FActiveLoc.suraNum, FActiveLoc.ayahNum));
  else
    inherited ExecuteQuranCmd(ACommand);
  end;
end;

function TQuranViewer.SelectSura(const ASura : TSuraNum) : Boolean;
begin
  FActiveLoc.suraNum := ASura;
  FActiveLoc.ayahNum := 1;
  UpdateContents;
  UpdateNavigation;
  Result := True;
end;

function TQuranViewer.GetActiveData : TAmlData;
begin
  Result := FBook;
end;

procedure TQuranViewer.InternalHtmlFootnoteCovered(Sender : TObject; const ANoteId : String);
var
  Data : Pointer;
  Html : String;
begin
  if (Sender = HtmlViewer) and (FActiveNotes <> Nil) then begin
    if FActiveNotes.Exists(ANoteId, Data) then begin
      Html := FootnoteToHtml(TXmlElement(Data), True, FSearchHits);
      NotesViewer.LoadFromBuffer(PChar(Html), Length(Html));
      if FBook.FlagIsSet(qdfAutoRevealNotes) then
        NotesSplitter.Maximized := False;
    end;
  end;
end;

procedure TQuranViewer.InternalHtmlFootnoteClick(Sender: TObject; const ANoteId : String);
var
  Data : Pointer;
  TextViewer : TTextViewer;
begin
  if (Sender = HtmlViewer) and (FActiveNotes <> Nil) then begin
    if FActiveNotes.Exists(ANoteId, Data) then begin
      TextViewer := TTextViewer.Create(Application);
      TextViewer.Heading := Format('%s - Note %s (Sura %d Ayah %d)', [FBook.ShortName, ANoteId, FActiveLoc.SuraNum, FActiveLoc.AyahNum]);
      TextViewer.Html := FootnoteToHtml(TXmlElement(Data), False, FSearchHits);
      TextViewer.Position := poScreenCenter;
      TextViewer.Show;
    end;
  end;
end;

procedure TQuranViewer.InternalHtmlRightClick(Sender: TObject; Parameters: TRightClickParameters);
begin
  if FContextMenu = Nil then
    FContextMenu := TRxPopupMenu.Create(Self)
  else
    ClearMenu(FContextMenu);

  if not FBook.FlagIsSet(qdfIsArabic) then begin
    AddContextMenuItem('Show Ayah Themes', -10, FBook.FlagIsSet(qdfShowThemes));
    AddContextMenuItem('Automatically Reveal Notes', -11, FBook.FlagIsSet(qdfAutoRevealNotes));
  end else
    AddContextMenuItem('Display Special Icons', -12, FBook.FlagIsSet(qdfViewArabicIcons));
  AddContextMenuItem('Separate Ayahs with Lines', -13, FBook.FlagIsSet(qdfShowAyahSep));
  AddContextMenuItem('Recite Ayah when Ayah Number is Clicked', -14, FBook.FlagIsSet(qdfReciteOnClick)).GroupIndex := 10;
  AddContextMenuItem('-');
  AddContextMenuItem('Change Ayah Font...', -21);
  AddContextMenuItem('Change Alternate Ayah Color...', -22);
  AddContextMenuItem('Change Ayah Color used in Quran Comparsion...', -23);
  if not FBook.FlagIsSet(qdfIsArabic) then begin
    AddContextMenuItem('Change Theme Font...', -24);
    AddContextMenuItem('Change Comment/Note Font...', -25);
  end;

  PopupContextMenu;
end;

procedure TQuranViewer.ContextMenuCommand(const ATag : LongInt);

  procedure Toggle(const AFlag : LongInt; const AUpdate : Boolean = True);
  begin
    FBook.SetFlag(AFlag, not FBook.FlagIsSet(AFlag));
    if AUpdate then UpdateContents;
  end;

  procedure FontChange(const AFontId : String; const AColorOnly : Boolean = False);
  begin
    if FBook.ChangeFont(AFontId, AColorOnly) then UpdateContents;
  end;

begin
  case ATag of
    -10 : Toggle(qdfShowThemes);
    -11 : Toggle(qdfAutoRevealNotes, False);
    -12 : Toggle(qdfViewArabicIcons);
    -13 : Toggle(qdfShowAyahSep);
    -14 : Toggle(qdfReciteOnClick);

    -21 : FontChange('ayah');
    -22 : FontChange('altayah', True);
    -23 : FontChange('compare', True);
    -24 : FontChange('theme');
    -25 : FontChange('note');
  end;
end;

function TQuranViewer.GetEnableCopy : Boolean;
begin
  Result := (FBook <> Nil) and FBook.FlagIsSet(adfCanCopyText) and (HtmlViewer.SelLength > 0);
end;

function TQuranViewer.GetEnablePrint : Boolean;
begin
  Result := (FBook <> Nil) and FBook.FlagIsSet(adfCanPrintText);
end;

procedure TQuranViewer.Print(const ADialog : TPrintDialog);
begin
  AyahSelectionForm.Action := acaPrint;
  AyahSelectionForm.DataMgr := FBook.Owner;
  AyahSelectionForm.QuranStruct := FBook.QuranStruct;
  AyahSelectionForm.BookId := FBook.Id;
  AyahSelectionForm.ShowModal;
end;

procedure TQuranViewer.CopyToClipboard;
begin
  Assert(FBook <> Nil);

  if HtmlViewer.SelLength > 0 then begin
    if HtmlViewer.SelLength < MaxCopyCharsToClipboard then begin
      HtmlViewer.CopyToClipboard;
      FStatus.StatusBegin(Format('%d characters copied to clipboard', [HtmlViewer.SelLength]), False, 0);
    end else
      ShowMessage('Too much text to copy. Please select less text then try again.');
  end;

  //else begin
  //  AyahSelectionForm.Action := acaCopy;
  //  AyahSelectionForm.DataMgr := FBook.Owner;
  //  AyahSelectionForm.QuranStruct := FBook.QuranStruct;
  //  AyahSelectionForm.BookId := FBook.Id;
  //  AyahSelectionForm.ShowModal;
  //end;
end;

procedure TQuranViewer.Customize;
begin
  ShowMessage('You can customize the Qur''an display by right-clicking in the window and choosing options.');
end;

end.
