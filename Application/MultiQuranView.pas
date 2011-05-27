unit MultiQuranView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst, Htmlview, ExtCtrls, DFSSplitter, RXCtrls, islctrls,
  AmlView, IslUtils, QuranStruct, CompDoc, QuranView, StDict, XmlObjModel,
  TextView, RXCombos, Menus, RxMenus, HtmlUn2, moneyctrls, AyahComposer;

type
  TSingleQuranInfoArray = array of TQuranData;

  TMultiQuranData = class(TAmlData)
  protected
    FAyahs : TObjDict;
    FQurans : TSingleQuranInfoArray;

    function GetLibName : String; override;
    function CreateViewer : TAmlViewer; override;
  public
    destructor Destroy; override;
    function InitializeLoad(const AFileName : String;
                            const ACatalog : TXmlElement) : Boolean; override;
    procedure FinalizeLoad; override;
    function GetAddressCaption(const AAddress : TAddress; const AIncludeName : Boolean) : String; override;
    function ReadSuraAyah(const ALoc : TSuraAyah) : TObjDict;
    property Qurans : TSingleQuranInfoArray read FQurans;
  end;

  TMultiQuranViewer = class(TAmlViewer)
    SubHeadingPanel: TWallpaperPanel;
    Location: TRxLabel;
    ContentsPanel: TPanel;
    ToolsPanel: TWallpaperPanel;
    MainPanel: TPanel;
    NotesSplitter: TDFSSplitter;
    NotesViewer: THTMLViewer;
    HtmlViewer: THTMLViewer;
    CompareList: TCheckListBox;
    HeadingPanel: TWallpaperPanel;
    AyahSpinBtn: TMoneySpinButton;
    Image1: TImage;
    Panel1: TPanel;
    MoneyLabel1: TMoneyLabel;
    procedure CompareListClickCheck(Sender: TObject);
    procedure CompareListDblClick(Sender: TObject);
    procedure LabelSuraIntroClick(Sender: TObject);
    procedure AyahSpinBtnUpClick(Sender: TObject);
    procedure AyahSpinBtnDownClick(Sender: TObject);
    procedure MoneyLabel1Click(Sender: TObject);
  protected
    FBook : TMultiQuranData;
    FActiveLoc : TSuraAyah;
    FActiveNotes : TStDictionary;
    FShowAyahSep : Boolean;
    FAutoRevealNotes : Boolean;
    FSearchLoc : TSuraAyah;

    procedure ClearContents; virtual;
    procedure UpdateContents; virtual;
    procedure UpdateNavigation; virtual;
    procedure FillBookList; virtual;

    procedure SetAddress(const AAddress : TAddress); override;
    function GetActiveData : TAmlData; override;
    procedure InternalHtmlFootnoteCovered(Sender : TObject; const ANoteId : String); override;
    procedure InternalHtmlFootnoteClick(Sender: TObject; const ANoteId : String); override;
    function GetEnableCopy : Boolean; override;
    function GetEnablePrint : Boolean; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure Customize; override;
    procedure Navigate(const AType : TNavigate; const ADirection : TNavigateDirection; const AAddress : TCaptionedAddress = Nil); override;
    procedure ExecuteQuranCmd(const ACommand : TQuranCommand); override;
    function SelectSura(const ASura : TSuraNum) : Boolean; override;

    property Book : TMultiQuranData read FBook;
    procedure Print(const ADialog : TPrintDialog); override;
    procedure CopyToClipboard; override;
  end;

implementation

uses Main, HtmlSubs;

{$R *.DFM}

destructor TMultiQuranData.Destroy;
begin
  if FAyahs <> Nil then begin
    FAyahs.Free;
    FAyahs := Nil;
  end;
  inherited Destroy;
end;

function TMultiQuranData.InitializeLoad(const AFileName : String;
                                        const ACatalog : TXmlElement) : Boolean;
var
  BN : TBookmarkNode;
begin
  // don't do anything with inherited data -- we're not a real file
  // Result := inherited InitializeLoad(AFile, AProperties);

  Result := True;
  if Result then begin
    FFileName := 'quran_multi';
    FContentType := 'quran';
    FContentSubType := 'multi';
    FId := MultiQuranId;
    FName := 'Quran Comparison';
    FShortName := 'Quran Comparison';
    //FFile := AFile;
    FCatalog := ACatalog;

    BN := FAddrMgr.AddShortcut(LibName, Format('%s:%s', [FId, '']), FName, '000' { move up in the list });
    BN.SmallIconIdx := DefaultBookIconIdxMultiBook;
    BN.Shortcut := 'F3';
    BN.BreakAfter := True;
    SetFlag(adfIsQuranData);
  end;
  SetFlag(adfShareViewer);
end;

procedure TMultiQuranData.FinalizeLoad;
var
  QuranList : TDataList;
  I : Integer;
begin
  QuranList := FOwner.DataTypes['quran_text'];
  if QuranList <> Nil then begin
    SetLength(FQurans, QuranList.Count);
    if QuranList.Count > 0 then
      for I := 0 to QuranList.Count-1 do
        FQurans[I] := QuranList.Data[I] as TQuranData;
  end else
    SetLength(FQurans, 0);
end;

function TMultiQuranData.GetAddressCaption(const AAddress : TAddress; const AIncludeName : Boolean) : String;
begin
  if AIncludeName then
    Result := Format('%s (Sura %s Ayah %s)', [Name, AAddress.Volume, AAddress.Element])
  else
    Result := Format('Sura %s Ayah %s', [AAddress.Volume, AAddress.Element]);
end;

function TMultiQuranData.GetLibName : String;
begin
  Result := 'Quran';
end;

function TMultiQuranData.CreateViewer : TAmlViewer;
begin
  Result := TMultiQuranViewer.Create(Application);
end;

function TMultiQuranData.ReadSuraAyah(const ALoc : TSuraAyah) : TObjDict;
var
  Q : Integer;
  AyahNumStr : String;

  procedure FillAyah(QuranData : TQuranData);
  begin
    try
      FAyahs.AddObject(QuranData.Id, QuranData.ReadAyah(ALoc.SuraNum, ALoc.AyahNum));
    except
      FAyahs.AddObject(QuranData.Id, Nil);
    end;
  end;

begin
  Assert(FQurans <> Nil);
  Assert(Length(FQurans) > 0);

  AyahNumStr := IntToStr(ALoc.ayahNum);
  if FAyahs = Nil then begin
    FAyahs := TObjDict.Create;
    FAyahs.OwnTheData := True;
    FAyahs.Sorted := False;
  end else
    FAyahs.Clear;

  // make sure the Arabic quran shows up on the top
  for Q := Low(FQurans) to High(FQurans) do begin
    if FQurans[Q].FlagIsSet(qdfShowInCompare) and FQurans[Q].FlagIsSet(qdfIsArabic) then
      FillAyah(FQurans[Q]);
  end;

  // now get the rest of the qurans
  for Q := Low(FQurans) to High(FQurans) do begin
    if FQurans[Q].FlagIsSet(qdfShowInCompare) and (not FQurans[Q].FlagIsSet(qdfIsArabic)) then
      FillAyah(FQurans[Q]);
  end;
  Result := FAyahs;
end;

{------------------------------------------------------------------------------}

constructor TMultiQuranViewer.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FActiveLoc.suraNum := 1;
  FActiveLoc.ayahNum := 1;
  FAutoRevealNotes := True;
  FShowAyahSep := True;

  SetupHtmlViewerDefaults(HtmlViewer);
  SetupHtmlViewerDefaults(NotesViewer);
end;

destructor TMultiQuranViewer.Destroy;
begin
  if FActiveNotes <> Nil then
    FActiveNotes.Free;
  inherited Destroy;
end;

procedure TMultiQuranViewer.Navigate(const AType : TNavigate; const ADirection : TNavigateDirection; const AAddress : TCaptionedAddress);
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

procedure TMultiQuranViewer.Customize;
begin
  ToolsPanel.Visible := True;
end;

procedure TMultiQuranViewer.ClearContents;
begin
  FActiveLoc.suraNum := 1;
  FActiveLoc.ayahNum := 1;
  HtmlViewer.LoadFromBuffer(PChar(''), 0);
end;

procedure TMultiQuranViewer.FillBookList;
var
  I : Integer;
begin
  Assert(FBook <> Nil);
  if Length(FBook.FQurans) > 0 then begin
    CompareList.Items.Clear;
    for I := Low(FBook.FQurans) to High(FBook.FQurans) do begin
      CompareList.Items.AddObject(FBook.FQurans[I].ShortName, FBook.FQurans[I]);
      CompareList.Checked[I] := FBook.FQurans[I].FlagIsSet(qdfShowInCompare);
    end;
  end;
  CompareList.Height := CompareList.ItemHeight * Succ(Length(FBook.FQurans));
end;

procedure TMultiQuranViewer.UpdateContents;
const
  AyahSepHtml = '<hr width=100% color=silver size=1>';
  NoteRefFmt = ' <sup><a href="fn:%0:s_%1:s">%1:s</a></sup> ';
  BookAyahRowFmt =
    '<tr valign=top>'+
    '  <td align=right>'+
    '    <font color="silver" size=2 face="Arial">'+
    '      <a href="%0:s:%1:d.%2:d">%3:s</a>%6:s'+
    '    </font>'+
    '  </td>'+
    '  <td>'+
    '    %4:s %5:s'+
    '  </td>'+
    '</tr>';
var
  AllowSelect : Boolean;
  A, AC : Cardinal;
  AyahChild : TXmlNode;
  AyahElem : TXmlElement;
  BookId, ShowName, AyahText, AyahSep, NoteId, ExtraId : String;
  Quran : TQuranData;
  Data : Pointer;
  Ayahs : TObjDict;
  AyahFont, CompareFont : TAmlDataFont;
  Html : String;
begin
  Assert(FBook <> Nil);

  if FActiveNotes = Nil then
    FActiveNotes := TStDictionary.Create(256)
  else
    FActiveNotes.Clear;

  Ayahs := FBook.ReadSuraAyah(FActiveLoc);

  if (Ayahs = Nil) or (Ayahs.Count <= 0) then begin
    ClearContents;
    Exit;
  end;

  if FShowAyahSep then
    AyahSep := AyahSepHtml
  else
    AyahSep := '';

  AllowSelect := True;
  Html := '';
  for A := 0 to Ayahs.Count-1 do begin
    BookId := Ayahs[A];
    Quran := FDataMgr.DataIds[BookId] as TQuranData;
    if AllowSelect then AllowSelect := Quran.FlagIsSet(adfCanCopyText) and not Quran.FlagIsSet(qdfIsArabic);
    ShowName := Quran.ShortName;
    Assert(Quran <> Nil);

    AyahElem := Ayahs.Objects[A] as TXmlElement;
    if AyahElem = Nil then begin
      Html := Html + Format('<tr valign=top><td align=right>%s</td><td><b><font color=red>Unable to read Sura %d Ayah %d from this book.</b> Please report this problem to ISL Software.</font>%s</td></tr>', [ShowName, FActiveLoc.suraNum, FActiveLoc.ayahNum, AyahSep]);
      continue;
    end;

    AyahText := '';
    for AC := 0 to AyahElem.ChildNodes.Length-1 do begin
      AyahChild := AyahElem.ChildNodes.Item(AC);
      if AyahChild.NodeName = '' then begin
        AyahText := AyahText + AyahChild.NodeValue
      end else if AyahChild.NodeName = 'fn' then begin
        AyahText := AyahText + Format(NoteRefFmt, [BookId, (AyahChild as TXmlElement).Text]);
      end else if AyahChild.NodeName = 'note' then begin
        NoteId := Format('%s_%s', [BookId, (AyahChild as TXmlElement).GetAttribute('id')]);
        if not FActiveNotes.Exists(NoteId, Data) then
          FActiveNotes.Add(NoteId, AyahChild as TXmlElement);
      end;
    end;
    if AyahText = '' then
      AyahText := Format('<font color=red>Unable to read Sura %d Ayah %d from %s</font>', [FActiveLoc.suraNum, FActiveLoc.ayahNum, ShowName]);

    // HTMLViewer was modified to show Arabic RTL when alignment was "Right"
    // and font was one of RtlFonts (like Zr)
    AyahFont := Quran.GetFont('ayah');
    CompareFont := Quran.GetFont('compare');
    if Quran.FlagIsSet(qdfIsArabic) then begin
      AyahText := '<font face="'+AyahFont.Name+'" size=6 color="'+CompareFont.HtmlColor+'"><p align="right">'+Reverse(AyahText)+'</p></font>';
      ShowName := 'Arabic Text';
      //BookId := ArabicQuranPagesId;
      ExtraId := Format('<br><a href="%s"><img src="#18" border=0></a><a href="%s"><img src="#28" border=0></a>', [CreateQuranAddress(qatArabicImages, FActiveLoc.suraNum, FActiveLoc.ayahNum), CreateQuranAddress(qatReciteOne, FActiveLoc.suraNum, FActiveLoc.ayahNum)]);
    end else begin
      AyahText := '<font face="'+AyahFont.Name+'" color="'+CompareFont.HtmlColor+'">'+AyahText+'</font>';
      ExtraId := '';
    end;

    if FSearchHits <> '' then begin
      if (FActiveLoc.suraNum = FSearchLoc.suraNum) and (FActiveLoc.ayahNum = FSearchLoc.ayahNum) then
        HighlightWords(AyahText, FSearchHits);
    end;

    Html := Html + Format(BookAyahRowFmt, [BookId, FActiveLoc.suraNum, FActiveLoc.ayahNum, ShowName, AyahText, AyahSep, ExtraId, AyahFont.Name, CompareFont.HtmlColor]);
  end;
  Html := '<table cellspacing=3 cellpadding=1>' + Html + '</table>';
  HtmlViewer.LoadFromBuffer(PChar(Html), Length(Html));

  HtmlViewer.NoSelect := not AllowSelect;
  NotesViewer.NoSelect := HtmlViewer.NoSelect;
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

procedure TMultiQuranViewer.UpdateNavigation;
begin
  Assert(FBook <> Nil);

  FillQuickLinksWithQurans(FBook.ShortName, FActiveLoc.suraNum, FActiveLoc.ayahNum,
                           SubheadingPanel.Color);
  FAddrMgr.SetActiveSuraAyah(FActiveLoc);

  if FBook.QuranStruct <> Nil then begin
    with FBook.QuranStruct.Sura[FActiveLoc.suraNum] do begin
      Location.Caption := Format('Sura %d. %s Ayah %d', [suraNum, suraName, FActiveLoc.ayahNum]);
    end;
  end else begin
    Location.Caption := Format('Sura %d Ayah %d', [FActiveLoc.suraNum, FActiveLoc.ayahNum]);
  end;
  FPrintHeader.BookPage := Location.Caption;
end;

procedure TMultiQuranViewer.SetAddress(const AAddress : TAddress);
var
  BookChanged : Boolean;
  NewBook : TAmlData;
  S, A : Integer;
begin
  NewBook := FDataMgr.DataIds[AAddress.BookId];
  if not (NewBook is TMultiQuranData) then begin
    ClearContents;
    Exit;
  end;

  BookChanged := NewBook <> FBook;
  if BookChanged then begin
    FBook := NewBook as TMultiQuranData;
    HeadingPanel.Caption := FBook.Name;
    FillBookList;
    FPrintHeader.BookName := FBook.Name;
  end;

  Assert(FBook <> Nil);
  Assert(FBook.QuranStruct <> Nil);

  if AAddress.BookPage <> '' then begin
    S := StrToInt(AAddress.Volume);
    A := StrToInt(AAddress.Element);
    if ((S >= Low(TSuraNum)) and (S <= High(TSuraNum))) and
       ((A >= Low(TAyahNum)) and (A <= FBook.QuranStruct.Sura[S].ayahCount)) then begin
      FActiveLoc.suraNum := S;
      FActiveLoc.ayahNum := A;
    end;
    if AAddress.SearchHits <> '' then begin
      FSearchHits := AAddress.SearchHits;
      FSearchLoc := FActiveLoc;
    end else
      FSearchHits := '';
  end;

  UpdateContents;
  UpdateNavigation;
end;

procedure TMultiQuranViewer.ExecuteQuranCmd(const ACommand : TQuranCommand);
begin
  if FBook = Nil then
    Exit;

  case ACommand of
    qcArabicSearch  :
      ShowMessage('Arabic Searching is performed in the Arabic Quran only. Please open the Arabic Quran and try again.');
    qcViewSuraIntro : GlobalSetAddress(CreateQuranAddress(qatSuraIntro, FActiveLoc.suraNum, 1));
    qcRecite : GlobalSetAddress(CreateQuranAddress(qatReciteContinue, FActiveLoc.suraNum, FActiveLoc.ayahNum));
  else
    inherited ExecuteQuranCmd(ACommand);
  end;
end;

function TMultiQuranViewer.SelectSura(const ASura : TSuraNum) : Boolean;
var
  Addr : TAddress;
begin
  Addr := TAddress.Create;
  Addr.BookId := FBook.Id;
  Addr.BookPage := Format('%d.1', [ASura]);
  SetAddress(Addr);
  Addr.Free;
  Result := True;
end;

procedure TMultiQuranViewer.CompareListClickCheck(Sender: TObject);
begin
  Assert(FBook <> Nil);
  FBook.FQurans[CompareList.ItemIndex].SetFlag(qdfShowInCompare, not FBook.FQurans[CompareList.ItemIndex].FlagIsSet(qdfShowInCompare));
  //FillBookList;
  UpdateContents;
end;

procedure TMultiQuranViewer.CompareListDblClick(Sender: TObject);
var
  I : Integer;
begin
  Assert(FBook <> Nil);
  Assert(Length(FBook.FQurans) > 0);
  for I := 0 to High(FBook.FQurans) do begin
    // show only the one double-clicked
    FBook.FQurans[I].SetFlag(qdfShowInCompare, I = CompareList.ItemIndex);
    CompareList.Checked[I] := FBook.FQurans[I].FlagIsSet(qdfShowInCompare);
  end;
  FillBookList;
  UpdateContents;
end;

function TMultiQuranViewer.GetActiveData : TAmlData;
begin
  Result := FBook;
end;

procedure TMultiQuranViewer.InternalHtmlFootnoteCovered(Sender : TObject; const ANoteId : String);
var
  Data : Pointer;
  Html : String;
begin
  if (Sender = HtmlViewer) and (FActiveNotes <> Nil) then begin
    if FActiveNotes.Exists(ANoteId, Data) then begin
      Html := FootnoteToHtml(TXmlElement(Data), True, FSearchHits);
      NotesViewer.LoadFromBuffer(PChar(Html), Length(Html));
      if FAutoRevealNotes then
        NotesSplitter.Maximized := False;
    end;
  end;
end;

procedure TMultiQuranViewer.InternalHtmlFootnoteClick(Sender: TObject; const ANoteId : String);
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

function TMultiQuranViewer.GetEnableCopy : Boolean;
begin
  Result := HtmlViewer.SelLength > 0;
end;

function TMultiQuranViewer.GetEnablePrint : Boolean;
begin
  Result := True;
end;

procedure TMultiQuranViewer.Print(const ADialog : TPrintDialog);
begin
  AyahSelectionForm.Action := acaPrint;
  AyahSelectionForm.DataMgr := FBook.Owner;
  AyahSelectionForm.QuranStruct := FBook.QuranStruct;
  AyahSelectionForm.ShowModal;
end;

procedure TMultiQuranViewer.CopyToClipboard;
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
  //  AyahSelectionForm.ShowModal;
  //end;
end;

procedure TMultiQuranViewer.LabelSuraIntroClick(Sender: TObject);
begin
  GlobalSetAddress(CreateQuranAddress(qatSuraIntro, FActiveLoc.SuraNum));
end;

procedure TMultiQuranViewer.AyahSpinBtnUpClick(Sender: TObject);
begin
  Navigate(nvItem, nvdPrevious);
end;

procedure TMultiQuranViewer.AyahSpinBtnDownClick(Sender: TObject);
begin
  Navigate(nvItem, nvdNext);
end;

procedure TMultiQuranViewer.MoneyLabel1Click(Sender: TObject);
begin
  ToolsPanel.Visible := False;
end;

end.
