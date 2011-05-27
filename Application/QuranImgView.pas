unit QuranImgView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RXSpin, ExtCtrls, Htmlview, AmlView, XmlObjModel, QuranStruct,
  CompDoc, QuranView, RXCtrls, islctrls, moneyctrls;

type
  TQuranImgData = class(TAmlData)
  protected
    FSura : Cardinal;
    FSuraData : TXmlElement;
    FPage : Cardinal;
    FPageData : TXmlElement;
    FPageFNameFmt : String;
    FMaxPage : Cardinal;

    function GetLibName : String; override;
    function CreateViewer : TAmlViewer; override;

    procedure SetPage(APage : Cardinal);
    function GetFirstAyah : TSuraAyah;
    function GetLastAyah : TSuraAyah;
    function GetSuraData(ASura : TSuraNum) : TXmlElement;

    procedure ClearActivePage; virtual;
    procedure ClearActiveSura; virtual;
  public
    destructor Destroy; override;
    function InitializeLoad(const AFileName : String;
                            const ACatalog : TXmlElement) : Boolean; override;
    function GetAddressCaption(const AAddress : TAddress; const AIncludeName : Boolean) : String; override;
    function GetAyahPage(Ayah : TSuraAyah) : Cardinal;

    property Page : Cardinal read FPage write SetPage;
    property PageData : TXmlElement read FPageData;
    property FirstAyah : TSuraAyah read GetFirstAyah;
    property LastAyah : TSuraAyah read GetLastAyah;
    property MaxPage : Cardinal read FMaxPage;
  end;

  TQuranImgViewer = class(TAmlViewer)
    HtmlViewer: THTMLViewer;
    LocationPanel: TWallpaperPanel;
    Location: TRxLabel;
    HeadingPanel: TWallpaperPanel;
    procedure HtmlViewerImageRequest(Sender: TObject; const SRC: String;
      var Stream: TMemoryStream);
  protected
    FActivePage : Cardinal;
    FBook : TQuranImgData;
    FPageImage : TMemoryStream;

    function GetActiveData : TAmlData; override;
    procedure ClearContents; virtual;
    procedure UpdateContents; virtual;
    procedure UpdateNavigation; virtual;
    procedure SetAddress(const AAddress : TAddress); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure Navigate(const AType : TNavigate; const ADirection : TNavigateDirection; const AAddress : TCaptionedAddress = Nil); override;
    procedure ExecuteQuranCmd(const ACommand : TQuranCommand); override;
    function SelectSura(const ASura : TSuraNum) : Boolean; override;

    property Book : TQuranImgData read FBook;
  end;

implementation

uses IslUtils;

{$R *.DFM}

destructor TQuranImgData.Destroy;
begin
  ClearActiveSura;
  ClearActivePage;
  inherited Destroy;
end;

procedure TQuranImgData.ClearActivePage;
begin
  if FPageData <> Nil then
    FPageData.Free;
  FPageData := Nil;
  FPage := 0;
end;

procedure TQuranImgData.ClearActiveSura;
begin
  if FSuraData <> Nil then
    FSuraData.Free;
  FSuraData := Nil;
  FSura := 0;
end;

function TQuranImgData.InitializeLoad(const AFileName : String;
                                      const ACatalog : TXmlElement) : Boolean;
begin
  Result := inherited InitializeLoad(AFileName, ACatalog);
  if Result then begin
    //FAddrMgr.AddShortcut(LibName, Format('%s:%s', [FId, '']), FName, '01' + FName).SmallIconIdx := BookIconSmallIdxQuran;
    FPageFNameFmt := FCatalogContent.GetAttribute('imgfmt');
    FMaxPage := StrToInt(FCatalogContent.GetAttribute('maxpage'));
  end;
  SetFlag(adfShareViewer+adfIsQuranData);
end;

function TQuranImgData.GetAddressCaption(const AAddress : TAddress; const AIncludeName : Boolean) : String;
begin
  if Pos(SuraAyahStrDelim, AAddress.BookPage) > 0 then begin
    if AIncludeName then
      Result := Format('%s (Sura %s Ayah %s)', [Name, AAddress.Volume, AAddress.Element])
    else
      Result := Format('Sura %s Ayah %s', [AAddress.Volume, AAddress.Element]);
  end else begin
    if AIncludeName then
      Result := Format('%s (Page %s)', [Name, AAddress.BookPage])
    else
      Result := 'Page ' + AAddress.BookPage;
  end;
end;

function TQuranImgData.GetLibName : String;
begin
  Result := 'Quran';
end;

function TQuranImgData.CreateViewer : TAmlViewer;
begin
  Result := TQuranImgViewer.Create(Application);
end;

function TQuranImgData.GetFirstAyah : TSuraAyah;
var
  AyahElem : TXmlElement;
begin
  Result.SuraNum := 1;
  Result.AyahNum := 1;

  if FPageData <> Nil then begin
    AyahElem := GetChildElem(FPageData, True);
    if AyahElem <> Nil then begin
      Result.SuraNum := StrToInt(AyahElem.GetAttribute('sura'));
      Result.AyahNum := StrToInt(AyahElem.GetAttribute('num'));
    end;
  end;
end;

function TQuranImgData.GetLastAyah : TSuraAyah;
var
  AyahElem : TXmlElement;
begin
  Result.SuraNum := 1;
  Result.AyahNum := 1;

  if FPageData <> Nil then begin
    AyahElem := GetChildElem(FPageData, False);
    if AyahElem <> Nil then begin
      Result.SuraNum := StrToInt(AyahElem.GetAttribute('sura'));
      Result.AyahNum := StrToInt(AyahElem.GetAttribute('num'));
    end;
  end;
end;

procedure TQuranImgData.SetPage(APage : Cardinal);
var
  StreamName : String;
begin
  if FPage = APage then
    Exit;

  ClearActivePage;
  FStatus.StatusBegin(Format('Loading Page %d...', [APage]), True);
  try
    StreamName := 'P'+IntToStr(APage);
    FPageData := ReadXML(StreamName);
    FPage := APage;
  except
    ShowMessage(Format('Page %s Data not found.', [StreamName]));
    FPageData := Nil;
    FPage := 0;
  end;

  FStatus.StatusEnd;
end;

function TQuranImgData.GetAyahPage(Ayah : TSuraAyah) : Cardinal;
var
  AyahsList : TXmlNodeList;
  AyahElem : TXmlElement;
begin
  GetSuraData(Ayah.SuraNum);
  Result := 0;
  if FSuraData <> Nil then begin
    AyahsList := FSuraData.GetElementsByTagNameWithAttribute('ayah', 'num', IntToStr(Ayah.AyahNum));
    if AyahsList.Length = 1 then begin
      AyahElem := AyahsList.Item(0) as TXmlElement;
      Result := StrToInt(AyahElem.GetAttribute('page'));
    end;
  end;
end;

function TQuranImgData.GetSuraData(ASura : TSuraNum) : TXmlElement;
begin
  if FSura = ASura then begin
    Result := FSuraData;
    Exit;
  end;

  ClearActiveSura;
  FStatus.StatusBegin(Format('Loading Sura %d...', [ASura]), True);
  try
    FSuraData := ReadXML('S'+IntToStr(ASura));
    FSura := ASura;
  except
    FSuraData := Nil;
    FSura := 0;
  end;
  FStatus.StatusEnd;
  Result := FSuraData;
end;

{------------------------------------------------------------------------------}

constructor TQuranImgViewer.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FActivePage := 1;

  HtmlViewer.OnHotSpotCovered := DefaultHtmlHotSpotCovered;
  HtmlViewer.OnHotSpotClick := DefaultHtmlHotspotClick;
  HtmlViewer.OnRightClick := DefaultHtmlRightClick;

  // the image request is special for this viewer
  //HtmlViewer.OnImageRequest := DefaultHtmlImageRequest;
end;

destructor TQuranImgViewer.Destroy;
begin
  if FPageImage <> Nil then
    FPageImage.Free;
  inherited Destroy;
end;

procedure TQuranImgViewer.Navigate(const AType : TNavigate; const ADirection : TNavigateDirection; const AAddress : TCaptionedAddress);
begin
  if (FBook = Nil) or (ADirection = nvdCaptionedAddr) or (AType <> nvPage) then
    Exit;

  case ADirection of
    nvdPrevious :
      if FActivePage > 1 then
        FActivePage := Pred(FActivePage)
      else
        FActivePage := FBook.MaxPage;
    nvdNext :
      if FActivePage < FBook.MaxPage then
        FActivePage := Succ(FActivePage)
      else
        FActivePage := 1;
  end;
  UpdateContents;
  UpdateNavigation;
end;

function TQuranImgViewer.GetActiveData : TAmlData;
begin
  Result := FBook;
end;

procedure TQuranImgViewer.ClearContents;
begin
  FActivePage := 0;
  HtmlViewer.LoadFromBuffer(PChar(''), 0);
end;

procedure TQuranImgViewer.UpdateContents;
const
  PageImageWidth = 456;
  SuraNameBoxHeight = 115;
  OneLineHeight = 45;
  MinimumLineHeight = 25;

  function GetAyahBoundingBox(x, y : Cardinal) : TRect;
  begin
    if x < PageImageWidth then begin
      Result.Left := x - 17;
      Result.Right := x + 17;
    end else begin
      Result.Left := PageImageWidth;
      Result.Right := PageImageWidth;
    end;
    if y > 0 then begin
      Result.Top := y - 7;
      Result.Bottom := y + 37;
    end else begin
      Result.Top := 0;
      Result.Bottom := 0;
    end;
  end;

  function CreateImageMap(Ayahs : TXmlElement) : String;
  const
    AreaFmt = '<area shape="rect" coords="%d,%d,%d,%d" href="%s">';
  var
    A, xStart, xEnd, yStart, yEnd, AyahNum, SuraNum : Cardinal;
    AyahImgHeight : Cardinal;
    AyahElem : TXmlElement;
    StartRect, EndRect : TRect;
    MultiQuranRef : String;
  begin
    Result := '';
    if (Ayahs = Nil) or (Ayahs.ChildNodes.Length <= 0) then
      Exit;

    for A := 0 to Ayahs.ChildNodes.Length-1 do begin
      if Ayahs.ChildNodes.Item(A).NodeType <> ELEMENT_NODE then
        continue;
      AyahElem := Ayahs.ChildNodes.Item(A) as TXmlElement;

      SuraNum := StrToInt(AyahElem.GetAttribute('sura'));
      AyahNum := StrToInt(AyahElem.GetAttribute('num'));
      xStart := StrToInt(AyahElem.GetAttribute('xstart'));
      xEnd := StrToInt(AyahElem.GetAttribute('xend'));
      yStart := StrToInt(AyahElem.GetAttribute('ystart'));
      yEnd := StrToInt(AyahElem.GetAttribute('yend'));
      MultiQuranRef := CreateQuranAddress(qatMultiQuran, SuraNum, AyahNum);

      // starting ayahs have the sura name block above them
      if AyahNum = 1 then
        StartRect := GetAyahBoundingBox(PageImageWidth, yStart+SuraNameBoxHeight)
      else
        StartRect := GetAyahBoundingBox(xStart, yStart);
      EndRect := GetAyahBoundingBox(xEnd, yEnd);

      // if the ayah doesn't start on the same line as it "number block", go to the next line
      if (StartRect.Left < 50) and (StartRect.Left > 0) then begin
        StartRect.Left := PageImageWidth;
        StartRect.Right := PageImageWidth;
        StartRect.Top := StartRect.Top + OneLineHeight;
      end;

      AyahImgHeight := EndRect.Bottom - StartRect.Top;
      if(AyahImgHeight < MinimumLineHeight) then
        continue;

      Result := Result + Format(AreaFmt, [EndRect.Left, EndRect.Top, EndRect.Right, EndRect.Bottom, MultiQuranRef]);
    end;
    Result := '<map name="ayahs">' + Result + '</map>';
  end;

var
  Html : String;
begin
  Assert(FBook <> Nil);

  FBook.Page := FActivePage;
  if FBook.FPageData = Nil then begin
    ClearContents;
    Exit;
  end;

  FStatus.StatusBegin('Rendering...', True);
  Html := Format('<center>%s<img src="%d" usemap="#ayahs"></center>', [CreateImageMap(FBook.PageData), FActivePage]);
  HtmlViewer.LoadFromBuffer(PChar(Html), Length(Html));
  FStatus.StatusEnd;
end;

procedure TQuranImgViewer.UpdateNavigation;
var
  FirstAyah, LastAyah : TSuraAyah;
  FirstName, LastName : String;
begin
  Assert(FBook <> Nil);

  FirstAyah := FBook.FirstAyah;
  LastAyah := FBook.LastAyah;
  FillQuickLinksWithQurans(FBook.ShortName, FirstAyah.suraNum, FirstAyah.ayahNum, LocationPanel.Color);
  FAddrMgr.SetActiveSuraAyah(FirstAyah);

  if FBook.QuranStruct <> Nil then begin
    FirstName := FBook.QuranStruct.Sura[FirstAyah.SuraNum].suraName;
    LastName := FBook.QuranStruct.Sura[LastAyah.SuraNum].suraName;

    if FirstName = LastName then
      Location.Caption := Format('Page %d (Sura %d. %s Ayaat %d to %d)', [FActivePage, FirstAyah.SuraNum, FirstName, FirstAyah.AyahNum, LastAyah.AyahNum])
    else
      Location.Caption := Format('Page %d (Sura %s Ayah %d to Sura %s Ayah %d)', [FActivePage, FirstName, FirstAyah.AyahNum, LastName, LastAyah.AyahNum]);
  end else begin
    FirstName := '';
    LastName := '';
    if FirstAyah.SuraNum = LastAyah.SuraNum then
      Location.Caption := Format('Page %d (Sura %d Ayaat %d to %d)', [FActivePage, FirstAyah.SuraNum, FirstAyah.AyahNum, LastAyah.AyahNum])
    else
      Location.Caption := Format('Page %d (Sura %d Ayah %d to Sura %d Ayah %d)', [FActivePage, FirstAyah.SuraNum, FirstAyah.AyahNum, LastAyah.SuraNum, LastAyah.AyahNum]);
  end;
end;

procedure TQuranImgViewer.SetAddress(const AAddress : TAddress);
var
  BookChanged : Boolean;
  NewBook : TAmlData;
  NewPage : Integer;
  NewAyah : TSuraAyah;
begin
  NewBook := FDataMgr.DataIds[AAddress.BookId];
  if not (NewBook is TQuranImgData) then begin
    ClearContents;
    Exit;
  end;

  BookChanged := NewBook <> FBook;
  if BookChanged then begin
    FBook := NewBook as TQuranImgData;
  end;

  Assert(FBook <> Nil);
  Assert(FBook.QuranStruct <> Nil);

  if AAddress.BookPage <> '' then begin
    if Pos(SuraAyahStrDelim, AAddress.BookPage) > 0 then begin
      NewAyah := FBook.QuranStruct.StrToSuraAyah(AAddress.BookPage, SuraAyah(1, 1));
      NewPage := FBook.GetAyahPage(NewAyah)
    end else
      NewPage := StrToInt(AAddress.BookPage);
  end else
    NewPage := FActivePage;

  if BookChanged or (Cardinal(NewPage) <> FActivePage) then begin
    FActivePage := NewPage;
    UpdateContents;
  end;
  UpdateNavigation;
end;

procedure TQuranImgViewer.ExecuteQuranCmd(const ACommand : TQuranCommand);
var
  FirstAyah : TSuraAyah;
begin
  if FBook = Nil then
    Exit;

  FirstAyah := FBook.FirstAyah;
  case ACommand of
    qcArabicSearch  :
      ShowMessage('Arabic Searching is performed in the Arabic Quran (text) only. Please open the Arabic Quran (text) and try again.');
    qcViewSuraIntro : GlobalSetAddress(CreateQuranAddress(qatSuraIntro, FirstAyah.suraNum, 1));
    qcRecite : GlobalSetAddress(CreateQuranAddress(qatReciteContinue, FirstAyah.suraNum, FirstAyah.ayahNum));
  else
    inherited ExecuteQuranCmd(ACommand);
  end;
end;

function TQuranImgViewer.SelectSura(const ASura : TSuraNum) : Boolean;
var
  Active : TSuraAyah;
begin
  Assert(FBook <> Nil);
  Active.suraNum := ASura;
  Active.ayahNum := 1;
  FActivePage := FBook.GetAyahPage(Active);
  if FActivePage >= 1 then begin
    UpdateContents;
    UpdateNavigation;
  end else
    ClearContents;
  Result := True;
end;

procedure TQuranImgViewer.HtmlViewerImageRequest(Sender: TObject;
  const SRC: String; var Stream: TMemoryStream);
var
  StorageName : String;
begin
  Assert(FBook <> Nil);

  if FPageImage <> Nil then
    FPageImage.Free;

  FStatus.StatusBegin('Loading page...', True);
  StorageName := Format(FBook.FPageFNameFmt, [StrToInt(SRC)]);
  FPageImage := FBook.ReadStream(StorageName);
  if FPageImage = Nil then
    ShowMessage(Format('Error reading page Image %s.', [StorageName]));
  Stream := FPageImage;
  FStatus.StatusEnd;
end;

end.
