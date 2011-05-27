unit HadithView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Htmlview, ExtCtrls, RXCtrls, islctrls, AmlView, XmlObjModel, CompDoc,
  StdCtrls, jpeg, IndexStruct, sEdits, moneyctrls;

const
  HadithNumStrDelim = '.';

type
  THadithFiqhData = class(TAmlData)
  protected
    FIsFiqh : Boolean;

    function GetLibName : String; override;
    function CreateViewer : TAmlViewer; override;
  public
    function InitializeLoad(const AFileName : String;
                            const ACatalog : TXmlElement) : Boolean; override;
    function GetAddressCaption(const AAddress : TAddress; const AIncludeName : Boolean) : String; override;

    property IsFiqh : Boolean read FIsFiqh;
  end;

  THadithViewer = class(TAmlViewer)
    ContentsPanel: TPanel;
    HtmlViewer: THTMLViewer;
    NavgPanel: TPanel;
    GotoPanel: TWallpaperPanel;
    InfoPanel: TWallpaperPanel;
    InfoViewer: THTMLViewer;
    GotoLabel: TRxLabel;
    VolElemEdit: TVolElemEdit;
    HeadingPanel: TWallpaperPanel;
    ImgHadith: TImage;
    ImgFiqh: TImage;
    NextLabel2: TMoneyLabel;
    PrevLabel2: TMoneyLabel;
    FillerPanel: TWallpaperPanel;
    NavgCloseLabel: TMoneyLabel;
    OpenNavgLabel: TMoneyLabel;
    PrevLabel1: TMoneyLabel;
    NextLabel1: TMoneyLabel;
    CustomizeLabel: TMoneyLabel;
    procedure VolElemEditEnterKey(Sender: TObject);
    procedure PrevLabel2Click(Sender: TObject);
    procedure NextLabel2Click(Sender: TObject);
    procedure NavgCloseLabelClick(Sender: TObject);
    procedure OpenNavgLabelClick(Sender: TObject);
    procedure CustomizeLabelClick(Sender: TObject);
  protected
    FBook : THadithFiqhData;
    FAmlData : TXmlElement;
    FAmlIndexes : TXmlNodeList;
    FNarrator : String;
    FActiveVol : String;
    FActiveElem : String;

    function GetActiveData : TAmlData; override;

    procedure ClearContents; virtual;
    procedure UpdateContents; virtual;
    procedure UpdateNavigation; virtual;
    procedure SetActiveBook(const ABook : THadithFiqhData); virtual;
    procedure SetActivePage(const AKey : String); virtual;
    procedure SetAddress(const AAddress : TAddress); override;
    procedure SetNavgPanelDisplay(const AShow : Boolean);

    function GetEnableCopy : Boolean; override;
    function GetEnablePrint : Boolean; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Navigate(const AType : TNavigate; const ADirection : TNavigateDirection; const AAddress : TCaptionedAddress = Nil); override;
    procedure Print(const ADialog : TPrintDialog); override;
    procedure CopyToClipboard; override;
  end;

implementation

uses StStrS;

{$R *.DFM}

function THadithFiqhData.InitializeLoad(const AFileName : String;
                                        const ACatalog : TXmlElement) : Boolean;
var
  NarrIdx : TXmlElement;
begin
  Result := inherited InitializeLoad(AFileName, ACatalog);
  if Result then begin
    FIsFiqh := CompareText(ContentType, 'fiqh') = 0;
    if (FCatalogIndexes <> Nil) then begin
      NarrIdx := FCatalogIndexes.FindElement('narrators');
      if NarrIdx <> Nil then
        FIndexNames.Values['Narrators'] := NarrIdx.GetAttribute('storage');
    end;
  end;
  SetFlag(adfShareViewer);
end;

function THadithFiqhData.GetAddressCaption(const AAddress : TAddress; const AIncludeName : Boolean) : String;
begin
  if FIsFiqh then begin
    if AIncludeName then
      Result := Format('%s (Fiqh %s)', [Name, AAddress.BookPage])
    else
      Result := 'Fiqh ' + AAddress.BookPage;
  end else begin
    if AIncludeName then
      Result := Format('%s (Hadith %s)', [Name, AAddress.BookPage])
    else
      Result := 'Hadith ' + AAddress.BookPage;
  end;
end;

function THadithFiqhData.GetLibName : String;
begin
  Result := 'Hadith';
end;

function THadithFiqhData.CreateViewer : TAmlViewer;
begin
  Result := THadithViewer.Create(Application);
end;

{------------------------------------------------------------------------------}

constructor THadithViewer.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  SetupHtmlViewerDefaults(HtmlViewer);
  SetupHtmlViewerDefaults(InfoViewer);
  SetNavgPanelDisplay(True);
end;

destructor THadithViewer.Destroy;
begin
  if FAmlData <> Nil then
    FAmlData.Free;
  if FAmlIndexes <> Nil then
    FAmlIndexes.Free;
  inherited Destroy;
end;

procedure THadithViewer.Navigate(const AType : TNavigate; const ADirection : TNavigateDirection; const AAddress : TCaptionedAddress);
begin
  if (FAmlData = Nil) or (AType <> nvPage) then
    Exit;

  case ADirection of
    nvdPrevious : SetActivePage(FAmlData.GetAttribute('prev'));
    nvdNext : SetActivePage(FAmlData.GetAttribute('next'));
    nvdCaptionedAddr : ;
  end;
end;

function THadithViewer.GetActiveData : TAmlData;
begin
  Result := FBook;
end;

procedure THadithViewer.ClearContents;
var
  Blank : String;
begin
  FActiveVol := '';
  FActiveElem := '';
  FNarrator := '';

  HeadingPanel.Caption := '';
  Blank := '     ';
  HtmlViewer.LoadFromBuffer(PChar(Blank), Length(Blank));
  InfoViewer.LoadFromBuffer(PChar(Blank), Length(Blank));
end;

procedure THadithViewer.SetActiveBook(const ABook : THadithFiqhData);
begin
  if ABook = Nil then begin
    ClearContents;
    Exit;
  end;

  if ABook <> FBook then begin
    FBook := ABook;
    FActiveVol := '';
    FActiveElem := '';
    ImgFiqh.Visible := FBook.IsFiqh;
    ImgHadith.Visible := not FBook.IsFiqh;
    FPrintHeader.BookName := FBook.Name;
  end;
end;

procedure THadithViewer.SetActivePage(const AKey : String);
var
  NarrElem : TXmlElement;
begin
  if (FBook = Nil) then begin
    ClearContents;
    Exit;
  end;

  if FAmlData <> Nil then
    FAmlData.Free;
  if FAmlIndexes <> Nil then
    FAmlIndexes.Free;
  FAmlData := Nil;
  FAmlIndexes := Nil;

  try
    if AKey <> '' then
      FAmlData := FBook.ReadXML(TrimS(AKey))
    else
      FAmlData := FBook.ReadXML(FBook.Home.GetAttribute('stream'));
  except
    FAmlData := Nil;
  end;

  if FAmlData <> Nil then begin
    FAmlIndexes := FAmlData.GetChildElementsByTagName('index');
    FActiveVol := FAmlData.GetAttribute('vol');
    FActiveElem := FAmlData.GetAttribute('num');
    NarrElem := FAmlData.FindElement('narration');
    if NarrElem <> Nil then
      FNarrator := NarrElem.GetAttribute('by')
    else
      FNarrator := '';
    UpdateContents;
  end else begin
    ClearContents;
    ShowMessage(Format('Page %s not found', [AKey]));
  end;
end;

procedure THadithViewer.UpdateContents;
var
  Html : String;
begin
  if FAmlData = Nil then
    Exit;
  HeadingPanel.Caption := FBook.Name;

  Html := FBook.AsHtml(FAmlData, FSearchHits);
  FSearchHits := '';                               // clear them so later updates don't show them
  if FNarrator <> '' then
    Html := Format('<b>Narrated <font color="green">%s</font></b><p>%s', [FNarrator, Html]);
  HtmlViewer.LoadFromBuffer(PChar(Html), Length(Html));
  FBook.GetFont('default').AssignInHtmlViewer(HtmlViewer);
  UpdateNavigation;
  HtmlViewer.NoSelect := not FBook.FlagIsSet(adfCanCopyText);
end;

procedure THadithViewer.UpdateNavigation;
const
  VolElemFmt = '<nobr>Volume %s</nobr><br><nobr>Number %s</nobr>';
  ElemFmt = 'Number %s';
  NarratorFmt = '<font color="green">Narrator</font><br><b><font color="navy"><a href="index:narrators/%0:s">%0:s</a></font></b>';
  IndexSubsFmt = '<b><a href="index:subjects/%0:s">%0:s</b> : <a href="index:subjects/%0:s/%1:s">%1:s</a>';
  IndexNoSubsFmt = '<a href="index:subjects/%0:s">%0:s</a>';
  IndexesFmt = '<font color="green">Classification</font><br>%s';
  InfoFmt = '<font color="green">%s</font><br><b><font color="navy">%s</font></b><p>%s<p>%s';
var
  Typ, Page, Narr, Html, S, T, Indexes : String;
  IndexElem : TXmlElement;
  I : Integer;
begin
  if FAmlData = Nil then
    Exit;

  if FAmlData.NodeName = 'hadith' then
    Typ := 'Hadith'
  else
    Typ := 'Fiqh';

  if FActiveVol <> '' then begin
    Page := Format(VolElemFmt, [FActiveVol, FActiveElem]);
    VolElemEdit.Element := FActiveElem;
    VolElemEdit.Volume := FActiveVol;
    VolElemEdit.VolumeDelim := HadithNumStrDelim;
    FPrintHeader.BookPage := Format('%s %s.%s', [Typ, FActiveElem, FActiveVol]);
  end else begin
    Page := Format(ElemFmt, [FActiveElem]);
    VolElemEdit.VolumeDelim := #0;
    VolElemEdit.Element := FActiveElem;
    FPrintHeader.BookPage := Format('%s %s', [Typ, FActiveElem]);
  end;

  if FNarrator <> '' then begin
    Narr := Format(NarratorFmt, [FNarrator]);
    FPrintHeader.BookSection := Format('Narrator: %s', [FNarrator]);
  end else begin
    Narr := '';
    FPrintHeader.BookSection := '';
  end;

  Indexes := '';
  if FAmlIndexes.Length > 0 then begin
    if FAmlIndexes.Length = 1 then begin
      IndexElem := FAmlIndexes.Item(0) as TXmlElement;
      T := IndexElem.GetAttribute('topic');
      S := IndexElem.GetAttribute('subtopic');
      if S <> '' then
        Indexes := Format(IndexSubsFmt, [T, S])
      else
        Indexes := Format(IndexNoSubsFmt, [T]);
    end else begin
      for I := 0 to FAmlIndexes.Length-1 do begin
        IndexElem := FAmlIndexes.Item(I) as TXmlElement;
        T := IndexElem.GetAttribute('topic');
        S := IndexElem.GetAttribute('subtopic');
        if S <> '' then
          Indexes := Indexes + '<li>' + Format(IndexSubsFmt, [T, S])
        else
          Indexes := Indexes + '<li>' + Format(IndexNoSubsFmt, [T]);
      end;
      Indexes := '<ul>'+Indexes+'</ul>';
    end;
  end;
  if Indexes <> '' then
    Indexes := Format(IndexesFmt, [Indexes]);

  Html := Format(InfoFmt, [Typ, Page, Narr, Indexes]);
  InfoViewer.LoadFromBuffer(PChar(Html), Length(Html));
  InfoPanel.Height := (InfoPanel.BorderWidth*2) + InfoViewer.VertPixels+5;
end;

procedure THadithViewer.SetNavgPanelDisplay(const AShow : Boolean);
begin
  OpenNavgLabel.Visible := not AShow;
  PrevLabel1.Visible := not AShow;
  NextLabel1.Visible := not AShow;
  NavgPanel.Visible := AShow;
end;

procedure THadithViewer.SetAddress(const AAddress : TAddress);
var
  Book, OtherData : TAmlData;
  OtherHadith : TDataList;
  I : Integer;
begin
  Book := FDataMgr.DataIds[AAddress.BookId];
  if Book is THadithFiqhData then begin
    if FQuickLinks.Id <> 'HadithDataList' then begin
      OtherHadith := TDataList.Create;
      FDataMgr.FillAll(dslDataType, 'hadith', OtherHadith);
      FDataMgr.FillAll(dslDataType, 'fiqh', OtherHadith);
      FQuickLinks.Clear;
      if (OtherHadith.Count > 1) then begin
        FQuickLinks.Id := 'HadithDataList';
        for I := 0 to OtherHadith.Count-1 do begin
          OtherData := OtherHadith[I];
          FQuickLinks.AddChild('', OtherData.Id+':', OtherData.ShortName, OtherData.ShortName);
        end;
      end;
    end;
    FAddrMgr.SetQuickLinks('Hadith', GotoPanel.Color, FQuickLinks, Book.ShortName, tpNone);
    FSearchHits := AAddress.SearchHits;
    SetActiveBook(Book as THadithFiqhData);
    SetActivePage(AAddress.BookPage);
    //UpdateContents;
  end;
end;

function THadithViewer.GetEnableCopy : Boolean;
begin
  Result := FBook.FlagIsSet(adfCanCopyText);
end;

function THadithViewer.GetEnablePrint : Boolean;
begin
  Result := FBook.FlagIsSet(adfCanPrintText);
end;

procedure THadithViewer.Print(const ADialog : TPrintDialog);
begin
  if not ADialog.Execute then
    Exit;

  if ADialog.PrintRange = prAllPages then
    HtmlViewer.Print(1, 9999)
  else
    HtmlViewer.Print(ADialog.FromPage, ADialog.ToPage);
end;

procedure THadithViewer.CopyToClipboard;
begin
  if HtmlViewer.SelLength > 0 then
    HtmlViewer.CopyToClipboard
  else begin
    HtmlViewer.SelectAll;
    HtmlViewer.CopyToClipboard;
    HtmlViewer.SelLength := 0;
  end;
end;

procedure THadithViewer.VolElemEditEnterKey(Sender: TObject);
begin
  SetActivePage(VolElemEdit.Text);
end;

procedure THadithViewer.PrevLabel2Click(Sender: TObject);
begin
  Navigate(nvPage, nvdPrevious);
end;

procedure THadithViewer.NextLabel2Click(Sender: TObject);
begin
  Navigate(nvPage, nvdNext);
end;

procedure THadithViewer.NavgCloseLabelClick(Sender: TObject);
begin
  SetNavgPanelDisplay(False);
end;

procedure THadithViewer.OpenNavgLabelClick(Sender: TObject);
begin
  SetNavgPanelDisplay(True);
end;

procedure THadithViewer.CustomizeLabelClick(Sender: TObject);
begin
  if FBook.ChangeFont('default') then UpdateContents;
end;

end.
