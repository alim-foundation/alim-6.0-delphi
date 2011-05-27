unit ConcordView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, AmlView, CompDoc, IndexStruct, sCombos,
  sEdits, moneyctrls;

type
  TConcordanceViewer = class(TForm)
    Panel1: TPanel;
    WordsList: TListView;
    BooksCombo: TsComboBox;
    LinkPanel: TPanel;
    SearchLabel: TMoneyLabel;
    procedure WordsListData(Sender: TObject; Item: TListItem);
    procedure BooksComboChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure WordsListDblClick(Sender: TObject);
    procedure WordsListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure WordsListDataFind(Sender: TObject; Find: TItemFind;
      const FindString: String; const FindPosition: TPoint;
      FindData: Pointer; StartIndex: Integer; Direction: TSearchDirection;
      Wrap: Boolean; var Index: Integer);
    procedure SearchLabelClick(Sender: TObject);
  protected
    FAddrMgr : IAddressManager;
    FDataMgr : TDataManager;
    FStatus : IStatusDisplay;
    FActiveData : TAmlData;
    FActiveIndex : TStaticIndex;
    FActiveIndexName : String;
    FRtl : Boolean;
    FWordsCol : Integer;
    FCountsCol : Integer;

    procedure SetDataMgr(const ADataMgr : TDataManager);
    procedure SelectBook(const AIndex : Integer);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    property AddressMgr : IAddressManager read FAddrMgr write FAddrMgr;
    property DataMgr : TDataManager read FDataMgr write SetDataMgr;
    property StatusMgr : IStatusDisplay read FStatus write FStatus;
  end;

implementation

{$R *.DFM}

uses QuranView, HtmlSubs, IslUtils;

constructor TConcordanceViewer.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FormResize(Self);
  FActiveIndexName := SpecialIdxFullText;
end;

destructor TConcordanceViewer.Destroy;
begin
  inherited Destroy;
end;

procedure TConcordanceViewer.SetDataMgr(const ADataMgr : TDataManager);
var
  DataList : TDataList;
  I : Integer;
  Data : TAmlData;
begin
  FDataMgr := ADataMgr;
  DataList := FDataMgr.SpecialIndexes[SpecialIdxFullText];
  BooksCombo.Items.Clear;
  BooksCombo.Sorted := True;
  if (DataList <> Nil) and (DataList.Count > 0) then begin
    for I := 0 to DataList.Count-1 do begin
      Data := DataList[I];
      if not ((Data is TQuranData) and ((Data as TQuranData).FlagIsSet(qdfIsArabic))) then
        BooksCombo.Items.AddObject(Data.Name, Data);
    end;
  end;
  BooksCombo.Sorted := False;
  BooksCombo.Items.Insert(0, 'Choose a book');
  BooksCombo.ItemIndex := 0;  
end;

procedure TConcordanceViewer.SelectBook(const AIndex : Integer);
begin
  if BooksCombo.ItemIndex <> AIndex then
    BooksCombo.ItemIndex := AIndex;

  if (FActiveIndex <> Nil) and (FActiveData <> Nil) then
    FActiveData.CloseConcordance(FActiveIndexName);

  FActiveData := BooksCombo.Items.Objects[AIndex] as TAmlData;
  if FActiveData = Nil then begin
    WordsList.Items.Count := 0;
    WordsList.Invalidate;
    FActiveIndex := Nil;
    Exit;
  end;

  try
    FActiveIndex := FActiveData.OpenConcordance(FActiveIndexName);
    WordsList.Items.Count := FActiveIndex.Count;
    WordsList.Invalidate;
  except
    FActiveIndex := Nil;
    WordsList.Items.Count := 0;
  end;

  FRtl := (FActiveData is TQuranData) and ((FActiveData as TQuranData).FlagIsSet(qdfIsArabic));
  if FRtl then begin
    FWordsCol := 1;
    FCountsCol := 0;
    with WordsList.Columns[FWordsCol] do begin
      Alignment := taRightJustify;
      Caption := 'Word';
    end;
    with WordsList.Columns[FCountsCol] do begin
      Alignment := taRightJustify;
      Caption := 'Count';
    end;
  end else begin
    FWordsCol := 0;
    FCountsCol := 1;
    with WordsList.Columns[FWordsCol] do begin
      Alignment := taLeftJustify;
      Caption := 'Word';
    end;
    with WordsList.Columns[FCountsCol] do begin
      Alignment := taRightJustify;
      Caption := 'Count';
    end;
  end;
  WordsList.Columns[FCountsCol].Width := 45;
  FormResize(Self);
end;

procedure TConcordanceViewer.WordsListData(Sender: TObject;
  Item: TListItem);
var
  DE : TDirectoryEntry;
begin
  DE := FActiveIndex.GetEntry(Item.Index);
  if FRtl then begin
    Item.Caption := IntToStr(DE.LocCount);
    Item.SubItems.Add(Reverse(FActiveIndex.GetEntryKey(DE)));
  end else begin
    Item.Caption := FActiveIndex.GetEntryKey(DE);
    Item.SubItems.Add(IntToStr(DE.LocCount));
  end;
end;

procedure TConcordanceViewer.BooksComboChange(Sender: TObject);
begin
  if BooksCombo.ItemIndex >= 0 then
    SelectBook(BooksCombo.ItemIndex);
  WordsList.SetFocus;
end;

procedure TConcordanceViewer.FormResize(Sender: TObject);
begin
  WordsList.Columns[FWordsCol].Width := Width - WordsList.Columns[FCountsCol].Width - GetSystemMetrics(SM_CXVSCROLL) - 0;
end;

procedure TConcordanceViewer.WordsListDblClick(Sender: TObject);
var
  DE : TDirectoryEntry;
begin
  if WordsList.Selected <> Nil then begin
    DE := FActiveIndex.GetEntry(WordsList.Selected.Index);
    if not FRtl then
      FAddrMgr.SetAddress(Format('search:%s/%s', [FActiveData.Id, FActiveIndex.GetEntryKey(DE)]));
  end;
end;

procedure TConcordanceViewer.WordsListKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then WordsListDblClick(Sender);
end;

procedure TConcordanceViewer.WordsListDataFind(Sender: TObject;
  Find: TItemFind; const FindString: String; const FindPosition: TPoint;
  FindData: Pointer; StartIndex: Integer; Direction: TSearchDirection;
  Wrap: Boolean; var Index: Integer);
var
  EntryIdx : Cardinal;
  DE : TDirectoryEntry;
begin
  if FActiveIndex = Nil then
    Exit;

  FStatus.StatusBegin('Searching', True);
  FActiveIndex.FindEntry(FindString, EntryIdx, DE);
  if (EntryIdx >= 0) and (EntryIdx < Cardinal(WordsList.Items.Count)) then
    Index := EntryIdx;
  FStatus.StatusEnd;
end;

procedure TConcordanceViewer.SearchLabelClick(Sender: TObject);
begin
  FAddrMgr.SetAddress('search:');
end;

end.
