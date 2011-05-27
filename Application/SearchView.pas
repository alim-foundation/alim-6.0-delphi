unit SearchView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, ToolEdit, AmlView, IslUtils, ImgList, ComCtrls, CheckLst,
  ExtCtrls, sEdits, sTrans, sCtrls, islctrls, Menus, RxMenus, RXCtrls, StBase,
  SearchExpr, IndexStruct, CompDoc, moneyctrls;

type
  PSingleIndexInfo = ^TSingleIndexInfo;
  TSingleIndexInfo =
    record
      Data : TAmlData;
      Search : Boolean;
      MenuItem : TMenuItem;
    end;
  TSingleIndexInfoArray = array of TSingleIndexInfo;

  EExpression = class(EStException);

  TSearchViewer = class(TForm, ISchExprValueManager)
    IndexImages: TImageList;
    DataEntryPanel: TWallpaperPanel;
    FindTextLabel: TLabel;
    Results: TTreeView;
    FindText: TsLinkEdit;
    ResultsLabel: TLabel;
    RxSpeedButton1: TRxSpeedButton;
    BooksMenu: TRxPopupMenu;
    RxSpeedButton2: TRxSpeedButton;
    OptionsMenu: TRxPopupMenu;
    OptionCountOnly: TMenuItem;
    OptionCountAndLoc: TMenuItem;
    OptionMenuBreak: TMenuItem;
    OptionAndTerms: TMenuItem;
    OptionOrTerms: TMenuItem;
    StartButton: TRxSpeedButton;
    LinkPanel: TPanel;
    ConcordanceLabel: TMoneyLabel;
    Bevel1: TBevel;
    SearchProgress: TProgressBar;
    PrevSchHitBtn: TMoneyLabel;
    NextSchHitBtn: TMoneyLabel;
    procedure FindTextButtonClick(Sender: TObject);
    procedure ResultsDblClick(Sender: TObject);
    procedure FindTextKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure OptionCountAndLocClick(Sender: TObject);
    procedure OptionCountOnlyClick(Sender: TObject);
    procedure OptionAndTermsClick(Sender: TObject);
    procedure OptionOrTermsClick(Sender: TObject);
    procedure ResultsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ConcordanceLabelClick(Sender: TObject);
    procedure NextSchHitBtnClick(Sender: TObject);
    procedure PrevSchHitBtnClick(Sender: TObject);
  private
    FAddrMgr : IAddressManager;
    FDataMgr : TDataManager;
    FStatus : IStatusDisplay;
    FIndexNames : String;
    FIndexNamesList : TStringList;
    FInverseAddr : Boolean;
    FIndexes : TSingleIndexInfoArray;
    FResults : TList;
    FBooksInMenu : TStringList;
    FGroupsInMenu : TStringList;
    FHighlightWords : String;

    // valid only inside of a Search method call
    FCancelSearch : Boolean;
    FExpression : TSearchExpression;
    FActiveIndex : TStaticIndex;
    FActiveData : TAmlData;
    FActiveIndexName : String;

    procedure CreateExpression;
    procedure BooksMenuItemClick(Sender : TObject);
    procedure CreateBooksMenu(const AIndexNames : String); virtual;
    procedure ClearResults; virtual;
    procedure SetIndexNames(const AIndexNames : String);
  public
    destructor Destroy; override;
    procedure Search(const ACriteria : String);
    procedure SearchSingle(const ADataId, ACriteria : String);

    function CreateValue(var T : TSchExprToken) : TSchExprValue;
    function GetSimpleResult(var T : TSchExprToken) : TSchExprValue;
    function GetUnaryOpResult(var T : TSchExprToken; const X : TSchExprValue) : TSchExprValue;
    function GetBinaryOpResult(var T : TSchExprToken; const X, Y : TSchExprValue) : TSchExprValue;
    procedure DisposeValue(const AValue : TSchExprValue);

    procedure Navigate(const ADirection : TNavigateDirection); virtual;

    property CancelSearch : Boolean write FCancelSearch;
    property IndexNames : String read FIndexNames write SetIndexNames;
    property InverseAddr : Boolean read FInverseAddr write FInverseAddr;
    property AddressMgr : IAddressManager read FAddrMgr write FAddrMgr;
    property DataMgr : TDataManager read FDataMgr write FDataMgr;
    property StatusMgr : IStatusDisplay read FStatus write FStatus;
  end;

implementation

{$R *.DFM}

uses StStrS;

const
  CombineAllBooksTag = -99999;
  SingleBookSubmenuTag = -99998;
  GroupSubmenuTag = -99997;
  GroupsDelta = -10000;
  NodeIsWordTag = Pointer($FFFFFFFF);
  NodeIsSubjectTag = Pointer($FFFFFFFE);

type
  TSrchLibData = class(TObject)
    FLibNode : TTreeNode;
    FLibName : ShortString;
    FResultsCount : Cardinal;
  end;

  PResultRec = ^TResultRec;
  TResultRec = record
    AmlData : TAmlData;
    Address : PString;
  end;

destructor TSearchViewer.Destroy;
begin
  ClearResults;
  if FIndexNamesList <> Nil then
    FIndexNamesList.Free;
  if FResults <> Nil then
    FResults.Free;
  if FBooksInMenu <> Nil then
    FBooksInMenu.Free;
  if FGroupsInMenu <> Nil then
    FGroupsInMenu.Free;
  inherited Destroy;
end;

procedure TSearchViewer.Navigate(const ADirection : TNavigateDirection);
var
  ActiveNode : TTreeNode;
  Done : Boolean;
begin
  ActiveNode := Results.Selected;
  if ActiveNode = Nil then
    ActiveNode := Results.Items.GetFirstNode;

  if ActiveNode <> Nil then begin
    if ADirection = nvdPrevious then begin
      repeat
        ActiveNode := ActiveNode.GetPrev;
        Done := (ActiveNode = Nil) or ((ActiveNode.Data <> Nil) and (ActiveNode.Data <> NodeIsWordTag) and (ActiveNode.Data <> NodeIsSubjectTag));
      until Done;
    end else begin
      repeat
        ActiveNode := ActiveNode.GetNext;
        Done := (ActiveNode = Nil) or ((ActiveNode.Data <> Nil) and (ActiveNode.Data <> NodeIsWordTag) and (ActiveNode.Data <> NodeIsSubjectTag));
      until Done;
    end;
    if ActiveNode <> Nil then begin
      Results.Selected := ActiveNode;
      ResultsDblClick(Results);
    end;
  end;
end;

procedure TSearchViewer.BooksMenuItemClick(Sender : TObject);
var
  MenuItem : TMenuItem;
  I, GroupIdx, IndexIdx : Integer;
  GroupName : String;
begin
  if Length(FIndexes) <= 0 then
    Exit;

  MenuItem := Sender as TMenuItem;
  if MenuItem.Tag = CombineAllBooksTag then begin
    for I := Low(FIndexes) to High(FIndexes) do begin
      with FIndexes[I] do
        Search := True;
    end;
  end else if MenuItem.Tag >= 0 then begin
    with FIndexes[MenuItem.Tag] do
      Search := not Search;
  end else if MenuItem.Tag <= GroupsDelta then begin
    GroupIdx := Abs(Abs(GroupsDelta) + MenuItem.Tag);
    GroupName := FGroupsInMenu[GroupIdx];
    for I := Low(FIndexes) to High(FIndexes) do begin
      with FIndexes[I] do
        Search := CompareText(Data.LibName, GroupName) = 0;
    end;
  end else if MenuItem.Tag < 0 then begin
    IndexIdx := Abs(MenuItem.Tag)-1;
    for I := Low(FIndexes) to High(FIndexes) do begin
      with FIndexes[I] do
        Search := IndexIdx = I;
    end;
  end;
  for I := Low(FIndexes) to High(FIndexes) do begin
    with FIndexes[I] do
      MenuItem.Checked := Search;
  end;
end;

procedure TSearchViewer.CreateBooksMenu(const AIndexNames : String);

  procedure BuildLists(const AIndexName : String);
  var
    Data : TAmlData;
    B, ThisRecIdx : Integer;
    Books : TDataList;
  begin
    Books := FDataMgr.SpecialIndexes[AIndexName];
    if (Books <> Nil) and (Books.Count > 1) then begin
      for B := 0 to Books.Count-1 do begin
        Data := Books.Data[B];

        if FBooksInMenu.IndexOf(Data.Name) = -1 then begin
          ThisRecIdx := Length(FIndexes);
          SetLength(FIndexes, ThisRecIdx + 1);
          FBooksInMenu.AddObject(Data.Name, TObject(ThisRecIdx));

          FIndexes[ThisRecIdx].Data := Data;
          FIndexes[ThisRecIdx].Search := True;

          if FGroupsInMenu.IndexOf(Data.LibName) = -1 then
            FGroupsInMenu.Add(Data.LibName);
        end;
      end;
    end;
  end;

  procedure BuildMenus(const ASinglesMenu, AGroupsMenu : TMenuItem; AOnClick : TNotifyEvent);
  var
    RecIdx, J : Integer;
    MenuItem : TMenuItem;
  begin
    if FBooksInMenu.Count > 0 then begin
      for J := 0 to FBooksInMenu.Count-1 do begin
        RecIdx := Integer(FBooksInMenu.Objects[J]);
        MenuItem := TMenuItem.Create(Self);
        FIndexes[RecIdx].MenuItem := MenuItem;
        MenuItem.Tag := RecIdx;
        MenuItem.Caption := FIndexes[RecIdx].Data.Name;
        MenuItem.Checked := FIndexes[RecIdx].Search;
        MenuItem.OnClick := AOnClick;
        BooksMenu.Items.Add(MenuItem);

        MenuItem := TMenuItem.Create(Self);
        MenuItem.Tag := -RecIdx-1;
        MenuItem.Caption := FIndexes[RecIdx].Data.Name;
        MenuItem.OnClick := AOnClick;
        ASinglesMenu.Add(MenuItem);
      end;
    end;

    if FGroupsInMenu.Count > 0 then begin
      for J := 0 to FGroupsInMenu.Count-1 do begin
        MenuItem := TMenuItem.Create(Self);
        MenuItem.Tag := GroupsDelta - J;
        MenuItem.Caption := FGroupsInMenu[J];
        MenuItem.OnClick := AOnClick;
        AGroupsMenu.Add(MenuItem);
      end;
    end;
  end;

var
  MenuItem, SinglesSubMenu, GroupsSubMenu : TMenuItem;
  SingleName : String;
  NC, N : Integer;
begin
  NC := WordCountS(AIndexNames, ',');
  ClearMenu(BooksMenu);
  if NC <= 0 then
    Exit;

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := 'All';
  MenuItem.Tag := CombineAllBooksTag;
  MenuItem.OnClick := BooksMenuItemClick;
  BooksMenu.Items.Add(MenuItem);

  SinglesSubMenu := TMenuItem.Create(Self);
  SinglesSubMenu.Caption := 'Only text';
  SinglesSubMenu.Tag := SingleBookSubmenuTag;
  BooksMenu.Items.Add(SinglesSubMenu);

  GroupsSubMenu := TMenuItem.Create(Self);
  GroupsSubMenu.Caption := 'Only group';
  GroupsSubMenu.Tag := GroupSubmenuTag;
  BooksMenu.Items.Add(GroupsSubMenu);
  BooksMenu.Items.Add(NewLine);

  if FGroupsInMenu = Nil then begin
    FGroupsInMenu := TStringList.Create;
    FGroupsInMenu.Sorted := True;
    FGroupsInMenu.Duplicates := dupIgnore;
  end;
  if FBooksInMenu = Nil then begin
    FBooksInMenu := TStringList.Create;
    FBooksInMenu.Sorted := True;
    FBooksInMenu.Duplicates := dupIgnore;
  end;

  for N := 1 to NC do begin
    SingleName := ExtractWordS(N, AIndexNames, ',');
    FIndexNamesList.Add(SingleName);
    BuildLists(SingleName);
  end;
  BuildMenus(SinglesSubMenu, GroupsSubMenu, BooksMenuItemClick);
end;

procedure TSearchViewer.ClearResults;
var
  I : Integer;
begin
  FStatus.StatusBegin('Clearing results...', True);
  Results.Items.BeginUpdate;
  Results.Items.Clear;
  Results.Items.EndUpdate;
  ResultsLabel.Caption := '';

  if (FResults <> Nil) and (FResults.Count > 0) then begin
    for I := 0 to FResults.Count-1 do begin
      DisposeStr(PResultRec(FResults[I]).Address);
      Dispose(FResults[I]);
    end;
    FResults.Clear;
  end;
  FStatus.StatusEnd;
end;

function TSearchViewer.CreateValue(var T : TSchExprToken) : TSchExprValue;
begin
  FStatus.StatusBegin(Format('Searching %s for %s (press ESC to cancel)', [FActiveData.Name, T.Text]), False);
  Result := FActiveIndex.GetEntryLocs(T, FExpression.HighlightList);
  Application.ProcessMessages;
end;

function TSearchViewer.GetSimpleResult(var T : TSchExprToken) : TSchExprValue;
begin
  FStatus.StatusBegin(Format('Searching %s for %s (press ESC to cancel)', [FActiveData.Name, T.Text]), False);
  Result := FActiveIndex.GetEntryLocs(T, FExpression.HighlightList);
  Application.ProcessMessages;
end;

function TSearchViewer.GetUnaryOpResult(var T : TSchExprToken; const X : TSchExprValue) : TSchExprValue;
begin
  // like and not are supported yet
  Result := Nil;
end;

function TSearchViewer.GetBinaryOpResult(var T : TSchExprToken; const X, Y : TSchExprValue) : TSchExprValue;
var
  ResultEntries, XEntries, YEntries : TEntryLocs;
  I, XMax, YMax : Integer;
begin
  ResultEntries := TEntryLocs.Create;
  Result := ResultEntries;
  XEntries := TEntryLocs(X);
  YEntries := TEntryLocs(Y);
  Application.ProcessMessages;

  case T.Code of
    tcAnd :
      begin
        if (X = Nil) or (Y=Nil) then
         Exit;

        XMax := XEntries.Count-1;
        YMax := YEntries.Count-1;

        if XMax > 0 then begin
          for I := 0 to XMax do begin
            if YEntries.IndexOf(XEntries[I]) >= 0 then
              ResultEntries.Add(XEntries[I]);
          end;
        end;
        if YMax > 0 then begin
          for I := 0 to YMax do begin
            if XEntries.IndexOf(YEntries[I]) >= 0 then
              ResultEntries.Add(YEntries[I]);
          end;
        end;
      end;
    tcOr :
      begin
        if X <> Nil then XMax := XEntries.Count-1 else XMax := 0;
        if Y <> NIl then YMax := YEntries.Count-1 else YMax := 0;
        if XMax > 0 then begin
          for I := 0 to XMax do
            ResultEntries.Add(XEntries[I]);
        end;
        if YMax > 0 then begin
          for I := 0 to YMax do
            ResultEntries.Add(YEntries[I]);
        end;
      end;
  end;
end;

procedure TSearchViewer.DisposeValue(const AValue : TSchExprValue);
begin
  if AValue <> Nil then
    TEntryLocs(AValue).Free;
end;

procedure TSearchViewer.CreateExpression;
begin
  FExpression := TSearchExpression.Create;
  FExpression.ValueManager := Self;
  FExpression.UnsupportedOps.Add('near');
  FExpression.UnsupportedOps.Add('not');
  if OptionAndTerms.Checked then
    FExpression.ImplicitOp := 'and'
  else
    FExpression.ImplicitOp := 'or';
end;

procedure TSearchViewer.Search(const ACriteria : String);

  procedure PopulateAddresses(const Data : TAmlData; const LibMap : TObjDict; const Locs : TEntryLocs);
  var
    L, LibIdx : Integer;
    LibData : TSrchLibData;
    ResultRec : PResultRec;
    Address : TAddress;
    LocNode, BookNode : TTreeNode;
  begin
    if LibMap.Find(Data.LibName, LibIdx) then
      LibData := LibMap.Objects[LibIdx] as TSrchLibData
    else begin
      LibData := TSrchLibData.Create;
      LibData.FLibNode := Results.Items.Add(Nil, Data.LibName);
      LibData.FLibNode.ImageIndex := 5;
      LibData.FLibNode.SelectedIndex := 5;
      LibData.FLibName := Data.LibName;
      LibMap.AddObject(Data.LibName, LibData);
    end;
    Inc(LibData.FResultsCount, Locs.Count);

    LibData.FLibNode.Text := Format('%s [%d]', [LibData.FLibName, LibData.FResultsCount]);

    BookNode := Results.Items.AddChild(LibData.FLibNode, Format('%s [%d]', [Data.Name, Locs.Count]));
    BookNode.ImageIndex := 0;
    BookNode.StateIndex := 0;
    BookNode.SelectedIndex := 0;
    if not OptionCountOnly.Checked then begin
      for L := 0 to Locs.Count-1 do begin
        New(ResultRec);
        FResults.Add(ResultRec);
        Address := TAddress.Create;
        ResultRec.AmlData := Data;
        ResultRec.Address := NewStr(Locs[L]);
        Address.Address := Locs[L];
        LocNode := Results.Items.AddChildObject(BookNode, Data.GetAddressCaption(Address, False), ResultRec);
        LocNode.ImageIndex := 4;
        LocNode.SelectedIndex := 4;
        Address.Free;
      end;
    end;
  end;

var
  LibMap : TObjDict;
  I, NameIdx : Integer;
  Locs : TEntryLocs;
  ResultsCount : Integer;
  SynopsisNode, WordsSearchedNode, BooksSearchedNode, SubjLinkNode, NewNode : TTreeNode;
  SearchedBooks : TStringList;
begin
  if FResults <> Nil then
    ClearResults
  else
    FResults := TList.Create;

  if (Length(FIndexes) <= 0) or (ACriteria = '') then
    Exit;

  if ACriteria <> FindText.Text then
    FindText.Text := ACriteria;

  CreateExpression;
  FExpression.Expression := ACriteria;
  FCancelSearch := False;

  LibMap := TObjDict.Create;
  LibMap.OwnTheData := True;
  ResultsCount := 0;

  DataEntryPanel.Height := SearchProgress.Top + SearchProgress.Height + 4;
  SearchProgress.Visible := True;
  SearchProgress.Min := 0;
  SearchProgress.Max := High(FIndexes);

  SearchedBooks := TStringList.Create;
  SearchedBooks.Sorted := True;
  for I := Low(FIndexes) to High(FIndexes) do begin
    if not FIndexes[I].Search then
      continue;
    if FCancelSearch then
      break;
    SearchProgress.Position := I;

    FActiveData := FIndexes[I].Data;
    SearchedBooks.Add(FActiveData.Name);
    ResultsLabel.Caption := Format('Searching (%d found)...', [ResultsCount]);
    ResultsLabel.Update;
    for NameIdx := 0 to FIndexNamesList.Count-1 do begin
      FActiveIndexName := FIndexNamesList[NameIdx];
      try
        FActiveIndex := FActiveData.OpenConcordance(FActiveIndexName);
        FExpression.Execute;
        Locs := TEntryLocs(FExpression.Result);
        if (Locs <> Nil) and (Locs.Count > 0) then begin
          Inc(ResultsCount, Locs.Count);
          PopulateAddresses(FActiveData, LibMap, Locs);
        end;
        FExpression.DisposeResult;
        FActiveData.CloseConcordance(FActiveIndexName);
      except
        FActiveIndex := Nil
      end;
    end;
  end;
  FStatus.StatusEnd;

  SearchProgress.Visible := False;
  DataEntryPanel.Height := ResultsLabel.Top + ResultsLabel.Height + 4;

  ResultsLabel.Caption := Format('Locations: %d', [ResultsCount]);
  FStatus.StatusBegin('Filling list...', False);
  Results.Items.EndUpdate;
  FStatus.StatusEnd;
  LibMap.Free;

  FHighlightWords := FExpression.HighlightList.CommaText;
  SynopsisNode := Results.Items.Add(Nil, 'Results Synopsis');
  SynopsisNode.ImageIndex := 7;
  SynopsisNode.SelectedIndex := 7;
  SubjLinkNode := Results.Items.Add(Nil, 'Look in Subject Index');
  SubjLinkNode.ImageIndex := 7;
  SubjLinkNode.SelectedIndex := 7;
  WordsSearchedNode := Results.Items.AddChild(SynopsisNode, 'Words Searched');
  WordsSearchedNode.ImageIndex := 7;
  WordsSearchedNode.SelectedIndex := 7;
  BooksSearchedNode := Results.Items.AddChild(SynopsisNode, 'Books Searched');
  BooksSearchedNode.ImageIndex := 5;
  BooksSearchedNode.SelectedIndex := 5;
  for I := 0 to FExpression.HighlightList.Count-1 do begin
    NewNode := Results.Items.AddChild(WordsSearchedNode, FExpression.HighlightList[I]);
    NewNode.ImageIndex := 6;
    NewNode.SelectedIndex := 6;
    NewNode.Data := NodeIsWordTag;
    NewNode := Results.Items.AddChild(SubjLinkNode, FExpression.HighlightList[I]);
    NewNode.ImageIndex := 6;
    NewNode.SelectedIndex := 6;
    NewNode.Data := NodeIsSubjectTag;
  end;
  if SearchedBooks.Count > 0 then
    for I := 0 to SearchedBooks.Count-1 do begin
      NewNode := Results.Items.AddChild(BooksSearchedNode, SearchedBooks[I]);
      NewNode.ImageIndex := 0;
      NewNode.SelectedIndex := 0;
    end;

  Results.SetFocus;
  SearchedBooks.Free;
  FExpression.Free;
end;

procedure TSearchViewer.SearchSingle(const ADataId, ACriteria : String);

  procedure PopulateAddresses(const AData : TAmlData; const Locs : TEntryLocs);
  var
    ResultRec : PResultRec;
    Address : TAddress;
    LocNode, BookNode : TTreeNode;
    L : Integer;
  begin
    BookNode := Results.Items.Add(Nil, Format('%s [%d]', [AData.Name, Locs.Count]));
    BookNode.ImageIndex := 0;
    BookNode.StateIndex := 0;
    BookNode.SelectedIndex := 0;
    if not OptionCountOnly.Checked then begin
      for L := 0 to Locs.Count-1 do begin
        New(ResultRec);
        FResults.Add(ResultRec);
        Address := TAddress.Create;
        ResultRec.AmlData := AData;
        ResultRec.Address := NewStr(Locs[L]);
        Address.Address := Locs[L];
        LocNode := Results.Items.AddChildObject(BookNode, AData.GetAddressCaption(Address, False), ResultRec);
        LocNode.ImageIndex := 4;
        LocNode.SelectedIndex := 4;
        Address.Free;
      end;
    end;
    BookNode.Expand(True);
  end;

var
  Data : TAmlData;
  NameIdx : Integer;
  Locs : TEntryLocs;
  ResultsCount : Integer;
begin
  if FResults <> Nil then
    ClearResults
  else
    FResults := TList.Create;

  if (Length(FIndexes) <= 0) or (ACriteria = '') then
    Exit;

  Data := FDataMgr.DataIds[ADataId];
  if Data = Nil then begin
    ShowMessage(Format('Book "%s" not found', [ADataId]));
    Exit;
  end;

  if ACriteria <> FindText.Text then
    FindText.Text := ACriteria;

  CreateExpression;
  FExpression.Expression := ACriteria;
  FCancelSearch := False;

  ResultsCount := 0;
  FActiveData := Data;
  ResultsLabel.Caption := Format('Searching...', [ResultsCount]);
  ResultsLabel.Update;
  for NameIdx := 0 to FIndexNamesList.Count-1 do begin
    FActiveIndexName := FIndexNamesList[NameIdx];
    try
      FActiveIndex := FActiveData.OpenConcordance(FActiveIndexName);
      FExpression.Execute;
      Locs := TEntryLocs(FExpression.Result);
      if (Locs <> Nil) and (Locs.Count > 0) then begin
        Inc(ResultsCount, Locs.Count);
        PopulateAddresses(Data, Locs);
      end;
      FExpression.DisposeResult;
      FActiveData.CloseConcordance(FActiveIndexName);
    except
      FActiveIndex := Nil
    end;
  end;
  FStatus.StatusEnd;
  FHighlightWords := FExpression.HighlightList.CommaText;

  ResultsLabel.Caption := Format('Locations: %d', [ResultsCount]);
  FStatus.StatusBegin('Filling list...', True);
  Results.Items.EndUpdate;
  FStatus.StatusEnd;

  Results.SetFocus;
  FExpression.Free;
end;

procedure TSearchViewer.SetIndexNames(const AIndexNames : String);
begin
  if AIndexNames = FIndexNames then
    Exit;

  ClearResults;
  if FIndexNamesList = Nil then begin
    FIndexNamesList := TStringList.Create;
    FIndexNamesList.Sorted := True;
  end else
    FIndexNamesList.Clear;

  SetLength(FIndexes, 0);
  FIndexNames := AIndexNames;

  CreateBooksMenu(AIndexNames);
end;

procedure TSearchViewer.FindTextButtonClick(Sender: TObject);
begin
  Search(FindText.Text);
end;

procedure TSearchViewer.ResultsDblClick(Sender: TObject);
var
  ResultRec : PResultRec;
  NodeCaption : String;
  SelData : Pointer;
begin
  Assert(FAddrMgr <> Nil);

  if Results.Selected <> Nil then begin
    SelData := Results.Selected.Data;
    NodeCaption := Results.Selected.Text;
    if SelData = NodeIsWordTag then
      Search(NodeCaption)
    else if SelData = NodeIsSubjectTag then
      FAddrMgr.SetAddress(Format('index:subjects/%s', [NodeCaption]))
    else if SelData <> Nil then begin
      ResultRec := PResultRec(SelData);
      FAddrMgr.SetAddress(Format('%s:%s?%s', [ResultRec.AmlData.Id, ResultRec.Address^, FHighlightWords]));
    end;
  end;
end;

procedure TSearchViewer.FindTextKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_Return then
    Search(FindText.Text);
end;

procedure TSearchViewer.OptionCountAndLocClick(Sender: TObject);
begin
  OptionCountANdLoc.Checked := True;
end;

procedure TSearchViewer.OptionCountOnlyClick(Sender: TObject);
begin
  OptionCountOnly.Checked := True;
end;

procedure TSearchViewer.OptionAndTermsClick(Sender: TObject);
begin
  OptionAndTerms.Checked := True;
end;

procedure TSearchViewer.OptionOrTermsClick(Sender: TObject);
begin
  OptionOrTerms.Checked := True;
end;

procedure TSearchViewer.ResultsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Results.Selected <> Nil) then begin
    if Results.Selected.Count > 0 then
      Results.Selected.Expanded := not Results.Selected.Expanded
    else
      ResultsDblClick(Sender);
  end;
end;

procedure TSearchViewer.ConcordanceLabelClick(Sender: TObject);
begin
  FAddrMgr.SetAddress('concordance:');
end;

procedure TSearchViewer.NextSchHitBtnClick(Sender: TObject);
begin
  Navigate(nvdNext);
end;

procedure TSearchViewer.PrevSchHitBtnClick(Sender: TObject);
begin
  Navigate(nvdPrevious);
end;

end.

