unit MultiIndexView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, AmlView, HadithView, IslUtils, IndexStruct,
  CompDoc, ImgList, CheckLst, DFSSplitter, RXCtrls, CSTCBase, CSTC32, Tabs,
  islctrls, Htmlview, Mask, ToolEdit, XmlObjModel, sEdits, Menus, RxMenus;

type
  TIndexData = class(TAmlData)
  protected
    function GetLibName : String; override;
    function CreateViewer : TAmlViewer; override;
  end;

  TSingleIndexInfo =
    record
      Data : TAmlData;
      Display : Boolean;
      MenuItem : TMenuItem;
    end;
  TSingleIndexInfoArray = array of TSingleIndexInfo;

  TMultiIndexViewer = class(TForm)
    IndexImages: TImageList;
    IndexTabs: TTabControl;
    Entries: TTreeView;
    IndexToolsPanel: TPanel;
    RxSpeedButton1: TRxSpeedButton;
    BooksMenu: TRxPopupMenu;
    FindEdit: TsEdit;
    procedure EntriesClick(Sender: TObject);
    procedure LocationsCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure IndexTabsChange(Sender: TObject);
    procedure EntriesDblClick(Sender: TObject);
    procedure FindEditChange(Sender: TObject);
    procedure FindEditEnter(Sender: TObject);
    procedure FindEditExit(Sender: TObject);
    procedure EntriesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  protected
    FDataMgr : TDataManager;
    FAddrMgr : IAddressManager;
    FStatus : IStatusDisplay;
    FActiveIndexName : String;
    FEntryIcon : Integer;
    FIndexes : TSingleIndexInfoArray;
    FMergedEntries : TStringList;
    FLocsList : TList;

    procedure ClearLocsList;
    procedure BooksMenuItemClick(Sender : TObject);
    procedure ExpandNode(const AExpandNode : TTreeNode);
    procedure CreateBooksMenu(const AIndexName : String);
    procedure MergeEntries(const AIndexName : String);
    procedure SetDataMgr(const ADataMgr : TDataManager);
    function GetEntry : String;
    procedure SetEntry(const AName : String);
    procedure SetIndexName(const ANewName : String);
    procedure SeeAlso(const AEntry : String);
  public
    destructor Destroy; override;

    property ActiveIndexName : String read FActiveIndexName write SetIndexName;
    property Entry : String read GetEntry write SetEntry;
    property DataMgr : TDataManager read FDataMgr write SetDataMgr;
    property AddressMgr : IAddressManager read FAddrMgr write FAddrMgr;
    property Status : IStatusDisplay read FStatus write FStatus;
  end;

implementation

{$R *.DFM}

const
  SeeAlsoCaption = 'See Also';
  CombineAllBooksTag = -99999;
  SingleBookSubmenuTag = -99998;

function TIndexData.GetLibName : String;
begin
  Result := '';
end;

function TIndexData.CreateViewer : TAmlViewer;
begin
  Result := Nil;
end;

{------------------------------------------------------------------------------}

type
  PLocRec = ^TLocRec;
  TLocRec = record
    Kind : TEntryLocKind;
    LinkData : PString;
  end;

destructor TMultiIndexViewer.Destroy;
begin
  if FMergedEntries <> Nil then
    FMergedEntries.Free;
  ClearLocsList;
  if FLocsList <> Nil then FLocsList.Free;
  inherited Destroy;
end;

procedure TMultiIndexViewer.ClearLocsList;
var
  I : Integer;
begin
  if (FLocsList <> Nil) and (FLocsList.Count > 0) then begin
    for I := 0 to FLocsList.Count-1 do begin
      DisposeStr(PLocRec(FLocsList[I]).LinkData);
      Dispose(FLocsList[I]);
    end;
    FLocsList.Clear;
  end;
end;

procedure TMultiIndexViewer.BooksMenuItemClick(Sender : TObject);
var
  MenuItem : TMenuItem;
  I, IndexIdx : Integer;
begin
  if Length(FIndexes) <= 0 then
    Exit;

  MenuItem := Sender as TMenuItem;
  if MenuItem.Tag = CombineAllBooksTag then begin
    for I := Low(FIndexes) to High(FIndexes) do begin
      with FIndexes[I] do
        Display := True;
    end;
  end else if MenuItem.Tag >= 0 then begin
    with FIndexes[MenuItem.Tag] do
      Display := not Display;
  end else if MenuItem.Tag < 0 then begin
    IndexIdx := Abs(MenuItem.Tag)-1;
    for I := Low(FIndexes) to High(FIndexes) do begin
      with FIndexes[I] do
        Display := IndexIdx = I;
    end;
  end;
  for I := Low(FIndexes) to High(FIndexes) do begin
    with FIndexes[I] do
      MenuItem.Checked := Display;
  end;

  MergeEntries(FActiveIndexName);
end;

procedure TMultiIndexViewer.SetDataMgr(const ADataMgr : TDataManager);
var
  I : Integer;
  Indexes : TDataList;
  IndexName, SelIndex : String;
begin
  IndexTabs.Tabs.Clear;
  FDataMgr := ADataMgr;

  SelIndex := '';
  for I := 0 to FDataMgr.PageIndexesDict.Count-1 do begin
    IndexName := FDataMgr.PageIndexesDict.Strings[I];
    Indexes := FDataMgr.PageIndexes[IndexName];
    if SelIndex = '' then
      SelIndex := IndexName;

    IndexTabs.Tabs.AddObject(IndexName, Indexes);
  end;
end;

function TMultiIndexViewer.GetEntry : String;
begin
  if Entries.Selected <> Nil then
    Result := Entries.Selected.Text
  else
    Result := '';
end;

procedure TMultiIndexViewer.SetEntry(const AName : String);
var
  I : Integer;
  SelItem : TTreeNode;
begin
  I := FMergedEntries.IndexOf(AName);
  if I >= 0 then begin
    SelItem := FMergedEntries.Objects[I] as TTreeNode;
    if Entries.Selected <> SelItem then
      Entries.Selected := SelItem;
    Entries.TopItem := SelItem;
    ExpandNode(SelItem);
  end else
    ShowMessage(Format('Index entry %s not found', [AName]));
end;

procedure TMultiIndexViewer.ExpandNode(const AExpandNode : TTreeNode);

  function PopulateLocs(const EntryLocs : TEntryLocs; const AParentNode : TTreeNode = Nil) : Integer;
  var
    SeeAlso, SubNode, ThisNode : TTreeNode;
    L : Integer;
    Kind : TEntryLocKind;
    Caption : String;
    SubLocs : TEntryLocs;
    LocRec : PLocRec;
    Addr : TAddress;
  begin
    Result := 0;
    if (EntryLocs = Nil) or (EntryLocs.Count <= 0) then
      Exit;

    SeeAlso := Nil;
    for L := 0 to EntryLocs.Count-1 do begin
      EntryLocs.GetEntryDetails(L, Kind, Caption, SubLocs);
      case Kind of
        ekUnknown : ;
        ekAddress : begin
            New(LocRec);
            FLocsList.Add(LocRec);
            LocRec.Kind := Kind;
            LocRec.LinkData := NewStr(Caption);
            Addr := TAddress.Create;
            Addr.Address := Caption;
            ThisNode := Entries.Items.AddChildObject(AParentNode, FAddrMgr.GetAddressCaption(Addr, True), LocRec);
            Addr.Free;
            ThisNode.ImageIndex := 2;
            ThisNode.SelectedIndex := 2;
            Inc(Result);
          end;
        ekAlias :
          begin
            if FMergedEntries.IndexOf(Caption) >= 0 then begin
              if SeeAlso = Nil then begin
                SeeAlso := Entries.Items.AddChild(AParentNode, SeeAlsoCaption);
                SeeAlso.ImageIndex := 3;
                SeeAlso.SelectedIndex := 3;
              end;
              New(LocRec);
              FLocsList.Add(LocRec);
              LocRec.Kind := Kind;
              LocRec.LinkData := NewStr(Caption);
              ThisNode := Entries.Items.AddChildObject(SeeAlso, Caption, LocRec);
              ThisNode.ImageIndex := 4;
              ThisNode.SelectedIndex := 4;
            end;
          end;
        ekSubentry :
          begin
            SubNode := Entries.Items.AddChild(AParentNode, Caption);
            SubNode.ImageIndex := 7;
            SubNode.SelectedIndex := 7;
            Inc(Result, PopulateLocs(SubLocs, SubNode));
          end;
      end;
    end;
  end;

var
  I : Integer;
  BookIndex : TPageIndex;
  EntryLocs : TEntryLocs;
begin
  Assert(FMergedEntries <> Nil);
  if (Length(FIndexes) <= 0) or (AExpandNode = Nil) then
    Exit;

  if AExpandNode.HasChildren then
    Exit;

  if FLocsList = Nil then
   FLocsList := TList.Create;

  Entries.Items.BeginUpdate;
  EntryLocs := TEntryLocs.Create;
  try
    for I := 0 to High(FIndexes) do begin
      BookIndex := FIndexes[I].Data.Index[FActiveIndexName];
      if BookIndex <> Nil then
        EntryLocs.Merge(BookIndex.GetEntryLocs(AExpandNode.Text));
    end;

    PopulateLocs(EntryLocs, AExpandNode);
    AExpandNode.Expand(False);
  finally
    EntryLocs.Free;
    Entries.Items.EndUpdate;
  end;
end;

procedure TMultiIndexViewer.SeeAlso(const AEntry : String);
begin
  SetEntry(AEntry);
end;

procedure TMultiIndexViewer.CreateBooksMenu(const AIndexName : String);
var
  MenuItem, SinglesSubMenu : TMenuItem;
  Books : TDataList;
  I : Integer;
begin
  if FDataMgr.PageIndexesDict.Find(AIndexName, I) then
    FActiveIndexName := FDataMgr.PageIndexesDict[I]
  else begin
    ShowMessage(Format('Index "%s" not found.', [AIndexName]));
    Exit;
  end;

  FStatus.StatusBegin('Initializing...', True);
  IndexTabs.TabIndex := IndexTabs.Tabs.IndexOf(FActiveIndexName);

  ClearMenu(BooksMenu);
  Books := FDataMgr.PageIndexes[FActiveIndexName];
  if Books.Count <= 0 then
    Exit;

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := 'All';
  MenuItem.Tag := CombineAllBooksTag;
  MenuItem.OnClick := BooksMenuItemClick;
  BooksMenu.Items.Add(MenuItem);

  SinglesSubMenu := TMenuItem.Create(Self);
  SinglesSubMenu.Caption := 'Only';
  SinglesSubMenu.Tag := SingleBookSubmenuTag;
  BooksMenu.Items.Add(SinglesSubMenu);
  BooksMenu.Items.Add(NewLine);

  if (Books <> Nil) and (Books.Count >= 1) then begin
    SetLength(FIndexes, Books.Count);
    for I := 0 to Books.Count-1 do begin
      FIndexes[I].Data := Books.Data[I];
      FIndexes[I].Display := True;
      if Books.Count > 1 then begin
        MenuItem := TMenuItem.Create(Self);
        FIndexes[I].MenuItem := MenuItem;
        MenuItem.Tag := I;
        MenuItem.Caption := FIndexes[I].Data.Name;
        MenuItem.Checked := FIndexes[I].Display;
        MenuItem.OnClick := BooksMenuItemClick;
        BooksMenu.Items.Add(MenuItem);

        MenuItem := TMenuItem.Create(Self);
        MenuItem.Tag := -I-1;
        MenuItem.Caption := FIndexes[I].Data.Name;
        MenuItem.OnClick := BooksMenuItemClick;
        SinglesSubMenu.Add(MenuItem);
      end;
    end;
  end;

  FStatus.StatusEnd;
end;

procedure TMultiIndexViewer.MergeEntries(const AIndexName : String);
var
  B, I : Integer;
  BookIndex : TPageIndex;
  BookIdxEntries : TObjDict;
  //BookIdxEntriesData : TStringList;
  NewNode : TTreeNode;
begin
  Assert(FDataMgr <> Nil);
  FActiveIndexName := AIndexName;
  FindEdit.Text := '';

  FStatus.StatusBegin('Populating index entries...', True);
  Entries.Items.BeginUpdate;
  Entries.Items.Clear;
  Entries.Items.Add(Nil, 'Populating entries...');
  Entries.Items.EndUpdate;
  Application.ProcessMessages;
  ClearLocsList;

  if FMergedEntries = Nil then begin
    FMergedEntries := TStringList.Create;
    FMergedEntries.Sorted := True;
    FMergedEntries.Duplicates := dupIgnore;
  end else
    FMergedEntries.Clear;

  if Length(FIndexes) > 0 then begin
    for B := 0 to High(FIndexes) do begin
      if FIndexes[B].Display then begin
        BookIndex := FIndexes[B].Data.Index[FActiveIndexName];
        if BookIndex <> Nil then begin
          BookIdxEntries := BookIndex.Entries;
          //BookIdxEntriesData := BookIndex.EntriesData;
          for I := 0 to BookIdxEntries.Count-1 do begin
            // for now the entriesData only has counts
            FMergedEntries.Add(BookIdxEntries[I]);
          end;
        end;
      end;
    end;
  end;

  Entries.Items.BeginUpdate;
  Entries.Items.Clear;
  if FMergedEntries.Count > 0 then begin
    for I := 0 to FMergedEntries.Count-1 do begin
      NewNode := Entries.Items.Add(Nil, FMergedEntries[I]);
      FMergedEntries.Objects[I] := NewNode;
    end;
  end;
  Entries.Items.EndUpdate;
  FStatus.StatusEnd;
end;

procedure TMultiIndexViewer.SetIndexName(const ANewName : String);
var
  ChangeTo : String;
begin
  if (ANewName = '') and (FDataMgr.PageIndexesDict.Count > 0) then begin
    if FDataMgr.PageIndexesDict.IndexOf('Subjects') >= 0 then
      ChangeTo := 'Subjects'
    else
      ChangeTo := FDataMgr.PageIndexesDict[0];
  end else
    ChangeTo := ANewName;

  if CompareText(ChangeTo, FActiveIndexName) <> 0 then begin
    CreateBooksMenu(ChangeTo);
    MergeEntries(ChangeTo);
  end;
end;

procedure TMultiIndexViewer.EntriesClick(Sender: TObject);
begin
  if (Entries.Selected <> Nil) and (Entries.Selected.Data = Nil) then
    FindEdit.Text := '';
end;

procedure TMultiIndexViewer.LocationsCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  IsSeeAlso : Boolean;
begin
  IsSeeAlso := (Node.Data <> Nil) and (PLocRec(Node.Data).Kind = ekAlias);
  if (Node.Text = SeeAlsoCaption) or IsSeeAlso then
    Sender.Canvas.Font.Style := Sender.Canvas.Font.Style + [fsBold];
end;

procedure TMultiIndexViewer.IndexTabsChange(Sender: TObject);
var
  NewIndexName : String;
begin
  NewIndexName := IndexTabs.Tabs[IndexTabs.TabIndex];
  if NewIndexName <> FActiveIndexName then
    ActiveIndexName := NewIndexName;
end;

procedure TMultiIndexViewer.EntriesDblClick(Sender: TObject);
var
  SelItem : TTreeNode;
  LocRec : PLocRec;
  LinkData : String;
begin
  Assert(FAddrMgr <> Nil);

  SelItem := Entries.Selected;
  if Entries.Selected <> Nil then begin
    if SelItem.Data = Nil then
      ExpandNode(SelItem)
    else begin
      LocRec := PLocRec(SelItem.Data);
      LinkData := LocRec.LinkData^;
      if LocRec.Kind = ekAlias then
        SeeAlso(LinkData)
      else
        FAddrMgr.SetAddress(LinkData)
    end;
  end;
end;

procedure TMultiIndexViewer.FindEditChange(Sender: TObject);
var
  X : Integer;
  FindLen : Integer;
  FindString : String;
begin
  FStatus.StatusBegin('Searching', True);

  FindString := FindEdit.Text;
  FindLen := Length(FindString);
  if FindLen > 0 then begin
    for X := 0 to FMergedEntries.Count-1 do begin
      if CompareText(Copy(FMergedEntries[X], 1, FindLen), FindString) = 0 then begin
        Entries.Selected := FMergedEntries.Objects[X] as TTreeNode;
        Entries.TopItem := Entries.Selected;
        FStatus.StatusEnd;
        Exit;
      end;
    end;
  end;

  FStatus.StatusEnd;
end;

procedure TMultiIndexViewer.FindEditEnter(Sender: TObject);
begin
  Entries.HideSelection := False;
end;

procedure TMultiIndexViewer.FindEditExit(Sender: TObject);
begin
  Entries.HideSelection := True;
end;

procedure TMultiIndexViewer.EntriesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Entries.Selected <> Nil) then begin
    if Entries.Selected.Count > 0 then
      Entries.Selected.Expanded := not Entries.Selected.Expanded
    else
      EntriesDblClick(Sender);
  end;
end;

end.
