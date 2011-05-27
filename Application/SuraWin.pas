unit SuraWin;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QuranStruct, ComCtrls, ExtCtrls, DFSSplitter, StdCtrls, AmlView, ImgList,
  TeeProcs, TeEngine, Chart, Series;

type
  TSuraSelectEvent = procedure(const ASura : PSuraInfoRec) of object;
  TSuraListSort = (slsNum, slsName, slsAyahCount, slsRevealed, slsCity);
  TSuraListForm = class(TForm)
    ImageList: TImageList;
    PageControl: TPageControl;
    SurasSheet: TTabSheet;
    RukuSplitter: TDFSSplitter;
    SuraList: TListView;
    RukuPanel: TPanel;
    RukuListLabel: TLabel;
    RukuList: TListView;
    AjzaSheet: TTabSheet;
    AjzaList: TListView;
    SajdasSheet: TTabSheet;
    SajdaList: TListView;
    OptionsPanel: TPanel;
    StayOnTopCheckbox: TCheckBox;
    AutoHideCheckbox: TCheckBox;
    TabSheet1: TTabSheet;
    AyahChart: TChart;
    Series1: TBarSeries;
    procedure SuraListCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure SuraListColumnClick(Sender: TObject; Column: TListColumn);
    procedure SuraListDblClick(Sender: TObject);
    procedure SuraListClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RukuListDblClick(Sender: TObject);
    procedure AjzaListDblClick(Sender: TObject);
    procedure SajdaListDblClick(Sender: TObject);
    procedure StayOnTopCheckboxClick(Sender: TObject);
    procedure ListBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  protected
    FAddrMgr : IAddressManager;
    FDataMgr : TDataManager;
    FSort : TSuraListSort;
    FSortDescend : Boolean;
    FStruct : TQuranStructure;
    FActiveSura : PSuraInfoRec;

    procedure Populate(const ASura : TSuraNum); overload;
  public
    procedure Populate(const AStruct : TQuranStructure); overload;
    property AddressMgr : IAddressManager read FAddrMgr write FAddrMgr;
    property DataMgr : TDataManager read FDataMgr write FDataMgr;
  end;

var
  SuraListForm: TSuraListForm;

implementation

{$R *.DFM}

procedure TSuraListForm.Populate(const AStruct : TQuranStructure);
var
  S : TSuraNum;
  Item : TListItem;
  ThisLoc, NextLoc : TSuraAyah;
  Range, R : Cardinal;
  AyahCountSeries : TBarSeries;
begin
  if FStruct = AStruct then
    Exit;

  FStruct := AStruct;
  SuraList.Items.Clear;
  AjzaList.Items.Clear;
  if AStruct = Nil then
    Exit;

  AyahCountSeries := AyahChart[0] as TBarSeries;

  SuraList.Items.BeginUpdate;
  for S := Low(TSuraNum) to High(TSuraNum) do begin
    Item := SuraList.Items.Add;
    Item.Caption := IntToStr(S);
    Item.ImageIndex := 0;
    Item.StateIndex := 0;
    Item.Data := AStruct.SuraData[S];
    with AStruct.Sura[S] do begin
      Item.Subitems.Add(suraName);
      Item.Subitems.Add(IntToStr(ayahCount));
      Item.Subitems.Add(IntToStr(revealedNum));
      Item.Subitems.Add(revealedInCityName);
      AyahCountSeries.Add(ayahCount, IntToStr(S), clInfoBk);
    end;
  end;
  SuraList.Items.EndUpdate;

  AjzaList.Items.BeginUpdate;
  for S := Low(TJuzNum) to High(TJuzNum) do begin
    Item := AjzaList.Items.Add;
    Item.Caption := IntToStr(S);
    Item.ImageIndex := 0;
    Item.StateIndex := 0;
    if S < High(TJuzNum) then
      NextLoc := AStruct.Ajza[S+1]
    else begin
      NextLoc.suraNum := High(TSuraNum);
      NextLoc.ayahNum := AStruct.Sura[NextLoc.suraNum].ayahCount;
    end;
    ThisLoc := AStruct.Ajza[S];
    if ThisLoc.suraNum = NextLoc.suraNum then
      Range := NextLoc.ayahNum - ThisLoc.ayahNum + 1
    else begin
      Range := AStruct.Sura[ThisLoc.suraNum].ayahCount - ThisLoc.ayahNum + 1;
      for R := Succ(ThisLoc.suraNum) to Pred(NextLoc.suraNum) do begin
        Inc(Range, AStruct.Sura[R].ayahCount);
      end;
      Inc(Range, NextLoc.ayahNum);
    end;
    with AStruct.Ajza[S] do begin
      Item.Subitems.Add(IntToStr(suraNum));
      Item.Subitems.Add(AStruct.SuraName[suraNum]);
      Item.Subitems.Add(IntToStr(ayahNum));
      Item.Subitems.Add(IntToStr(Range));
    end;
  end;
  AjzaList.Items.EndUpdate;

  SajdaList.Items.BeginUpdate;
  for S := Low(TSajdaNum) to High(TSajdaNum) do begin
    Item := SajdaList.Items.Add;
    Item.Caption := IntToStr(S);
    Item.ImageIndex := 0;
    Item.StateIndex := 0;
    with AStruct.Sajda[S] do begin
      Item.Subitems.Add(IntToStr(suraNum));
      Item.Subitems.Add(AStruct.SuraName[suraNum]);
      Item.Subitems.Add(IntToStr(ayahNum));
    end;
  end;
  SajdaList.Items.EndUpdate;
end;

procedure TSuraListForm.Populate(const ASura : TSuraNum);
var
  I : Integer;
begin
  Assert(FStruct <> Nil);

  RukuList.Items.BeginUpdate;
  RukuList.Items.Clear;
  FActiveSura := FStruct.SuraData[ASura];
  RukuListLabel.Caption := Format('Sura %d%sSections (%d)  ', [FActiveSura.suraNum, #13#10, FActiveSura.rukuCount]);

  if FActiveSura.rukuCount > 1 then begin
    for I := 1 to FActiveSura.rukuCount do begin
      with FActiveSura.rukus[I], RukuList.Items.Add do begin
        ImageIndex := 1;
        StateIndex := 1;
        Caption := IntToStr(I);
        Subitems.Add(IntToStr(startAyah));
        Subitems.Add(IntToStr(endAyah));
        Subitems.Add(IntToStr(endAyah-startAyah+1));
      end;
    end;
  end else begin
    with FActiveSura^, RukuList.Items.Add do begin
      ImageIndex := 1;
      StateIndex := 1;
      Caption := '1';
      Subitems.Add('1');
      Subitems.Add(IntToStr(ayahCount));
      Subitems.Add(IntToStr(ayahCount));
    end;
  end;
  RukuList.Items.EndUpdate;
end;

procedure TSuraListForm.SuraListCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);

  procedure CompareNum(Num1, Num2 : LongInt);
  begin
    if Num1 < Num2 then
      Compare := -1
    else if Num1 > Num2 then
      Compare := 1
    else
      Compare := 0;
  end;

begin
  FSort := TSuraListSort(Data);
  case FSort of
    slsNum : CompareNum(PSuraInfoRec(Item1.Data)^.suraNum, PSuraInfoRec(Item2.Data)^.suraNum);
    slsName : Compare := CompareText(PSuraInfoRec(Item1.Data)^.suraName, PSuraInfoRec(Item2.Data)^.suraName);
    slsAyahCount : CompareNum(PSuraInfoRec(Item1.Data)^.ayahCount, PSuraInfoRec(Item2.Data)^.ayahCount);
    slsRevealed : CompareNum(PSuraInfoRec(Item1.Data)^.revealedNum, PSuraInfoRec(Item2.Data)^.revealedNum);
    slsCity :
      begin
        Compare := CompareText(PSuraInfoRec(Item1.Data)^.revealedInCityName, PSuraInfoRec(Item2.Data)^.revealedInCityName);
        if Compare = 0 then
          CompareNum(PSuraInfoRec(Item1.Data)^.revealedNum, PSuraInfoRec(Item2.Data)^.revealedNum);
      end;
  end;

  if FSortDescend then begin
    if Compare < 0 then
      Compare := 1
    else if Compare > 0 then
      Compare := -1;
  end;
end;

procedure TSuraListForm.SuraListColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  if FSort = TSuraListSort(Column.Id) then
    FSortDescend := not FSortDescend;
  SuraList.CustomSort(Nil, Column.Id);
end;

procedure TSuraListForm.SuraListDblClick(Sender: TObject);
begin
  if Assigned(FAddrMgr) then begin
    if AutoHideCheckbox.Checked then begin
      Hide;
      Application.ProcessMessages;
    end;
    FAddrMgr.SetAddress(Format('qs:%d', [PSuraInfoRec(SuraList.Selected.Data).SuraNum]));
  end;
end;

procedure TSuraListForm.SuraListClick(Sender: TObject);
begin
  Populate(PSuraInfoRec(SuraList.Selected.Data).suraNum);
end;

procedure TSuraListForm.FormCreate(Sender: TObject);
begin
  // the design-time width is assumed to be exact width of SuraList width
  Width := GetSystemMetrics(SM_CXVSCROLL) + (PageControl.ClientWidth-SuraList.Width) +
           SuraList.Width + (SuraList.Columns.Count * 2);
end;

procedure TSuraListForm.RukuListDblClick(Sender: TObject);
begin
  if Assigned(FAddrMgr) and Assigned(FActiveSura) then begin
    if AutoHideCheckbox.Checked then begin
      Hide;
      Application.ProcessMessages;
    end;
    if FActiveSura.rukuCount > 1 then
      FAddrMgr.SetAddress(CreateQuranAddress(qatGeneric, FActiveSura.SuraNum, FActiveSura.rukus[RukuList.Selected.Index+1].startAyah))
    else
      FAddrMgr.SetAddress(CreateQuranAddress(qatGeneric, FActiveSura.SuraNum));
  end;
end;

procedure TSuraListForm.AjzaListDblClick(Sender: TObject);
var
  Loc : TSuraAyah;
begin
  if Assigned(FAddrMgr) then begin
    if AutoHideCheckbox.Checked then begin
      Hide;
      Application.ProcessMessages;
    end;
    Loc := FStruct.Ajza[AjzaList.Selected.Index+1];
    FAddrMgr.SetAddress(CreateQuranAddress(qatGeneric, Loc.suraNum, Loc.ayahNum));
  end;
end;

procedure TSuraListForm.SajdaListDblClick(Sender: TObject);
var
  Loc : TSuraAyah;
begin
  if Assigned(FAddrMgr) then begin
    if AutoHideCheckbox.Checked then begin
      Hide;
      Application.ProcessMessages;
    end;
    Loc := FStruct.Sajda[SajdaList.Selected.Index+1];
    FAddrMgr.SetAddress(CreateQuranAddress(qatGeneric, Loc.suraNum, Loc.ayahNum));
  end;
end;

procedure TSuraListForm.StayOnTopCheckboxClick(Sender: TObject);
begin
  if StayOnTopCheckbox.Checked then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
end;

procedure TSuraListForm.ListBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Sender is TListView) and (Assigned((Sender as TListView).OnDblClick)) then
    (Sender as TListView).OnDblClick(Sender);
end;

end.
