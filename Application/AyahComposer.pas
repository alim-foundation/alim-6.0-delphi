unit AyahComposer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst, AmlView, HtmlView, QuranStruct, StStrS, ClipBrd,
  StStrL, XmlObjModel, IslUtils, TextView;

type
  TAyahCompositionAction = (acaNone, acaCopy, acaPrint);
  TAyahSelectionForm = class(TForm)
    TextsGroupBox: TGroupBox;
    BooksList: TCheckListBox;
    AyahsGroupBox: TGroupBox;
    AyahChooseLabel: TLabel;
    AyahsEdit: TEdit;
    OK: TButton;
    Cancel: TButton;
    ExampleLabel: TLabel;
    Label2: TLabel;
    InclNotesCheck: TCheckBox;
    AyahSepCheck: TCheckBox;
    HTMLViewer: THTMLViewer;
    FontDialog: TFontDialog;
    ChooseFontBtn: TButton;
    procedure OKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HTMLViewerPrintHeader(Sender: TObject; Canvas: TCanvas;
      NumPage, W, H: Integer; var StopPrinting: Boolean);
    procedure ChooseFontBtnClick(Sender: TObject);
  protected
    FDataMgr : TDataManager;
    FAction : TAyahCompositionAction;
    FQuranStruct : TQuranStructure;
    FAyahsMarked : TMarkedAyahs;
    FPrintHeader : TPrintHeaderInfo;

    function GetAsText : String;
    function GetAsHtml : String;

    function GetAyahs : String; virtual;
    procedure SetAction(const AAction : TAyahCompositionAction);
    procedure SetAyahs(const AAyahs : String); virtual;
    procedure SetBookId(const ABookId : String); virtual;
    procedure SetDataManager(const ADataMgr : TDataManager); virtual;
    procedure SetQuranStruct(const AStruct : TQuranStructure); virtual;
  public
    property DataMgr : TDataManager read FDataMgr write SetDataManager;
    property BookId : String write SetBookId;
    property QuranStruct : TQuranStructure read FQuranStruct write SetQuranStruct;

    property Ayahs : String read GetAyahs write SetAyahs;
    property Action : TAyahCompositionAction read FAction write SetAction;

    property AyahsMarked : TMarkedAyahs read FAyahsMarked write FAyahsMarked;
    property AsText : String read GetAsText;
    property AsHtml : String read GetAsHtml;
  end;

var
  AyahSelectionForm : TAyahSelectionForm;

implementation

uses QuranView, StBits;

{$R *.DFM}

function TAyahSelectionForm.GetAsText : String;
const
  CRLF = #13#10;
var
  I, J, TotalChecked : Integer;
  SingleChecked : Boolean;
  Marked : TStBits;
  S : TSuraNum;
  A : TAyahNum;
  Bit : LongInt;
  QuranData : TQuranData;
  Notes : TXmlNodeList;
  AyahElem, NoteElem : TXmlElement;
  AyahText : String;
begin
  Result := '';

  TotalChecked := 0;
  for I := 0 to BooksList.Items.Count-1 do
    if BooksList.Checked[I] then
      Inc(TotalChecked);

  if TotalChecked = 0 then
    Exit;
  SingleChecked := TotalChecked = 1;

  for S := Low(TSuraNum) to High(TSuraNum) do begin
    Marked := FAyahsMarked.Suras[S];
    if Marked.Count > 0 then begin
      Result := Result + Format('%0:sSura %1:s (%2:d)%0:s%3:s%0:s', [CRLF, FAyahsMarked.QuranStruct.SuraName[S], S, PadChS('', '-', 79)]);
      Bit := Marked.FirstSet;
      while Bit <> -1 do begin
        A := Bit+1;
        if not SingleChecked then
          Result := Result + Format('%s[Ayah %d]%s', [CRLF, A, CRLF]);
        for I := 0 to BooksList.Items.Count-1 do begin
          if not BooksList.Checked[I] then
            continue;

          QuranData := TQuranData(BooksList.Items.Objects[I]);
          try
            AyahElem := QuranData.ReadAyah(S, A);
            AyahText := GetElementText(AyahElem, True);
          except
            AyahText := Format('Unable to read Sura %s Ayah %d from %s', [S, A, QuranData.Name]);
          end;
          if SingleChecked then
            Result := Result + Format('%d.  %s%s', [A, AyahText, CRLF])
          else
            Result := Result + Format('(%s) %s%s', [QuranData.ShortName, AyahText, CRLF]);
          if InclNotesCheck.Checked then begin
            Notes := AyahElem.GetChildElementsByTagName('note');
            if Notes.Length > 0 then begin
              for J := 0 to Notes.Length-1 do begin
                NoteElem := Notes.Item(J) as TXmlElement;
                Result := Result + Format('* note %s: %s%s', [GetElemAttr(NoteElem, 'id', '??'), GetElementText(NoteElem, True), CRLF]);
              end;
            end;
            Notes.Free;
          end;
        end;
        Bit := Marked.NextSet(Bit);
      end;
    end;
  end;
end;

function TAyahSelectionForm.GetAsHtml : String;
var
  AC, I, J, TotalChecked : Integer;
  SingleChecked : Boolean;
  Marked : TStBits;
  S : TSuraNum;
  A : TAyahNum;
  Bit : LongInt;
  QuranData : TQuranData;
  Notes : TXmlNodeList;
  AyahElem, NoteElem : TXmlElement;
  AyahChild : TXmlNode;
  AyahText, AyahAlign, FontSize, AyahSep : String;
  AyahFont : TAmlDataFont;
  SingleCheckedBook : TAmlData;
begin
  Result := '';
  TotalChecked := 0;
  for I := 0 to BooksList.Items.Count-1 do
    if BooksList.Checked[I] then begin
      Inc(TotalChecked);
      SingleCheckedBook := TAmlData(BooksList.Items.Objects[I]);
    end;

  if TotalChecked = 0 then
    Exit;
  SingleChecked := TotalChecked = 1;

  FillChar(FPrintHeader, SizeOf(FPrintHeader), 0);
  FPrintHeader.AppTitle := PrintHeaderAppName;
  if SingleChecked then
    FPrintHeader.BookName := SingleCheckedBook.Name
  else
    FPrintHeader.BookName := 'Quran Comparison';

  if AyahSepCheck.Checked then
    AyahSep := '<hr size=1 width=100% color=silver noshade>'
  else
    AyahSep := '';

  for S := Low(TSuraNum) to High(TSuraNum) do begin
    Marked := FAyahsMarked.Suras[S];
    if Marked.Count > 0 then begin
      Result := Result + Format('<h2>Sura %s (%d)</h2><hr size=1 width=100%% color=silver noshade>', [FAyahsMarked.QuranStruct.SuraName[S], S]);
      Result := Result + '<table>';
      Bit := Marked.FirstSet;
      while Bit <> -1 do begin
        A := Bit+1;
        if not SingleChecked then
          Result := Result + Format('<tr><td colspan=2><font size=+1><b>Ayah %d</b></font></td></tr>', [A]);
        for I := 0 to BooksList.Items.Count-1 do begin
          if not BooksList.Checked[I] then
            continue;

          QuranData := TQuranData(BooksList.Items.Objects[I]);
          AyahFont := QuranData.GetFont('ayah');
          try
            AyahElem := QuranData.ReadAyah(S, A);
            AyahText := '';
            for AC := 0 to AyahElem.ChildNodes.Length-1 do begin
              AyahChild := AyahElem.ChildNodes.Item(AC);
              if AyahChild.NodeName = '' then
                AyahText := AyahText + AyahChild.NodeValue
              else if AyahChild.NodeName = 'fn' then
                AyahText := AyahText + Format(' <sup>%s</sup> ', [(AyahChild as TXmlElement).Text]);
            end;
          except
            AyahText := Format('Unable to read Sura %d Ayah %d from %s', [S, A, QuranData.Name]);
          end;
          Result := Result + '<tr valign=top>';
          if QuranData.FlagIsSet(qdfIsArabic) then begin
            AyahAlign := 'align=right';
            FontSize := 'face="' + AyahFont.Name + '" size=5';
            AyahText := Reverse(AyahText);
          end else begin
            AyahAlign := '';
            FontSize := '';
          end;
          if SingleChecked then
            Result := Result + Format('<td align=right>%d.</td><td %s><font %s>%s</font>', [A, AyahAlign, FontSize, AyahText])
          else
            Result := Result + Format('<td align=right>%s</td><td %s><font %s>%s</font>', [QuranData.ShortName, AyahAlign, FontSize, AyahText]);
          if InclNotesCheck.Checked then begin
            Notes := AyahElem.GetChildElementsByTagName('note');
            if Notes.Length > 0 then begin
              for J := 0 to Notes.Length-1 do begin
                NoteElem := Notes.Item(J) as TXmlElement;
                Result := Result + Format('<br><u>Note %s</u>: %s', [GetElemAttr(NoteElem, 'id', '??'), GetElementText(NoteElem, False)]);
              end;
            end;
            Notes.Free;
          end;
          Result := Result + AyahSep + '</td></tr>';
        end;
        Bit := Marked.NextSet(Bit);
      end;
      Result := Result + '</table>';
    end;
  end;
end;

function TAyahSelectionForm.GetAyahs : String;
begin
  Result := AyahsEdit.Text;
end;

procedure TAyahSelectionForm.SetAction(const AAction : TAyahCompositionAction);
begin
  FAction := AAction;
  AyahSepCheck.Enabled := FAction = acaPrint;
  ChooseFontBtn.Enabled := FAction = acaPrint;
end;

procedure TAyahSelectionForm.SetAyahs(const AAyahs : String);
begin
  AyahsEdit.Text := AAyahs;
end;

procedure TAyahSelectionForm.SetBookId(const ABookId : String);
var
  I : Integer;
begin
  if BooksList.Items.Count > 0 then begin
    for I := 0 to BooksList.Items.Count-1 do
      BooksList.Checked[I] := CompareText(ABookId, TAmlData(BooksList.Items.Objects[I]).Id) = 0;
  end;
end;

procedure TAyahSelectionForm.SetDataManager(const ADataMgr : TDataManager);

  function Include(const AData : TAmlData) : Boolean;
  begin
    Result :=
        (FAction = acaNone) or
        ((FAction = acaCopy) and AData.FlagIsSet(adfCanCopyText)) or
        ((FAction = acaPrint) and AData.FlagIsSet(adfCanPrintText));
  end;

var
  QuranList : TDataList;
  I, NewItem : Integer;
  AmlData : TAmlData;
begin
  FDataMgr := ADataMgr;
  BooksList.Clear;
  QuranList := FDataMgr.DataTypes['quran_text'];
  if (QuranList <> Nil) and (QuranList.Count > 0) then begin
    // first add all the non-Arabic texts in sorted order
    BooksList.Sorted := True;
    for I := 0 to QuranList.Count-1 do begin
      AmlData := QuranList.Data[I] as TAmlData;
      if AmlData.FlagIsSet(qdfIsArabic) then
        continue;

      if Include(AmlData) then begin
        NewItem := BooksList.Items.AddObject(AmlData.Name, AmlData);
        BooksList.Checked[NewItem] := AmlData.FlagIsSet(qdfMarkedForCopyPrint);
      end;
    end;

    // now add the Arabic texts at the top of the list
    BooksList.Sorted := False;
    for I := 0 to QuranList.Count-1 do begin
      AmlData := QuranList.Data[I] as TAmlData;
      if not AmlData.FlagIsSet(qdfIsArabic) then
        continue;
      if Include(AmlData) then begin
        BooksList.Items.InsertObject(0, AmlData.Name, AmlData);
        BooksList.Checked[0] := AmlData.FlagIsSet(qdfMarkedForCopyPrint);
      end;
    end;
  end;
end;

procedure TAyahSelectionForm.SetQuranStruct(const AStruct : TQuranStructure);
begin
  FAyahsMarked.QuranStruct := AStruct;
end;

procedure TAyahSelectionForm.OKClick(Sender: TObject);
var
  Html : String;
begin
  FAyahsMarked.Clear;
  if not FAyahsMarked.Mark(Ayahs) then
    Exit;

  case FAction of
    acaNone : ;
    acaCopy : Clipboard.AsText := Self.AsText;
    acaPrint :
      begin
        Html := Self.AsHtml;
        if Html <> '' then begin
          HtmlViewer.DefFontName := FontDialog.Font.Name;
          HtmlViewer.DefFontSize := FontDialog.Font.Size;
          HtmlViewer.LoadFromBuffer(PChar(Html), Length(Html));
          HtmlViewer.Print(1, 9999);
        end;
      end;
  end;
  ModalResult := mrOk;
end;

procedure TAyahSelectionForm.FormCreate(Sender: TObject);
begin
  FAyahsMarked := TMarkedAyahs.Create;
end;

procedure TAyahSelectionForm.FormDestroy(Sender: TObject);
begin
  FAyahsMarked.Free;
end;

procedure TAyahSelectionForm.HTMLViewerPrintHeader(Sender: TObject;
  Canvas: TCanvas; NumPage, W, H: Integer; var StopPrinting: Boolean);
begin
  PrintStandardHeader(FPrintHeader, Canvas, NumPage, W, H);
end;

procedure TAyahSelectionForm.ChooseFontBtnClick(Sender: TObject);
begin
  FontDialog.Execute;
end;

end.
