unit QuranStruct;

interface

uses
  SysUtils, Windows, Classes, XmlObjModel, StBits;

const
  SuraNameLen = 32;
  CityNameLen = 10;
  SuraAyahStrDelim = '.';

type
  TSuraNum = 1..114;
  TAyahNum = 1..286;
  TRukuNum = 1..40;
  TJuzNum = 1..30;
  TSajdaNum = 1..19;

  TSuraAyah =
    record
      suraNum : TSuraNum;
      ayahNum : TAyahNum;
    end;

  TRukuInfoRec =
    record
      startAyah : SmallInt;
      endAyah : SmallInt;
    end;

  PSuraInfoRec = ^TSuraInfoRec;
  TSuraInfoRec =
    record
      suraNum : TSuraNum;
      revealedNum : TSuraNum;
      suraName : array[0..SuraNameLen+1] of Char;
      revealedInCityName : array[0..CityNameLen+1] of Char;
      ayahCount : TAyahNum;
      rukuCount : TRukuNum;
      rukus : array[TRukuNum] of TRukuInfoRec;
    end;
  TSuraInfoArray = array[TSuraNum] of TSuraInfoRec;
  TJuzArray = array[TJuzNum] of TSuraAyah;
  TSajdaArray = array[TSajdaNum] of TSuraAyah;

  TMoveDirection = (mdPrev, mdNext);
  TQuranStructure = class(TObject)
  protected
    FSuraInfo : TSuraInfoArray;
    FSajda : TSajdaArray;
    FAjza : TJuzArray;

    function GetSura(Index : TSuraNum) : TSuraInfoRec;
    function GetSuraData(Index : TSuraNum) : PSuraInfoRec;
    function GetSuraName(Index : TSuraNum) : String;
  public

    procedure SaveToStream(Stream : TStream); virtual;
      {-save the entire sura info array to a stream}
    procedure LoadFromStream(Stream : TStream); virtual;
      {-load the entire sura info array from a stream}
    procedure LoadFromAML(const ARootElem : TXmlElement);
      {-load the entire info array from a structured XML file}
    function FindAyahRuku(ASura : TSuraNum; AAyah : TAyahNum) : TRukuNum;
      {-given the sura and ayah, find what ruku the ayah is in}
    function MoveSura(const ASura : TSuraNum; const ADirection : TMoveDirection) : TSuraNum;
      {-given a sura, move either forward or backward in that sura}
    function MoveAyah(const ALoc : TSuraAyah; const ADirection : TMoveDirection) : TSuraAyah;
      {-move to a specific ayah in a given direction}
    function StrToSuraAyah(const ASuraAyah : ShortString; const ADefault : TSuraAyah) : TSuraAyah;
      {-convert ASuraAyah string to a record format, return ADefault if any part is errorneous}

    property Sura[Index : TSuraNum] : TSuraInfoRec read GetSura;
    property SuraData[Index : TSuraNum] : PSuraInfoRec read GetSuraData;
    property SuraName[Index : TSuraNum] : String read GetSuraName;
    property Suras : TSuraInfoArray read FSuraInfo;
    property Sajda : TSajdaArray read FSajda;
    property Ajza : TJuzArray read FAjza;
  end;

  EDuplicateAyahMarked = Exception;
  TMarkedSurasArray = array[TSuraNum] of TStBits;
  TMarkDuplicateAyahs = (mdAllow, mdPrevent);
  TMarkedAyahs = class(TObject)
  protected
    FQuranStruct : TQuranStructure;
    FSuras : TMarkedSurasArray;
    FDuplicates : TMarkDuplicateAyahs;

    function GetMarkedCount : Cardinal; virtual;
    procedure SetQuranStruct(const AStruct : TQuranStructure); virtual;
  public
    destructor Destroy; override;

    procedure Clear; virtual;
    procedure Mark(const ASura  : TSuraNum); overload;
    procedure Mark(const ASura  : TSuraNum; const AAyah : TAyahNum); overload;
    procedure Mark(const ASura  : TSuraNum; const AStartAyah, AEndAyah : TAyahNum); overload;
    procedure Mark(const AStartSura  : TSuraNum; const AStartAyah : TAyahNum;
                   const AEndSura  : TSuraNum; const AEndAyah : TAyahNum); overload;

    function Mark(const AExpression : String) : Boolean; overload;
    property MarkedCount : Cardinal read GetMarkedCount;

    property QuranStruct : TQuranStructure read FQuranStruct write SetQuranStruct;
    property Suras : TMarkedSurasArray read FSuras;
    property Duplicates : TMarkDuplicateAyahs read FDuplicates write FDuplicates;
  end;

  TAyahImgRect =
    record
      Sura : TSuraNum;
      Ayah : TAyahNum;
      xStart, xEnd : Word;
      yStart, yEnd : Word;
    end;

  TAyahImgArray = array of TAyahImgRect;
  TPageImgData = array of TAyahImgArray;

  function SuraAyah(const ASura : TSuraNum; const AAyah : TAyahNum) : TSuraAyah;

implementation

uses StStrS, IslUtils, Dialogs;

function TQuranStructure.GetSura(Index : TSuraNum) : TSuraInfoRec;
begin
  Result := FSuraInfo[Index];
end;

function TQuranStructure.GetSuraData(Index : TSuraNum) : PSuraInfoRec;
begin
  Result := @FSuraInfo[Index];
end;

function TQuranStructure.GetSuraName(Index : TSuraNum) : String;
begin
  Result := FSuraInfo[Index].suraName;
end;

procedure TQuranStructure.SaveToStream(Stream : TStream);
begin
  Stream.Write(FSuraInfo, SizeOf(FSuraInfo));
  Stream.Write(Sajda, SizeOf(Sajda));
  Stream.Write(Ajza, SizeOf(Ajza));
end;

procedure TQuranStructure.LoadFromStream(Stream : TStream);
begin
  Stream.Read(FSuraInfo, SizeOf(FSuraInfo));
  Stream.Read(FSajda, SizeOf(FSajda));
  Stream.Read(FAjza, SizeOf(FAjza));
end;

function TQuranStructure.FindAyahRuku(ASura : TSuraNum; AAyah : TAyahNum) : TRukuNum;
var
  R : TRukuNum;
begin
  Result := 1;
  with FSuraInfo[ASura] do
    for R := 1 to RukuCount do
      if (AAyah >= Rukus[R].StartAyah) and (AAyah <= Rukus[R].EndAyah) then begin
        Result := R;
        Exit;
      end;
end;

function TQuranStructure.MoveSura(const ASura : TSuraNum; const ADirection : TMoveDirection) : TSuraNum;
begin
  Result := ASura;
  case ADirection of
    mdPrev :
      if ASura > Low(TSuraNum) then
        Result := Pred(ASura)
      else
        Result := High(TSuraNum);
    mdNext :
      if ASura < High(TSuraNum) then
        Result := Succ(ASura)
      else
        Result := Low(TSuraNum);
  end;
end;

function TQuranStructure.MoveAyah(const ALoc : TSuraAyah; const ADirection : TMoveDirection) : TSuraAyah;
begin
  Result := ALoc;
  case ADirection of
    mdPrev :
      with FSuraInfo[ALoc.suraNum] do begin
        if ALoc.ayahNum > 1 then
          Dec(Result.ayahNum)
        else begin
          if ALoc.suraNum > 1 then begin
            Dec(Result.suraNum);
            Result.ayahNum := FSuraInfo[Result.suraNum].ayahCount;
          end else begin
            Result.suraNum := High(TSuraNum);
            Result.ayahNum := FSuraInfo[Result.suraNum].ayahCount;
          end;
        end;
      end;

    mdNext :
      with FSuraInfo[ALoc.suraNum] do begin
        if ALoc.ayahNum < ayahCount then
          Inc(Result.ayahNum)
        else begin
          if ALoc.suraNum < High(TSuraNum) then begin
            Inc(Result.suraNum);
            Result.ayahNum := 1;
          end else begin
            Result.suraNum := 1;
            Result.ayahNum := 1;
          end;
        end;
      end;
  end;
end;

procedure TQuranStructure.LoadFromAML(const ARootElem : TXmlElement);
var
  SuraElem, SuraChildElem, RukuElem : TXmlElement;
  S, E, R, I, SuraNumAttr, RukuNumAttr, TagNumAttr : Cardinal;
  SurasTag, SajdaTag, AzjaTag, TagChild : TXmlElement;
  SuraAyah : TSuraAyah;
begin
  SajdaTag := ARootElem.FindElement('sajdatilawa');
  for I := 0 to SajdaTag.ChildNodes.Length-1 do begin
    if SajdaTag.ChildNodes.Item(I).NodeType <> ELEMENT_NODE then
      continue;
    TagChild := SajdaTag.ChildNodes.Item(I) as TXmlElement;
    TagNumAttr := StrToInt(TagChild.GetAttribute('num'));
    SuraAyah.SuraNum := StrToInt(TagChild.GetAttribute('sura'));
    SuraAyah.AyahNum := StrToInt(TagChild.GetAttribute('ayah'));
    FSajda[TagNumAttr] := SuraAyah;
  end;

  AzjaTag := ARootElem.FindElement('ajza');
  for I := 0 to AzjaTag.ChildNodes.Length-1 do begin
    if AzjaTag.ChildNodes.Item(I).NodeType <> ELEMENT_NODE then
      continue;
    TagChild := AzjaTag.ChildNodes.Item(I) as TXmlElement;
    TagNumAttr := StrToInt(TagChild.GetAttribute('num'));
    with FAjza[TagNumAttr] do begin
      SuraNum := StrToInt(TagChild.GetAttribute('sura'));
      AyahNum := StrToInt(TagChild.GetAttribute('ayah'));
    end;
  end;

  SurasTag := ARootElem.FindElement('suras');
  for S := 0 to SurasTag.ChildNodes.Length-1 do begin
    if SurasTag.ChildNodes.Item(S).NodeType <> ELEMENT_NODE then
      continue;
    SuraElem := (SurasTag.ChildNodes.Item(S) as TXmlElement);
    SuraNumAttr := StrToInt(SuraElem.GetAttribute('num'));
    with Suras[SuraNumAttr] do begin
      suraNum := SuraNumAttr;
      ayahCount := StrToInt(SuraElem.GetAttribute('ayahcount'));
      rukuCount := 1;

      for E := 0 to SuraElem.ChildNodes.Length-1 do begin
        if SuraElem.ChildNodes.Item(E).NodeType <> ELEMENT_NODE then
          continue;
        SuraChildElem := (SuraElem.ChildNodes.Item(E) as TXmlElement);
        if SuraChildElem.TagName = 'name' then begin
          StrPCopy(suraName, SuraChildElem.Text);
        end else if SuraChildElem.TagName = 'revealed' then begin
          revealedNum := StrToInt(SuraChildElem.GetAttribute('num'));
          StrPCopy(revealedInCityName, SuraChildElem.GetAttribute('city'));
        end else if SuraChildElem.TagName = 'rukus' then begin
          for R := 0 to SuraChildElem.ChildNodes.Length-1 do begin
            if SuraChildElem.ChildNodes.Item(R).NodeType <> ELEMENT_NODE then
              continue;
            RukuElem := (SuraChildElem.ChildNodes.Item(R) as TXmlElement);
            RukuNumAttr := StrToInt(RukuElem.GetAttribute('num'));
            with Rukus[RukuNumAttr] do begin
              startAyah := StrToInt(RukuElem.GetAttribute('startayah'));
              endAyah := StrToInt(RukuElem.GetAttribute('endayah'));
            end;
          end;

          // the rukuCount is whatever the last rukuNumber was
          rukuCount := RukuNumAttr;
        end;
      end;
    end;
  end;
end;

function TQuranStructure.StrToSuraAyah(const ASuraAyah : ShortString; const ADefault : TSuraAyah) : TSuraAyah;
var
  P, D : Integer;
  Data : String;
  Sura, Ayah : Word;
begin
  Result := ADefault;

  // if there is any data like a book id, skip it
  P := Pos(':', ASuraAyah);
  if P > 0 then
    Data := Copy(ASuraAyah, P+1, Length(ASuraAyah))
  else
    Data := ASuraAyah;

  // now find the sura.ayah formatted text
  // -- if there is no SuraAyahStrDelim then we assume it's an ayah only (same sura)
  D := Pos(SuraAyahStrDelim, Data);
  if D > 0 then begin
    if Str2WordS(Copy(Data, 1, D-1), Sura) then begin
      if (Sura > 0) and (Sura <= High(TSuraNum)) then begin
        Result.suraNum := Sura;
        Str2Words(Copy(Data, D+1, Length(Data)), Ayah);
      end;
    end;
  end else
    Str2WordS(Data, Ayah);

  if (Ayah > 0) and (Ayah <= FSuraInfo[Result.suraNum].ayahCount) then
    Result.ayahNum := Ayah
  else
    Result.ayahNum := 1;
end;

//------------------------------------------------------------------------------

destructor TMarkedAyahs.Destroy;
var
  I : Integer;
begin
  for I := Low(TSuraNum) to High(TSuraNum) do
    if FSuras[I] <> Nil then FSuras[I].Free;
  inherited Destroy;
end;

procedure TMarkedAyahs.Clear;
var
  I : Integer;
begin
  for I := Low(TSuraNum) to High(TSuraNum) do
    if FSuras[I] <> Nil then FSuras[I].Clear;
end;

function TMarkedAyahs.GetMarkedCount : Cardinal;
var
  I : Integer;
begin
  Result := 0;
  for I := Low(TSuraNum) to High(TSuraNum) do
    Inc(Result, FSuras[I].Count);
end;

procedure TMarkedAyahs.Mark(const ASura  : TSuraNum);
begin
  if (FDuplicates = mdPrevent) and (FSuras[ASura].Count > 0) then
    raise EDuplicateAyahMarked.CreateFmt('Duplicate ayahs marked in Sura', [ASura]);

  FSuras[ASura].SetBits;
end;

procedure TMarkedAyahs.Mark(const ASura  : TSuraNum; const AAyah : TAyahNum);
begin
  if (FDuplicates = mdPrevent) and FSuras[ASura].BitIsSet(AAyah-1) then
    raise EDuplicateAyahMarked.CreateFmt('Ayah %d in Sura %d is already marked', [AAyah, ASura]);
  FSuras[ASura].SetBit(AAyah-1)
end;

procedure TMarkedAyahs.Mark(const ASura  : TSuraNum; const AStartAyah, AEndAyah : TAyahNum);
var
  A : TAyahNum;
begin
  Assert(AStartAyah <= AEndAyah);
  if (FDuplicates = mdPrevent) then begin
    for A := AStartAyah to AEndAyah do
      if FSuras[ASura].BitIsSet(A-1) then
        raise EDuplicateAyahMarked.CreateFmt('Ayah %d in Sura %d is already marked', [A, ASura]);
  end;
  for A := AStartAyah to AEndAyah do
    FSuras[ASura].SetBit(A-1);
end;

procedure TMarkedAyahs.Mark(const AStartSura  : TSuraNum; const AStartAyah : TAyahNum;
                            const AEndSura  : TSuraNum; const AEndAyah : TAyahNum);
var
  SurasCount, S : TSuraNum;
begin
  Assert(AStartSura <= AEndSura);
  SurasCount := (AEndSura - AStartSura) + 1;
  if SurasCount = 1 then
    Mark(AStartSura, AStartAyah, AEndAyah)
  else begin
    with FQuranStruct.Sura[AStartSura] do
      Mark(AStartSura, AStartAyah, ayahCount);
    if SurasCount > 2 then
      for S := Succ(AStartSura) to Pred(AEndSura) do
        Mark(S);
    with FQuranStruct.Sura[AEndSura] do
      Mark(AEndSura, 1, AEndAyah);
  end;
end;

{
  Format:
  multiple "singletons" separated by ',' where each singleton can be:
  * s          the whole sura
  * s.a        a single sura/ayah
  * s:r        a single sura/ruku
  * s.a-a      a range of ayahs within a sura
  * s.a-s.a    a range of ayahs across a range of suras
  * s:r-s:r    a range of rukus across a range of suras

  e.g: 4,4-5,9.5-8,9.17-10.12
}

type
  TSuraAyahPropertyStyle = (psSura, psSuraRuku, psSuraAyah);
  TSuraAyahAlonePropStyle = (apsSura, apsRuku, apsAyah);
  TSuraAyahProperty = class(TObject)
  protected
    FExpression : String;
    FStyle : TSuraAyahPropertyStyle;
    FSura : TSuraNum;
    FRuku : TRukuNum;
    FAyah : TAyahNum;
  public
    procedure Parse(const AExpr : String;
                    const AQuranStruct : TQuranStructure;
                    const AErrors : TStrings;
                    const AAloneStyle : TSuraAyahAlonePropStyle;
                    const AAloneSura : Integer = -1);

    property Expression : String read FExpression;
    property Style : TSuraAyahPropertyStyle read FStyle;
    property Sura : TSuraNum read FSura;
    property Ruku : TRukuNum read FRuku;
    property Ayah : TAyahNum read FAyah;
  end;

  TSuraAyahNodeStyle = (nsSingle, nsRange);
  TSuraAyahNode = class(TObject)
  protected
    FExpression : String;
    FStyle : TSuraAyahNodeStyle;
    FStart : TSuraAyahProperty;
    FFinish : TSuraAyahProperty;
  public
    procedure Parse(const AExpr : String; const AQuranStruct : TQuranStructure; const AErrors : TStrings);

    property Expression : String read FExpression;
    property Style : TSuraAyahNodeStyle read FStyle;
    property Single : TSuraAyahProperty read FStart;
    property Start : TSuraAyahProperty read FStart;
    property Finish : TSuraAyahProperty read FFinish;
  end;

  TSuraAyahNodeArray = array of TSuraAyahNode;
  TSuraAyahExpr = class(TObject)
  protected
    FExpression : String;
    FErrors : TStringList;
    FNodes : TSuraAyahNodeArray;
    FQuranStruct : TQuranStructure;

    procedure Clear;
    procedure SetExpression(const AExpr : String);
  public
    constructor Create;
    destructor Destroy; override;

    property QuranStruct : TQuranStructure read FQuranStruct write FQuranStruct;
    property Expression : String read FExpression write SetExpression;
    property Errors : TStringList read FErrors;
    property Nodes : TSuraAyahNodeArray read FNodes;
  end;

procedure TSuraAyahProperty.Parse(
            const AExpr : String;
            const AQuranStruct : TQuranStructure;
            const AErrors : TStrings;
            const AAloneStyle : TSuraAyahAlonePropStyle;
            const AAloneSura : Integer = -1);

  procedure AddError(const AMsg : String; AParams : array of const);
  begin
    AErrors.Add(Format(AMsg, AParams));
  end;

  function ParseSuraNum(const ASura : String) : Boolean;
  begin
    try
      FSura := StrToInt(TrimS(ASura));
      if (FSura >= Low(TSuraNum)) and (FSura <= High(TSuraNum)) then
        Result := True
      else begin
        AddError('Invalid Sura Number %s (range is %d to %d)', [ASura, Low(TSuraNum), High(TSuraNum)]);
        Result := False;
      end;
    except
      AddError('Invalid Sura Number %s', [ASura]);
      Result := False;
    end;
  end;

  function ParseAyahNum(const AAyah : String) : Boolean;
  begin
    Result := False;
    try
      FAyah := StrToInt(TrimS(AAyah));
      if (FAyah < 1) or (FAyah > AQuranStruct.Sura[FSura].ayahCount) then
        AddError('Invalid Ayah Number %d (valid range for Sura %d is %d to %d)', [FAyah, FSura, 1, AQuranStruct.Sura[FSura].ayahCount])
      else
        Result := True;
    except
      AddError('Invalid Ayah Number %s', [AAyah]);
    end;
  end;

  function ParseRukuNum(const ARuku : String) : Boolean;
  begin
    Result := False;
    try
      FRuku := StrToInt(TrimS(ARuku));
      if (FRuku < 1) or (FRuku > AQuranStruct.Sura[FSura].rukuCount) then
        AddError('Invalid Ruku Number %d (valid range for Sura %d is %d to %d)', [FRuku, FSura, 1, AQuranStruct.Sura[FSura].rukuCount])
      else
        Result := True;
    except
      AddError('Invalid Ruku Number %s', [ARuku]);
    end;
  end;

const
  SuraRukuSep = ':';
  SuraAyahSep = '.';
var
  Sura, Ruku, Ayah : String;
begin
  FExpression := AExpr;
  if SplitText(FExpression, SuraAyahSep, Sura, Ayah) then begin
    FStyle := psSuraAyah;
    if ParseSuraNum(Sura) then
      ParseAyahNum(Ayah);
  end else if SplitText(FExpression, SuraRukuSep, Sura, Ruku) then begin
    FStyle := psSuraRuku;
    if ParseSuraNum(Sura) then
      ParseRukuNum(Ruku);
  end else begin
    case AAloneStyle of
      apsSura :
        begin
          FStyle := psSura;
          ParseSuraNum(FExpression);
        end;
      apsRuku :
        begin
          Assert((AAloneSura >= Low(TSuraNum)) and (AAloneSura <= High(TSuraNum)));
          FStyle := psSuraRuku;
          FSura := AAloneSura;
          ParseRukuNum(FExpression);
        end;
      apsAyah :
        begin
          Assert((AAloneSura >= Low(TSuraNum)) and (AAloneSura <= High(TSuraNum)));
          FStyle := psSuraAyah;
          FSura := AAloneSura;
          ParseAyahNum(FExpression);
        end;
    end;
  end;
end;

procedure TSuraAyahNode.Parse(const AExpr : String; const AQuranStruct : TQuranStructure; const AErrors : TStrings);
const
  RangeSep = '-';
var
  Start, Finish : String;
  AloneStyle : TSuraAyahAlonePropStyle;
begin
  FExpression := AExpr;
  if SplitText(FExpression, RangeSep, Start, Finish) then begin
    FStyle := nsRange;
    FStart := TSuraAyahProperty.Create;
    FStart.Parse(TrimS(Start), AQuranStruct, AErrors, apsSura);
    case FStart.Style of
      psSura : AloneStyle := apsSura;
      psSuraRuku : AloneStyle := apsRuku;
      psSuraAyah : AloneStyle := apsAyah;
    else
      AloneStyle := apsSura;
    end;
    FFinish := TSuraAyahProperty.Create;
    FFinish.Parse(TrimS(Finish), AQuranStruct, AErrors, AloneStyle, FStart.Sura);

    if (FStart.Style <> FFinish.Style) then
      AErrors.Add(Format('%s is a different style than %s in "%s" (ranges must be of same type such as Sura, Sura.Ayah, or Sura:Ruku)', [Start, Finish, FExpression]));
    if FStart.Sura > FFinish.Sura then
      AErrors.Add(Format('Sura %d does not come before Sura %d in "%s"', [FStart.Sura, FFinish.Sura, FExpression]));
  end else begin
    FStyle := nsSingle;
    FStart := TSuraAyahProperty.Create;
    FStart.Parse(FExpression, AQuranStruct, AErrors, apsSura);
  end;
end;

constructor TSuraAyahExpr.Create;
begin
  inherited Create;
  FErrors := TStringList.Create;
end;

destructor TSuraAyahExpr.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TSuraAyahExpr.Clear;
var
  I : Integer;
begin
  if Length(FNodes) > 0 then begin
    for I := 0 to High(FNodes) do
      if FNodes[I] <> Nil then FNodes[I].Free;
  end;
  SetLength(FNodes, 0);
  FErrors.Clear;
end;

procedure TSuraAyahExpr.SetExpression(const AExpr : String);
const
  ExprSep = ',';
var
  I, Total : Integer;
  NodeStr : String;
begin
  Clear;
  FExpression := AExpr;
  if FExpression = '' then
    Exit;

  Total := WordCountS(FExpression, ExprSep);
  SetLength(FNodes, Total);
  if Total > 0 then begin
    for I := 1 to Total do begin
      NodeStr := TrimS(ExtractWordS(I, FExpression, ExprSep));
      if NodeStr = '' then
        continue;

      FNodes[I-1] := TSuraAyahNode.Create;
      FNodes[I-1].Parse(NodeStr, FQuranStruct, FErrors);
    end;
  end;
end;

function TMarkedAyahs.Mark(const AExpression : String) : Boolean;
var
  Expr : TSuraAyahExpr;
  Node : TSuraAyahNode;
  SurasCount, I, S, R : Integer;
begin
  Expr := TSuraAyahExpr.Create;
  Expr.QuranStruct := FQuranStruct;
  Expr.Expression := AExpression;

  Result := False;
  if Expr.Errors.Count > 0 then begin
    ShowMessage(Format('There are errors in the specification: '#13'%s'#13#13'%s', [AExpression, Expr.Errors.Text]));
    Expr.Free;
    Exit;
  end;

  for I := 0 to Length(Expr.Nodes)-1 do begin
    Node := Expr.Nodes[I];

    case Node.Style of
      nsSingle :
        begin
          case Node.Single.Style of
            psSura : Mark(Node.Single.Sura);
            psSuraRuku :
              with FQuranStruct.Sura[Node.Single.Sura] do
                Mark(Node.Single.Sura, Rukus[Node.Single.Ruku].startAyah, Rukus[Node.Single.Ruku].endAyah);
            psSuraAyah : Mark(Node.Single.Sura, Node.Single.Ayah);
          end;
        end;
      nsRange :
        begin
          case Node.Start.Style of
            psSura :
              for S := Node.Start.Sura to Node.Finish.Sura do
                Mark(S);
            psSuraRuku :
              begin
                SurasCount := (Node.Finish.Sura - Node.Start.Sura) + 1;
                if SurasCount = 1 then begin
                  for R := Node.Start.Ruku to Node.Finish.Ruku do
                    with FQuranStruct.Sura[Node.Start.Sura] do
                      Mark(Node.Start.Sura, Rukus[R].startAyah, Rukus[R].endAyah);
                end else begin
                  with FQuranStruct.Sura[Node.Start.Sura] do
                    for R := Node.Start.Ruku to rukuCount do
                      Mark(Node.Start.Sura, Rukus[R].startAyah, Rukus[R].endAyah);
                  if SurasCount > 2 then
                    for S := Node.Start.Sura+1 to Node.Finish.Sura-1 do
                      Mark(S);
                  with FQuranStruct.Sura[Node.Finish.Sura] do
                    for R := 1 to Node.Finish.Ruku do
                      Mark(Node.Finish.Sura, Rukus[R].startAyah, Rukus[R].endAyah);
                end;
              end;
            psSuraAyah :
              Mark(Node.Start.Sura, Node.Start.Ayah, Node.Finish.Sura, Node.Finish.Ayah);
          end;
        end;
    end;
  end;

  Result := True;
  Expr.Free;
end;

procedure TMarkedAyahs.SetQuranStruct(const AStruct : TQuranStructure);
var
  I : Integer;
begin
  FQuranStruct := AStruct;
  for I := Low(TSuraNum) to High(TSuraNum) do begin
    if FSuras[I] <> Nil then
      FSuras[I].Free;
    FSuras[I] := TStBits.Create(AStruct.Sura[I].ayahCount-1);
  end;
end;

//------------------------------------------------------------------------------

function SuraAyah(const ASura : TSuraNum; const AAyah : TAyahNum) : TSuraAyah;
begin
  Result.suraNum := ASura;
  Result.ayahNum := AAyah;
end;

end.
