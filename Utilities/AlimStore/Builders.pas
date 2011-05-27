unit Builders;

interface

uses Classes, CompDoc, QuranStruct, XmlObjModel, StDict, IndexStruct, IslUtils, StStrL;

const
  GenericAyahId = 'QA:';
  DefaultEncryptKey = 'aml';
  DontEncryptKey = '';

type
  TStatusEventMode = (setStart, setProgress, setEnd);
  TStoreStatusEvent = function(const AMode : TStatusEventMode; const AMsg : String; const AProgress, ATotal : LongInt) : Boolean of object;
    {-if status event returns false, processing is stopped}

  TAlimStorageBuilder = class(TObject)
  protected
    FOnStatus : TStoreStatusEvent; { status event processor (required) }
    FDocument : TXmlDocument;      { the complete document parsed from file }
    FRootTag : TXmlElement;        { the root tag (first tag under AML that is not viewer) }
    FCatalog : TXmlElement;        { catalog tag, if any }
    FCatalogNames : TXmlElement;   { the names tag of the catalog }
    FCatalogContent : TXmlElement; { the content tag of the catalog }
    FCatalogIndexes : TXmlElement; { the indexes tag of the catalog (created if not found) }
    FCatalogBuild : TXmlElement;   { the build tag of the catalog (created if not found) }
    FCatalogLanguage : TXmlElement;{ the lanugage tag of the catalog }
    FCatalogStruct : TXmlElement;  { the structure tag of the catalog }
    FCatalogNavg : TXmlElement;    { the navigation tag of the catalog }
    FCatalogDisplay : TXmlElement; { the display tag of the catalog }
    FStructHome : TXmlElement;     { the home tag of the structure tag }
    FContentId : String;           { the "id" attribute of the catalog tag }
    FContentType : String;         { the value of <content type="xxx"> }
    FContentSubType : String;      { the value of <content subtype="xxx"> }
    FSubStoreName : String;         { the substorage name, if any }
    FErrors : TStringList;         { list of errors/warnings }
    FCreateSubStorage : Boolean;   { should a substorage be created? }
    FBytesStored : LongInt;        { total bytes stored by StoreDocument }
    FCanceled : Boolean;           { did the status event return false? }
    FScanOnly : Boolean;           { will this object store the data or scan only (TIndexStorageBuilder scans only)? }
    FInverseSubjects : TPageIndex; { the subjects found in specific locations }
    FSubjects : TPageIndex;        { the subject index }
    FGenericAyahInSubjIdx : Boolean; { was any 'qa:x.y' address indexed in the subjects index? }
    FGenericAyahInHrefIdx : Boolean; { wan any 'qa:x.y' address indexed in the hrefs index? }
    FHRefs : TPageIndex;           { any <a href="x"> references }
    FWords : TPageIndex;           { the words index }
    FTrackWords : Boolean;         { are we creating a full-text index? }
    FStopWords : TStringList;      { list of words not to put into full-text index }
    FRtlWords : Boolean;           { are words RTL (right to left, Arabic)? }
    FQuranStruct : TQuranStructure;

    procedure AddIndexElems(const AElem : TXMLElement; const AAddress : String);
      {-add any index elements from AElem into an index }
    procedure AddWords(const AWords : String; AAddress : String);
      {-add any index elements from AElem into FWords index }
    function FindCatalog : Boolean; virtual;
      {-return the catalog for this document}
    function FindCatalogElems : Boolean; virtual;
      {-return the catalog elements for this document}
    function HasElement(const AParent : TXMLElement;
                        const AElemName : String;
                        var Child : TXmlElement;
                        const ACreate : Boolean;
                        const AMsgFmt : String = '') : Boolean;
      {-return true if AParent has Child AElemName, add to Errors if AMsgFmt is not ''}
    function HasAttributes(const AElem : TXMLElement;
                           const Attrs : array of String;
                           const AMsgFmt : String = '') : Boolean;
      {-return true if all attributes in Attrs are found in AElem, add to Errors if AMsgFmt is not ''}
    procedure StoreCatalog(AStorage : TStorage); virtual;
      {-store the catalog in the provided Storage object as 'Catalog'}
    procedure StoreSubjectsIndex(AStorage : TStorage); virtual;
      {-store the subject index as 'SubjectsIndex'}
    procedure StoreWordsIndex(AStorage : TStorage); virtual;
      {-store the words index as 'SubjectsIndex'}
  public
    constructor Create;
    destructor Destroy; override;

    function CanHandleDocument(ADocument : TXMLDocument) : Boolean; virtual;
      {-return True if ADocument is something this object can handle (also fills in the FProperties object) }
    procedure StoreDocument(AStorage : TStorage; ADocument : TXMLDocument); virtual; abstract;
      {-store the document in the provided Storage object}

    property CreateSubStorage : Boolean read FCreateSubStorage write FCreateSubStorage;
    property ScanDataOnly : Boolean read FScanOnly;
    property Errors : TStringList read FErrors;
    property RootTag : TXmlElement read FRootTag;
    property BytesStored : LongInt read FBytesStored;
    property StatusCancel : Boolean read FCanceled;
    property InverseSubjects : TPageIndex read FInverseSubjects;
    property SubjectsIndex : TPageIndex read FSubjects;
    property HRefsIndex : TPageIndex read FHRefs;
    property WordsIndex : TPageIndex read FWords;
    property QuranStruct : TQuranStructure read FQuranStruct write FQuranStruct;

    property OnStatus : TStoreStatusEvent read FOnStatus write FOnStatus;
  end;

  TQuranType = (qtText, qtImages, qtThemes);
  TQuranStorageBuilder = class(TAlimStorageBuilder)
  protected
    FType : TQuranType;       { valid only between CanHandleDocument and StoreDocument }
    FSuras : TXmlNodeList;    { all of the suras in the root tag }

    procedure StoreQuranThemes(AStorage : TStorage; ADocument : TXMLDocument);
      {-store the quran themes in ADocument into the provided AStorage object}
    procedure StoreQuranText(AStorage : TStorage; ADocument : TXMLDocument); virtual;
      {-store the quran text given in ADocument in the provided AStorage object}
    procedure StoreQuranImagesDefn(AStorage : TStorage; ADocument : TXMLDocument); virtual;
      {-store the quran image definitions in the provided AStorage object}
  public
    function CanHandleDocument(ADocument : TXMLDocument) : Boolean; override;
      {-return True if ADocument is a Quran file (also fills in the FProperties object) }
    procedure StoreDocument(AStorage : TStorage; ADocument : TXMLDocument); override;
      {-store the document in the provided Storage object}
  end;

  TIndexType = (itSubject);
  TIndexStorageBuilder = class(TAlimStorageBuilder)
  protected
    FType : TIndexType;       { valid only between CanHandleDocument and StoreDocument }

    procedure StoreSubjectIndex(AStorage : TStorage; ADocument : TXMLDocument); virtual;
      {-store the subject in ADocument in the provided AStorage object}
  public
    function CanHandleDocument(ADocument : TXMLDocument) : Boolean; override;
      {-return True if ADocument is a index file (also fills in the FProperties object) }
    procedure StoreDocument(AStorage : TStorage; ADocument : TXMLDocument); override;
      {-store the document in the provided Storage object}
  end;

  THadithType = (htHadith, htFiqh);
  THadithFiqhStorageBuilder = class(TAlimStorageBuilder)
  protected
    FNarrators : TPageIndex;
    FType : THadithType;

    procedure StoreNarrators(AStorage : TStorage); virtual;
      {-store the narrator index in AStorage as 'NarratorsIndex'}
  public
    function CanHandleDocument(ADocument : TXMLDocument) : Boolean; override;
      {-return True if ADocument is a hadith or fiqh file (also fills in the FProperties object) }
    procedure StoreDocument(AStorage : TStorage; ADocument : TXMLDocument); override;
      {-store the document in the provided Storage object}
  end;

  TArticlesStorageBuilder = class(TAlimStorageBuilder)
  protected
    FStructPage : TXmlElement;
    FStructItem : TXmlElement;
    FPageTag : String;
    FPageAttr : String;
    FItemTag : String;
    FItemAttr : String;
  public
    function CanHandleDocument(ADocument : TXMLDocument) : Boolean; override;
      {-return True if ADocument is a hadith or fiqh file (also fills in the FProperties object) }
    procedure StoreDocument(AStorage : TStorage; ADocument : TXMLDocument); override;
      {-store the document in the provided Storage object}
  end;

implementation

uses SysUtils, StStrS, Main, Dialogs;

const
  CompressStrings = False;
  TagNameCatalog = 'catalog';
  TagNameIndexes = 'indexes';
  TagNameIndex = 'index';
  TagNameBuild = 'build';
  TagNameOption = 'option';
  TagNameLanguage = 'language';
  TagNameStructure = 'structure';
  TagNameHome = 'home';
  TagNameNames = 'names';
  TagNameContent = 'content';
  TagNameDisplay = 'display';
  TagAttrType = 'type';
  TagAttrSubType = 'subtype';

  TagAttrId = 'id';
  TagAttrFullName = 'full';
  TagAttrShortName = 'short';
  TagAttrTabName = 'tab';

  LangIdEnglish = 'english';
  LangIdArabic = 'arabic';

  SubtypeSuras = 'suras';

  StrmNameCatalog = 'Catalog';

procedure DisposeObject(Data : Pointer); far;
begin
  if Data <> Nil then
    TObject(Data).Free;
end;

procedure WriteStringToStream(S : TStream; const AStr : String; const AEncryptKey : String = DefaultEncryptKey);
var
  Encrypted : String;
begin
  if AEncryptKey <> '' then begin
    Encrypted := ScrambleL(AStr, AEncryptKey);
    S.Write(PChar(Encrypted)^, Length(Encrypted));
  end else
    S.Write(PChar(AStr)^, Length(AStr));
end;

function CopyNodeEmpty(const ADoc : TXmlDocument; const ANode : TXmlElement) : TXmlElement;
var
  I : Integer;
  Attr : TXmlAttribute;
begin
  Result := ADoc.CreateElement(ANode.NodeName);
  if ANode.Attributes.Length > 0 then
    for I := 0 to ANode.Attributes.Length-1 do begin
      Attr := ANode.Attributes.Item(I) as TXmlAttribute;
      Result.SetAttribute(Attr.Name, Attr.Value);
    end;
end;

function GetAttrOrSetDefault(const AElem : TXmlElement; AAttrName, ADefault : String) : String;
begin
  Result := AElem.GetAttribute(AAttrName);
  if Result = '' then begin
    Result := ADefault;
    AElem.SetAttribute(AAttrName, Result);
  end;
end;

{------------------------------------------------------------------------------}

constructor TAlimStorageBuilder.Create;
begin
  inherited Create;
  FErrors := TStringList.Create;
end;

destructor TAlimStorageBuilder.Destroy;
begin
  FErrors.Free;
  if FSubjects <> Nil then
    FSubjects.Free;
  if FInverseSubjects <> Nil then
    FInverseSubjects.Free;
  if FHRefs <> Nil then
    FHRefs.Free;
  if FWords <> Nil then
    FWords.Free;
  if FStopWords <> Nil then
    FStopWords.Free;
  inherited Destroy;
end;

procedure TAlimStorageBuilder.AddWords(const AWords : String; AAddress : String);
const
  WordDelims = ' .,;!?:/''"<>|\[]{}~!@#$%^&*()-=+`'#10#13#9'‘’“”';
var
  P, I, SLen, Count, WordIdx : Integer;
  WordS : ShortString;
  WordLen : Byte absolute WordS;
begin
  if not FTrackWords then
    Exit;

  Count := 0;
  I := 1;
  SLen := Length(AWords);
  WordIdx := 0;

  while (I <= SLen) do begin
    {skip over delimiters}
    while (I <= SLen) and (Pos(AWords[I], WordDelims) > 0) do
      Inc(I);

    {if we're not beyond end of S, we're at the start of a word}
    if I <= SLen then
      Inc(Count);

    {if not finished, find the end of the current word}
    WordS := '';
    while (I <= SLen) and (Pos(AWords[I], WordDelims) = 0) do begin
      WordS := WordS + AWords[I];
      Inc(I);
    end;

    Inc(WordIdx);
    // single character words are not stored
    // words in stop list are not stored
    // words that start with a number are not stored
    if (WordLen > 0) and not (WordS[1] in ['0'..'9']) then begin
      if FRtlWords then WordS := Reverse(WordS);
      if (WordLen > 1) and (FStopWords.IndexOf(WordS) = -1) then
        FWords.AddEntry(WordS, AAddress {+ '/' + IntToStr(WordIdx)});
    end;
  end;
end;

procedure TAlimStorageBuilder.AddIndexElems(const AElem : TXMLElement; const AAddress : String);
var
  IndexElems : TXmlNodeList;
  IndexElem : TXmlElement;
  HRefElems : TXmlNodeList;
  HRefElem : TXmlElement;
  E : Integer;
  HRef, Topic, Subtopic : String;
begin
  Assert(AElem <> Nil, 'Null Element sent to AddIndexElem');

  IndexElems := AElem.GetElementsByTagName('index');
  if (IndexElems <> Nil) and (IndexElems.Length > 0) then begin
    if (not FGenericAyahInSubjIdx) and (CompareText(Copy(AAddress, 1, Length(GenericAyahId)), GenericAyahId) = 0) then
      FGenericAyahInSubjIdx := True;

    for E := 0 to IndexElems.Length-1 do begin
      IndexElem := IndexElems.Item(E) as TXmlElement;
      Topic := IndexElem.GetAttribute('topic');
      Subtopic := IndexElem.GetAttribute('subtopic');
      if (Topic <> '') and (Subtopic <> '') then begin
        FSubjects.AddSubEntry(Topic, Subtopic, AAddress);
        FInverseSubjects.AddEntry(AAddress, Topic + '/' + Subtopic);
      end else if (Topic <> '') then begin
        FSubjects.AddEntry(Topic, AAddress);
        FInverseSubjects.AddEntry(AAddress, Topic);
      end;
    end;
  end;

  HRefElems := AElem.GetElementsByTagName('a');
  if (HRefElems <> Nil) and (HRefElems.Length > 0) then begin
    if (not FGenericAyahInHrefIdx) and (CompareText(Copy(AAddress, 1, Length(GenericAyahId)), GenericAyahId) = 0) then
      FGenericAyahInHrefIdx := True;

    for E := 0 to HRefElems.Length-1 do begin
      HRefElem := HRefElems.Item(E) as TXmlElement;
      HRef := HRefElem.GetAttribute('href');
      if HRef <> '' then
        FHrefs.AddEntry(HRef, AAddress)
      else
        FErrors.Add('empty href in <a> in '+AAddress);
    end;
  end;
end;

function TAlimStorageBuilder.FindCatalog : Boolean;
begin
  Result := False;

  if not HasElement(FRootTag, TagNameCatalog, FCatalog, False, '%1:s is missing %0:s tag') then
    Exit;
  if not HasAttributes(FCatalog, [TagAttrId], 'Catalog tag missing required attribute "%s"') then
    Exit;
  FContentId := FCatalog.GetAttribute(TagAttrId);

  Result := True;
end;

function TAlimStorageBuilder.FindCatalogElems : Boolean;
begin
  Result := False;

  if not HasElement(FCatalog, TagNameNames, FCatalogNames, False, '%1:s is missing %0:s tag') then
    Exit;
  if not HasAttributes(FCatalogNames, [TagAttrFullName], 'Catalog <names> tag missing required attribute "%s"') then
    Exit;
  if FCatalogNames.GetAttribute(TagAttrShortName) = '' then
    FCatalogNames.SetAttribute(TagAttrShortName, FCatalogNames.GetAttribute(TagAttrFullName));
  if FCatalogNames.GetAttribute(TagAttrTabName) = '' then
    FCatalogNames.SetAttribute(TagAttrTabName, FCatalogNames.GetAttribute(TagAttrShortName));

  if not HasElement(FCatalog, TagNameContent, FCatalogContent, False, '%1:s is missing %0:s tag') then
    Exit;
  if not HasAttributes(FCatalogContent, [TagAttrType], 'Content tag missing required attribute "%s"') then
    Exit;
  FContentType := FCatalogContent.GetAttribute(TagAttrType);
  FContentSubType := FCatalogContent.GetAttribute(TagAttrSubType);

  if not HasElement(FCatalog, TagNameLanguage, FCatalogLanguage, True) then
    Exit;
  if FCatalogLanguage.GetAttribute(TagAttrId) = '' then
    FCatalogLanguage.SetAttribute(TagAttrId, LangIdEnglish);

  if not HasElement(FCatalog, TagNameIndexes, FCatalogIndexes, True) then
    Exit;

  if not HasElement(FCatalog, TagNameBuild, FCatalogBuild, True) then
    Exit;

  if not HasElement(FCatalogContent, TagNameStructure, FCatalogStruct, True) then
    Exit;
  if not HasElement(FCatalogContent, TagNameHome, FStructHome, True) then
    Exit;
  if not HasElement(FCatalog, TagNameDisplay, FCatalogDisplay, True) then
    Exit;
  Result := True;
end;

function TAlimStorageBuilder.HasElement(const AParent : TXMLElement;
                                        const AElemName : String;
                                        var Child : TXmlElement;
                                        const ACreate : Boolean;
                                        const AMsgFmt : String = '') : Boolean;
begin
  Child := AParent.FindElement(AElemName);
  if Child = Nil then begin
    if ACreate then begin
      Result := True;
      Child := FDocument.CreateElement(AElemName);
      AParent.AppendChild(Child);
    end else begin
      Result := False;
      if AMsgFmt <> '' then
        FErrors.Add(Format(AMsgFmt, [AElemName, AParent.NodeName]));
    end;
  end else
    Result := True;
end;

function TAlimStorageBuilder.HasAttributes(const AElem : TXMLElement;
                                           const Attrs : array of String;
                                           const AMsgFmt : String = '') : Boolean;
var
  A : Integer;
  Missing : Integer;
begin
  Missing := 0;
  for A := Low(Attrs) to High(Attrs) do begin
    if AElem.GetAttribute(Attrs[A]) = '' then begin
      Inc(Missing);
      if AMsgFmt <> '' then
        FErrors.Add(Format(AMsgFmt, [Attrs[A]]));
    end;
  end;

  Result := Missing = 0;
end;

function TAlimStorageBuilder.CanHandleDocument(ADocument : TXMLDocument) : Boolean;
const
  StopWords : String = 'am,is,are,was,were,be,being,been,the,and,of,to';
var
  MatchList : TXmlNodeList;
  ChildNode : TXmlNode;
  DocElem, ShortcutElem : TXmlElement;
  SW, StopWordsCount, I : Integer;
begin
  FDocument := Nil;
  FRootTag := Nil;
  Result := False;

  if not ADocument.HasChildNodes then
    Exit;

  FDocument := ADocument;
  DocElem := ADocument.DocumentElement;
  if (DocElem = Nil) or (DocElem.NodeName <> 'aml') or (not DocElem.HasChildNodes) then
    Exit;

  FCatalog := Nil;
  FErrors.Clear;
  FBytesStored := 0;
  FGenericAyahInSubjIdx := False;
  FGenericAyahInHrefIdx := False;
  if FSubjects <> Nil then
    FSubjects.Free;
  if FHRefs <> Nil then
    FHrefs.Free;
  if FInverseSubjects <> Nil then
    FInverseSubjects.Free;
  if FWords <> Nil then
    FWords.Free;
  if FStopWords <> Nil then
    FStopWords.Free;
  FCanceled := False;

  for I := 0 to DocElem.ChildNodes.Length-1 do begin
    ChildNode := DocElem.ChildNodes.Item(I) as TXmlNode;
    if ChildNode.NodeType = ELEMENT_NODE then begin
      if FRootTag = Nil then
        FRootTag := ChildNode as TXmlElement
      else begin
        FErrors.Add(Format('Only expected a single root tag under the AML tag (%s will not be processed)', [ChildNode.NodeName]));
      end;
    end;
  end;

  if FRootTag = Nil then begin
    FErrors.Add('Root tag not found');
    Exit;
  end;

  if not FindCatalog then begin
    FErrors.Add('Catalog tag not found.');
    Exit;
  end;

  if not FindCatalogElems then begin
    FErrors.Add('Catalog elements not initialized.');
    Exit;
  end;

  FSubjects := TPageIndex.Create;
  FInverseSubjects := TPageIndex.Create;
  FHRefs := TPageIndex.Create;
  FWords := TPageIndex.Create;

  FStopWords := TStringList.Create;
  FStopWords.Sorted := True;
  StopWordsCount := WordCountS(StopWords, ',');
  for SW := 1 to StopWordsCount do
    FStopWords.Add(ExtractWordS(SW, StopWords, ','));

  FRtlWords := False;
  FSubStoreName := '$' + FCatalogNames.GetAttribute('short');

  MatchList := FCatalogBuild.GetElementsByTagNameWithAttribute(TagNameOption, 'fulltextindex', 'no');
  FTrackWords := MatchList.Length = 0;

  Result := True;
end;

procedure TAlimStorageBuilder.StoreCatalog(AStorage : TStorage);
var
  DataStream : TStorageStream;
  SCList : TXmlNodeList;
  Shortcut : TXmlElement;
  I : Integer;
begin
  SCList := FCatalog.GetElementsByTagName('shortcut');
  if SCList.Length = 0 then begin
    FErrors.Add('warning: no shortcuts found');
  end else begin
    for I := 0 to SCList.Length-1 do begin
      Shortcut := SCList.Item(I) as TXmlElement;
      if (Shortcut.GetAttribute('caption') = '') and (Shortcut.GetAttribute('href') = '') then begin
        Shortcut.SetAttribute('caption', FCatalogNames.GetAttribute(TagAttrFullName));
        Shortcut.SetAttribute('href', FContentId+':');
      end else if (Shortcut.GetAttribute('caption') = '') and (Shortcut.GetAttribute('href') <> '') then begin
        Shortcut.SetAttribute('caption', Shortcut.GetAttribute('href'));
      end else if (Shortcut.GetAttribute('caption') <> '') and (Shortcut.GetAttribute('href') = '') then begin
        Shortcut.SetAttribute('href', Shortcut.GetAttribute('caption'));
      end;
    end;
  end;

  FOnStatus(setStart, 'Storing Catalog', 0, 0);
  DataStream := TStorageStream.Create(StrmNameCatalog, AStorage, amReadWrite, True);
  WriteStringToStream(DataStream, FCatalog.XmlDocument);
  Inc(FBytesStored, DataStream.Size);
  DataStream.Free;
  FOnStatus(setEnd, '', 0, 0);
end;

procedure TAlimStorageBuilder.StoreSubjectsIndex(AStorage : TStorage);
const
  SubjectIndexName = 'SubjectsIndex';
  HRefsIndexName = 'HRefsIndex';
  InvSubjectIndexName = 'InverseSubjectsIndex';
var
  Storage : TStorage;
  IndexElem : TXmlElement;
begin
  if FSubjects.Entries.Count > 0 then begin
    IndexElem := FDocument.CreateElement('subjects');
    IndexElem.SetAttribute('storage', SubjectIndexName);
    if FGenericAyahInSubjIdx then
      IndexElem.SetAttribute('hasgenericayah', 'true');
    FCatalogIndexes.AppendChild(IndexElem);

    FOnStatus(setStart, 'Storing Subjects', 0, 0);
    Storage := TStorage.Create(SubjectIndexName, AStorage, amReadWrite, tmDirect, True);
    FSubjects.Store(Storage);
    Inc(FBytesStored, FSubjects.StoreSize);
    Storage.Free;
    FOnStatus(setEnd, '', 0, 0);
  end;

  if FInverseSubjects.Entries.Count > 0 then begin
    IndexElem := FDocument.CreateElement('inversesubjects');
    IndexElem.SetAttribute('storage', InvSubjectIndexName);
    FCatalogIndexes.AppendChild(IndexElem);

    FOnStatus(setStart, 'Storing Inverse Subjects', 0, 0);
    Storage := TStorage.Create(InvSubjectIndexName, AStorage, amReadWrite, tmDirect, True);
    FInverseSubjects.StorePacked(Storage, ldsEntryLocs);
    Inc(FBytesStored, FInverseSubjects.StoreSize);
    Storage.Free;
    FOnStatus(setEnd, '', 0, 0);
  end;

  if FHRefs.Entries.Count > 0 then begin
    IndexElem := FDocument.CreateElement('hrefs');
    IndexElem.SetAttribute('storage', HRefsIndexName);
    if FGenericAyahInHrefIdx then
      IndexElem.SetAttribute('hasgenericayah', 'true');
    FCatalogIndexes.AppendChild(IndexElem);

    FOnStatus(setStart, 'Storing HRefs Index', 0, 0);
    Storage := TStorage.Create(HRefsIndexName, AStorage, amReadWrite, tmDirect, True);
    FHRefs.StorePacked(Storage, ldsEntryLocs);
    Inc(FBytesStored, FHRefs.StoreSize);
    Storage.Free;
    FOnStatus(setEnd, '', 0, 0);
  end;
end;

procedure TAlimStorageBuilder.StoreWordsIndex(AStorage : TStorage);
const
  WordsIndexName = 'WordsIndex';
  StopWordsName = 'StopWords';
var
  IndexElem : TXmlElement;
  Storage : TStorage;
  DataStream : TStorageStream;
  I, StemIdx : Integer;
  WordS, Stem : String;
begin
  if FWords.Entries.Count <= 0 then
    Exit;

  IndexElem := FDocument.CreateElement('fulltext');
  IndexElem.SetAttribute('storage', WordsIndexName);
  FCatalogIndexes.AppendChild(IndexElem);

  FOnStatus(setStart, 'Storing Words', 0, 0);
  Storage := TStorage.Create(WordsIndexName, AStorage, amReadWrite, tmDirect, True);
  FWords.StorePacked(Storage, ldsEntryLocs);
  Inc(FBytesStored, FWords.StoreSize);

  DataStream := TStorageStream.Create(StopWordsName, Storage, amReadWrite, True);
  FStopWords.SaveToStream(DataStream);
  DataStream.Free;
  Storage.Free;

  FOnStatus(setEnd, '', 0, 0);
end;

{------------------------------------------------------------------------------}

function TQuranStorageBuilder.CanHandleDocument(ADocument : TXMLDocument) : Boolean;
const
  ContentTypes : array[TQuranType] of String =
    ('text', 'images', 'themes');
var
  IsAML : Boolean;
begin
  IsAML := inherited CanHandleDocument(ADocument);
  Result := IsAML and (FRootTag.NodeName = 'quran');
  if not Result then
    Exit;

  if CompareText(FContentType, 'quran') <> 0 then begin
    FErrors.Add('content type must be quran');
    Result := False;
    Exit;
  end;

  case FindString(FContentSubType, ['text', 'images', 'themes']) of
   -1 : {not found} ;
    0 : begin
          FType := qtText;
          FRtlWords := FCatalogLanguage.GetAttribute(TagAttrId) = 'arabic';
        end;
    1 : begin
          FType := qtImages;
          if not HasAttributes(FCatalogContent, ['imgfmt'], 'Content tag missing required attribute "%s"') then
            Result := False;
        end;
    2 : FType := qtThemes;
  else
    FErrors.Add('Unkown Quran Type "' + FContentSubType + '"');
    Result := False;
  end;
end;

procedure TQuranStorageBuilder.StoreDocument(AStorage : TStorage; ADocument : TXMLDocument);
var
  DocStorage : TStorage;
begin
  if FCreateSubStorage then
    DocStorage := TStorage.Create(FSubStoreName, AStorage, amReadWrite, tmDirect, True)
  else
    DocStorage := AStorage;
  FSuras := FRootTag.GetChildElementsByTagName('sura');
  case FType of
    qtText         : StoreQuranText(DocStorage, ADocument);
    qtThemes       : StoreQuranThemes(DocStorage, ADocument);
    qtImages       : StoreQuranImagesDefn(DocStorage, ADocument);
  end;

  StoreSubjectsIndex(DocStorage);
  StoreWordsIndex(DocStorage);
  StoreCatalog(DocStorage);
  if FCreateSubStorage then
    DocStorage.Free;
end;

procedure TQuranStorageBuilder.StoreQuranText(AStorage : TStorage; ADocument : TXMLDocument);
type
  TSuraMapData =
    record
      SuraNum : Word;
      AyahCount : Word;
      AyahMapOffs : LongInt;
    end;
  TAyahMapData =
    record
      AyahNum : Word;
      AyahTextStart : LongInt;
      AyahTextLen : Word;
    end;
  TAyahMap = array[TAyahNum] of TAyahMapData;
  TAyahMaps = array[TSuraNum] of TAyahMap;
  TSuraMap = array[TSuraNum] of TSuraMapData;
var
  DataStream : TStorageStream;
  SuraStorageName, AyahXML, SuraText : String;
  SuraElem, AyahElem : TXmlElement;
  S, A, ThisSuraNum, ThisAyahNum, LastSuraNum, LastAyahNum : Cardinal;
  AyahsMaps : TAyahMaps;
  SuraMap : TSuraMap;
  MarkedAyahs : TMarkedAyahs;
  AyahBit : LongInt;
begin
  LastSuraNum := 0;
  FillChar(AyahsMaps, SizeOf(AyahsMaps), 0);
  FillChar(SuraMap, SizeOf(SuraMap), 0);

  FOnStatus(setStart, 'Storing Text', 0, FSuras.Length-1);
  MarkedAyahs := TMarkedAyahs.Create;
  MarkedAyahs.QuranStruct := FQuranStruct;
  MarkedAyahs.Duplicates := mdPrevent;

  for S := 0 to FSuras.Length-1 do begin
    SuraElem := (FSuras.Item(S) as TXmlElement);

    try
      ThisSuraNum := StrToInt(SuraElem.GetAttribute('num'));
    except
      FErrors.Add(Format('Invalid sura number %s in element %d', [SuraElem.GetAttribute('num'), S]));
      continue;
    end;
    if ThisSuraNum <> LastSuraNum+1 then begin
      FErrors.Add(Format('Sura number sequence error - found %d when %d expected', [ThisSuraNum, LastSuraNum+1]));
      continue;
    end;
    SuraStorageName := Format('%d', [ThisSuraNum-1]);

    LastAyahNum := 0;
    SuraText := Format('<sura num="%d">', [ThisSuraNum]);
    for A := 0 to SuraElem.ChildNodes.Length-1 do begin
      if SuraElem.ChildNodes.Item(A).NodeType <> ELEMENT_NODE then
        continue;

      AyahElem := (SuraElem.ChildNodes.Item(A) as TXmlElement);
      if AyahElem.NodeName <> 'ayah' then begin
        FErrors.Add(Format('Invalid tag "%s" found where ayah expected', [AyahElem.NodeName]));
        continue;
      end;

      try
        ThisAyahNum := StrToInt(AyahElem.GetAttribute('num'));
      except
        FErrors.Add(Format('Invalid ayah number %s in element %d.%d', [AyahElem.GetAttribute('num'), S, A]));
        continue;
      end;
      if ThisAyahNum <> LastAyahNum+1 then begin
        FErrors.Add(Format('Ayah number sequence error - found %d when %d expected in sura %d', [ThisAyahNum, LastAyahNum+1, ThisSuraNum]));
        continue;
      end;

      try
        MarkedAyahs.Mark(ThisSuraNum, ThisAyahNum);
      except
        FErrors.Add(Format('Duplicate: Sura %d Ayah %d in element %d.%d', [ThisSuraNum, ThisAyahNum, S, A]));
      end;

      if DefaultEncryptKey <> '' then
        AyahXML := ScrambleL(AyahElem.XMLDocument, DefaultEncryptKey)
      else
        AyahXML := AyahElem.XMLDocument;
      AyahsMaps[ThisSuraNum][ThisAyahNum].AyahNum := ThisAyahNum;
      AyahsMaps[ThisSuraNum][ThisAyahNum].AyahTextStart := Length(SuraText);
      AyahsMaps[ThisSuraNum][ThisAyahNum].AyahTextLen := Length(AyahXML);
      SuraText := SuraText + AyahXML;

      AddIndexElems(AyahElem, Format(GenericAyahId+'%d.%d', [ThisSuraNum, ThisAyahNum]));
      AddWords(AyahElem.Text, Format('%d.%d', [ThisSuraNum, ThisAyahNum]));
      LastAyahNum := ThisAyahNum;
    end;
    SuraText := SuraText + '</sura>';
    SuraMap[ThisSuraNum].SuraNum := ThisSuraNum;
    SuraMap[ThisSuraNum].AyahCount := LastAyahNum;

    DataStream := TStorageStream.Create(SuraStorageName, AStorage, amReadWrite, True);
    WriteStringToStream(DataStream, SuraText, DontEncryptKey);
    Inc(FBytesStored, DataStream.Size);
    DataStream.Free;

    LastSuraNum := ThisSuraNum;
    if not FOnStatus(setProgress, '', S, 0) then begin
      FCanceled := True;
      break;
    end;
  end;

  for S := Low(TSuraNum) to High(TSuraNum) do begin
    with MarkedAyahs.Suras[S] do begin
      AyahBit := FirstClear;
      while AyahBit <> -1 do begin
        FErrors.Add(Format('Ayah %d.%d is missing', [S, AyahBit+1]));
        AyahBit := NextClear(AyahBit);
      end;
    end;
  end;
  try
    MarkedAyahs.Free;
  except
    on E : Exception do
      FErrors.Add(Format('Exception: %s on MarkedAyahs.Free in StoreQuranText', [E.Message]));
  end;

  DataStream := TStorageStream.Create('AyahMap', AStorage, amReadWrite, True);
  for S := Low(TSuraNum) to High(TSuraNum) do begin
    if not (SuraMap[S].SuraNum <> 0) then
      FErrors.Add(Format('SuraMap[%d].surNum should not be 0', [S]));
    SuraMap[S].AyahMapOffs := DataStream.Position;
    DataStream.Write(AyahsMaps[S], SuraMap[S].ayahCount * SizeOf(TAyahMapData));
    Inc(FBytesStored, DataStream.Size);
  end;
  DataStream.Free;

  DataStream := TStorageStream.Create('SuraMap', AStorage, amReadWrite, True);
  DataStream.Write(SuraMap, SizeOf(SuraMap));
  Inc(FBytesStored, DataStream.Size);
  DataStream.Free;

  FOnStatus(setEnd, '', 0, 0);
end;

procedure TQuranStorageBuilder.StoreQuranThemes(AStorage : TStorage; ADocument : TXMLDocument);
var
  DataStream : TStorageStream;
  SuraElem, ThemeElem : TXmlElement;
  S, T : Cardinal;
  SuraNum : TSuraNum;
  AyahNum1, AyahNum2 : TAyahNum;
  Themes : TStringList;
  SuraNumStr, SuraStorageName, ThemeKey, ThemeText : String;
  StartAyahDict : TStringList;
  MarkedAyahs : TMarkedAyahs;
begin
  Themes := TStringList.Create;
  StartAyahDict := TStringList.Create;
  StartAyahDict.Sorted := True;
  StartAyahDict.Duplicates := dupError;

  FOnStatus(setStart, 'Storing Themes', 0, FSuras.Length-1);
  MarkedAyahs := TMarkedAyahs.Create;
  MarkedAyahs.QuranStruct := FQuranStruct;
  MarkedAyahs.Duplicates := mdPrevent;

  for S := 0 to FSuras.Length-1 do begin
    SuraElem := (FSuras.Item(S) as TXmlElement);
    try
      SuraNumStr := SuraElem.GetAttribute('num');
      SuraNum := StrToInt(SuraNumStr);
    except
      FErrors.Add(Format('Sura number %s is invalid in theme block %d', [SuraNumStr, S]));
      continue;
    end;
    SuraStorageName := Format('S%d', [SuraNum]);

    Themes.Clear;
    for T := 0 to SuraElem.ChildNodes.Length-1 do begin
      if SuraElem.ChildNodes.Item(T).NodeType <> ELEMENT_NODE then
        continue;
      ThemeElem := (SuraElem.ChildNodes.Item(T) as TXmlElement);

      ThemeText := FilterS(TrimS(ThemeElem.Text), #9#10#11#13);
      ThemeKey := '';
      try
        if ThemeElem.GetAttribute('ayah') <> '' then begin
          try
            MarkedAyahs.Mark(Format('%s.%s', [SuraNumStr, ThemeElem.GetAttribute('ayah')]));
          except
            on E : Exception do
              FErrors.Add(E.Message);
          end;
          ThemeKey := ThemeElem.GetAttribute('ayah');
          StartAyahDict.Add(SuraStorageName+'_'+ThemeKey);
        end;
        if (ThemeElem.GetAttribute('startAyah') <> '') and (ThemeElem.GetAttribute('endAyah') <> '') then begin
          try
            MarkedAyahs.Mark(Format('%s.%s-%s', [SuraNumStr, ThemeElem.GetAttribute('startAyah'), ThemeElem.GetAttribute('endAyah')]));
          except
            on E : Exception do
              FErrors.Add(E.Message);
          end;
          ThemeKey := Format('%s-%s', [ThemeElem.GetAttribute('startAyah'), ThemeElem.GetAttribute('endAyah')]);
          StartAyahDict.Add(SuraStorageName+'_'+ThemeElem.GetAttribute('startAyah'));
        end;
      except
        FErrors.Add(Format('Theme %s of %s may be a duplicate', [ThemeKey, SuraStorageName]));
      end;

      if ThemeKey <> '' then
        Themes.Add(Format('%s=%s', [ThemeKey, ThemeText]))
      else
        FErrors.Add(Format('Theme %d key not found', [T]));
    end;

    DataStream := TStorageStream.Create(SuraStorageName, AStorage, amReadWrite, True);
    Themes.SaveToStream(DataStream);
    Inc(FBytesStored, DataStream.Size);
    DataStream.Free;

    if not FOnStatus(setProgress, '', S, 0) then begin
      FCanceled := True;
      break;
    end;
  end;
  FOnStatus(setEnd, '', 0, 0);

  MarkedAyahs.Free;
  StartAyahDict.Free;
  Themes.Free;
end;

procedure TQuranStorageBuilder.StoreQuranImagesDefn(AStorage : TStorage; ADocument : TXMLDocument);
var
  DataStream : TStorageStream;
  DefnElem : TXmlElement;
  D, DefnNum, MaxPage : Cardinal;
  StorageName : String;
begin
  MaxPage := 0;

  FOnStatus(setStart, 'Storing Image Definitions', 0, FRootTag.ChildNodes.Length-1);
  for D := 0 to FRootTag.ChildNodes.Length-1 do begin
    if FRootTag.ChildNodes.Item(D).NodeType <> ELEMENT_NODE then
      continue;
    DefnElem := (FRootTag.ChildNodes.Item(D) as TXmlElement);
    if DefnElem.TagName = TagNameCatalog then
      continue;
    try
      DefnNum := StrToInt(DefnElem.GetAttribute('num'));
    except
      FErrors.Add(Format('Invalid number %s in element %d', [DefnElem.GetAttribute('num'), D]));
      continue;
    end;

    if DefnElem.NodeName = 'page' then begin
      StorageName := Format('P%d', [DefnNum]);
      if DefnNum > MaxPage then MaxPage := DefnNum;
    end else if DefnElem.NodeName = 'sura' then
      StorageName := Format('S%d', [DefnNum])
    else if DefnElem.NodeName <> TagNameCatalog then begin
      FErrors.Add(Format('Unknown tag %s encountered at node %d', [DefnElem.NodeName, D]));
      continue;
    end;

    DataStream := TStorageStream.Create(StorageName, AStorage, amReadWrite, True);
    WriteStringToStream(DataStream, DefnElem.XmlDocument);
    Inc(FBytesStored, DataStream.Size);
    DataStream.Free;

    if not FOnStatus(setProgress, '', D, 0) then begin
      FCanceled := True;
      break;
    end;
  end;
  FOnStatus(setEnd, '', 0, 0);

  FCatalogContent.SetAttribute('maxpage', IntToStr(MaxPage));
end;

{------------------------------------------------------------------------------}

function TIndexStorageBuilder.CanHandleDocument(ADocument : TXMLDocument) : Boolean;
var
  IsAML : Boolean;
begin
  IsAML := inherited CanHandleDocument(ADocument);
  Result := IsAML and (FRootTag.NodeName = 'index');
  if not Result then
    Exit;

  if (CompareText(FContentSubtype, 'subject') = 0)then
    FType := itSubject
  else begin
    FErrors.Add('Unknown index type "'+FContentSubtype+'"');
    Result := False;
  end;

  { since we're only going to scan the file, we don't need a TStorage object }
  //FScanOnly := True;
end;

procedure TIndexStorageBuilder.StoreDocument(AStorage : TStorage; ADocument : TXMLDocument);
var
  DocStorage : TStorage;
begin
  if FCreateSubStorage then
    DocStorage := TStorage.Create(FSubStoreName, AStorage, amReadWrite, tmDirect, True)
  else
    DocStorage := AStorage;

  case FType of
    itSubject : StoreSubjectIndex(DocStorage, ADocument);
  end;

  if FCreateSubStorage then
    DocStorage.Free;
end;

procedure TIndexStorageBuilder.StoreSubjectIndex(AStorage : TStorage; ADocument : TXMLDocument);
var
  TopicElem, ChildElem, SubtopicChildElem : TXmlElement;
  T, TChild, STChild : Cardinal;
  TopicName, SubtopicName, AyahId : String;
begin
  { remember, FScanOnly is true so AStorage will be UNDEFINED, don't use it }

  FOnStatus(setStart, 'Capturing Subjects', 0, FRootTag.ChildNodes.Length-1);
  for T := 0 to FRootTag.ChildNodes.Length-1 do begin
    if FRootTag.ChildNodes.Item(T).NodeType <> ELEMENT_NODE then
      continue;
    TopicElem := (FRootTag.ChildNodes.Item(T) as TXmlElement);
    if (TopicElem.TagName = TagNameCatalog) then
      continue;

    TopicName := TopicElem.GetAttribute('name');
    for TChild := 0 to TopicElem.ChildNodes.Length-1 do begin
      if TopicElem.ChildNodes.Item(TChild).NodeType <> ELEMENT_NODE then
        continue;
      ChildElem := (TopicElem.ChildNodes.Item(TChild) as TXmlElement);
      if ChildElem.NodeName = 'see' then
        FSubjects.AddEntryAlias(TopicName, ChildElem.GetAttribute('topic'))
      else if ChildElem.NodeName = 'ayah' then begin
        AyahId := ChildElem.GetAttribute('id');
        if AyahId <> '' then begin
          FGenericAyahInSubjIdx := True;
          FSubjects.AddEntry(TopicName, GenericAyahId+ChildElem.GetAttribute('id'));
          FInverseSubjects.AddEntry(GenericAyahId+ChildElem.GetAttribute('id'), TopicName);
        end else
          FErrors.Add(Format('Topic %s has an ayah element (%d) with no id', [TopicName, Succ(TChild)]));
      end else if ChildElem.NodeName = 'subtopic' then begin
        SubtopicName := ChildElem.GetAttribute('name');
        if ChildElem.ChildNodes.Length <= 0 then begin
          FErrors.Add(Format('Topic "%s" subtopic "%s" has no items', [TopicName, SubtopicName]));
          continue;
        end;
        for STChild := 0 to ChildElem.ChildNodes.Length-1 do begin
          if ChildElem.ChildNodes.Item(STChild).NodeType <> ELEMENT_NODE then
            continue;
          SubtopicChildElem := (ChildElem.ChildNodes.Item(STChild) as TXmlElement);
          if SubtopicChildElem.NodeName = 'ayah' then begin
            AyahId := SubtopicChildElem.GetAttribute('id');
            if AyahId <> '' then begin
              FGenericAyahInSubjIdx := True;
              FSubjects.AddSubEntry(TopicName, SubtopicName, GenericAyahId+AyahId);
              FInverseSubjects.AddEntry(GenericAyahId+AyahId, TopicName + '/' + SubtopicName);
            end else
              FErrors.Add(Format('Topic %s Subtopic %s has an ayah element (%d) with no id', [TopicName, SubtopicName, Succ(STChild)]));
          end else
            FErrors.Add(Format('Unexpected tag "%s" found in topic "%s", subtopic "%s"', [SubtopicChildElem.NodeName, TopicName, SubtopicName]));
        end;
      end else
        FErrors.Add(Format('Unexpected tag "%s" found in topic "%s"', [TopicElem.NodeName, TopicName]));
    end;

    if not FOnStatus(setProgress, '', T, 0) then begin
      FCanceled := True;
      break;
    end;
  end;
  FOnStatus(setEnd, '', 0, 0);

  FOnStatus(setStart, 'Storing Subjects', 0, 0);
  StoreSubjectsIndex(AStorage);
  StoreCatalog(AStorage);
  FOnStatus(setEnd, '', 0, 0);
end;

{------------------------------------------------------------------------------}

function THadithFiqhStorageBuilder.CanHandleDocument(ADocument : TXMLDocument) : Boolean;
var
  IsAML : Boolean;
begin
  IsAML := inherited CanHandleDocument(ADocument);
  Result := IsAML and ((FRootTag.NodeName = 'ahadith') or (FRootTag.NodeName = 'fiqa'));
  if not Result then
    Exit;

  if (FContentType <> 'fiqh') and (FContentType <> 'hadith') then begin
    FErrors.Add('content type of fiqh or hadith expected');
    Result := False;
    Exit;
  end;
  if (FContentSubType <> '') then begin
    FErrors.Add('content subtype not expected');
    Result := False;
    Exit;
  end;

  if FNarrators <> Nil then
    FNarrators.Free;
  FNarrators := TPageIndex.Create;

  if FRootTag.NodeName = 'ahadith' then begin
    FType := htHadith
  end else begin
    FType := htFiqh;
  end;
end;

procedure THadithFiqhStorageBuilder.StoreNarrators(AStorage : TStorage);
const
  NarrIndexName = 'NarratorIndex';
var
  Storage : TStorage;
  IndexElem : TXmlElement;
begin
  if FNarrators.Entries.Count <= 0 then
    Exit;

  IndexElem := FDocument.CreateElement('narrators');
  IndexElem.SetAttribute('storage', NarrIndexName);
  FCatalogIndexes.AppendChild(IndexElem);

  FOnStatus(setStart, 'Storing Narrators', 0, 0);
  Storage := TStorage.Create(NarrIndexName, AStorage, amReadWrite, tmDirect, True);
  FNarrators.Store(Storage);
  Inc(FBytesStored, FNarrators.StoreSize);
  Storage.Free;
  FOnStatus(setEnd, '', 0, 0);
end;

procedure THadithFiqhStorageBuilder.StoreDocument(AStorage : TStorage; ADocument : TXMLDocument);
var
  DocStorage : TStorage;
  DataStream : TStorageStream;
  StorageName, NarrName : String;
  NextElem, PrevElem, Elem, NarrElem : TXmlElement;
  H, MaxH : Cardinal;
  FirstHadith : Boolean;

  function GetStorageName(AElem : TXmlElement) : String;
  begin
    if AElem.GetAttribute('vol') <> '' then
      Result := Format('%s.%s', [AElem.GetAttribute('vol'), AElem.GetAttribute('num')])
    else
      Result := Format('%s', [AElem.GetAttribute('num')]);
  end;

begin
  if FCreateSubStorage then
    DocStorage := TStorage.Create(FSubStoreName, AStorage, amReadWrite, tmDirect, True)
  else
    DocStorage := AStorage;

  FirstHadith := True;
  FOnStatus(setStart, 'Storing Hadith/Fiqh', 0, FRootTag.ChildNodes.Length-1);
  MaxH := Pred(FRootTag.ChildNodes.Length);
  for H := 0 to MaxH do begin
    if (FRootTag.ChildNodes.Item(H).NodeType <> ELEMENT_NODE) then
      continue;
    Elem := (FRootTag.ChildNodes.Item(H) as TXmlElement);
    if (Elem.TagName = TagNameCatalog) then
      continue;

    if (Elem.TagName <> 'hadith') and (Elem.TagName <> 'fiqh') then begin
      FErrors.Add(Format('Unknown tag %s encountered at node %d', [Elem.TagName, H]));
      continue;
    end;

    StorageName := GetStorageName(Elem);
    if FirstHadith then begin
      if FStructHome.GetAttribute('href') = '' then
        FStructHome.SetAttribute('href', Format('%s:%s', [FContentId, StorageName]));
      if FStructHome.GetAttribute('name') = '' then
        FStructHome.SetAttribute('name', StorageName);
      if FStructHome.GetAttribute('stream') = '' then
        FStructHome.SetAttribute('stream', StorageName);
      FirstHadith := False;
    end;

    NextElem := GetSiblingElem(Elem, True);
    PrevElem := GetSiblingElem(Elem, False);
    if PrevElem <> Nil then
      Elem.SetAttribute('prev', GetStorageName(PrevElem));
    if NextElem <> Nil then
      Elem.SetAttribute('next', GetStorageName(NextElem));

    NarrElem := Elem.FindElement('narration');
    if NarrElem <> Nil then begin
      NarrName := NarrElem.GetAttribute('by');
      if NarrName <> '' then
        FNarrators.AddEntry(NarrName, Format('%s:%s', [FContentId, StorageName]))
        //FGlobals.AddNarrator(NarrName, Format('%s:%s', [FProperties.Values['ID'], StorageName]))
      else
        FErrors.Add(Format('No narrator name (attribute "by") found in narration element %s (%d)', [StorageName, H]));
    end{ else      -- commented out because of Al-Muwatta's lack of Narrators
      if FType = htHadith then
        FErrors.Add(Format('No narration element found in Hadith %s (%d)', [StorageName, H]))};

    AddIndexElems(Elem, Format('%s:%s', [FContentId, StorageName]));
    AddWords(Elem.Text, StorageName);

    DataStream := TStorageStream.Create(StorageName, DocStorage, amReadWrite, True);
    WriteStringToStream(DataStream, Elem.XmlDocument);
    Inc(FBytesStored, DataStream.Size);
    DataStream.Free;

    if not FOnStatus(setProgress, '', H, 0) then begin
      FCanceled := True;
      break;
    end;
  end;
  FOnStatus(setEnd, '', 0, 0);

  StoreSubjectsIndex(DocStorage);
  StoreWordsIndex(DocStorage);
  StoreNarrators(DocStorage);
  StoreCatalog(DocStorage);

  if FCreateSubStorage then
    DocStorage.Free;
end;

{------------------------------------------------------------------------------}

function TArticlesStorageBuilder.CanHandleDocument(ADocument : TXMLDocument) : Boolean;
begin
  Result := inherited CanHandleDocument(ADocument);
  Result := Result and (FContentType = 'article');
  if not Result then
    Exit;

  if not HasElement(FCatalog, TagNameStructure, FCatalogNavg, True) then
    Exit;
  if FCatalogNavg.GetAttribute('style') = '' then
    FCatalogNavg.SetAttribute('style', 'book');

  if not HasElement(FCatalogContent, 'page', FStructPage, True) then
    Exit;
  if not HasElement(FCatalogContent, 'item', FStructItem, True) then
    Exit;

  if FContentSubType = SubtypeSuras then begin
    FPageTag := GetAttrOrSetDefault(FStructPage, 'tag', 'sura');
    FPageAttr := GetAttrOrSetDefault(FStructPage, 'attr', 'num');
    GetAttrOrSetDefault(FStructPage, 'name', 'Sura');
  end else begin
    FPageTag := GetAttrOrSetDefault(FStructPage, 'tag', 'article');
    FPageAttr := GetAttrOrSetDefault(FStructPage, 'attr', 'title');
    GetAttrOrSetDefault(FStructPage, 'name', 'Article');
  end;
  GetAttrOrSetDefault(FStructPage, 'browse', 'yes');
  GetAttrOrSetDefault(FStructPage, 'menus', 'no');
  GetAttrOrSetDefault(FStructPage, 'tabs', 'no');

  FItemTag := GetAttrOrSetDefault(FStructItem, 'tag', 'section');
  FItemAttr := GetAttrOrSetDefault(FStructItem, 'attr', 'heading');
  GetAttrOrSetDefault(FStructItem, 'name', 'Section');
  GetAttrOrSetDefault(FStructItem, 'browse', 'yes');
  case FindString(FCatalogNavg.GetAttribute('viewer'), ['none', 'html', 'htmlcombo', 'list', 'listcombo']) of
    -1, 0, 1, 3 : GetAttrOrSetDefault(FStructItem, 'menus', 'yes');
  else
    GetAttrOrSetDefault(FStructItem, 'menus', 'no');
  end;
  GetAttrOrSetDefault(FStructItem, 'split', 'yes');     // show sections in multiple pages (instead of all together)
  GetAttrOrSetDefault(FStructItem, 'tabs', 'no');

  Result := True;
end;

procedure TArticlesStorageBuilder.StoreDocument(AStorage : TStorage; ADocument : TXMLDocument);
const
  IllegalChars = '?:/!';
var
  DocStorage : TStorage;
  DataStream : TStorageStream;
  StorageName, ArticleName : String;
  ArticleElem, SectElem : TXmlElement;
  ArticleStructNode, SectStructNode : TXmlElement;
  SectList : TXmlNodeList;
  SectIdx, A, IllCh : Cardinal;
  ElemIdx : Integer;
begin
  if FCreateSubStorage then
    DocStorage := TStorage.Create(FSubStoreName, AStorage, amReadWrite, tmDirect, True)
  else
    DocStorage := AStorage;

  ElemIdx := 0;
  FOnStatus(setStart, 'Storing Articles', 0, FRootTag.ChildNodes.Length-1);
  for A := 0 to FRootTag.ChildNodes.Length-1 do begin
    if FRootTag.ChildNodes.Item(A).NodeType <> ELEMENT_NODE then
      continue;
    ArticleElem := (FRootTag.ChildNodes.Item(A) as TXmlElement);
    ArticleName := ArticleElem.GetAttribute(FPageAttr);
    if (ArticleElem.TagName = TagNameCatalog) then
      continue;
    if (ArticleElem.NodeName <> FPageTag) then begin
      FErrors.Add(Format('Ignoring element %d (%s)', [A, ArticleElem.TagName]));
      continue;
    end;
    if ArticleName = '' then begin
      FErrors.Add(Format('Article %d has blank element name', [A]));
      continue;
    end;

    //The following characters have special meanings in Alim addresses, so alert errors
    for IllCh := 1 to Length(IllegalChars) do
      if Pos(IllegalChars[IllCh], ArticleName) > 0 then
        FErrors.Add(Format('Article %d has %s in name: %s', [A, IllegalChars[IllCh], ArticleName]));

    if ArticleElem.GetAttribute(TagAttrTabName) = '' then
      ArticleElem.SetAttribute(TagAttrTabName, ArticleName);

    StorageName := IntToStr(ElemIdx);
    ArticleStructNode := CopyNodeEmpty(FDocument, ArticleElem);
    ArticleStructNode.SetAttribute('stream', StorageName);
    FCatalogStruct.AppendChild(ArticleStructNode);

    if ElemIdx = 0 then begin
      if FStructHome.GetAttribute('href') = '' then
        FStructHome.SetAttribute('href', Format('%s:%s', [FContentId, ArticleName]));
      if FStructHome.GetAttribute('name') = '' then
        FStructHome.SetAttribute('name', ArticleName);
      if FStructHome.GetAttribute('stream') = '' then
        FStructHome.SetAttribute('stream', StorageName);
    end;

    AddIndexElems(ArticleElem, Format('%s:%s', [FContentId, StorageName]));
    SectList := ArticleElem.GetChildElementsByTagName(FItemTag);
    if SectList.Length > 0 then begin
      for SectIdx := 0 to SectList.Length-1 do begin
        SectElem := SectList.Item(SectIdx) as TXmlElement;
        SectStructNode := CopyNodeEmpty(FDocument, SectElem);
        ArticleStructNode.AppendChild(SectStructNode);
        AddWords(SectElem.Text, Format('%s/%d', [StorageName, SectIdx]));
      end;
    end else begin
      AddWords(ArticleElem.Text, StorageName);
    end;

    DataStream := TStorageStream.Create(StorageName, DocStorage, amReadWrite, True);
    WriteStringToStream(DataStream, ArticleElem.XmlDocument);
    Inc(FBytesStored, DataStream.Size);
    DataStream.Free;

    if not FOnStatus(setProgress, '', A, 0) then begin
      FCanceled := True;
      break;
    end;

    Inc(ElemIdx);
  end;
  FOnStatus(setEnd, '', 0, 0);

  StoreSubjectsIndex(DocStorage);
  StoreWordsIndex(DocStorage);
  StoreCatalog(DocStorage);
  if FCreateSubStorage then
    DocStorage.Free;
end;

end.
