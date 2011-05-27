unit IndexStruct;

interface

uses Classes, CompDoc, IslUtils, SysUtils, Dialogs, SearchExpr;

type
  { EntryLocs key legend:
    abc       string is an internal address, objects is empty
    xyz:abc   string is an external address to bookId xyz, objects is empty
    @xyz      string is an alias to another entry, object is empty
    _xyz      string is a subentry, the object value is another TEntryLocs class
  }
  TEntryLocKind = (ekUnknown, ekAddress, ekAlias, ekSubentry);
  TEntryLocs = class(TObjDict)
  public
    procedure AddAlias(const Alias : String); virtual;
    procedure AddSub(const SubEntry, Addr : String; const AddKeyPrefix : Boolean = True); virtual;
    procedure GetEntryDetails(const Index : Integer; var Kind : TEntryLocKind; var Caption : String; var Sublocs : TEntryLocs);
    function RecursiveCount : Integer; virtual;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure Merge(const Locs : TEntryLocs); virtual;
  end;

  EEntryLocOpenError = class(Exception);
  EEntryLocReadError = class(Exception);

  TLocsDataStyle = (ldsEntryLocs, ldsNumeric);
  TPageIndex = class(TObject)
  protected
    FEntries : TObjDict;   // all the entries in the index
    FEntriesData : TStringList; // for each entry in FEntries, the number of locations it has
    FStorage : TStorage;   // this is only non-null if Load method was called
    FStoreSize : LongInt;  // valid after call to Store (tells how much was stored }
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure AddEntryAlias(const Entry, Alias : String); virtual;
      {-when called like this, Addr is a link to Entry}
    procedure AddEntry(const Entry, Addr : String); virtual;
      {-when called like this, Addr is a link to Entry}
    procedure AddSubEntry(const Entry, SubEntry, Addr : String); virtual;
      {-when called like this, Addr is a link to Subentry of Entry}
    function GetEntryLocs(const Entry : String) : TEntryLocs; overload; virtual;
      {-finds key in the index; if key is not found, returns new TEntryLocs }

    procedure Load(const AName : String; const AStorage : TStorage); overload; virtual;
      {-load the index from substorage of given storage}
    procedure Load(const AStorage : TStorage); overload; virtual;
      {-load the index from given storage}
    procedure Store(const AStorage : TStorage); virtual;
      {-store the index in the given storage}
    procedure StorePacked(const AStorage : TStorage; const ALocsDataStyle : TLocsDataStyle); virtual;
      {-store the index in the given storage in a packed format (saves space)}

    property Entries : TObjDict read FEntries;
    property EntriesData : TStringList read FEntriesData;
    property Storage : TStorage read FStorage;
    property StoreSize : LongInt read FStoreSize; // useful ONLY after call to Store
  end;

  TCharIdxArray = array[Char] of Word;
  TCharIndex = record
    KeyIdx : Word;
    SecondCharIndex : TCharIdxArray;
  end;
  TFirstCharIndex = array[Char] of TCharIndex;  // quick access to first words with begin with a particular character

  TDirectoryHeader = record
    Version : array[0..9] of Char;
    EntriesCount : LongInt;
    LocsDataStyle : TLocsDataStyle;
    FirstCharIndex : TFirstCharIndex;
    FirstCharLastIndex : TCharIdxArray;
  end;

  TDirectoryEntry = record
    KeyPos : Cardinal;       // starting position of the key in the stream
    KeySize : Cardinal;      // number of bytes used to store the key
    LocCount : Cardinal;     // the number of times the entry appears
    LocPos : Cardinal;       // starting position of the locations data in the stream
    LocSize : Cardinal;      // number of bytes used to store the locations data
  end;

  EStaticIndexNoStorage = Exception;
  EStaticIndexInvalidEntry = Exception;
  TStaticIndex = class(TObject)
  protected
    FStorage : TStorage;         // the storage object in which index data is stored
    FDirectory : TStorageStream; // the index stream
    FData : TStorageStream;      // the data stream
    FHeader : TDirectoryHeader;  // the directory's header
    FRefCount : Word;         // reference count

    procedure Clear; virtual;
      {-close the storages, streams, etc and clear any cache}
    function GetCount : Cardinal;
      {-return the count of entries in this index}
    procedure SetStorage(const AStorage : TStorage); virtual;
      {-setup the storage for this index}
  public
    constructor Create;
    destructor Destroy; override;

    procedure FillKeys(const APrefix : String; const AKeys : TStringList);
      {-fill the AKeys object with all of the keys in this index}
    function FindEntry(const Key : String; var Index: Cardinal; var Entry : TDirectoryEntry): Boolean;
      {-given key Entry, find the corresponding Entry index using binary search}
    function GetEntry(const Entry : Cardinal) : TDirectoryEntry; overload; virtual;
      {-get the Entry'th entry in the directory }
    function GetEntryKey(const Entry : TDirectoryEntry) : String; overload; virtual;
      {-find given Entry in the disk-based index, return entry's key }
    function GetEntryKey(const Entry : Cardinal) : String; overload; virtual;
      {-find given Entry in the disk-based index, return entry's key }
    function GetEntryLocs(const Entry : TDirectoryEntry) : TEntryLocs; overload; virtual;
      {-given an Entry record, get the locations it is found in }
    function GetEntryLocs(const Entry : TSchExprToken; const AMatched : TStringList) : TEntryLocs; overload; virtual;
      {-given an Entry record, get the locations it is found in }
    function GetEntryLocs(const Entry : String) : TEntryLocs; overload; virtual;
      {-given an Entry record, get the locations it is found in }

    property RefCount : Word read FRefCount write FRefCount;
    property Storage : TStorage read FStorage write SetStorage;
    property Count : Cardinal read GetCount;
  end;

implementation

uses StUtils, StStrS;

procedure TEntryLocs.AddAlias(const Alias : String);
begin
  Add('@' + Alias);
end;

procedure TEntryLocs.AddSub(const SubEntry, Addr : String; const AddKeyPrefix : Boolean = True);
var
  RealKey : String;
  SubLocs : TEntryLocs;
  SubIdx : Integer;
begin
  if AddKeyPrefix then
    RealKey := '_' + SubEntry
  else
    RealKey := SubEntry;

  SubIdx := IndexOf(RealKey);
  if SubIdx >= 0 then begin
    SubLocs := Objects[SubIdx] as TEntryLocs;
    if SubLocs = Nil then begin
      SubLocs := TEntryLocs.Create;
      Objects[SubIdx] := SubLocs;
    end;
  end else begin
    SubLocs := TEntryLocs.Create;
    AddObject(RealKey, SubLocs);
  end;
  SubLocs.Add(Addr);
end;

procedure TEntryLocs.GetEntryDetails(const Index : Integer; var Kind : TEntryLocKind; var Caption : String; var Sublocs : TEntryLocs);
begin
  Kind := ekUnknown;
  Caption := Strings[Index];
  Sublocs := Nil;

  case Caption[1] of
    '@' :
      begin
        Kind := ekAlias;
        System.Delete(Caption, 1, 1);
      end;
    '_' :
      begin
        Kind := ekSubentry;
        System.Delete(Caption, 1, 1);
        Sublocs := Objects[Index] as TEntryLocs;
      end;
  else
    if Pos(':', Caption) > 0 then
      Kind := ekAddress
  end;
end;

function TEntryLocs.RecursiveCount : Integer;
var
  I : Integer;
  Kind : TEntryLocKind;
  Caption : String;
  SubLocs : TEntryLocs;
begin
  Result := 0;
  if Count <= 0 then
    Exit;

  for I := 0 to Count-1 do begin
    GetEntryDetails(I, Kind, Caption, Sublocs);
    case Kind of
      ekUnknown, ekAddress, ekAlias : Inc(Result);
      ekSubentry : Inc(Result, Sublocs.RecursiveCount);
    end;
  end;
end;

procedure TEntryLocs.LoadFromStream(Stream: TStream);
var
  TmpList : TStringList;
  EntryLocs : TEntryLocs;
  S : String;
  I, P : Integer;
begin
  Clear;

  TmpList := TStringList.Create;
  TmpList.LoadFromStream(Stream);

  for I := 0 to TmpList.Count-1 do begin
    S := TmpList[I];
    if S[1] = '_' then begin
      P := AnsiPos('~', S);
      if P > 0 then begin
        EntryLocs := TEntryLocs.Create;
        EntryLocs.CommaText := Copy(S, P+1, Length(S));
        Self.AddObject(Copy(S, 1, P-1), EntryLocs);
      end;
    end else begin
      Self.Add(S);
    end;
  end;

  TmpList.Free;
end;

procedure TEntryLocs.SaveToStream(Stream: TStream);
var
  TmpList : TStringList;
  EntryLocs : TEntryLocs;
  S : String;
  I : Integer;
begin
  TmpList := TStringList.Create;

  for I := 0 to Self.Count-1 do begin
    S := Self.Strings[I];
    if S[1] = '_' then begin
      EntryLocs := Self.Objects[I] as TEntryLocs;
      TmpList.Add(S + '~' + EntryLocs.CommaText);
    end else begin
      TmpList.Add(S);
    end;
  end;

  TmpList.SaveToStream(Stream);
  TmpList.Free;
end;

procedure TEntryLocs.Merge(const Locs : TEntryLocs);
var
  SubLocs : TEntryLocs;
  I, S : Integer;
  Loc : String;
  Kind : TEntryLocKind;
begin
  if (Locs = Nil) or (Locs.Count <= 0) then
    Exit;

  for I := 0 to Locs.Count-1 do begin
    Locs.GetEntryDetails(I, Kind, Loc, SubLocs);
    case Kind of
      ekUnknown, ekAddress : Add(Loc);
      ekAlias : AddAlias(Loc);
      ekSubentry :
        if (SubLocs <> Nil) and (SubLocs.Count > 0) then begin
          for S := 0 to SubLocs.Count-1 do
            AddSub(Loc, SubLocs[S]);
        end
    end;
  end;
end;

{------------------------------------------------------------------------------}

constructor TPageIndex.Create;
begin
  inherited Create;
  FEntries := TObjDict.Create;
  FEntriesData := TStringList.Create;
end;

destructor TPageIndex.Destroy;
begin
  FEntries.Free;
  if FEntriesData <> Nil then
    FEntriesData.Free;
  FEntriesData := Nil;
  if FStorage <> Nil then
    FStorage.Free;
  FStorage := Nil;
  inherited Destroy;
end;

procedure TPageIndex.AddEntryAlias(const Entry, Alias : String);
begin
  Assert(FStorage = Nil, 'Can''t add to a stream that''s being read from disk');
  GetEntryLocs(Entry).AddAlias(Alias);
end;

procedure TPageIndex.AddEntry(const Entry, Addr : String);
begin
  Assert(FStorage = Nil, 'Can''t add to a stream that''s being read from disk');
  GetEntryLocs(Entry).Add(Addr);
end;

procedure TPageIndex.AddSubEntry(const Entry, SubEntry, Addr : String);
begin
  Assert(FStorage = Nil, 'Can''t add to a stream that''s being read from disk');
  GetEntryLocs(Entry).AddSub(SubEntry, Addr);
end;

function TPageIndex.GetEntryLocs(const Entry : String) : TEntryLocs;
var
  DataStream : TStorageStream;
  EntryIdx : Integer;
begin
  Result := Nil;

  if FEntries.Find(Entry, EntryIdx) then begin
    Result := FEntries.Objects[EntryIdx] as TEntryLocs;
    if (Result = Nil) and (FStorage <> Nil) then begin
      Result := TEntryLocs.Create;
      FEntries.Objects[EntryIdx] := Result; // cache the results

      try
        DataStream := TStorageStream.Create(IntToStr(EntryIdx), FStorage, amRead, False);
        try
          Result.LoadFromStream(DataStream);
        except
          raise EEntryLocReadError.CreateFmt('Unable to read index entry "%s" (%d) locations from %s.', [Entry, EntryIdx, FStorage.Name]);
        end;
        DataStream.Free;
      except
        raise EEntryLocOpenError.CreateFmt('Unable to open index entry "%s" (%d) locations from %s.', [Entry, EntryIdx, FStorage.Name]);
      end;
    end;
  end else begin
    if FStorage = Nil then begin
      Result := TEntryLocs.Create;
      FEntries.AddObject(Entry, Result);
    end;
  end;
end;

procedure TPageIndex.Load(const AName : String; const AStorage : TStorage);
begin
  Load(TStorage.Create(AName, AStorage, amRead, tmDirect, False));
end;

procedure TPageIndex.Load(const AStorage : TStorage);
var
  DataStream : TStorageStream;
begin
  if FStorage <> Nil then
    FStorage.Free;
  FStorage := AStorage;

  DataStream := TStorageStream.Create('Entries', FStorage, amRead, False);
  FEntries.LoadFromStream(DataStream);
  DataStream.Free;

  DataStream := TStorageStream.Create('EntriesData', FStorage, amRead, False);
  FEntriesData.LoadFromStream(DataStream);
  DataStream.Free;

  // loading of entries will happen dynamically in GetEntryLocs
end;

procedure TPageIndex.Store(const AStorage : TStorage);
var
  DataStream : TStorageStream;
  I : Integer;
begin
  FStoreSize := 0;

  FEntriesData.Clear;
  FEntriesData.Sorted := False;

  for I := 0 to FEntries.Count-1 do begin
    DataStream := TStorageStream.Create(IntToStr(I), AStorage, amReadWrite, True);
    (FEntries.Objects[I] as TEntryLocs).SaveToStream(DataStream);
    FEntriesData.Add(IntToStr((FEntries.Objects[I] as TEntryLocs).RecursiveCount));
    Inc(FStoreSize, DataStream.Size);
    DataStream.Free;
  end;

  DataStream := TStorageStream.Create('Entries', AStorage, amReadWrite, True);
  FEntries.SaveToStream(DataStream);
  Inc(FStoreSize, DataStream.Size);
  DataStream.Free;

  DataStream := TStorageStream.Create('EntriesData', AStorage, amReadWrite, True);
  FEntriesData.SaveToStream(DataStream);
  Inc(FStoreSize, DataStream.Size);
  DataStream.Free;
end;

procedure TPageIndex.StorePacked(const AStorage : TStorage; const ALocsDataStyle : TLocsDataStyle);
type
  TDirectoryEntries = array[0..High(Word)] of TDirectoryEntry;
  PDirectoryEntries = ^TDirectoryEntries;
  TLocsDataNumeric = array[0..High(Word)] of Word;
  PLocsDataNumeric = ^TLocsDataNumeric;
var
  DirectoryStream, DataStream : TStorageStream;
  Directory : PDirectoryEntries;
  Locs : TEntryLocs;
  EntryKey, LocsData : String;
  I, DirectorySize, LocsNumericSize, J : Integer;
  Header : TDirectoryHeader;
  LocsNumeric : PLocsDataNumeric;
  FirstCh, SecondCh : Char;
begin
  FStoreSize := 0;
  if FEntries.Count <= 0 then
    Exit;

  Header.Version := '6.1.0';
  Header.EntriesCount := FEntries.Count;
  Header.LocsDataStyle := ALocsDataStyle;
  FillChar(Header.FirstCharIndex, SizeOf(Header.FirstCharIndex), $FF);

  DirectorySize := SizeOf(TDirectoryEntry) * FEntries.Count;
  GetMem(Directory, DirectorySize);
  DataStream := TStorageStream.Create('Data', AStorage, amReadWrite, True);
  for I := 0 to FEntries.Count-1 do begin
    EntryKey := Entries[I];
    Locs := (FEntries.Objects[I] as TEntryLocs);
    Assert(Locs <> Nil);

    FirstCh := UpCase(EntryKey[1]);
    with Header.FirstCharIndex[FirstCh] do begin
      if KeyIdx = $FFFF then KeyIdx := I;
      if Length(EntryKey) > 1 then begin
        SecondCh := UpCase(EntryKey[2]);
        if SecondCharIndex[SecondCh] = $FFFF then
          SecondCharIndex[SecondCh] := I;
      end;
    end;
    Header.FirstCharLastIndex[FirstCh] := I;

    Directory^[I].KeyPos := DataStream.Position;
    DataStream.Write(PChar(EntryKey)^, Length(EntryKey));
    Directory^[I].KeySize := Length(EntryKey);
    Inc(FStoreSize, Length(EntryKey)); 

    if ALocsDataStyle = ldsEntryLocs then begin
      LocsData := Locs.Text;
      Directory^[I].LocPos := DataStream.Position;
      DataStream.Write(PChar(LocsData)^, Length(LocsData));
      Directory^[I].LocSize := Length(LocsData);
      Directory^[I].LocCount := Locs.RecursiveCount;
      Inc(FStoreSize, Length(LocsData));
    end else begin
      LocsNumericSize := Locs.Count * SizeOf(Word);
      GetMem(LocsNumeric, LocsNumericSize);
      for J := 0 to Locs.Count-1 do
        LocsNumeric^[J] := StrToInt(Locs[J]);
      Directory^[I].LocPos := DataStream.Position;
      DataStream.Write(LocsNumeric, LocsNumericSize);
      Directory^[I].LocSize := LocsNumericSize;
      Directory^[I].LocCount := Locs.Count;
      Inc(FStoreSize, LocsNumericSize);
    end;
  end;
  DataStream.Free;

  (*
  DebugStr := '';
  for FirstCh := Low(Char) to High(Char) do begin
    with Header.FirstCharIndex[FirstCh] do begin
      if KeyIdx <> $FFFF then
        DebugStr := DebugStr + Format('%s - %d %d'#13#10, [FirstCh, KeyIdx, Header.FirstCharLastIndex[FirstCh]]);
    end;
  end;
  ShowMessage(DebugStr);
  *)

  DirectoryStream := TStorageStream.Create('Directory', AStorage, amReadWrite, True);
  DirectoryStream.Write(Header, SizeOf(Header));
  DirectoryStream.Write(Directory^, DirectorySize);
  Inc(FStoreSize, DirectoryStream.Size);
  DirectoryStream.Free;

  FreeMem(Directory, DirectorySize);
end;

// ----------------------------------------------------------------------------

constructor TStaticIndex.Create;
begin
  inherited Create;
end;

destructor TStaticIndex.Destroy;
begin
  if FData <> Nil then begin
    FData.Free;
    FData := Nil;
  end;
  if FDirectory <> Nil then begin
    FDirectory.Free;
    FDirectory := Nil;
  end;
  if FStorage <> Nil then begin
    FStorage.Free;
    FStorage := Nil;
  end;
  inherited Destroy;
end;

procedure CheckAndFree(var AObject : TObject);
begin
  if AObject <> Nil then begin
    AObject.Free;
    AObject := Nil;
  end;
end;

procedure TStaticIndex.Clear;
begin
  CheckAndFree(TObject(FDirectory));
  CheckAndFree(TObject(FData));
  CheckAndFree(TObject(FStorage));
end;

function TStaticIndex.GetCount : Cardinal;
begin
  if FStorage = Nil then
    raise EStaticIndexNoStorage.Create('Storage not provided');
  Result := FHeader.EntriesCount;
end;

procedure TStaticIndex.SetStorage(const AStorage : TStorage);
begin
  Clear;
  FStorage := AStorage;

  if FStorage <> Nil then begin
    FDirectory := TStorageStream.Create('Directory', FStorage, amRead, False);
    FDirectory.Read(FHeader, SizeOf(FHeader));
    FData := TStorageStream.Create('Data', FStorage, amRead, False);
  end;
end;

procedure TStaticIndex.FillKeys(const APrefix : String; const AKeys : TStringList);
var
  Len, I, L, H : Cardinal;
  InitialCh, SecondCh : Char;
  Key : String;
begin
  Len := Length(APrefix);
  if Len = 0 then begin
    for I := 0 to FHeader.EntriesCount-1 do
      AKeys.Add(GetEntryKey(I));
  end else begin
    InitialCh := UpCase(APrefix[1]);
    if Len > 1 then begin
      SecondCh := UpCase(APrefix[2]);
      L := FHeader.FirstCharIndex[InitialCh].SecondCharIndex[SecondCh];
    end else
      L := FHeader.FirstCharIndex[InitialCh].KeyIdx;
    H := FHeader.FirstCharLastIndex[InitialCh];

    for I := L to H do begin
      Key := GetEntryKey(I);
      if AnsiCompareText(Copy(Key, 1, Len), APrefix) = 0 then
        AKeys.Add(Key);
    end;
  end;
end;

function TStaticIndex.GetEntry(const Entry : Cardinal) : TDirectoryEntry;
begin
  Assert(FDirectory <> Nil);

  if Entry >= Cardinal(FHeader.EntriesCount) then
    EStaticIndexInvalidEntry.CreateFmt('Invalid Entry number %d', [Entry]);

  FDirectory.Position := SizeOf(TDirectoryHeader) + (Entry * SizeOf(TDirectoryEntry));
  FDirectory.Read(Result, SizeOf(TDirectoryEntry));
end;

function TStaticIndex.GetEntryKey(const Entry : TDirectoryEntry) : String;
begin
  Assert(FData <> Nil);

  SetLength(Result, Entry.KeySize);
  FData.Position := Entry.KeyPos;
  FData.Read(PChar(Result)^, Entry.KeySize);
end;

function TStaticIndex.GetEntryKey(const Entry : Cardinal) : String;
var
  DE : TDirectoryEntry;
begin
  Assert(FDirectory <> Nil);
  Assert(FData <> Nil);

  if Entry >= Cardinal(FHeader.EntriesCount) then
    EStaticIndexInvalidEntry.CreateFmt('Invalid Entry number %d', [Entry]);

  FDirectory.Position := SizeOf(TDirectoryHeader) + (Entry * SizeOf(TDirectoryEntry));
  FDirectory.Read(DE, SizeOf(TDirectoryEntry));

  SetLength(Result, DE.KeySize);
  FData.Position := DE.KeyPos;
  FData.Read(PChar(Result)^, DE.KeySize);
end;

function TStaticIndex.FindEntry(const Key : String; var Index: Cardinal; var Entry : TDirectoryEntry): Boolean;
var
  Len, L, H, I, C : Integer;
  InitialCh, SecondCh : Char;
begin
  Result := False;
  Len := Length(Key);
  if Len = 0 then
    Exit;

  //L := 0;
  //H := FHeader.EntriesCount - 1;
  InitialCh := UpCase(Key[1]);
  if Len > 1 then begin
    SecondCh := UpCase(Key[2]);
    L := FHeader.FirstCharIndex[InitialCh].SecondCharIndex[SecondCh];
  end else
    L := FHeader.FirstCharIndex[InitialCh].KeyIdx;
  H := FHeader.FirstCharLastIndex[InitialCh];

  while L <= H do begin
    I := (L + H) shr 1;

    Entry := GetEntry(I);
    C := AnsiCompareText(GetEntryKey(Entry), Key);

    if C < 0 then
      L := I + 1
    else begin
      H := I - 1;
      if C = 0 then begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

function TStaticIndex.GetEntryLocs(const Entry : TDirectoryEntry) : TEntryLocs;
var
  SLText : String;
begin
  Assert(FData <> Nil);

  SetLength(SLText, Entry.LocSize);
  FData.Position := Entry.LocPos;
  FData.Read(PChar(SLText)^, Entry.LocSize);

  Result := TEntryLocs.Create;
  Result.Text := SLText;
end;

function TStaticIndex.GetEntryLocs(const Entry : TSchExprToken; const AMatched : TStringList) : TEntryLocs;
var
  Len, I, L, H, FullLen : Cardinal;
  CompareResult : Integer;
  DE : TDirectoryEntry;
  InitialCh, SecondCh : Char;
  FullKey, MatchSound : ShortString;
  TempLocs : TEntryLocs;
begin
  Result := Nil;
  Len := Length(Entry.Text);
  if Len = 0 then
    Exit;

  if (not FlagIsSet(Entry.Flags, setfLikeSearch)) and (not FlagIsSet(Entry.Flags, setfStemSearch)) then begin
    if FindEntry(Entry.Text, I, DE) then
      Result := GetEntryLocs(DE);
  end else begin
    if FlagIsSet(Entry.Flags, setfStemSearch) then begin
      InitialCh := UpCase(Entry.Text[1]);
      if Len > 1 then begin
        SecondCh := UpCase(Entry.Text[2]);
        L := FHeader.FirstCharIndex[InitialCh].SecondCharIndex[SecondCh];
      end else
        L := FHeader.FirstCharIndex[InitialCh].KeyIdx;
      H := FHeader.FirstCharLastIndex[InitialCh];

      for I := L to H do begin
        DE := GetEntry(I);
        FullKey := GetEntryKey(DE);
        FullLen := Length(FullKey);
        if FullLen = Len then
          CompareResult := AnsiCompareText(FullKey, Entry.Text)
        else if FullLen > Len then
          CompareResult := AnsiCompareText(Copy(FullKey, 1, Len), Entry.Text)
        else
          CompareResult := -1;

        if CompareResult = 0 then begin
          AMatched.Add(FullKey);
          TempLocs := GetEntryLocs(DE);
          if Result = Nil then
            Result := TEntryLocs.Create;
          Result.Merge(TempLocs);
          TempLocs.Free;
        end else if CompareResult > 0 then
          Exit;
      end;
    end else if FlagIsSet(Entry.Flags, setfLikeSearch) then begin
      MatchSound := SoundexS(Entry.Text);
      InitialCh := UpCase(Entry.Text[1]);
      L := FHeader.FirstCharIndex[InitialCh].KeyIdx;
      H := FHeader.FirstCharLastIndex[InitialCh];
      for I := L to H do begin
        DE := GetEntry(I);
        FullKey := GetEntryKey(DE);
        if SoundexS(FullKey) = MatchSound then begin
          AMatched.Add(FullKey);
          TempLocs := GetEntryLocs(DE);
          if Result = Nil then
            Result := TEntryLocs.Create;
          Result.Merge(TempLocs);
          TempLocs.Free;
        end;
      end;
    end;
  end;
end;

function TStaticIndex.GetEntryLocs(const Entry : String) : TEntryLocs;
var
  I : Cardinal;
  DE : TDirectoryEntry;
begin
  Result := Nil;
  if FindEntry(Entry, I, DE) then
    Result := GetEntryLocs(DE);
end;

end.
