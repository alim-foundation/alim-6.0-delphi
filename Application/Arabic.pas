unit Arabic;

interface

uses Classes, SysUtils, XmlObjModel, Dialogs;

type
  TAraAlphaName = String[16];
  TAraAlphaId =
   { 00-04 } (aaUnknown, aaAlif, aaBaa, aaTaa, aaSaa,
   { 05-09 }  aaJeem, aaHaa, aaKhaa, aaDaal, aaDhaal,
   { 10-14 }  aaRaa, aaZaa, aaSeen, aaSheen, aaSaud,
   { 15-19 }  aaDhaud, aaTau, aaZau, aaAain, aaGhain,
   { 20-24 }  aaFaa, aaQaaf, aaKaaf, aaLaam, aaMeem,
   { 25-29 }  aaNoon, aaHa, aaWaw, aaYaa, aaHamza,
   { 30-31 }  aaAlifMud, aaThaaMarbutah, aaLamAlif);

const
  FirstArabicLetter : TAraAlphaId = aaAlif;
  LastArabicLetter : TAraAlphaId = aaYaa;
  AraAlphaNames : array [TAraAlphaId] of TAraAlphaName =
   { 00-04 } ('', 'alif', 'baa', 'taa', 'saa',
   { 05-09 }  'jeem', 'haa', 'khaa', 'daal', 'dhaal',
   { 10-14 }  'raa', 'zaa', 'seen', 'sheen', 'saud',
   { 15-19 }  'dhaud', 'tau', 'zau', 'aain', 'ghain',
   { 20-24 }  'faa', 'qaaf', 'kaaf', 'laam', 'meem',
   { 25-29 }  'noon', 'ha', 'waw', 'yaa', 'hamza',
   { 30-31 }  'alifmud', 'thaamarbutah', 'lamalif');

type
  TAraLetterContext = (lcIsolated, lcInitial, lcMedial, lcFinal);
  TAraCharMapItem =
    record
      AlphaId : TAraAlphaId;
      AlphaName : TAraAlphaName;
      ConnectToLeft : Boolean;
      ConnectToRight : Boolean;
      ContextChars : array[TAraLetterContext] of Byte;
    end;

  PAraCharMap = ^TAraCharMap;
  PAraKeyboardMap = ^TAraKeyboardMap;
  TAraCharMap = array[TAraAlphaId] of TAraCharMapItem;
  TAraKeyboardMap = array[Char] of TAraAlphaId;

  EArabicEncodingInvalidName = Exception;
  EArabicEncodingParseError = Exception;

  TArabicEncodings = class(TObject)
  protected
    FActiveCharMapName : String;          // name of character map in use
    FActiveKeyboardMapName : String;      // name keyboard map in use
    FActiveCharMap : PAraCharMap;         // actual map in use
    FActiveKeyboardMap : PAraKeyboardMap; // actual map in use
    FCharMaps : TStringList;              // key is encoding type, value is PAraCharMap
    FKeyboardMaps : TStringList;          // key is map name, value is PAraKeyboardMap
    FFontEncodings : TStringList;         // key is the font name, value is the encoding name (FCharMap)
    FEncodingFonts : TStringList;         // key is the encoding name, value is a TStringList of font names
    FDefaultFonts : TStringList;          // key is the encoding name, value is the name of the default font
    FActiveEncDefaultFont : String;       // the default font of the active encoding

    procedure Clear;
    function GetActiveCharMapFonts : TStringList;
    function GetFontEncoding(AFontName : String) : String;
    procedure SetActiveCharMapName(AName : String);
    procedure SetActiveKeyboardMapName(AName : String);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure LoadFromAML(const ARootElem : TXmlElement);
    function ToArabic(const AText : String) : String;
    function GetLetterFontChar(const AAraAlphaNum : Byte; const AShape : TAraLetterContext) : Char;

    property ActiveCharMapName : String read FActiveCharMapName write SetActiveCharMapName;
    property ActiveKeyboardMapName : String read FActiveKeyboardMapName write SetActiveKeyboardMapName;
    property AvailableFonts : TStringList read GetActiveCharMapFonts;
    property FontEncoding[Index : String] : String read GetFontEncoding;

    property ActiveCharMap : PAraCharMap read FActiveCharMap;
    property ActiveKeyboardMap : PAraKeyboardMap read FActiveKeyboardMap;
    property CharMaps : TStringList read FCharMaps;
    property KeyboardMaps : TStringList read FKeyboardMaps;
    property FontEncodings : TStringList read FFontEncodings;
    property EncodingFonts : TStringList read FEncodingFonts;
    property DefaultFontName : String read FActiveEncDefaultFont;
  end;

  function AraAlphaNameToId(const AName : String) : TAraAlphaId;

implementation

uses StStrS;

function AraAlphaNameToId(const AName : String) : TAraAlphaId;
var
  L : TAraAlphaId;
begin
  Result := aaUnknown;
  for L := Low(TAraAlphaId) to High(TAraAlphaId) do begin
    if CompareText(AraAlphaNames[L], AName) = 0 then begin
      Result := L;
      Exit;
    end;
  end;
end;

constructor TArabicEncodings.Create;
begin
  inherited Create;
  FCharMaps := TStringList.Create;
  FCharMaps.Sorted := True;
  FCharMaps.Duplicates := dupError;

  FKeyboardMaps := TStringList.Create;
  FKeyboardMaps.Sorted := True;
  FKeyboardMaps.Duplicates := dupError;

  FFontEncodings := TStringList.Create;
  FDefaultFonts := TStringList.Create;

  FEncodingFonts := TStringList.Create;
  FEncodingFonts.Sorted := True;
  FEncodingFonts.Duplicates := dupError;
end;

destructor TArabicEncodings.Destroy;
begin
  Clear;
  FCharMaps.Free;
  FKeyboardMaps.Free;
  FEncodingFonts.Free;
  FFontEncodings.Free;
  FDefaultFonts.Free;
  inherited Destroy;
end;

procedure TArabicEncodings.Clear;
var
  I : Integer;
begin
  FActiveCharMapName := '';
  FActiveKeyboardMapName := '';
  FActiveCharMap := Nil;
  FActiveKeyboardMap := Nil;
  if (FCharMaps <> Nil) and (FCharMaps.Count > 0) then begin
    for I := 0 to FCharMaps.Count-1 do Dispose(Pointer(FCharMaps.Objects[I]));
    FCharMaps.Clear;
  end;
  if (FKeyboardMaps <> Nil) and (FKeyboardMaps.Count > 0) then begin
    for I := 0 to FKeyboardMaps.Count-1 do Dispose(Pointer(FKeyboardMaps.Objects[I]));
    FKeyboardMaps.Clear;
  end;
  if (FEncodingFonts <> Nil) and (FEncodingFonts.Count > 0) then begin
    for I := 0 to FEncodingFonts.Count-1 do FEncodingFonts.Objects[I].Free;
    FEncodingFonts.Clear;
  end;
  FFontEncodings.Clear;
  FDefaultFonts.Clear;
end;

function TArabicEncodings.GetActiveCharMapFonts : TStringList;
var
  I : Integer;
begin
  Result := Nil;
  if FEncodingFonts.Find(FActiveCharMapName, I) then
    Result := TStringList(FEncodingFonts.Objects[I]);
end;

function TArabicEncodings.GetFontEncoding(AFontName : String) : String;
begin
  Result := FFontEncodings.Values[AFontName];
end;

procedure TArabicEncodings.SetActiveCharMapName(AName : String);
var
  I : Integer;
  Fonts : TStringList;
begin
  FActiveCharMapName := AName;
  if FCharMaps.Find(AName, I) then begin
    FActiveCharMap := PAraCharMap(FCharMaps.Objects[I]);
    Fonts := AvailableFonts;
    FActiveEncDefaultFont := FDefaultFonts.Values[AName];
    if (FActiveEncDefaultFont = '') and (Fonts.Count > 0) then
      FActiveEncDefaultFont := Fonts[0];
  end else begin
    FActiveCharMap := Nil;
    raise EArabicEncodingInvalidName.CreateFmt('Invalid char map name %s', [AName]);
  end;
end;

procedure TArabicEncodings.SetActiveKeyboardMapName(AName : String);
var
  I : Integer;
begin
  FActiveKeyboardMapName := AName;
  if FKeyboardMaps.Find(AName, I) then
    FActiveKeyboardMap := PAraKeyboardMap(FKeyboardMaps.Objects[I])
  else begin
    FActiveKeyboardMap := Nil;
    raise EArabicEncodingInvalidName.CreateFmt('Invalid keyboard map name %s', [AName]);
  end;
end;

procedure TArabicEncodings.LoadFromAML(const ARootElem : TXmlElement);
var
  T, C, ListIdx : Integer;
  TypeNode, ItemNode : TXmlNode;
  TypeElem, ItemElem : TXmlElement;
  AraCharMap : PAraCharMap;
  AraKeyMap : PAraKeyboardMap;
  ChStr : ShortString;
  Letter : TAraAlphaId;
  FontNamesList : TStringList;
  FontName, EncodingName : ShortString;
  DefFont : Boolean;
begin
  Clear;
  if ARootElem.ChildNodes.Length <= 0 then
    Exit;

  for T := 0 to ARootElem.ChildNodes.Length-1 do begin
    TypeNode := ARootElem.ChildNodes.Item(T) as TXmlNode;
    if (TypeNode.NodeType = ELEMENT_NODE) then begin
      TypeElem := TypeNode as TXmlElement;
      if TypeElem.NodeName = 'charmap' then begin
        EncodingName := TypeElem.GetAttribute('encoding');
        New(AraCharMap);
        FillChar(AraCharMap^, SizeOf(AraCharMap^), 0);
        try
          FCharMaps.AddObject(EncodingName, TObject(AraCharMap));
        except
          ShowMessage('Duplicate encoding format found: ' + EncodingName);
          continue;
        end;
        for C := 0 to TypeElem.ChildNodes.Length-1 do begin
          ItemNode := TypeElem.ChildNodes.Item(C) as TXmlNode;
          if ItemNode.NodeType = ELEMENT_NODE then begin
            ItemElem := ItemNode as TXmlElement;
            if ItemElem.NodeName = 'font' then begin
              FontName := ItemElem.GetAttribute('name');
              DefFont := ItemElem.GetAttribute('default') = 'yes';
              if DefFont then
                FDefaultFonts.Values[EncodingName] := FontName;
              FFontEncodings.Values[FontName] := EncodingName;
              if not FEncodingFonts.Find(EncodingName, ListIdx) then begin
                FontNamesList := TStringList.Create;
                FontNamesList.Sorted := True;
                FontNamesList.Duplicates := dupIgnore;
                FEncodingFonts.AddObject(EncodingName, FontNamesList);
              end else
                FontNamesList := FEncodingFonts.Objects[ListIdx] as TStringList;
              FontNamesList.Add(FontName)
            end else if ItemElem.NodeName = 'context' then begin
              Letter := AraAlphaNameToId(ItemElem.GetAttribute('letter'));
              if Letter = aaUnknown then begin
                Clear;
                raise EArabicEncodingParseError.CreateFmt('Unknown arabic letter %s', [ItemElem.GetAttribute('letter')]);
              end;
              with AraCharMap[Letter] do begin
                AlphaId := Letter;
                AlphaName := AraAlphaNames[Letter];
                ConnectToLeft := ItemElem.GetAttribute('connectleft') <> 'no';
                ConnectToRight := ItemElem.GetAttribute('connectright') <> 'no';
                try
                  ContextChars[lcIsolated] := StrToInt(ItemElem.GetAttribute('isolated'));
                  ContextChars[lcInitial] := StrToInt(ItemElem.GetAttribute('initial'));
                  ContextChars[lcMedial] := StrToInt(ItemElem.GetAttribute('medial'));
                  ContextChars[lcFinal] := StrToInt(ItemElem.GetAttribute('final'));
                except
                  Clear;
                  raise EArabicEncodingParseError.CreateFmt('Invalid context format (number expected) for %s', [AlphaName]);
                end;
              end;
            end;
          end;
        end;
      end else if TypeElem.NodeName = 'keymap' then begin
        New(AraKeyMap);
        FillChar(AraKeyMap^, SizeOf(AraKeyMap^), 0);
        try
          FKeyboardMaps.AddObject(TypeElem.GetAttribute('name'), TObject(AraKeyMap));
        except
          ShowMessage('Duplicate keyboard map found: %s' + TypeElem.GetAttribute('encoding'));
          continue;
        end;
        for C := 0 to TypeElem.ChildNodes.Length-1 do begin
          ItemNode := TypeElem.ChildNodes.Item(C) as TXmlNode;
          if ItemNode.NodeType = ELEMENT_NODE then begin
            ItemElem := ItemNode as TXmlElement;
            if ItemElem.NodeName = 'key' then begin
              ChStr := ItemElem.GetAttribute('char');
              if Length(ChStr) <> 1 then begin
                Clear;
                raise EArabicEncodingParseError.CreateFmt('Single character expected for key map %s', [ChStr]);
              end;
              AraKeyMap^[ChStr[1]] := AraAlphaNameToId(ItemElem.GetAttribute('letter'));
              if AraKeyMap^[ChStr[1]] = aaUnknown then begin
                Clear;
                raise EArabicEncodingParseError.CreateFmt('Key map %s letter not found', [ChStr]);
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  if FCharMaps.Count > 0 then
    SetActiveCharMapName(FCharMaps[0]);
  if FKeyboardMaps.Count > 0 then
    SetActiveKeyboardMapName(FKeyboardMaps[0]);
end;

function TArabicEncodings.GetLetterFontChar(const AAraAlphaNum : Byte; const AShape : TAraLetterContext) : Char;
begin
  if FActiveCharMap = Nil then
    Result := Char(0)
  else begin
    if AAraAlphaNum <= Byte(Ord(High(TAraAlphaId))) then
      Result := Char(FActiveCharMap^[TAraAlphaId(AAraAlphaNum)].ContextChars[AShape])
    else
      Result := Char(AAraAlphaNum);
  end;
end;

function TArabicEncodings.ToArabic(const AText : String) : String;
const
  WordDelims = ' ';
var
  I, AW, ArabicWords, First, Last : Integer;
  ArabicWord, TranslatedWord : String;
  AraLetter, PrevLetter : TAraAlphaId;
  LetterForm : TAraLetterContext;
begin
  Result := '';
  if AText = '' then
    Exit;

  if FActiveKeyboardMap = Nil then begin
    Result := 'No active keyboard map '+FActiveKeyboardMapName;
    Exit;
  end;
  if FActiveCharMap = Nil then begin
    Result := 'No active character map '+FActiveCharMapName;
    Exit;
  end;

  ArabicWords := WordCountS(AText, WordDelims);
  PrevLetter := aaUnknown;
  for AW := 1 to ArabicWords do begin
    if AW > 1 then
      Result := Result + ' ';

    ArabicWord := ExtractWordS(AW, AText, WordDelims);
    First := Length(ArabicWord);
    Last := 1;

    TranslatedWord := '';
    for I := First downto Last do begin
      AraLetter := FActiveKeyboardMap^[ArabicWord[I]];

      if (First = Last) then
        LetterForm := lcIsolated
      else if I = First then
        LetterForm := lcInitial
      else begin
        if FActiveCharMap^[PrevLetter].ConnectToRight then begin
          if I = Last then
            LetterForm := lcFinal
          else
            LetterForm := lcMedial;
        end else begin
          if I = Last then
            LetterForm := lcIsolated
          else
            LetterForm := lcInitial;
        end;
      end;

      if ArabicWord[I] in ['0'..'9'] then
        TranslatedWord := ArabicWord[I] + TranslatedWord
      else
        TranslatedWord := Char(FActiveCharMap^[AraLetter].ContextChars[LetterForm]) + TranslatedWord;
      PrevLetter := AraLetter;
    end;

    Result := Result + TranslatedWord;
  end;
end;

end.
