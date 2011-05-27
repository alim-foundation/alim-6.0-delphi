unit IslUtils;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus,
  XmlObjModel;

type
  {- an object dictionary is a TStringList which automatically disposes
     of all child objects when free'd (list is sorted, duplicates are ignored)-}
  TObjDict = class(TStringList)
  protected
    FOwnTheData : Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;

    property OwnTheData : Boolean read FOwnTheData write FOwnTheData;
  end;

  TObjList = class(TList)
  protected
    FOwnTheData : Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; override;

    property OwnTheData : Boolean read FOwnTheData write FOwnTheData;
  end;

  PSearchRec = ^TSearchRec;
  TFileNamesList = class(TStringList)   // key is full path to file, value is PSearchRec
  protected
    function GetFileInfo(Index : Integer) : PSearchRec;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddFile(const AFullPath : String; var ASearchRec : TSearchRec);
    property FileInfo[Index : Integer] : PSearchRec read GetFileInfo;
  end;

//function ForceIntoAppPath(ARelativePath : String) : String;
  {-given a path, force it to be relative to the active application}

procedure DisposeObject(Data : Pointer); far;
  {-the generic DisposeData procedure that can free any object}

function Join(const ASeparator : String; AList : TStringList) : String;
  {-join the given AList elements separated by ASeparator}

function Reverse(const S : String) : String;
  {-reverse the contents of S and return the reverted string}

procedure ClearMenu(const AMenu : TMenu);
  {-clear the menu items in AMenu }

function ColorToHtmlColor(Color : LongInt) : String;
  {-return a TColor as an HTML color string}

function GetChildElem(const AElem : TXmlElement; const AFirst : Boolean) : TXmlElement;
  {-find the first or last child element in AElem}

function GetSiblingElem(const AElem : TXmlElement; const ANext : Boolean) : TXmlElement;
  {-find the next or previous sibling element in AElem}

function GetElemAttr(const ANode : TXmlElement; const AName, ADefault : String) : String; overload;
  {-return an element attribute or the default if not found}

function GetElemAttr(const ANode : TXmlElement; const AName : String; const ADefault : Integer) : Integer; overload;
  {-return an element attribute or the default if not found}

function GetElementText(const AElem : TXmlElement; const ANormalize : Boolean = False) : String;
  {-given an element, get only the text at the immediate child level}

function SplitText(const ASource, ADelim : String; var ALeft, ARight : String) : Boolean;
  {-split the source text into Left and Right and return true if ADelim found}

function DosWildMatch(const APattern, ASource : ShortString) : Boolean;
  { Returns TRUE if pattern defined in "pat" matches string passed in "s". }
  { Allows wild cards '?' (single character) and '*' (zero or more of any  }
  { character) as in DOS.                                                  }

procedure ReadFilesInDirTree(const ADirPath : String; AMask : String; const AList : TFileNamesList);
  {-given a path, read all files that match AMask in that directory and all subdirectories into AList}

function ReadFilesInDirTrees(const ADirPaths : String; AMask : String = '*.*'; const APathsDelim : String = ';') : TFileNamesList;
  {-given a path (or paths separated by ;) get all files in those directories that match AMask
   -caller should free the result}

//function ReadFilesInAppTrees(const ADirPaths : String; AMask : String = '*.*'; const APathsDelim : String = ';') : TFileNamesList;
  {-given a path (or paths separated by ;) get all files in those directories that match AMask
   -all paths in APaths are considered to be relative to the application directory (Exe)
   -caller should free the result}

function FindString(const AMatch : String; const AList : array of String; const ADefault : Integer = -1) : Integer;
  {-given a list of string, find out which one the match string is (-1 if none)}

function GetAppVersionInfo : String;

implementation

uses StStrS, StStrL;

constructor TObjDict.Create;
begin
  inherited Create;
  Sorted := True;
  Duplicates := dupIgnore;
  FOwnTheData := True;
end;

destructor TObjDict.Destroy;
var
  I : Cardinal;
begin
  if (Count > 0) and FOwnTheData then begin
    for I := 0 to Count-1 do
      if Objects[I] <> Nil then begin
        Objects[I].Free;
        Objects[I] := Nil;
      end;
  end;
  inherited Destroy;
end;

procedure TObjDict.Clear;
var
  I : Cardinal;
begin
  if (Count > 0) and FOwnTheData then begin
    for I := 0 to Count-1 do
      if Objects[I] <> Nil then begin
        Objects[I].Free;
        Objects[I] := Nil;
      end;
  end;
  inherited Clear;
end;

procedure TObjDict.Delete(Index: Integer);
begin
  if FOwnTheData and (Objects[Index] <> Nil) then begin
    Objects[Index].Free;
    Objects[Index] := Nil;
  end;
  inherited Delete(Index);
end;

constructor TObjList.Create;
begin
  inherited Create;
  FOwnTheData := True;
end;

destructor TObjList.Destroy;
var
  I : Cardinal;
begin
  if (Count > 0) and FOwnTheData then begin
    for I := 0 to Count-1 do
      if Items[I] <> Nil then begin
        TObject(Items[I]).Free;
        Items[I] := Nil;
      end;
  end;
  inherited Destroy;
end;

procedure TObjList.Clear;
var
  I : Cardinal;
begin
  if (Count > 0) and FOwnTheData then begin
    for I := 0 to Count-1 do
      if Items[I] <> Nil then begin
        TObject(Items[I]).Free;
        Items[I] := Nil;
      end;
  end;
  inherited Clear;
end;

function ForceIntoAppPath(ARelativePath : String) : String;
begin
  Result := AddBackSlashL(JustPathNameL(Application.ExeName))+ARelativePath;
end;

procedure DisposeObject(Data : Pointer); far;
begin
  if (Data <> Nil) then
    TObject(Data).Free;
end;

function Join(const ASeparator : String; AList : TStringList) : String;
var
  I : Integer;
begin
  Result := '';
  if AList.Count > 0 then begin
    for I := 0 to AList.Count-1 do begin
      if I > 0 then
        Result := Result + ASeparator + AList.Strings[I]
      else
        Result := AList.Strings[I];
    end;
  end;
end;

function Reverse(const S : String) : String;
var
  I : Cardinal;
begin
  Result := '';
  for I := Length(S) downto 1 do
    Result := Result + S[I];
end;

procedure ClearMenu(const AMenu : TMenu);
begin
  while AMenu.Items.Count > 0 do
    AMenu.Items.Remove(AMenu.Items[0]);
end;

function ColorToHtmlColor(Color : LongInt) : String;
begin
  Result := '#' + HexBS(GetRValue(Color)) + HexBS(GetGValue(Color)) + HexBS(GetBValue(Color));
end;

function GetChildElem(const AElem : TXmlElement; const AFirst : Boolean) : TXmlElement;
var
  E : TXmlNode;
begin
  if AFirst then begin
    E := AElem.FirstChild;
    while (E <> Nil) and (E.NodeType <> ELEMENT_NODE) do
      E := E.NextSibling;
    Result := TXmlElement(E);
  end else begin
    E := AElem.LastChild;
    while (E <> Nil) and (E.NodeType <> ELEMENT_NODE) do
      E := E.PreviousSibling;
    Result := TXmlElement(E);
  end;
end;

function GetElemAttr(const ANode : TXmlElement; const AName, ADefault : String) : String; overload;
var
  Attr : String;
begin
  Attr := ANode.GetAttribute(AName);
  if Attr = '' then Result := ADefault else Result := Attr;
end;

function GetElemAttr(const ANode : TXmlElement; const AName : String; const ADefault : Integer) : Integer; overload;
var
  Attr : String;
begin
  try
    Attr := GetElemAttr(ANode, AName, IntToStr(ADefault));
    Result := StrToInt(Attr);
  except
    Result := ADefault;
  end;
end;

function GetSiblingElem(const AElem : TXmlElement; const ANext : Boolean) : TXmlElement;
var
  E : TXmlNode;
begin
  if ANext then begin
    E := AElem.NextSibling;
    while (E <> Nil) and (E.NodeType <> ELEMENT_NODE) do
      E := E.NextSibling;
    Result := TXmlElement(E);
  end else begin
    E := AElem.PreviousSibling;
    while (E <> Nil) and (E.NodeType <> ELEMENT_NODE) do
      E := E.PreviousSibling;
    Result := TXmlElement(E);
  end;
end;

function GetElementText(const AElem : TXmlElement; const ANormalize : Boolean) : String;
var
  Node : TXmlNode;
  I, ChildCount : Integer;
begin
  Result := '';
  if AElem = Nil then
    Exit;

  ChildCount := AElem.ChildNodes.Length;
  if ChildCount > 0 then begin
    Dec(ChildCount);
    if ANormalize then begin
      for I := 0 to ChildCount do begin
        Node := AElem.ChildNodes.Item(I);
        if Node.NodeType = TEXT_NODE then
          Result := Result + FilterL(TrimL((Node as TXmlText).Data), #13#10#9) + ' ';
      end;
    end else begin
      for I := 0 to ChildCount do begin
        Node := AElem.ChildNodes.Item(I);
        if Node.NodeType = TEXT_NODE then
          Result := Result + (Node as TXmlText).Data;
      end;
    end;
  end;
end;

function SplitText(const ASource, ADelim : String; var ALeft, ARight : String) : Boolean;
var
  P : Integer;
begin
  P := Pos(ADelim, ASource);
  if P > 0 then begin
    ALeft := Copy(ASource, 1, P-1);
    ARight := Copy(ASource, P+Length(ADelim), Length(ASource));
    Result := True;
  end else
    Result := False;
end;

function Remainder(start : Integer; s : ShortString) : ShortString;
  { returns portion of string to right of index "start" }
begin
  Remainder := Copy(s, start, Length(s) - start + 1);
end {Remainder} ;

function DosWildMatch(const APattern, ASource : ShortString) : Boolean;
var
  Pat, S : String;
  PatLen : Integer;
  { Returns TRUE if pattern defined in "pat" matches string passed in "s". }
  { Allows wild cards '?' (single character) and '*' (zero or more of any  }
  { character) as in DOS.                                                  }

  function Match(pIndex, sIndex : Integer) : Boolean;
    { Used to handle recursive match process.  Engineered to take up as   }
    { little stack space as possible.                                     }
  var i : Integer;
  begin
    if (pIndex = Length(pat) + 1) and (sIndex = Length(s) + 1) then Match := True
    else if Remainder(pIndex, pat) = '*' then Match := True
    else if (pIndex > Length(pat)) or (sIndex > Length(s)) then Match := False
    else
      case pat[pIndex] of
        '?' : Match := Match(pIndex + 1, sIndex + 1);
        '*' : begin
                for i := sIndex to Length(s) do
                  if Match(pIndex + 1, i) then
                  begin
                    Match := True;
                    Exit;
                  end;
                Match := False;
              end;
      else Match := (pat[pIndex] = s[sIndex]) and Match(pIndex + 1, sIndex + 1);
      end;
  end {Match} ;

begin
  PatLen := Length(APattern);
  if PatLen = 0 then begin
    DosWildMatch := True;
    Exit;
  end;

  Pat := AnsiUpperCase(APattern);
  S := AnsiUpperCase(ASource);

  DosWildMatch := Match(1, 1);
end {Dos Wild Match} ;

constructor TFileNamesList.Create;
begin
  inherited Create;
  Sorted := True;
end;

destructor TFileNamesList.Destroy;
var
  I : Integer;
begin
  if Count > 0 then
  for I := 0 to Count-1 do
    Dispose(PSearchRec(Objects[I]));
  inherited Destroy;
end;

function TFileNamesList.GetFileInfo(Index : Integer) : PSearchRec;
begin
  Result := PSearchRec(Objects[Index]);
end;

procedure TFileNamesList.AddFile(const AFullPath : String; var ASearchRec : TSearchRec);
var
  PRec : PSearchRec;
begin
  New(PRec);
  PRec^ := ASearchRec;
  AddObject(AFullPath, TObject(PRec));
end;

procedure ReadFilesInDirTree(const ADirPath : String; AMask : String; const AList : TFileNamesList);
var
  FRes : Integer;
  FRec : TSearchRec;
  PathWithSlash : String;
begin
  PathWithSlash := AddBackSlashL(ADirPath);
  FRes := FindFirst(PathWithSlash + '*.*', faAnyFile, FRec);
  if FRes <> 0 then begin
    FindClose(FRec);
    Exit;
  end;

  repeat
    if (FRec.Attr and faDirectory = faDirectory) then begin
      if (FRec.Name <> '.') and (FRec.Name <> '..') then
        ReadFilesInDirTree(PathWithSlash + FRec.Name, AMask, AList);
    end else begin
      if DosWildMatch(AMask, FRec.Name) then begin
        AList.AddFile(PathWithSlash + FRec.Name, FRec);
      end;
    end;
    FRes := FindNext(FRec);
  until FRes <> 0;
  FindClose(FRec);
end;

function ReadFilesInDirTrees(const ADirPaths : String; AMask : String; const APathsDelim : String) : TFileNamesList;
var
  PC, P : Integer;
begin
  Result := TFileNamesList.Create;
  PC := WordCountL(ADirPaths, APathsDelim);
  if PC > 1 then begin
    for P := 1 to PC do
      ReadFilesInDirTree(ExtractWordL(P, ADirPaths, APathsDelim), AMask, Result);
  end else
    ReadFilesInDirTree(ADirPaths, AMask, Result);
end;

function ReadFilesInAppTrees(const ADirPaths : String; AMask : String; const APathsDelim : String) : TFileNamesList;
var
  PC, P : Integer;
begin
  Result := TFileNamesList.Create;
  PC := WordCountL(ADirPaths, APathsDelim);
  if PC > 1 then begin
    for P := 1 to PC do
      ReadFilesInDirTree(ForceIntoAppPath(ExtractWordL(P, ADirPaths, APathsDelim)), AMask, Result);
  end else
    ReadFilesInDirTree(ForceIntoAppPath(ADirPaths), AMask, Result);
end;

function FindString(const AMatch : String; const AList : array of String; const ADefault : Integer) : Integer;
var
  I : Integer;
begin
  Result := ADefault;
  if (AMatch = '') or (Length(AList) <= 0) then
    Exit;

  for I := 0 to High(AList) do begin
    if CompareText(AMatch, AList[I]) = 0 then begin
      Result := I;
      Exit;
    end;
  end;
end;

function GetAppVersionInfo : String;
const
  vqvFmt    = '\StringFileInfo\%4.4x%4.4x\%s';
var
  VFileDescription      : String;
  VFileVersion          : String;
  VInternalName         : String;
  VOriginalFileName     : String;
  VProductName          : String;
  VProductVersion       : String;

  FInfoSize : longint ;
  FInfo     : pointer;
  FLang     : PInteger;
  vptr      : pchar;
  vlen      : DWord;
  FName     : String;
begin
  Result := '';

  FName := Paramstr(0);
  FInfoSize := GetFileVersionInfoSize(pchar(fname), vlen);
  if FInfoSize > 0 then begin
    GetMem(FInfo, FInfoSize);
    if not GetFileVersionInfo(pchar(fname), vlen, FInfoSize, FInfo) then
      raise Exception.Create('Cannot retrieve Version Information for ' + fname);
    VerQueryValue(FInfo, '\VarFileInfo\Translation', pointer(FLang), vlen);
  end
    else exit;

  try
    if VerQueryValue(FInfo, pchar(Format(vqvFmt, [LoWord(FLang^), HiWord(FLang^),
                    'FileDescription'])), pointer(vptr), vlen)
      then VFileDescription := vptr;
    if VerQueryValue(FInfo, pchar(Format(vqvFmt, [LoWord(FLang^), HiWord(FLang^),
                    'FileVersion'])), pointer(vptr), vlen)
      then VFileVersion := vptr;
    if VerQueryValue(FInfo, pchar(Format(vqvFmt, [LoWord(FLang^), HiWord(FLang^),
                    'InternalName'])), pointer(vptr), vlen)
      then VInternalName := vptr;
    if VerQueryValue(FInfo, pchar(Format(vqvFmt, [LoWord(FLang^), HiWord(FLang^),
                    'OriginalFileName'])), pointer(vptr), vlen)
      then VOriginalFileName := vptr;
    if VerQueryValue(FInfo, pchar(Format(vqvFmt, [LoWord(FLang^), HiWord(FLang^),
                    'ProductName'])), pointer(vptr), vlen)
      then VProductName := vptr;
    if VerQueryValue(FInfo, pchar(Format(vqvFmt, [LoWord(FLang^), HiWord(FLang^),
                    'ProductVersion'])), pointer(vptr), vlen)
      then VProductVersion := vptr;

  finally
    if FInfoSize > 0 then
      FreeMem(FInfo, FInfoSize);
  end;{ try }

  Result := VFileVersion;
end;

end.
