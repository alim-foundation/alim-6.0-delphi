
{Version 7.01}
{*********************************************************}
{*                     READHTML.PAS                      *}
{*                Copyright (c) 1995-9 by                *}
{*                   L. David Baldwin                    *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$i htmlcons.inc}

{             The Parser
This module contains the parser which reads thru the document.  It divides it
into sections storing the pertinent information in Section objects.  The
document itself is then a TList of section objects.  See the HTMLSubs unit for
the definition of the section objects.

Key Variables:

  Sy:
      An enumerated type which indicates what the current token is.  For
      example, a value of TextSy would indicate a hunk of text, PSy that a <P>
      tag was encountered, etc.
  LCh:
      The next character in the stream to be analyzed.  In mixed case.
  Ch:
      The same character in upper case.
  LCToken:
      A string which is associated with the current token.  If Sy is TextSy,
      then LCToken contains the text.
  Attributes:
      A list of TAttribute's for tokens such as <img>, <a>, which have
      attributes.
  Section:
      The current section being built.
  SectionList:
      The list of sections which form the document.  When in a Table,
      SectionList will contain the list that makes up the current cell.

Key Routines:

  GetCh:
      Gets the next character from the stream.  Fills Ch and LCh.  Skips
      comments.
  Next:
      Gets the next token.  Fills Sy, LCToken, Attributes.  Calls GetCh so the
      next character after the present token is available.  Each part of the
      parser is responsible for calling Next after it does its thing.
}

unit Readhtml;

interface
uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Dialogs, StdCtrls, HTMLUn2;

type
  LoadStyleType = (lsFile, lsBuffer, lsStrings, lsStream, lsInclude);
  TIncludeType = procedure(Sender: TObject; const Command: string;
                    Params: TStrings; var Buffer: PChar; var BuffSize: LongInt) of Object;
  TSoundType = procedure(Sender: TObject; const SRC: string; Loop: integer;
                    Terminate: boolean) of Object;
  TMetaType = procedure(Sender: TObject; const HttpEq, Name, Content: string) of Object;

  TFrameViewerBase = class(TWinControl)
    protected
    FOnInclude: TIncludeType;
    FOnSoundRequest: TSoundType;
    FOnScript: TScriptEvent;

    procedure AddFrame(FrameSet: TObject; Attr: TAttributeList; const FName: string); virtual; abstract;
    function CreateSubFrameSet(FrameSet: TObject): TObject; virtual; abstract;
    procedure DoAttributes(FrameSet: TObject; Attr: TAttributeList); virtual; abstract;
    procedure EndFrameSet(FrameSet: TObject); virtual; abstract;
    end;

const
  FontConv: array[1..7] of integer = (8,10,12,14,18,24,36);
  PreFontConv:  array[1..7] of integer = (7,8,10,12,15,20,30);

var
  Title: string;
  Base: string;
  BaseTarget: string;

procedure HtmlParseFile(ASectionList: TList; ANameList: TStringList;
                   const FName : string; AIncludeEvent: TIncludeType;
                   ASoundEvent: TSoundType; AMetaEvent: TMetaType);   
procedure HtmlParseStrings(ASectionList: TList; ANameList: TStringList;
                   Strings : TStrings; AIncludeEvent: TIncludeType;
                   ASoundEvent: TSoundType; AMetaEvent: TMetaType);    
procedure HtmlParseBuffer(ASectionList: TList; ANameList: TStringList;
                   ABuffer : PChar; BuffSize: LongInt; AIncludeEvent: TIncludeType;
                   ASoundEvent: TSoundType; AMetaEvent: TMetaType);    
procedure HtmlParseStream(ASectionList: TList; ANameList: TStringList;
                   AStream: TStream; AIncludeEvent: TIncludeType;
                   ASoundEvent: TSoundType; AMetaEvent: TMetaType);    
function GetColor(S: String; Var Value: TColor): boolean;
procedure HtmlParseTextFile(ASectionList: TList; ANameList: TStringList;
                   const FName : string);
procedure HtmlParseTextStrings(ASectionList: TList; ANameList: TStringList;
                   Strings : TStrings);
procedure HtmlParseTextStream(ASectionList: TList; ANameList: TStringList;
                   AStream: TStream);

function FrameParseFile(FrameViewer: TFrameViewerBase; FrameSet: TObject;  ALoadStyle: LoadStyleType;
          const FName : string; Strings : TStrings; AStream: TStream;
          ABuffer : PChar; BuffSize: LongInt; AMetaEvent: TMetaType): boolean;
function IsFrameFile(ALoadStyle: LoadStyleType;
          const FName : string; Strings : TStrings; AStream: TStream;
          ABuffer : PChar; BuffSize: LongInt; FrameViewer: TObject): boolean;
{$ifdef Delphi3_4_CppBuilder3_4}
function TranslateCharset(DefCharset: TFontCharset; const Content: string): TFontCharset;
{$endif}

implementation

uses
  htmlsubs, htmlsbs1, htmlview;    

Const
  Tokenleng = 150;
  Tab = #9;
  EofChar = ^Z;
  MaxStack = 25;

var
  Section : TSection;
  SectionList: TCell;
  MasterList: TSectionList;
  NameList: TStringList;  {a list of the <a Name= > attributes along with the
                           section where found}
  FontStack: array[1..MaxStack] of TMyFont; {handy way of keeping track of past
                                           fonts}
  StackIndex: integer;
  CurrentURLTarget: TURLTarget;
  InHref: boolean;
  Attributes : TAttributeList;
  Sy : Symb;
  PreFormat: boolean;    {set when doing preformat <pre> text}
  Justify: JustifyType;
  BaseFontSize: integer;
  InScript: boolean;     {when in a <SCRIPT>}
  NoPSpace: boolean;      {start a new cell (to avoid paragraph space on <p>}

type
  SymString = string[12];

Const
  MaxRes = 65;
  MaxEndRes = 48;
  ResWords : array[1..MaxRes] of SymString =
     ('HTML', 'TITLE', 'BODY', 'HEAD', 'B', 'I', 'H', 'EM', 'STRONG',
      'U', 'CITE', 'VAR', 'TT', 'CODE', 'KBD', 'SAMP', 'OL', 'UL', 'DIR',
      'MENU', 'DL',
      'A', 'ADDRESS', 'BLOCKQUOTE', 'PRE', 'CENTER', 'TABLE', 'TD', 'TH',
      'CAPTION', 'FORM', 'TEXTAREA', 'SELECT', 'OPTION', 'FONT', 'SUB', 'SUP',
      'BIG', 'SMALL', 'P', 'MAP', 'FRAMESET', 'NOFRAMES', 'SCRIPT', 'DIV',
      'S', 'STRIKE', 'TR',

      'LI', 'BR', 'HR', 'DD', 'DT', 'IMG', 'BASE', 'BUTTON','INPUT',
      'SELECTED', 'BASEFONT', 'AREA', 'FRAME', 'PAGE', 'BGSOUND', 'WRAP',
      'META');   

  ResSy : array[1..MaxRes] of Symb =
     (htmlSy, TitleSy, BodySy, HeadSy, BSy, ISy, HeadingSy, EmSy, StrongSy,
      USy, CiteSy, VarSy, TTSy, CodeSy, KbdSy, SampSy, OLSy, ULSy, DirSy,
      MenuSy, DLSy, ASy, AddressSy, BlockQuoteSy, PreSy, CenterSy,TableSy,
      TDsy, THSy, CaptionSy, FormSy, TextAreaSy,  SelectSy, OptionSy, FontSy,
      SubSy, SupSy, BigSy, SmallSy, PSy, MapSy, FrameSetSy, NoFramesSy,
      ScriptSy, DivSy, SSy, StrikeSy, TRSy,

      LISy, BRSy, HRSy, DDSy, DTSy, ImageSy, BaseSy, ButtonSy,
      InputSy, SelectedSy, BaseFontSy, AreaSy, FrameSy, PageSy, BgSoundSy,
      WrapSy, MetaSy);

  {keep these in order with those above}
  EndResSy : array[1..MaxEndRes] of Symb =
     (HtmlEndSy, TitleEndSy, BodyEndSy, HeadEndSy, BEndSy, IEndSy, HeadingEndSy,
      EmEndSy, StrongEndSy, UEndSy, CiteEndSy, VarEndSy, TTEndSy, CodeEndSy,
      KbdEndSy, SampEndSy,
      OLEndSy, ULEndSy, DirEndSy, MenuEndSy, DLEndSy, AEndSy, AddressEndSy,
      BlockQuoteEndSy, PreEndSy, CenterEndSy, TableEndSy, TDEndSy, THEndSy,
      CaptionEndSy, FormEndSy, TextAreaEndSy, SelectEndSy, OptionEndSy, FontEndSy,
      SubEndSy, SupEndSy, BigEndSy, SmallEndSy, PEndSy, MapEndSy, FrameSetEndSy,
      NoFramesEndSy, ScriptEndSy, DivEndSy, SEndSy, StrikeEndSy, TREndSy);

  TextBuffSize = 2000;

Type
  EParseError = class(Exception);
  TokenString = string[Tokenleng];
Var
  LCh, Ch, Back: Char;
  Inf : TextFile;
  InBuff : PChar;
  Value : LongInt;
  LCToken : TokenString;
  St: string;
  Chi, LineNo : integer;
  TheStrings: TStrings;
  LoadStyle: LoadStyleType;
  Buffer, BuffEnd: PChar;
  Stream: TStream;

  IBuff, IBuffEnd: PChar;
  IncludeEvent: TIncludeType;
  CallingObject: TObject;
  SaveLoadStyle: LoadStyleType;
  SoundEvent: TSoundType;
  MetaEvent: TMetaType;
  
function GetAttribute(var Sym: Symb; var St, S: OpenString; var Val: LongInt): boolean; forward;

{-------------GetCh}
PROCEDURE GetCh;
{Return next char in Lch, its uppercase value in Ch.  Ignore comments}
var
  NextCh: char;
  Comment, Done: boolean;

  function ReadChar: char;
  var
    Cnt: integer;
  begin
  case LoadStyle of
    lsStrings:
       begin                {read from TheStrings}
       if Chi > Length(St) then
         begin
         Chi := 1;    {read a new line}
         if LineNo < TheStrings.Count then
           begin
           St := TheStrings[LineNo];
           Inc(LineNo);
           AppendStr(St, ^M);
           end
         else St := EofChar;
         end;
       Result := St[Chi];
       Inc(Chi);
       end;
    lsBuffer:
       begin
       if Buffer < BuffEnd then
         begin
         Result := Buffer^;
         Inc(Buffer);
         end
       else Result := EOFChar;
       end;
    lsStream:
       begin                {read from stream}
       if Chi > Length(St) then
         begin
         Chi := 1;    {read a new line}
         SetLength(St, 250);
         Cnt := Stream.Read(St[1], 250);
         SetLength(St, Cnt);
         if Cnt < 250 then
           St := St + EOFChar;
         end;
       Result := St[Chi];
       Inc(Chi);
       end;

    lsFile:
       Read(Inf, Result);    {read from a file}

    lsInclude:
       if IBuff < IBuffEnd then
         begin
         Result := IBuff^;
         Inc(IBuff);
         end
       else
         begin
         LoadStyle := SaveLoadStyle;
         Result := ReadChar;
         end;
    else Result := #0;       {to prevent warning msg}
    end;
  end;

  procedure GetchBasic; {read a character}
  var
    C: char;
  begin
  if Boolean(Back) then
    begin
    LCh := Back;  {ressurect the char put back}
    Back := #0;
    end
  else LCh := ReadChar;

  if LCh in [^M, ^J] then
    begin  {eat a ^J after a ^M or a ^M after a ^J}
    C := ReadChar;
    case LCh of
      ^M: if C <> ^J then Back := C;
      ^J: begin
          if C <> ^M then Back := C;
          LCh := ^M;    {^M indicates end of line hereafter}
          end;
      end;
    end
  else if LCh = Tab then
    LCh := ' ';
  Ch := UpCase(LCh);
  if (LCh = EofChar) and Comment then
    Raise EParseError.Create('Open Comment at End of HTML File');
  end;

  procedure DoDashDash;   {do the comment after a <!-- }
  begin
  repeat
    while Ch <> '-' do GetChBasic; {get first '-'}
    GetChBasic;
    if Ch = '-' then  {second '-'}
      begin
      while Ch = '-' do GetChBasic;  {any number of '-'}
      while (Ch = ' ') or (Ch = ^M) do GetChBasic;  {eat white space}
      Done := Ch = '>';
      end
    else Done := False;
  until Done;
  end;

  procedure ReadToGT;    {read to the next '>' }
  begin
  while Ch <> '>' do
    GetChBasic;
  end;

  procedure DoInclude;    
  var
    S, SymStr, AttrStr: string;
    L: LongInt;
    Sym: Symb;
    BuffSize: LongInt;
    SL: TStringList;
  begin
  S := '';
  GetChBasic;
  while Ch in ['A'..'Z'] do
    begin
    S := S + LCh;
    GetChBasic;
    end;
  SL := TStringList.Create;
  while GetAttribute(Sym, SymStr, AttrStr, L) do
    SL.Add(SymStr+'="'+AttrStr+'"');
  DoDashDash;
  IncludeEvent(CallingObject, S, SL, IBuff, BuffSize);
  if BuffSize > 0 then
    begin
    SaveLoadStyle := LoadStyle;
    LoadStyle := lsInclude;
    IBuffEnd := IBuff+BuffSize;
    end;
  end;

begin  {Getch}
repeat    {in case a comment immediately follows another comment}
   {comments may be either '<! stuff >' or '<!-- stuff -->'  }
  Comment := False;
  GetchBasic;
  if (Ch = '<') and not InScript then
    begin
    NextCh := ReadChar;
    if NextCh = '!' then
      begin
      Comment:=True;
      GetChBasic;
      if Ch = '-' then
        begin
        GetChBasic;
        if Ch = '-' then
          begin
          GetChBasic;
          if Assigned(IncludeEvent) and (Ch = '#') and (LoadStyle <> lsInclude) then
            DoInclude
          else
            DoDashDash;  {a <!-- comment}
          end
        else ReadToGT;
        end
      else ReadToGT;
      end
    else Back := NextCh; {put character back}
    end;
until not Comment;
end;

{-------------SkipWhiteSpace}
procedure SkipWhiteSpace;
begin
while (LCh in [' ', Tab, ^M]) do
  GetCh;
end;

{----------------GetValue}
function GetValue(var S: string; var Value: LongInt): boolean;
{read a numeric}
var
  Code: integer;
begin
Result := Ch in ['-', '+', '0'..'9'];
if not Result then Exit;
Value := 0;
if Ch in ['-', '+'] then
  begin
  S := Ch;
  GetCh;
  end
else S := '';
while not (Ch in [' ', Tab, ^M, '>', '%', EofChar]) do
  begin
  S := S + Ch;
  GetCh;
  end;
SkipWhiteSpace;
Val(S, Value, Code);    
if Code <> 0 then
  Value := 0;
if LCh = '%' then
  begin
  S := S + '%';
  GetCh; 
  end;
end;

{----------------IsOddChar}
function IsOddChar : boolean;
const
  Lim = 7;
  MaxChars = Lim+96;
  OddCharStr : array[1..MaxChars] of string[6] =
    ('amp', 'lt', 'gt', 'quot', 'rsquo', 'ndash', 'hellip', {Lim special items}
'nbsp',      {no-break space}
'iexcl',     {inverted exclamation mark}
'cent',      {cent sign}
'pound',     {pound sterling sign}
'curren',    {general currency sign}
'yen',       {yen sign}
'brvbar',    {broken (vertical) bar}
'sect',      {section sign}
'uml',       {umlaut (dieresis)}
'copy',      {copyright sign}
'ordf',      {ordinal indicator', feminine}
'laquo',     {angle quotation mark', left}
'not',       {not sign}
'shy',       {soft hyphen}
'reg',       {registered sign}
'macr',      {macron}
'deg',       {degree sign}
'plusmn',    {plus-or-minus sign}
'sup2',      {superscript two}
'sup3',      {superscript three}
'acute',     {acute accent}
'micro',     {micro sign}
'para',      {pilcrow (paragraph sign)}
'middot',    {middle dot}
'cedil',     {cedilla}
'sup1',      {superscript one}
'ordm',      {ordinal indicator', masculine}
'raquo',     {angle quotation mark', right}
'frac14',    {fraction one-quarter}
'frac12',    {fraction one-half}
'frac34',    {fraction three-quarters}
'iquest',    {inverted question mark}

'Agrave',    {Capital A, grave accent}
'Aacute',    {Capital A, acute accent}
'Acirc',     {Capital A, circumflex accent}
'Atilde',    {Capital A, tilde}
'Auml',      {Capital A, dieresis or umlaut mark}
'Aring',     {Capital A, ring}
'AElig',     {Capital AE dipthong (ligature)}
'Ccedil',    {Capital C, cedilla}
'Egrave',    {Capital E, grave accent}
'Eacute',    {Capital E, acute accent}
'Ecirc',     {Capital E, circumflex accent}
'Euml',      {Capital E, dieresis or umlaut mark}
'Igrave',    {Capital I, grave accent}
'Iacute',    {Capital I, acute accent}
'Icirc',     {Capital I, circumflex accent}
'Iuml',      {Capital I, dieresis or umlaut mark}
'ETH',       {Capital Eth, Icelandic}
'Ntilde',    {Capital N, tilde}
'Ograve',    {Capital O, grave accent}
'Oacute',    {Capital O, acute accent}
'Ocirc',     {Capital O, circumflex accent}
'Otilde',    {Capital O, tilde}
'Ouml',      {Capital O, dieresis or umlaut mark}
'times',          {missing}
'Oslash',    {Capital O, slash}
'Ugrave',    {Capital U, grave accent}
'Uacute',    {Capital U, acute accent}
'Ucirc',     {Capital U, circumflex accent}
'Uuml',      {Capital U, dieresis or umlaut mark}
'Yacute',    {Capital Y, acute accent}
'THORN',     {Capital THORN, Icelandic}
'szlig',     {Small sharp s, German (sz ligature)}
'agrave',    {Small a, grave accent}
'aacute',    {Small a, acute accent}
'acirc',     {Small a, circumflex accent}
'atilde',    {Small a, tilde}
'auml',      {Small a, dieresis or umlaut mark}
'aring',     {Small a, ring}
'aelig',     {Small ae dipthong (ligature)}
'ccedil',    {Small c, cedilla}
'egrave',    {Small e, grave accent}
'eacute',    {Small e, acute accent}
'ecirc',     {Small e, circumflex accent}
'euml',      {Small e, dieresis or umlaut mark}
'igrave',    {Small i, grave accent}
'iacute',    {Small i, acute accent}
'icirc',     {Small i, circumflex accent}
'iuml',      {Small i, dieresis or umlaut mark}
'eth',       {Small eth, Icelandic}
'ntilde',    {Small n, tilde}
'ograve',    {Small o, grave accent}
'oacute',    {Small o, acute accent}
'ocirc',     {Small o, circumflex accent}
'otilde',    {Small o, tilde}
'ouml',      {Small o, dieresis or umlaut mark}
'divide',          {missing}
'oslash',    {Small o, slash}
'ugrave',    {Small u, grave accent}
'uacute',    {Small u, acute accent}
'ucirc',     {Small u, circumflex accent}
'uuml',      {Small u, dieresis or umlaut mark}
'yacute',    {Small y, acute accent}
'thorn',     {Small thorn, Icelandic}
'yuml');     {Small y, dieresis or umlaut mark}


  OddChar : array[1..Lim] of string[3] =
    ('&', '<', '>', '"', '''', '-', '...');
var
  I, J, N: integer;
  S: string[8];
  Term: char;
begin
if Ch <> '&' then
  begin
  Result := False;
  Exit;
  end;
Result := True;
Sy := TextSy;
I := 0;
S := '';
GetCh;
if Ch = '#' then
  begin   {look for char #}
  GetCh;
  while I <=5 do
    begin
    if not (Ch in ['0'..'9']) then
      begin
      Term := Ch;
      if Term = ';' then GetCh;
      if S <> '' then
        begin
        N := StrToInt(S);
        if (N <= 255) and (byte(N) in [9, 10, 32..255]) then
          begin
          if N = 9 then LCToken := ' '          
          else LCToken := chr(N);
          Exit;
          end;
        end;
      LCToken := '&#'+S;
      if Term = ';' then LCToken := LCToken + ';';
      Exit;
      end;
    S := S+LCh;
    Inc(I);
    GetCh;
    end;
  LCToken := '&#'+S;
  Exit;
  end
else while I <= 6 do
  begin
  if not (Ch in ['A'..'Z', '0'..'9']) then  {note: Ch is always upper case} 
    begin
    Term := Ch;
    if Ch = ';' then GetCh;
    for  J := 1 to MaxChars do
      if S = OddCharStr[J] then
        begin
        if J <= Lim then
          LCToken := OddChar[J]
        else LCToken := chr(J - Lim + 159);
        Exit;
        end;
    LCToken := '&'+S;  {S doesn't match, assume we have just text}
    if Term = ';' then LCToken := LCToken + ';';
    Exit
    end;
  S := S+LCh;
  Inc(I);
  GetCh;
  end;
{too many chars, assume it's just text}
LCToken := '&'+S;
end;

{----------------GetQuotedStr}
function GetQuotedStr(var S: OpenString; var Value: LongInt): boolean;
{get a quoted string but strip the quotes, check to see if it is numerical}
var
  Term: char;
  S1: string;
  Code: integer;
  SaveSy: Symb;
begin
Result := False;
Term := Ch;
if (Term <> '"') and (Term <> '''') then Exit;
Result := True;
SaveSy := Sy;
GetCh;
while not (Ch in [Term, EofChar]) do
  begin
  if LCh <> ^M then
    begin
    if IsOddChar then
      begin
      {$ifdef Windows}
      LCToken[0] := Chr(IntMin(Length(LCToken), High(S)-Length(S)));
      {$endif}
      S := S + LCToken;
      end
    else
      begin
      {$ifdef Windows}
      if Length(S) < High(S) then
      {$endif}
         S := S + LCh;
      GetCh;
      end;
    end
  else GetCh;   {pass ^M by}
  end;
if Ch = Term then GetCh;  {pass termination char}
S := Trim(S);
S1 := S;
if Pos('%', S1) = Length(S1) then SetLength(S1, Length(S1)-1);
Val(S1, Value, Code);
if Code <> 0 then Value := 0;
Sy := SaveSy;
end;

{----------------GetSomething}
procedure GetSomething(var S: OpenString);

begin
while not (Ch in [' ', Tab, ^M, '>', EofChar]) do
  begin
  {$ifdef Windows}
  if Length(S) < High(S) then
  {$endif}
    S := S+LCh;
  GetCh;
  end;
end;

{----------------GetID}
function GetID(var S: OpenString): boolean;

begin
Result := False;
if not (Ch in ['A'..'Z']) then Exit;
while Ch in ['A'..'Z', '-'] do
  begin
  S := S+Ch;
  GetCh;
  end;
Result := True;
end;

{----------------GetAttribute}
function GetAttribute(var Sym: Symb; var St, S: OpenString; var Val: LongInt): boolean;

const
  MaxAttr = 61;
  Attrib : array[1..MaxAttr] of string[12] =
     ('HREF', 'NAME', 'SRC', 'ALT', 'ALIGN', 'TEXT', 'BGCOLOR', 'LINK',
      'BACKGROUND', 'COLSPAN', 'ROWSPAN', 'BORDER', 'CELLPADDING',
      'CELLSPACING', 'VALIGN', 'WIDTH', 'START', 'VALUE', 'TYPE',
      'CHECKBOX', 'RADIO', 'METHOD', 'ACTION', 'CHECKED', 'SIZE',
      'MAXLENGTH', 'COLS', 'ROWS', 'MULTIPLE', 'VALUE', 'SELECTED',
      'FACE', 'COLOR', 'TRANSP', 'CLEAR', 'ISMAP', 'BORDERCOLOR',
      'USEMAP', 'SHAPE', 'COORDS', 'NOHREF', 'HEIGHT', 'PLAIN', 'TARGET',
      'NORESIZE', 'SCROLLING', 'HSPACE', 'LANGUAGE', 'FRAMEBORDER',
      'MARGINWIDTH', 'MARGINHEIGHT', 'LOOP', 'ONCLICK', 'WRAP', 'NOSHADE',
      'HTTP-EQUIV', 'CONTENT', 'ENCTYPE', 'VLINK', 'OLINK', 'ACTIVE'); 
  AttribSym: array[1..MaxAttr] of Symb =
     (HrefSy, NameSy, SrcSy, AltSy, AlignSy, TextSy, BGColorSy, LinkSy,
      BackgroundSy, ColSpanSy, RowSpanSy, BorderSy, CellPaddingSy,
      CellSpacingSy, VAlignSy, WidthSy, StartSy, ValueSy, TypeSy,
      CheckBoxSy, RadioSy, MethodSy, ActionSy, CheckedSy, SizeSy,
      MaxLengthSy, ColsSy, RowsSy, MultipleSy, ValueSy, SelectedSy,
      FaceSy, ColorSy, TranspSy, ClearSy, IsMapSy, BorderColorSy,
      UseMapSy, ShapeSy, CoordsSy, NoHrefSy, HeightSy, PlainSy, TargetSy,
      NoResizeSy, ScrollingSy, HSpaceSy, LanguageSy, FrameBorderSy,
      MarginWidthSy, MarginHeightSy, LoopSy, OnClickSy, WrapSy, NoShadeSy,
      HttpEqSy, ContentSy, EncTypeSy, VLinkSy, OLinkSy, ActiveSy);
var
  I: integer;
begin
Sym := OtherAttribute;
Result := False;
SkipWhiteSpace;
St := '';
if GetID(St) then
  begin
  for I := 1 to MaxAttr do
    if St = Attrib[I] then
      begin
      Sym := AttribSym[I];
      Break;
      end;
  end
else Exit;   {no ID}
SkipWhiteSpace;
S := '';
if Sym = BorderSy then Val := 1 else Val := 0;
Result := True;      {at least have an ID}
if Ch <> '=' then Exit;
GetCh;

SkipWhiteSpace;
if not GetQuotedStr(S, Val) then   {either it's a quoted string or a number}
if not GetValue(S, Val) then
  GetSomething(S);    {in case quotes left off string}
end;

{-------------GetTag}
function GetTag: boolean;  {Pick up a Tag or pass a single '<'} 
Var
  Done, EndTag : Boolean;
  Compare: TokenString;
  SymStr, AttrStr: string;  
  I: Integer;
  L: LongInt;
  Sym: Symb;
begin
if Ch <> '<' then  
  begin
  Result := False;
  Exit;
  end
else Result := True;
Compare := '';
GetCh;
if Ch = '/' then
  begin
  EndTag := True;
  GetCh;
  end
else if not (Ch in ['A'..'Z']) then   
  begin     {an odd '<'}
  Sy := TextSy;
  LCToken := '<';
  Exit;
  end
else
  EndTag := False;
Sy := CommandSy;
Done := False;
while not Done do
  case Ch of
    'A'..'Z' :
          begin
          if Length(Compare) < Tokenleng then
            begin
            Inc(Compare[0]);
            Compare[Length(Compare)] := Ch;
            end;
          GetCh;
          end;
    else Done := True;
   end;
for I := 1 to MaxRes do
  if Compare = ResWords[I] then
    begin
    if not EndTag then
      Sy := ResSy[I]
    else
      if I <= MaxEndRes then
        Sy := EndResSy[I];   {else Sy  := CommandSy}
    Break;
    end;
SkipWhiteSpace;
Value := 0;
if ((Sy = HeadingSy) or (Sy = HeadingEndSy)) and (Ch in ['1'..'6']) then
  begin
  Value := ord(Ch)-ord('0');
  GetCh;
  end;

if Sy in [ImageSy, BaseSy, ASy, BodySy, TDSy, THSy, TRSy, TableSy,
      CaptionSy, OLSy, ULSy, InputSy, FormSy, TextAreaSy, SelectSy, OptionSy,
      PSy, FontSy, BaseFontSy, BRSy, HeadingSy, MapSy, AreaSy,
      HRSy, FrameSy, FrameSetSy, NoResizeSy, ScrollingSy, ScriptSy, DivSy,
      BgSoundSy, WrapSy, MetaSy] then
  Attributes.Clear;
while GetAttribute(Sym, SymStr, AttrStr, L) do
  if Sy in [ImageSy, BaseSy, ASy, BodySy, TDSy, THSy, TRSy, TableSy,
     CaptionSy, OLSy, ULSy, InputSy, FormSy, TextAreaSy, SelectSy, OptionSy,
     PSy, FontSy, BaseFontSy, BRSy, HeadingSy, MapSy, AreaSy,
     HRSy, FrameSy, FrameSetSy, NoResizeSy, ScrollingSy, ScriptSy, DivSy,
     BgSoundSy, WrapSy, MetaSy] then
    Attributes.Add(TAttribute.Create(Sym, L, AttrStr));
       {else ignore (but pass over) other attributes}

while (Ch <> '>') and (Ch <> EofChar) do GetCh;
GetCh;
end;

{----------------IsText}
function IsText : boolean;
begin
LCToken := '';
while (Length(LCToken) < 100) and
      (LCh in [^M, ' '..'%', ''''..';', '=', '?'..#255, '>']) do  
  if not PreFormat and ((LCh = ' ') or (LCh = ^M)) then
    begin
    LCToken := LCToken + ' ';  {^M becomes space}
    SkipWhiteSpace;      {eliminate multiple spaces}
    end
  else
    begin
    LCToken := LCToken+LCh;
    GetCh;
    end;
if Length(LCToken) > 0 then
  begin
  Sy := TextSy;
  IsText := True;
  end
else IsText := False;
end;

{-----------Next}
PROCEDURE Next;
  {Get the next token}
begin  {already have fresh character loaded here}
LCToken := '';
if LCh = EofChar then Sy := EofSy
else if GetTag then     {handles '<'}  
else if IsOddChar then
else if IsText then
else
  begin
  Sy := OtherChar;
  LCToken := LCh;
  GetCh;
  end;
if NoPSpace and (Sy in [TextSy, ImageSy, TableSy, InputSy, SelectSy, TextAreaSy,
     BrSy, HeadingSy, HRSy]) then NoPSpace := False;   
end;

function PushNewFont: boolean;
{add a font to the font stack identical to the last one}
begin
if StackIndex < MaxStack then
  begin
  Inc(StackIndex);
  FontStack[StackIndex] := TMyFont.Create;
  FontStack[StackIndex].Assign(FontStack[StackIndex-1]);
  Result := True;
  end
else Result := False;
end;

procedure PopFont;
{pop and free a font from the font stack}
begin
if StackIndex > 1 then
  begin
  FontStack[StackIndex].Free;
  Dec(StackIndex);
  end;
end;

procedure DoTextArea(TxtArea: TTextAreaFormControlObj);
{read and save the text for a TextArea form control}
var
  S: string;

  procedure Next1;
    {Special Next routine to get the next token}
    procedure GetTag1;  {simplified Pick up a Tag routine}
    begin
    LCToken := '<';
    GetCh;
    Sy := CommandSy;
    while not (LCh in [' ', ^M, Tab, '>']) do
      begin
      LCToken := LCToken + LCh;
      GetCh;
      end;
    if CompareText(LCToken, '</textarea') = 0 then
      Sy := TextAreaEndSy
    else Sy := CommandSy;    {anything else}
    end;

    function IsText1 : boolean;
    begin
    while (Length(LCToken) < 100) and  not (LCh in [^M, '<', '&', EofChar]) do  
      begin
      LCToken := LCToken+LCh;
      GetCh;
      end;
    if Length(LCToken) > 0 then
      begin
      Sy := TextSy;
      IsText1 := True;
      end
    else IsText1 := False;
    end;

  begin  {already have fresh character loaded here}
  LCToken := '';
  if LCh = EofChar then Sy := EofSy
  else if LCh = ^M then
    begin
    Sy := EolSy;
    GetCh;
    end
  else if LCh = '<' then
     begin GetTag1;  Exit;  end
  else if IsOddChar then      
  else if IsText1 then
  else
    begin
    Sy := OtherChar;
    LCToken := LCh;
    GetCh;
    end;
  end;

begin
Next1;
S := '';
while (Sy <> TextAreaEndSy) and (Sy <> EofSy) do
  begin
  case Sy of
    TextSy: S := S + LCToken;
    EolSy:
      begin
      TxtArea.AddStr(S);
      S := '';
      end;
    else
      S := S + LCToken;
    end;
  Next1;
  end;
while not (LCh in ['>', EofChar]) do
  GetCh; {remove chars to and past '>'}
GetCh;
if S <> '' then TxtArea.AddStr(S);
TxtArea.ResetToValue;
end;

function FindAlignment: JustifyType;  {pick up Align= attribute}
var
  UpName: string[10];
  T: TAttribute;
begin
Result := Justify;
if Sy = PEndSy then Exit;
if Attributes.Find(AlignSy, T) then
  begin
  UpName := Lowercase(T.Name^);
  if UpName = 'left' then Result := Left
  else if UpName = 'center' then Result := Centered
  else if UpName = 'right' then Result := Right;
  end;
end;

type
  SymbSet = Set of Symb;
const
  TableTermSet = [TableEndSy, TDSy, TRSy, TREndSy, THSy, THEndSy, TDEndSy,
                  CaptionSy, CaptionEndSy];

procedure DoBody(Level: integer; const TermSet: SymbSet); forward;

procedure DoAEnd;  {do the </a>}
begin
if InHref then   {see if we're in an href}
  begin
  CurrentUrlTarget.Clear;
  InHref := False;
  PopFont;   {the blue stuff}
  end;
if Assigned(Section) then
  Section.HRef(AEndSy, MasterList, Nil, FontStack[StackIndex]);
end;

{----------------DoTable}
procedure DoTable(ALevel: integer);
var
  Table: ThtmlTable;
  SaveSectionList, JunkSaveSectionList: TCell;
  SaveStyle: TFontStyles;
  SaveJustify, RowJustify, TmpJustify: JustifyType;
  RowVAlign, VAlign: AlignmentType;
  Row: TCellList;
  CellObj: TCellObj;
  T: TAttribute;
  SaveIndex: integer;

  function GetJustify: JustifyType;
  var
    S: string[9];
    T: TAttribute;
  begin
  Result := NoJustify;
  if Attributes.Find(AlignSy, T) then
    begin
    S := LowerCase(T.Name^);
    if S = 'left' then Result := Left
    else if S = 'center' then Result := Centered
    else if S = 'right' then Result := Right;
    end;
  end;

  function GetVAlign(Default: AlignmentType): AlignmentType;
  var
    S: string[9];
    T: TAttribute;
  begin
  Result := Default;
  if Attributes.Find(VAlignSy, T) then
    begin
    S := LowerCase(T.Name^);
    if (S = 'top') or (S = 'baseline') then Result := Top
    else if S = 'middle' then Result := Middle
    else if S = 'bottom' then Result := Bottom;
    end;
  end;

  procedure AddSection;
  begin
  if Assigned(SectionList) then
    begin
    SectionList.Add(Section);
    Section := Nil;
    if CellObj.Cell = SectionList then
      Row.Add(CellObj)
    else SectionList.Free;   {won't happen}
    SectionList := Nil;
    end;
  end;

begin
if InHref then DoAEnd;    {terminate <a>}
SaveIndex := StackIndex;
SectionList.Add(Section);
Section := Nil;
SaveSectionList := SectionList;
SaveJustify := Justify;
SaveStyle := CurrentStyle;
SectionList := Nil;
Table := ThtmlTable.Create(MasterList, Attributes, Justify, SaveSectionList, ALevel);
Row := Nil;
RowJustify := NoJustify;
RowVAlign := Middle;
Next;
while (Sy <> TableEndSy) and (Sy <> EofSy) do
  case Sy of
    TDSy, THSy:
      Begin
      if InHref then DoAEnd;    
      while StackIndex > SaveIndex do
        PopFont;          {terminate any <font> introduced in table}
      CurrentStyle := SaveStyle;  
      if not Assigned(Row) then   {in case <tr> is missing}
        begin
        Row := TCellList.Create;
        RowJustify := NoJustify;
        RowVAlign := Middle;
        end
      else AddSection;
      if Sy = THSy then
        begin
        if RowJustify = NoJustify then
          Justify := Centered   {default <TH> is centered}
        else Justify := RowJustify;
        CurrentStyle := CurrentStyle + [fsBold];
        end
      else
        begin
        if RowJustify = NoJustify then
          Justify := Left   {default <TD> is Left}
        else Justify := RowJustify;
        CurrentStyle := CurrentStyle - [fsBold];
        end;
      TmpJustify := GetJustify;     {see if there is Align override}
      if TmpJustify <> NoJustify then
        Justify := TmpJustify;
      VAlign := GetVAlign(RowVAlign);
      CellObj := TCellObj.Create(MasterList, VAlign, Attributes);
      SectionList := CellObj.Cell;
      SkipWhiteSpace;
      Next;
      NoPSpace := True;   
      DoBody(0, TableTermSet);
      end;
    CaptionSy:
      begin
      if InHref then DoAEnd;    
      while StackIndex > SaveIndex do
        PopFont;          {terminate any <font> introduced in table}
      CurrentStyle := SaveStyle;  
      AddSection;
      if Attributes.Find(AlignSy, T) then
        Table.TopCaption := Lowercase(T.Name^) <> 'bottom';
      SectionList := Table.Caption.Cell;
      Justify := Centered;
      Next;
      DoBody(0, TableTermSet);

      SectionList.Add(Section);
      Section := Nil;
      SectionList := Nil;
      if Sy = CaptionEndSy then Next;  {else it's TDSy, THSy, etc}
      end;
    TREndSy:    
      begin
      if InHref then DoAEnd;
      if Assigned(Row) then    
        begin
        AddSection;
        Table.Rows.Add(Row);
        Row := Nil;
        end;
      Next;
      end;
    TRSy:
      begin
      if InHref then DoAEnd;
      if Assigned(Row) then    
        begin
        AddSection;
        Table.Rows.Add(Row);
        end;
      Row := TCellList.Create;
      RowJustify := GetJustify;
      RowVAlign := GetVAlign(Middle);
      Row.DoAttributes(Attributes);
      Next;
      end;
    TDEndSy, THEndSy:
      begin AddSection; Next; end;
    TextSy:
      begin
      JunkSaveSectionList := SectionList;
      SectionList := SaveSectionList;   {the original one}
      DoBody(0, TableTermSet);
      SectionList.Add(Section);
      Section := Nil;
      SectionList := JunkSaveSectionList;
      end;
    else Next;  {ignore all else}
    end;
if InHref then DoAEnd;
while StackIndex > SaveIndex do
  PopFont;          {terminate any <font> introduced in table}
if Sy <> EofSy then
  AddSection
else
  begin
  SectionList.Free;
  SectionList := Nil;
  end;
if Assigned(Row) then   
  Table.Rows.Add(Row);
SectionList := SaveSectionList;
SectionList.Add(Table);
CurrentStyle := SaveStyle;
Justify := SaveJustify;
Next;
end;

procedure GetOptions(Select: TListBoxFormControlObj);
 {get the <option>s for Select form control}
var
  Selected, InOption: boolean;
  Value, S: string[80];
  T: TAttribute;
begin
Next;
S := '';  Value := '';
Selected := False;  InOption := False;
while not (Sy in[SelectEndSy, EofSy]) do
  begin
  case Sy of
    OptionSy, OptionEndSy:
      begin
      S := Trim(S);
      if S <> '' then
        begin
        if InOption then
          Select.AddStr(S, Value, Selected);
        S := '';
        Value := '';
        Selected := False;
        end;
      InOption := Sy = OptionSy;
      if InOption then
        begin
        Selected := Attributes.Find(SelectedSy, T);
        if Attributes.Find(ValueSy, T) then
          Value := T.Name^;
        end;
      end;
    TextSy: if InOption then
              S := S+LCToken;
    end;
  Next;
  end;
if InOption then
  begin
  S := Trim(S);
  if S <> '' then
    Select.AddStr(S, Value, Selected);
  end;
Select.ResetToValue;
end;

function GetColor(S: String; Var Value: TColor): boolean;
const
  ColorValues: array[1..16] of TColor =
     (clBLACK, clMAROON, clGREEN, clOLIVE, clNAVY, clPURPLE, clTEAL, clGRAY,
      clSILVER, clRED, clLIME, clYELLOW, clBLUE, clFUCHSIA, clAQUA, clWHITE);

const
  Colors: array[1..16] of string[7] =
     ('BLACK', 'MAROON', 'GREEN', 'OLIVE', 'NAVY', 'PURPLE', 'TEAL', 'GRAY',
      'SILVER', 'RED', 'LIME', 'YELLOW', 'BLUE', 'FUCHSIA', 'AQUA', 'WHITE');
var
  Red, Blue: LongInt;
  I: integer;
  S1: string[10];
begin
GetColor := False;
if Length(S) > 0 then
  begin
  S1 := Uppercase(S);
  for I := 1 to 16 do
    if S1 = Colors[I] then
      begin
      Value := ColorValues[I];
      GetColor := True;
      Exit;
      end;
  try
    I := Pos('#', S);   {apparently # is allowed}
    if I > 0 then Delete(S, I, 1);
    while Length(S) < 6 do
      S := S+'0';
    Value := StrToInt('$'+S);  {but bytes are backwards!}
    Red := Value and $FF;
    Blue := Value and $FF0000;
    Value := (Value and $00FF00) + (Red shl 16) + (Blue shr 16);
    GetColor := True;
  except
  end;
  end;
end;

{----------------DoMap}
procedure DoMap;
var
  Item: TMapItem;
  T: TAttribute;
  ErrorCnt: integer;
begin
Item := TMapItem.Create;
ErrorCnt := 0;
try
  if Attributes.Find(NameSy, T) then
    AssignStr(Item.MapName, Uppercase(T.Name^));
  Next;
  while (Sy <> MapEndSy) and (Sy <> EofSy) and (ErrorCnt < 3) do
    begin
    if Sy = AreaSy then Item.AddArea(Attributes)
    else if Sy <> TextSy then  
      Inc(ErrorCnt);
    Next;
    end;
  if Sy = MapEndSy then MasterList.MapList.Add(Item)
    else Item.Free;
except
  Item.Free;
  Raise;
  end;
Next;
end;

procedure DoScript(Ascript: TScriptEvent);  
const
  Block = 32500;
var
  Lang: string;
  T: TAttribute;
  Buffer: PChar;
  Pos, Size: integer;

  procedure AddText(const S: string);
  begin
  if Pos + Length(S) >= Size then
    {$ifdef Windows}
      Exit;   {Delphi 1}
    {$else}
      begin   {Delphi 2,3, add to buffer}
      ReAllocMem(Buffer, Size+10000);
      Inc(Size, 10000);
      end;
    {$endif}
  Move(S[1], Buffer[Pos], Length(S));
  Inc(Pos, Length(S));
  end;

  procedure Next1;
    {Special Next routine to get the next token}
    procedure GetTag1;  {simplified 'Pick up a Tag' routine}
    var
      Count: integer;
    begin
    LCToken := '<'; GetCh;
    Sy := CommandSy;   {catch all}
    Count := 0;
    while (Ch in ['A'..'Z', '/']) and (Count <= 6) do
      begin
      LCToken := LCToken + LCh;
      GetCh;
      Inc(Count);
      end;
    if LCh = '>' then
      begin
      LCToken := LCToken + LCh;
      GetCh;
      end;
    if CompareText(LCToken, '</script>') = 0 then
      Sy := ScriptEndSy;
    end;

  begin  {already have fresh character loaded here}
  LCToken := '';
  if LCh = EofChar then Sy := EofSy
  else if LCh = ^M then
    begin
    Sy := EolSy;
    GetCh;
    end
  else if LCh = '<' then
     GetTag1
  else
    begin
    Sy := TextSy;
    while (Length(LCToken) < 100) and  not (LCh in [^M, '<', EofChar]) do
      begin
      LCToken := LCToken+LCh;
      GetCh;
      end;
    end;
  end;

begin
try
  if Assigned(AScript) then
    begin
    InScript := True;
    if Attributes.Find(LanguageSy, T) then
      Lang := T.Name^
    else Lang := '';
    GetMem(Buffer, Block);
    Pos := 0;
    Size := Block;
    try
      Next1;
      while (Sy <> ScriptEndSy) and (Sy <> EofSy) do
        begin
        if Sy = EolSy then AddText(^M^J)
        else
          AddText(LCToken);
        Next1;
        end;
      AddText(#0);
      {$ifdef Windows}
      Buffer := ReAllocMem(Buffer, Size, Pos);
      {$else}
      ReAllocMem(Buffer, Size);
      {$endif}
      AScript(CallingObject, Lang, Buffer);
    except
      {$ifdef Windows}
      FreeMem(Buffer, Size);
      {$else}
      FreeMem(Buffer);
      {$endif}
      Raise;
      end;
    end
  else
    begin
    repeat
      Next1;
    until Sy in [ScriptEndSy, EofSy];
    end;
finally
  InScript := False;
  end;
end;

{----------------DoCommonSy}
procedure DoCommonSy(Lev: integer);
var
  I: integer;
  TxtArea: TTextAreaFormControlObj;
  FormControl: TFormControlObj;
  T: TAttribute;
  TmpJustify, LastAlign: JustifyType;
  Tmp: string;

  function IncrementFont(Sy: Symb; Pre: boolean): boolean;
  var
    NewSize: integer;

    function GetSizeIndex(Pre: boolean; Size: integer): integer;
    begin
    for Result := 1 to 7 do
      if Pre and (Size = PreFontConv[Result]) then Exit
      else if Size = FontConv[Result] then Exit;
    Result := -1;
    end;

  begin
  Result := False;
  NewSize := GetSizeIndex(Pre, FontStack[StackIndex].NormalSize);
  if (Sy = BigSy) then
    begin
    if (NewSize in [1..6]) then Inc(NewSize);
    end
  else
    if NewSize in [2..7] then Dec(NewSize);   

  if PushNewFont then
    begin
    if Pre then NewSize := PreFontConv[NewSize]
    else NewSize := FontConv[NewSize];
    FontStack[StackIndex].SetNormalSize(MasterList, NewSize);
    Result := True;
    end;
  end;

  function ChangeTheFont(Sy: Symb; Pre: boolean): boolean;
  var
    FaceName: string[50];
    NewColor: TColor;
    NewSize, I, K: integer;
    FontResults: set of (Face, Colr, Siz);
  begin
  FontResults := [];
  NewSize := 0;  {get rid of warning}
  for I := 0 to Attributes.Count-1 do
    with TAttribute(Attributes[I]) do
      case Which of
        SizeSy:
          begin
          if (Length(Name^) >= 2) and (Name^[1] in ['+', '-']) then
            Value := BaseFontSize + Value;
          NewSize := IntMax(1, IntMin(7, Value)); {limit 1..7}
          if (Sy = BaseFontSy) then BaseFontSize := NewSize;
          Include(FontResults, Siz);
          end;
        ColorSy:
          if (Sy <> BaseFontSy) and GetColor(Name^, NewColor) then Include(FontResults, Colr);
        FaceSy:
          if (Sy <> BaseFontSy) and (Name^ <> '') then
            begin
            FaceName := Name^;
            K := Pos(',', FaceName);
            if K > 0 then
              Delete(FaceName, K, 255);
            FaceName := Trim(FaceName);
            if FaceName <> '' then
              Include(FontResults, Face);
            end;
        end;
  Result := False;
  if ((Sy <> BasefontSy) or (SectionList.Count = 0) and
          (SectionList = MasterList)) and  {new font only if at start for Basefont}
       PushNewFont and (FontResults <> []) then
    with FontStack[StackIndex] do
      begin
      if Colr in FontResults then
        Color := NewColor or $2000000;
      if Siz in FontResults then
        begin
        if Pre then NewSize := PreFontConv[NewSize]
        else NewSize := FontConv[NewSize];
        SetNormalSize(MasterList, NewSize);
        end;
      if Face in FontResults then
        Name := FaceName;
      Result := True;
      end;
  end;

  procedure DoPreSy;
  var
    S, Tmp: String;
    Done: boolean;
    I: integer;

    procedure NewSection;
    begin
    Section.AddText(S);
    S := '';
    SectionList.Add(Section);
    Section := TPreFormated.Create(MasterList, Lev, FontStack[StackIndex],
               CurrentUrlTarget, Left);
    end;

  begin
  SectionList.Add(Section);
  PushNewFont;
  with FontStack[StackIndex] do
    begin
    Name := MasterList.PreFontName;
    SetNormalSize(MasterList, 10);
    Fixed := True;
    end;
  Section := TPreformated.Create(MasterList, Lev, FontStack[StackIndex],
             CurrentUrlTarget, Left);
  S := '';
  PreFormat := True;
  Done := False;
  while not Done do
    case Ch of
      '&': begin
           Next;
           S := S+LCToken;
           end;
      '<':
         begin
         Next;
         case Sy of
           PSy, BRSy:
             begin
             NewSection;
             if Ch = ^M then GetCh;
             end;

           PreEndSy, TDEndSy, THEndSy:    
             Done := True;

           BSy, ISy, BEndSy, IEndSy, EmSy, EmEndSy, StrongSy, StrongEndSy,
               USy, UEndSy, CiteSy, CiteEndSy, VarSy, VarEndSy,
               SSy, SEndSy, StrikeSy, StrikeEndSy:        
             begin
             Section.AddText(S);
             S := '';
             case Sy of
               BSy, StrongSy:  CurrentStyle := CurrentStyle + [fsBold];
               BEndSy, StrongEndSy:  CurrentStyle := CurrentStyle - [fsBold];
               ISy, EmSy, CiteSy, VarSy:  CurrentStyle := CurrentStyle + [fsItalic];
               IEndSy, EmEndSy,
                 CiteEndSy, VarEndSy:  CurrentStyle := CurrentStyle - [fsItalic];
               USy:  CurrentStyle := CurrentStyle + [fsUnderline];
               UEndSy:  CurrentStyle := CurrentStyle - [fsUnderline];
               SSy, StrikeSy:  CurrentStyle := CurrentStyle + [fsStrikeOut];      
               SEndSy, StrikeEndSy:  CurrentStyle := CurrentStyle - [fsStrikeOut]; 
               end;

             TSection(Section).ChangeFont(MasterList, FontStack[StackIndex]);
             end;

           FontSy, BaseFontSy:
             begin
             Section.AddText(S);
             S := '';
             if ChangeTheFont(Sy, True) then
               TSection(Section).ChangeFont(MasterList, FontStack[StackIndex]);
             end;
           FontEndSy:
             begin
             PopFont;
             Section.AddText(S);
             S := '';
             TSection(Section).ChangeFont(MasterList, FontStack[StackIndex]);
             end;
           ASy:
             for I := 0 to Attributes.Count-1 do
               with TAttribute(Attributes[I]) do
                 case Which of
                   NameSy:
                     if Assigned(Name) then
                       begin     
                       Tmp := UpperCase(Name^);
                       {Author may have added '#' by mistake}
                       if (Length(Tmp) > 0) and (Tmp[1] = '#') then
                         Delete(Tmp, 1, 1);
                       NameList.AddObject(Tmp, Section);
                       end;
                   HRefSy:
                     begin
                     Section.AddText(S);
                     S := '';
                     if InHref then DoAEnd;    
                     if Assigned(Name) then  {also have a string}
                       begin
                       if Attributes.Find(TargetSy, T) then
                         CurrentUrlTarget.Assign(Name, T.Name)
                       else CurrentUrlTarget.Assign(Name, Nil);
                       InHref := True;
                       PushNewFont;
                       with FontStack[StackIndex] do
                         begin
                         Style := Style + MasterList.UnLine;  
                         Color := MasterList.HotSpotColor;
                         end;
                       end;
                     Section.HRef(HRefSy, MasterList, CurrentUrlTarget, FontStack[StackIndex]);
                     end;
                   end;
           AEndSy:
             begin
             Section.AddText(S);
             S := '';
             DoAEnd;   
             end;
           ImageSy:
             begin
             Section.AddText(S);
             TSection(Section).AddImage(Attributes, SectionList);
             S := '';
             end;
           PageSy:
             begin
             Section.AddText(S);
             S := '';
             SectionList.Add(Section);
             SectionList.Add(TPage.Create(MasterList));
             Section := TPreFormated.Create(MasterList, Lev, FontStack[StackIndex],
                        CurrentUrlTarget, Left);
             end;
           InputSy, SelectSy:
             begin
             Section.AddText(S);
             FormControl := TSection(Section).AddFormControl(Sy, MasterList,
                         Attributes, SectionList);
             if Sy = SelectSy then
               GetOptions(FormControl as TListBoxFormControlObj);
             S := '';;
             end;
           TextAreaSy:
             Begin
             Section.AddText(S);
             TxtArea := TSection(Section).AddFormControl(TextAreaSy, MasterList,
                        Attributes, SectionList) as TTextAreaFormControlObj;
             DoTextArea(TxtArea);
             S := '';
             end;
           FormSy:
             CurrentForm := ThtmlForm.Create(MasterList, Attributes);
           FormEndSy:
             CurrentForm := Nil;
           MapSy: DoMap;
           ScriptSy: DoScript(MasterList.ScriptEvent);
           end;
         end;
      ^M : begin NewSection; GetCh; end;
      EofChar : Done := True;
      else
        begin   {all other chars}
        S := S+LCh;
        if Length(S) > 200 then
          begin
          Section.AddText(S);
          S := '';
          end;
        GetCh;
        end;
      end;
  Section.AddText(S);
  SectionList.Add(Section);
  Section := Nil;
  PreFormat := False;
  PopFont;
  Next;
  end;

  function CreateFont(HeadNmb: integer; OldFont: TMyFont): TMyFont;
  var
    F : TMyFont;
    Siz: integer;
  begin
  F := TMyFont.Create;
  F.Assign(OldFont);
  case HeadNmb of
    0: Siz := 12;     {0 is no heading}
    1: Siz := 24;
    2: Siz := 18; 
    3: Siz := 14;  
    4: Siz := 12;
    5: Siz := 10;
    6: Siz := 8;
    else Siz := 12;
  end;
  if HeadNmb > 0 then
    F.Style := F.Style + [fsBold];
  F.SetNormalSize(MasterList, Siz);
  Result := F;
  end;

begin
case Sy of
  TextSy :
    begin
    if not Assigned(Section) then
      Section := TSection.Create(MasterList, Lev, FontStack[StackIndex],
                 CurrentUrlTarget, Justify);
    Section.AddText(LCToken);
    Next;
    end;
  ImageSy:
    begin
    if not Assigned(Section) then
      Section := TSection.Create(MasterList, Lev, FontStack[StackIndex],
                 CurrentUrlTarget, Justify);
    TSection(Section).AddImage(Attributes, SectionList);
    Next;
    end;
  InputSy, SelectSy:
    begin
    if not Assigned(Section) then
      Section := TSection.Create(MasterList, Lev, FontStack[StackIndex],
                 CurrentUrlTarget, Justify);
    FormControl := TSection(Section).AddFormControl(Sy, MasterList, Attributes, SectionList);
    if Sy = SelectSy then
      GetOptions(FormControl as TListBoxFormControlObj);
    Next;
    end;
  TextAreaSy:
    Begin
    if not Assigned(Section) then
      Section := TSection.Create(MasterList, Lev, FontStack[StackIndex],
                 CurrentUrlTarget, Justify);
    TxtArea := TSection(Section).AddFormControl(TextAreaSy, MasterList,
               Attributes, SectionList) as TTextAreaFormControlObj;
    DoTextArea(TxtArea);
    Next;
    end;
  TextAreaEndSy:   {a syntax error but shouldn't hang}
    Next;
  FormSy:
    begin
    CurrentForm := ThtmlForm.Create(MasterList, Attributes);
    Next;
    end;
  FormEndSy:
    begin
    CurrentForm := Nil;
    Next;
    end;
  PageSy:
    begin
    SectionList.Add(Section);
    Section := Nil;
    SectionList.Add(TPage.Create(MasterList));
    Next;
    end;
  PSy, PEndSy:
    begin
    SectionList.Add(Section);
    Section := Nil;
    if Not NoPSpace then
      SectionList.Add(TParagraphSpace.Create(MasterList));
    SkipWhiteSpace;
    LastAlign := FindAlignment;
    NoPSpace := True;
    Next;
    while Sy in [PSy, PEndSy] do
      begin           {recognize only the first <p>}
      LastAlign := FindAlignment;   {if a series of <p>, get last alignment}
      SkipWhiteSpace;
      NoPSpace := True;
      Next;
      end;
    Section := TSection.Create(MasterList, Lev, FontStack[StackIndex],
               CurrentUrlTarget, LastAlign);
    end;
  BRSy:
    begin
    if Assigned(Section) then
      TmpJustify := TSection(Section).BreakInfo  {so <br> doesn't change justification}
    else TmpJustify := Justify;
    SectionList.Add(Section);
    Section := TSection.Create(MasterList, Lev, FontStack[StackIndex],
               CurrentUrlTarget, TmpJustify);
    Section.DoClearAttribute(Attributes);     {check for clear attribute}
    Next;
    end;
  BSy, ISy, BEndSy, IEndSy, EmSy, EmEndSy, StrongSy, StrongEndSy,
      USy, UEndSy, CiteSy, CiteEndSy, VarSy, VarEndSy,
      SubSy, SubEndSy, SupSy, SupEndSy, SSy, SEndSy, StrikeSy, StrikeEndSy:
    begin
    case Sy of
      BSy, StrongSy:  CurrentStyle := CurrentStyle + [fsBold];
      BEndSy, StrongEndSy:  CurrentStyle := CurrentStyle - [fsBold];
      ISy, EmSy, CiteSy, VarSy:  CurrentStyle := CurrentStyle + [fsItalic];
      IEndSy, EmEndSy,
        CiteEndSy, VarEndSy:  CurrentStyle := CurrentStyle - [fsItalic];
      USy:  CurrentStyle := CurrentStyle + [fsUnderline];
      UEndSy:  CurrentStyle := CurrentStyle - [fsUnderline];
      SSy, StrikeSy:  CurrentStyle := CurrentStyle + [fsStrikeOut];
      SEndSy, StrikeEndSy:  CurrentStyle := CurrentStyle - [fsStrikeOut];
      SubEndSy, SupEndSy: CurrentSScript := Normal;
      SubSy, SupSy:
        begin
        if not Assigned(Section) then
          Section := TSection.Create(MasterList, Lev, FontStack[StackIndex],
                     CurrentUrlTarget, Justify);
        if Sy = SubSy then CurrentSScript := SubSc
          else CurrentSScript := SupSc;
        end;
      end;

    if Assigned(Section) then    {CurrentStyle used in ChangeFont}
      TSection(Section).ChangeFont(MasterList, FontStack[StackIndex]);
    Next;
    end;
  TTSy, CodeSy, KbdSy, SampSy:
    begin
    if PushNewFont then
      begin
      with FontStack[StackIndex] do
        begin
        Name := MasterList.PreFontName;
        SetNormalSize(MasterList, 10);
        Fixed := True;
        end;
      if Assigned(Section) then
        TSection(Section).ChangeFont(MasterList, FontStack[StackIndex]);
      end;
    Next;
    end;
  TTEndSy, CodeEndSy, KbdEndSy, SampEndSy, FontEndSy, BigEndSy, SmallEndSy:
    begin
    PopFont;
      if Assigned(Section) then
        TSection(Section).ChangeFont(MasterList, FontStack[StackIndex]);
    Next;
    end;
  FontSy, BaseFontSy:
    begin
    if ChangeTheFont(Sy, False) and Assigned(Section) then
      TSection(Section).ChangeFont(MasterList, FontStack[StackIndex]);
    Next;
    end;
  BigSy, SmallSy:
    begin
    if IncrementFont(Sy, False) and Assigned(Section) then
      TSection(Section).ChangeFont(MasterList, FontStack[StackIndex]);
    Next;
    end;
  AddressSy:
    begin
    SectionList.Add(Section);
    PushNewFont;
    with FontStack[StackIndex] do
      Style := Style + [fsItalic];
    Section := TSection.Create(MasterList, Lev, FontStack[StackIndex],
               CurrentUrlTarget, Justify);
    Next;
    end;
  AddressEndSy:
    begin
    SectionList.Add(Section);
    Section := Nil;
    PopFont;
    Next;
    end;
  ASy:
    begin
    for I := 0 to Attributes.Count-1 do
      with TAttribute(Attributes[I]) do
        case Which of
          NameSy:
            if Assigned(Name) then
              begin
              if not Assigned(Section) then
                Section := TSection.Create(MasterList, Lev, FontStack[StackIndex],
                           CurrentUrlTarget, Justify);
              Tmp := UpperCase(Name^);
              {Author may have added '#' by mistake}
              if (Length(Tmp) > 0) and (Tmp[1] = '#') then
                Delete(Tmp, 1, 1);
              NameList.AddObject(Tmp, Section);
              end;
          HRefSy:
            begin
            if InHref then DoAEnd;    
            if Assigned(Name) then  {also have a string}
              begin
              if Attributes.Find(TargetSy, T) then
                CurrentUrlTarget.Assign(Name, T.Name)
              else CurrentUrlTarget.Assign(Name, Nil);
              InHref := True;     
              PushNewFont;
              with FontStack[StackIndex] do
                begin
                Style := Style + MasterList.UnLine; 
                Color := MasterList.HotSpotColor;
                end;
              if Assigned(Section) then
                Section.HRef(HRefSy, MasterList, CurrentUrlTarget, FontStack[StackIndex]);
              end;
            end;
          end;
    Next;
    end;
  AEndSy:
    begin
    DoAEnd;  
    Next;
    end;
  HeadingSy:
    begin
    if StackIndex < MaxStack then
      begin
      SectionList.Add(Section);
      Inc(StackIndex);
      FontStack[StackIndex] := CreateFont(Value, FontStack[StackIndex-1]);
      SectionList.Add(THeadingSpace.Create(MasterList, Value));
      Section := TSection.Create(MasterList, Lev, FontStack[StackIndex],
                 CurrentUrlTarget, FindAlignment);
      end;
    Next;
    end;
  HeadingEndSy:
    begin
    if StackIndex > 1 then
      begin
      SectionList.Add(Section);
      SectionList.Add(THeadingSpace.Create(MasterList, Value));
      Section := Nil;
      PopFont;
      end;
    Next;
    end;
  PreSy:  DoPreSy;
  TableSy: DoTable(Lev);
  MapSy: DoMap;
  ScriptSy: begin DoScript(MasterList.ScriptEvent); Next; end;
  end;
end;   {DoCommon}

{-------------DoLists}
procedure DoLists(Level: integer; Sym: Symb; const TermSet: SymbSet);
var
  LineCount, CurrentLevel: integer;
  T: TAttribute;
  Plain: boolean;
  Index: char;   

begin
LineCount := 1;
Index := '1';
Plain := False;
if (Sym = OLSy) then
  begin
  if Attributes.Find(StartSy, T) then
    if T.Value >= 0 then LineCount := T.Value;
  if Attributes.Find(TypeSy, T) and (T.Name^ <> '') then
    Index := T.Name^[1];
  end
else Plain := (Sym = ULSy) and Attributes.Find(PlainSy, T);
CurrentLevel := Level;
SectionList.Add(Section);
Section := Nil;
Next;
if Sy in [OLEndSy, ULEndSy, DirEndSy, MenuEndSy, DLEndSy, BlockQuoteEndSy] then
  Exit;    {guard against <ul></ul> and similar combinations}
repeat
  case Sy of
    LISy:
      begin
      SectionList.Add(Section);
      if Sym = OLSy then
        begin
        Section := TOListItem.Create(MasterList, Level, LineCount, Index,
                    FontStack[StackIndex], CurrentUrlTarget);
        Inc(LineCount);
        end
      else Section := TUlistItem.Create(MasterList, Level, FontStack[StackIndex],
                      CurrentUrlTarget);
      if (Sym = ULSy) and Plain then
        TUlistItem(Section).Plain := True;
      CurrentLevel := Level;
      SkipWhiteSpace;
      Next;
      if Sy = PSy then Next;
      end;
    DTSy, DDSy:
      begin
      SectionList.Add(Section);
      if Sy = DTSy then
        CurrentLevel := Level-1
      else CurrentLevel := Level;
      Section := TDListItem.Create(MasterList, CurrentLevel, FontStack[StackIndex],
                 CurrentUrlTarget);
      Next;
      end;
    OLSy, ULSy, DirSy, MenuSy, DLSy:
      begin
      DoLists(Level+1, Sy, TermSet);
      Next;
      end;
    BlockQuoteSy:
      begin
      SectionList.Add(Section);
      Section := Nil;
      DoLists(Level+1, Sy, TermSet);
      Next;
      end;
    DivSy, CenterSy:
      DoBody(CurrentLevel, [OLEndSy, ULEndSy, DirEndSy, MenuEndSy, DLEndSy,
             BlockQuoteEndSy, EofSy]+TermSet);
    HRSy:
      begin
      SectionList.Add(Section);
      SectionList.Add(THorzLine.Create(MasterList, Attributes));
      Section := Nil;
      Next;
      end;
  TableSy:
      begin
      if Assigned(Section) then
        TSection(Section).BreakInfo;
      DoTable(CurrentLevel);   
      end;

    TextSy, BRSy, PSy, PEndSy,
    BSy, ISy, BEndSy, IEndSy, EmSy, EmEndSy, StrongSy, StrongEndSy,
        USy, UEndSy, CiteSy, CiteEndSy, VarSy, VarEndSy,
        SubSy, SubEndSy, SupSy, SupEndSy, SSy, SEndSy, StrikeSy, StrikeEndSy, 
    TTSy, CodeSy, KbdSy, SampSy,  TTEndSy, CodeEndSy, KbdEndSy, SampEndSy,
    NameSy, HRefSy, ASy, AEndSy,
    HeadingSy, HeadingEndSy, AddressSy, AddressEndSy, PreSy,
    InputSy, FormSy, FormEndSy, TextAreaSy, TextAreaEndSy, SelectSy,
    ImageSy, FontSy, FontEndSy, BaseFontSy, BigSy, BigEndSy, SmallSy,
    SmallEndSy, MapSy, PageSy, ScriptSy:
      DoCommonSy(CurrentLevel);
    else if Sy in TermSet then Exit
      else Next;
    end;
Until (Sy in [OLEndSy, ULEndSy, DirEndSy, MenuEndSy, DLEndSy,
             BlockQuoteEndSy, EofSy]);
SectionList.Add(Section);
Section := Nil;
end;

{----------------DoBase}
procedure DoBase;
var
  I: integer;
begin
with Attributes do
  for I := 0 to Count-1 do
    with TAttribute(Attributes[I]) do
      if Which = HrefSy then
        Base := Name^
      else if Which = TargetSy then
        BaseTarget := Name^;
Next;
end;

{----------------DoSound}
procedure DoSound;
var
  Loop: integer;
  T, T1: TAttribute;
begin
if Assigned(SoundEvent) and Attributes.Find(SrcSy, T) then
  begin
  if Attributes.Find(LoopSy, T1) then Loop := T1.Value
    else Loop := 1;
  SoundEvent(CallingObject, T.Name^, Loop, False);
  end;
Next;
end;

{$ifdef Delphi3_4_CppBuilder3_4}     
function TranslateCharset(DefCharset: TFontCharset; const Content: string): TFontCharset;
type
  XRec = record S: string; CSet: TFontCharset; end;
const
  MaxX = 14;
  XTable: array[1..MaxX] of XRec =
     ((S:'1252';    CSet:ANSI_CHARSET),
      (S:'8859-1';    CSet:ANSI_CHARSET),
      (S:'1253';    CSet:GREEK_CHARSET),
      (S:'8859-7';    CSet:GREEK_CHARSET),
      (S:'1250';    CSet:EASTEUROPE_CHARSET),
      (S:'8859-2';    CSet:EASTEUROPE_CHARSET),
      (S:'1251';    CSet:RUSSIAN_CHARSET),
      (S:'8859-5';    CSet:RUSSIAN_CHARSET),
      (S:'koi8-r';    CSet:RUSSIAN_CHARSET),
      (S:'1254';    CSet:TURKISH_CHARSET),
      (S:'8859-3';    CSet:TURKISH_CHARSET),
      (S:'8859-9';    CSet:TURKISH_CHARSET),
      (S:'1257';    CSet:BALTIC_CHARSET),
      (S:'8859-4';    CSet:BALTIC_CHARSET));
var
  I: integer;
begin
Result := DefCharset;
for I := 1 to MaxX do
  if Pos(XTable[I].S, Lowercase(Content)) > 0 then
    Begin
    Result := XTable[I].CSet;
    Break;
    end;
end;
{$endif}

{----------------DoMeta}
procedure DoMeta(Sender: TObject);
var
  T: TAttribute;
  HttpEq, Name, Content: string;
  {$ifdef Delphi3_4_CppBuilder3_4}
  Charset: TFontCharset;
  {$endif}
begin
if Attributes.Find(HttpEqSy, T) then HttpEq := T.Name^
  else HttpEq := '';
if Attributes.Find(NameSy, T) then Name := T.Name^
  else Name := '';
if Attributes.Find(ContentSy, T) then Content := T.Name^
  else Content := '';
{$ifdef Delphi3_4_CppBuilder3_4}
if (Sender is ThtmlViewer) and (CompareText(HttpEq, 'content-type') = 0) then  
  begin
  CharSet := TranslateCharset(TSectionList(SectionList).Charset, Content);
  FontStack[StackIndex].Charset := Charset;
  end;
{$endif}
if Assigned(MetaEvent) then
  MetaEvent(Sender, HttpEq, Name, Content);
Next;
end;

{----------------DoTitle}
procedure DoTitle;
begin
Title := '';
Next;
while Sy = TextSy do
  begin
  Title := Title+LCToken;
  Next;
  end;
end;

{-------------DoBody}
procedure DoBody(Level: integer; const TermSet: SymbSet);
var
  I: integer;
  Val: TColor;
  T: TAttribute;
  SaveJustify: JustifyType;
  S: string[10];
  SaveSy: Symb;

begin
repeat
  case Sy of
    TextSy, BRSy, PSy, PEndSy,
    NameSy, HRefSy, ASy, AEndSy,
    BSy, ISy, BEndSy, IEndSy, EmSy, EmEndSy, StrongSy, StrongEndSy,
        USy, UEndSy, CiteSy, CiteEndSy, VarSy, VarEndSy,
        SubSy, SubEndSy, SupSy, SupEndSy,  SSy, SEndSy, StrikeSy, StrikeEndSy,
    TTSy, CodeSy, KbdSy, SampSy,  TTEndSy, CodeEndSy, KbdEndSy, SampEndSy,
    HeadingSy, HeadingEndSy, AddressSy, AddressEndSy, PreSy, TableSy,
    InputSy, FormSy, FormEndSy, TextAreaSy, TextAreaEndSy, SelectSy,
    ImageSy, FontSy, FontEndSy, BaseFontSy, BigSy, BigEndSy, SmallSy,
    SmallEndSy, MapSy, PageSy, ScriptSy:
      DoCommonSy(Level);
    BodySy:
      begin
      if SectionList.Count = 0 then   {make sure we're at beginning}
        begin
        Section.Free;   {Will start with a new section}
        for I := 0 to Attributes.Count-1 do
          with TAttribute(Attributes[I]) do
            case Which of
              BackgroundSy: MasterList.SetBackgroundBitmap(Name);
              TextSy: if GetColor(Name^, Val) then
                begin
                FontStack[StackIndex].Color := Val or $2000000;
                MasterList.FontColor := Val or $2000000;
                end;
              BGColorSy: if GetColor(Name^, Val) then MasterList.SetBackGround(Val or $2000000);
              LinkSy: if GetColor(Name^, Val) then MasterList.HotSpotColor := Val or $2000000;
              VLinkSy: if GetColor(Name^, Val) then MasterList.LinkVisitedColor := Val or $2000000;
              OLinkSy: if GetColor(Name^, Val) then
                 begin
                 MasterList.LinkActiveColor := Val or $2000000;
                 MasterList.LinksActive := True;
                 end;
            end;
        Section := TSection.Create(MasterList, Level, FontStack[1], Nil, Justify);
        end;
      Next;
      end;
    HRSy:
      begin
      SectionList.Add(Section);
      SectionList.Add(THorzLine.Create(MasterList, Attributes));
      Section := Nil;
      Next;
      end;
    OLSy, ULSy, DirSy, MenuSy, DLSy:
      begin
      DoLists(1, Sy, TermSet);
      Next;
      end;
    BlockQuoteSy:
      begin
      SectionList.Add(Section);
      Section := Nil;
      DoLists(1, Sy, TermSet);
      Next;
      end;
    DivSy, CenterSy:
      begin
      SaveSy := Sy;
      SectionList.Add(Section);
      SaveJustify := Justify;
      if SaveSy = CenterSy then
        Justify := Centered
      else
        if Attributes.Find(AlignSy, T) then
          begin
          S := LowerCase(T.Name^);
          if S = 'left' then Justify := Left
          else if S = 'center' then Justify := Centered
          else if S = 'right' then Justify := Right;
          end;
      Section := TSection.Create(MasterList, Level, FontStack[StackIndex],
                 CurrentUrlTarget, Justify);
      Next;
      DoBody(Level, [CenterEndSy, DivEndSy]+TermSet);
      SectionList.Add(Section);
      Justify := SaveJustify;
      Section := TSection.Create(MasterList, Level, FontStack[StackIndex],
                 CurrentUrlTarget, Justify);
      if Sy in [CenterEndSy, DivEndSy] then
        Next;
      end;
    TitleSy:
      DoTitle;
    BgSoundSy:
      DoSound;
    MetaSy:
      DoMeta(CallingObject);
    BaseSy:
      DoBase;
    else if Sy in TermSet then Exit
    else Next;
    end;
Until (Sy = EofSy);
Next;
end;

procedure DoFrameSet(FrameViewer: TFrameViewerBase; FrameSet: TObject; const FName: string);
var
  NewFrameSet: TObject;
begin
FrameViewer. DoAttributes(FrameSet, Attributes);
Next;
while (Sy <> FrameSetEndSy) and (Sy <> EofSy) do
  begin
  case Sy of
    FrameSy:
      begin
      FrameViewer.AddFrame(FrameSet, Attributes, FName);
      end;
    FrameSetSy:
      begin
      NewFrameSet := FrameViewer.CreateSubFrameSet(FrameSet);
      DoFrameSet(FrameViewer, NewFrameSet, FName);
      end;
    NoFramesSy:
      begin
      repeat
        Next;
      until (Sy = NoFramesEndSy) or (Sy = EofSy);
      end;
    ScriptSy:  begin DoScript(FrameViewer.FOnScript); Next; end; 
    end;
  Next;
  end;
FrameViewer.EndFrameSet(FrameSet);
end;

procedure ParseInit(ASectionList: TList; ANameList: TStringList; AIncludeEvent: TIncludeType);
begin
SectionList := TSectionList(ASectionList);
MasterList := TSectionList(SectionList);
CallingObject := TSectionList(ASectionList).TheOwner;
IncludeEvent := AIncludeEvent;
NameList := ANameList;
PreFormat := False;
StackIndex := 1;
FontStack[1] := TMyFont.Create;
FontStack[1].Name := MasterList.FontName;
FontStack[1].Color := MasterList.FontColor;
FontStack[1].SetNormalSize(MasterList, 12);
{$ifdef Delphi3_4_CppBuilder3_4}
FontStack[1].Charset := TSectionList(SectionList).Charset;
{$endif}
CurrentURLTarget := TUrlTarget.Create;
InHref := False;
BaseFontSize := 3;

Title := '';
Base := '';
BaseTarget := '';
Justify := Left;
CurrentStyle := [];
CurrentForm := Nil;
Section := TSection.Create(MasterList, 0, FontStack[1], Nil, Justify);
Attributes := TAttributeList.Create;
Back := #0;
InScript := False;
NoPSpace := False;
end;

{----------------HtmlParseStream}
procedure HtmlParseStream(ASectionList: TList; ANameList: TStringList;
                   AStream: TStream; AIncludeEvent: TIncludeType;
                   ASoundEvent: TSoundType; AMetaEvent: TMetaType);   
begin
LoadStyle := lsStream;
ParseInit(ASectionList, ANameList, AIncludeEvent);
SoundEvent := ASoundEvent;
Stream := AStream;
MetaEvent := AMetaEvent;
try
  Stream.Seek(0, 0);
  St := '';
  Chi := 1;
  GetCh; {get the reading started}
  Next;
  DoBody(0, []);
finally
  Attributes.Free;
  if Assigned(Section) then
    SectionList.Add(Section);
  while StackIndex >= 1 do
    begin
    FontStack[StackIndex].Free;
    Dec(StackIndex);
    end;
  CurrentURLTarget.Free;
  end;
end;

{----------------HtmlParseBuffer}
procedure HtmlParseBuffer(ASectionList: TList; ANameList: TStringList;
                   ABuffer : PChar; BuffSize: LongInt; AIncludeEvent: TIncludeType;
                   ASoundEvent: TSoundType; AMetaEvent: TMetaType);
begin
LoadStyle := lsBuffer;
ParseInit(ASectionList, ANameList, AIncludeEvent);
SoundEvent := ASoundEvent;   
MetaEvent := AMetaEvent;
Buffer := ABuffer;
BuffEnd := Buffer+BuffSize;
try
  GetCh; {get the reading started}
  Next;
  DoBody(0, []);
finally
  Attributes.Free;
  if Assigned(Section) then
    SectionList.Add(Section);
  while StackIndex >= 1 do
    begin
    FontStack[StackIndex].Free;
    Dec(StackIndex);
    end;
  CurrentURLTarget.Free;
  end;
end;

{----------------HtmlParseStrings}
procedure HtmlParseStrings(ASectionList: TList; ANameList: TStringList;
                   Strings : TStrings; AIncludeEvent: TIncludeType;
                   ASoundEvent: TSoundType; AMetaEvent: TMetaType);   
begin
LoadStyle := lsStrings;
TheStrings := Strings;
ParseInit(ASectionList, ANameList, AIncludeEvent);
SoundEvent := ASoundEvent;   
MetaEvent := AMetaEvent;
try
  St := '';
  Chi := 1;
  LineNo := 0;
  GetCh; {get the reading started}
  Next;
  DoBody(0, []);
finally
  Attributes.Free;
  if Assigned(Section) then
    SectionList.Add(Section);
  while StackIndex >= 1 do
    begin
    FontStack[StackIndex].Free;
    Dec(StackIndex);
    end;
  CurrentURLTarget.Free;
  end;
end;

{-------------HtmlParseFile}
procedure HtmlParseFile(ASectionList: TList; ANameList: TStringList;
                   const FName : string; AIncludeEvent: TIncludeType;
                   ASoundEvent: TSoundType; AMetaEvent: TMetaType);
var
  FOpen: boolean;
begin
LoadStyle := lsFile;
ParseInit(ASectionList, ANameList, AIncludeEvent);
SoundEvent := ASoundEvent;
MetaEvent := AMetaEvent;

{$I+}
FOpen := False;
try
  GetMem(InBuff, TextBuffSize);
  try
    AssignFile(Inf, FName);
    SetTextBuf(Inf, InBuff^, TextBuffSize);
    Reset(Inf);
    FOpen := True;

    GetCh; {get the reading started}
    Next;
    DoBody(0, []);
  except
    On EParseError do;   {ignore this error}
  end;

finally
  FreeMem(InBuff, TextBuffSize);
  if FOpen then CloseFile(Inf);
  Attributes.Free;
  if Assigned(Section) then
    SectionList.Add(Section);
  while StackIndex >= 1 do
    begin
    FontStack[StackIndex].Free;
    Dec(StackIndex);
    end;
  CurrentURLTarget.Free;
 end;   {finally}
end;

{----------------DoText}
procedure DoText;
var
  S: String;
  Done: boolean;

  procedure NewSection;
  begin
  Section.AddText(S);
  S := '';
  SectionList.Add(Section);
  Section := TPreFormated.Create(MasterList, 0, FontStack[StackIndex], CurrentURLTarget, Left);
  end;

begin
SectionList.Add(Section);
PushNewFont;
with FontStack[StackIndex] do
  begin
  Name := MasterList.PreFontName;
  SetNormalSize(MasterList, 10);
  Fixed := True;
  end;
Section := TPreformated.Create(MasterList, 0, FontStack[StackIndex],
           CurrentURLTarget, Left);
S := '';
PreFormat := True;
Done := False;
while not Done do
  case Ch of
    ^M : begin NewSection; GetCh; end;
    EofChar : Done := True;
    else
      begin   {all other chars}
      S := S+LCh;
      if Length(S) > 200 then
        begin
        Section.AddText(S);
        S := '';
        end;
      GetCh;
      end;
    end;
Section.AddText(S);
SectionList.Add(Section);
Section := Nil;
PreFormat := False;
PopFont;
Next;
end;

{-------------HtmlParseTextFile}
procedure HtmlParseTextFile(ASectionList: TList; ANameList: TStringList;
                   const FName : string);
var
  FOpen: boolean;
begin
LoadStyle := lsFile;
ParseInit(ASectionList, ANameList, Nil);

{$I+}
FOpen := False;
try
  GetMem(InBuff, TextBuffSize);
  try
    AssignFile(Inf, FName);
    SetTextBuf(Inf, InBuff^, TextBuffSize);
    Reset(Inf);
    FOpen := True;

    GetCh; {get the reading started}
    DoText;
  except
    On EParseError do;   {ignore this error}
  end;

finally
  FreeMem(InBuff, TextBuffSize);
  if FOpen then CloseFile(Inf);
  Attributes.Free;
  if Assigned(Section) then
    SectionList.Add(Section);
  while StackIndex >= 1 do
    begin
    FontStack[StackIndex].Free;
    Dec(StackIndex);
    end;
 end;   {finally}
end;

{----------------HtmlParseTextStream}
procedure HtmlParseTextStream(ASectionList: TList; ANameList: TStringList;
                   AStream: TStream);
begin
LoadStyle := lsStream;
ParseInit(ASectionList, ANameList, Nil);
Stream := AStream;
try
  Stream.Seek(0, 0);
  St := '';
  Chi := 1;
  GetCh; {get the reading started}
  DoText;
finally
  Attributes.Free;
  if Assigned(Section) then
    SectionList.Add(Section);
  while StackIndex >= 1 do
    begin
    FontStack[StackIndex].Free;
    Dec(StackIndex);
    end;
  CurrentURLTarget.Free;
  end;
end;

{----------------HtmlParseTextStrings}
procedure HtmlParseTextStrings(ASectionList: TList; ANameList: TStringList;
                   Strings : TStrings);
begin
LoadStyle := lsStrings;
TheStrings := Strings;
ParseInit(ASectionList, ANameList, NIl);
try
  St := '';
  Chi := 1;
  LineNo := 0;
  GetCh; {get the reading started}
  DoText;
finally
  Attributes.Free;
  if Assigned(Section) then
    SectionList.Add(Section);
  while StackIndex >= 1 do
    begin
    FontStack[StackIndex].Free;
    Dec(StackIndex);
    end;
  end;
end;

{-------------FrameParseFile}
function FrameParseFile(FrameViewer: TFrameViewerBase; FrameSet: TObject;  ALoadStyle: LoadStyleType;
          const FName : string; Strings : TStrings; AStream: TStream;
          ABuffer : PChar; BuffSize: LongInt; AMetaEvent: TMetaType): boolean;
var
  FOpen: boolean;

  procedure Parse;
  var
    SetExit: boolean;
  begin
  SetExit := False;
  GetCh; {get the reading started}
  Next;
  repeat
    case Sy of
      FrameSetSy:
        begin
        DoFrameSet(FrameViewer, FrameSet, FName);
        Result := True;
        end;
      BaseSy: DoBase;
      TitleSy: DoTitle;
      BgSoundSy: DoSound;
      ScriptSy: begin DoScript(FrameViewer.FOnScript); Next; end;
      NoFramesSy:
        begin
        repeat
          Next;
        until (Sy = NoFramesEndSy) or (Sy = EofSy);
        Next;
        end;
      MetaSy: DoMeta(FrameSet);
      BodySy, HeadingSy, HRSy, TableSy, ImageSy, OLSy, ULSy, MenuSy, DirSy,
      PSy, PreSy, FormSy, AddressSy, BlockQuoteSy, DLSy:
        SetExit := True;
      else Next;
      end;
  until SetExit or (Sy = EofSy);
  end;

begin
LoadStyle := ALoadStyle;
CallingObject := FrameViewer;
IncludeEvent := FrameViewer.FOnInclude;
SoundEvent := FrameViewer.FOnSoundRequest; 
MetaEvent := AMetaEvent;
Attributes := TAttributeList.Create;
Back := #0;
Title := '';
Base := '';
BaseTarget := '';
InScript := False;
NoPSpace := False;
StackIndex := 0;

if LoadStyle = lsFile then
  begin
  {$I+}
  FOpen := False;
  Result := False;
  try
    GetMem(InBuff, TextBuffSize);
    try
      AssignFile(Inf, FName);
      SetTextBuf(Inf, InBuff^, TextBuffSize);
      Reset(Inf);
      FOpen := True;
      Parse;
    except
      On EParseError do;   {ignore this error}
    end;
  finally
    FreeMem(InBuff, TextBuffSize);
    if FOpen then CloseFile(Inf);
    Attributes.Free;
   end;   {finally}
  end
else if LoadStyle = lsStrings then
  begin
  TheStrings := Strings;
  try
    St := '';
    Chi := 1;
    LineNo := 0;
    Parse;
  except    {ignore error}
    end;
  Attributes.Free;
  end
else if LoadStyle = lsStream then
  begin
  Stream := AStream;
  try
    Stream.Seek(0, 0);
    St := '';
    Chi := 1;
    Parse;
  except    {ignore error}
    end;
  Attributes.Free;
  end
else if LoadStyle = lsBuffer then
  begin
  Buffer := ABuffer;
  BuffEnd := Buffer+BuffSize;
  try
    Parse;
  except    {ignore error}
    end;
  Attributes.Free;
  end;
end;

{----------------IsFrameFile:}
function IsFrameFile(ALoadStyle: LoadStyleType;
          const FName : string; Strings : TStrings; AStream: TStream;
          ABuffer : PChar; BuffSize: LongInt; FrameViewer: TObject): boolean;
var
  FOpen: boolean;

  function Parse: boolean;
  var
    SetExit: boolean;
  begin
  Result := False;
  SetExit := False;
  GetCh; {get the reading started}
  Next;
  repeat
    case Sy of
      FrameSetSy:
        begin
        Result := True;
        break;
        end;
      ScriptSy: begin DoScript(Nil); Next; end;  {to skip the script stuff}

      BodySy, HeadingSy, HRSy, TableSy, ImageSy, OLSy, ULSy, MenuSy, DirSy,
      PSy, PreSy, FormSy, AddressSy, BlockQuoteSy, DLSy:
        SetExit := True;
      else Next;
      end;
  until SetExit or (Sy = EofSy);
  end;

begin
LoadStyle := ALoadStyle;
CallingObject := FrameViewer;
SoundEvent := Nil;
Attributes := TAttributeList.Create;
Back := #0;
Title := '';
Base := '';
BaseTarget := '';
Result := False;
InScript := False;
NoPSpace := False;

if LoadStyle = lsFile then
  begin
  {$I+}
  FOpen := False;
  try
    GetMem(InBuff, TextBuffSize);
    try
      AssignFile(Inf, FName);
      SetTextBuf(Inf, InBuff^, TextBuffSize);
      Reset(Inf);
      FOpen := True;
      Result := Parse;
    except
      On EParseError do;   {ignore this error}
    end;

  finally
    FreeMem(InBuff, TextBuffSize);
    Attributes.Free;
    if FOpen then CloseFile(Inf);
   end;   {finally}
  end
else if LoadStyle = lsStrings then
  begin
  TheStrings := Strings;
  try
    St := '';
    Chi := 1;
    LineNo := 0;
    Result := Parse;
  except    {ignore error}
    end;
  Attributes.Free;
  end
else if LoadStyle = lsStream then
  begin
  Stream := AStream;
  try
    Stream.Seek(0, 0);
    St := '';
    Chi := 1;
    Result := Parse;
  except    {ignore error}
    end;
  Attributes.Free;
  end
else if LoadStyle = lsBuffer then
  begin
  Buffer := ABuffer;
  BuffEnd := Buffer+BuffSize;
  try
    Result := Parse;
  except    {ignore error}
    end;
  Attributes.Free;
  end;
end;

end.
