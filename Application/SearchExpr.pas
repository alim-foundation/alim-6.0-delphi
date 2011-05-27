unit SearchExpr;

interface

uses Classes, SysUtils;

const
  MaxStack      = 100;
  MaxExprTokens = 100;
  MaxPrecedence = 6;
  NoPrecedence  = 1;
  WildMatchChar = '*';

  // search expression token flags
  setfNotSearch  = $0001;     // flag set in token if search should be "negative" search
  setfLikeSearch = $0002;     // flag set in token if search should be a "likeness" search (soundex)
  setfStemSearch = $0004;     // flag set in token if search should be a "word stem" search (xyz*)

type
  TSchExprPrecedence = 1..MaxPrecedence;

  TSchExprTokenKind = (tkWord, tkPhrase, tkFlagOp, tkUnaryOp, tkBinaryOp, tkLeftparen, tkRightparen, tkEndExpression);
  TSchExprToken =
    record
      Text : String;
      Kind : TSchExprTokenKind;
      Code : Integer;
      Priority : TSchExprPrecedence;
      Flags : Word;
      UserData : Pointer;
    end;
  TSchExprTokens = array of TSchExprToken;

  TSchExprValue = Pointer;
  ISchExprValueManager = interface
    function CreateValue(var T : TSchExprToken) : TSchExprValue;
    function GetSimpleResult(var T : TSchExprToken) : TSchExprValue;
    function GetUnaryOpResult(var T : TSchExprToken; const X : TSchExprValue) : TSchExprValue;
    function GetBinaryOpResult(var T : TSchExprToken; const X, Y : TSchExprValue) : TSchExprValue;
    procedure DisposeValue(const AValue : TSchExprValue);
  end;

  TSearchExpression = class(TObject)
  protected
    FValueMgr : ISchExprValueManager;
    FImplicitOp : String;
    FUnsupportedOps : TStringList;
    FInputString : String;
    FInfixExpr : TSchExprTokens;
    FPostfixExpr : TSchExprTokens;
    FExprResult : TSchExprValue;
    FOperandsOnly : TStringList;

    function IsOperator(const AWord : String; var Token : TSchExprToken) : Boolean; virtual;

    procedure ScanInfix; virtual;
    procedure CreatePostfix; virtual;

    procedure RaiseException(const ACode, APos : Cardinal); virtual;
    procedure SetExpression(const AExpr : String); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Execute; virtual;
    procedure DisposeResult; virtual;

    property Infix : TSchExprTokens read FInfixExpr;
    property Postfix : TSchExprTokens read FPostfixExpr;
    property Result : TSchExprValue read FExprResult;
    property Expression : String read FInputString write SetExpression;
    property ValueManager : ISchExprValueManager read FValueMgr write FValueMgr;
    property UnsupportedOps : TStringList read FUnsupportedOps;
    property ImplicitOp : String read FImplicitOp write FImplicitOp;
    property HighlightList : TStringList read FOperandsOnly;
  end;

const
  tcLeftParen   =  2; // should always be equal to 2
  tcRightParen  =  3; // should always be equal to 3
  tcNot         = 10;
  tcLike        = 11;
  tcAnd         = 12;
  tcOr          = 13;
  tcNear        = 14;

implementation

uses IslUtils, StUtils;

const
  ecMismatchedParen     =  1;
  ecIncompleteExpr      =  2;
  ecBinOpIllegalPos     =  3;
  ecBinOpExpected       =  4;
  ecOpFollowsOp         =  5;
  ecUnrecogSymbol       =  6;
  ecWrongExpr           =  7;
  ecBinOpOrParenExp     =  8;
  ecNoLeftParen         =  9;
  ecStackOverflow       = 10;
  ecStackEmpty          = 11;
  ecCalcError           = 12;
  ecParmsOverflow       = 13;
  ecImplicitOpNotFound  = 14;
  ecFlagOpTokenNotFound = 15;

resourcestring
  rsExpressionErrorPrefix = 'Search expression error:';
  rsMismatchedParen       = 'mismatched parenthesis';
  rsIncompleteExpr        = 'incomplete expression';
  rsBinOpIllegalPos       = 'binary operator found in an illegal position';
  rsBinOpExpected         = 'binary operator expected';
  rsOpFollowsOp           = 'operator can not follow another operator';
  rsUnrecogSymbol         = 'unrecognized symbol';
  rsWrongExpr             = 'invalid expression';
  rsBinOpOrParenExp       = 'binary operator or parenthesis expected';
  rsNoLeftParen           = 'no left parenthesis found';
  rsStackOverflow         = 'stack overflow';
  rsStackEmpty            = 'stack is empty';
  rsCalcError             = 'expression calculation error (items still on stack)';
  rsParmsOverflow         = 'parameters overflow';
  rsImplicitOpNotFound    = 'implicit operator not found';
  rsOperatorNotSupport    = '"%s" is not supported at this time';
  rsFlagOpTokenNotFound   = 'no token found to apply flag to';

constructor TSearchExpression.Create;
begin
  inherited Create;
  FOperandsOnly := TStringList.Create;
  FOperandsOnly.Sorted := True;
  FOperandsOnly.Duplicates := dupIgnore;

  FUnsupportedOps := TStringList.Create;
  FUnsupportedOps.Sorted := True;
  FUnsupportedOps.Duplicates := dupIgnore;
  FImplicitOp := 'and';
end;

destructor TSearchExpression.Destroy;
begin
  DisposeResult;
  FUnsupportedOps.Free;
  FOperandsOnly.Free;
  inherited Destroy;
end;

function TSearchExpression.IsOperator(const AWord : String; var Token : TSchExprToken) : Boolean;
var
  TokenId : Integer;

  procedure SetTokenValues(const AKind : TSchExprTokenKind; ACode, APriority : Integer);
  begin
    Token.Kind := AKind; Token.Code := ACode; Token.Priority := APriority;
  end;

begin
  if FUnsupportedOps.IndexOf(AWord) <> -1 then
    raise Exception.CreateFmt(rsOperatorNotSupport, [AWord]);

  TokenId := FindString(AWord, ['and', 'or', 'not', 'like', 'near', '(', ')']);
  Token.Text := AWord;
  if TokenId = -1 then begin
    Token.Kind := tkWord;
    if AWord[Length(AWord)] = '*' then begin
      SetFlag(Token.Flags, setfStemSearch);
      Delete(Token.Text, Length(Token.Text), 1);
    end;
    Result := False;
  end else begin
    Result := True;
    case TokenId of
      0 : SetTokenValues(tkBinaryOp, tcAnd, 4);
      1 : SetTokenValues(tkBinaryOp, tcOr, 3);
      //2 : SetTokenValues(tkUnaryOp, tcNot, 6);
      2 : begin SetTokenValues(tkFlagOp, tcNot, 6); Token.Flags := setfNotSearch; end;
      3 : begin SetTokenValues(tkFlagOp, tcLike, 6); Token.Flags := setfLikeSearch; end;
      4 : SetTokenValues(tkBinaryOp, tcNear, 5);
      5 : SetTokenValues(tkLeftParen, tcLeftParen, NoPrecedence);
      6 : SetTokenValues(tkRightparen, tcRightParen, NoPrecedence);
    end;
  end;
end;

procedure TSearchExpression.DisposeResult;
begin
  if FValueMgr <> Nil then
    FValueMgr.DisposeValue(FExprResult);
  FExprResult := Nil;
end;

procedure TSearchExpression.RaiseException(const ACode, APos : Cardinal);
var
  Msg : String;
begin
  case ACode of
    ecMismatchedParen     : Msg := rsMismatchedParen;
    ecIncompleteExpr      : Msg := rsIncompleteExpr;
    ecBinOpIllegalPos     : Msg := rsBinOpIllegalPos;
    ecBinOpExpected       : Msg := rsBinOpExpected;
    ecOpFollowsOp         : Msg := rsOpFollowsOp;
    ecUnrecogSymbol       : Msg := rsUnrecogSymbol;
    ecWrongExpr           : Msg := rsWrongExpr;
    ecBinOpOrParenExp     : Msg := rsBinOpOrParenExp;
    ecNoLeftParen         : Msg := rsNoLeftParen;
    ecStackOverflow       : Msg := rsStackOverflow;
    ecStackEmpty          : Msg := rsStackEmpty;
    ecCalcError           : Msg := rsCalcError;
    ecParmsOverflow       : Msg := rsParmsOverflow;
    ecImplicitOpNotFound  : Msg := rsImplicitOpNotFound;
    ecFlagOpTokenNotFound : Msg := rsFlagOpTokenNotFound;
  else
    Msg := 'Unknown code ' + IntToStr(ACode);
  end;
  raise Exception.CreateFmt('%s %s (%d)', [rsExpressionErrorPrefix, Msg, APos]);
end;

procedure TSearchExpression.ScanInfix;
const
  Whitespace = [' ', #9];
  WordChars = ['A'..'Z', 'a'..'z', '0'..'9', '*'];
  QuoteChars = ['"'];
var
  I, J, ExprLen : Integer;   // current size of stack
  ParenCount : Integer;      // parenthesis level
  StringLimit : Integer;     // length of string to parse
  Position : Integer;        // current index into string to parse
  ImplicitOperator : TSchExprToken; // when no operator found between words, this is it
  TokenToFlagFound : Boolean;

  procedure PutInfix(var T : TSchExprToken);
  // push a token onto the Infix stack
  begin
    ExprLen := ExprLen+1;
    FInfixExpr[ExprLen] := T;
  end;

  function Leading : Boolean;
  // True if another token is expected after current one is parsed
  begin
    if ExprLen = 0 then
      Result := False
    else
      Result := FInfixExpr[ExprLen].Kind in [tkFlagOp, tkUnaryOp, tkBinaryOp, tkLeftparen];
  end;

  procedure ReadToken;
  var
    ExprWord : String;
    Token : TSchExprToken;
  begin
    ExprWord := '';                      // clear the variable name string

    while (Position <= StringLimit) and (FInputString[Position] in WordChars) do begin
      ExprWord := ExprWord + FInputString[Position];
      Position := Position+1;
    end;

    FillChar(Token, SizeOf(Token), 0);
    if IsOperator(ExprWord, Token) then begin
      // if the previous token is another word, add an implicit operator
      if (ExprLen >= 0) and (Token.Kind in [tkFlagOp, tkUnaryOp]) then begin
        if (FInfixExpr[ExprLen].Kind in [tkWord, tkPhrase]) then
          PutInfix(ImplicitOperator);
      end;

      if Leading then begin                           // are we expecting something?
        if Token.Kind = tkBinaryOp then
          RaiseException(ecBinOpIllegalPos, Position)
        else                                     // got it, stack it
          PutInfix(Token);
      end else if not (Token.Kind in [tkFlagOp, tkUnaryOp, tkBinaryOp]) then // expecting something, not found
        RaiseException(ecBinOpExpected, Position)
      else
        PutInfix(Token);                         // got it, stack it
    end else begin
      // if the previous token is another word, add an implicit operator
      if ExprLen >= 0 then begin
        if (FInfixExpr[ExprLen].Kind in [tkWord, tkPhrase]) then
          PutInfix(ImplicitOperator);
      end;
      PutInfix(Token);                         // stack the new word
      FOperandsOnly.Add(Token.Text);
    end;
  end;

  procedure ReadSymbol;
  // look for a symbol (+, -, /, etc.)
  var
    Token : TSchExprToken;
  begin
    FillChar(Token, SizeOf(Token), 0);
    if IsOperator(FInputString[Position], Token) then begin
      if Leading then begin
        if Token.Kind = tkRightParen then   // don't want a right parenthesis
          RaiseException(ecWrongExpr, Position)
        else
          PutInfix(Token);               // everything ok, stack operator
      end else begin
        if Token.Kind in [tkRightParen, tkBinaryOp] then
          PutInfix(Token)
        else
          RaiseException(ecBinOpOrParenExp, Position);
      end;
    end else
      RaiseException(ecUnrecogSymbol, Position);

    if Token.Kind = tkLeftParen then        // check for matching parenthesis
      ParenCount := ParenCount + 1
    else if Token.Kind = tkRightParen then begin
      ParenCount := ParenCount - 1;
      if ParenCount < 0 then
        RaiseException(ecNoLeftParen, Position);
    end;
    Position := Position + 1;
  end;

begin
  FInputString := FInputString + ' ';
  StringLimit := Length(FInputString);
  SetLength(FInfixExpr, MaxExprTokens);
  FOperandsOnly.Clear;

  FillChar(ImplicitOperator, SizeOf(ImplicitOperator), 0);
  if not IsOperator(FImplicitOp, ImplicitOperator) then
    RaiseException(ecImplicitOpNotFound, 0);

  ExprLen := -1;        // nothing in the stack
  ParenCount := 0;      // no parenthesis yet
  Position := 1;        // begin at first character of input string

  while Position <= StringLimit do begin
    if FInputString[Position] in Whitespace then
      Position := Position + 1
    else if FInputString[Position] in WordChars then
      ReadToken
    else
      ReadSymbol;
  end;

  if (ParenCount <> 0) or (Leading) then begin    // check parenthesis count
    if ParenCount <> 0 then
      RaiseException(ecMismatchedParen, Position);
    if Leading then
      RaiseException(ecIncompleteExpr, Position);
  end;

  SetLength(FInfixExpr, ExprLen+1);

  // flagOps apply flags to the token next to them, so apply the flags now
  // -- then, we can just ignore Flag token when it appears the CreatePostfix method
  for I := 0 to High(FInfixExpr) do begin
    if FInfixExpr[I].Kind = tkFlagOp then begin
      TokenToFlagFound := False;
      if I < High(FInfixExpr) then begin
        for J := Succ(I) to High(FInfixExpr) do begin
          if FInfixExpr[J].Kind in [tkWord, tkPhrase] then begin
            FInfixExpr[J].Flags := FInfixExpr[J].Flags or FInfixExpr[I].Flags;
            TokenToFlagFound := True;
            break;
          end;
        end;
      end;
      if not TokenToFlagFound then
        RaiseException(ecFlagOpTokenNotFound, I);
    end;
  end;
end;

procedure TSearchExpression.CreatePostfix;
var
  InfixIndex,                // index to Infix stack
  PostIndex : Integer;       // index to Postfix stack
  InfixLen : Integer;        // the length of the infix stack
  T, X : TSchExprToken;             // temporary token variables
  EndRight : Boolean;
  TStackIdx : Integer;       // token stack index
  TokenStack : TSchExprTokens;      // actual token stack

  procedure GetInfix(var T : TSchExprToken);
  // pop infix token from infix stack
  begin
    if InfixIndex < InfixLen then begin
      T := FInfixExpr[InfixIndex];
      Inc(InfixIndex);
    end else
      RaiseException(ecParmsOverflow, 0)
  end;

  procedure PutToken(T : TSchExprToken);
  // push postfix token onto postfix stack
  begin
    PostIndex := PostIndex+1;
    FPostFixExpr[PostIndex] := T;
  end;

  procedure PushToken(T : TSchExprToken);
  // push a token onto the token stack
  begin
    if TStackIdx < MaxStack then begin
      TStackIdx := TStackIdx + 1;
      TokenStack[TStackIdx] := T;
    end else
      RaiseException(ecStackOverflow, 0)
  end;

  procedure PopToken(var T : TSchExprToken);
  // pop a token from the token stack
  begin
    if TStackIdx < 0 then
      RaiseException(ecStackEmpty, 0)
    else begin
      T := TokenStack[TStackIdx];
      TStackIdx := TStackIdx - 1;
    end;
  end;

begin
  SetLength(TokenStack, MaxStack);
  SetLength(FPostfixExpr, MaxExprTokens);

  InfixIndex := 0;                       // starting infix index
  InfixLen := Length(FInfixExpr);        // the length of the infix stack
  PostIndex := -1;                       // no elements in postfix stack
  TStackIdx := -1;                       // no tokens in token stack

  while InfixIndex < InfixLen do begin
    GetInfix(T);                         // read a token
    case T.Kind of
      tkWord, tkPhrase :                    // operand, just stack it
        PutToken(T);
      tkLeftparen :                  // left parenthesis, stack name
        PushToken(T);
      tkRightparen :                 // pop till left paren found
        begin
          PopToken(T);                     // get a token
          while T.Kind <> tkLeftParen do begin
            PutToken(T);                   // put it into the postfix stack
            PopToken(T);                   // get the next token
          end;
        end;
      tkFlagOp : ; // just ignore it, we've taken care of it in ScanInfix
      tkUnaryOp, tkBinaryOp :
        begin
          repeat
            if TStackIdx = -1 then             // no tokens are available
              EndRight := True
            else begin
              if TokenStack[TStackIdx].Kind = tkLeftParen then
                EndRight := True
              else begin
                if TokenStack[TStackIdx].Priority < T.Priority then
                  EndRight := True
                else if (TokenStack[TStackIdx].Priority = T.Priority) and (T.Priority = MaxPrecedence) then
                  EndRight := True
                else begin
                  EndRight := False;
                  PopToken(X);             // get token from token stack
                  PutToken(X);             // put it into the postfix stack
                end;
              end;
            end;
          until EndRight;
          PushToken(T);
        end;
    end;
  end;

  while TStackIdx >= 0 do begin
    PopToken(X);
    PutToken(X);
  end;

  SetLength(FPostfixExpr, PostIndex+1);
end;

procedure TSearchExpression.Execute;
var
  PostIndex : Integer;         // index into postfix stack
  PostLen : Integer;
  X, Y : TSchExprValue;           // used for temporary value storage
  T : TSchExprToken;                  // current token
  NStackIdx : Integer;         // index into stack of values
  Stack : array of TSchExprValue; // stack of values

  procedure GetToken(var T : TSchExprToken);
  // pop token from postfix stack
  begin
    if PostIndex < PostLen then begin
      T := FPostFixExpr[PostIndex];
      Inc(PostIndex);;
    end else
      RaiseException(ecParmsOverflow, 0)
  end;

  procedure Push(const V : TSchExprValue);
  // push value onto value stack
  begin
    if NStackIdx > MaxStack then
      RaiseException(ecStackOverflow, 0)
    else begin
      NStackIdx := NStackIdx + 1;
      Stack[NStackIdx] := V;
    end;
  end;

  procedure Pop(var V : TSchExprValue);
  // pop value from value stack
  begin
    if NStackIdx < 0 then
      RaiseException(ecStackEmpty, 0)
    else begin
      V := Stack[NStackIdx];
      NStackIdx := NStackIdx - 1;
    end;
  end;

begin
  Assert(FValueMgr <> Nil, 'ValueManager property not assigned');
  SetLength(Stack, MaxStack);

  PostIndex := 0;                   // start at first postfix location
  PostLen := Length(FPostfixExpr);  // the number of tokens in the postfix
  NStackIdx := -1;                  // no values are stacked yet

  if PostLen = 1 then begin
    FExprResult := FValueMgr.GetSimpleResult(FPostfixExpr[0]);
    Exit;
  end;

  while PostIndex < PostLen do begin
    GetToken(T);
    case T.Kind of
      tkWord, tkPhrase :
        Push(FValueMgr.CreateValue(T));
      tkUnaryOp :
        begin
          Pop(X);
          Push(FValueMgr.GetUnaryOpResult(T, X));
          FValueMgr.DisposeValue(X);
        end;
      tkBinaryOp :
        begin
          Pop(Y);
          Pop(X);
          Push(FValueMgr.GetBinaryOpResult(T, X, Y));
          FValueMgr.DisposeValue(X);
          FValueMgr.DisposeValue(Y);
        end;
    end;
  end;

  if NStackIdx = 0 then
    Pop(FExprResult)
  else
    RaiseException(ecCalcError, 0);
end;

procedure TSearchExpression.SetExpression(const AExpr : String);
begin
  FInputString := AExpr;
  ScanInfix;
  CreatePostfix;
end;

end.
