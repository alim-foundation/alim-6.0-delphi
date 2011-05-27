unit ArabicKeyboardForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Htmlview, RXSpin, Mask, ToolEdit, CurrEdit, Arabic,
  RXCombos;

type
  TArabicKeyboard = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    BtnBackspace: TButton;
    Button16: TButton;
    Button17: TButton;
    Button18: TButton;
    Button19: TButton;
    Button20: TButton;
    Button21: TButton;
    Button22: TButton;
    Button23: TButton;
    Button24: TButton;
    Button26: TButton;
    Button27: TButton;
    Button25: TButton;
    Button28: TButton;
    Button29: TButton;
    Button30: TButton;
    Button31: TButton;
    Button32: TButton;
    Button33: TButton;
    Button34: TButton;
    Button35: TButton;
    Button36: TButton;
    Button37: TButton;
    Button38: TButton;
    Button39: TButton;
    Button40: TButton;
    Button41: TButton;
    Button42: TButton;
    Button43: TButton;
    Button44: TButton;
    Button45: TButton;
    Button46: TButton;
    Button47: TButton;
    Button48: TButton;
    Button49: TButton;
    Button50: TButton;
    btnSpace: TButton;
    Button52: TButton;
    Button54: TButton;
    Button55: TButton;
    Button56: TButton;
    Button57: TButton;
    RadioLetterPosition: TRadioGroup;
    SpnEdtFontSize: TRxSpinEdit;
    Label1: TLabel;
    EdtArabicText: TMemo;
    btnClear: TButton;
    btnOk: TButton;
    btnCancel: TButton;
    FontsCombo: TComboBox;
    Label2: TLabel;
    procedure RadioLetterPositionClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SpnEdtFontSizeChange(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LetterButtonClick(Sender: TObject);
    procedure FontsComboChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FEncodings : TArabicEncodings;
    FActiveText : String;
    FActiveFontSize : Integer;
    FActiveFont : String;
    FLetterButtons : TStringList;
    procedure CreateButtonDict;
    procedure SetupKeyboard(const AContext : TAraLetterContext; const ShowLatin : Boolean = False);
    function GetActiveText : String;
    procedure SetActiveText(const AText : String);
    procedure SetActiveFont(const AIndex : Integer);
    procedure SetEncodings(AEncodings : TArabicEncodings);
  public
    property ArabicText : String read GetActiveText write SetActiveText;
    property Encodings : TArabicEncodings read FEncodings write SetEncodings;
  end;

var
  ArabicKeyboard : TArabicKeyboard;

implementation

{$R *.DFM}

uses StStrS;

function CreateCharMapHtml : String;
const
  ColsPerRow = 5;
var
  I, Cols : Integer;
  Html, Line, S : String;
begin
  Html := '<tr>';
  Cols := 0;
  for I := 27 to 255 do begin
    S := Char(I);
    Line := Format('<td><font face="arial" size=1>%d</font> %s</td>', [I, S]);
    Html := Html + Line;

    Inc(Cols);
    if Cols > ColsPerRow then begin
      Html := Html + '</tr><tr>';
      Cols := 0;
    end;
  end;
  Html := '<table border=1>' + Html + '</tr></table>';
end;

procedure TArabicKeyboard.SetEncodings(AEncodings : TArabicEncodings);
var
  I : Integer;
begin
  if FEncodings = AEncodings then
    Exit;

  FEncodings := AEncodings;
  CreateButtonDict;

  FActiveFontSize := 24;
  SpnEdtFontSize.AsInteger := FActiveFontSize;
  SetupKeyboard(lcIsolated);

  FActiveFont := '';

  for I := 0 to FEncodings.AvailableFonts.Count-1 do
    if Screen.Fonts.IndexOf(FEncodings.AvailableFonts[I]) >= 0 then
      FontsCombo.Items.Add(FEncodings.AvailableFonts[I]);

  SetActiveFont(0);
end;

procedure TArabicKeyboard.SetActiveFont(const AIndex : Integer);
begin
  if FontsCombo.ItemIndex <> AIndex then FontsCombo.ItemIndex := AIndex;
  FActiveFont := FontsCombo.Items[AIndex];
  EdtArabicText.Font.Name := FActiveFont;
  SetupKeyboard(TAraLetterContext(RadioLetterPosition.ItemIndex));
end;

procedure TArabicKeyboard.CreateButtonDict;
var
  I : Integer;
  Button : TButton;
  AraLetter : TAraAlphaId;
begin
  Assert(FEncodings <> Nil);
  if FEncodings.ActiveKeyboardMap = Nil then
    Exit;

  FLetterButtons := TStringList.Create;
  FLetterButtons.Sorted := True;
  FLetterButtons.Duplicates := dupError;

  for I := 0 to ComponentCount-1 do begin
    if not(Components[I] is TButton) then
      continue;

    Button := Components[I] as TButton;
    if Button.Caption = '' then begin
      Button.Enabled := False;
      continue;
    end;

    case Button.Tag of
      -1 : { ignore these buttons (like Backspace, Space, etc) } ;
       0 : begin
             AraLetter := FEncodings.ActiveKeyboardMap^[Button.Caption[1]];
             if AraLetter <> aaUnknown then
               FLetterButtons.AddObject(Button.Caption, Button)
             else
               Button.Enabled := False;
           end;
       1 : FLetterButtons.AddObject(Button.Caption, Button);      { take these as is }
    end;
  end;
end;

procedure TArabicKeyboard.SetupKeyboard(const AContext : TAraLetterContext; const ShowLatin : Boolean);
var
  B : Integer;
  Letter : Char;
  AraLetter : TAraAlphaId;
  Button : TButton;
begin
  Assert(FEncodings <> Nil);
  if (FEncodings.ActiveCharMap = Nil) or (FEncodings.ActiveKeyboardMap = Nil) then
    Exit;

  for B := 0 to FLetterButtons.Count-1 do begin
    Button := FLetterButtons.Objects[B] as TButton;
    Letter := FLetterButtons.Strings[B][1];
    if Letter in ['0'..'9'] then
      Button.Caption := Letter
    else begin
      if ShowLatin then
        Button.Caption := Letter
      else begin
        AraLetter := FEncodings.ActiveKeyboardMap^[FLetterButtons.Strings[B][1]];
        Button.Caption := Char(FEncodings.ActiveCharMap^[AraLetter].ContextChars[AContext]);
      end;
    end;
    if ShowLatin then begin
      Button.Font.Name := 'Arial';
      Button.Font.Style := [];
      Button.Font.Size := 14;
    end else begin
      Button.Font.Name := FActiveFont;
      Button.Font.Style := [fsBold];
      Button.Font.Size := FActiveFontSize;
    end;
  end;

  EdtArabicText.Font.Name := FActiveFont;
  EdtArabicText.Font.Style := [fsBold];
  EdtArabicText.Font.Size := FActiveFontSize;
end;

function TArabicKeyboard.GetActiveText : String;
begin
  Result := FEncodings.ToArabic(FActiveText);
end;

procedure TArabicKeyboard.SetActiveText(const AText : String);
begin
  FActiveText := AText;
  EdtArabicText.Lines.Text := FEncodings.ToArabic(FActiveText);
  EdtArabicText.SelStart := 0;
end;

procedure TArabicKeyboard.RadioLetterPositionClick(Sender: TObject);
begin
  if (RadioLetterPosition.ItemIndex >= Ord(Low(TAraLetterContext))) and
     (RadioLetterPosition.ItemIndex <= Ord(High(TAraLetterContext))) then
    SetupKeyboard(TAraLetterContext(RadioLetterPosition.ItemIndex));
end;

procedure TArabicKeyboard.FormDestroy(Sender: TObject);
begin
  if FLetterButtons <> Nil then FLetterButtons.Free;
end;

procedure TArabicKeyboard.SpnEdtFontSizeChange(Sender: TObject);
begin
   FActiveFontSize := SpnEdtFontSize.AsInteger;
   SetupKeyboard(TAraLetterContext(RadioLetterPosition.ItemIndex));
end;

procedure TArabicKeyboard.FormKeyPress(Sender: TObject; var Key: Char);
var
  KeyIdx : Integer;
begin
  if (Key = ' ') or FLetterButtons.Find(Key, KeyIdx) then
    SetActiveText(Key + FActiveText);
end;

procedure TArabicKeyboard.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_BACK) or (Key = VK_DELETE) then begin
    if EdtArabicText.SelLength > 0 then
      System.Delete(FActiveText, EdtArabicText.SelStart, EdtArabicText.SelLength)
    else
      System.Delete(FActiveText, 1, 1);
    SetActiveText(FActiveText);
    Key := 0;
  end;
  if Key = VK_CONTROL then
    SetupKeyboard(TAraLetterContext(RadioLetterPosition.ItemIndex));
end;

procedure TArabicKeyboard.LetterButtonClick(Sender: TObject);
var
  I : Integer;
begin
  if Sender = btnSpace then
    FActiveText := ' ' + FActiveText
  else if Sender = btnBackspace then
    System.Delete(FActiveText, 1, 1)
  else if Sender = btnClear then
    FActiveText := ''
  else begin
    I := FLetterButtons.IndexOfObject(Sender);
    if I >= 0 then
      FActiveText := FLetterButtons[I] + FActiveText;
  end;
  SetActiveText(FActiveText);
end;

procedure TArabicKeyboard.FontsComboChange(Sender: TObject);
begin
  SetActiveFont(FontsCombo.ItemIndex);
end;

procedure TArabicKeyboard.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_CONTROL then
    SetupKeyboard(TAraLetterContext(RadioLetterPosition.ItemIndex), True);
end;

end.
