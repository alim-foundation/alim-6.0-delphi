unit moneydialogs;

interface

uses Windows,Messages,SysUtils,Classes,Controls,StdCtrls,
     ExtCtrls,Graphics,Forms;

type
  TMoneyMsgDlgType = (mmtWarning, mmtError, mmtInformation,
  mmtConfirmation, mmtApplication,mmtNone);
  TMoneyMsgDlgBtn = (mmbYes, mmbNo, mmbOK, mmbCancel, mmbAbort, mmbRetry, mmbIgnore,
    mmbAll, mmbNoToAll, mmbYesToAll, mmbHelp);
  TMoneyMsgDlgButtons = set of TMoneyMsgDlgBtn;

function MoneyMessageDlgPos(const Msg: string; DlgType: TMoneyMsgDlgType;
  Buttons: TMoneyMsgDlgButtons; HelpCtx: Longint; X, Y: Integer; FocusBtnID:TMoneyMsgDlgBtn): Integer;
function MoneyMessageDlg(const Msg: string; DlgType: TMoneyMsgDlgType;
  Buttons: TMoneyMsgDlgButtons; HelpCtx: Longint; FocusBtnID:TMoneyMsgDlgBtn): Integer;

procedure MoneyShowMessagePos(const Msg: string; X, Y: Integer; FocusBtnID:TMoneyMsgDlgBtn);

procedure MoneyShowMessage(const Msg: string; FocusBtnID:TMoneyMsgDlgBtn);
procedure MoneyShowMessageFmt(const Msg: string; Params: array of const; FocusBtnID:TMoneyMsgDlgBtn);
function  MoneyInputQuery(const ACaption, APrompt: string;
  var Value: string): Boolean;
function  MoneyConfirm(const Msg:String; FocusBtnID:TMoneyMsgDlgBtn): Boolean;

implementation
uses moneyctrls;

type
 TMoneyMessageForm = class(TForm)
 private
    procedure HelpButtonClick(Sender: TObject);
 public
    constructor CreateNew(AOwner: TComponent);
 end;


function Max(I, J: Integer): Integer;
begin
  if I > J then Result := I else Result := J;
end;

function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;


constructor TMoneyMessageForm.CreateNew(AOwner: TComponent);
var
  NonClientMetrics: TNonClientMetrics;
begin
  inherited CreateNew(AOwner);
  Color:=$00C8DBDB;
  NonClientMetrics.cbSize := sizeof(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    Font.Handle := CreateFontIndirect(NonClientMetrics.lfMessageFont);
end;

procedure TMoneyMessageForm.HelpButtonClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

const
 IDI_APP = 'IDI_APP';
var
  MoneyDialogsCaptions: array[TMoneyMsgDlgType] of String =
  ('Warning', 'Error','Information', 'Confirm', '','');
  MoneyIconIDs: array[TMoneyMsgDlgType] of PChar = (IDI_EXCLAMATION, IDI_HAND,
    IDI_ASTERISK, IDI_QUESTION,IDI_APP,nil);
  MoneyButtonNames: array[TMoneyMsgDlgBtn] of string = (
    'mbnYes', 'mbnNo', 'mbnOK', 'mbnCancel', 'mbnAbort', 'mbnRetry',
    'mbnIgnore', 'mbnAll', 'mbnNoToAll','mbnYesToAll', 'mbnHelp');

  MoneyButtonCaptions: array[TMoneyMsgDlgBtn] of string = (
  '&Yes', '&No', '&OK', '&Cancel', '&Abort', '&Retry', '&Ignore',
  '&All', '&NoToAll',
    '&YesToAll', '&Help');


  ModalResults: array[TMoneyMsgDlgBtn] of Integer = (
    mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry, mrIgnore,
    mrAll, mrNoToAll, mrYesToAll, 0);

function CreateMoneyMessageDialog(const Msg: string; DlgType: TMoneyMsgDlgType;
  Buttons: TMoneyMsgDlgButtons; FocusBtnID:TMoneyMsgDlgBtn): TForm;
const
  mcHorzMargin = 8;
  mcVertMargin = 8;
  mcHorzSpacing = 10;
  mcVertSpacing = 10;
  mcButtonWidth = 50;
  mcButtonHeight = 14;
  mcButtonSpacing = 4;
var
  DialogUnits: TPoint;
  HorzMargin, VertMargin, HorzSpacing, VertSpacing, ButtonWidth,
  ButtonHeight, ButtonSpacing, ButtonCount, ButtonGroupWidth,
  IconTextWidth, IconTextHeight, X: Integer;
  B ,CancelButton: TMoneyMsgDlgBtn;
  FButton:TMoneyButton;
  IconID: PChar;
  TextRect: TRect;
begin
  Result := TMoneyMessageForm.CreateNew(Application);
  with Result do
  begin
    BorderStyle := bsDialog;
    Canvas.Font := Font;
    DialogUnits := GetAveCharSize(Canvas);
    HorzMargin := MulDiv(mcHorzMargin, DialogUnits.X, 4);
    VertMargin := MulDiv(mcVertMargin, DialogUnits.Y, 8);
    HorzSpacing := MulDiv(mcHorzSpacing, DialogUnits.X, 4);
    VertSpacing := MulDiv(mcVertSpacing, DialogUnits.Y, 8);
    ButtonWidth := MulDiv(mcButtonWidth, DialogUnits.X, 4);
    ButtonHeight := MulDiv(mcButtonHeight, DialogUnits.Y, 8);
    ButtonSpacing := MulDiv(mcButtonSpacing, DialogUnits.X, 4);
    SetRect(TextRect, 0, 0, Screen.Width div 2, 0);
    DrawText(Canvas.Handle, PChar(Msg), Length(Msg), TextRect,
      DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK);
    IconID := MoneyIconIDs[DlgType];
    IconTextWidth := TextRect.Right;
    IconTextHeight := TextRect.Bottom;
    if IconID <> nil then
    begin
      Inc(IconTextWidth, 32 + HorzSpacing);
      if IconTextHeight < 32 then IconTextHeight := 32;
    end;
    ButtonCount := 0;
    for B := Low(TMoneyMsgDlgBtn) to High(TMoneyMsgDlgBtn) do
      if B in Buttons then Inc(ButtonCount);
    ButtonGroupWidth := 0;
    if ButtonCount <> 0 then
      ButtonGroupWidth := ButtonWidth * ButtonCount +
        ButtonSpacing * (ButtonCount - 1);
    ClientWidth := Max(IconTextWidth, ButtonGroupWidth) + HorzMargin * 2;
    ClientHeight := IconTextHeight + ButtonHeight + VertSpacing +
      VertMargin * 2;
    Left := (Screen.Width div 2) - (Width div 2);
    Top := (Screen.Height div 2) - (Height div 2);
    if DlgType <> mmtNone then
      Caption := MoneyDialogsCaptions[DlgType] else
      Caption := Application.Title;
    if IconID <> nil then
      with TImage.Create(Result) do
      begin
        Name := 'Image';
        Parent := Result;
        if DlgType<>mmtApplication then
         Picture.Icon.Handle := LoadIcon(0, IconID)
        else
         Picture.Icon.Handle := Application.Icon.Handle;
        SetBounds(HorzMargin, VertMargin, 32, 32);
      end;
    with TLabel.Create(Result) do
    begin
      Name := 'Message';
      Parent := Result;
      WordWrap := True;
      Caption := Msg;
      BoundsRect := TextRect;
      SetBounds(IconTextWidth - TextRect.Right + HorzMargin, VertMargin,
        TextRect.Right, TextRect.Bottom);
    end;
    if mmbCancel in Buttons then CancelButton := mmbCancel else

    if mmbNo in Buttons then CancelButton := mmbNo else
        CancelButton := mmbOk;

    X := (ClientWidth - ButtonGroupWidth) div 2;
    for B := Low(TMoneyMsgDlgBtn) to High(TMoneyMsgDlgBtn) do
     if B in Buttons then
       begin
        FButton:=TMoneyButton.Create(Result);
        with FButton do
        begin
          Name := MoneyButtonNames[B];
          Parent := Result;
          Caption := (MoneyButtonCaptions[B]);
          ModalResult := ModalResults[B];
          if B = CancelButton then Cancel := True;
          SetBounds(X, IconTextHeight + VertMargin + VertSpacing,
            ButtonWidth, ButtonHeight);
          Inc(X, ButtonWidth + ButtonSpacing);
          if B = mmbHelp then
            OnClick := TMoneyMessageForm(Result).HelpButtonClick;
          if B = FocusBtnID then
           ActiveControl:=FButton;
        end;
       end;
  end;
end;

function MoneyMessageDlgPosHelp(const Msg: string; DlgType: TMoneyMsgDlgType;
  Buttons: TMoneyMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  const HelpFileName: string; FocusBtnID:TMoneyMsgDlgBtn): Integer;
begin
  with CreateMoneyMessageDialog(Msg, DlgType, Buttons,FocusBtnID) do
    try
      HelpContext := HelpCtx;
      HelpFile := HelpFileName;
      if X >= 0 then Left := X;
      if Y >= 0 then Top := Y;
      Result := ShowModal;
    finally
      Free;
    end;
end;

function MoneyMessageDlgPos(const Msg: string; DlgType: TMoneyMsgDlgType;
  Buttons: TMoneyMsgDlgButtons; HelpCtx: Longint; X, Y: Integer; FocusBtnID:TMoneyMsgDlgBtn): Integer;
begin
  Result := MoneyMessageDlgPosHelp(Msg, DlgType, Buttons, HelpCtx, X, Y, '',FocusBtnID);
end;

function MoneyMessageDlg(const Msg: string; DlgType: TMoneyMsgDlgType;
  Buttons: TMoneyMsgDlgButtons; HelpCtx: Longint; FocusBtnID:TMoneyMsgDlgBtn): Integer;
begin
  Result := MoneyMessageDlgPosHelp(Msg, DlgType, Buttons, HelpCtx, -1, -1, '',FocusBtnID);
end;


procedure MoneyShowMessagePos(const Msg: string; X, Y: Integer; FocusBtnID:TMoneyMsgDlgBtn);
begin
  MoneyMessageDlgPos(Msg, mmtNone, [mmbOK], 0, X, Y,FocusBtnID);
end;

procedure MoneyShowMessage(const Msg: string; FocusBtnID:TMoneyMsgDlgBtn);
begin
  MoneyShowMessagePos(Msg, -1, -1,FocusBtnID);
end;


procedure MoneyShowMessageFmt(const Msg: string; Params: array of const; FocusBtnID:TMoneyMsgDlgBtn);
begin
  MoneyShowMessage(Format(Msg, Params),FocusBtnID);
end;

function  MoneyConfirm(const Msg:String; FocusBtnID:TMoneyMsgDlgBtn): Boolean;
begin
 Result:=MoneyMessageDlg(Msg,mmtConfirmation,[mmbOk,mmbCancel],0,FocusBtnID)=mrOK;
end;

function MoneyInputQuery(const ACaption, APrompt: string;
  var Value: string): Boolean;
var
  Form: TForm;
  Prompt: TLabel;
  Edit: TEdit;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result := False;
  Form := TForm.Create(Application);
  with Form do
    try
      Canvas.Font := Font;
      Color:=$00C8DBDB;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      ClientWidth := MulDiv(180, DialogUnits.X, 4);
      ClientHeight := MulDiv(63, DialogUnits.Y, 8);
      Position := poScreenCenter;
      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Ctl3D:=False;
        Parent := Form;
        WordWrap:=True;
        //AutoSize := True;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        Width := MulDiv(164, DialogUnits.X, 4);
        Caption := APrompt;
      end;
      Edit := TEdit.Create(Form);
      with Edit do
      begin
        Parent := Form;
        Left := Prompt.Left;
        Top := Prompt.Top+Prompt.Height+5;//MulDiv(19, DialogUnits.Y, 8);
        Width := MulDiv(164, DialogUnits.X, 4);
        MaxLength := 255;
        Text := Value;
        SelectAll;
      end;
      ButtonTop := MulDiv(41, DialogUnits.Y, 8);
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TMoneyButton.Create(Form) do
      begin
        Parent := Form;
        Caption := '&OK';
        ModalResult := mrOk;
        SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;
      with TMoneyButton.Create(Form) do
      begin
        Parent := Form;
        Caption := '&Cancel';
        ModalResult := mrCancel;
        SetBounds(MulDiv(92, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;
      if ShowModal = mrOk then
      begin
        Value := Edit.Text;
        Result := True;
      end;
    finally
      Form.Free;
    end;
end;


end.
