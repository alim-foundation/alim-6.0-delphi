unit moneydbctrls;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  MoneyCtrls,StdCtrls,DB,DBCtrls;

type

  {MoneyDBCheckBox}
  TMoneyDBCheckBox = class(TMoneyCheckBox)
  private
    FDataLink: TFieldDataLink;
    FValueCheck: string;
    FValueUncheck: string;
    procedure DataChange(Sender: TObject);
    function  GetDataField: string;
    function  GetDataSource: TDataSource;
    function  GetField: TField;
    function  GetReadOnly: Boolean;
    function  GetFieldChecked:Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure SetValueCheck(const Value: string);
    procedure SetValueUncheck(const Value: string);
    procedure UpdateData(Sender: TObject);
    function  ValueMatch(const ValueList, Value: string): Boolean;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure Click; override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Checked;
    property Field: TField read GetField;
  published
    property Alignment;
    property Caption;
    property Color;
    property Ctl3D;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property ValueChecked: string read FValueCheck write SetValueCheck;
    property ValueUnchecked: string read FValueUncheck write SetValueUncheck;
    property Visible;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;


{ TMoneyDBLookupControl }
  TMoneyDBLookupControl = class;

  TMoneyDataSourceLink = class(TDataLink)
  private
    FDBLookupControl: TMoneyDBLookupControl;
  protected
    procedure FocusControl(Field: TFieldRef); override;
    procedure ActiveChanged; override;
    procedure RecordChanged(Field: TField); override;
  end;

  TMoneySourceLink = class(TDataLink)
  private
    FDBLookupControl: TMoneyDBLookupControl;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
  end;

  TMoneyDBLookupControl = class(TCustomControl)
  private
    FHeight:Integer;
    FLookupSource: TDataSource;
    FDataLink: TMoneyDataSourceLink;
    FListLink: TMoneySourceLink;
    FDataFieldName: string;
    FKeyFieldName: string;
    FListFieldName: string;
    FListFieldIndex: Integer;
    FDataField: TField;
    FMasterField: TField;
    FKeyField: TField;
    FListField: TField;
    FListFields: TList;
    FKeyValue: Variant;
    FSearchText: string;
    FLookupMode: Boolean;
    FListActive: Boolean;
    FFocused: Boolean;
    function CanModify: Boolean;
    procedure CheckNotCircular;
    procedure CheckNotLookup;
    procedure DataLinkActiveChanged;
    procedure DataLinkRecordChanged(Field: TField);
    function GetBorderSize: Integer;
    function GetDataSource: TDataSource;
    function GetKeyFieldName: string;
    function GetListSource: TDataSource;
    function GetReadOnly: Boolean;
    function GetTextHeight: Integer;
    procedure KeyValueChanged; virtual;
    procedure ListLinkActiveChanged; virtual;
    procedure ListLinkDataChanged; virtual;
    function LocateKey: Boolean;
    procedure ProcessSearchKey(Key: Char);
    procedure SelectKeyValue(const Value: Variant);
    procedure SetDataFieldName(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetKeyFieldName(const Value: string);
    procedure SetKeyValue(const Value: Variant);
    procedure SetListFieldName(const Value: string);
    procedure SetListSource(Value: TDataSource);
    procedure SetLookupMode(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure SetTextHeight(Value :Integer);
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    property DataField: string read FDataFieldName write SetDataFieldName;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property KeyField: string read GetKeyFieldName write SetKeyFieldName;
    property KeyValue: Variant read FKeyValue write SetKeyValue;
    property ListField: string read FListFieldName write SetListFieldName;
    property ListFieldIndex: Integer read FListFieldIndex write FListFieldIndex default 0;
    property ListSource: TDataSource read GetListSource write SetListSource;
    property ParentColor default False;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property TabStop default True;
    property TextHeight:Integer read GetTextHeight write SetTextHeight;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read FDataField;
  end;

{ TMoneyDBLookupListBox }

  TMoneyDBLookupListBox = class(TMoneyDBLookupControl)
  private
    FDrawBorder:Boolean;
    FSelectColor,FHighLightColor,
    FSecondColor,FBorderColor:TColor;
    FRecordIndex: Integer;
    FRecordCount: Integer;
    FRowCount: Integer;
    FBorderStyle: TBorderStyle;
    FPopup: Boolean;
    FKeySelected: Boolean;
    FTracking: Boolean;
    FTimerActive: Boolean;
    FLockPosition: Boolean;
    FMousePos: Integer;
    FSelectedItem: string;
    function GetKeyIndex: Integer;
    procedure KeyValueChanged; override;
    procedure ListLinkActiveChanged; override;
    procedure ListLinkDataChanged; override;
    procedure SelectCurrent;
    procedure SelectItemAt(X, Y: Integer);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetRowCount(Value: Integer);
    procedure StopTimer;
    procedure SetColor(Index:Integer; Value :TColor);
    procedure SetDrawBorder(Value:Boolean);
    procedure StopTracking;
    procedure TimerScroll;
    procedure UpdateScrollBar;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMCancelMode(var Message: TMessage); message WM_CANCELMODE;
    procedure WMTimer(var Message: TMessage); message WM_TIMER;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    property KeyValue;
    property SelectedItem: string read FSelectedItem;
  published
    property SelectColor:TColor index 1 read FSelectColor write SetColor;
    property SecondColor:TColor index 3 read FSecondColor write SetColor;
    property HighLightColor:TColor index 2 read FHighLightColor write SetColor;
    property BorderColor:TColor index 4 read FBorderColor write SetColor;
    property TextHeight;
    property Align;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property DrawBorder: Boolean read FDrawBorder write SetDrawBorder;
    property Color;
    property Ctl3D;
    property DataField;
    property DataSource;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property KeyField;
    property ListField;
    property ListFieldIndex;
    property ListSource;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RowCount: Integer read FRowCount write SetRowCount stored False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;


{ TMoneyDBLookupComboBox }

  TMoneyPopupDataList = class(TMoneyDBLookupListBox)
  private
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TDropDownAlign = (daLeft, daRight, daCenter);

  TMoneyDBLookupComboBox = class(TMoneyDBLookupControl)
  private
    FAlwaysShow:Boolean;
    FBitmapDownBlack,FBitmapDownWhite:TBitmap;
    FDataList: TMoneyPopupDataList;
    FButtonWidth: Integer;
    FText: string;
    FDropDownRows: Integer;
    FDropDownWidth: Integer;
    FDropDownAlign: TDropDownAlign;
    FListVisible: Boolean;
    FPressed: Boolean;
    FHasMouse:Boolean;
    FTracking: Boolean;
    FAlignment: TAlignment;
    FLookupMode: Boolean;
    FOnDropDown: TNotifyEvent;
    FOnCloseUp: TNotifyEvent;
    procedure SetAlwaysShow(Value:Boolean);
    procedure KeyValueChanged; override;
    procedure ListLinkActiveChanged; override;
    procedure ListLinkDataChanged; override;
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StopTracking;
    function GetColor(Index:Integer):TColor;
    procedure SetColor(Index:Integer; Value:TColor);
    procedure TrackButton(X, Y: Integer);
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure WMCancelMode(var Message: TMessage); message WM_CANCELMODE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
   protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CloseUp(Accept: Boolean);
    procedure DropDown;
    property  KeyValue;
    property  ListVisible: Boolean read FListVisible;
    property  Text: string read FText;
  published
    property Color;
    property SecondColor:TColor index 1 read GetColor write SetColor;
    property SelectColor:TColor index 2 read GetColor write SetColor;
    property HighLightColor:TColor index 3 read GetColor write SetColor;
    property AlwaysShow:Boolean read FAlwaysShow write SetAlwaysShow;
    property Ctl3D;
    property DataField;
    property DataSource;
    property DragCursor;
    property DragMode;
    property DropDownAlign: TDropDownAlign read FDropDownAlign write FDropDownAlign default daLeft;
    property DropDownRows: Integer read FDropDownRows write FDropDownRows default 7;
    property DropDownWidth: Integer read FDropDownWidth write FDropDownWidth default 0;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property KeyField;
    property ListField;
    property ListFieldIndex;
    property ListSource;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

  TCustomMoneyDBComboBox = class(TCustomComboBox)
  private
    FValues: TStrings;
    FEnableValues: Boolean;
    FDataLink: TFieldDataLink;
    FPaintControl: TPaintControl;
    procedure SetEnableValues(Value: Boolean);
    procedure SetValues(Value: TStrings);
    procedure ValuesChanged(Sender: TObject);
    procedure SetComboStyle(Value:TMoneyComboStyle);
    function  GetComboStyle:TMoneyComboStyle;
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetComboText: string;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetComboText(const Value: string);
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetEditReadOnly;
    procedure SetItems(Value: TStrings);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure Change; override;
    procedure Click; override;
    procedure ComboWndProc(var Message: TMessage; ComboWnd: HWnd;
      ComboProc: Pointer); override;
    procedure CreateWnd; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetStyle(Value: TComboboxStyle); override;
    procedure WndProc(var Message: TMessage); override;

    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property Values: TStrings read FValues write SetValues;
    property Style:TMoneyComboStyle read GetComboStyle write SetComboStyle;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Items write SetItems;
    property EnableValues: Boolean read FEnableValues write SetEnableValues;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;
    property Text;
  end;


  {MoneyDBComboBox}
  TMoneyDBComboBox = class(TCustomMoneyDBComboBox)
  private
    FCanvas:TCanvas;
    FBitmaps:array[1..2] of TBitmap;
    FButtonWidth: Integer;
    FDown :Boolean;
    FMouseInControl: Boolean;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMMouseEnter(var Msg:TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg:TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure InvalidateButton;
    function  ButtonRect:TRect;
    procedure DrawControlBorder(DC: HDC);
    procedure DrawButton(DC: HDC; ARect:TRect);
  protected
    procedure UpdateTracking;
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy; override;
  published
    property Style;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property ReadOnly;
    property Values;
    property DataField;
    property DataSource;
    property Items;
    property EnableValues;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
    property Color;
    property Ctl3D;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
  end;

  {MoneyDBDateEdit}
  TMoneyDBDateEdit = class(TMoneyDateEdit)
  private
    FDataLink: TFieldDataLink;
    function  GetDataField: string;
    function  GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    function  GetField: TField;
    procedure SetDataSource(Value: TDataSource);
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    function  GetReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure DoChange; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;
  published
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;


  {MoneyDBLinker}

  TMoneyDBLinkerDataLink = class;

  TMoneyDBLinker = class(TComponent)
  private
    FControls:array[1..9] of TControl;
    FDataLink:TMoneyDBLinkerDataLink;
    procedure SetControl(Index: Integer; Value :TControl);
    function  GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
  protected
    procedure DataChanged;
    procedure EditingChanged;
    procedure ActiveChanged;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property First:TControl index 1 read FControls[1] write SetControl;
    property Prev:TControl index 2 read FControls[2] write SetControl;
    property Next:TControl index 3 read FControls[3] write SetControl;
    property Last:TControl index 4 read FControls[4] write SetControl;
    property Insert:TControl index 5 read FControls[5] write SetControl;
    property Delete:TControl index 6 read FControls[6] write SetControl;
    property Edit:TControl index 7 read FControls[7] write SetControl;
    property Save:TControl index 8 read FControls[8] write SetControl;
    property Cancel:TControl index 9 read FControls[9] write SetControl;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;


  {TMoneyDBLinkerDataLink}

  TMoneyDBLinkerDataLink = class(TDataLink)
  private
    FDBLinker: TMoneyDBLinker;
  protected
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
  public
    constructor Create(AOwner: TMoneyDBLinker);
    destructor Destroy; override;
  end;


procedure Register;

implementation
uses  DBConsts;

{ TMoneyDBCheckBox }

constructor TMoneyDBCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FValueCheck := STextTrue;
  FValueUncheck := STextFalse;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TMoneyDBCheckBox.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TMoneyDBCheckBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TMoneyDBCheckBox.GetFieldChecked:Boolean;
var
  Text: string;
begin
  if FDatalink.Field <> nil then
    if FDataLink.Field.IsNull then
      Result := False
    else if FDataLink.Field.DataType = ftBoolean then
      if FDataLink.Field.AsBoolean then
        Result := True
      else
        Result := False
    else
    begin
      Result := False;
      Text := FDataLink.Field.Text;
      if ValueMatch(FValueCheck, Text) then Result := True else
        if ValueMatch(FValueUncheck, Text) then Result := False;
    end
  else
    Result := False;
end;

procedure TMoneyDBCheckBox.DataChange(Sender: TObject);
begin
  Checked:=GetFieldChecked;
end;

procedure TMoneyDBCheckBox.UpdateData(Sender: TObject);
var
  Pos: Integer;
  S: string;
begin
    if FDataLink.Field.DataType = ftBoolean then
      FDataLink.Field.AsBoolean := Checked
    else
    begin
      if Checked then S := FValueCheck else S := FValueUncheck;
      Pos := 1;
      FDataLink.Field.Text := ExtractFieldName(S, Pos);
    end;
end;

function TMoneyDBCheckBox.ValueMatch(const ValueList, Value: string): Boolean;
var
  Pos: Integer;
begin
  Result := False;
  Pos := 1;
  while Pos <= Length(ValueList) do
    if AnsiCompareText(ExtractFieldName(ValueList, Pos), Value) = 0 then
    begin
      Result := True;
      Break;
    end;
end;

procedure TMoneyDBCheckBox.Click;
begin
  if FDataLink.Edit then
  begin
    inherited Click;
    FDataLink.Modified;
  end;
end;

function TMoneyDBCheckBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TMoneyDBCheckBox.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TMoneyDBCheckBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TMoneyDBCheckBox.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TMoneyDBCheckBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TMoneyDBCheckBox.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TMoneyDBCheckBox.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TMoneyDBCheckBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #8, ' ':
      FDataLink.Edit;
    #27:
      FDataLink.Reset;
  end;
end;

procedure TMoneyDBCheckBox.SetValueCheck(const Value: string);
begin
  FValueCheck := Value;
  DataChange(Self);
end;

procedure TMoneyDBCheckBox.SetValueUncheck(const Value: string);
begin
  FValueUncheck := Value;
  DataChange(Self);
end;


procedure TMoneyDBCheckBox.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TMoneyDBCheckBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

////////////////////////////////////////////////////

procedure TMoneyDataSourceLink.ActiveChanged;
begin
  if FDBLookupControl <> nil then FDBLookupControl.DataLinkActiveChanged;
end;

procedure TMoneyDataSourceLink.RecordChanged(Field: TField);
begin
  if FDBLookupControl <> nil then FDBLookupControl.DataLinkRecordChanged(Field);
end;

procedure TMoneyDataSourceLink.FocusControl(Field: TFieldRef);
begin
  if (Field^ <> nil) and (Field^ = FDBLookupControl.Field) and
    (FDBLookupControl <> nil) and FDBLookupControl.CanFocus then
  begin
    Field^ := nil;
    FDBLookupControl.SetFocus;
  end;
end;

{ TMoneySourceLink }

procedure TMoneySourceLink.ActiveChanged;
begin
  if FDBLookupControl <> nil then FDBLookupControl.ListLinkActiveChanged;
end;

procedure TMoneySourceLink.DataSetChanged;
begin
  if FDBLookupControl <> nil then FDBLookupControl.ListLinkDataChanged;
end;

{ TMoneyDBLookupControl }

function VarEquals(const V1, V2: Variant): Boolean;
begin
  Result := False;
  try
    Result := V1 = V2;
  except
  end;
end;

var
  SearchTickCount: Integer = 0;

constructor TMoneyDBLookupControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHeight:=15;
  if NewStyleControls then
    ControlStyle := [csOpaque] else
    ControlStyle := [csOpaque, csFramed];
  ParentColor := False;
  TabStop := True;
  FLookupSource := TDataSource.Create(Self);
  FDataLink := TMoneyDataSourceLink.Create;
  FDataLink.FDBLookupControl := Self;
  FListLink := TMoneySourceLink.Create;
  FListLink.FDBLookupControl := Self;
  FListFields := TList.Create;
  FKeyValue := Null;
end;

destructor TMoneyDBLookupControl.Destroy;
begin
  FListFields.Free;
  FListLink.FDBLookupControl := nil;
  FListLink.Free;
  FDataLink.FDBLookupControl := nil;
  FDataLink.Free;
  inherited Destroy;
end;

function TMoneyDBLookupControl.CanModify: Boolean;
begin
  Result := FListActive and not ReadOnly and ((FDataLink.DataSource = nil) or
    (FMasterField <> nil) and FMasterField.CanModify);
end;

procedure TMoneyDBLookupControl.CheckNotCircular;
begin
  if FDataLink.Active and FDataLink.DataSet.IsLinkedTo(ListSource) then
    DatabaseError(SCircularDataLink);
  if FListLink.Active and FListLink.DataSet.IsLinkedTo(DataSource) then
    DatabaseError(SCircularDataLink);
end;

procedure TMoneyDBLookupControl.CheckNotLookup;
begin
  if FLookupMode then DatabaseError(SPropDefByLookup);
  if FDataLink.DataSourceFixed then DatabaseError(SDataSourceFixed);
end;

procedure TMoneyDBLookupControl.DataLinkActiveChanged;
begin
  FDataField := nil;
  FMasterField := nil;
  if FDataLink.Active and (FDataFieldName <> '') then
  begin
    CheckNotCircular;
    FDataField := GetFieldProperty(FDataLink.DataSet, Self, FDataFieldName);
    FMasterField := FDataField;
  end;
  SetLookupMode((FDataField <> nil) and (FDataField.FieldKind = fkLookup));
  DataLinkRecordChanged(nil);
end;

procedure TMoneyDBLookupControl.DataLinkRecordChanged(Field: TField);
begin
  if (Field = nil) or (Field = FMasterField) then
    if FMasterField <> nil then
      SetKeyValue(FMasterField.Value) else
      SetKeyValue(Null);
end;

function TMoneyDBLookupControl.GetBorderSize: Integer;
var
  Params: TCreateParams;
  R: TRect;
begin
  CreateParams(Params);
  SetRect(R, 0, 0, 0, 0);
  AdjustWindowRectEx(R, Params.Style, False, Params.ExStyle);
  Result := R.Bottom - R.Top;
end;

function TMoneyDBLookupControl.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TMoneyDBLookupControl.GetKeyFieldName: string;
begin
  if FLookupMode then Result := '' else Result := FKeyFieldName;
end;

function TMoneyDBLookupControl.GetListSource: TDataSource;
begin
  if FLookupMode then Result := nil else Result := FListLink.DataSource;
end;

function TMoneyDBLookupControl.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

function TMoneyDBLookupControl.GetTextHeight: Integer;
begin
  Result := FHeight;
end;

procedure TMoneyDBLookupControl.KeyValueChanged;
begin
end;

procedure TMoneyDBLookupControl.ListLinkActiveChanged;
var
  DataSet: TDataSet;
  ResultField: TField;
begin
  FListActive := False;
  FKeyField := nil;
  FListField := nil;
  FListFields.Clear;
  if FListLink.Active and (FKeyFieldName <> '') then
  begin
    CheckNotCircular;
    DataSet := FListLink.DataSet;
    FKeyField := GetFieldProperty(DataSet, Self, FKeyFieldName);
    try
      DataSet.GetFieldList(FListFields, FListFieldName);
    except
      DatabaseErrorFmt(SFieldNotFound, [Self.Name, FListFieldName]);
    end;
    if FLookupMode then
    begin
      ResultField := GetFieldProperty(DataSet, Self, FDataField.LookupResultField);
      if FListFields.IndexOf(ResultField) < 0 then
        FListFields.Insert(0, ResultField);
      FListField := ResultField;
    end else
    begin
      if FListFields.Count = 0 then FListFields.Add(FKeyField);
      if (FListFieldIndex >= 0) and (FListFieldIndex < FListFields.Count) then
        FListField := FListFields[FListFieldIndex] else
        FListField := FListFields[0];
    end;
    FListActive := True;
  end;
end;

procedure TMoneyDBLookupControl.ListLinkDataChanged;
begin
end;

function TMoneyDBLookupControl.LocateKey: Boolean;
begin
  Result := False;
  try
    if not VarIsNull(FKeyValue) and
      FListLink.DataSet.Locate(FKeyFieldName, FKeyValue, []) then
      Result := True;
  except
  end;
end;

procedure TMoneyDBLookupControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if (FDataLink <> nil) and (AComponent = DataSource) then DataSource := nil;
    if (FListLink <> nil) and (AComponent = ListSource) then ListSource := nil;
  end;
end;

procedure TMoneyDBLookupControl.ProcessSearchKey(Key: Char);
var
  TickCount: Integer;
  S: string;
begin
  if (FListField <> nil) and (FListField.FieldKind = fkData) and
    (FListField.DataType = ftString) then
    case Key of
      #8, #27: FSearchText := '';
      #32..#255:
        if CanModify then
        begin
          TickCount := GetTickCount;
          if TickCount - SearchTickCount > 2000 then FSearchText := '';
          SearchTickCount := TickCount;
          if Length(FSearchText) < 32 then
          begin
            S := FSearchText + Key;
            if FListLink.DataSet.Locate(FListField.FieldName, S,
              [loCaseInsensitive, loPartialKey]) then
            begin
              SelectKeyValue(FKeyField.Value);
              FSearchText := S;
            end;
          end;
        end;
    end;
end;

procedure TMoneyDBLookupControl.SelectKeyValue(const Value: Variant);
begin
  if FMasterField <> nil then
  begin
    if FDataLink.Edit then
      FMasterField.Value := Value;
  end else
    SetKeyValue(Value);
  Repaint;
  Click;
end;

procedure TMoneyDBLookupControl.SetDataFieldName(const Value: string);
begin
  if FDataFieldName <> Value then
  begin
    FDataFieldName := Value;
    DataLinkActiveChanged;
  end;
end;

procedure TMoneyDBLookupControl.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TMoneyDBLookupControl.SetKeyFieldName(const Value: string);
begin
  CheckNotLookup;
  if FKeyFieldName <> Value then
  begin
    FKeyFieldName := Value;
    ListLinkActiveChanged;
  end;
end;

procedure TMoneyDBLookupControl.SetKeyValue(const Value: Variant);
begin
  if not VarEquals(FKeyValue, Value) then
  begin
    FKeyValue := Value;
    KeyValueChanged;
  end;
end;

procedure TMoneyDBLookupControl.SetListFieldName(const Value: string);
begin
  if FListFieldName <> Value then
  begin
    FListFieldName := Value;
    ListLinkActiveChanged;
  end;
end;

procedure TMoneyDBLookupControl.SetListSource(Value: TDataSource);
begin
  CheckNotLookup;
  FListLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TMoneyDBLookupControl.SetLookupMode(Value: Boolean);
begin
  if FLookupMode <> Value then
    if Value then
    begin
      FMasterField := GetFieldProperty(FDataField.DataSet, Self, FDataField.KeyFields);
      FLookupSource.DataSet := FDataField.LookupDataSet;
      FKeyFieldName := FDataField.LookupKeyFields;
      FLookupMode := True;
      FListLink.DataSource := FLookupSource;
    end else
    begin
      FListLink.DataSource := nil;
      FLookupMode := False;
      FKeyFieldName := '';
      FLookupSource.DataSet := nil;
      FMasterField := FDataField;
    end;
end;

procedure TMoneyDBLookupControl.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

procedure TMoneyDBLookupControl.SetTextHeight(Value :Integer);
begin
 if FHeight<>Value then
 begin
  FHeight:=Value;
  Repaint;
 end;
end;

procedure TMoneyDBLookupControl.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

procedure TMoneyDBLookupControl.WMKillFocus(var Message: TMessage);
begin
  FFocused := False;
  Inherited;
  Invalidate;
end;

procedure TMoneyDBLookupControl.WMSetFocus(var Message: TMessage);
begin
  if FListActive and Enabled then
  FFocused := True;
  Inherited;
  Invalidate;
end;

{ TMoneyDBLookupListBox }

constructor TMoneyDBLookupListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csDoubleClicks];
  Width := 121;
  Color:=$00E0F0F0;
  FSelectColor:=$009EE0F1;
  FBorderColor:=$00BFCFCF;
  FHighLightColor:=clBlack;
  FSecondColor:=$00E0F0F0;
  FBorderStyle := bsSingle;
  RowCount := 7;
end;

procedure TMoneyDBLookupListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    if FBorderStyle = bsSingle then
      if NewStyleControls and Ctl3D then
        ExStyle := ExStyle or WS_EX_CLIENTEDGE
      else
        Style := Style or WS_BORDER;
end;

procedure TMoneyDBLookupListBox.CreateWnd;
begin
  inherited CreateWnd;
  UpdateScrollBar;
end;

function TMoneyDBLookupListBox.GetKeyIndex: Integer;
var
  FieldValue: Variant;
begin
  if not VarIsNull(FKeyValue) then
    for Result := 0 to FRecordCount - 1 do
    begin
      FListLink.ActiveRecord := Result;
      FieldValue := FKeyField.Value;
      FListLink.ActiveRecord := FRecordIndex;
      if VarEquals(FieldValue, FKeyValue) then Exit;
    end;
  Result := -1;
end;

procedure TMoneyDBLookupListBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  Delta, KeyIndex: Integer;
begin
  inherited KeyDown(Key, Shift);
  if CanModify then
  begin
    Delta := 0;
    case Key of
      VK_UP, VK_LEFT: Delta := -1;
      VK_DOWN, VK_RIGHT: Delta := 1;
      VK_PRIOR: Delta := 1 - FRowCount;
      VK_NEXT: Delta := FRowCount - 1;
      VK_HOME: Delta := -Maxint;
      VK_END: Delta := Maxint;
    end;
    if Delta <> 0 then
    begin
      FSearchText := '';
      if Delta = -Maxint then FListLink.DataSet.First else
        if Delta = Maxint then FListLink.DataSet.Last else
        begin
          KeyIndex := GetKeyIndex;
          if KeyIndex >= 0 then
            FListLink.DataSet.MoveBy(KeyIndex - FRecordIndex)
          else
          begin
            KeyValueChanged;
            Delta := 0;
          end;
          FListLink.DataSet.MoveBy(Delta);
        end;
      SelectCurrent;
    end;
  end;
end;

procedure TMoneyDBLookupListBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  ProcessSearchKey(Key);
end;

procedure TMoneyDBLookupListBox.KeyValueChanged;
begin
  if FListActive and not FLockPosition then
    if not LocateKey then FListLink.DataSet.First;
  if FListField <> nil then
    FSelectedItem := FListField.DisplayText else
    FSelectedItem := '';
end;

procedure TMoneyDBLookupListBox.ListLinkActiveChanged;
begin
  try
    inherited;
  finally
    if FListActive then KeyValueChanged else ListLinkDataChanged;
  end;
end;

procedure TMoneyDBLookupListBox.ListLinkDataChanged;
begin
  if FListActive then
  begin
    FRecordIndex := FListLink.ActiveRecord;
    FRecordCount := FListLink.RecordCount;
    FKeySelected := not VarIsNull(FKeyValue) or
      not FListLink.DataSet.BOF;
  end else
  begin
    FRecordIndex := 0;
    FRecordCount := 0;
    FKeySelected := False;
  end;
  if HandleAllocated then
  begin
    UpdateScrollBar;
    Invalidate;
  end;
end;

procedure TMoneyDBLookupListBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FSearchText := '';
    if not FPopup then
    begin
      SetFocus;
      if not FFocused then Exit;
    end;
    if CanModify then
      if ssDouble in Shift then
      begin
        if FRecordIndex = Y div GetTextHeight then DblClick;
      end else
      begin
        MouseCapture := True;
        FTracking := True;
        SelectItemAt(X, Y);
      end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TMoneyDBLookupListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FTracking then
  begin
    SelectItemAt(X, Y);
    FMousePos := Y;
    TimerScroll;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TMoneyDBLookupListBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if FTracking then
  begin
    StopTracking;
    SelectItemAt(X, Y);
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TMoneyDBLookupListBox.Paint;
var
  I, J, W, X, TextWidth, TextHeight, LastFieldIndex: Integer;
  S: string;
  R: TRect;
  Selected: Boolean;
  Field: TField;
begin
  Canvas.Font := Font;
  TextWidth := Canvas.TextWidth('0');
  TextHeight :=GetTextHeight;
  LastFieldIndex := FListFields.Count - 1;
  if ColorToRGB(Color) <> ColorToRGB(clBtnFace) then
    Canvas.Pen.Color := clBtnFace else
    Canvas.Pen.Color := clBtnShadow;
  for I := 0 to FRowCount - 1 do
  begin
    Canvas.Font.Color := Font.Color;
    if ((i mod 2) =1) then
     Canvas.Brush.Color :=FSecondColor
    else
     Canvas.Brush.Color :=Color;

    Selected := not FKeySelected and (I = 0);
    R.Top := I * TextHeight;
    R.Bottom := R.Top + TextHeight;
    if I < FRecordCount then
    begin
      FListLink.ActiveRecord := I;
      if not VarIsNull(FKeyValue) and
        VarEquals(FKeyField.Value, FKeyValue) then
      begin
        Canvas.Font.Color := FHighLightColor;
        Canvas.Brush.Color := FSelectColor;
        Selected := True;
      end;
      R.Right := 0;
      for J := 0 to LastFieldIndex do
      begin
        Field := FListFields[J];
        if J < LastFieldIndex then
          W := Field.DisplayWidth * TextWidth + 4 else
          W := ClientWidth - R.Right;
        S := Field.DisplayText;
        X := 2;
        case Field.Alignment of
          taRightJustify: X := W - Canvas.TextWidth(S) - 3;
          taCenter: X := (W - Canvas.TextWidth(S)) div 2;
        end;
        R.Left := R.Right;
        R.Right := R.Right + W;
        Canvas.TextRect(R, R.Left + X, R.Top, S);

        {if J < LastFieldIndex then
        begin }
          Canvas.MoveTo(R.Right, R.Top);
          Canvas.LineTo(R.Right, R.Bottom);
          Inc(R.Right);
          if R.Right >= ClientWidth then Break;
        {end;}
      end;
    end;
    R.Left := 0;
    R.Right := ClientWidth;
    if I >= FRecordCount then Canvas.FillRect(R);
    {Draw Border}
    if FDrawBorder then
    begin
     Canvas.Pen.Color := FBorderColor;
     Canvas.MoveTo(R.Left,R.Bottom-1);
     Canvas.LineTo(R.Right,R.Bottom-1);
    end;
    if Selected and (FFocused or FPopup) then Canvas.DrawFocusRect(R);
  end;
  if FRecordCount <> 0 then FListLink.ActiveRecord := FRecordIndex;

end;

procedure TMoneyDBLookupListBox.SelectCurrent;
begin
  FLockPosition := True;
  try
    SelectKeyValue(FKeyField.Value);
  finally
    FLockPosition := False;
  end;
end;

procedure TMoneyDBLookupListBox.SelectItemAt(X, Y: Integer);
var
  Delta: Integer;
begin
  if Y < 0 then Y := 0;
  if Y >= ClientHeight then Y := ClientHeight - 1;
  Delta := Y div GetTextHeight - FRecordIndex;
  FListLink.DataSet.MoveBy(Delta);
  SelectCurrent;
end;

procedure TMoneyDBLookupListBox.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
    RowCount := RowCount;
  end;
end;

procedure TMoneyDBLookupListBox.SetDrawBorder(Value:Boolean);
begin
 if FDrawBorder<>Value then
 begin
  FDrawBorder:=Value;
  Invalidate;
 end;
end;

procedure TMoneyDBLookupListBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  BorderSize, TextHeight, Rows: Integer;
begin
  BorderSize := GetBorderSize;
  TextHeight := GetTextHeight;
  Rows := (AHeight - BorderSize) div TextHeight;
  if Rows < 1 then Rows := 1;
  FRowCount := Rows;
  if FListLink.BufferCount <> Rows then
  begin
    FListLink.BufferCount := Rows;
    ListLinkDataChanged;
  end;
  {inherited SetBounds(ALeft, ATop, AWidth, Rows * TextHeight + BorderSize);}

  inherited SetBounds(ALeft, ATop, AWidth,AHeight);
end;

procedure TMoneyDBLookupListBox.SetRowCount(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if Value > 100 then Value := 100;
  Height := Value * GetTextHeight + GetBorderSize;
end;

procedure TMoneyDBLookupListBox.SetColor(Index:Integer; Value :TColor);
begin
 case Index of
  1:if FSelectColor<>Value then
    begin
     FSelectColor:=Value;
     Repaint;
    end;
  2:if FHighLightColor<>Value then
    begin
     FHighLightColor:=Value;
     Repaint;
    end;
  3:if FSecondColor<>Value then
    begin
     FSecondColor:=Value;
     Repaint;
    end;
  4:if FBorderColor<>Value then
    begin
     FBorderColor:=Value;
     Repaint;
    end;
  end;
end;

procedure TMoneyDBLookupListBox.StopTimer;
begin
  if FTimerActive then
  begin
    KillTimer(Handle, 1);
    FTimerActive := False;
  end;
end;

procedure TMoneyDBLookupListBox.StopTracking;
begin
  if FTracking then
  begin
    StopTimer;
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TMoneyDBLookupListBox.TimerScroll;
var
  Delta, Distance, Interval: Integer;
begin
  Delta := 0;
  Distance := 0;
  if FMousePos < 0 then
  begin
    Delta := -1;
    Distance := -FMousePos;
  end;
  if FMousePos >= ClientHeight then
  begin
    Delta := 1;
    Distance := FMousePos - ClientHeight + 1;
  end;
  if Delta = 0 then StopTimer else
  begin
    if FListLink.DataSet.MoveBy(Delta) <> 0 then SelectCurrent;
    Interval := 200 - Distance * 15;
    if Interval < 0 then Interval := 0;
    SetTimer(Handle, 1, Interval, nil);
    FTimerActive := True;
  end;
end;

procedure TMoneyDBLookupListBox.UpdateScrollBar;
var
  Pos, Max: Integer;
  ScrollInfo: TScrollInfo;
begin
  Pos := 0;
  Max := 0;
  if FRecordCount = FRowCount then
  begin
    Max := 4;
    if not FListLink.DataSet.BOF then
      if not FListLink.DataSet.EOF then Pos := 2 else Pos := 4;
  end;
  ScrollInfo.cbSize := SizeOf(TScrollInfo);
  ScrollInfo.fMask := SIF_POS or SIF_RANGE;
  if not GetScrollInfo(Handle, SB_VERT, ScrollInfo) or
    (ScrollInfo.nPos <> Pos) or (ScrollInfo.nMax <> Max) then
  begin
    ScrollInfo.nMin := 0;
    ScrollInfo.nMax := Max;
    ScrollInfo.nPos := Pos;
    SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
  end;
end;

procedure TMoneyDBLookupListBox.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then
  begin
    RecreateWnd;
    RowCount := RowCount;
  end;
  inherited;
end;

procedure TMoneyDBLookupListBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Height := Height;
end;

procedure TMoneyDBLookupListBox.WMCancelMode(var Message: TMessage);
begin
  StopTracking;
  inherited;
end;

procedure TMoneyDBLookupListBox.WMTimer(var Message: TMessage);
begin
  TimerScroll;
end;

procedure TMoneyDBLookupListBox.WMVScroll(var Message: TWMVScroll);
begin
  FSearchText := '';
  with Message, FListLink.DataSet do
    case ScrollCode of
      SB_LINEUP: MoveBy(-FRecordIndex - 1);
      SB_LINEDOWN: MoveBy(FRecordCount - FRecordIndex);
      SB_PAGEUP: MoveBy(-FRecordIndex - FRecordCount + 1);
      SB_PAGEDOWN: MoveBy(FRecordCount - FRecordIndex + FRecordCount - 2);
      SB_THUMBPOSITION:
        begin
          case Pos of
            0: First;
            1: MoveBy(-FRecordIndex - FRecordCount + 1);
            2: Exit;
            3: MoveBy(FRecordCount - FRecordIndex + FRecordCount - 2);
            4: Last;
          end;
        end;
      SB_BOTTOM: Last;
      SB_TOP: First;
    end;
end;


{ TXPopupDataList }

constructor TMoneyPopupDataList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable];
  FPopup := True;
end;

procedure TMoneyPopupDataList.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW;
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

procedure TMoneyPopupDataList.WMMouseActivate(var Message: TMessage);
begin
  Message.Result := MA_NOACTIVATE;
end;

{ TMoneyDBLookupComboBox }

constructor TMoneyDBLookupComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  Width := 145;
  Height := 0;
  ParentCtl3D:=False;
  Ctl3D:=False;
  FBitmapDownBlack:=TBitmap.Create;
  FBitmapDownBlack.Transparent:=True;
  FBitmapDownBlack.LoadFromResourceName(hInstance,'MSPIN_DOWNBLACK');
  FBitmapDownWhite:=TBitmap.Create;
  FBitmapDownWhite.Transparent:=True;
  FBitmapDownWhite.LoadFromResourceName(hInstance,'MSPIN_DOWNWHITE');
  FDataList := TMoneyPopupDataList.Create(Self);
  FDataList.SecondColor:=Color;
  FDataList.Visible := False;
  FDataList.Parent := Self;
  FDataList.OnMouseUp := ListMouseUp;
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL)+1;
  FDropDownRows := 7;
end;

destructor TMoneyDBLookupComboBox.Destroy;
begin
 FBitmapDownBlack.Free;
 FBitmapDownWhite.Free;
 inherited Destroy;
end;

procedure TMoneyDBLookupComboBox.CloseUp(Accept: Boolean);
var
  ListValue: Variant;
begin
  if FListVisible then
  begin
    if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    ListValue := FDataList.KeyValue;
    SetWindowPos(FDataList.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
    FListVisible := False;
    FDataList.ListSource := nil;
    Invalidate;
    FSearchText := '';
    if Accept and CanModify then SelectKeyValue(ListValue);
    if Assigned(FOnCloseUp) then FOnCloseUp(Self);
  end;
end;

procedure TMoneyDBLookupComboBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    if NewStyleControls and Ctl3D then
      ExStyle := ExStyle or WS_EX_CLIENTEDGE
    else
      Style := Style or WS_BORDER;
end;

procedure TMoneyDBLookupComboBox.DropDown;
var
  P: TPoint;
  I, Y: Integer;
  S: string;
begin
  if not FListVisible and FListActive then
  begin
    if Assigned(FOnDropDown) then FOnDropDown(Self);
    FDataList.Color := Color;
    FDataList.Font := Font;
    if FDropDownWidth > 0 then
      FDataList.Width := FDropDownWidth else
      FDataList.Width := Width;
    FDataList.ReadOnly := not CanModify;
    FDataList.RowCount := FDropDownRows;
    FDataList.KeyField := FKeyFieldName;
    for I := 0 to FListFields.Count - 1 do
      S := S + TField(FListFields[I]).FieldName + ';';
    FDataList.ListField := S;
    FDataList.ListFieldIndex := FListFields.IndexOf(FListField);
    FDataList.ListSource := FListLink.DataSource;
    FDataList.KeyValue := KeyValue;
    P := Parent.ClientToScreen(Point(Left, Top));
    Y := P.Y + Height;
    if Y + FDataList.Height > Screen.Height then Y := P.Y - FDataList.Height;
    case FDropDownAlign of
      daRight: Dec(P.X, FDataList.Width - Width);
      daCenter: Dec(P.X, (FDataList.Width - Width) div 2);
    end;
    SetWindowPos(FDataList.Handle, HWND_TOP, P.X, Y, 0, 0,
      SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
    FListVisible := True;
    Repaint;
  end;
end;

procedure TMoneyDBLookupComboBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  Delta: Integer;
begin
  inherited KeyDown(Key, Shift);
  if FListActive and ((Key = VK_UP) or (Key = VK_DOWN)) then
    if ssAlt in Shift then
    begin
      if FListVisible then CloseUp(True) else DropDown;
      Key := 0;
    end else
      if not FListVisible then
      begin
        if not LocateKey then
          FListLink.DataSet.First
        else
        begin
          if Key = VK_UP then Delta := -1 else Delta := 1;
          FListLink.DataSet.MoveBy(Delta);
        end;
        SelectKeyValue(FKeyField.Value);
        Key := 0;
      end;
  if (Key <> 0) and FListVisible then FDataList.KeyDown(Key, Shift);
end;

procedure TMoneyDBLookupComboBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if FListVisible then
    if Key in [#13, #27] then
      CloseUp(Key = #13)
    else
      FDataList.KeyPress(Key)
  else
    ProcessSearchKey(Key);
end;

procedure TMoneyDBLookupComboBox.KeyValueChanged;
begin
  if FLookupMode then
  begin
    FText := FDataField.DisplayText;
    FAlignment := FDataField.Alignment;
  end else
  if FListActive then
  begin
    FAlignment := FListField.Alignment;
    if FAlwaysShow then
     FText := FListField.DisplayText
    else
    begin
     if LocateKey then
      FText := FListField.DisplayText
     else
      FText:='';
    end;
  end else
  begin
    FText := '';
    FAlignment := taLeftJustify;
  end;
  Invalidate;
end;

procedure TMoneyDBLookupComboBox.ListLinkActiveChanged;
begin
  inherited;
  KeyValueChanged;
end;

{Added 12/16/98}

procedure TMoneyDBLookupComboBox.ListLinkDataChanged;
begin
  inherited;
  if not (Assigned(DataSource) or Assigned(Field)) then
  begin
   FText := FListField.DisplayText;
   Invalidate;
  end;
end;

procedure TMoneyDBLookupComboBox.SetAlwaysShow(Value:Boolean);
begin
 if FAlwaysShow<>Value then
 begin
   FAlwaysShow:=Value;
   KeyValueChanged;
 end;
end;

function TMoneyDBLookupComboBox.GetColor(Index:Integer):TColor;
begin
 Case Index of
  1:result:=FDataList.FSecondColor;
  2:result:=FDataList.FSelectColor;
  3:result:=FDataList.FHighLightColor;
 else result:=Color;
 end;
end;

procedure TMoneyDBLookupComboBox.SetColor(Index:Integer; Value:TColor);
begin
 case Index of
  1:FDataList.SecondColor:=Value;
  2:FDataList.SelectColor:=Value;
  3:FDataList.HighLightColor:=Value;
 end;
end;

procedure TMoneyDBLookupComboBox.ListMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    CloseUp(PtInRect(FDataList.ClientRect, Point(X, Y)));
end;

procedure TMoneyDBLookupComboBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if FListActive then SetFocus;
    if not FFocused then Exit;
    if FListVisible then CloseUp(False) else
      if FListActive then
      begin
        MouseCapture := True;
        FTracking := True;
        TrackButton(X, Y);
        DropDown;
      end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TMoneyDBLookupComboBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ListPos: TPoint;
  MousePos: TSmallPoint;
begin
  if FTracking then
  begin
    TrackButton(X, Y);
    if FListVisible then
    begin
      ListPos := FDataList.ScreenToClient(ClientToScreen(Point(X, Y)));
      if PtInRect(FDataList.ClientRect, ListPos) then
      begin
        StopTracking;
        MousePos := PointToSmallPoint(ListPos);
        SendMessage(FDataList.Handle, WM_LBUTTONDOWN, 0, Integer(MousePos));
        Exit;
      end;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TMoneyDBLookupComboBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  StopTracking;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TMoneyDBLookupComboBox.Paint;
var
  W, X,L,T: Integer;
  Text: string;
  Alignment: TAlignment;
  Selected: Boolean;
  R: TRect;
begin
  Canvas.Font := Font;
  Canvas.Brush.Color := Color;
  Selected := FFocused and not FListVisible and
    not (csPaintCopy in ControlState);
  if Selected then
  begin
    Canvas.Font.Color := clHighlightText;
    Canvas.Brush.Color := clHighlight;
  end;
  if (csPaintCopy in ControlState) and (FDataField <> nil) then
  begin
    Text := FDataField.DisplayText;
    Alignment := FDataField.Alignment;
  end else
  begin
    Text := FText;
    Alignment := FAlignment;
  end;
  W := ClientWidth - FButtonWidth;
  X := 2;
  case Alignment of
    taRightJustify: X := W - Canvas.TextWidth(Text) - 3;
    taCenter: X := (W - Canvas.TextWidth(Text)) div 2;
  end;
  SetRect(R, 1, 1, W - 1, ClientHeight - 1);
  Canvas.TextRect(R, X, 2, Text);
  if Selected then Canvas.DrawFocusRect(R);
  SetRect(R, W, 0, ClientWidth, ClientHeight);
  if not FListActive then
   Canvas.Brush.Color:=clBtnFace
  else if FPressed then
   Canvas.Brush.Color:=clBlack
  else
   Canvas.Brush.Color:=clMoneybtnFace;

  Canvas.FillRect(R);
  Canvas.Brush.Color:=clBlack;
  Canvas.MoveTo(R.Left,R.Top);
  Canvas.LineTo(R.Left,R.Bottom);

  L:=R.Left+(((R.Right-R.Left) div 2)-(FBitmapDownBlack.Width div 2));
  T:=R.Top+(Height-FBitmapDownBlack.Height) div 2;
  if Ctl3D then T:=T-2
  else T:=T-1;
  if FHasMouse and FListActive then
    Canvas.Draw(L,T,FBitmapDownWhite)
   else
   Canvas.Draw(L,T,FBitmapDownBlack);
end;

procedure TMoneyDBLookupComboBox.CMMouseEnter(var Message: TMessage);
begin
 inherited;
 FHasMouse:=True;
 Invalidate;
end;
procedure TMoneyDBLookupComboBox.CMMouseLeave(var Message: TMessage);
begin
 inherited;
 FHasMouse:=False;
 Invalidate;
end;

procedure TMoneyDBLookupComboBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, GetTextHeight + GetBorderSize + 4);
end;

procedure TMoneyDBLookupComboBox.StopTracking;
begin
  if FTracking then
  begin
    TrackButton(-1, -1);
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TMoneyDBLookupComboBox.TrackButton(X, Y: Integer);
var
  NewState: Boolean;
begin
  NewState := PtInRect(Rect(ClientWidth - FButtonWidth, 0, ClientWidth,
    ClientHeight), Point(X, Y));
  if FPressed <> NewState then
  begin
    FPressed := NewState;
    Repaint;
  end;
end;

procedure TMoneyDBLookupComboBox.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and (Message.Sender <> FDataList) then
    CloseUp(False);
end;

procedure TMoneyDBLookupComboBox.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls then
  begin
    RecreateWnd;
    Height := 0;
  end;
  inherited;
end;

procedure TMoneyDBLookupComboBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Height := 0;
end;

procedure TMoneyDBLookupComboBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

procedure TMoneyDBLookupComboBox.WMCancelMode(var Message: TMessage);
begin
  StopTracking;
  inherited;
end;

procedure TMoneyDBLookupComboBox.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  CloseUp(False);
end;


constructor TCustomMoneyDBComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataLink.OnEditingChange := EditingChange;
  FPaintControl := TPaintControl.Create(Self, 'COMBOBOX');
  FValues := TStringList.Create;
  TStringList(FValues).OnChange := ValuesChanged;
  EnableValues := False;
end;

destructor TCustomMoneyDBComboBox.Destroy;
begin
  FPaintControl.Free;
  FDataLink.Free;
  FDataLink := nil;
  TStringList(FValues).OnChange := nil;
  FValues.Free;
  inherited Destroy;
end;




procedure TCustomMoneyDBComboBox.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TCustomMoneyDBComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TCustomMoneyDBComboBox.CreateWnd;
begin
  inherited CreateWnd;
  SetEditReadOnly;
end;

procedure TCustomMoneyDBComboBox.DataChange(Sender: TObject);
begin
  if DroppedDown then Exit;
  if FDataLink.Field <> nil then
    SetComboText(FDataLink.Field.Text)
  else
    if csDesigning in ComponentState then
      SetComboText(Name)
    else
      SetComboText('');
end;

procedure TCustomMoneyDBComboBox.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Text := GetComboText;
end;

procedure TCustomMoneyDBComboBox.ValuesChanged(Sender: TObject);
begin
  if FEnableValues then DataChange(Self);
end;

procedure TCustomMoneyDBComboBox.SetComboText(const Value: string);
var
  I: Integer;
  Redraw: Boolean;
begin
  if Value <> GetComboText then begin
    if inherited Style <> csDropDown then begin
      Redraw := (inherited Style <> csSimple) and HandleAllocated;
      if Redraw then SendMessage(Handle, WM_SETREDRAW, 0, 0);
      try
        if Value = '' then I := -1 else
          if FEnableValues then I := Values.IndexOf(Value)
          else I := Items.IndexOf(Value);
        if I >= Items.Count then I := -1;
        ItemIndex := I;
      finally
        if Redraw then begin
          SendMessage(Handle, WM_SETREDRAW, 1, 0);
          Invalidate;
        end;
      end;
      if I >= 0 then Exit;
    end;
    if inherited Style in [csDropDown, csSimple] then Text := Value;
  end;
end;

function TCustomMoneyDBComboBox.GetComboText: string;
var
  I: Integer;
begin
  if (inherited Style in [csDropDown, csSimple]) and (not FEnableValues) then
    Result := Text
  else begin
    I := ItemIndex;
    if (I < 0) or (FEnableValues and (FValues.Count < I + 1)) then
      Result := ''
    else
      if FEnableValues then Result := FValues[I]
      else Result := Items[I];
  end;
end;

procedure TCustomMoneyDBComboBox.SetEnableValues(Value: Boolean);
begin
  if FEnableValues <> Value then begin
    if Value and (inherited Style in [csDropDown, csSimple]) then
      inherited Style := csDropDownList;
    FEnableValues := Value;
    DataChange(Self);
  end;
end;

procedure TCustomMoneyDBComboBox.SetValues(Value: TStrings);
begin
  FValues.Assign(Value);
end;

procedure TCustomMoneyDBComboBox.Change;
begin
  FDataLink.Edit;
  inherited Change;
  FDataLink.Modified;
end;

procedure TCustomMoneyDBComboBox.Click;
begin
  FDataLink.Edit;
  inherited Click;
  FDataLink.Modified;
end;

function TCustomMoneyDBComboBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TCustomMoneyDBComboBox.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TCustomMoneyDBComboBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TCustomMoneyDBComboBox.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TCustomMoneyDBComboBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TCustomMoneyDBComboBox.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TCustomMoneyDBComboBox.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TCustomMoneyDBComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key in [VK_BACK, VK_DELETE, VK_UP, VK_DOWN, 32..255] then
  begin
    if not FDataLink.Edit and (Key in [VK_UP, VK_DOWN]) then
      Key := 0;
  end;
end;

procedure TCustomMoneyDBComboBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key in [#32..#255]) and (FDataLink.Field <> nil) and
    not FDataLink.Field.IsValidChar(Key) then
  begin
    MessageBeep(0);
    Key := #0;
  end;
  case Key of
    ^H, ^V, ^X, #32..#255:
      FDataLink.Edit;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
      end;
  end;
end;

procedure TCustomMoneyDBComboBox.EditingChange(Sender: TObject);
begin
  SetEditReadOnly;
end;

procedure TCustomMoneyDBComboBox.SetEditReadOnly;
begin
  if (inherited Style in [csDropDown, csSimple]) and HandleAllocated then
    SendMessage(EditHandle, EM_SETREADONLY, Ord(not FDataLink.Editing), 0);
end;

procedure TCustomMoneyDBComboBox.WndProc(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then
    case Message.Msg of
      WM_COMMAND:
        if TWMCommand(Message).NotifyCode = CBN_SELCHANGE then
          if not FDataLink.Edit then
          begin
            if inherited Style <> csSimple then
              PostMessage(Handle, CB_SHOWDROPDOWN, 0, 0);
            Exit;
          end;
      CB_SHOWDROPDOWN:
        if Message.WParam <> 0 then FDataLink.Edit else
          if not FDataLink.Editing then DataChange(Self); {Restore text}
      WM_CREATE,
      WM_WINDOWPOSCHANGED,
      CM_FONTCHANGED:
        FPaintControl.DestroyHandle;
    end;
  inherited WndProc(Message);
end;

procedure TCustomMoneyDBComboBox.ComboWndProc(var Message: TMessage; ComboWnd: HWnd;
  ComboProc: Pointer);
begin
  if not (csDesigning in ComponentState) then
    case Message.Msg of
      WM_LBUTTONDOWN:
        if (inherited Style = csSimple) and (ComboWnd <> EditHandle) then
          if not FDataLink.Edit then Exit;
    end;
  inherited ComboWndProc(Message, ComboWnd, ComboProc);
end;

procedure TCustomMoneyDBComboBox.CMEnter(var Message: TCMEnter);
begin
  inherited;
  if SysLocale.FarEast and FDataLink.CanModify then
    SendMessage(EditHandle, EM_SETREADONLY, Ord(False), 0);
end;

procedure TCustomMoneyDBComboBox.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TCustomMoneyDBComboBox.WMPaint(var Message: TWMPaint);
var
  S: string;
  R: TRect;
  P: TPoint;
  Child: HWND;
begin
  if csPaintCopy in ControlState then
  begin
    if FDataLink.Field <> nil then S := FDataLink.Field.Text else S := '';
    if inherited Style = csDropDown then
    begin
      SendMessage(FPaintControl.Handle, WM_SETTEXT, 0, Longint(PChar(S)));
      SendMessage(FPaintControl.Handle, WM_PAINT, Message.DC, 0);
      Child := GetWindow(FPaintControl.Handle, GW_CHILD);
      if Child <> 0 then
      begin
        Windows.GetClientRect(Child, R);
        Windows.MapWindowPoints(Child, FPaintControl.Handle, R.TopLeft, 2);
        GetWindowOrgEx(Message.DC, P);
        SetWindowOrgEx(Message.DC, P.X - R.Left, P.Y - R.Top, nil);
        IntersectClipRect(Message.DC, 0, 0, R.Right - R.Left, R.Bottom - R.Top);
        SendMessage(Child, WM_PAINT, Message.DC, 0);
      end;
    end else
    begin
      SendMessage(FPaintControl.Handle, CB_RESETCONTENT, 0, 0);
      if Items.IndexOf(S) <> -1 then
      begin
        SendMessage(FPaintControl.Handle, CB_ADDSTRING, 0, Longint(PChar(S)));
        SendMessage(FPaintControl.Handle, CB_SETCURSEL, 0, 0);
      end;
      SendMessage(FPaintControl.Handle, WM_PAINT, Message.DC, 0);
    end;
  end else
    inherited;
end;

procedure TCustomMoneyDBComboBox.SetItems(Value: TStrings);
begin
  Items.Assign(Value);
  DataChange(Self);
end;

procedure TCustomMoneyDBComboBox.SetStyle(Value: TComboboxStyle);
begin
  if (Value <> csSimple) then
   begin
    if (Value = csDropDown) and FEnableValues then
     Value:=csDropDownList;
    inherited SetStyle(Value);
   end;
end;

procedure TCustomMoneyDBComboBox.SetComboStyle(Value:TMoneyComboStyle);
begin
 if GetComboStyle<>Value then
 begin
  if Value=mcsDropDown then
   inherited Style:=csDropDown
  else
   inherited Style:=csDropDownList;
 end;
end;

function TCustomMoneyDBComboBox.GetComboStyle:TMoneyComboStyle;
begin
 if inherited Style = csDropDown then
  result:=mcsDropDown
 else
  result:=mcsDropDownList;
end;

procedure TCustomMoneyDBComboBox.CMGetDatalink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;





{MoneyDBComboBox}

function TMoneyDBComboBox.ButtonRect:TRect;
var
 ARect:TRect;
begin
  GetWindowRect(Handle, ARect);
  OffsetRect(ARect, -ARect.Left, -ARect.Top);
  Inc(ARect.Left, ClientWidth - FButtonWidth);
  InflateRect(ARect, -1, -1);
  result:=ARect;
end;


procedure TMoneyDBComboBox.WMPaint(var Message: TWMPaint);
var
 DC: HDC;
 PaintStruct: TPaintStruct;
begin
  if Message.DC = 0 then
    DC := BeginPaint(Handle, PaintStruct)
  else
    DC := Message.DC;
  try
    if TComboBox(Self).Style <> csSimple then
    begin
     DrawButton(DC,ButtonRect);
     ExcludeClipRect(DC, ClientWidth - FButtonWidth , 1, ClientWidth-1, ClientHeight-1);
    end;
    PaintWindow(DC);
    DrawControlBorder(DC);
  finally
    if Message.DC = 0 then
      EndPaint(Handle, PaintStruct);
  end;
end;

procedure TMoneyDBComboBox.DrawControlBorder(DC: HDC);
var
  ARect: TRect;
  BorderBrush,
  ConrolBrush: HBRUSH;
begin
  BorderBrush := CreateSolidBrush(0);
  ConrolBrush := CreateSolidBrush(ColorToRGB(Color));
  try
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);
    FrameRect(DC, ARect, BorderBrush);
    InflateRect(ARect, -1, -1);
    FrameRect(DC, ARect, ConrolBrush);
  finally
    DeleteObject(ConrolBrush);
    DeleteObject(BorderBrush);
  end;
end;

procedure TMoneyDBComboBox.DrawButton(DC: HDC; ARect:TRect);
var
  BorderBrush,
  ButtonBrush: HBRUSH;
  X,Y,i:Integer;
  C:Integer;
begin
  BorderBrush := CreateSolidBrush(0);
  if FDown then
   begin
    C:=0;
    i:=2;
   end
  else if FMouseInControl then
   begin
    C:=$009BA8A8;
    i:=2;
   end else
   begin
    C:=$009BA8A8;
    i:=1;
   end;

  ButtonBrush := CreateSolidBrush(ColorToRGB(C));
  try
    FillRect(DC,ARect,ButtonBrush);
    try
     X:=ARect.Left+((ARect.Right-ARect.Left) div 2)-(FBitmaps[i].Width div 2)-1;
     Y:=ARect.Top+((ARect.Bottom-ARect.Top) div 2)-(FBitmaps[i].Height div 2);
     FCanvas.Handle:=DC;
     FCanvas.Draw(X,Y,FBitmaps[i]);
    finally
     FCanvas.Handle:=0;
    end;
    InflateRect(ARect,1,1);
    FrameRect(DC,ARect,BorderBrush);
  finally
    DeleteObject(ButtonBrush);
    DeleteObject(BorderBrush);
  end;
end;


constructor TMoneyDBComboBox.Create(AOwner:TComponent);
const
 c:array[1..2] of string =('MSPIN_DOWNBLACK','MSPIN_DOWNWHITE');
var
 i:Integer;
begin
  inherited Create(AOwner);
  FCanvas:=TCanvas.Create;
  for i:=1 to 2 do
  begin
   FBitmaps[i]:=TBitmap.Create;
   FBitmaps[i].LoadFromResourceName(hInstance,C[i]);
   FBitmaps[i].Transparent:=True;
  end;
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL)+2;
end;

destructor TMoneyDBComboBox.Destroy;
var
 i:Integer;
begin
  for i:=1 to 2 do FBitmaps[i].Free;
  FCanvas.Free;
  inherited Destroy;
end;


procedure TMoneyDBComboBox.UpdateTracking;
var
  P: TPoint;
begin
 if Enabled then
 begin
   GetCursorPos(P);
   FMouseInControl := not (FindDragTarget(P, True) = Self);
   if FMouseInControl then
     Perform(CM_MOUSELEAVE, 0, 0)
   else
     Perform(CM_MOUSEENTER, 0, 0);
   end;
end;


procedure TMoneyDBComboBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then
  begin
    //if not Focused and TabStop then SetFocus;
    FDown := DroppedDown;
    InvalidateButton;
  end;
end;

procedure TMoneyDBComboBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FDown:=False;
  UpdateTracking;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TMoneyDBComboBox.MouseMove(Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
end;

procedure TMoneyDBComboBox.MouseEnter;
begin
 if not FMouseInControl and Enabled then
 begin
  FMouseInControl := True;
  InvalidateButton;
 end;
end;

procedure TMoneyDBComboBox.MouseLeave;
begin
 if FMouseInControl and Enabled then
 begin
  FMouseInControl := False;
  FDown:=False;
  InvalidateButton;
 end;
end;


procedure TMoneyDBComboBox.CMMouseEnter(var Msg:TMessage);
begin
 inherited;
 MouseEnter;
end;

procedure TMoneyDBComboBox.CMMouseLeave(var Msg:TMessage);
begin
 inherited;
 MouseLeave;
end;

procedure TMoneyDBComboBox.CMEnabledChanged(var Message: TMessage);
begin
  Repaint;
  inherited;
end;


procedure TMoneyDBComboBox.InvalidateButton;
var
 ARect:TRect;
begin
 if HandleAllocated then
 begin
  ARect:=ButtonRect;
  InvalidateRect(Handle,@ARect,True)
 end;
end;

{MoneyDBDateEdit}



constructor TMoneyDBDateEdit.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FDataLink := TFieldDataLink.Create;
 FDataLink.Control := Self;
 FDataLink.OnDataChange := DataChange;
 FDataLink.OnUpdateData := UpdateData;
end;

destructor  TMoneyDBDateEdit.Destroy;
begin
  FDataLink.OnDataChange := nil;
  FDataLink.OnUpdateData := nil;
  FDataLink.Free;
  FDataLink:=nil;
  inherited Destroy;
end;

procedure TMoneyDBDateEdit.DataChange(Sender: TObject);
begin
 if FDataLink.Field = nil then
  SetDate(0)
 else SetDate(FDataLink.Field.AsDateTime);
end;

procedure TMoneyDBDateEdit.UpdateData(Sender: TObject);
var
  D: TDateTime;
begin
  D := Self.Date;
  if (Text<>'') or (D <> 0) then
    FDataLink.Field.AsDateTime := D
  else FDataLink.Field.Clear;
end;

procedure TMoneyDBDateEdit.DoChange;
begin
 if FDataLink.Edit then
  begin
    inherited DoChange;
    FDataLink.Modified;
  end;
end;

procedure TMoneyDBDateEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #27:
      FDataLink.Reset;
  end;
end;

function TMoneyDBDateEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TMoneyDBDateEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
  inherited ReadOnly := Value;
end;

function TMoneyDBDateEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TMoneyDBDateEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TMoneyDBDateEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TMoneyDBDateEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TMoneyDBDateEdit.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TMoneyDBDateEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TMoneyDBDateEdit.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SelectAll;
    if CanFocus then SetFocus;
    raise;
  end;
  inherited;
end;

procedure TMoneyDBDateEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;


{MoneyDBLinker}


constructor TMoneyDBLinker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink :=TMoneyDBLinkerDataLink.Create(Self);
end;

destructor TMoneyDBLinker.Destroy;
var
 i:Integer;
begin
  FDataLink.Free;
  FDataLink := nil;
  for i:=9 downto 1 do FControls[i]:=nil;
  inherited Destroy;
end;

procedure TMoneyDBLinker.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
   if (FDataLink <> nil) and (AComponent = DataSource) then DataSource := nil;
   if (FControls[1] <> nil) and (AComponent = First) then First := nil;
   if (FControls[2] <> nil) and (AComponent = Prev) then Prev := nil;
   if (FControls[3] <> nil) and (AComponent = Next) then Next := nil;
   if (FControls[4] <> nil) and (AComponent = Last) then Last := nil;
   if (FControls[5] <> nil) and (AComponent = Insert) then Insert := nil;
   if (FControls[6] <> nil) and (AComponent = Delete) then Delete := nil;
   if (FControls[7] <> nil) and (AComponent = Edit) then Edit := nil;
   if (FControls[8] <> nil) and (AComponent = Save) then Save := nil;
   if (FControls[9] <> nil) and (AComponent = Cancel) then Cancel := nil;
  end;
end;

procedure TMoneyDBLinker.DataChanged;
var
  UpEnable, DnEnable: Boolean;
begin
  UpEnable := FDataLink.Active and not FDataLink.DataSet.BOF and not FDataLink.Editing;
  DnEnable := FDataLink.Active and not FDataLink.DataSet.EOF and not FDataLink.Editing;
  {First}
  if Assigned(FControls[1]) then FControls[1].Enabled:=UpEnable;
  {Next}
  if Assigned(FControls[2]) then FControls[2].Enabled:=UpEnable;
  {Prev}
  if Assigned(FControls[3]) then FControls[3].Enabled:=DnEnable;
  {Last}
  if Assigned(FControls[4]) then FControls[4].Enabled:=DnEnable;

end;

procedure TMoneyDBLinker.EditingChanged;
var
  CanModify: Boolean;
begin
  DataChanged;
  CanModify :=FDataLink.Active and FDataLink.DataSet.CanModify;
  {Insert}
  if Assigned(FControls[5]) then FControls[5].Enabled:=
   CanModify and not FDataLink.Editing;
  {Edit}
  if Assigned(FControls[7]) then FControls[7].Enabled:=
   CanModify and not FDataLink.Editing;

   {Save}
  if Assigned(FControls[8]) then FControls[8].Enabled:=
   CanModify and FDataLink.Editing;

   {Cancel}
  if Assigned(FControls[9]) then FControls[9].Enabled:=
   CanModify and FDataLink.Editing;

  {Delete}
  if Assigned(FControls[6]) then FControls[6].Enabled:=
  FDataLink.Active and FDataLink.DataSet.CanModify and not FDataLink.Editing and
    not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF);

end;

procedure TMoneyDBLinker.ActiveChanged;
var
 i:Integer;
begin
  if not (FDataLink.Active) then
  begin
   for i:=9 downto 1 do
    if Assigned(FControls[i]) then FControls[i].Enabled:=False;
  end
  else
  begin
    DataChanged;
    EditingChanged;
  end;
end;

procedure TMoneyDBLinker.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if not (csLoading in ComponentState) then ActiveChanged;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TMoneyDBLinker.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TMoneyDBLinker.SetControl(Index: Integer; Value :TControl);
begin
  FControls[Index] := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TMoneyDBLinker.Loaded;
begin
  inherited Loaded;
  ActiveChanged;
end;


{TMoneyDBLinkerDataLink}


constructor TMoneyDBLinkerDataLink.Create(AOwner: TMoneyDBLinker);
begin
  inherited Create;
  FDBLinker:=AOwner;
end;

destructor TMoneyDBLinkerDataLink.Destroy;
begin
  FDBLinker := nil;
  inherited Destroy;
end;

procedure TMoneyDBLinkerDataLink.EditingChanged;
begin
  if FDBLinker <> nil then FDBLinker.EditingChanged;
end;

procedure TMoneyDBLinkerDataLink.DataSetChanged;
begin
  if FDBLinker <> nil then FDBLinker.DataChanged;
end;

procedure TMoneyDBLinkerDataLink.ActiveChanged;
begin
  if FDBLinker <> nil then FDBLinker.ActiveChanged;
end;


procedure Register;
begin
  RegisterComponents('Money DB Controls',
  [TMoneyDBLinker,
  TMoneyDBCheckBox,
  TMoneyDBComboBox,
  TMoneyDBDateEdit,
  TMoneyDBLookupListBox,
  TMoneyDBLookupComboBox]);
end;

end.
