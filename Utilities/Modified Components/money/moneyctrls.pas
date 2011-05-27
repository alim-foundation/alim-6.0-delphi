
{*******************************************************}
{                                                       }
{       Delphi Money Controls Library                   }
{                                                       }
{       Copyright (c) 1998,99 Azret Botash              }
{                                                       }
{*******************************************************}



unit moneyctrls;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs,StdCtrls,ExtCtrls,Mask;


// setup this optimized palette somewhere an assign it!
const
  ThePalette : HPalette = 0;

const
 clMoneyOrange=$00007FE0;
 clMoneyBtnFace=$009BA8A8;
 clMoneyBtnShadow=$006F6F6F;
 clMoneyCream=$009EE0F1;
 clMoneyBg=$00C0D0D0;
 clCoffeeDark=$00C8DBDB;
 clCoffeeLight=$00E0F0F0;
 clMoneyBlue=$00906030;
 MRB_UNCHECK = WM_USER+121;


type
  TMoneyMouseState = set of (msDown,msOver);
  TMoneyCustomControl = class(TCustomControl)
  private
    FDown: Boolean;
    FMouseInControl: Boolean;
    FOnMouseEnter:TNotifyEvent;
    FOnMouseLeave:TNotifyEvent;
    procedure CMMouseEnter(var Msg:TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg:TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  protected
    procedure UpdateTracking;
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    property  OnMouseEnter:TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property  OnMouseLeave:TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  public
    function  MouseState:TMoneyMouseState;
  published
  end;

  {TMoneyCustomButton}
  TMoneyButtonColors = (ctBorder,ctInnerBorder,ctDown,ctDownText,ctOverText);
  TMoneyCustomButton = class(TMoneyCustomControl)
  private
     FCancel: Boolean;
     FColors:  array[TMoneyButtonColors] of TColor;
     FModalResult: TModalResult;
     function  GetColors(Index:Integer):TColor;
     procedure SetColors(Index:Integer; Value:TColor);
     procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
     procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
  protected
     procedure Paint; override;
     procedure Click; override;
     procedure KeyPress(var Key: Char); override;
     property  Cancel: Boolean read FCancel write FCancel default False;
     property  ModalResult:TModalResult read FModalResult write FModalResult default 0;
     property  TabStop default True;
     property  BorderColor:TColor index 0 read GetColors write SetColors
               default clBlack;
     property  InnerBorderColor:TColor index 1 read GetColors write SetColors
               default $006F6F6F;
     property  DownColor:TColor index 2 read GetColors write SetColors
               default clBlack;
     property  DownTextColor:TColor index 3 read GetColors write SetColors
               default clWhite;
     property  OverTextColor:TColor index 4 read GetColors write SetColors
               default clWhite;
     property  Color
               default $009BA8A8;
  public
     constructor Create(AOwner :TComponent); override;
     destructor  Destroy; override;
  end;

  {TMoneyButton}

  TMoneyButton = class(TMoneyCustomButton)
  published
    property BorderColor;
    property InnerBorderColor;
    property DownColor;
    property DownTextColor;
    property OverTextColor;
    property Color;
    property ModalResult;
    property Cancel;
    property TabStop;
    property Caption;
    property OnClick;
    property OnMouseEnter;
    property OnMouseLeave;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property Visible;
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

  {MoneyLabel}

  TMoneyAlignment = (maDefault,maLeft,maRight);
  TMoneyLabel = class(TMoneyCustomButton)
  private
    FAlignment:TMoneyAlignment;
    procedure SetAlignment(Value:TMoneyAlignment);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner :TComponent); override;
  published
    property Anchors;
    property Alignment:TMoneyAlignment read FAlignment write SetAlignment
     default maDefault;
    property DownTextColor;
    property OverTextColor;
    property Color;
    property ModalResult;
    property Caption;
    property OnClick;
    property OnMouseEnter;
    property OnMouseLeave;
    property Font;
    property Align;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

  {MoneyCheckBox}

  TMoneyCheckBox = class(TMoneyLabel)
  protected
    FChecked:Boolean;
    procedure SetChecked(Value:Boolean); virtual;
    procedure Paint; override;
    procedure Click; override;
    constructor Create(AOwner :TComponent); override;
  published
    property TabStop;
    property Checked:Boolean read FChecked write SetChecked;
    property Caption;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property BorderColor;
    property TabOrder;
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


  {MoneyRadioBox}

  TMoneyRadioButton = class(TMoneyCheckBox)
  private
    FGroupIndex:Integer;
    procedure MRBUncheck(var Msg:TMessage); message MRB_UNCHECK;
  protected
    procedure SetChecked(Value:Boolean); override;
    procedure Paint; override;
    procedure Click; override;
  published
    property GroupIndex:Integer read FGroupIndex write FGroupIndex;
  end;



const
  InitRepeatPause = 400;  { pause before repeat timer (ms) }
  RepeatPause     = 100;  { pause before hint window displays (ms)}

type

  TMoneyTimerButton = class;

{ TMoneySpinButton }

  TMoneySpinButton = class (TWinControl)
  private
    FUpButton: TMoneyTimerButton;
    FDownButton: TMoneyTimerButton;
    FFocusedButton: TMoneyTimerButton;
    FFocusControl: TWinControl;
    FOnUpClick: TNotifyEvent;
    FOnDownClick: TNotifyEvent;
    function CreateButton(IsDown:Boolean): TMoneyTimerButton;
    procedure BtnClick(Sender: TObject);
    procedure BtnMouseDown (Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetFocusBtn (Btn: TMoneyTimerButton);
    procedure AdjustSize (var W: Integer; var H: Integer);
    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Anchors;
    property Color;
    property Align;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FocusControl: TWinControl read FFocusControl write FFocusControl;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnDownClick: TNotifyEvent read FOnDownClick write FOnDownClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnStartDrag;
    property OnUpClick: TNotifyEvent read FOnUpClick write FOnUpClick;
  end;


  TMoneySpinEdit = class(TCustomEdit)
  private
    FMinValue: LongInt;
    FMaxValue: LongInt;
    FIncrement: Double;
    FButton: TMoneySpinButton;
    FEditorEnabled: Boolean;
    function  GetMinHeight: Integer;
    function  GetValue: Double;
    function  CheckValue (NewValue: Double): Double;
    procedure SetValue (NewValue: Double);
    procedure SetEditRect;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMExit);   message CM_EXIT;
    procedure WMPaste(var Message: TWMPaste);   message WM_PASTE;
    procedure WMCut(var Message: TWMCut);   message WM_CUT;
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function  IsValidChar(Key: Char): Boolean; virtual;
    procedure UpClick (Sender: TObject); virtual;
    procedure DownClick (Sender: TObject); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Button: TMoneySpinButton read FButton;
  published
    property AutoSelect;
    property AutoSize;
    property Color;
    property DragCursor;
    property DragMode;
    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property Enabled;
    property Font;
    property Increment: Double read FIncrement write FIncrement;
    property MaxLength;
    property MaxValue: LongInt read FMaxValue write FMaxValue;
    property MinValue: LongInt read FMinValue write FMinValue;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Value: Double read GetValue write SetValue;
    property Visible;
    property OnChange;
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

{ TMoneyTimerButton }

  TTimeBtnState = set of (tbFocusRect, tbAllowTimer,tbTypeDown);

  TMoneyTimerButton = class(TMoneyCustomButton)
  private
    FBitmaps:array[1..4] of TBitmap;
    FRepeatTimer: TTimer;
    FTimeBtnState: TTimeBtnState;
    procedure TimerExpired(Sender: TObject);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    property TimeBtnState: TTimeBtnState read FTimeBtnState write FTimeBtnState;
  end;


 {MoneyShortCutBar}

type
  TOnChangingEvent=procedure(Sender: TObject; var CanChange:Boolean ; const Index:Integer) of object;
  TMoneyShortcutBar = class(TCustomControl)
  private
     FHotTrack:Boolean;
     FOnChange:TNotifyEvent;
     FOnChanging:TOnChangingEvent;
     FTabs:TStringList;
     FBorderColor,FOverColor,FSelectedColor:TColor;
     FTabHeight,FTopOffSet,FLeftOffSet,FRightOffSet:Integer;
     FTabOver,FOldOver:Integer;
     FTabIndex:Integer;
     procedure SetTabIndex(Value:Integer);
     procedure SetTabs(Value:TStringList);
     procedure SetTabHeight(Value:Integer);
     procedure SetTopOffSet(Value:Integer);
     procedure SetLeftOffSet(Value:Integer);
     procedure SetRightOffSet(Value:Integer);
     procedure SetColor(Index:Integer; Value:TColor);
     procedure SetHotTrack(Value:Boolean);
     procedure CMMouseLeave(var Msg:TMessage); message CM_MOUSELEAVE;
     procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
     procedure TabsChanged(Sender :TObject);
  protected
     procedure Change; dynamic;
     procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
     procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
     procedure Paint; override;
     function  TabRect(const Index:Integer):TRect;
     function  TabFromMouse(X,Y:Integer):Integer;
     procedure InvalidateTab(Index:Integer);
     procedure DrawTab(Index:Integer; ARect:TRect; Over:Boolean; Selected:Boolean); virtual;
  public
     function  TabCount:Integer;
     constructor Create(AOwner :TComponent); override;
     destructor Destroy; override;
     property Canvas;
  published
     property BorderColor:TColor Index 0 read FBorderColor write SetColor;
     property OverColor:TColor Index 1 read FOverColor write SetColor;
     property SelectedColor:TColor Index 2 read FSelectedColor write SetColor;
     property Tabs:TStringList read FTabs write SetTabs;
     property TabIndex:Integer read FTabIndex write SetTabIndex;
     property TabHeight:Integer read FTabHeight write SetTabHeight;
     property TopOffSet:Integer read FTopOffSet write SetTopOffSet;
     property LeftOffSet:Integer read FLeftOffSet write SetLeftOffSet;
     property RightOffSet:Integer read FRightOffSet write SetRightOffSet;
     property HotTrack:Boolean read FHotTrack write SetHotTrack default True;
     property Align;
     property Color;
     property Font;
     property Enabled;
     property OnMouseDown;
     property OnMouseUp;
     property OnMouseMove;
     property OnClick;
     property OnDblClick;
     property OnDragDrop;
     property OnDragOver;
     property OnKeyDown;
     property OnKeyUp;
     property OnKeyPress;
     property OnChange:TNotifyEvent read FOnChange write FOnChange;
     property OnChanging:TOnChangingEvent read FOnChanging write FOnChanging;
     property Visible;
  end;


 {MoneyTabControl}

  TMoneyTabControl = class;

  TMoneyTab = class(TCollectionItem)
  private
    FCaption: string;
    FWidth: Integer;
    FAlignment: TAlignment;
    procedure SetAlignment(Value: TAlignment);
    procedure SetCaption(const Value: string);
    procedure SetWidth(Value: Integer);
    function  GetLeft: Integer;
    function  GetRight: Integer;
  protected
    function  GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    property Left: Integer read GetLeft;
    property Right: Integer read GetRight;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Caption: string read FCaption write SetCaption;
    property Width: Integer read FWidth write SetWidth;
  end;


  TMoneyTabs = class(TCollection)
  private
    FMoneyTabControl: TMoneyTabControl;
    function  GetItem(Index: Integer): TMoneyTab;
    procedure SetItem(Index: Integer; Value: TMoneyTab);
  protected
    function  GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(MoneyTabControl: TMoneyTabControl);
    function Add: TMoneyTab;
    property Items[Index: Integer]: TMoneyTab read GetItem write SetItem; default;
  end;

  //TOnChangingEvent=procedure(Sender: TObject; var CanChange:Boolean ; const Index:Integer) of object;
  TMoneyTabDrawState = set of (mtdsFocused,mtdsSelected);
  TMoneyTabControl = class(TCustomControl)
  private
    FOnChange:TNotifyEvent;
    FOnChanging:TOnChangingEvent;
    FIndex:Integer;
    FBackColor,FBackTextColor,FBackTabColor:TColor;
    FTabHeight:Integer;
    FSpace:Integer;
    FMoneyTabs: TMoneyTabs;
    procedure SetColor(Index :Integer; Value :TColor);
    procedure SetMoneyTabs(Value: TMoneyTabs);
    procedure SetIndex(Value: Integer);
    procedure SetSpace(Value :Integer);
    procedure SetTabHeight(Value :Integer);
    procedure UpdateTab(Index: Integer);
    procedure UpdateTabs;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  protected
    function  GetClientRect: TRect; override;
    procedure Change; dynamic;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function  GetTabRect(const Index:Integer):TRect;
    function  GetTabFromMouse(const X,Y:Integer):Integer;
    procedure DrawTab(Index:Integer; ARect:TRect; AState:TMoneyTabDrawState); virtual;
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnChange:TNotifyEvent read FOnChange write FOnChange;
    property OnChanging:TOnChangingEvent read FOnChanging write FOnChanging;
    property BackColor:TColor index 1 read FBackColor write SetColor;
    property BackTextColor:TColor index 2 read FBackTextColor write SetColor;
    property BackTabColor:TColor index 3 read FBackTabColor write SetColor;
    property TabIndex:Integer read FIndex write SetIndex;
    property TabHeight :Integer read FTabHeight write SetTabHeight;
    property Space :Integer read FSpace write SetSpace;
    property Align;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property MoneyTabs: TMoneyTabs read FMoneyTabs write SetMoneyTabs;
    property ShowHint;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Visible;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

type
  TMoneyComboStyle = (mcsDropDown,mcsDropDownList);
  TMoneyComboBox = class(TCustomComboBox)
  private
    FCanvas:TCanvas;
    FBitmaps:array[1..2] of TBitmap;
    FStyle:TMoneyComboStyle;
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
    procedure SetComboStyle(Value:TMoneyComboStyle); virtual;
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
    property Style:TMoneyComboStyle read FStyle write SetComboStyle;
    property Items;
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDrag;
  end;


{MoneyPanel}

  TMoneyPanel = class(TCustomControl)
  private
    FOffSets:array[1..4] of Integer;
    procedure SetOffSets(Index,Value:Integer);
  public
    constructor Create(AOwner:TComponent); override;
  protected
    procedure Paint; override;
    function GetClientRect: TRect; override;
  published
    property Align;
    property Color;
    property Font;
    property TopOffSet:Integer Index 1 read FOffSets[1] write SetOffSets;
    property LeftOffSet:Integer Index 2 read FOffSets[2] write SetOffSets;
    property BottomOffSet:Integer Index 3 read FOffSets[3] write SetOffSets;
    property RightOffSet:Integer Index 4 read FOffSets[4] write SetOffSets;
  end;

type
  TMoneyGridDrawState = set of (dsSelected,dsFocused);
  TMoneyCustomGrid = class(TCustomControl)
  private
    FOnCellMouseUp:TMouseEvent;
    FOnCellClick:TNotifyEvent;
    FColCount:Integer;
    FRowCount:Integer;
    FRow:Integer;
    FCol:Integer;
    procedure SetCount(Index,Value:Integer);
    procedure SetCell(Index,Value:Integer);
    procedure WMSize(var Msg:TWMSize); message WM_SIZE;
  protected
    procedure Paint; override;
    procedure DrawCell(ACol,ARow:Integer; ARect:TRect; AState:TMoneyGridDrawState); virtual;
    procedure MouseDown(Button: TMouseButton;Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton;Shift: TShiftState; X, Y: Integer); override;
    function  CellRect(ACol,ARow:Integer):TRect;
    function  CellFromMouse(X,Y:Integer):TPoint;
    function  SelectCell(ACol,ARow:Integer):Boolean; virtual;
    procedure InvalidateCell(ACol,ARow:Integer);
    procedure ResetSize;
    procedure CellMouseUp(Button: TMouseButton;Shift: TShiftState; ACol,ARow:Integer); virtual;
    procedure CellClick(ACol,ARow:Integer); virtual;
    property  ColCount:Integer index 1 read FColCount write SetCount;
    property  RowCount:Integer index 2 read FRowCount write SetCount;
    property  Col:Integer index 1 read FCol write SetCell;
    property  Row:Integer index 2 read FRow write SetCell;
    property  OnCellMouseUp:TMouseEvent read FOnCellMouseUp write FOnCellMouseUp;
    property  OnCellClick:TNotifyEvent read FOnCellClick write FOnCellClick;
  public
    constructor Create(AOwner :TComponent); override;
  end;

  {MoneyMonthSelect}

  TMoneyMonthSelect = class(TMoneyCustomGrid)
  private
    FColors:array[1..4] of TColor;
    procedure SetColor(Index:Integer; Value:TColor);
    function  GetMonth:Integer;
    procedure SetMonth(Value:Integer);
  protected
    function  MonthFromCell(ACol,ARow:Integer):Integer;
    procedure DrawCell(ACol,ARow:Integer; ARect:TRect; AState:TMoneyGridDrawState); override;
  public
    constructor Create(AOwner :TComponent); override;
  published
    property  Font;
    property  Color;
    property  SelectedColor:TColor index 1 read FColors[1] write SetColor;
    property  SelectedTextColor:TColor index 2 read FColors[2] write SetColor;
    property  SelectedBorderColor:TColor index 3 read FColors[3] write SetColor;
    property  BorderColor:TColor index 4 read FColors[4] write SetColor;
    property  OnCellClick;
    property  OnCellMouseUp;
    property  Month:Integer read GetMonth write SetMonth;
  end;

const
  MMC_TRAILINGLEFT = -1;
  MMC_TRAILINGRIGHT = 1;
  MMC_WEEKDAYNAMES = 0;
  MMC_WEEKDAYS = 2;

type
  TDayOfWeek =0..6;
  TMoneyMonthCalendar=class(TMoneyCustomGrid)
  private
    FColors:array[1..11] of TColor;
    FOnChange:TNotifyEvent;
    FChanging,FTrailing:Boolean;
    FReadOnly:Boolean;
    FYear,FMonth,FDay:Integer;
    FMonthOffset: Integer;
    FStartOfWeek: TDayOfWeek;
    FUpdating: Boolean;
    procedure SetColor(Index:Integer; Value:TColor);
    function  GetCellText(ACol,ARow: Integer): string;
    procedure SetStartOfWeek(Value:TDayOfWeek);
    function  GetDate: TDateTime;
    procedure SetDate(Value: TDateTime);
    procedure SetYear(Value:Integer);
    procedure SetMonth(Value:Integer);
    procedure SetDay(Value:Integer);
    function  GetCellValue(ACol,ARow:Integer):TPoint;
    procedure UpdateCalendar;
  protected
    procedure Paint; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TMoneyGridDrawState); override;
    procedure DrawWeekDayName(AWeekDay:Integer; ARect:TRect); virtual;
    function  SelectCell(ACol, ARow: Integer): Boolean; override;
    procedure CellClick(ACol,ARow:Integer); override;
    procedure Change; dynamic;
    property  CellText[ACol, ARow: Integer]: string read GetCellText;
  public
    constructor Create(AOwner: TComponent); override;
    function  NextMonth:TDateTime;
    function  PrevMonth:TDateTime;
    function  NextYear:TDateTime;
    function  PrevYear:TDateTime;
  published
    property  TrailingColor:TColor index 1 read FColors[1] write SetColor;
    property  TrailingTextColor:TColor index 2 read FColors[2] write SetColor;
    property  WeekNamesColor:TColor index 3 read FColors[3] write SetColor;
    property  WeekNamesTextColor:TColor index 4 read FColors[4] write SetColor;
    property  TodaysColor:TColor index 5 read FColors[5] write SetColor;
    property  TodaysTextColor:TColor index 6 read FColors[6] write SetColor;
    property  SelectedColor:TColor index 7 read FColors[7] write SetColor;
    property  SelectedTextColor:TColor index 8 read FColors[8] write SetColor;
    property  SelectedBorderColor:TColor index 9 read FColors[9] write SetColor;
    property  BorderColor:TColor index 10 read FColors[10] write SetColor;
    property  TodaysBorderColor:TColor index 11 read FColors[11] write SetColor;
    property  OnChange:TNotifyEvent read FOnChange write FOnChange;
    property  ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property  StartOfWeek: TDayOfWeek read FStartOfWeek write SetStartOfWeek;
    property  Year:Integer read  FYear write SetYear stored False;
    property  Month:Integer read FMonth write SetMonth stored False;
    property  Day:Integer read FDay write SetDay stored False;
    property  Date: TDateTime  read GetDate write SetDate stored False;
    property  OnCellClick;
    property  OnCellMouseUp;
  end;


  {MoneyImageButton}

  TMoneyImageButton = class(TMoneyCustomButton)
  private
    FBitmaps:array[1..3] of TBitmap;
    procedure SetBitmap(Index :Integer; Value:TBitmap);
  public
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy; override;
  protected
    procedure Paint; override;
  published
    property Align;
    property Bitmap:TBitmap index 1 read FBitmaps[1] write SetBitmap;
    property BitmapOn:TBitmap index 2 read FBitmaps[2] write SetBitmap;
    property BitmapDown:TBitmap index 3 read FBitmaps[3] write SetBitmap;
    property Color;
    property Enabled;
    property OnClick;
    property ModalResult;
  end;


  TMoneyCalendarState = (csMonth,csYear);
  TMoneyCalendar = class(TCustomControl)
  private
    FOnCellMouseUp:TMouseEvent;
    FOnChange:TNotifyEvent;
    FButtonWidth:Integer;
    FTopOff:Integer;
    FOffSet:Integer;
    FSelectText:String;
    FState:TMoneyCalendarState;
    FMonthCal:TMoneyMonthCalendar;
    FMonthSelect:TMoneyMonthSelect;
    FMonthLeftButton:TMoneyImageButton;
    FMonthRightButton:TMoneyImageButton;
    FTitle:TMoneyLabel;
    function  CellHeight:Integer;
    procedure SetSelectText(Value:String);
    procedure SetState(Value:TMoneyCalendarState);
    procedure WMSize(var Msg:TWMSize); message WM_SIZE;
    function  GetColor(Index:Integer):TColor;
    procedure SetColor(Index:Integer; Value:TColor);
    procedure UpDateTitle;
    function  GetDate:TDateTime;
    procedure SetDate(Value:TDateTime);
    function  GetStartOfWeek:TDayOfWeek;
    procedure SetStartOfWeek(Value:TDayOfWeek);
  protected
    procedure AdjustControl;
    procedure Paint; override;
    function  GetClientRect: TRect; override;
    procedure CreateWnd; override;
    procedure DoMonthSelect(Sender:TObject; Button: TMouseButton;Shift: TShiftState; ACol,ARow:Integer);
    procedure DoCellMouseUp(Sender:TObject; Button: TMouseButton;Shift: TShiftState; ACol,ARow:Integer);
    procedure DoLeftClick(Sender:TObject);
    procedure DoRightClick(Sender:TObject);
    procedure DoTitleClick(Sender:TObject);
    procedure DoChange(Sender:TObject);
    procedure Change; dynamic;
  public
    property MonthCal:TMoneyMonthCalendar read FMonthCal;
    property MonthSelect:TMoneyMonthSelect read FMonthSelect;
    property MonthLeftButton:TMoneyImageButton read FMonthLeftButton;
    property MonthRightButton:TMoneyImageButton read FMonthRightButton;
    property Title:TMoneyLabel read FTitle;
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property StartOfWeek:TDayOfWeek read GetStartOfWeek write SetStartOfWeek;
    property WeekNamesColor:TColor Index 1 read GetColor write SetColor default $004F4F4F;
    property WeekNamesTextColor:TColor Index 2 read GetColor write SetColor default clWhite;
    property TrailingColor:TColor Index 3 read GetColor write SetColor default clBlack;
    property TrailingTextColor:TColor Index 4 read GetColor write SetColor default clGray;
    property SelectedColor:TColor Index 5 read GetColor write SetColor default clGray;
    property SelectedTextColor:TColor Index 6 read GetColor write SetColor default clWhite;
    property SelectedBorderColor:TColor Index 7 read GetColor write SetColor default clGray;
    property BorderColor:TColor Index 8 read GetColor write SetColor default clGray;
    property TodaysColor:TColor Index 9 read GetColor write SetColor default $00BF5F5F;
    property TodaysTextColor:TColor Index 10 read GetColor write SetColor default clWhite;
    property TodaysBorderColor:TColor Index 11 read GetColor write SetColor default $00BF5F5F;
    property Font;
    property Date:TDateTime read GetDate write SetDate stored False;
    property SelectText:String read FSelectText write SetSelectText;
    property State:TMoneyCalendarState read FState write SetState stored False;
    property Color default clBlack;
    property OnChange:TNotifyEvent read FOnChange write FOnChange;
    property OnCellMouseUp:TMouseEvent read FOnCellMouseUp write FOnCellMouseUp;
  end;


  {MoneyDateButton}

  TMoneyDateButton = class(TMoneyImageButton)
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner:TComponent); override;
  end;

  {MoneyPopupCal}

  TMoneyPopupCal = class(TMoneyCalendar)
  private
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TMoneyDropDownAlign = (mdaLeft, mdaRight, mdaCenter);

  {MoneyDateEdit}

  TMoneyDateEdit = class(TCustomMaskEdit)
  private
    FOnError:TNotifyEvent;
    FIncrement: Integer;
    FButton: TMoneyDateButton;
    FEditorEnabled: Boolean;
    FExiting:Boolean;
    FCal: TMoneyPopupCal;
    FDropDownAlign: TMoneyDropDownAlign;
    FCalVisible: Boolean;
    FOnDropDown: TNotifyEvent;
    FOnCloseUp: TNotifyEvent;
    FOnChange: TNotifyEvent;
    procedure CalMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMCancelMode(var Message: TMessage); message WM_CANCELMODE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    function  GetColor(Index:Integer):TColor;
    procedure SetColor(Index:Integer; Value:TColor);
    function  GetSelectText:String;
    procedure SetSelectText(Value:String);
    function  GetMinHeight: Integer;
    function  GetDate:TDateTime;
    function  GetStartOfWeek:TDayOfWeek;
    procedure SetStartOfWeek(Value:TDayOfWeek);
    procedure SetEditRect;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMExit);   message CM_EXIT;
    procedure WMPaste(var Message: TWMPaste);   message WM_PASTE;
    procedure WMCut(var Message: TWMCut);   message WM_CUT;
  protected
    procedure SetDate(Value: TDateTime); virtual;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function IsValidChar(Key: Char): Boolean; virtual;
    procedure ButtonClick (Sender: TObject); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DoChange; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Button: TMoneyDateButton read FButton;
    procedure CloseUp(Accept: Boolean);
    procedure DropDown;
    property CalVisible: Boolean read FCalVisible;
    property Date: TDateTime read GetDate write SetDate;
  published
    property StartOfWeek:TDayOfWeek read GetStartOfWeek write SetStartOfWeek;
    property WeekNamesColor:TColor Index 1 read GetColor write SetColor default $004F4F4F;
    property WeekNamesTextColor:TColor Index 2 read GetColor write SetColor default clWhite;
    property TrailingColor:TColor Index 3 read GetColor write SetColor default clBlack;
    property TrailingTextColor:TColor Index 4 read GetColor write SetColor default clGray;
    property SelectedColor:TColor Index 5 read GetColor write SetColor default clGray;
    property SelectedTextColor:TColor Index 6 read GetColor write SetColor default clWhite;
    property SelectedBorderColor:TColor Index 7 read GetColor write SetColor default clGray;
    property BorderColor:TColor Index 8 read GetColor write SetColor default clGray;
    property TodaysColor:TColor Index 9 read GetColor write SetColor default $00BF5F5F;
    property TodaysTextColor:TColor Index 10 read GetColor write SetColor default clWhite;
    property TodaysBorderColor:TColor Index 11 read GetColor write SetColor default $00BF5F5F;
    property Color;
    property DropDownAlign: TMoneyDropDownAlign read FDropDownAlign write FDropDownAlign default mdaLeft;
    property Enabled;
    property Font;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnError :TNotifyEvent read FOnError write FOnError;
    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property Increment: Integer read FIncrement write FIncrement default 1;
    property SelectText:String read GetSelectText write SetSelectText;
    property DragMode;
    property DragCursor;
    property ImeMode;
    property ImeName;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
  end;



procedure Register;
function  DaysPerMonth(AYear, AMonth: Integer): Integer;
function  IsLeapYear(AYear: Integer): Boolean;

implementation
//uses DsgnIntf;

{$R MoneyCtrls.res}

{MoneyCustomControl}

const
  MapToPalette = $2000000;

procedure ChangeThePalette(const Canvas : TCanvas; ResetDefault : Boolean = False);
const
  DC: HDC = 0;
  OldPal: HPalette = 0;
begin
  if ResetDefault then
    SelectPalette(DC, OldPal, False)
  else begin
    DC := Canvas.Handle;
    OldPal := SelectPalette(DC, ThePalette, False);
    RealizePalette(DC);
  end;
end;

function  TMoneyCustomControl.MouseState:TMoneyMouseState;
begin
 Result:=[];
 if FDown then
  Include(Result,msDown);
 if FMouseInControl then
  Include(Result,msOver);
end; {MouseState}

procedure TMoneyCustomControl.UpdateTracking;
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
end; {UpdateTracking}


procedure TMoneyCustomControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then
  begin
    if not Focused and TabStop then SetFocus;
    FDown := True;
    Invalidate;
  end;
end; {MouseDown}

procedure TMoneyCustomControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  DoClick: Boolean;
begin
  FDown:=False;
  UpdateTracking;
  Perform(CM_MOUSEENTER, 0, 0);
  Perform(CM_MOUSELEAVE, 0, 0);

  inherited MouseUp(Button, Shift, X, Y);
    DoClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);
  if Enabled and (Button=mbLeft) and DoClick then
  try
   Click;
  finally
   UpdateTracking;
  end;
end; {MouseUp}

procedure TMoneyCustomControl.DoEnter;
begin
 Invalidate;
 inherited DoEnter;
end; {DoEnter}

procedure TMoneyCustomControl.DoExit;
begin
 Invalidate;
 inherited DoExit;
end; {DoExit}

procedure TMoneyCustomControl.MouseEnter;
begin
 if not FMouseInControl and Enabled then
 begin
  FMouseInControl := True;
  Invalidate;
  if Assigned(FOnMouseEnter) then
  FOnMouseEnter(Self);
 end;
end; {MouseEnter}

procedure TMoneyCustomControl.MouseLeave;
begin
 if FMouseInControl and Enabled then
 begin
  FMouseInControl := False;
  Invalidate;
  if Assigned(FOnMouseLeave) then
   FOnMouseLeave(Self);
 end;
end; {MouseLeave}


procedure TMoneyCustomControl.CMMouseEnter(var Msg:TMessage);
begin
 inherited;
 MouseEnter;
end; {CMMouseEnter}

procedure TMoneyCustomControl.CMMouseLeave(var Msg:TMessage);
begin
 inherited;
 MouseLeave;
end; {CMMouseLeave}

procedure TMoneyCustomControl.CMEnabledChanged(var Message: TMessage);
begin
  Repaint;
  inherited;
end; {CMEnabledChanged}


procedure TMoneyCustomControl.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end; {CMFontChanged}

procedure TMoneyCustomControl.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end; {CMTextChanged}

procedure TMoneyCustomControl.CMSysColorChange(var Message: TMessage);
begin
 inherited;
 Invalidate;
end; {CMSysColorChange}


procedure TMoneyCustomControl.WMSize(var Message: TWMSize);
begin
 inherited;
 Invalidate;
end;


{MoneyCustomButton}


constructor TMoneyCustomButton.Create(AOwner :TComponent);
begin
 inherited Create(AOwner);
 ControlStyle := [csCaptureMouse, csOpaque,csSetCaption, csDoubleClicks];
 SetBounds(0,0,80,25);
 Color:=$009BA8A8;
 FColors[ctBorder]:=clBlack;
 FColors[ctInnerBorder]:=$006F6F6F;
 FColors[ctDown]:=clBlack;
 FColors[ctDownText]:=clWhite;
 FColors[ctOverText]:=clWhite;
 ParentFont := True;
 TabStop:=True;
end; {Create}

destructor TMoneyCustomButton.Destroy;
begin
 inherited Destroy;
end; {Destroy}


procedure DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; const AColor:TColor; Flags:Integer);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Color := AColor;
  DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds,Flags);
end;

procedure TMoneyCustomButton.Paint;
var
 DrawRect:TRect;
 AColor:TColor;
begin
 ChangeThePalette(Canvas);
 DrawRect:=Rect(0,0,Width,Height);
 if msDown in MouseState then
  Canvas.Brush.Color:=FColors[ctDown] or MapToPalette
 else
  Canvas.Brush.Color:=Color or MapToPalette;

 Canvas.Font:=Font;
 Canvas.FillRect(DrawRect);
 Canvas.Brush.Color:=FColors[ctBorder] or MapToPalette;
 Canvas.FrameRect(DrawRect);
 if Enabled and Focused then
 begin
  InflateRect(DrawRect,-1,-1);
  Canvas.Brush.Color:=FColors[ctBorder] or MapToPalette;
  Canvas.FrameRect(DrawRect);
  InflateRect(DrawRect,-2,-2);
  Canvas.Brush.Color:=FColors[ctInnerBorder] or MapToPalette;
  Canvas.FrameRect(DrawRect);
  InflateRect(DrawRect,3,3);
 end;
 AColor:=Font.Color or MapToPalette;
 if not Enabled then
  AColor:=clGray
 else
 begin
  if msOver in MouseState then AColor:=FColors[ctOverText];
  if msDown in MouseState then AColor:=FColors[ctDownText];
 end;
 DrawButtonText(Canvas,Caption,DrawRect,AColor,DT_CENTER or DT_VCENTER or DT_SINGLELINE);
 ChangeThePalette(Canvas, True);
end; {Paint}


procedure TMoneyCustomButton.Click;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  inherited Click;
  Invalidate;
  if Form <> nil then Form.ModalResult := FModalResult;
end; {Click}

procedure TMoneyCustomButton.KeyPress(var Key: Char);
begin
 inherited KeyPress(Key);
 if (Key=#13) or (Key=#32) then Click;
end; {KeyPress}

procedure TMoneyCustomButton.CMDialogKey(var Message: TCMDialogKey);
begin
  with Message do
    if (* (((CharCode = VK_RETURN) and Enabled) or*)
      ((CharCode = VK_ESCAPE) and FCancel) and
      (KeyDataToShiftState(Message.KeyData) = []) and CanFocus then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

procedure TMoneyCustomButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if Enabled and Visible and IsAccel(CharCode, Caption)  then
    begin
      Click;
      Result := 1;
    end else inherited;
end; {CMDialogChar}

function TMoneyCustomButton.GetColors(Index:Integer):TColor;
begin
  result:=FColors[TMoneyButtonColors(Index)];
end; {GetColors}

procedure TMoneyCustomButton.SetColors(Index:Integer; Value:TColor);
begin
 if FColors[TMoneyButtonColors(Index)]<>Value then
 begin
   FColors[TMoneyButtonColors(Index)]:=Value;
   Invalidate;
 end;
end; {SetColors}



{TMoneyLabel}

constructor TMoneyLabel.Create(AOwner :TComponent);
begin
 inherited Create(AOwner);
 SetBounds(0,0,80,15);
 ParentFont := True;
 TabStop:=False;
 ParentColor:=True;
end; {Create}

procedure TMoneyLabel.Paint;
var
 DrawRect:TRect;
 AColor:TColor;
 Flags:Integer;
begin
 ChangeThePalette(Canvas);
 DrawRect:=Rect(0,0,Width,Height);
 Canvas.Brush.Color:=Color or MapToPalette;
 Canvas.Font:=Font;
 Canvas.FillRect(DrawRect);
 AColor:=Font.Color or MapToPalette;
 if not Enabled then
  AColor:=clGray
 else
 begin
  if msOver in MouseState then AColor:=FColors[ctOverText] or MapToPalette;
  if msDown in MouseState then AColor:=FColors[ctDownText] or MapToPalette;
 end;
 if FAlignment=maRight then
  Flags:=DT_RIGHT
 else if FAlignment=maLeft then
  Flags:=DT_LEFT
 else
  Flags:=DT_CENTER;

 DrawButtonText(Canvas,Caption,DrawRect,AColor,Flags or DT_VCENTER or DT_SINGLELINE);
 ChangeThePalette(Canvas, True);
end; {Paint}

procedure TMoneyLabel.SetAlignment(Value:TMoneyAlignment);
begin
 if FAlignment<>Value then
 begin
  FAlignment:=Value;
  Invalidate;
 end;
end;







{MoneyCheckBox}

constructor TMoneyCheckBox.Create(AOwner :TComponent);
begin
 inherited Create(AOwner);
 SetBounds(0,0,105,15);
 TabStop:=True;
 Alignment:=maLeft;
end; {Create}

procedure TMoneyCheckBox.Paint;
var
 ARect:TRect;
 Flags,CBFlags:Integer;
 AColor:TColor;
begin
 ChangeThePalette(Canvas);
 ARect:=Rect(0,0,Width,Height);
 Canvas.Brush.Color:=Color or MapToPalette;
 Canvas.Font:=Font;
 Canvas.FillRect(ARect);
 ARect:=Rect(0,0,13,13);

 if (Alignment=maLeft) then
  OffSetRect(ARect,0,(Height div 2)-6)
 else
  OffSetRect(ARect,Width-13,(Height div 2)-6);

 CBFlags:=DFCS_FLAT;
 if FChecked then
  CBFlags:=CBFlags or DFCS_CHECKED;

 if MouseState=[msOver] then
   CBFlags:=CBFlags or DFCS_INACTIVE;

 if not Enabled then
  CBFlags:=DFCS_INACTIVE or DFCS_FLAT;

 DrawFrameControl(Canvas.Handle,ARect,DFC_BUTTON,DFCS_BUTTONCHECK or
  CBFlags);

 if ((not Enabled) or FMouseInControl) and not FDown then
  Canvas.Brush.Color:=clBtnFace
 else
  Canvas.Brush.Color:=clWhite;

 InflateRect(ARect,-1,-1);
 Canvas.FrameRect(ARect);
 ARect:=Rect(0,0,Width,Height);
 if Alignment=maLeft then
 begin
  Flags:=DT_LEFT;
  ARect.Left:=ARect.Left+16
 end
 else if Alignment = maRight then
 begin
  Flags:=DT_RIGHT;
  ARect.Right:=ARect.Right-16
 end
 else
 begin
  Flags:=DT_CENTER;

 end;

 AColor:=Font.Color or MapToPalette;
 if not Enabled then
  AColor:=clGray
 else
 begin
  if FMouseInControl then AColor:=FColors[ctOverText];
  if FDown then AColor:=FColors[ctDownText];
 end;
 DrawButtonText(Canvas,Caption,ARect,AColor,Flags or DT_VCENTER or DT_SINGLELINE);

 Canvas.Brush.Color:=BorderColor or MapToPalette;

 if Focused then
 Canvas.FrameRect(ARect);
 ChangeThePalette(Canvas, True);
end; {Paint}

procedure TMoneyCheckBox.Click;
begin
 FChecked:=not FChecked;
 inherited Click;
end;

procedure TMoneyCheckBox.SetChecked(Value:Boolean);
begin
 if FChecked<>Value then
 begin
  FChecked:=Value;
  Invalidate;
 end;
end;


{TMoneyRadioButton}


procedure TMoneyRadioButton.MRBUncheck(var Msg:TMessage);
begin
 FChecked:=False;
 Invalidate;
end;

procedure TMoneyRadioButton.Click;
begin
 if not Checked and Enabled then
 begin
  inherited Click;
  Checked:=True;
  {if Parent<>nil then
  for i:=0 to Parent.ControlCount-1 do
   if Parent.Controls[i] is TMoneyRadioButton then
     if Parent.Controls[i]<>Self then
      if (TMoneyRadioButton(Parent.Controls[i]).GroupIndex>0) and
       (TMoneyRadioButton(Parent.Controls[i]).GroupIndex=FGroupIndex) then
       TMoneyRadioButton(Parent.Controls[i]).Perform(MRB_UNCHECK,0,0);}
 end;
end;

procedure TMoneyRadioButton.SetChecked(Value:Boolean);
var
 i:Integer;
begin
if not Enabled then Exit;
FChecked:=Value;
Invalidate;
if not (csLoading in ComponentState) then
 if Parent<>nil then
  for i:=0 to Parent.ControlCount-1 do
   if Parent.Controls[i] is TMoneyRadioButton then
     if Parent.Controls[i]<>Self then
      if (TMoneyRadioButton(Parent.Controls[i]).GroupIndex>0) and
       (TMoneyRadioButton(Parent.Controls[i]).GroupIndex=FGroupIndex) then
       TMoneyRadioButton(Parent.Controls[i]).Perform(MRB_UNCHECK,0,0);
end;



procedure TMoneyRadioButton.Paint;
var
 ARect:TRect;
 Flags,CBFlags:Integer;
 AColor:TColor;
begin
 ChangeThePalette(Canvas);
 ARect:=Rect(0,0,Width,Height);
 Canvas.Brush.Color:=Color or MapToPalette;
 Canvas.Font:=Font;
 Canvas.FillRect(ARect);
 ARect:=Rect(0,0,13,13);

 if (Alignment=maLeft) then
  OffSetRect(ARect,0,(Height div 2)-6)
 else
  OffSetRect(ARect,Width-13,(Height div 2)-6);

 CBFlags:=DFCS_FLAT;
 if FChecked then
  CBFlags:=CBFlags or DFCS_CHECKED;

 if MouseState=[msOver] then
   CBFlags:=CBFlags or DFCS_INACTIVE;

 if not Enabled then
  CBFlags:=DFCS_INACTIVE or DFCS_FLAT;

 DrawFrameControl(Canvas.Handle,ARect,DFC_BUTTON,DFCS_BUTTONRADIO or
  CBFlags);

 ARect:=Rect(0,0,Width,Height);
 if Alignment=maLeft then
 begin
  Flags:=DT_LEFT;
  ARect.Left:=ARect.Left+16
 end
 else if Alignment = maRight then
 begin
  Flags:=DT_RIGHT;
  ARect.Right:=ARect.Right-16
 end else
 begin
  Flags:=DT_CENTER;
 end;

 AColor:=Font.Color or MapToPalette;
 if not Enabled then
  AColor:=clGray
 else
 begin
  if FMouseInControl then AColor:=FColors[ctOverText];
  if FDown then AColor:=FColors[ctDownText];
 end;
 DrawButtonText(Canvas,Caption,ARect,AColor,Flags or DT_VCENTER or DT_SINGLELINE);

 Canvas.Brush.Color:=BorderColor or MapToPalette;

 if Focused then
 Canvas.FrameRect(ARect);
 ChangeThePalette(Canvas, True);
end; {Paint}




{ TMoneySpinButton }

constructor TMoneySpinButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] +
    [csFramed, csOpaque];
  ParentCtl3D:=False;

  Ctl3D:=False;
  FUpButton := CreateButton(False);
  FDownButton := CreateButton(True);
  Width := 20;
  Height := 25;
  FFocusedButton := FUpButton;
end;

function TMoneySpinButton.CreateButton(IsDown:Boolean): TMoneyTimerButton;
begin
  Result := TMoneyTimerButton.Create (Self);
  Result.OnClick := BtnClick;
  Result.OnMouseDown := BtnMouseDown;
  Result.Visible := True;
  Result.TabStop:=False;
  Result.Enabled := True;
  if IsDown then
   Result.TimeBtnState:=[tbAllowTimer,tbTypeDown]
  else
   Result.TimeBtnState := [tbAllowTimer];
  Result.Parent := Self;
end;

procedure TMoneySpinButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TMoneySpinButton.AdjustSize (var W: Integer; var H: Integer);
begin
  if (FUpButton = nil) or (FDownButton = nil) or (csLoading in ComponentState) then Exit;
  if W < 15 then W := 15;
  FUpButton.SetBounds (0, 0, W, (H div 2));
  FDownButton.SetBounds (0, FUpButton.Height-1, W, H - FUpButton.Height+1);
end;

procedure TMoneySpinButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  AdjustSize (W, H);
  inherited SetBounds (ALeft, ATop, W, H);
end;

procedure TMoneySpinButton.WMSize(var Message: TWMSize);
var
  W, H: Integer;
begin
  inherited;

  { check for minimum size }
  W := Width;
  H := Height;
  AdjustSize (W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds(Left, Top, W, H);
  Message.Result := 0;
end;

procedure TMoneySpinButton.WMSetFocus(var Message: TWMSetFocus);
begin
  FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState + [tbFocusRect];
  FFocusedButton.Invalidate;
end;

procedure TMoneySpinButton.WMKillFocus(var Message: TWMKillFocus);
begin
  FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState - [tbFocusRect];
  FFocusedButton.Invalidate;
end;

procedure TMoneySpinButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP:
      begin
        SetFocusBtn (FUpButton);
        FUpButton.Click;
      end;
    VK_DOWN:
      begin
        SetFocusBtn (FDownButton);
        FDownButton.Click;
      end;
    VK_SPACE:
      FFocusedButton.Click;
  end;
end;

procedure TMoneySpinButton.BtnMouseDown (Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    SetFocusBtn (TMoneyTimerButton (Sender));
    if (FFocusControl <> nil) and FFocusControl.TabStop and
        FFocusControl.CanFocus and (GetFocus <> FFocusControl.Handle) then
      FFocusControl.SetFocus
    else if TabStop and (GetFocus <> Handle) and CanFocus then
      SetFocus;
  end;
end;

procedure TMoneySpinButton.BtnClick(Sender: TObject);
begin
  if Sender = FUpButton then
  begin
    if Assigned(FOnUpClick) then FOnUpClick(Self);
  end
  else
    if Assigned(FOnDownClick) then FOnDownClick(Self);
end;

procedure TMoneySpinButton.SetFocusBtn (Btn: TMoneyTimerButton);
begin
  if TabStop and CanFocus and  (Btn <> FFocusedButton) then
  begin
    FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState - [tbFocusRect];
    FFocusedButton := Btn;
    if (GetFocus = Handle) then
    begin
       FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState + [tbFocusRect];
       Invalidate;
    end;
  end;
end;

procedure TMoneySpinButton.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TMoneySpinButton.Loaded;
var
  W, H: Integer;
begin
  inherited Loaded;
  W := Width;
  H := Height;
  AdjustSize (W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds (Left, Top, W, H);
end;


{ TMoneySpinEdit }

constructor TMoneySpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentCtl3D:=False;
  Ctl3D:=False;
  FButton := TMoneySpinButton.Create (Self);
  FButton.TabStop:=False;
  FButton.Width := 15;
  FButton.Height := 20;
  FButton.Ctl3D:=False;
  FButton.Visible := True;
  FButton.Parent := Self;
  FButton.FocusControl := Self;
  FButton.OnUpClick := UpClick;
  FButton.OnDownClick := DownClick;
  Text := '0.00';
  ControlStyle := ControlStyle - [csSetCaption];
  FIncrement := 1;
  FEditorEnabled := True;
end;

destructor TMoneySpinEdit.Destroy;
begin
  FButton.Free;
  FButton := nil;
  inherited Destroy;
end;

procedure TMoneySpinEdit.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TMoneySpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_UP) or (Key = VK_ADD) then UpClick (Self)
  else if (Key = VK_DOWN) or (Key = Vk_SUBTRACT) then DownClick (Self);
  inherited KeyDown(Key, Shift);
end;

procedure TMoneySpinEdit.KeyPress(var Key: Char);
begin
  if not IsValidChar(Key) then
  begin
    Key := #0;
    MessageBeep(0)
  end
  else if (Key = '+') or (Key = '-') then
  begin
    Key := #0;
  end;
  if Key <> #0 then inherited KeyPress(Key);
end;

function TMoneySpinEdit.IsValidChar(Key: Char): Boolean;
begin
  Result := (Key in [DecimalSeparator, '+', '-', '0'..'9']) or
    ((Key < #32) and (Key <> Chr(VK_RETURN)));
  if not FEditorEnabled and Result and ((Key >= #32) or
      (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE))) then
    Result := False;
end;

procedure TMoneySpinEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
{  Params.Style := Params.Style and not WS_BORDER;  }
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

procedure TMoneySpinEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

procedure TMoneySpinEdit.SetEditRect;
var
  Loc: TRect;
begin
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));
  Loc.Bottom := ClientHeight + 1;  {+1 is workaround for windows paint bug}
  Loc.Right := ClientWidth - FButton.Width - 2;
  Loc.Top := 0;
  Loc.Left := 0;
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));  {debug}
end;

procedure TMoneySpinEdit.WMSize(var Message: TWMSize);
var
  MinHeight: Integer;
begin
  inherited;
  MinHeight := GetMinHeight;
    { text edit bug: if size to less than minheight, then edit ctrl does
      not display the text }
  if Height < MinHeight then
    Height := MinHeight
  else if FButton <> nil then
  begin
    if NewStyleControls and Ctl3D then
      FButton.SetBounds(Width - FButton.Width - 5, 0, FButton.Width, Height-4)
    else FButton.SetBounds (Width - FButton.Width, 0, FButton.Width, Height);
    SetEditRect;
  end;
end;

function TMoneySpinEdit.GetMinHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  I := SysMetrics.tmHeight;
  if I > Metrics.tmHeight then I := Metrics.tmHeight;
  Result := Metrics.tmHeight + I div 4 + GetSystemMetrics(SM_CYBORDER) * 4 + 2;
end;

procedure TMoneySpinEdit.UpClick (Sender: TObject);
begin
  if ReadOnly then MessageBeep(0)
  else Value := Value + FIncrement;
end;

procedure TMoneySpinEdit.DownClick (Sender: TObject);
begin
  if ReadOnly then MessageBeep(0)
  else Value := Value - FIncrement;
end;

procedure TMoneySpinEdit.WMPaste(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TMoneySpinEdit.WMCut(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TMoneySpinEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  if CheckValue (Value) <> Value then
    SetValue (Value);
end;

function TMoneySpinEdit.GetValue: Double;
begin
  try
    if Text='' then
     Result:=0
    else
      Result := StrToFloat (Text);
  except
    Result := FMinValue;
  end;
end;

procedure TMoneySpinEdit.SetValue (NewValue: Double);
begin
  Text := FormatFloat('0.00',CheckValue (NewValue));
end;

function TMoneySpinEdit.CheckValue (NewValue: Double): Double;
begin
  Result := NewValue;
  if (FMaxValue <> FMinValue) then
  begin
    if NewValue < FMinValue then
      Result := FMinValue
    else if NewValue > FMaxValue then
      Result := FMaxValue;
  end;
end;

procedure TMoneySpinEdit.CMEnter(var Message: TCMGotFocus);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
end;

{TMoneyTimerButton}


constructor TMoneyTimerButton.Create(AOwner: TComponent);
const
 c:array[1..4] of string =('MSPIN_UPBLACK','MSPIN_DOWNBLACK','MSPIN_UPWHITE','MSPIN_DOWNWHITE');
var
 i:Integer;
begin
  inherited Create(AOwner);
  //ParentColor:=True;
  for i:=1 to 4 do
  begin
   FBitmaps[i]:=TBitmap.Create;
   FBitmaps[i].Transparent:=True;
   FBitmaps[i].LoadFromResourceName(hInstance,c[i]);
  end;
end;

destructor TMoneyTimerButton.Destroy;
var
 i:Integer;
begin
  if FRepeatTimer <> nil then
    FRepeatTimer.Free;
  for i:=1 to 4 do FBitmaps[i].Free;
  inherited Destroy;
end;

procedure TMoneyTimerButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown (Button, Shift, X, Y);
  if tbAllowTimer in FTimeBtnState then
  begin
    if FRepeatTimer = nil then
      FRepeatTimer := TTimer.Create(Self);

    FRepeatTimer.OnTimer := TimerExpired;
    FRepeatTimer.Interval := InitRepeatPause;
    FRepeatTimer.Enabled  := True;
  end;
end;

procedure TMoneyTimerButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
                                  X, Y: Integer);
begin
  inherited MouseUp (Button, Shift, X, Y);
  if FRepeatTimer <> nil then
    FRepeatTimer.Enabled  := False;
end;

procedure TMoneyTimerButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatPause;
  if (FDown) and MouseCapture then
  begin
    try
      Click;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;


procedure TMoneyTimerButton.Paint;
var
  R: TRect;
  L,T:Integer;
begin
 ChangeThePalette(Canvas);
 R:=Rect(0,0,Width,Height);
 if FDown then
  Canvas.Brush.Color:=DownColor  or MapToPalette
 else
  Canvas.Brush.Color:=Color or MapToPalette;
 Canvas.FillRect(R);

 Canvas.Brush.Color:=clBlack;
 Canvas.FrameRect(R);

 L:=R.Left+(((R.Right-R.Left) div 2)-(FBitmaps[1].Width div 2));
 T:=R.Top+(Height-FBitmaps[1].Height) div 2;

  if tbTypeDown in FTimeBtnState then
  begin
   if FDown or FMouseInControl then
    Canvas.Draw(L,T,FBitmaps[4])
   else
    Canvas.Draw(L,T,FBitmaps[2]);
  end
 else
  begin
   if FDown or FMouseInControl then
    Canvas.Draw(L,T,FBitmaps[3])
   else
    Canvas.Draw(L,T,FBitmaps[1]);
  end;
 ChangeThePalette(Canvas, True);
end;

{MoneyShortCutBar}

constructor TMoneyShortcutBar.Create(AOwner :TComponent);
begin
 inherited Create(AOwner);
 SetBounds(0,0,150,100);
 FTabs:=TStringList.Create;
 FTabs.Add('&Default');
 FTabs.OnChange:=TabsChanged;
 FHotTrack:=True;
 Color:=clBlack;
 Font.Color:=clWhite;
 FBorderColor:=$00708090;
 FOverColor:=$0050C0E0;
 FTabHeight:=24;
 FTopOffSet:=10;
 FLeftOffSet:=10;
 FRightOffSet:=3;
 FTabOver:=-1;
 FOldOver:=-1;
end;


destructor TMoneyShortcutBar.Destroy;
begin
 FTabs.Free;
 inherited Destroy;
end;

function TMoneyShortcutBar.TabCount:Integer;
begin
 result:=FTabs.Count;
end;

function TMoneyShortcutBar.TabRect(const Index:Integer):TRect;
var
 R:TRect;
begin
 with R do
 begin
  Left:=FLeftOffSet;
  Top:=(Index*FTabHeight);
  Right:=Width-FRightOffSet;
  Bottom:=Top+FTabHeight;
 end;
 R.Top:=R.Top+FTopOffSet;
 R.Bottom:=R.Bottom+FTopOffSet;
 result:=R;
end;

function TMoneyShortcutBar.TabFromMouse(X,Y:Integer):Integer;
begin
 result:=(Y-FTopOffSet) div FTabHeight;
end;

procedure TMoneyShortcutBar.TabsChanged(Sender :TObject);
begin
 Invalidate;
end;


procedure TMoneyShortcutBar.Paint;
var
 i:Integer;
 ARect:TRect;
begin
 ChangeThePalette(Canvas);
 for i:=0 to TabCount-1 do
 begin
   ARect:=TabRect(i);
   DrawTab(i,ARect,FTabOver=i,FTabIndex=i);
 end;
 ChangeThePalette(Canvas, True);
end;

procedure DrawLeft(ACanvas:TCanvas; R:TRect);
begin
with ACanvas,R do
 begin
  MoveTo(Right-1,Top-2);
  LineTo(Right-1,Top+1);
  LineTo(Left,Top+1);
  LineTo(Left,Bottom-1);
  LineTo(Right-1,Bottom-1);
  LineTo(Right-1,Bottom+2);
 end;
end;

procedure TMoneyShortcutBar.DrawTab(Index:Integer; ARect:TRect; Over:Boolean; Selected:Boolean);
var
 TheText:String;
 DrawRect,TextRect:TRect;
begin
 TheText:=FTabs[Index];
 DrawRect:=ARect;
 TextRect:=ARect;
 Canvas.Font:=Font;
 Canvas.brush.Color:=Color or MapToPalette;
 If Over then Canvas.Font.Color:=FOverColor  or MapToPalette
 else if Selected then Canvas.Font.Color := FSelectedColor or MapToPalette;

 if not Enabled then Canvas.Font.Color:=clGray;

 InflateRect(TextRect,-5,-2);
 InflateRect(DrawRect,0,-2);

 DrawText(Canvas.Handle,PChar(TheText),Length(TheText),TextRect,DT_LEFT or DT_VCENTER or DT_SINGLELINE);

 if Selected and Enabled then
 begin
  Canvas.Pen.Color:=FBorderColor or MapToPalette;
  DrawLeft(Canvas,DrawRect);
 end;
end;

function PtInRect(X, Y: Integer; const Rect: TRect): Boolean;
begin
 with Rect do Result := (X >= Left) and (X <= Right) and (Y >= Top) and
   (Y <= Bottom);
end;

procedure TMoneyShortcutBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
 R:TRect;
 T:Integer;
begin
 inherited MouseDown(Button,Shift,X,Y);
 if not Focused then SetFocus;
 T:=TabFromMouse(X,Y);
 R:=TabRect(T);
 if PtInRect(X,Y,R) then
 if (T>-1) and (T<TabCount) then TabIndex:=T;
end;

procedure TMoneyShortcutBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
 R:TRect;
 T,SaveTab:Integer;
begin
 inherited MouseMove(Shift,X,Y);
 if not FHotTrack then Exit;
 T:=TabFromMouse(X,Y);
 R:=TabRect(T);
 if (Y>FTopOffSet) and (Y<FTopOffSet+TabCount*FTabHeight) and (X>FLeftOffSet) then
 begin
   if PtInRect(X,Y,R) then
     begin
        SaveTab:=FTabOver;
        FTabOver:=T;
        if FOldOver<>FTabOver then
        begin
         InvalidateTab(SaveTab);
         InvalidateTab(FTabOver);
         FOldOver:=FTabOver;
        end;
     end;
 end else
 begin
   FTabOver:=-1;
   InvalidateTab(FOldOver);
   FOldOver:=-1;
 end;
end;

procedure TMoneyShortcutBar.CMMouseLeave(var Msg:TMessage);
begin
 inherited;
 If not FHotTrack then Exit;
 FTabOver:=-1;
 InvalidateTab(FOldOver);
 FOldOver:=-1;
end;


procedure TMoneyShortcutBar.InvalidateTab(Index:Integer);
var
 ARect:TRect;
begin
 if HandleAllocated then
 begin
   ARect:=TabRect(Index);
   InvalidateRect(Handle,@ARect,True);
 end else Invalidate;
end;

procedure TMoneyShortcutBar.Change;
begin
 if Assigned(FOnChange) then
  FOnChange(Self);
end;

procedure TMoneyShortcutBar.SetTabIndex(Value:Integer);
var
 FCanChange:Boolean;
begin
 if (Value>-1) and (Value<TabCount) and (FTabIndex<>Value) then
 begin
  FCanChange:=True;
  if Assigned(FOnChanging) then
  FOnChanging(Self,FCanChange,Value);
  if FCanChange then
  begin
   InvalidateTab(FTabIndex);
   FTabIndex:=Value;
   InvalidateTab(FTabIndex);
   Change;
  end;
 end;
end;

procedure TMoneyShortcutBar.SetHotTrack(Value:Boolean);
begin
 if FHotTrack<>Value then
 begin
  FHotTrack:=Value;
  Invalidate;
 end;
end;

procedure TMoneyShortcutBar.SetTabHeight(Value:Integer);
begin
 if FTabHeight<>Value then
 begin
  FTabHeight:=Value;
  Invalidate;
 end;
end;

procedure TMoneyShortcutBar.SetColor(Index:Integer; Value:TColor);
begin
 case Index of
  0:
   if FBorderColor<>Value then
    begin
      FBorderColor:=Value;
      Invalidate;
    end;
  1:
   if FOverColor<>Value then
    begin
      FOverColor:=Value;
      Invalidate;
    end;
  2:
   if FSelectedColor<>Value then
    begin
      FSelectedColor:=Value;
      Invalidate;
    end;
 end;
end;

procedure TMoneyShortcutBar.SetTopOffSet(Value:Integer);
begin
 if FTopOffSet<>Value then
 begin
  FTopOffSet:=Value;
  Invalidate;
 end;
end;

procedure TMoneyShortcutBar.SetLeftOffSet(Value:Integer);
begin
 if FLeftOffSet<>Value then
 begin
  FLeftOffSet:=Value;
  Invalidate;
 end;
end;

procedure TMoneyShortcutBar.SetRightOffSet(Value:Integer);
begin
 if FRightOffSet<>Value then
 begin
  FRightOffSet:=Value;
  Invalidate;
 end;
end;


procedure TMoneyShortcutBar.SetTabs(Value:TStringList);
begin
 FTabs.Assign(Value);
 Invalidate;
end;

procedure TMoneyShortcutBar.CMEnabledChanged(var Message: TMessage);
begin
 inherited;
 Invalidate;
end;




{MoneyTabControl}

constructor TMoneyTab.Create(Collection: TCollection);
begin
  FWidth := 85;
  inherited Create(Collection);
end;

procedure TMoneyTab.Assign(Source: TPersistent);
begin
  if Source is TMoneyTab then
  begin
    Caption := TMoneyTab(Source).Caption;
    Width := TMoneyTab(Source).Width;
    Alignment := TMoneyTab(Source).Alignment;
    Exit;
  end;
  inherited Assign(Source);
end;

function TMoneyTab.GetDisplayName: string;
begin
  Result := Caption;
  if Result = '' then Result := inherited GetDisplayName;
end;

function TMoneyTab.GetLeft: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Index - 1 do
    Inc(Result, TMoneyTabs(Collection)[I].Width);
end;

function TMoneyTab.GetRight: Integer;
begin
  Result := Left + Width;
end;

procedure TMoneyTab.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Changed(False);
  end;
end;


procedure TMoneyTab.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TMoneyTab.SetWidth(Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed(Index < Collection.Count - 1);
  end;
end;





{ TMoneyTabs }

constructor TMoneyTabs.Create(MoneyTabControl: TMoneyTabControl);
begin
  inherited Create(TMoneyTab);
  FMoneyTabControl := MoneyTabControl;
end;

function TMoneyTabs.Add: TMoneyTab;
begin
  Result := TMoneyTab(inherited Add);
  FMoneyTabControl.TabIndex:=Result.Index;
end;

function TMoneyTabs.GetItem(Index: Integer): TMoneyTab;
begin
  Result := TMoneyTab(inherited GetItem(Index));
end;

function TMoneyTabs.GetOwner: TPersistent;
begin
  Result := FMoneyTabControl;
end;

procedure TMoneyTabs.SetItem(Index: Integer; Value: TMoneyTab);
begin
  inherited SetItem(Index, Value);
end;

procedure TMoneyTabs.Update(Item: TCollectionItem);
begin
 {Removed Dec,13 1998}
 {
  Last Tab was not repained when chenging it's proporties
  at design time
 }
  //if Item <> nil then
  //  FMoneyTabControl.UpdateTab(Item.Index) else
    FMoneyTabControl.UpdateTabs;
end;



{ TMoneyTabControl }

constructor TMoneyTabControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle:=ControlStyle + [csAcceptsControls];
  SetBounds(0,0,200,100);
  Color:=$00E0F0F0;
  FBackColor:= $00C8DBDB;
  FBackTabColor:= $00906030;
  FBackTextColor:= clWhite;
  FTabHeight:=18;
  FSpace:=5;
  FMoneyTabs := TMoneyTabs.Create(Self);
  FMoneyTabs.Add.Caption:='&Deafult';
end;

destructor TMoneyTabControl.Destroy;
begin
  FMoneyTabs.Free;
  inherited Destroy;
end;

procedure TMoneyTabControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

procedure TMoneyTabControl.CreateWnd;
begin
  inherited CreateWnd;
end;


procedure TMoneyTabControl.SetMoneyTabs(Value: TMoneyTabs);
begin
  FMoneyTabs.Assign(Value);
end;

procedure TMoneyTabControl.UpdateTab(Index: Integer);
var
 ARect:TRect;
begin
 if HandleAllocated then
  if (Index>=0) and (Index<MoneyTabs.Count) then
   begin
    ARect:=GetTabRect(Index);
    InvalidateRect(Handle,@ARect,True);
   end;
end;

procedure TMoneyTabControl.UpdateTabs;
begin
  Invalidate;
end;


function TMoneyTabControl.GetTabRect(const Index:Integer):TRect;
begin
 if (Index>=0) and (Index<MoneyTabs.Count) then
 begin
  Result:=Rect(MoneyTabs.Items[Index].GetLeft,0,MoneyTabs.Items[Index].GetRight,FTabHeight);
  Result.Right:=Result.Right-FSpace;
 end
 else
  Result:=Rect(0,0,0,0);
end;

function TMoneyTabControl.GetTabFromMouse(const X,Y:Integer):Integer;
var
 i:Integer;
begin
 result:=-1;
 for i:=0 to MoneyTabs.Count-1 do
 begin
  if PtInRect(X,Y,GetTabRect(i)) then
  begin
   result:=i;
   Break;
  end;
 end;
end;

procedure TMoneyTabControl.SetIndex(Value: Integer);
var
 CanChange:Boolean;
begin
 if (Value>=0) and (Value<MoneyTabs.Count) and (FIndex<>Value) then
 begin
  CanChange:=True;
  if Assigned(FOnChanging) then
     FOnChanging(Self,CanChange,Value);
  if CanChange then
  begin
   UpdateTab(FIndex);
   FIndex:=Value;
   UpdateTab(FIndex);
   Change;
  end;
 end;
end;

procedure TMoneyTabControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
 TempTab:Integer;
begin
 inherited MouseDown(Button,Shift,X,Y);
 TempTab:=GetTabFromMouse(X,Y);
 if Enabled and (TempTab>-1) then
  if TempTab<>TabIndex then
    begin
      TabIndex:=TempTab;
    end;
end;

procedure TMoneyTabControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
 inherited MouseMove(Shift,X,Y);
end;

procedure DrawTop(ACanvas:TCanvas; R:TRect);
begin
with ACanvas,R do
 begin
  MoveTo(Left,Bottom-1);
  LineTo(Left,Top);
  LineTo(Right-1,Top);
  LineTo(Right-1,Bottom);
 end;
end;

procedure DrawBottom(ACanvas:TCanvas; R:TRect);
begin
with ACanvas,R do
 begin
  MoveTo(Left,Top+1);
  LineTo(Left,Bottom-1);
  LineTo(Right-1,Bottom-1);
  LineTo(Right-1,Top);
 end;
end;

procedure TMoneyTabControl.Paint;
var
 i:Integer;
 ARect:TRect;
 AState:TMoneyTabDrawState;
begin
 ChangeThePalette(Canvas);
 {Draw the back of tabs}
 Canvas.Brush.Color:=FBackColor or MapToPalette;
 ARect:=Rect(0,0,Width,FTabHeight);
 Canvas.FillRect(ARect);

 {Draw the frame for Client Area}
 Canvas.Pen.Color:=clGray;
 Canvas.Brush.Color:=Color or MapToPalette;
 Canvas.Rectangle(0,FTabHeight-1,Width,Height);

 Canvas.Font:=Font;

 for i:=0 to MoneyTabs.Count-1 do
 begin
   AState:=[];
   ARect:=GetTabRect(i);
   if FIndex=I then
   begin
     Include(AState,mtdsSelected);
     if Focused then Include(AState,mtdsFocused);
   end;

   DrawTab(i,ARect,AState);
   if FIndex=I then
   begin
      DrawTop(Canvas,ARect);
   end;
 end;
 ChangeThePalette(Canvas, True);
end;

procedure TMoneyTabControl.DrawTab(Index:Integer; ARect:TRect; AState:TMoneyTabDrawState);
var
 TheText:String;
 Flag:Integer;
begin
 if mtdsSelected in AState then
 begin
   Canvas.Brush.Color:=Color or MapToPalette;
   Canvas.Font.Color:=Font.Color or MapToPalette;
 end
 else
 begin
   Canvas.Brush.Color:=FBackTabColor or MapToPalette;
   Canvas.Font.Color:=FBackTextColor or MapToPalette;
 end;

 Canvas.FillRect(ARect);

 TheText:=MoneyTabs.Items[Index].Caption;
 if not Enabled then
  Canvas.Font.Color:=clGray;

 InflateRect(ARect,-4,-1);
 if MoneyTabs.Items[Index].Alignment=taLeftJustify then
  Flag:=DT_LEFT
 else
 if MoneyTabs.Items[Index].Alignment=taRightJustify then
  Flag:=DT_RIGHT
 else
  Flag:=DT_CENTER;

 DrawText(Canvas.Handle,PChar(TheText),Length(TheText),ARect,Flag or DT_VCENTER or DT_SINGLELINE);

end;


function TMoneyTabControl.GetClientRect: TRect;
begin
 with Result do
 begin
  Top:=FTabHeight+1;
  Left:=1;
  Bottom:=Height-1;
  Right:=Width-1;
 end;
end;

procedure TMoneyTabControl.SetTabHeight(Value :Integer);
begin
 if FTabHeight<>Value then
 begin
  FTabHeight:=Value;
  if not (csLoading in ComponentState) then
  begin
   ClientWidth:=ClientWidth-1;
   ClientWidth:=ClientWidth+1;
  end;
  //Invalidate;
 end;
end;

procedure TMoneyTabControl.SetSpace(Value :Integer);
begin
 if FSpace<>Value then
 begin
  FSpace:=Value;
  Invalidate;
 end;
end;

procedure TMoneyTabControl.SetColor(Index :Integer; Value :TColor);
begin
 case Index of
  1:
   if FBackColor<>Value then
   begin
     FBackColor:=Value;
     Invalidate;
   end;
  2:
   if FBackTextColor<>Value then
   begin
     FBackTextColor:=Value;
     Invalidate;
   end;
  3:
   if FBackTabColor<>Value then
   begin
     FBackTabColor:=Value;
     Invalidate;
   end;
 end;
end;

procedure TMoneyTabControl.Change;
begin
 if Assigned(FOnChange) then
  FOnChange(Self);
end;

procedure TMoneyTabControl.CMEnabledChanged(var Message: TMessage);
var
 i:Integer;
begin
 inherited;
 for i:=0 to MoneyTabs.Count-1 do
 UpdateTab(i);
end;


{MoneyComboBox}

function TMoneyComboBox.ButtonRect:TRect;
var
 ARect:TRect;
begin
  GetWindowRect(Handle, ARect);
  OffsetRect(ARect, -ARect.Left, -ARect.Top);
  Inc(ARect.Left, ClientWidth - FButtonWidth);
  InflateRect(ARect, -1, -1);
  result:=ARect;
end;


procedure TMoneyComboBox.WMPaint(var Message: TWMPaint);
var
 DC: HDC;
 PaintStruct: TPaintStruct;
begin
  if Message.DC = 0 then
    DC := BeginPaint(Handle, PaintStruct)
  else
    DC := Message.DC;
  try
    if inherited Style <> csSimple then
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

procedure TMoneyComboBox.DrawControlBorder(DC: HDC);
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

procedure TMoneyComboBox.DrawButton(DC: HDC; ARect:TRect);
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


constructor TMoneyComboBox.Create(AOwner:TComponent);
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

destructor TMoneyComboBox.Destroy;
var
 i:Integer;
begin
  for i:=1 to 2 do
  FBitmaps[i].Free;
  FCanvas.Free;
  inherited Destroy;
end;


procedure TMoneyComboBox.UpdateTracking;
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
end; {UpdateTracking}


procedure TMoneyComboBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then
  begin
    //if not Focused and TabStop then SetFocus;
    FDown := DroppedDown;
    InvalidateButton;
  end;
end; {MouseDown}

procedure TMoneyComboBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FDown:=False;
  UpdateTracking;
  inherited MouseUp(Button, Shift, X, Y);
end; {MouseUp}

procedure TMoneyComboBox.MouseMove(Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
end; {MouseUp}

procedure TMoneyComboBox.MouseEnter;
begin
 if not FMouseInControl and Enabled then
 begin
  FMouseInControl := True;
  InvalidateButton;
 end;
end; {MouseEnter}

procedure TMoneyComboBox.MouseLeave;
begin
 if FMouseInControl and Enabled then
 begin
  FMouseInControl := False;
  FDown:=False;
  InvalidateButton;
 end;
end; {MouseLeave}


procedure TMoneyComboBox.CMMouseEnter(var Msg:TMessage);
begin
 inherited;
 MouseEnter;
end; {CMMouseEnter}

procedure TMoneyComboBox.CMMouseLeave(var Msg:TMessage);
begin
 inherited;
 MouseLeave;
end; {CMMouseLeave}

procedure TMoneyComboBox.CMEnabledChanged(var Message: TMessage);
begin
  Repaint;
  inherited;
end; {CMEnabledChanged}


procedure TMoneyComboBox.InvalidateButton;
var
 ARect:TRect;
begin
 if HandleAllocated then
 begin
  ARect:=ButtonRect;
  InvalidateRect(Handle,@ARect,True)
 end;
end;

procedure TMoneyComboBox.SetComboStyle(Value:TMoneyComboStyle);
begin
 if FStyle<>Value then
 begin
  if Value=mcsDropDown then
   inherited Style:=csDropDown
  else
   inherited Style:=csDropDownList;
  FStyle:=Value;
 end;
end;

{TMoneypanel}

constructor TMoneyPanel.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  ControlStyle:=ControlStyle + [csAcceptsControls];
  SetBounds(0,0,200,100);
end;

procedure TMoneyPanel.Paint;
begin
 ChangeThePalette(Canvas);
 Canvas.Brush.Color:=Color or MapToPalette;
 Canvas.Font:=Font;
 Canvas.FillRect(Rect(0,0,Width,Height));
 ChangeThePalette(Canvas, True);
end;

function TMoneyPanel.GetClientRect: TRect;
begin
 with Result do
 begin
  Top:=FOffSets[1]+1;
  Left:=FOffSets[2]+1;
  Bottom:=Height-1-FOffSets[3];
  Right:=Width-1-FOffSets[4];
 end;
end;

procedure TMoneyPanel.SetOffSets(Index,Value:Integer);
begin
 if FOffSets[Index]<>Value then
 begin
   FOffSets[Index]:=Value;
   ClientWidth:=ClientWidth-1;
   ClientWidth:=ClientWidth+1;
 end;
end;






{MoneyCalendar}

function TMoneyCalendar.CellHeight:Integer;
begin
 result:=FMonthCal.Height div FMonthCal.RowCount;
end;

procedure TMoneyCalendar.AdjustControl;
begin

 FMonthCal.SetBounds(FOffSet,FOffSet+FTopOff,Width-2*FOffSet,Height-2*FOffSet-FTopOff);

 FMonthSelect.Left:=FMonthCal.Left;
 FMonthSelect.Top:=FMonthCal.Top+CellHeight;
 FMonthSelect.Width:=FMonthCal.Width;
 FMonthSelect.Height:=FMonthCal.Height-CellHeight;

 FMonthLeftButton.Left:=FOffSet;
 FMonthLeftButton.Top:=FOffSet;
 FMonthLeftButton.Height:=FTopOff;
 FMonthLeftButton.Width:=FButtonWidth;

 FMonthRightButton.Left:=Width-FButtonWidth-FOffSet;
 FMonthRightButton.Top:=FOffSet;
 FMonthRightButton.Height:=FTopOff;
 FMonthRightButton.Width:=FButtonWidth;

 FTitle.SetBounds(FOffSet+FButtonWidth,FOffSet,
                   Width-2*FButtonWidth-2*FOffSet,
                   FTopOff);

 Width:=2*FOffSet+FMonthCal.Width;

end;

procedure TMoneyCalendar.WMSize(var Msg:TWMSize);
begin
 inherited;
 AdjustControl;
end;

function TMoneyCalendar.GetStartOfWeek:TDayOfWeek;
begin
 result:=FMonthCal.StartOfWeek;
end;

procedure TMoneyCalendar.SetStartOfWeek(Value:TDayOfWeek);
begin
 FMonthCal.StartOfWeek:=Value;
end;

function TMoneyCalendar.GetColor(Index:Integer):TColor;
begin
 result:=0;
 case Index of
  1:result:=FMonthCal.WeekNamesColor;
  2:result:=FMonthCal.WeekNamesTextColor;
  3:result:=FMonthCal.TrailingColor;
  4:result:=FMonthCal.TrailingTextColor;
  5:result:=FMonthCal.SelectedColor;
  6:result:=FMonthCal.SelectedTextColor;
  7:result:=FMonthCal.SelectedBorderColor;
  8:result:=FMonthCal.BorderColor;
  9:result:=FMonthCal.TodaysColor;
  10:result:=FMonthCal.TodaysTextColor;
  11:result:=FMonthCal.TodaysBorderColor;
 end;
end;

procedure TMoneyCalendar.SetColor(Index:Integer; Value:TColor);
begin
 case Index of
  1:FMonthCal.WeekNamesColor:=Value;
  2:FMonthCal.WeekNamesTextColor:=Value;
  3:FMonthCal.TrailingColor:=Value;
  4:FMonthCal.TrailingTextColor:=Value;
  5:
   begin
    FMonthCal.SelectedColor:=Value;
    FMonthSelect.SelectedColor:=Value;
   end;
  6:
   begin
    FMonthCal.SelectedTextColor:=Value;
    FMonthSelect.SelectedTextColor:=Value;
   end;
  7:
   begin
    FMonthCal.SelectedBorderColor:=Value;
    FMonthSelect.SelectedBorderColor:=Value;
   end;
  8:
   begin
    FMonthCal.BorderColor:=Value;
    FMonthSelect.BorderColor:=Value;
   end;
  9: FMonthCal.TodaysColor:=Value;
  10: FMonthCal.TodaysTextColor:=Value;
  11: FMonthCal.TodaysBorderColor:=Value;
 end;
end;

procedure TMoneyCalendar.SetState(Value:TMoneyCalendarState);
begin
   if Value=csMonth then
   begin
    SetWindowPos(FMonthCal.Handle, HWND_TOP, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
    SetWindowPos(FMonthSelect.Handle, HWND_TOP, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
   end else
   begin
    SetWindowPos(FMonthSelect.Handle, HWND_TOP, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
    SetWindowPos(FMonthCal.Handle, HWND_TOP, 0, 0, 0, 0, SWP_NOZORDER or
     SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
   end;
   Invalidate;
   FState:=Value;
   UpDateTitle;
end;

constructor TMoneyCalendar.Create(AOwner: TComponent);
const
 COff:array[1..2] of String =('MCAL_LEFTOFF','MCAL_RIGHTOFF');
 COn :array[1..2] of String =('MCAL_LEFTON','MCAL_RIGHTON');
begin
 inherited Create(AOwner);
 SetBounds(0,0,146,140);
 Font.Color:=clWhite;
 Color:=clBlack;
 FTopOff:=15;
 FOffSet:=10;
 FButtonWidth:=10;
 FSelectText:='Select Month';

 {MonthLeftButton}
 FMonthLeftButton:=TMoneyImageButton.Create(Self);
 FMonthLeftButton.Parent:=Self;
 FMonthLeftButton.Bitmap.LoadFromResourceName(hInstance,COff[1]);
 FMonthLeftButton.BitmapOn.LoadFromResourceName(hInstance,COn[1]);
 FMonthLeftButton.OnClick:=DoLeftClick;


 {MonthRightButton}
 FMonthRightButton:=TMoneyImageButton.Create(Self);
 FMonthRightButton.Parent:=Self;
 FMonthRightButton.Bitmap.LoadFromResourceName(hInstance,COff[2]);
 FMonthRightButton.BitmapOn.LoadFromResourceName(hInstance,COn[2]);
 FMonthRightButton.OnClick:=DoRightClick;

 {Title}
 FTitle:=TMoneyLabel.Create(Self);
 FTitle.Parent:=Self;
 FTitle.Font.Color:=clWhite;
 FTitle.OverTextColor:=clRed;
 FTitle.OnClick:=DoTitleClick;

 {MonthSelect}
 FMonthSelect:=TMoneyMonthSelect.Create(Self);
 FMonthSelect.Parent:=Self;
 FMonthSelect.ParentColor:=True;
 FMonthSelect.ParentFont:=True;
 FMonthSelect.OnCellMouseUp:=DoMonthSelect;

 {Monthcal}
 FMonthCal:=TMoneyMonthCalendar.Create(Self);
 FMonthCal.Parent:=Self;
 FMonthCal.ParentColor:=True;
 FMonthCal.ParentFont:=True;
 FMonthCal.OnChange:=DoChange;
 FMonthCal.Date:=now;
 FMonthCal.OnCellMouseUp:=DoCellMouseUp;

end;

procedure TMoneyCalendar.CreateWnd;
begin
 inherited CreateWnd;
 FState:=csMonth;
 UpdateTitle;
 AdjustControl;

end;

procedure TMoneyCalendar.Paint;
var
 ARect:TRect;
begin
 ChangeThePalette(Canvas);
 Canvas.Brush.Color:=Color or MapToPalette;
 Canvas.FillRect(Rect(0,0,Width,Height));
 Canvas.Brush.Color:=BorderColor or MapToPalette;
 Canvas.FrameRect(Rect(5,5,Width-5,Height-5));

 if State = csYear then
 begin
  Canvas.Brush.Color:=WeekNamesColor or MapToPalette;
  Canvas.Font.Color:=WeekNamesTextColor or MapToPalette;
  ARect:=Rect(FOffSet,FOffSet+FTopOff,Width-FOffSet,FOffSet+FTopOff+CellHeight);
  Canvas.FillRect(ARect);
  DrawText(Canvas.Handle,PChar(FSelectText),Length(FSelectText),ARect,DT_CENTER or DT_VCENTER or DT_SINGLELINE);
 end;
 ChangeThePalette(Canvas, True);
end;

procedure TMoneyCalendar.DoCellMouseUp(Sender:TObject; Button: TMouseButton;Shift: TShiftState; ACol,ARow:Integer);
begin
 if Assigned(FOnCellMouseUp) then
  FOnCellMouseUp(Self,Button,Shift,ACol,ARow);
end;

procedure TMoneyCalendar.DoMonthSelect(Sender:TObject; Button: TMouseButton;Shift: TShiftState; ACol,ARow:Integer);
begin
 FMonthCal.Month:=FMonthSelect.Month;
 State:=csMonth;
end;

procedure TMoneyCalendar.DoLeftClick(Sender:TObject);
begin
 if State = csMonth then
  FMonthCal.PrevMonth
 else
  FMonthCal.PrevYear;
end;

procedure TMoneyCalendar.DoRightClick(Sender:TObject);
begin
 if State = csMonth then
  FMonthCal.NextMonth
 else
  FMonthCal.NextYear;
end;

procedure TMoneyCalendar.DoChange(Sender:TObject);
begin
 FMonthSelect.Month:=FMonthCal.Month;
 UpDateTitle;
 Change;
end;

procedure TMoneyCalendar.Change;
begin
 if Assigned(FOnChange) then
 FOnChange(Self);
end;

procedure TMoneyCalendar.UpDateTitle;
begin
 if State = csMonth then
   FTitle.Caption:=UpperCase(ShortMonthNames[FMonthCal.Month])+' '+IntToStr(FMonthCal.Year)
 else
   FTitle.Caption:=IntToStr(FMonthCal.Year);
end;

procedure TMoneyCalendar.DoTitleClick(Sender:TObject);
begin
 if State = csMonth then
 begin
   FMonthSelect.Month:=FMonthCal.Month;
   State:=csYear;
 end else
 begin
   FMonthCal.Month:=FMonthSelect.Month;
   State:=csMonth;
 end;
end;

procedure TMoneyCalendar.SetSelectText(Value:String);
begin
 if FSelectText<>Value then
 begin
  FSelectText:=Value;
  if State = csYear then
   Invalidate;
 end;
end;

function TMoneyCalendar.GetDate:TDateTime;
begin
 result:=FMonthCal.Date;
end;

procedure TMoneyCalendar.SetDate(Value:TDateTime);
begin
 FMonthCal.Date:=Value;
end;

function TMoneyCalendar.GetClientRect: TRect;
begin
 result:=inherited GetClientRect;
end;


{TMoneyImageButton}

  constructor TMoneyImageButton.Create(AOwner:TComponent);
  var
   i:Integer;
  begin
   inherited Create(AOwner);
   SetBounds(0,0,50,50);
   ParentColor:=True;
   for i:=1 to 3 do
   begin
    FBitmaps[i]:=TBitmap.Create;
    FBitmaps[i].Transparent:=True;
   end;
   TabStop:=False;
  end;

  destructor TMoneyImageButton.Destroy;
  var
   i:Integer;
  begin
   for i:=1 to 3 do FBitmaps[i].Free;
   inherited Destroy;
  end;

  procedure TMoneyImageButton.Paint;
  var
   X,Y:Integer;
   Index:Integer;
  begin
   ChangeThePalette(Canvas);
   Canvas.Brush.Color:=Color or MapToPalette;
   Canvas.Pen.Color:=Color or MapToPalette;
   Canvas.Font:=Font;
   Index:=1;
   if msOver in MouseState then
   begin
    Index:=2;
    if FBitmaps[Index].Empty then
    Index:=1;
   end;
   if msDown in MouseState then
   begin
    Canvas.Brush.Color:=DownColor or MapToPalette;
    Index:=3;
    if FBitmaps[Index].Empty then
    Index:=1;
   end;

   Canvas.FillRect(ClientRect);

   X:= (Width div 2) - (FBitmaps[Index].Width div 2);
   Y:= (Height div 2) - (FBitmaps[Index].Height div 2);

   if Enabled then
    Canvas.Draw(X,Y,FBitmaps[Index])
   else
    DrawState(Canvas.Handle,0,nil,FBitmaps[Index].Handle,0,
     X,X,0,0,DST_BITMAP or DSS_DISABLED);
   ChangeThePalette(Canvas, True);
  end;

  procedure TMoneyImageButton.SetBitmap(Index :Integer; Value:TBitmap);
  begin
   FBitmaps[Index].Assign(Value);
   Invalidate;
  end;



{MoneyMonthCalendar}

const
  CShortDayNames:array[1..7] of String =('Su','Mo','Tu','We','Th','Fr','Sa');

function IsLeapYear(AYear: Integer): Boolean;
begin
  Result := (AYear mod 4 = 0) and ((AYear mod 100 <> 0) or (AYear mod 400 = 0));
end;

function DaysPerMonth(AYear, AMonth: Integer): Integer;
const
  DaysInMonth: array[1..12] of Integer = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
begin
  Result := DaysInMonth[AMonth];
  if (AMonth = 2) and IsLeapYear(AYear) then Inc(Result);
end;


function TMoneyMonthCalendar.GetCellValue(ACol,ARow:Integer):TPoint;
var
  DayNum: Integer;
begin
  if ARow = 0 then
   with Result do
   begin
    X:= (StartOfWeek + ACol) mod 7 + 1;
    Y:=MMC_WEEKDAYNAMES;
   end
  else
   begin
    DayNum := FMonthOffset + ACol + (ARow - 1) * 7;
    if (DayNum < 1) or (DayNum > DaysPerMonth(Year,Month)) then
      begin
        if DayNum < 1 then
         with Result do
          begin
           if Month = 1 then
            X:=DaysPerMonth(Year-1,12)+DayNum
           else
            X:=DaysPerMonth(Year,Month-1)+DayNum;
           Y:=MMC_TRAILINGLEFT;
          end else
        if DayNum > DaysPerMonth(Year,Month) then
         with Result do
          begin
           if Month=12 then
              X:=DayNum - DaysPerMonth(Year+1,1)
           else
              X:=DayNum - DaysPerMonth(Year,Month);
           Y:=MMC_TRAILINGRIGHT;
          end;
     end
    else
     with Result do
      begin
       X:=DayNum;
       Y:=MMC_WEEKDAYS;
      end;
  end;
end;

function TMoneyMonthCalendar.GetCellText(ACol,ARow: Integer): string;
var
  X,Y:Integer;
begin
  X:=GetCellValue(ACol,ARow).X;
  Y:=GetCellValue(ACol,ARow).Y;
  if Y = MMC_WEEKDAYNAMES then
   Result := CShortDayNames[X]
  else
   Result := IntToStr(X)
end;

procedure TMoneyMonthCalendar.CellClick(ACol,ARow:Integer);
var
 ACellIndex:Integer;
 ADay:Integer;
begin
 ACellIndex:=GetCellValue(ACol,ARow).Y;
 ADay:=GetCellValue(ACol,ARow).X;
 if ACellIndex=MMC_WEEKDAYS then
 begin
  Day:=ADay;
 end
 else if ACellIndex = MMC_TRAILINGLEFT then
 begin
  try
   FTrailing:=True;
   SetDate(IncMonth(Self.Date,-1));
   Day:=ADay;
  finally
   FTrailing:=False;
  end;
 end
 else if ACellIndex = MMC_TRAILINGRIGHT then
 begin
  try
   FTrailing:=True;
   SetDate(IncMonth(Self.Date,1));
   Day:=ADay;
  finally
   FTrailing:=False;
  end;
 end;
end;

function TMoneyMonthCalendar.NextMonth:TDateTime;
begin
 SetDate(IncMonth(Self.Date,1));
 result:=Self.Date;
end;

function TMoneyMonthCalendar.PrevMonth:TDateTime;
begin
 SetDate(IncMonth(Self.Date,-1));
 result:=Self.Date;
end;

function TMoneyMonthCalendar.NextYear:TDateTime;
begin
 SetYear(Year+1);
 result:=Self.Date;
end;

function TMoneyMonthCalendar.PrevYear:TDateTime;
begin
 SetYear(Year-1);
 result:=Self.Date;
end;

procedure TMoneyMonthCalendar.DrawWeekDayName(AWeekDay:Integer; ARect:TRect);
var
 TheText:String;
begin
  Canvas.Font.Color:=clBtnFace;
  Canvas.Brush.Color:=$004F4F4F;
  Canvas.FillRect(ARect);
  TheText:=CellText[AWeekDay,0];
end;

procedure TMoneyMonthCalendar.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TMoneyGridDrawState);
var
 TheText:String;
 ACellIndex:Integer;
 ACellValue:Integer;
 IsToday:Boolean;
 TempDate:TDateTime;
begin
 ACellIndex:=GetCellValue(ACol,ARow).Y;
 ACellValue:=GetCellValue(ACol,ARow).X;

 IsToday:=False;
 if ACellIndex = MMC_WEEKDAYS then
 begin
   TempDate:=EncodeDate(Year,Month,ACellValue);
   IsToday:=Trunc(TempDate) = Trunc(Now);
 end;

 TheText:=CellText[ACol,ARow];

 Canvas.Brush.Color:=Color;
 Canvas.Font:=Font;

 if ACellIndex = MMC_WEEKDAYNAMES then
 begin
  Canvas.Brush.Color:=WeekNamesColor;
  Canvas.Font.Color:=WeekNamesTextColor;
 end;

 if (ACellIndex=MMC_TRAILINGLEFT) or (ACellIndex=MMC_TRAILINGRIGHT) then
 begin
  Canvas.Brush.Color:=TrailingColor;
  Canvas.Font.Color:=TrailingTextColor;
 end;

 if dsSelected in AState then
 begin
  Canvas.Brush.Color:=SelectedColor;
  Canvas.Font.Color:=SelectedTextColor;
 end;

 if IsToday then
 begin
  Canvas.Brush.Color:=TodaysColor;
  Canvas.Font.Color:=TodaysTextColor;
 end;

 if ACellIndex <> MMC_WEEKDAYNAMES then
  InflateRect(ARect,-1,-1);

 Canvas.FillRect(ARect);
 DrawText(Canvas.Handle,PChar(TheText),Length(TheText),ARect,DT_CENTER or DT_VCENTER or DT_SINGLELINE);

 if ACellIndex <> MMC_WEEKDAYNAMES then
 begin
  InflateRect(ARect,1,1);

  if ARow < ColCount then
  begin
   Canvas.Pen.Color:=BorderColor;
   Canvas.MoveTo(ARect.Left,ARect.Bottom);
   Canvas.LineTo(ARect.Right,ARect.Bottom);
  end;
 end;
 
 if IsToday then
 begin
  InflateRect(ARect,-1,-1);
  Canvas.Brush.Color:=TodaysBorderColor;
  Canvas.FrameRect(ARect);
  InflateRect(ARect,1,1);
 end;

 if dsSelected in AState then
 begin
  InflateRect(ARect,-1,-1);
  Canvas.Brush.Color:=SelectedBorderColor;
  Canvas.FrameRect(ARect);
  InflateRect(ARect,1,1);
 end;

end;

procedure TMoneyMonthCalendar.Paint;
begin
 ChangeThePalette(Canvas);
 inherited Paint;
 ChangeThePalette(Canvas, True);
end;

constructor TMoneyMonthCalendar.Create(AOwner: TComponent);
var
  wYear, wMonth, wDay: Word;
begin
  inherited Create(AOwner);
  DecodeDate(now,wYear,wMonth,wDay);
  FYear:=wYear;
  FMonth:=wMonth;
  FDay:=wDay;

  BorderColor:=$004F4F4F;
  WeekNamesColor:=$004F4F4F;
  WeekNamesTextColor:=clWhite;

  TrailingColor:=clBlack;
  TrailingTextColor:=clGray;

  SelectedColor:=clGray;
  SelectedTextColor:=clWhite;
  SelectedBorderColor:=clGray;

  TodaysColor:=$00BF5F5F;
  TodaysBorderColor:=$00BF5F5F;
  TodaysTextColor:=clWhite;

  Color:=clBlack;
  Font.Color:=clWhite;


  SetBounds(0,0,119,119);
  FStartOfWeek:=1;
  TabStop:=False;
  ColCount := 7;
  RowCount := 7;
  UpdateCalendar;
end;


procedure TMoneyMonthCalendar.SetColor(Index:Integer; Value:TColor);
begin
 if FColors[Index] <> Value then
 begin
  FColors[Index]:=Value;
  Invalidate;
 end;
end;

procedure TMoneyMonthCalendar.UpdateCalendar;
var
  FirstDate: TDateTime;
begin
  FUpdating := True;
  try
    FirstDate := EncodeDate(FYear, FMonth, 1);
    FMonthOffset := 2 - ((DayOfWeek(FirstDate) - StartOfWeek + 7) mod 7);
    if FMonthOffset = 2 then FMonthOffset := -5;
    Col:=(FDay - FMonthOffset) mod 7;
    Row:=(FDay - FMonthOffset) div 7 + 1;
  finally
    FUpdating := False;
  end;
end;



procedure TMoneyMonthCalendar.SetDate(Value: TDateTime);
var
 wYear,wMonth,wDay:Word;
begin
  DecodeDate(Value,wYear,wMonth,wDay);
  try
   FChanging:=True;
   SetYear(wYear);
   SetMonth(wMonth);
   SetDay(wDay);
   if not FTrailing then Change;
  finally
   FChanging:=False;
  end;
end;

function TMoneyMonthCalendar.GetDate: TDateTime;
begin
  result:=EncodeDate(FYear,FMonth,FDay)
end;

procedure TMoneyMonthCalendar.Change;
begin
 if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TMoneyMonthCalendar.SetYear(Value:Integer);
begin
  if FYear <> Value then
  begin
   if IsLeapYear(FYear) and (FMonth = 2) and (FDay = 29) then FDay := 28;
   FYear:=Value;
   UpdateCalendar;
   Invalidate;
   if not FChanging then Change;
  end;
end;


procedure TMoneyMonthCalendar.SetMonth(Value:Integer);
begin
  if (FMonth <> Value) and (Value>0) and (Value<=12)then
  begin
   if FDay > DaysPerMonth(FYear, Value) then FDay := DaysPerMonth(FYear, Value);

   FMonth:=Value;
   UpdateCalendar;
   Invalidate;
   if not FChanging then Change;
  end;
end;

procedure TMoneyMonthCalendar.SetDay(Value:Integer);
begin
  if (FDay <> Value) and (Value>0) and (Value <=DaysPerMonth(FYear,FMonth)) then
  begin
   FDay:=Value;
   UpdateCalendar;
   if not FChanging then Change;
  end;
end;


function TMoneyMonthCalendar.SelectCell(ACol, ARow: Integer): Boolean;
begin
 result:=(not FUpdating) and (not FReadOnly) and (ARow<>0);
end;


procedure TMoneyMonthCalendar.SetStartOfWeek(Value: TDayOfWeek);
begin
  if Value <> FStartOfWeek then
  begin
    FStartOfWeek := Value;
    UpdateCalendar;
    Invalidate;
  end;
end;

{MoneyMonthSelect}

constructor TMoneyMonthSelect.Create(AOwner :TComponent);
begin
 inherited Create(AOwner);
 Col:=0;
 Row:=0;
 ColCount:=4;
 RowCount:=3;
 Color:=clBlack;
 Font.Color:=clWhite;
 SelectedColor:=clGray;
 SelectedBorderColor:=clBlack;
 SelectedTextColor:=clWhite;
 BorderColor:=$004F4F4F;
 SetBounds(0,0,120,102);
end;

procedure TMoneyMonthSelect.DrawCell(ACol,ARow:Integer; ARect:TRect; AState:TMoneyGridDrawState);
var
 TheText:String;
begin
 Canvas.Brush.Color:=Color;
 Canvas.Font:=Font;
 if dsSelected in AState then
 begin
  Canvas.Brush.Color:=SelectedColor;
  Canvas.Font.Color:=SelectedTextColor;
 end;
 InflateRect(ARect,-1,-1);
 Canvas.FillRect(ARect);
 TheText:=ShortMonthNames[MonthFromCell(ACol,ARow)];
 DrawText(Canvas.Handle,PChar(TheText),Length(TheText),ARect,DT_CENTER or DT_VCENTER or DT_SINGLELINE);
 if dsSelected in AState then
 begin
  Canvas.Brush.Color:=SelectedBorderColor;
  Canvas.FrameRect(ARect);
 end;

 if ARow<>0 then
 begin
  InflateRect(ARect,1,1);
  Canvas.Pen.Color:=BorderColor;
  Canvas.MoveTo(ARect.Left,ARect.Top-1);
  Canvas.LineTo(ARect.Right,ARect.Top-1);
 end;
end;

function TMoneyMonthSelect.MonthFromCell(ACol,ARow:Integer):Integer;
begin
 result:=(ACol+1)+(ARow*4);
end;

function TMoneyMonthSelect.GetMonth:Integer;
begin
 result:=MonthFromCell(Col,Row);
end;

procedure TMoneyMonthSelect.SetMonth(Value:Integer);
begin
 if (Month<>Value) then
  case Value of
   1..4:
       begin
         Row:=0;
         Col:=Value-1;
       end;
   5..8:
       begin
         Row:=1;
         Col:=(Value-4)-1;
       end;
   9..12:
       begin
         Row:=2;
         Col:=(Value-8)-1;
       end;
  end;
end;


procedure TMoneyMonthSelect.SetColor(Index:Integer; Value:TColor);
begin
 if FColors[Index] <> Value then
 begin
  FColors[Index]:=Value;
  Invalidate;
 end;
end;

{MoneyCustomGrid}

constructor TMoneyCustomGrid.Create(AOwner :TComponent);
begin
 inherited Create(AOwner);
 Setbounds(0,0,140,140);
 FCol:=-1;
 FRow:=-1;
 FRowCount:=7;
 FColCount:=7;
end;

procedure TMoneyCustomGrid.Paint;
var
 TempState:TMoneyGridDrawState;
 i,j:Integer;
begin
 ChangeThePalette(Canvas);
 Canvas.Font:=Font;
 for i:=0 to FColCount-1 do
  for j:=0 to FRowCount-1 do
   begin
     TempState:=[];
     if (i=FCol) and (j=FRow) then
      begin
       Include(TempState,dsSelected);
       if Focused then Include(TempState,dsFocused);
      end;
     DrawCell(i,j,CellRect(i,j),TempState);
   end;
 ChangeThePalette(Canvas, True);
end;

procedure TMoneyCustomGrid.DrawCell(ACol,ARow:Integer; ARect:TRect; AState:TMoneyGridDrawState);
begin
end;

procedure TMoneyCustomGrid.MouseDown(Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
var
 TempCol,TempRow:Integer;
begin
 if not Focused and TabStop then SetFocus;
 TempCol:=CellFromMouse(X,Y).X;
 TempRow:=CellFromMouse(X,Y).Y;
 if (Button=mbLeft) then
  if SelectCell(TempCol,TempRow) then
  begin
   Col:=TempCol;
   Row:=TempRow;
   CellClick(Col,Row);
  end;
 inherited MouseDown(Button,Shift,X,Y);
end;

procedure TMoneyCustomGrid.MouseUp(Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
begin
 if (Button=mbLeft) then
  if (CellFromMouse(X,Y).X= Col) and (CellFromMouse(X,Y).Y = Row) then
   CellMouseup(Button,Shift,Col,Row);
 inherited MouseUp(Button,Shift,X,Y);
end;

procedure TMoneyCustomGrid.CellMouseUp(Button: TMouseButton;Shift: TShiftState; ACol, ARow: Integer);
begin
  if Assigned(FOnCellMouseUp) then
    FOnCellMouseUp(Self,Button,Shift,ACol,ARow);
end;

procedure TMoneyCustomGrid.CellClick(ACol,ARow:Integer);
begin
 if Assigned(FOnCellClick) then
  FOnCellClick(Self);
end;

function TMoneyCustomGrid.CellRect(ACol,ARow:Integer):TRect;
var
 CellWidth,CellHeight:Integer;
begin
 CellWidth:=Width div FColCount;
 CellHeight:=Height div FRowCount;
 with Result do
 begin
  Left:=ACol*CellWidth;
  Top:=ARow*CellHeight;
  Right:=Left+CellWidth;
  Bottom:=Top+CellHeight;
 end;
end;

function  TMoneyCustomGrid.CellFromMouse(X,Y:Integer):TPoint;
begin
 Result.X:=X div (Width div FColCount);
 Result.Y:=Y div (Height div FRowCount);
end;

procedure TMoneyCustomGrid.InvalidateCell(ACol,ARow:Integer);
var
 DrawRect:TRect;
begin
 DrawRect:=CellRect(ACol,ARow);
 if HandleAllocated then
  InvalidateRect(Handle,@DrawRect,True)
 else
  Invalidate;
end;

function TMoneyCustomGrid.SelectCell(ACol,ARow:Integer):Boolean;
begin
 Result:=True;
end;

procedure TMoneyCustomGrid.SetCount(Index,Value:Integer);
begin
 case Index of
  1:
    if FColCount<>Value then
    begin
      FColCount:=Value;
      Invalidate;
    end;
  2:
    if FRowCount<>Value then
    begin
      FRowCount:=Value;
      Invalidate;
    end;
 end;
end;

procedure TMoneyCustomGrid.SetCell(Index,Value:Integer);
begin
 case Index of
  1:
    if (FCol<>Value) then
    begin
      InvalidateCell(FCol,FRow);
      FCol:=Value;
      InvalidateCell(FCol,FRow);
    end;
  2:
    if FRow<>Value then
    begin
      InvalidateCell(FCol,FRow);
      FRow:=Value;
      InvalidateCell(FCol,FRow);
    end;
 end;
end;

procedure TMoneyCustomGrid.ResetSize;
var
 CellWidth,CellHeight:Integer;
begin
 CellWidth:=Width div FColCount;
 CellHeight:=Height div FRowCount;
 Width:=CellWidth*FColCount;
 Height:=CellHeight*FRowCount;
end;

procedure TMoneyCustomGrid.WMSize(var Msg:TWMSize);
begin
 ResetSize;
 Inherited;
end;



{MoneyDateEdit}

procedure TMoneyDateEdit.ButtonClick (Sender: TObject);
begin
  if FCalVisible then CloseUp(False) else
    DropDown;
end;

procedure TMoneyDateEdit.CalMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  //if Button = mbLeft then
    CloseUp(True);
end;

procedure TMoneyDateEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if ReadOnly then Exit;
  if (Text='') or (Char(Key)='t') or (Char(Key)='T') then
  begin
   SetDate(Now);
   DoChange;
  end
  else
  begin
   if (Key=VK_ADD) or (Key=VK_UP)  or (Key=187) then
    begin
      SetDate(Date+FIncrement);
      DoChange;
    end
   else
   if (Key=VK_SUBTRACT) or (Key=VK_DOWN) or (Key=189) then
    begin
      SetDate(Date-FIncrement);
      DoChange;
    end;
  end;

  if (Key=VK_RETURN) then
   if FCalVisible then CloseUp(True)
   else
    DoChange;


  inherited KeyDown(Key, Shift);
end;

procedure TMoneyDateEdit.KeyPress(var Key: Char);
begin
  if not IsValidChar(Key) then
  begin
    Key := #0;
    if not FEditorEnabled and ((Key='+') or (Key='-') or (Key='t') or (Key='T')) then
    MessageBeep(0)
  end else
  begin
   if (Key='+') or (Key='-') or (Key='t') or (Key='T') then
   begin
    Key :=#0;
   end;
  end;

  if Key <> #0 then inherited KeyPress(Key);
end;

function TMoneyDateEdit.IsValidChar(Key: Char): Boolean;
begin
  Result := (Key in [DateSeparator, '+', '-', '0'..'9','t','T']) or
    ((Key < #32) and (Key <> Chr(VK_RETURN)));
  if not FEditorEnabled and Result and ((Key >= #32) or
      (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE))) then
    Result := False;
end;


constructor TMoneyDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := TMoneyDateButton.Create (Self);
  FButton.Width := GetSystemMetrics(SM_CXVSCROLL)+2;
  FButton.Align:=alRight;
  FButton.Visible := True;
  FButton.Parent := Self;
  FButton.OnClick := ButtonClick;

  FCal := TMoneyPopupCal.Create(Self);
  FCal.ParentColor:=False;
  FCal.Visible := False;
  FCal.Parent := Self;
  FCal.OnCellMouseUp := CalMouseUp;

  ShortDateFormat := 'mm/dd/yyyy';
  Text := '';
  ParentCtl3D:=False;
  //MaxLength:=Length(DateToStr(now))+2;
  Ctl3D:=False;
  ControlStyle := ControlStyle - [csSetCaption];
  FIncrement := 1;
  FEditorEnabled := True;
end;

destructor TMoneyDateEdit.Destroy;
begin
  FButton := nil;
  inherited Destroy;
end;


procedure TMoneyDateEdit.CloseUp(Accept: Boolean);
begin
  if FCalVisible then
  begin
    if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    SetWindowPos(FCal.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
    FCalVisible := False;
    FCal.Visible:=False;
    Invalidate;
    if not ReadOnly and Accept then
    begin
      SetDate(FCal.Date);
      DoChange;
    end;
    if Assigned(FOnCloseUp) then FOnCloseUp(Self);
  end;
end;

procedure TMoneyDateEdit.DropDown;
var
  P: TPoint;
  Y: Integer;
begin
  if not FCalVisible then
  begin
    if Assigned(FOnDropDown) then FOnDropDown(Self);
    P := Parent.ClientToScreen(Point(Left, Top));
    Y := P.Y + Height;
    if Y + FCal.Height > Screen.Height then Y := P.Y - FCal.Height;
    case FDropDownAlign of
      mdaRight: Dec(P.X, FCal.Width - Width);
      mdaCenter: Dec(P.X, (FCal.Width - Width) div 2);
    end;
    if CanFocus then SetFocus;
    FCal.State:=csMonth;
    if (Text='') or (Date=0) then
    begin
     FCal.Date:=now;
     //SetDate(now);
    end
    else
     FCal.Date:=Self.Date;

    SetWindowPos(FCal.Handle, HWND_TOP, P.X, Y, 0, 0,
      SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
    FCal.Visible:=True;
    FCalVisible := True;
    Repaint;
  end;
end;

procedure TMoneyDateEdit.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TMoneyDateEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
{  Params.Style := Params.Style and not WS_BORDER;  }
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

procedure TMoneyDateEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

procedure TMoneyDateEdit.SetEditRect;
var
  Loc: TRect;
begin
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));
  Loc.Bottom := ClientHeight + 1;  {+1 is workaround for windows paint bug}
  Loc.Right := ClientWidth - FButton.Width - 2;
  Loc.Top := 0;
  Loc.Left := 0;
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));  {debug}
  Repaint;
end;

procedure TMoneyDateEdit.WMSize(var Message: TWMSize);
begin
  inherited;
  Height := GetMinHeight;
  SetEditRect;
end;


function TMoneyDateEdit.GetMinHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  I := SysMetrics.tmHeight;
  if I > Metrics.tmHeight then I := Metrics.tmHeight;
  Result := Metrics.tmHeight + I div 4 + GetSystemMetrics(SM_CYBORDER) * 4+1;
end;


function TMoneyDateEdit.GetSelectText:String;
begin
 result:=FCal.SelectText;
end;

procedure TMoneyDateEdit.SetSelectText(Value:String);
begin
 if FCal.SelectText<>Value then
  FCal.SelectText:=Value;
end;

function TMoneyDateEdit.GetStartOfWeek:TDayOfWeek;
begin
 result:=FCal.StartOfWeek;
end;

procedure TMoneyDateEdit.SetStartOfWeek(Value:TDayOfWeek);
begin
 FCal.StartOfWeek:=Value;
end;


function TMoneyDateEdit.GetDate: TDateTime;
begin
  //result:=Trunc(FDate);
  try
    if Text ='' then
     Result:=0
    else
     Result := StrToDate (Text);
  except
     if Assigned(FOnError) then FOnError(Self);
     Result := 0;
  end;
end;

procedure TMoneyDateEdit.SetDate(Value: TDateTime);
begin
  if Value=0 then
   Text:=''
  else
   Text := DateToStr(Value);
   FCal.Date:=Value;
   //DoChange;
end;

procedure TMoneyDateEdit.DoChange;
begin
 if Not FExiting then
   if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TMoneyDateEdit.CMEnter(var Message: TCMGotFocus);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
end;

procedure TMoneyDateEdit.WMPaste(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TMoneyDateEdit.WMCut(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TMoneyDateEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  try
   FExiting:=True;
   SetDate(Date);
   DoChange;
  finally
   FExiting:=False;
  end;
end;

procedure TMoneyDateEdit.CMCancelMode(var Message: TCMCancelMode);
begin
 if (Message.Sender <> Self) and (Message.Sender <> FCal) and
    (Message.Sender <> FButton) and ((FCal <> nil) and
    not FCal.ContainsControl(Message.Sender)) then
    CloseUp(False);
end;

procedure TMoneyDateEdit.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls then
  begin
    RecreateWnd;
    Height := 0;
  end;
  inherited;
end;

procedure TMoneyDateEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Height := 0;
end;


procedure TMoneyDateEdit.WMCancelMode(var Message: TMessage);
begin
  inherited;
end;

procedure TMoneyDateEdit.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  CloseUp(False);
end;



function TMoneyDateEdit.GetColor(Index:Integer):TColor;
begin
 result:=0;
 case Index of
  1:result:=FCal.WeekNamesColor;
  2:result:=FCal.WeekNamesTextColor;
  3:result:=FCal.TrailingColor;
  4:result:=FCal.TrailingTextColor;
  5:result:=FCal.SelectedColor;
  6:result:=FCal.SelectedTextColor;
  7:result:=FCal.SelectedBorderColor;
  8:result:=FCal.BorderColor;
  9:result:=FCal.TodaysColor;
  10:result:=FCal.TodaysTextColor;
  11:result:=FCal.TodaysBorderColor;
 end;
end;


procedure TMoneyDateEdit.SetColor(Index:Integer; Value:TColor);
begin
 case Index of
  1:FCal.WeekNamesColor:=Value;
  2:FCal.WeekNamesTextColor:=Value;
  3:FCal.TrailingColor:=Value;
  4:FCal.TrailingTextColor:=Value;
  5:FCal.SelectedColor:=Value;
  6:FCal.SelectedTextColor:=Value;
  7:FCal.SelectedBorderColor:=Value;
  8:FCal.BorderColor:=Value;
  9:FCal.TodaysColor:=Value;
  10:FCal.TodaysTextColor:=Value;
  11:FCal.TodaysBorderColor:=Value;
 end;
end;


{TMoneyDateButton}
constructor TMoneyDateButton.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 Bitmap.LoadFromResourceName(hInstance,'MSPIN_DOWNBLACK');
 BitmapOn.LoadFromResourceName(hInstance,'MSPIN_DOWNWHITE');
 BitmapDown.LoadFromResourceName(hInstance,'MSPIN_DOWNWHITE');
 ParentColor:=False;
 DownColor:=clBlack;
 Color:=clMoneyBtnFace;
end;

procedure TMoneyDateButton.Paint;
begin
 ChangeThePalette(Canvas);
 inherited Paint;
 Canvas.Brush.Color:=clBlack;
 Canvas.FrameRect(Rect(0,0,Width,Height));
 ChangeThePalette(Canvas, True);
end;


{ TMoneyPopupCal }

constructor TMoneyPopupCal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable];
end;

procedure TMoneyPopupCal.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW;
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

procedure TMoneyPopupCal.WMMouseActivate(var Message: TMessage);
begin
  Message.Result := MA_NOACTIVATE;
end;




procedure Register;
begin
  RegisterComponents('Money Controls',
  [TMoneyButton,
  TMoneySpinButton,
  TMoneyCheckBox,
  TMoneyRadioButton,
  TMoneyLabel,
  TMoneySpinEdit,
  TMoneyComboBox,
  TMoneyDateEdit,
  TMoneyTabControl,
  TMoneyShortcutBar,
  TMoneyCalendar,
  TMoneyPanel,
  TMoneyImageButton]);
end;

end.
