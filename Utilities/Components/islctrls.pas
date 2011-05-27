unit islctrls;

{$I RX.INC}
{$S-,W-,R-}

interface

uses {$IFDEF WIN32} Windows, Registry, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  SysUtils, Classes, Messages, Menus, Buttons, Controls, Graphics, Forms,
  StdCtrls, ExtCtrls, RXCtrls, sEdits;

type
  TWallpaperPanel = class(TCustomPanel)
  protected
    FWallpaper: TPicture;
    procedure Paint; override;
    procedure SetWallpaper(Value: TPicture);
    procedure WallpaperChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
  published
    property Wallpaper: TPicture read FWallpaper write SetWallpaper;
    property Alignment;
    property Align;
    property Caption;
    property Font;
    property ParentFont;
{$IFDEF RX_D4}
    property BiDiMode;
    property Constraints;
    property ParentBiDiMode;
{$ENDIF}
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Locked;
    property ParentColor;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
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
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
{$IFDEF WIN32}
    property OnStartDrag;
{$ENDIF}
{$IFDEF RX_D4}
    property OnEndDock;
    property OnStartDock;
{$ENDIF}
    property OnResize;
  end;

  TVolElemEdit = class(TsEdit)
  protected
    FVolDelim : Char;
    FOnEnterKey : TNotifyEvent;
    function GetElement : String;
    procedure SetElement(AValue : String);
    function GetVolume : String;
    procedure SetVolume(AValue : String);
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetVolElem(AVolume, AElement : String);
  published
    property Anchors;
    property Font;
    property Flat;
    property Element : String read GetElement write SetElement;
    property Volume : String read GetVolume write SetVolume;
    property VolumeDelim : Char read FVolDelim write FVolDelim default '.';
    property OnEnterKey : TNotifyEvent read FOnEnterKey write FOnEnterKey;
  end;

const
  DefaultPalette : HPalette = 0;

procedure Register;

implementation

uses StStrS;

constructor TWallpaperPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWallpaper := TPicture.Create;
  FWallpaper.OnChange := WallpaperChanged;
end;

destructor TWallpaperPanel.Destroy;
begin
  FWallpaper.OnChange := nil;
  FWallpaper.Free;
  FWallpaper := nil;
  inherited Destroy;
end;

procedure TWallpaperPanel.Paint;
const
  Alignments: array[TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  XCnt, YCnt, X, Y, FontHeight : Integer;
  BevelSize, SaveIndex: Integer;
  Rect: TRect;
  DC: HDC;
  OldPal: HPalette;
  Flags: Longint;
begin
  Rect := ClientRect;
  BevelSize := BorderWidth;
  if BevelOuter <> bvNone then Inc(BevelSize, BevelWidth);
  if BevelInner <> bvNone then Inc(BevelSize, BevelWidth);
  InflateRect(Rect, -BevelSize, -BevelSize);
  if (FWallpaper.Graphic <> nil) and (FWallpaper.Width > 0) and
    (FWallpaper.Height > 0) then
  begin
    inherited Paint;
    SaveIndex := SaveDC(Canvas.Handle);
    try
      with Rect do
        IntersectClipRect(Canvas.Handle, Left, Top, Right - Left +
          BevelSize, Bottom - Top + BevelSize);
      XCnt := (ClientWidth - 2 * BevelSize) div FWallpaper.Width;
      YCnt := (ClientHeight - 2 * BevelSize) div FWallpaper.Height;
      for X := 0 to XCnt do
        for Y := 0 to YCnt do
          Canvas.Draw(Rect.Left + X * FWallpaper.Width,
            Rect.Top + Y * FWallpaper.Height, FWallpaper.Graphic);
    finally
      RestoreDC(Canvas.Handle, SaveIndex);
    end;
  end else if DefaultPalette <> 0 then begin
    with Canvas do begin
      DC := Handle;
      OldPal := SelectPalette(DC, DefaultPalette, False);
      RealizePalette(DC);
      Brush.Color := Self.Color or $2000000;
      FillRect(ClientRect);
      Brush.Style := bsClear;
      Font := Self.Font;
      FontHeight := TextHeight('W');
      with Rect do begin
        Top := ((Bottom + Top) - FontHeight) div 2;
        Bottom := Top + FontHeight;
      end;
      Flags := DT_EXPANDTABS or DT_VCENTER or Alignments[Alignment];
      Flags := DrawTextBiDiModeFlags(Flags);
      DrawText(Handle, PChar(Caption), -1, Rect, Flags);
      SelectPalette(DC, OldPal, False);
    end;
  end else
    inherited Paint;
end;

procedure TWallpaperPanel.SetWallpaper(Value: TPicture);
begin
  FWallpaper.Assign(Value);
end;

procedure TWallpaperPanel.WallpaperChanged(Sender: TObject);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

constructor TVolElemEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CharCase := ecUpperCase;
end;

function TVolElemEdit.GetElement : String;
var
  Elem : String;
  VolDelimPos : Integer;
begin
  Elem := Text;
  VolDelimPos := Pos(FVolDelim, Elem);
  if VolDelimPos > 0 then
    Result := Copy(Elem, VolDelimPos+1, 255)
  else
    Result := Elem;
  Result := TrimS(Result);
end;

procedure TVolElemEdit.SetElement(AValue : String);
var
  Vol : String;
begin
  if FVolDelim <> #0 then begin
    Vol := Volume;
    Text := ' '+Vol + FVolDelim + AValue;
  end else
    Text := ' '+AValue;
end;

function TVolElemEdit.GetVolume : String;
var
  Elem : String;
  VolDelimPos : Integer;
begin
  Elem := Text;
  VolDelimPos := Pos(FVolDelim, Text);
  if VolDelimPos > 0 then
    Result := TrimS(Copy(Elem, 1, VolDelimPos-1))
  else
    Result := '';
end;

procedure TVolElemEdit.SetVolume(AValue : String);
var
  Elem : String;
begin
  if FVolDelim <> #0 then begin
    Elem := Element;
    Text := ' '+AValue + FVolDelim + Elem;
  end;
end;

procedure TVolElemEdit.SetVolElem(AVolume, AElement : String);
begin
  Text := ' '+AVolume + FVolDelim + AElement;
end;

procedure TVolElemEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Assigned(FOnEnterKey) and (Key = Vk_Return) then begin
    FOnEnterKey(Self);
    Key := 0;
  end;
end;

procedure Register;
begin
  RegisterComponents('ISL', [TWallpaperPanel, TVolElemEdit]);
end;

end.
