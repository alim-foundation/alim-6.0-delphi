unit ShopCart;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Db, DBISAMTb, DBCtrls, ExtCtrls, Grids, DBGrids, RXDBCtrl,
  ComCtrls, MessageWin, Tabs, AmlView, Menus;

type
  TShoppingCart = class(TForm)
    dsCart: TDataSource;
    tblCart: TDBISAMTable;
    StatusBar: TStatusBar;
    pnlTools: TPanel;
    nvCart: TDBNavigator;
    btnSendOrder: TButton;
    TabSet: TTabSet;
    grdCart: TRxDBGrid;
    btnSaveOrder: TButton;
    PopupMenu1: TPopupMenu;
    MoveItemtoPreviousOrders1: TMenuItem;
    MoveItemtoCurrentCart1: TMenuItem;
    procedure TabSetClick(Sender: TObject);
    procedure btnSendOrderClick(Sender: TObject);
    procedure btnSaveOrderClick(Sender: TObject);
    procedure MoveItemtoCurrentCart1Click(Sender: TObject);
    procedure MoveItemtoPreviousOrders1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure grdCartDblClick(Sender: TObject);
  protected
    FAddrMgr : IAddressManager;
    FTotalProds : Cardinal;
    FTotalQty : Cardinal;
    FTotalVendors : Cardinal;
    FTotalCost : Real;
    procedure SetFilter(const ACurrentCart : Boolean);
    procedure CalcStats;
  public
    function GetProductsPostData : String;
    procedure MarkProductsSent;
    procedure AddItem(const AData : String);
    procedure SetPrimaryPath(const APath : String);
    property AddressMgr : IAddressManager read FAddrMgr write FAddrMgr;
  end;

var
  ShoppingCart: TShoppingCart;

implementation

{$R *.DFM}

uses StStrL;

const
  CRLF = #13#10;

resourcestring
  rsOrderMessage =
  'Are you sure you wish to purchase %d (total Quantity %d) products from %d vendors worth $%.2f?'+CRLF+CRLF+
  '1. Make sure you''re connected to the Internet.'+CRLF+
  '2. Once your order is complete, click on the "Save" button.';

procedure TShoppingCart.SetPrimaryPath(const APath : String);
begin
  tblCart.DatabaseName := APath;
  tblCart.Active := True;
  SetFilter(True);
end;

procedure TShoppingCart.AddItem(const AData : String);
var
  I, P, PCount : Word;
  ParamData : String;
  ParamKey : String;
  ParamValue : String;
  KeysValues : TStringList;
begin
  Show;

  KeysValues := TStringList.Create;
  KeysValues.Sorted := False;

  PCount := WordCountL(AData, '|');
  for I := 1 to PCount do begin
    ParamData := ExtractWordL(I, AData, '|');
    P := Pos('=', ParamData);
    if P > 0 then begin
      ParamKey := Copy(ParamData, 1, P-1);
      ParamValue := Copy(ParamData, P+1, Length(ParamData));
      KeysValues.Values[Lowercase(ParamKey)] := ParamValue;
    end;
  end;

  if tblCart.FindKey([KeysValues.Values['item_id']]) then begin
    tblCart.Edit;
    tblCart.FieldValues['Quantity'] := Succ(StrToInt(tblCart.FieldValues['Quantity']));
    tblCart.Post;
  end else begin
    try
      tblCart.Insert;
      for I := 0 to KeysValues.Count-1 do
        tblCart.FieldValues[KeysValues.Names[I]] := KeysValues.Values[KeysValues.Names[I]];
      tblCart.Post;
    except
      on E : Exception do begin
        MessageWindow.Add('SHOPCART', 'Unable to add to shopping cart: invalid parameter %s in "%s" (%s)', [ParamKey, AData, E.Message]);
        ShowMessage('Unable to add item to shopping cart (invalid data provided)');
      end;
    end;
  end;

  KeysValues.Free;
  CalcStats;
end;

procedure TShoppingCart.SetFilter(const ACurrentCart : Boolean);
begin
  if ACurrentCart then begin
    tblCart.Filtered := False;
    tblCart.Filter := 'Order_Placed = NULL';
    tblCart.Filtered := True;
    if TabSet.TabIndex <> 0 then
      TabSet.TabIndex := 0;
  end else begin
    tblCart.Filtered := False;
    tblCart.Filter := 'Order_Placed <> NULL';
    tblCart.Filtered := True;
    if TabSet.TabIndex <> 1 then
      TabSet.TabIndex := 1;
  end;
  CalcStats;
end;

procedure TShoppingCart.CalcStats;
var
  DistinctVendors : TStringList;
begin
  FTotalProds := 0;
  FTotalQty := 0;
  FTotalCost := 0.0;

  DistinctVendors := TStringList.Create;
  DistinctVendors.Sorted := True;
  DistinctVendors.Duplicates := dupIgnore;

  with tblCart do begin
    First;
    while not EOF do begin
      try
        Inc(FTotalProds);
        FTotalQty := FTotalQty + StrToInt(FieldValues['Quantity']);
        DistinctVendors.Add(FieldValues['Vendor']);
        FTotalCost := FTotalCost + (StrToFloat(FieldValues['Unit_Cost']) * StrToInt(FieldValues['Quantity']));
        Next;
      except
      end;
    end;
  end;

  FTotalVendors := DistinctVendors.Count;
  StatusBar.Panels[0].Text := Format('Products: %d (%d)', [FTotalProds, FTotalQty]);
  StatusBar.Panels[1].Text := Format('Vendors: %d', [FTotalVendors]);
  StatusBar.Panels[2].Text := Format('Total Cost: $%f', [FTotalCost]);

  DistinctVendors.Free;
  btnSendOrder.Enabled := FTotalProds > 0;
  btnSaveOrder.Enabled := FTotalProds > 0;
end;

function TShoppingCart.GetProductsPostData : String;
var
  SCItem, F : Integer;

  procedure addKeyValuePair(const AKey, AValue : String);
  var
    I : Integer;
    Ch : Char;
  begin
    if Result <> '' then
      Result := Result + '&';
    Result := Result + AKey + '=';
    for I := 1 to Length(AValue) do begin
      Ch := AValue[I];
      if Ch in ['A'..'Z', 'a'..'z', '0'..'9'] then
        Result := Result + Ch
      else
        Result := Result + '%' + IntToHex(Ord(Ch), 2);
    end;
  end;

begin
  Result := '';
  SCItem := 0;

  with tblCart do begin
    try
      First;
      while not EOF do begin
        for F := 0 to FieldList.Count-1 do
          addKeyValuePair(FieldList[F].FieldName + '_' + IntToStr(SCItem), FieldList[F].AsString);
        Next;
        Inc(SCItem);
      end;
    except
      on E : Exception do
        ShowMessage(Format('Error retreiving shopping cart items: %s', [E.Message]));
    end;
  end;

  addKeyValuePair('itemscount', IntToStr(SCItem));
end;

procedure TShoppingCart.MarkProductsSent;
begin
  with tblCart do begin
    First;
    while not EOF do begin
      Edit;
      with Fields.FieldByName('Order_Placed') as TDateTimeField do
        AsDateTime := Now;
      Post;
      Next;
    end;
  end;
  CalcStats;
end;

procedure TShoppingCart.TabSetClick(Sender: TObject);
begin
  SetFilter(TabSet.TabIndex = 0);
end;

procedure TShoppingCart.btnSendOrderClick(Sender: TObject);
begin
  if MessageDlg(Format(rsOrderMessage, [FTotalProds, FTotalQty, FTotalVendors, FTotalCost]), mtConfirmation, [mbYes, mbNo], 0) = mrNo then
    Exit;

  FAddrMgr.SetAddress('cartsend:');
end;

procedure TShoppingCart.btnSaveOrderClick(Sender: TObject);
begin
  MarkProductsSent;
end;

procedure TShoppingCart.MoveItemtoCurrentCart1Click(Sender: TObject);
begin
  try
    tblCart.Edit;
    with tblCart.Fields.FieldByName('Order_Placed') as TDateTimeField do
      AsVariant := NULL;
    tblCart.Post;
  except
    tblCart.Cancel;
  end;
  CalcStats;
end;

procedure TShoppingCart.MoveItemtoPreviousOrders1Click(Sender: TObject);
begin
  try
    tblCart.Edit;
    with tblCart.Fields.FieldByName('Order_Placed') as TDateTimeField do
      AsDateTime := Now;
    tblCart.Post;
  except
    tblCart.Cancel;
  end;
  CalcStats;
end;

procedure TShoppingCart.FormActivate(Sender: TObject);
begin
  grdCart.SetFocus;
end;

procedure TShoppingCart.grdCartDblClick(Sender: TObject);
var
  Address : String;
begin
  try
    if (tblCart.FieldValues['Item_Address'] <> NULL) then begin
      Address := tblCart.FieldValues['Item_Address'];
      if Address <> '' then
        FAddrMgr.SetAddress(Address);
    end;
  except
  end;
end;

end.
