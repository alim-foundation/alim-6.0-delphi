unit BookmarksView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  AmlView, ComCtrls;

type
  TBookmarksViewer = class(TForm)
    BookmarksTree: TTreeView;
    procedure BookmarksTreeDblClick(Sender: TObject);
    procedure BookmarksTreeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  protected
    FBookmarks : TBookmarks;
    FAddrMgr : IAddressManager;
    FStatus : IStatusDisplay;
    FOwnTheData : Boolean;

    procedure SetBookmarks(const ABookmarks : TBookmarks); virtual;
  public
    destructor Destroy; override;
    procedure UpdateContents; virtual;

    property AddressMgr : IAddressManager read FAddrMgr write FAddrMgr;
    property Bookmarks : TBookmarks read FBookmarks write SetBookmarks;
    property OwnTheData : Boolean read FOwnTheData write FOwnTheData;
    property Status : IStatusDisplay read FStatus write FStatus;
  end;

implementation

uses Main;

{$R *.DFM}

destructor TBookmarksViewer.Destroy;
begin
  if FOwnTheData and (FBookmarks <> Nil) then
    FBookmarks.Free;
  inherited Destroy;
end;

procedure TBookmarksViewer.SetBookmarks(const ABookmarks : TBookmarks);
begin
  FBookmarks := ABookmarks;
  UpdateContents;
end;

procedure TBookmarksViewer.UpdateContents;

  procedure AddNodes(const AParent : TTreeNode; const ANodes : TBookmarks); forward;

  procedure AddNode(const AParent : TTreeNode; const ANode : TBookmarkNode);
  var
    NewNode : TTreeNode;
  begin
    Assert(ANode <> Nil);
    NewNode := BookmarksTree.Items.AddChild(AParent, ANode.Caption);
    NewNode.Data := ANode;

    if ANode.SmallIcon <> Nil then
      NewNode.ImageIndex := MainForm.ImagesSmall.AddMasked(ANode.SmallIcon, clWhite)
    else if ANode.SmallIconIdx >= 0 then
      NewNode.ImageIndex := ANode.SmallIconIdx
    else begin
      if ANode.Children.Count > 0 then
        NewNode.ImageIndex := DefaultBmkCatgIdxSmall
      else
        NewNode.ImageIndex := DefaultBmkItemIdxSmall;
    end;
    NewNode.SelectedIndex := NewNode.ImageIndex;

    AddNodes(NewNode, ANode.Children);
  end;

  procedure AddNodes(const AParent : TTreeNode; const ANodes : TBookmarks);
  var
    C, I : Cardinal;
  begin
    C := ANodes.Count;
    if C > 0 then begin
      Dec(C);
      for I := 0 to C do
        AddNode(AParent, ANodes.Bookmarks[I]);
    end;
  end;

begin
  BookmarksTree.Items.BeginUpdate;
  BookmarksTree.Items.Clear;
  if FBookmarks <> Nil then
    AddNodes(Nil, FBookmarks);
  BookmarksTree.Items.EndUpdate;
end;

procedure TBookmarksViewer.BookmarksTreeDblClick(Sender: TObject);
var
  Node : TBookmarkNode;
begin
  if BookmarksTree.Selected <> Nil then begin
    Node := TBookmarkNode(BookmarksTree.Selected.Data);
    if (Node <> Nil) and (Node.Address <> '') then
      FAddrMgr.SetAddress(Node.Address);
  end;
end;

procedure TBookmarksViewer.BookmarksTreeKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (BookmarksTree.Selected <> Nil) then begin
    if BookmarksTree.Selected.Count > 0 then
      BookmarksTree.Selected.Expanded := not BookmarksTree.Selected.Expanded
    else
      BookmarksTreeDblClick(Sender);
  end;
end;

end.
