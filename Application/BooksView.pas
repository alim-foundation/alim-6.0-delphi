unit BooksView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  IslUtils, Htmlview, ExtCtrls, ComCtrls, AmlView;

type
  TInstalledBooksForm = class(TForm)
    BooksTree: TTreeView;
    Splitter1: TSplitter;
    InfoViewer: THTMLViewer;
    procedure BooksTreeClick(Sender: TObject);
  public
    procedure Populate(const APaths : String; const ByName, ById, ByType : TObjDict);
  end;

var
  InstalledBooksForm: TInstalledBooksForm;

implementation

{$R *.DFM}

procedure TInstalledBooksForm.Populate(const APaths : String; const ByName, ById, ByType : TObjDict);

  procedure FillTree(const ARoot : TTreeNode; AList : TObjDict);
  var
    I : Integer;
  begin
    if AList.Count <= 0 then
      Exit;

    for I := 0 to AList.Count-1 do
      BooksTree.Items.AddChildObject(ARoot, AList[I], AList.Objects[I]);
  end;

var
  ByTypeItem : TTreeNode;
  I : Integer;
begin
  BooksTree.Items.Clear;

  Assert(ByName <> Nil);
  Assert(ById <> Nil);
  Assert(ByType <> Nil);

  FillTree(BooksTree.Items.AddChild(Nil, 'Books By FileName'), ByName);
  FillTree(BooksTree.Items.AddChild(Nil, 'Books By Id'), ById);

  ByTypeItem := BooksTree.Items.AddChild(Nil, 'Books By Type');
  if ByType.Count > 0 then begin
    for I := 0 to ByType.Count-1 do
      FillTree(BooksTree.Items.AddChild(ByTypeItem, ByType[I]), ByType.Objects[I] as TObjDict);
  end;
end;

procedure TInstalledBooksForm.BooksTreeClick(Sender: TObject);
const
  PropertyRowFmt = '<tr valign=top bgcolor="%2:s"><td align="right">%0:s</td><td><font color="navy"><b>%1:s</b></font></td></tr>';
var
  AmlData : TAmlData;
  Html : String;

  procedure ShowProperty(const APropertyName, APropertyValue : String);
  begin
    Html := Html + Format(PropertyRowFmt, [APropertyName, APropertyValue, 'white']);
  end;

begin
  AmlData := TAmlData(BooksTree.Selected.Data);
  if AmlData = Nil then
    Exit;

  Html := '';
  ShowProperty('Id', AmlData.Id);
  ShowProperty('ContentType', AmlData.ContentType);
  ShowProperty('Name', AmlData.Name);
  ShowProperty('ShortName', AmlData.ShortName);

  Html := '<table cellspacing=0 cellpadding=3>'+Html+'</table>';
  InfoViewer.LoadFromBuffer(PChar(Html), Length(Html));
end;

end.
