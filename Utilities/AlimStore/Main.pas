unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, FileCtrl, StdCtrls, ExtCtrls, ComCtrls,
  XmlObjModel, XmlParser, CompDoc, QuranStruct, Builders, Mask,
  ToolEdit, IslUtils, RXCtrls;

type
  TFileNameData =
    record
      FullPathAndName : String;
      RelativePath : String;
      RelativePathAndName : String;
      JustPath : String;
      JustNameAndExtn : String;
      JustNameNoExtn : String;
      JustExtn : String;
      SingleStoreNameAndExtn : String;
    end;

  TProcessedFileData =
    record
      FileName : TFileNameData;
      Details : TSearchRec;
      BuilderName : String;
      ErrorCount : Cardinal;
      Errors : String;
      RootTag : String;
      RootTagElems : Cardinal;
    end;

  TFileHdlMethod = procedure(const AFile : TFileNameData) of object;
  TFileHandler =
    record
      Extension : String;
      Handler : TFileHdlMethod;
    end;

  TBuildAction = (baParse, baStore);
  TBuildActions = set of TBuildAction;

  TBuildLogSection = (blsProcessedFiles, blsNoRegBuilder, blsStoreOnly, blsIgnore);
  TBuildLogger = class(TObject)
  protected
    FLog : TTreeView;
    FSectionNodes : array[TBuildLogSection] of TTreeNode;
    FRootDir : String;
  public
    function AddToSection(const ASection : TBuildLogSection; const AMsg : String) : TTreeNode;
    function AddToNode(const ANode : TTreeNode; const AMsg : String) : TTreeNode;
    property Log : TTreeView read FLog write FLog;
    property RootDir : String read FRootDir write FRootDir;
  end;

  TMainForm = class(TForm)
    amlData: TXmlObjModel;
    gbxLog: TGroupBox;
    BuildLogTree: TTreeView;
    gbxProcess: TGroupBox;
    SrcDirEdit: TDirectoryEdit;
    DestDirEdit: TDirectoryEdit;
    StatusBar: TStatusBar;
    rgpStorageType: TRadioGroup;
    btnCancelProcessing: TButton;
    btnBeginProcessing: TButton;
    Label1: TLabel;
    Label2: TLabel;
    SaveDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    BuildSingleBtn: TRxSpeedButton;
    btnSaveLog: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnBeginProcessingClick(Sender: TObject);
    procedure btnCancelProcessingClick(Sender: TObject);
    procedure BuildSingleBtnClick(Sender: TObject);
    procedure btnSaveLogClick(Sender: TObject);
  private
    FRegisteredFileHandlers : array of TFileHandler;
    FRegisteredBuilders : array of TAlimStorageBuilder;
    FRootStorage : TRootStorage;
    FSubdirList : TStringList;
    FStorageStack : TList;
    FBuildActions : TBuildActions;
    FLogger : TBuildLogger;
    FCancelProcessing : Boolean;
    FConsolidateData : Boolean;
    FCurDestFullPath : String;
    FSingleFilesStored : TStringList;
    FQuranStruct : TQuranStructure;

    function GetFileData(const AFullPathName : String) : TFileNameData;
    procedure Process;
    procedure PackageDir(const APath : String);
    procedure ProcessDir(const APath : String);
    procedure ProcessAMLFile(const AFile : TFileNameData);
    procedure StoreBinaryFile(const AFile : TFileNameData);
    procedure IgnoreFile(const AFile : TFileNameData);
    function OnStorageStatus(const AMode : TStatusEventMode; const AMsg : String; const AProgress, ATotal : LongInt) : Boolean;
  public
    { Public declarations }
    property BuildActions : TBuildActions read FBuildActions write FBuildActions;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

uses StStrS;

const
  SingleStoreFileExtn = 'adf';
  ConsolidatedDataFileName = 'AlimData.adf';

procedure RemoveEmptyDirs(const APath : String);
var
  FRes, DRes : Integer;
  FRec, DRec : TSearchRec;
  SubDirPath : String;
begin
  DRes := FindFirst(APath + '\*.*', (faReadOnly+faArchive+faDirectory), DRec);
  if DRes = 0 then begin
    while (DRes = 0) do begin
      if (DRec.Attr and faDirectory = faDirectory) then begin
        if (DRec.Name <> '.') and (DRec.Name <> '..') then begin
          SubDirPath := AddBackSlashS(APath) + DRec.Name;
          FRes := FindFirst(AddBackSlashS(APath) + DRec.Name + '\*.*', faAnyFile+faDirectory, FRec);
          if FRes <> 0 then begin
            RmDir(SubDirPath);
          end else begin
            RemoveEmptyDirs(SubDirPath);
            FindClose(FRec);
          end;
        end;
      end;
      DRes := FindNext(DRec);
    end;
    FindClose(DRec);
  end;
end;

function TBuildLogger.AddToSection(const ASection : TBuildLogSection; const AMsg : String) : TTreeNode;
begin
  if FSectionNodes[ASection] = Nil then begin
    case ASection of
      blsProcessedFiles : FSectionNodes[ASection] := FLog.Items.Add(Nil, 'Files Processed');
      blsNoRegBuilder : FSectionNodes[ASection] := FLog.Items.Add(Nil, 'Files for which no builders have been registered');
      blsStoreOnly : FSectionNodes[ASection] := FLog.Items.Add(Nil, 'Files that are stored only (binary)');
      blsIgnore : FSectionNodes[ASection] := FLog.Items.Add(Nil, 'Files that are ignored');
    end;
  end;
  Result := AddToNode(FSectionNodes[ASection], AMsg);
end;

function TBuildLogger.AddToNode(const ANode : TTreeNode; const AMsg : String) : TTreeNode;
begin
  Result := FLog.Items.AddChild(ANode, AMsg);
  if (ANode <> FSectionNodes[blsNoRegBuilder]) and
     (ANode <> FSectionNodes[blsStoreOnly]) and
     (ANode <> FSectionNodes[blsIgnore]) then begin
    ANode.Expanded := True;
    FLog.TopItem := Result;
  end;
  Application.ProcessMessages;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  FHdlIdx, FBuilderIdx : Cardinal;
  QStructFile : String;

  procedure AddExtnHandler(const AExtn : String; const AHandler : TFileHdlMethod);
  begin
    SetLength(FRegisteredFileHandlers, FHdlIdx+1);
    with FRegisteredFileHandlers[FHdlIdx] do begin
      Extension := AExtn;
      Handler := AHandler;
    end;
    Inc(FHdlIdx);
  end;

  procedure AddBuilder(Builder : TAlimStorageBuilder);
  begin
    SetLength(FRegisteredBuilders, FBuilderIdx+1);
    FRegisteredBuilders[FBuilderIdx] := Builder;
    Builder.OnStatus := OnStorageStatus;
    Builder.CreateSubStorage := True;
    Inc(FBuilderIdx);
  end;

begin
  Caption := Caption + ' ' + GetAppVersionInfo;
  amlData.NormalizeData := False;
  FBuildActions := [baParse, baStore];

  QStructFile := AddBackSlashS(JustPathNameS(Application.ExeName)) + 'sndtrack\Quran Structure.aml';
  FQuranStruct := TQuranStructure.Create;
  try
    amlData.LoadDataSource(QStructFile);
    FQuranStruct.LoadFromAML(amlData.Document.DocumentElement);
  except
    ShowMessage('Very serious error in Quran Structure file: '+QStructFile);
    Application.Terminate;
  end;

  FLogger := TBuildLogger.Create;
  FLogger.Log := BuildLogTree;

  FHdlIdx := 0;
  AddExtnHandler('aml', ProcessAMLFile);
  AddExtnHandler('jpg', StoreBinaryFile);
  AddExtnHandler('jpeg', StoreBinaryFile);
  AddExtnHandler('gif', StoreBinaryFile);
  AddExtnHandler('bmp', StoreBinaryFile);

  FBuilderIdx := 0;
  AddBuilder(TQuranStorageBuilder.Create);
  AddBuilder(TIndexStorageBuilder.Create);
  AddBuilder(THadithFiqhStorageBuilder.Create);
  AddBuilder(TArticlesStorageBuilder.Create);

  SrcDirEdit.Text := JustPathNameS(Application.ExeName) + '\..\AML Data';
  DestDirEdit.Text := JustPathNameS(Application.ExeName) + '\..\CD Image\Books';

  OpenDialog.InitialDir := JustPathNameS(Application.ExeName) + '\..\AML Data';
  SaveDialog.InitialDir := JustPathNameS(Application.ExeName) + '\..\Install Image\Books';
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  B : Cardinal;
begin
  for B := Low(FRegisteredBuilders) to High(FRegisteredBuilders) do
    FRegisteredBuilders[B].Free;
  FLogger.Free;
end;

procedure TMainForm.Process;
begin
  FLogger.RootDir := SrcDirEdit.Text;
  case rgpStorageType.ItemIndex of
    0 : FConsolidateData := True;
    1 : FConsolidateData := False;
  end;

  FCancelProcessing := False;
  btnBeginProcessing.Visible := False;
  btnCancelProcessing.Visible := True;

  StatusBar.Panels[0].Text := 'Processing ' + FLogger.RootDir;
  Update;

  if FConsolidateData then begin
    FRootStorage := TRootStorage.Create(AddBackSlashS(DestDirEdit.Text) + ConsolidatedDataFileName, amReadWrite, smExclusive, tmDirect, True);
    FStorageStack := TList.Create;
    FStorageStack.Add(FRootStorage);
  end else
    FSingleFilesStored := TStringList.Create;
  FSubdirList := TStringList.Create;

  ProcessDir(FLogger.RootDir);

  FSubdirList.Free;
  StatusBar.Panels[0].Text := 'Storing Globals ';
  Update;

  if FConsolidateData then begin
    FRootStorage.Free;
    FStorageStack.Free;
  end else begin
    FSingleFilesStored.Free;
    RemoveEmptyDirs(DestDirEdit.Text);
  end;

  StatusBar.Panels[0].Text := 'Completed Processing.';
  btnBeginProcessing.Visible := True;
  btnCancelProcessing.Visible := False;
end;

function TMainForm.GetFileData(const AFullPathName : String) : TFileNameData;
begin
  with Result do begin
    FullPathAndName := AFullPathName;
    RelativePathAndName := FullPathAndName;
    if Copy(RelativePathAndName, 1, Length(FLogger.RootDir)) = FLogger.RootDir then
      RelativePathAndName := Copy(FullPathAndName, Length(FLogger.RootDir)+1, Length(FullPathAndName));
    if RelativePathAndName[1] = '\' then
      Delete(RelativePathAndName, 1, 1);
    RelativePath := JustPathNameS(RelativePathAndName);
    JustPath := JustPathNameS(FullPathAndName);
    JustNameAndExtn := JustFileNameS(FullPathAndName);
    JustExtn := JustExtensionS(FullPathAndName);
    JustNameNoExtn := ForceExtensionS(JustNameAndExtn, '');
    if JustNameNoExtn[Length(JustNameNoExtn)] = '.' then
      Delete(JustNameNoExtn, Length(JustNameNoExtn), 1);
    SingleStoreNameAndExtn := ForceExtensionS(JustNameAndExtn, SingleStoreFileExtn);
  end;
end;

procedure TMainForm.PackageDir(const APath : String);
begin
end;

procedure TMainForm.ProcessDir(const APath : String);
var
  FileHdlIdx, HandledBy : Cardinal;
  FRes, DRes : Integer;
  FRec, DRec : TSearchRec;
  SubStorage : TStorage;
  FileData : TFileNameData;
begin
  { first process all of the files }
  FRes := FindFirst(APath + '\*.*', faReadOnly+faArchive, FRec);
  if FRes = 0 then begin
    while (FRes = 0) and not FCancelProcessing do begin
      HandledBy := 0;
      FileData := GetFileData(APath + '\' + FRec.Name);
      for FileHdlIdx := Low(FRegisteredFileHandlers) to High(FRegisteredFileHandlers) do begin
        try
          with FRegisteredFileHandlers[FileHdlIdx] do begin
            if CompareText(Extension, FileData.JustExtn) = 0 then begin
              Inc(HandledBy);
              Handler(FileData);
            end;
          end;
        except
          on E : Exception do
            FLogger.AddToNode(Nil, Format('Problem in ProcessDir (processing %s, Handler %d) [%s]', [APath, FileHdlIdx, E.Message]));
        end;
      end;
      if HandledBy <= 0 then
        IgnoreFile(FileData);
      FRes := FindNext(FRec);
      Application.ProcessMessages;
    end;
    FindClose(FRec);
  end;

  { now process the subdirectories }
  DRes := FindFirst(APath + '\*.*', (faReadOnly+faArchive+faDirectory), DRec);
  if DRes = 0 then begin
    while (DRes = 0) and not FCancelProcessing do begin
      if (DRec.Attr and faDirectory = faDirectory) then begin
        { only process directories that don't start with __, ., and .. }
        if (Copy(DRec.Name, 1, 2) <> '__') and (DRec.Name <> '.') and (DRec.Name <> '..') then begin
          FSubdirList.Add(DRec.Name);

          if FConsolidateData then begin
            SubStorage := TStorage.Create(DRec.Name, FStorageStack.Items[0], amReadWrite, tmDirect, True);
            FStorageStack.Insert(0, SubStorage);
          end else begin
            FCurDestFullPath := AddBackSlashS(DestDirEdit.Text) + Join('\', FSubdirList);
            if not FileExists(FCurDestFullPath) then begin
              try
                MkDir(FCurDestFullPath);
              except
              end;
            end;
          end;

          if JustExtensionS(DRec.Name) = 'pkg' then
            PackageDir(AddBackSlashS(APath) + DRec.Name)
          else
            ProcessDir(AddBackSlashS(APath) + DRec.Name);

          if FConsolidateData then begin
            SubStorage.Free;
            FStorageStack.Delete(0);
          end;
          FSubdirList.Delete(FSubdirList.Count-1);
        end;
      end;
      DRes := FindNext(DRec);
      Application.ProcessMessages;
    end;
    FindClose(DRec);
  end;
end;

procedure TMainForm.ProcessAMLFile(const AFile : TFileNameData);
var
  I, E, Handlers : Cardinal;
  FileNode : TTreeNode;
  FileData : TProcessedFileData;
  Builder : TAlimStorageBuilder;
  DataStore : TStorage;
begin
  FileNode := FLogger.AddToSection(blsProcessedFiles, AFile.RelativePathAndName);
  FillChar(FileData, SizeOf(FileData), 0);
  FileData.FileName := AFile;
  FileData.BuilderName := 'Unknown';

  if baParse in FBuildActions then begin
    OnStorageStatus(setStart, 'Parsing '+AFile.JustNameAndExtn+'...', 0, 0);
    try
      amlData.LoadDataSource(AFile.FullPathAndName);
    except
      on E : TXMLParserError do begin
        FLogger.AddToNode(FileNode, E.Message);
        OnStorageStatus(setEnd, '', 0, 0);
        Exit;
      end;
    end;
    OnStorageStatus(setEnd, '', 0, 0);
  end;

  Handlers := 0;
  for I := Low(FRegisteredBuilders) to High(FRegisteredBuilders) do begin
    Builder := FRegisteredBuilders[I];
    if Builder.CanHandleDocument(amlData.Document) then begin
      if (baParse in FBuildActions) and (baStore in FBuildActions) then begin
        if not Builder.ScanDataOnly then begin
          if FConsolidateData then
            DataStore := TStorage(FStorageStack.Items[0])
          else begin
            DataStore := TRootStorage.Create(AddBackSlashS(FCurDestFullPath) + AFile.SingleStoreNameAndExtn, amReadWrite, smExclusive, tmDirect, True);
            FSingleFilesStored.Values[AFile.SingleStoreNameAndExtn] := AddBackSlashS(FCurDestFullPath) + AFile.SingleStoreNameAndExtn;
          end;
        end;
        Builder.CreateSubStorage := FConsolidateData;
        Builder.QuranStruct := FQuranStruct;
        Builder.StoreDocument(DataStore, amlData.Document);

        if (not FConsolidateData) and (not Builder.ScanDataOnly) then
          DataStore.Free;
        if FCancelProcessing then begin
          FLogger.AddToNode(Nil, 'Processing canceled during storage of '+AFile.JustNameAndExtn);
          break;
        end;
      end;

      FileData.BuilderName := Builder.ClassName;
      FileData.ErrorCount := Builder.Errors.Count;
      FileData.Errors := Builder.Errors.Text;
      if Builder.RootTag <> Nil then begin
        FileData.RootTag := Builder.RootTag.NodeName;
        FileData.RootTagElems := Builder.RootTag.ChildNodes.Length;
      end;

      FLogger.AddToNode(FileNode, Format('Root tag is ''%s'' with %d elements', [FileData.RootTag, FileData.RootTagElems]));
      if Builder.BytesStored > 0 then
        FLogger.AddToNode(FileNode, Format('Stored %s bytes using %s', [CommaizeS(Builder.BytesStored), Builder.ClassName]))
      else
        Builder.Errors.Add(Format('Builder %s found, but no data was stored', [Builder.ClassName]));
      if Builder.SubjectsIndex.StoreSize > 0 then
        FLogger.AddToNode(FileNode, Format('Stored %s bytes in Subjects Index', [CommaizeS(Builder.SubjectsIndex.StoreSize)]));
      if Builder.WordsIndex.StoreSize > 0 then
        FLogger.AddToNode(FileNode, Format('Stored %s bytes in Words Index', [CommaizeS(Builder.WordsIndex.StoreSize)]));

      if Builder.Errors.Count > 0 then begin
        StatusBar.Panels[0].Text := 'Adding errors...';
        BuildLogTree.Items.BeginUpdate;
        for E := 0 to Builder.Errors.Count-1 do
          FLogger.AddToNode(FileNode, Builder.Errors[E]);
        BuildLogTree.Items.EndUpdate;
      end else
        FileNode.Expanded := False;
      Inc(Handlers);
    end else
      { CanHandleDocument may register some errors, so show those }
      if Builder.Errors.Count > 0 then begin
        for E := 0 to Builder.Errors.Count-1 do
          FLogger.AddToNode(FileNode, Builder.Errors[E]);
      end;
  end;
  if (Handlers <= 0) and (not FCancelProcessing) then begin
    FLogger.AddToNode(FileNode, 'No builder registered');
    FLogger.AddToSection(blsNoRegBuilder, AFile.RelativePathAndName);
  end;
  amlData.ClearDocument;

  Application.ProcessMessages;
end;

procedure TMainForm.IgnoreFile(const AFile : TFileNameData);
begin
  FLogger.AddToSection(blsIgnore, AFile.RelativePathAndName);
end;

procedure TMainForm.StoreBinaryFile(const AFile : TFileNameData);
var
  SrcStream : TFileStream;
  DestStream : TStorageStream;
  LookFor : String;
  SingleStore : TStorage;
begin
  StatusBar.Panels[0].Text := 'Storing ' + AFile.JustNameAndExtn;

  SrcStream := TFileStream.Create(AFile.FullPathAndName, fmOpenRead);
  if FConsolidateData then begin
    FLogger.AddToSection(blsStoreOnly, AFile.RelativePathAndName);
    DestStream := TStorageStream.Create(AFile.JustNameAndExtn, FStorageStack.Items[0], amReadWrite, True);
    DestStream.CopyFrom(SrcStream, 0);
    DestStream.Free;
  end else begin
    { find a single storage named <cursubdirectory>.SingleStoreFileExtn }
    SingleStore := Nil;
    LookFor := '<unknown subdirectory>';
    if FSubdirList.Count > 0 then begin
      LookFor := FSubdirList.Strings[FSubdirList.Count-1]+'.'+SingleStoreFileExtn;
      if FSingleFilesStored.Values[LookFor] <> '' then
        SingleStore := TRootStorage.Create(FSingleFilesStored.Values[LookFor], amReadWrite, smExclusive, tmDirect, false);
    end;

    if SingleStore <> Nil then begin
      FLogger.AddToSection(blsStoreOnly, AFile.RelativePathAndName);
      DestStream := TStorageStream.Create(AFile.JustNameAndExtn, SingleStore, amReadWrite, True);
      DestStream.CopyFrom(SrcStream, 0);
      DestStream.Free;
      SingleStore.Free;
    end else
      FLogger.AddToSection(blsIgnore, AFile.RelativePathAndName + ' (substorage ' + LookFor + ' not found)');
  end;

  SrcStream.Free;
  StatusBar.Panels[0].Text := '';
  Application.ProcessMessages;
end;

function TMainForm.OnStorageStatus(const AMode : TStatusEventMode; const AMsg : String; const AProgress, ATotal : LongInt) : Boolean;
const
  Max : LongInt = 0;
var
  Pct : Cardinal;
begin
  case AMode of
    setStart : begin
        StatusBar.Panels[0].Text := AMsg;
        StatusBar.Panels[1].Text := '';
        Max := ATotal;
      end;
    setProgress : begin
      if Max > 0 then begin
        Pct := Trunc((AProgress/Max)*100);
        StatusBar.Panels[1].Text := IntToStr(Pct) + '%';
      end;
    end;
    setEnd : begin
        StatusBar.Panels[0].Text := '';
        StatusBar.Panels[1].Text := '';
      end;
  end;
  Result := not FCancelProcessing;
  Application.ProcessMessages;
end;

procedure TMainForm.btnBeginProcessingClick(Sender: TObject);
begin
  Process;
end;

procedure TMainForm.btnCancelProcessingClick(Sender: TObject);
begin
  FCancelProcessing := True;
end;

procedure TMainForm.BuildSingleBtnClick(Sender: TObject);
var
  FileData : TFileNameData;
begin
  if OpenDialog.Execute then begin
    FConsolidateData := False;
    FSingleFilesStored := TStringList.Create;
    FSubdirList := TStringList.Create;
    FileData := GetFileData(OpenDialog.FileName);
    FSubdirList.Add(FileData.JustNameNoExtn);

    ProcessAmlFile(FileData);
    ProcessDir(AddBackSlashS(FileData.JustPath) + FileData.JustNameNoExtn);

    FSubdirList.Free;
    FSingleFilesStored.Free;
  end;
end;

procedure TMainForm.btnSaveLogClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    BuildLogTree.SaveToFile(SaveDialog.FileName);
end;

end.
