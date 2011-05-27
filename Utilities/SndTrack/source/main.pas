unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  MediaPlayer_TLB, ExtCtrls, StdCtrls, CheckLst, RXSlider, RXSpin, RXCtrls,
  RxGIF, Db, Grids, DBGrids, DBISAMTb, Mask, DBCtrls, FileCtrl,
  QuranStruct, ToolEdit, CSPC, CSTCBase, ComCtrls, IslUtils, XmlParser,
  XmlObjModel;

const
  QuranStructDataFile = 'Quran Structure.aml';

type
  TMarkingStatus = (msIdle, msMarking, msMarkPaused);
  TQuranMediaTracker = class(TForm)
    tblTrackingData: TDBISAMTable;
    dsTrackingData: TDataSource;
    dsReciters: TDataSource;
    tblReciters: TDBISAMTable;
    lblSeekMediaPosn: TLabel;
    btnSeekMediaPos: TRxSpeedButton;
    gbxControls: TGroupBox;
    lblSura: TLabel;
    lblAyahStart: TLabel;
    lblSuraCount: TLabel;
    lblAyahCount: TLabel;
    spnAyah: TRxSpinEdit;
    spnSura: TRxSpinEdit;
    spnMediaPosition: TRxSpinEdit;
    pgcTabs: TcsPageControl;
    shtReciters: TcsTabSheet;
    lblReciterId: TLabel;
    lblReciterName: TLabel;
    lblLanguage: TLabel;
    lblMediaPath: TLabel;
    dbnReciters: TDBNavigator;
    dbeReciterId: TDBEdit;
    dbeReciterName: TDBEdit;
    dbcRecitationLanguage: TDBComboBox;
    dbeMediaPath: TDBEdit;
    shtTrackingData: TcsTabSheet;
    grdTrackingData: TDBGrid;
    shtAyahImage: TcsTabSheet;
    shtPrefs: TcsTabSheet;
    StatusBar: TStatusBar;
    GroupBox1: TGroupBox;
    dirAyahImages: TDirectoryEdit;
    GroupBox2: TGroupBox;
    MediaDriveComboBox: TDriveComboBox;
    MediaDirectoryListBox: TDirectoryListBox;
    FilterComboBox1: TFilterComboBox;
    MediaFileListBox: TFileListBox;
    lblActiveMediaFile: TLabel;
    bvlControls: TBevel;
    pnlAyahDisplay: TPanel;
    pnlAyahDisplayBkg: TPanel;
    imgAyah: TImage;
    gbxMarkAnd: TGroupBox;
    btnMarkAndStart: TRxSpeedButton;
    btnMarkAndContinue: TRxSpeedButton;
    btnMarkAndStop: TRxSpeedButton;
    btnMarkAndPause: TRxSpeedButton;
    lblTrkDataDblClickMsg: TLabel;
    dbnTrackingData: TDBNavigator;
    shtErrors: TcsTabSheet;
    dbgErrors: TDBGrid;
    dbnErrors: TDBNavigator;
    tblErrors: TDBISAMTable;
    dsErrors: TDataSource;
    btnVerifyAyahImages: TRxSpeedButton;
    pgbImageVerification: TProgressBar;
    lblVerifyingImages: TLabel;
    btnErrorsClear: TRxSpeedButton;
    btnVerifyTrackingData: TRxSpeedButton;
    lblVerifyingTrkData: TLabel;
    pgbTrkDataVerifcation: TProgressBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblAfterDoubleClick: TLabel;
    cbxAfterDoubleClick: TComboBox;
    XmlParser: TXmlObjModel;
    procedure FormCreate(Sender: TObject);
    procedure btnMarkAndContinueClick(Sender: TObject);
    procedure btnMarkAndStartClick(Sender: TObject);
    procedure btnMarkAndStopClick(Sender: TObject);
    procedure spnSuraChange(Sender: TObject);
    procedure btnSeekMediaPosClick(Sender: TObject);
    procedure grdTrackingDataDblClick(Sender: TObject);
    procedure spnAyahChange(Sender: TObject);
    procedure MediaFileListBoxDblClick(Sender: TObject);
    procedure btnMarkAndPauseClick(Sender: TObject);
    procedure tblRecitersAfterScroll(DataSet: TDataSet);
    procedure btnVerifyAyahImagesClick(Sender: TObject);
    procedure btnErrorsClearClick(Sender: TObject);
    procedure btnVerifyTrackingDataClick(Sender: TObject);
  private
    FDataDir : String;
    FMediaDir : String;
    FLastAyahStart : Double;
    FMediaPlayer : TWindowsMediaPlayer;
    FMediaFile : String;
    FMediaFileJustDir : String;
    FMediaFileJustName : String;
    FMarkingStatus : TMarkingStatus;
    FQuranStruct : TQuranStructure;
    procedure LogError(ACode, ASura, AAyah : Integer; AMsg : String);
    procedure OpenMediaFile(AFile : String);
    procedure MarkMediaPosition;
    procedure SetSura(ASura : Integer);
    procedure SetAyah(AAyah : Integer);
    procedure SetMarkStatus(AStatus : TMarkingStatus);
    procedure UpdateReciterStatus;
    procedure MediaPlayerPosChange(Sender: TObject; oldPosition: Double;
                                   newPosition: Double);
  public
  end;

var
  QuranMediaTracker: TQuranMediaTracker;

implementation

uses StStrS;

const
  LE_AYAHIMAGENOTFOUND = 1;
  LE_CANTLOADAYAHIMAGE = 2;
  LE_MISSINGSOUNDDATA_START = 1000;
{$R *.DFM}

procedure TQuranMediaTracker.FormCreate(Sender: TObject);
begin
  Caption := Caption + ' ' + GetAppVersionInfo;

  // create this manually because delphi was having a problem creating it automatically
  // (kept erroring out)
  FMediaPlayer := TWindowsMediaPlayer.Create(Self);
  with FMediaPlayer do begin
    Parent := Self;
    Top := 36;
    Left := 24;
    Width := 240;
    Height := 70;
    AutoStart := False;
    AutoRewind := False;
    ShowDisplay := False;
    ShowControls := True;
    ShowPositionControls := False;
    ShowStatusBar := True;
    EnableTracker := True;
    SendPlayStateChangeEvents := True;
    OnPositionChange := MediaPlayerPosChange;
  end;
  FDataDir := AddBackSlashS(JustPathNameS(Application.ExeName))+'data';
  FMediaDir := AddBackSlashS(JustPathNameS(Application.ExeName))+'media';
  dirAyahImages.Text := AddBackSlashS(JustPathNameS(Application.ExeName))+'ayahimages';
  MediaDirectoryListBox.Directory := FMediaDir;

  FQuranStruct := TQuranStructure.Create;
  try
    XmlParser.LoadDataSource(AddBackSlashS(JustPathNameS(Application.ExeName)) + QuranStructDataFile);
    FQuranStruct.LoadFromAML(XmlParser.Document.DocumentElement);
  except
    raise Exception.Create('Very serious error in Quran Structure file. Please exit now.');
  end;

  with tblReciters do begin
    DatabaseName := FDataDir;
    TableName := 'reciters.dat';
    Active := True;
  end;
  with tblTrackingData do begin
    DatabaseName := FDataDir;
    TableName := 'recitedata.dat';
    Active := True;
  end;
  with tblErrors do begin
    DatabaseName := FDataDir;
    TableName := 'errors.dat';
    Active := True;
  end;

  cbxAfterDoubleClick.ItemIndex := 0;

  UpdateReciterStatus;
  SetMarkStatus(msIdle);
  SetSura(1);
end;

procedure TQuranMediaTracker.LogError(ACode, ASura, AAyah : Integer; AMsg : String);
begin
  StatusBar.Panels[3].Text := AMsg;
  with tblErrors do begin
    if not FindKey([ACode, ASura, AAyah]) then begin
      Append;
      FieldValues['code'] := ACode;
      FieldValues['sura'] := ASura;
      FieldValues['ayah'] := AAyah;
      FieldValues['message'] := AMsg;
      Post;
    end;
  end;
end;

procedure TQuranMediaTracker.OpenMediaFile(AFile : String);
begin
  lblActiveMediaFile.Caption := AFile;
  FMediaFile := AFile;
  FMediaFileJustDir := JustPathNameS(AFile);
  FMediaFileJustName := JustFileNameS(AFile);
  FMediaPlayer.Open(AFile);
end;

procedure TQuranMediaTracker.MarkMediaPosition;
begin
  with tblTrackingData do begin
    if FindKey([tblReciters.FieldValues['id'], spnSura.AsInteger, spnAyah.AsInteger]) then
      Edit
    else
      Append;
    FieldValues['id'] := tblReciters.FieldValues['id'];
    FieldValues['sura'] := spnSura.AsInteger;
    FieldValues['ayah'] := spnAyah.AsInteger;
    FieldValues['file'] := FMediaFileJustName;
    FieldValues['start'] := FLastAyahStart;
    FieldValues['end'] := FMediaPlayer.CurrentPosition;
    Post;
  end;
  FLastAyahStart := FMediaPlayer.CurrentPosition;
  grdTrackingData.Update;
end;

procedure TQuranMediaTracker.SetSura(ASura : Integer);
begin
  if spnSura.AsInteger <> ASura then
    spnSura.AsInteger := ASura;
  spnAyah.MaxValue := FQuranStruct.Sura[spnSura.AsInteger].AyahCount;
  lblAyahCount.Caption := 'of ' + IntToStr(FQuranStruct.Sura[spnSura.AsInteger].AyahCount);
  SetAyah(1);
end;

procedure TQuranMediaTracker.SetAyah(AAyah : Integer);
var
  ImageFName : String;
  ImageFullPath : String;
begin
  if spnAyah.AsInteger <> AAyah then
    spnAyah.AsInteger := AAyah;
  ImageFName := Format('s%d\a%d.gif', [spnSura.AsInteger, spnAyah.AsInteger]);
  ImageFullPath := Format('%s\%s', [dirAyahImages.Text, ImageFName]);
  if FileExists(ImageFullPath) then begin
    try
      imgAyah.Picture.LoadFromFile(ImageFullPath);
    except
      imgAyah.Picture := Nil;
      LogError(LE_CANTLOADAYAHIMAGE, spnSura.AsInteger, spnAyah.AsInteger, Format('Unable to load ayah image not found (%s)', [ImageFullPath]));
    end;
  end else
    LogError(LE_AYAHIMAGENOTFOUND, spnSura.AsInteger, spnAyah.AsInteger, Format('Ayah image not found (%s)', [ImageFName]));
  StatusBar.Panels[2].Text := Format('Ayah %d.%d', [spnSura.AsInteger, spnAyah.AsInteger]);
  tblTrackingData.FindKey([tblReciters.FieldValues['id'], spnSura.AsInteger, spnAyah.AsInteger])
end;

procedure TQuranMediaTracker.SetMarkStatus(AStatus : TMarkingStatus);
var
  Msg : String;
begin
  FMarkingStatus := AStatus;
  case AStatus of
    msIdle : Msg := 'Not marking.';
    msMarking : Msg := Format('Marking (%f)', [FLastAyahStart]);
    msMarkPaused  : Msg := 'Marking paused.';
  end;
  StatusBar.Panels[1].Text := Msg;
end;

procedure TQuranMediaTracker.UpdateReciterStatus;
begin
  StatusBar.Panels[0].Text := tblReciters.FieldValues['reciter'];
end;

procedure TQuranMediaTracker.MediaPlayerPosChange(Sender: TObject; oldPosition: Double;
                                   newPosition: Double);
begin
  spnMediaPosition.Value := newPosition;
end;

procedure TQuranMediaTracker.btnMarkAndContinueClick(Sender: TObject);
begin
  FMediaPlayer.Pause;
  MarkMediaPosition;
  if spnAyah.AsInteger < spnAyah.MaxValue then
    SetAyah(spnAyah.AsInteger+1)
  else begin
    if spnSura.AsInteger < High(TSuraNum) then
      SetSura(spnSura.AsInteger+1)
    else begin
      FMediaPlayer.Stop;
      SetMarkStatus(msIdle);
      Exit;
    end;
  end;
  SetMarkStatus(msMarking);
  FMediaPlayer.Play;
end;

procedure TQuranMediaTracker.btnMarkAndStartClick(Sender: TObject);
begin
  FLastAyahStart := FMediaPlayer.CurrentPosition;
  FMediaPlayer.Play;
  SetMarkStatus(msMarking);
end;

procedure TQuranMediaTracker.btnMarkAndStopClick(Sender: TObject);
begin
  FMediaPlayer.Pause;
  MarkMediaPosition;
  FMediaPlayer.Stop;
  SetMarkStatus(msIdle);
end;

procedure TQuranMediaTracker.spnSuraChange(Sender: TObject);
begin
  SetSura(spnSura.AsInteger);
end;

procedure TQuranMediaTracker.btnSeekMediaPosClick(Sender: TObject);
begin
  FMediaPlayer.CurrentPosition := spnMediaPosition.Value;
end;

procedure TQuranMediaTracker.grdTrackingDataDblClick(Sender: TObject);
var
  S, A : Integer;
begin
  FMediaPlayer.Stop;
  SetMarkStatus(msIdle);

  with tblTrackingData do begin
    S := FieldValues['sura'];
    A := FieldValues['ayah'];
    SetSura(S);
    SetAyah(A);
    FMediaPlayer.CurrentPosition := FieldValues['start'];
    spnMediaPosition.Value := FMediaPlayer.CurrentPosition;
  end;
  
  case cbxAfterDoubleClick.ItemIndex of
    0 : btnMarkAndStartClick(Sender);
    1 : FMediaPlayer.Play;
    2 : ;
  end;
end;

procedure TQuranMediaTracker.spnAyahChange(Sender: TObject);
begin
  SetAyah(spnAyah.AsInteger);
end;

procedure TQuranMediaTracker.MediaFileListBoxDblClick(Sender: TObject);
begin
  OpenMediaFile(MediaFileListBox.FileName);
end;

procedure TQuranMediaTracker.btnMarkAndPauseClick(Sender: TObject);
begin
  FMediaPlayer.Pause;
  MarkMediaPosition;
  SetMarkStatus(msMarkPaused);
end;

procedure TQuranMediaTracker.tblRecitersAfterScroll(DataSet: TDataSet);
begin
  UpdateReciterStatus;
end;


procedure TQuranMediaTracker.btnVerifyAyahImagesClick(Sender: TObject);
var
  Sura : TSuraNum;
  Ayah : TAyahNum;
  ImageFName : String;
  ImageFullPath : String;
  CheckedCount, ErrorsCount : Integer;
begin
  pgbImageVerification.Min := Low(TSuraNum);
  pgbImageVerification.Max := High(TSuraNum);
  pgbImageVerification.Visible := True;
  lblVerifyingImages.Visible := True;
  btnVerifyAyahImages.Enabled := False;
  ErrorsCount := 0;
  CheckedCount := 0;
  Update;
  for Sura := Low(TSuraNum) to High(TSuraNum) do begin
    pgbImageVerification.Position := Sura;
    for Ayah := 1 to FQuranStruct.Sura[Sura].AyahCount do begin
      ImageFName := Format('s%d\a%d.gif', [Sura, Ayah]);
      ImageFullPath := Format('%s\%s', [dirAyahImages.Text, ImageFName]);
      if not FileExists(ImageFullPath) then begin
        Inc(ErrorsCount);
        LogError(LE_AYAHIMAGENOTFOUND, Sura, Ayah, Format('Ayah image not found (%s)', [ImageFName]));
      end;
      Inc(CheckedCount);
    end;
  end;
  btnVerifyAyahImages.Enabled := True;
  pgbImageVerification.Visible := False;
  lblVerifyingImages.Visible := False;
  ShowMessage(Format('Verification Completed. %d files checked, %d errors were logged.', [CheckedCount, ErrorsCount]));
end;

procedure TQuranMediaTracker.btnErrorsClearClick(Sender: TObject);
begin
  if MessageDlg('Are you sure you want to clear the entire error log?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
    with tblErrors do begin
      Close;
      Exclusive := True;
      EmptyTable;
      Exclusive := False;
      Open;
    end;
  end;
end;

procedure TQuranMediaTracker.btnVerifyTrackingDataClick(Sender: TObject);
var
  Sura : TSuraNum;
  Ayah : TAyahNum;
  CheckedCount, ErrorsCount : Integer;
begin
  pgbTrkDataVerifcation.Min := Low(TSuraNum);
  pgbTrkDataVerifcation.Max := High(TSuraNum);
  pgbTrkDataVerifcation.Visible := True;
  lblVerifyingTrkData.Visible := True;
  btnVerifyTrackingData.Enabled := False;
  ErrorsCount := 0;
  CheckedCount := 0;
  Update;
  for Sura := Low(TSuraNum) to High(TSuraNum) do begin
    pgbTrkDataVerifcation.Position := Sura;
    for Ayah := 1 to FQuranStruct.Sura[Sura].AyahCount do begin
      if not tblTrackingData.FindKey([tblReciters.FieldValues['id'], Sura, Ayah]) then begin
        Inc(ErrorsCount);
        LogError(LE_MISSINGSOUNDDATA_START+tblReciters.FieldValues['id'], Sura, Ayah, Format('Sound data not found (%d.%d)', [Sura, Ayah]));
      end;
      Inc(CheckedCount);
    end;
  end;
  btnVerifyTrackingData.Enabled := True;
  pgbTrkDataVerifcation.Visible := False;
  lblVerifyingTrkData.Visible := False;
  ShowMessage(Format('Verification Completed. %d files checked, %d errors were logged. Error codes will be %d+Reciter Id.', [CheckedCount, ErrorsCount, LE_MISSINGSOUNDDATA_START]));
end;

end.
