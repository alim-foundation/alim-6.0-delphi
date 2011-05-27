unit ReciteView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Db, DBISAMTb, OleCtrls, MediaPlayer_TLB,
  StColl, RXCtrls, QuranStruct, ComCtrls, RXSpin, CheckLst, RXSlider,
  RxGIF, CSTCBase, CSTC32, Math, CSPC, AmlView, Menus, RxMenus, islctrls,
  TimerLst, MessageWin, Grids, DBGrids, RXDBCtrl;

const
  DefaultSelStartEndSetCount = 2;

type
  TReciter = class(TObject)
    FId : Integer;
    FName : String;
    FShortName : String;
    FPaths : TStringList;
    FLanguage : String;
    FMediaFileExt : String;
    FAddrMgr : IAddressManager;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function FindFileInPath(AFileName : String; var ASearched : String) : String;
    procedure AddPath(APath : String);
    property ID : Integer read FId;
    property Name : String read FName;
    property Language : String read FLanguage;
    property MediaFileExt : String read FMediaFileExt;
    property ShortName : String read FName;
    property AddrMgr : IAddressManager read FAddrMgr;
  end;

  TReciters = class(TStCollection)
  protected
    function GetReciter(AIndex : Integer) : TReciter;
  public
    constructor Create(ASize : Integer); override;
    procedure Add(AReciter : TReciter);
    property Reciter[Index : Integer] : TReciter read GetReciter; default;
  end;

  TRecitationViewer = class(TForm)
    tblReciters: TDBISAMTable;
    tblReciteData: TDBISAMTable;
    csPageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    PanelStatus: TWallpaperPanel;
    LabelStatus: TLabel;
    LabelReciter: TLabel;
    LabelAyah: TLabel;
    GroupBox2: TGroupBox;
    lblAyahEnd: TLabel;
    Label1: TLabel;
    spnAyahEnd: TRxSpinEdit;
    spnAyahStart: TRxSpinEdit;
    GroupBox1: TGroupBox;
    cbxSuras: TComboBox;
    btnStart: TRxSpeedButton;
    btnStop: TRxSpeedButton;
    lblRepeat: TLabel;
    spnRepeat: TRxSpinEdit;
    Label2: TLabel;
    GroupBox5: TGroupBox;
    TrackAyahsCheckBox: TCheckBox;
    Label3: TLabel;
    ContinuousCheckBox: TCheckBox;
    chlReciters: TCheckListBox;
    Label4: TLabel;
    StopOnErrorCheckBox: TCheckBox;
    AyahTimers: TRxTimerList;
    RxTimerEvent1: TRxTimerEvent;
    PanelDebug: TPanel;
    FileLabel: TLabel;
    StartEndLabel: TLabel;
    CurrentPosLabel: TLabel;
    RemainLabel: TLabel;
    FirstPlayLabel: TLabel;
    sldVolume: TRxSlider;
    Image2: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbxSurasChange(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure MediaPlayerPlayStateChange(Sender: TObject; OldState,
      NewState: Integer);
    procedure MediaPlayerReadyStateChange(Sender: TObject; ReadyState: ReadyStateConstants);
    procedure spnAyahStartChange(Sender: TObject);
    procedure spnAyahEndChange(Sender: TObject);
    procedure spnRepeatChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure sldVolumeChange(Sender: TObject);
    procedure RxTimerEvent1Timer(Sender: TObject);
  protected
    FMediaPlayer : TWindowsMediaPlayer;
    FReciters : TReciters;
    FActiveReciter : TReciter;
    FActiveReciterIdx : Integer;
    FActiveSura : TSuraNum;
    FActiveAyah : TAyahNum;
    FStopBtnPressed : Boolean;
    FStart : Double;
    FFinish : Double;
    FDuration : Double;
    FAddrMgr : IAddressManager;
    FDataMgr : TDataManager;
    FStruct : TQuranStructure;
    FSetFirstPlayLabel : Boolean;
    FPlayWholeFile : Boolean;
    FShowDebugPanel : Boolean;
    FSelStartEndSetCount : Integer;
    FRecitingRange : Boolean;

    procedure Recite;
    procedure ContinueReciting;

    procedure ClearAyah;
    function FindNextReciter : Boolean;
    procedure ForceStop;
    procedure SetAddrManager(const AMgr : IAddressManager);
    procedure SetReciter(AReciter : TReciter);
    procedure SetSura(ASura : TSuraNum);
    procedure SetAyah(AAyah : TAyahNum);
    procedure SetQuranStruct(const AStruct : TQuranStructure);
    procedure ReciterMenuItemChange(Sender: TObject);
    procedure ShowStatus(const AStatus : String);
  public
    procedure ReciteContinuous;
    procedure ReciteRange(const ASura : TSuraNum; const AFromAyah, AToAyah : TAyahNum);

    property AddressMgr : IAddressManager read FAddrMgr write SetAddrManager;
    property DataMgr : TDataManager read FDataMgr write FDataMgr;
    property QuranStruct : TQuranStructure read FStruct write SetQuranStruct;
    property Reciter : TReciter read FActiveReciter write SetReciter;
    property Sura : TSuraNum read FActiveSura write SetSura;
    property Ayah : TAyahNum read FActiveAyah write SetAyah;
    property ShowDebugPanel : Boolean read FShowDebugPanel write FShowDebugPanel;
    property SelStartEndSetCount : Integer read FSelStartEndSetCount write FSelStartEndSetCount;
  end;

implementation

uses StStrS, StrUtils, IslUtils;

resourcestring
  rsNoRecitersSelected  = 'No reciter has been selected yet.';
  rsMediaPlayerNotFound = 'To use the recitation features of The Alim you need to have the Microsoft Windows Media Player installed on your computer.'+
                          'You do not appear to have the Windows Media Player installed.'+CRLF+CRLF +
                          '1. Please exit The Alim.'+CRLF+
                          '2. From Windows, browse your Alim CD.'+CRLF+
                          '3. Open the folder called REDIST on the Alim CD.'+CRLF+
                          '4. Run the program called SetupMediaPlayer.'+CRLF+
                          '5. Follow any prompts or instructions in the Setup program.'+CRLF+CRLF+
                          'Once you have installed the Microsoft Windows Media Player, restart your computer and then run The Alim again. If you have any trouble '+
                          'or questions, please contact technical support via e-mail (support@islsoftware.com) or phone (301-622-3915).';
  rsMediaPlayerError    = 'The Alim has encountered a Microsoft MediaPlayer Error.' +CRLF + 'Please verify your MediaPlayer installation.' +CRLF+CRLF + 'Code: %s';
  rsErrorPlaying        = 'Error playing Ayah %d.%d.'+CRLF+CRLF+'Would you like to continue with the next Ayah or reciter?';

  rsMediaFileNotFound = 'Error: Sura %d Ayah %d media not found%2:s%2:sSearched%2:s%s.'+CRLF+CRLF+'Would you like to continue with the next Ayah or reciter?';
  rsSuraAyahLocNotFound = 'Error: Sura %d Ayah %d not found in ReciteData.dat for %s (%d).'+CRLF+'Would you like to continue with the next Ayah or reciter?'+CRLF+CRLF+
                          'Note: You can turn off (uncheck) the "Stop on Errors" option (found in the Customize tag)'+CRLF+'if you do not want to see this message again.';

{$R *.DFM}

procedure DisposeObject(AObject : Pointer); far;
begin
  TObject(AObject).Free;
end;

constructor TReciter.Create;
begin
  inherited Create;
  FPaths := TStringList.Create;
end;

destructor TReciter.Destroy;
begin
  FPaths.Free;
  inherited Destroy;
end;

procedure TReciter.AddPath(APath : String);
var
  MediaPath : String;
  Style : TDirPathStyle;
  PathCount, I : Integer;

  procedure AddSinglePath(const ASinglePath : String);
  var
    S : String;
  begin
    S := ReplaceStr(APath, '${mediaDir}', ASinglePath);
    S := ReplaceStr(S, '${reciteId}', IntToStr(Id));
    S := ReplaceStr(S, '${reciteName}', Name);
    S := ReplaceStr(S, '${reciteLang}', Language);
    FPaths.Add(S);
  end;

begin
  if TrimS(APath) = '' then
    Exit;

  MediaPath := FAddrMgr.GetDirectoryPath(MediaPathId, Style);
  if Style = dpsMultiple then begin
    PathCount := WordCountS(MediaPath, MultiDirSeparator);
    for I := 1 to PathCount do
      AddSinglePath(ExtractWordS(I, MediaPath, MultiDirSeparator));
  end else
    AddSinglePath(MediaPath);

  MediaPath := FAddrMgr.GetDirectoryPath(MediaSrcPathId, Style);
  if Style = dpsMultiple then begin
    PathCount := WordCountS(MediaPath, MultiDirSeparator);
    for I := 1 to PathCount do
      AddSinglePath(ExtractWordS(I, MediaPath, MultiDirSeparator));
  end else
    AddSinglePath(MediaPath);
end;

function TReciter.FindFileInPath(AFileName : String; var ASearched : String) : String;
var
  I, C : Integer;
begin
  Result := '';
  ASearched := '';

  C := FPaths.Count;
  if C <= 0 then
    Exit;

  Dec(C);
  for I := 0 to C do begin
    Result := AddBackSlashS(FPaths[I]) + DefaultExtensionS(AFileName, MediaFileExt);
    if ASearched <> '' then
      ASearched := ASearched + ';' + Result
    else
      ASearched := Result;
    if FileExists(Result) then
      Exit;
  end;

  Result := '';
end;

constructor TReciters.Create(ASize : Integer);
begin
  inherited Create(ASize);
  DisposeData := DisposeObject;
end;

function TReciters.GetReciter(AIndex : Integer) : TReciter;
begin
  Result := TReciter(Items[AIndex]);
end;

procedure TReciters.Add(AReciter : TReciter);
begin
  Insert(AReciter);
end;

//------------------------------------------------------------------------------

procedure MediaPlayerErrorMsg;
begin
  ShowMessage(rsMediaPlayerNotFound);
end;

procedure TRecitationViewer.FormCreate(Sender: TObject);
begin
  // create this manually because delphi was having a problem creating it automatically
  // (kept erroring out)
  try
    FMediaPlayer := TWindowsMediaPlayer.Create(Self);
    with FMediaPlayer do begin
      Parent := Self;
      SendPlayStateChangeEvents := True;
      OnPlayStateChange := MediaPlayerPlayStateChange;
      OnReadyStateChange := MediaPlayerReadyStateChange;
      Visible := False;
      AutoStart := False;
    end;
    sldVolume.Value := FMediaPlayer.Volume;
  except
    FMediaPlayer := Nil;
    MediaPlayerErrorMsg;
  end;
  if FSelStartEndSetCount < DefaultSelStartEndSetCount then
    FSelStartEndSetCount := DefaultSelStartEndSetCount;
end;

procedure TRecitationViewer.FormDestroy(Sender: TObject);
begin
  if FReciters <> Nil then
    FReciters.Free;
end;

procedure TRecitationViewer.SetAddrManager(const AMgr : IAddressManager);
const
  RecitersData = 'reciters.dat';
var
  NewReciter : TReciter;
  Path : String;
  RecIdx, I, PathCount : Integer;
begin
  FAddrMgr := AMgr;
  if FAddrMgr = Nil then begin
    if FReciters <> Nil then
      FReciters.Free;
    FReciters := Nil;
    Exit;
  end;

  FReciters := TReciters.Create(8);
  with tblReciters do begin
    if FileExists(FAddrMgr.ForceIntoPath(ReciteMapPathId, RecitersData)) then
      DatabaseName := FAddrMgr.GetDirectoryPath(ReciteMapPathId)
    else
      DatabaseName := FAddrMgr.GetDirectoryPath(ReciteMapSrcPathId);
    TableName := RecitersData;
    Open;

    chlReciters.Items.Clear;
    while not EOF do begin
      NewReciter := TReciter.Create;
      FReciters.Add(NewReciter);

      NewReciter.FAddrMgr := FAddrMgr;
      NewReciter.FID := FieldValues['id'];
      NewReciter.FName := FieldValues['reciter'];
      NewReciter.FLanguage := FieldValues['language'];
      NewReciter.FMediaFileExt := FieldValues['mediafileext'];

      Path := FieldValues['path'];
      PathCount := WordCountS(Path, MultiDirSeparator);
      for I := 1 to PathCount do
        NewReciter.AddPath(ExtractWordS(I, Path, MultiDirSeparator));

      RecIdx := chlReciters.Items.AddObject(Format('%s (%s)', [NewReciter.Name, NewReciter.Language]), NewReciter);
      chlReciters.Checked[RecIdx] := True;
      Next;
    end;
  end;

  with tblReciteData do begin
    DatabaseName := tblReciters.DatabaseName;
    TableName := 'recitedata.dat';
    Open;
  end;
end;

procedure TRecitationViewer.SetQuranStruct(const AStruct : TQuranStructure);
var
  S : TSuraNum;
begin
  FStruct := AStruct;
  if FStruct = Nil then begin
    cbxSuras.Items.Clear;
    Exit;
  end;

  cbxSuras.Items.BeginUpdate;
  cbxSuras.Items.Clear;
  for S := Low(TSuraNum) to High(TSuraNum) do begin
    cbxSuras.Items.Add(Format('%d. %s', [S, FStruct.SuraName[S]]));
  end;
  cbxSuras.Items.EndUpdate;

  Sura := 1;
end;

procedure TRecitationViewer.SetReciter(AReciter : TReciter);
var
  Idx : Integer;
begin
  FActiveReciter := AReciter;

  Idx := chlReciters.Items.IndexOfObject(AReciter);
  if Idx <> chlReciters.ItemIndex then
    chlReciters.ItemIndex := Idx;
end;

procedure TRecitationViewer.SetSura(ASura : TSuraNum);
begin
  FActiveSura := ASura;
  if cbxSuras.ItemIndex <> ASura-1 then
    cbxSuras.ItemIndex := ASura-1;

  Assert(FStruct <> Nil);
  with FStruct.Sura[FActiveSura] do begin
    spnAyahStart.MinValue := 1;
    spnAyahStart.MaxValue := ayahCount;

    spnAyahEnd.MinValue := 1;
    spnAyahEnd.MaxValue := ayahCount;

    spnAyahStart.Value := 1;
    spnAyahEnd.Value := ayahCount;
  end;

  Ayah := 1;
end;

procedure TRecitationViewer.SetAyah(AAyah : TAyahNum);
begin
  FActiveAyah := AAyah;
  if FActiveAyah <> spnAyahStart.AsInteger then
    spnAyahStart.Value := FActiveAyah;
end;

procedure TRecitationViewer.btnStartClick(Sender: TObject);
begin
  FActiveReciterIdx := -1;
  FActiveAyah := spnAyahStart.AsInteger;
  if FindNextReciter then
    Recite
  else
    ShowMessage(rsNoRecitersSelected);
end;

function TRecitationViewer.FindNextReciter : Boolean;
var
  I : Integer;
begin
  Result := False;
  if (chlReciters.Items.Count > 0) and (FActiveReciterIdx < chlReciters.Items.Count) then begin
    for I := Succ(FActiveReciterIdx) to chlReciters.Items.Count-1 do
      if chlReciters.Checked[I] then begin
        Reciter := FReciters[I];
        FActiveReciterIdx := I;
        Result := True;
        break;
      end;
  end;

  if not Result then
    FActiveReciterIdx := -1;
end;

procedure TRecitationViewer.ForceStop;
begin
  FStopBtnPressed := True;
  try
    if FMediaPlayer <> Nil then
      FMediaPlayer.Stop;
  except
  end;
  AyahTimers.Active := False;
end;

procedure TRecitationViewer.ReciteRange(const ASura : TSuraNum; const AFromAyah, AToAyah : TAyahNum);
begin
  Sura := ASura;
  Ayah := AFromAyah;
  spnAyahEnd.Value := AToAyah;
  FRecitingRange := True;
  btnStartClick(btnStart);
end;

procedure TRecitationViewer.ReciteContinuous;
begin
  FRecitingRange := False;
  btnStartClick(btnStart);
end;

procedure TRecitationViewer.Recite;
var
  JustFName, FullFName, Searched : String;
begin
  if FMediaPlayer = Nil then begin
    MediaPlayerErrorMsg;
    Exit;
  end;
  if FActiveReciter = Nil then begin
    ShowMessage(rsNoRecitersSelected);
    Exit;
  end;

  FStopBtnPressed := False;
  with tblReciteData do begin
    Filtered := False;
    Filter := Format('id=%d and sura=%d and ayah=%d', [FActiveReciter.Id, FActiveSura, FActiveAyah]);
    Filtered := True;
    try
      if not EOF then begin
        //FMediaPlayer.Stop;
        FMediaPlayer.FileName := '';
        
        JustFName := ReplaceStr(FieldValues['file'], '${suraNum}', IntToStr(FActiveSura));
        JustFName := ReplaceStr(JustFName, '${ayahNum}', IntToStr(FActiveAyah));
        FullFName := FActiveReciter.FindFileInPath(JustFName, Searched);
        if FullFName <> '' then begin
          FMediaPlayer.AutoStart := False;
          FStart := FieldValues['start'];
          FFinish := FieldValues['end'];
          FPlayWholeFile := (Trunc(FStart) = 0) and (Trunc(FFinish) = 0);

          FileLabel.Caption := FieldValues['file'];
          CurrentPosLabel.Caption := '';
          FirstPlayLabel.Caption := '';
          FSetFirstPlayLabel := True;

          FMediaPlayer.Open(FullFName);
        end else begin
          MessageWindow.Add('RECITE', 'Ayah %d.%d media file (%s) not found', [FActiveSura, FActiveAyah, JustFName]);
          if StopOnErrorCheckBox.Checked and (MessageDlg(Format(rsMediaFileNotFound, [FActiveSura, FActiveAyah, CRLF, ReplaceStr(Searched, ';', CRLF)]), mtError, [mbYes, mbNo], 0) = mrNo) then
            ClearAyah
          else
            ContinueReciting;
        end;
      end else begin
        MessageWindow.Add('RECITE', 'Ayah %d.%d tracking data not found', [FActiveSura, FActiveAyah]);
        if StopOnErrorCheckBox.Checked and (MessageDlg(Format(rsSuraAyahLocNotFound, [FActiveSura, FActiveAyah, FActiveReciter.Name, FActiveReciter.Id]), mtError, [mbYes, mbNo], 0) = mrNo) then
          ClearAyah
        else
          ContinueReciting;
      end;
  except
    on E : Exception do
      ShowMessage(Format(rsMediaPlayerError, [E.Message]));
  end;
  end;
end;

procedure TRecitationViewer.ContinueReciting;
begin
  if FStopBtnPressed then begin
    ClearAyah;
    FStopBtnPressed := False;
  end else begin
    if not FindNextReciter then begin   // we've come to the end of the list of reciters, so move to the next ayah
      if FindNextReciter then begin     // reset to first reciter
        if FActiveAyah < spnAyahEnd.AsInteger then begin
          Ayah := FActiveAyah + 1;
          Recite;
        end else if (FActiveAyah = FStruct.Sura[FActiveSura].ayahCount) and (ContinuousCheckBox.Checked) and (FActiveSura < High(TSuraNum)) then begin
          SetSura(FActiveSura+1);
          Ayah := 1;
          Recite;
        end else
          ClearAyah;
      end;
    end else
      Recite;
  end;
end;

procedure TRecitationViewer.btnStopClick(Sender: TObject);
begin
  ForceStop;
end;

procedure TRecitationViewer.ClearAyah;
begin
  ShowStatus('');
  btnStop.Enabled := False;
  btnStart.Enabled := True;
end;

procedure TRecitationViewer.MediaPlayerPlayStateChange(Sender: TObject;
  OldState, NewState: Integer);
var
  Msg : String;
begin
  Assert(FStruct <> Nil);

  if NewState = mpPlaying then begin
    btnStart.Enabled := False;
    btnStop.Enabled := True;
    ShowStatus('Reciting');
    if TrackAyahsCheckBox.Checked and not FRecitingRange then
      FAddrMgr.SetAddress(CreateQuranAddress(qatGeneric, FActiveSura, FActiveAyah));
  end else if (NewState = mpStopped) and (not Application.Terminated) then begin
    ContinueReciting;
  end else begin
    ClearAyah;
    Msg := '';
    case NewState of
      mpPaused : Msg := 'Paused';
      mpWaiting : Msg := 'Waiting';
      mpScanForward : Msg := 'Scanning';
      mpScanReverse : Msg := 'Scanning';
      mpClosed : Msg := 'Closed';
    end;
    ShowStatus(Msg);
  end;
end;

procedure TRecitationViewer.MediaPlayerReadyStateChange(Sender: TObject; ReadyState: ReadyStateConstants);
var
  I : Integer;
begin
  if ReadyState <> mpReadyStateComplete then
    Exit;

  try
    if FPlayWholeFile then begin
      StartEndLabel.Caption := 'Playing entire file';
      RemainLabel.Caption := '';
    end else begin
      FDuration := FFinish - FStart;
      for I := 1 to FSelStartEndSetCount do begin
        FMediaPlayer.SelectionStart := FStart;
        FMediaPlayer.SelectionEnd := FFinish;
      end;
      StartEndLabel.Caption := Format('%f - %f', [FStart, FFinish]);
      RemainLabel.Caption := Format('%f - %f', [FDuration, FDuration]);
    end;

    FMediaPlayer.PlayCount := spnRepeat.AsInteger;
    FMediaPlayer.Play;
  except
    on E : Exception do begin
      MessageWindow.Add('RECITE', 'Error playing Ayah %d.%d (%f-%f): not found', [FActiveSura, FActiveAyah, FStart, FFinish]);
      if StopOnErrorCheckBox.Checked and (MessageDlg(Format(rsErrorPlaying, [FActiveSura, FActiveAyah]), mtError, [mbYes, mbNo], 0) = mrNo) then
        ClearAyah
      else
        ContinueReciting;
    end;
  end;

  AyahTimers.Active := FShowDebugPanel;
end;

procedure TRecitationViewer.ShowStatus(const AStatus : String);
begin
  if FActiveReciter = Nil then
    Exit;

  PanelStatus.Visible := AStatus <> '';
  PanelDebug.Visible := FShowDebugPanel and (AStatus <> '');
  LabelStatus.Caption := AStatus;
  LabelReciter.Caption := FActiveReciter.ShortName;
  LabelAyah.Caption := Format('%s %d', [FStruct.SuraName[FActiveSura], FActiveAyah]);
end;

procedure TRecitationViewer.ReciterMenuItemChange(Sender: TObject);
begin
end;

procedure TRecitationViewer.cbxSurasChange(Sender: TObject);
begin
  ForceStop;
  Sura := cbxSuras.ItemIndex+1;
end;

procedure TRecitationViewer.spnAyahStartChange(Sender: TObject);
begin
  ForceStop;
  if spnAyahStart.Value > spnAyahEnd.Value then
    spnAyahEnd.Value := spnAyahStart.Value;
  Ayah := spnAyahStart.AsInteger;
end;

procedure TRecitationViewer.spnAyahEndChange(Sender: TObject);
begin
  ForceStop;
  if spnAyahEnd.Value < spnAyahStart.Value then
    spnAyahStart.Value := spnAyahEnd.Value;
  Ayah := spnAyahStart.AsInteger;
end;

procedure TRecitationViewer.spnRepeatChange(Sender: TObject);
begin
  if FMediaPlayer <> Nil then
    FMediaPlayer.PlayCount := spnRepeat.AsInteger;
end;

procedure TRecitationViewer.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  ForceStop;
  CanClose := True;
end;

procedure TRecitationViewer.sldVolumeChange(Sender: TObject);
begin
  if FMediaPlayer <> Nil then
    FMediaPlayer.Volume := sldVolume.Value;
end;

procedure TRecitationViewer.RxTimerEvent1Timer(Sender: TObject);
var
  CurrPos, Remaining : Double;
begin
  CurrPos := FMediaPlayer.CurrentPosition;
  Remaining := FFinish - CurrPos;
  CurrentPosLabel.Caption := FloatToStr(FMediaPlayer.CurrentPosition);
  if FSetFirstPlayLabel and (CurrPos >= 0) then begin
    FirstPlayLabel.Caption:= CurrentPosLabel.Caption;
    FSetFirstPlayLabel := False;
  end;
  if not FPlayWholeFile then
    RemainLabel.Caption := Format('%f - %f', [FDuration, Remaining]);
end;

end.
 