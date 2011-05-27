unit MediaPlayerWin;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  MediaPlayer_TLB, MessageWin, Math;

type
  TMediaPlayerForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  protected
    FMediaPlayer : TWindowsMediaPlayer;
    procedure MediaPlayerReadyStateChange(Sender: TObject; ReadyState: ReadyStateConstants);
    procedure SetFormToImageSize;
  public
    procedure Play(const ASRC : String); overload;
    procedure Play(const ASRC : String; AStart : LongInt); overload;
    procedure Play(const ASRC : String; AStart, AEnd : LongInt); overload;

    property Player : TWindowsMediaPlayer read FMediaPlayer;
  end;

implementation

{$R *.DFM}

const
  CRLF = #13#10;

resourcestring
  rsMediaPlayerNotFound = 'To use the audio/video media features of The Alim you need to have the Microsoft Windows Media Player installed on your computer.'+
                          'You do not appear to have the Windows Media Player installed.'+CRLF+CRLF +
                          '1. Please exit The Alim.'+CRLF+
                          '2. From Windows, browse your Alim CD.'+CRLF+
                          '3. Open the folder called REDIST on the Alim CD.'+CRLF+
                          '4. Run the program called SetupMediaPlayer.'+CRLF+
                          '5. Follow any prompts or instructions in the Setup program.'+CRLF+CRLF+
                          'Once you have installed the Microsoft Windows Media Player, restart your computer and then run The Alim again. If you have any trouble '+
                          'or questions, please contact technical support via e-mail (support@islsoftware.com) or phone (301-622-3915).';

procedure TMediaPlayerForm.FormCreate(Sender: TObject);
begin
  // create this manually because delphi was having a problem creating it automatically
  // (kept erroring out)
  try
    FMediaPlayer := TWindowsMediaPlayer.Create(Self);
    with FMediaPlayer do begin
      Parent := Self;
      //OnPlayStateChange := MediaPlayerPlayStateChange;
      OnReadyStateChange := MediaPlayerReadyStateChange;
      Visible := True;
      Align := alClient;
      //ShowCaptioning := True;
      //ShowStatusBar := True
    end;
    //sldVolume.Value := FMediaPlayer.Volume;
  except
    FMediaPlayer := Nil;
    ShowMessage(rsMediaPlayerNotFound);
  end;
end;

procedure TMediaPlayerForm.Play(const ASRC : String);
begin
  if FMediaPlayer = Nil then
    Exit;

  FMediaPlayer.FileName := ASRC;
  try
    FMediaPlayer.Play;
  except
    on E : Exception do
      MessageWindow.Add('MP', '%s (%s)', [E.Message, ASRC]);
  end;
end;

procedure TMediaPlayerForm.Play(const ASRC : String; AStart : LongInt);
begin
  if FMediaPlayer = Nil then
    Exit;

  FMediaPlayer.Open(ASRC);
  FMediaPlayer.CurrentPosition := AStart;
  FMediaPlayer.SelectionStart := AStart;
  FMediaPlayer.Play;
end;

procedure TMediaPlayerForm.Play(const ASRC : String; AStart, AEnd : LongInt);
begin
  if FMediaPlayer = Nil then
    Exit;

  FMediaPlayer.Open(ASRC);
  FMediaPlayer.CurrentPosition := AStart;
  FMediaPlayer.SelectionStart := AStart;
  FMediaPlayer.SelectionEnd := AEnd;
  FMediaPlayer.Play;
end;

procedure TMediaPlayerForm.SetFormToImageSize;
const
  ControlPaneHeight = 75;
  ControlPaneWidth = 300;
begin
  if FMediaPlayer = Nil then
    Exit;

  if FMediaPlayer.ImageSourceWidth > 0 then begin
    Width := Max(FMediaPlayer.ImageSourceWidth, ControlPaneWidth);
    Height := FMediaPlayer.ImageSourceHeight+ControlPaneHeight;
  end;
end;

procedure TMediaPlayerForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if FMediaPlayer <> Nil then
    FMediaPlayer.Stop;
end;

procedure TMediaPlayerForm.MediaPlayerReadyStateChange(Sender: TObject; ReadyState: ReadyStateConstants);
begin
  if ReadyState = mpReadyStateComplete then
    SetFormToImageSize;
end;

end.
