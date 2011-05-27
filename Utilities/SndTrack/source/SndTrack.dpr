program SndTrack;

uses
  Forms,
  main in 'main.pas' {QuranMediaTracker};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TQuranMediaTracker, QuranMediaTracker);
  Application.Run;
end.
