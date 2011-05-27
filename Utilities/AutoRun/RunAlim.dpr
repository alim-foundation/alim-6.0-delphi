program RunAlim;

uses
  Windows,
  Forms,
  SysUtils,
  StRegIni,
  StStrS;

{$R *.RES}

  function FindAndRun(const AExeName : String) : Boolean;
  begin
    if FileExists(AExeName) then begin
      Result := True;
      WinExec(PChar(AExeName), SW_SHOW);
    end else
      Result := False;
  end;

  procedure FindAndRunSetup;
  var
    ThePath : String;
  begin
    ThePath := AddBackSlashS(JustPathNameS(Application.ExeName));
    if FindAndRun(ThePath + 'AlimSetup.Exe') then
      Exit;
    if FindAndRun(ThePath + 'Setup.Exe') then
      Exit;
  end;

const
  PrimaryRegistryKey = 'SOFTWARE\ISL Software Corporation\The Alim';
var
  Registry : TStRegIni;
  TheApp : String;
begin
  Application.Initialize;
  Registry := TStRegIni.Create(RIMachine, False);
  if Registry.KeyExists(PrimaryRegistryKey) then begin
    Registry.CurSubKey := PrimaryRegistryKey;
    TheApp := Registry.ReadString('Application', '');
    if TheApp <> '' then
      WinExec(PChar(TheApp), SW_SHOW)
    else
      FindAndRunSetup;
  end else
    FindAndRunSetup;
  Application.Run;
end.
