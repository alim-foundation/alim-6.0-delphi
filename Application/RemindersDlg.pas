unit RemindersDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Htmlview, StdCtrls, ExtCtrls, Db, DBISAMTb;

type
  TRemindersDialog = class(TForm)
    Panel1: TPanel;
    DontRemindChkBox: TCheckBox;
    CloseButton: TButton;
    HtmlViewer: THTMLViewer;
    tblReminders: TDBISAMTable;
    procedure HtmlViewerHotSpotClick(Sender: TObject; const SRC: String;
      var Handled: Boolean);
  private
    { Private declarations }
  public
  end;

var
  RemindersDialog: TRemindersDialog;

procedure CheckForReminders(const RunCount : Integer; const ARemindersPath : String);

implementation

{$R *.DFM}

uses StStrS, Main;

procedure CheckForReminders(const RunCount : Integer; const ARemindersPath : String);
var
  FirstShowCount, LastShowCount, RecurShowCount : Integer;
  Show : Boolean;
  Reminder : String;
begin
  with RemindersDialog.tblReminders do begin
    try
      DatabaseName := ARemindersPath;
      TableName := 'Reminders';
      Open;
    except
      exit;
    end;

    while not EOF do begin
      if (FieldValues['LastShownCount'] > RunCount) then begin
        Edit;
        FieldValues['LastShownCount'] := 0;
        Post;
      end;

      FirstShowCount := FieldValues['FirstShowCount'];
      RecurShowCount := FieldValues['RecurShowCount'];
      LastShowCount := FieldValues['LastShownCount'];

      if (LastShowCount > 0) then
        Show := (RunCount - LastShowCount > RecurShowCount)
      else
        Show := (RunCount >= FirstShowCount);

      if Show and FieldValues['Show'] then begin
        Reminder := FieldValues['Reminder'];
        RemindersDialog.HtmlViewer.LoadFromBuffer(PChar(Reminder), Length(Reminder));
        RemindersDialog.DontRemindChkBox.Checked := not FieldValues['Show'];
        RemindersDialog.DontRemindChkBox.Enabled := FieldValues['AllowHide'];

        RemindersDialog.ShowModal;

        Edit;
        if RunCount = FirstShowCount then
          FieldValues['FirstShownOn'] := Date;
        FieldValues['LastShownOn'] := Date;
        FieldValues['LastShownCount'] := RunCount;
        FieldValues['Show'] := not RemindersDialog.DontRemindChkBox.Checked;
        Post;
      end;

      Next;
    end;
  end;
end;

procedure TRemindersDialog.HtmlViewerHotSpotClick(Sender: TObject;
  const SRC: String; var Handled: Boolean);
begin
  MainForm.SetAddress(SRC);
  ModalResult := mrOk;
end;

end.
