program alim;

uses
  Forms,
  SysUtils,
  Dialogs,
  Main in 'Main.pas' {MainForm},
  QuranStruct in 'QuranStruct.pas',
  ArticleView in 'ArticleView.pas' {ArticleViewer},
  IslUtils in 'IslUtils.pas',
  HadithView in 'HadithView.pas' {HadithViewer},
  AmlView in 'AmlView.pas',
  QuranView in 'QuranView.pas' {QuranViewer},
  BooksView in 'BooksView.pas' {InstalledBooksForm},
  SuraWin in 'SuraWin.pas' {SuraListForm},
  MultiQuranView in 'MultiQuranView.pas' {MultiQuranViewer},
  QuranImgView in 'QuranImgView.pas' {QuranImgViewer},
  TextView in 'TextView.pas' {TextViewer},
  IndexStruct in 'IndexStruct.pas',
  ReciteView in 'ReciteView.pas' {RecitationViewer},
  WebView in 'WebView.pas' {WebViewer},
  HomeView in 'HomeView.pas' {HomeViewer},
  ArabicKeyboardForm in 'ArabicKeyboardForm.pas' {ArabicKeyboard},
  MultiIndexView in 'MultiIndexView.pas',
  BookmarksView in 'BookmarksView.pas' {BookmarksViewer},
  Arabic in 'Arabic.pas',
  SearchExpr in 'SearchExpr.pas',
  ConcordView in 'ConcordView.pas' {ConcordanceViewer},
  SearchView in 'SearchView.pas' {SearchViewer},
  MediaPlayerWin in 'MediaPlayerWin.pas' {MediaPlayerForm},
  AyahComposer in 'AyahComposer.pas' {AyahSelectionForm},
  AuthenticForm in 'AuthenticForm.pas' {AuthenticDlg},
  MessageWin in 'MessageWin.pas' {MessageWindow},
  RemindersDlg in 'RemindersDlg.pas' {RemindersDialog},
  ShopCart in 'ShopCart.pas' {ShoppingCart};

{$R *.RES}

begin
  Application.Initialize;

  Application.Title := 'The Alim';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TInstalledBooksForm, InstalledBooksForm);
  Application.CreateForm(TSuraListForm, SuraListForm);
  Application.CreateForm(TArabicKeyboard, ArabicKeyboard);
  Application.CreateForm(TAyahSelectionForm, AyahSelectionForm);
  Application.CreateForm(TMessageWindow, MessageWindow);
  Application.CreateForm(TRemindersDialog, RemindersDialog);
  Application.CreateForm(TShoppingCart, ShoppingCart);
  SuraListForm.AddressMgr := MainForm;

  Application.Run;
end.
