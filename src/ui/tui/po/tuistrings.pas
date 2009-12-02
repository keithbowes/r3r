unit TuiStrings;

{$codepage utf8}

interface

resourcestring
  Feed = 'Feed Address: ';
  GoUrl = 'g)o to a URL';
  AboutItem = 'a)bout item number';
  OpenLink = 'open l)ink';
  Options = 'o)ptions';
  Donate = 'd)onate';
  Quit = 'q)uit the program';

  AboutKey = 'a';
  OpenKey = 'l';
  GoKey = 'g';
  OptionsKey = 'o';
  DonateKey = 'd';
  QuitKey = 'q';

  ItemNo = 'Item number: ';

  ItemTitle = 'Title: ';
  ItemSubject = 'Subject: ';
  ItemCreated = 'Created: ';
  ItemDesc = 'Description: ';
  ItemLink = 'Link %d: %s';

  InvalidNumber = 'Invalid Number!';

  LinkNo = 'Link number: ';
  
  ErrorError = 'Error';
  ErrorWarning = 'Warning';

  OptionName = 'Option Name';
  OptionVal = 'Option Value';

  SettingToChange = 'Option to Change: ';
  NewValue = 'New Value: ';

implementation

{$IFNDEF NO_GETTEXT}

uses
  GetText, SysUtils;

initialization

TranslateResourceStrings(ExtractFilePath(ParamStr(0)) + '../share/locale/%s/LC_MESSAGES/r3r_tui.mo');


{$ENDIF}

end.
