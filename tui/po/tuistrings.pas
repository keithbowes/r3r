unit TuiStrings;

interface

resourcestring
  Feed = 'Feed Address: ';
  GoUrl = 'g)o to a URL';
  AboutItem = 'a)bout item number';
  Help = 'h)elp, this screen';
  Quit = 'q)uit the program';

  AboutKey = 'a';
  GoKey = 'g';
  HelpKey = 'h';
  QuitKey = 'q';

  ItemNo = 'Item number: ';

  ItemSubject = 'Subject: ';
  ItemCreated = 'Created: ';
  ItemDesc = 'Description: ';
  ItemLink = 'Link %d: %s';

  InvalidNumber = 'Invalid Number!';

implementation

{$IFNDEF NO_GETTEXT}

uses
  GetText, SysUtils;

initialization

TranslateResourceStrings(ExtractFilePath(ParamStr(0)) + '../share/locale/%s/LC_MESSAGES/r3r_tui.mo');


{$ENDIF}

end.
