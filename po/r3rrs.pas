unit R3RRs;

interface

resourcestring
  Feed = 'Feed Address: ';
  GoUrl = 'g)o to a URL';
  Help = 'h)elp, this screen';
  Quit = 'q)uit the program';

  GoKey = 'g';
  HelpKey = 'h';
  QuitKey = 'q';

  ErrorGetting = 'ERROR:  Couldn''t retrieve the feed.';

implementation

{$IFNDEF NO_GETTEXT}

uses
  GetText, SysUtils;

initialization

TranslateResourcestrings(ExtractFilePath(ParamStr(0)) + '../share/locale/%s/LC_MESSAGES/r3r.mo');

{$ENDIF}

end.
