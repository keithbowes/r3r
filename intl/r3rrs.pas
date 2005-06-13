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

  GetError = 'An error occurred while trying to retrieve the feed.';

implementation

{$IFNDEF NO_GETTEXT}

uses
  GetText, SysUtils;

initialization

TranslateResourcestrings(ExtractFilePath(ParamStr(0)) + 'intl/r3r.%s.mo');

{$ENDIF}

end.
