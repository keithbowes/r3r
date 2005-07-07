unit Tui_Rs;

interface

resourcestring
  Feed = 'Feed Address: ';
  GoUrl = 'g)o to a URL';
  Help = 'h)elp, this screen';
  Quit = 'q)uit the program';

  GoKey = 'g';
  HelpKey = 'h';
  QuitKey = 'q';

implementation

{$IFNDEF NO_GETTEXT}

uses
  GetText, SysUtils;

initialization

TranslateResourcestrings(ExtractFilePath(ParamStr(0)) + '../share/locale/%s/LC_MESSAGES/r3r_tui.mo');

{$ENDIF}

end.
