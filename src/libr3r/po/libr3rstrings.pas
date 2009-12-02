unit LibR3RStrings;

{$codepage utf8}

interface

resourcestring
  ErrorGetting = 'Couldn''t retrieve the feed.';
  InvalidHeaders = 'The web server doesn''t exist or sent invalid information.';

implementation

{$IFNDEF NO_GETTEXT}

uses
  GetText, SysUtils;

initialization

TranslateResourceStrings(ExtractFilePath(ParamStr(0)) + '../share/locale/%s/LC_MESSAGES/libr3r.mo');

{$ENDIF}

end.
