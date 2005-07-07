unit LibR3R_Rs;

interface

resourcestring
  ErrorGetting = 'ERROR:  Couldn''t retrieve the feed.';

implementation

{$IFNDEF NO_GETTEXT}

uses
  GetText, SysUtils;

initialization

TranslateResourcestrings(ExtractFilePath(ParamStr(0)) + '../share/locale/%s/LC_MESSAGES/libr3r.mo');

{$ENDIF}

end.
