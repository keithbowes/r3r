unit LibR3RStrings;

interface

resourcestring
  ErrorGetting = 'ERROR:  Couldn''t retrieve the feed.';

implementation

{$IFNDEF NO_GETTEXT}

uses
  GetText, SysUtils;

initialization

TranslateResourceStrings(ExtractFilePath(ParamStr(0)) + '../share/locale/%s/LC_MESSAGES/libr3r.mo');

{$ENDIF}

end.
