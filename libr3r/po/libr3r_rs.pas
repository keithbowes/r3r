unit LibR3R_Rs;

interface

resourcestring
  ErrorGetting = 'ERROR:  Couldn''t retrieve the feed.';

implementation

{$IFNDEF NO_GETTEXT}

uses
  GetText, SysUtils;

var
  FallbackLang, Lang: String;
  Mo: TMoFile;

initialization

try
  Mo := TMOFile.Create(Format(ExtractFilePath(ParamStr(0)) + '../share/locale/%s/LC_MESSAGES/r3r_libr3r.%s.mo', [FallbackLang, FallbackLang]));
  try
    TranslateResourceStrings(Mo);
  finally
    Mo.Free;
  end;
except
end;

{$ENDIF}

end.
