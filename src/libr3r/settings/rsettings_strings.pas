unit RSettings_Strings;

interface

var
  DescMsg: String;
  DescDisplay: String;
  DescHide: String;
  DescHideItems: String;
  DescCacheExpiry: String;
  DescGuess: String;
  DescCheck: String;
  DescWarn: String;
  DescFilters: String;
{$IFDEF USE_ICONV}
  DescEncoding: String;
{$ENDIF}
  LoadSubscriptions: String;
  DescProxy: String;
  DescProxyAddress: String;
  DescProxyPort: String;
  DescUseTypes: String;
  DescTypes: String;
  DescUseLang: String;
  DescLang: String;
  DescBrowser: String;
  DescMail: String;
  DescMedia: String;
  DescPrefix: String;

{$IFNDEF USE_NLS}
function _(s: String): String;
{$ENDIF}

implementation

{$IFDEF USE_NLS}
uses
  LibIntl, RSettings_Routines, RStrings, SysUtils;
{$ENDIF}

procedure InitStrings;
begin
  DescMsg := _('Show messages');
  DescDisplay := _('Show feed titles only');
  DescHide := _('Hide cached feeds');
  DescHideItems := _('Hide cached feed items');
  DescCacheExpiry := _('Keep cached feeds for these amount of days');
  DescGuess := _('Guess the type of feed');
  DescCheck := _('Check for updates');
  DescWarn := _('Warn about missing data');
  DescFilters := _('Enable item filters');
{$IFDEF USE_ICONV}
  DescEncoding := _('Display character encoding');
{$ENDIF}
  LoadSubscriptions := _('Load subscriptions on startup');
  DescProxy := _('Use a proxy?');
  DescProxyAddress := _('Proxy address');
  DescProxyPort := _('Proxy port');
  DescUseTypes := _('Use custom accept types?');
  DescTypes := _('Custom accept types');
  DescUSeLang := _('Use custom accept languages?');
  DescLang := _('Custom accept languages');
  DescBrowser := _('Browser');
  DescMail := _('Email client');
  DescMedia := _('Media player');
  DescPrefix := _('Where the program is installed');
end;

{$IFNDEF USE_NLS}
function _(s: String): String;
begin
  _ := s;
end;
{$ENDIF}

initialization

{$IFDEF USE_NLS}
setlocale(LC_ALL, '');
textdomain('libr3r');
bindtextdomain('libr3r', StrToPchar(GetInstalledPrefix + PathDelim + 'share' + PathDelim + 'locale'));
{$ENDIF}

InitStrings;

end.
