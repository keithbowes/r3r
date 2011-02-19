unit RSettings_Strings;

interface

var
  DescMsg: String;
  DescDisplay: String;
  DescHide: String;
  DescHideItems: String;
  DescGuess: String;
  DescCheck: String;
  DescWarn:String;
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

implementation

uses
  LibIntl, RSettings_Routines, RStrings, SysUtils;

procedure InitStrings;
begin
  DescMsg := _('Show messages');
  DescDisplay := _('Show feed titles only');
  DescHide := _('Hide cached feeds');
  DescHideItems := _('Hide cached feed items');
  DescGuess := _('Guess the type of feed');
  DescCheck := _('Check for updates');
  DescWarn := _('Warn about missing data');
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

initialization

setlocale(LC_ALL, '');
textdomain('libr3r');
bindtextdomain('libr3r', StrToPchar(GetInstalledPrefix + PathDelim + 'share' + PathDelim + 'locale'));

InitStrings;

end.
