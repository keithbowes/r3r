unit LibR3RStrings;

interface

var
  ErrorGetting: String;
  InvalidHeaders: String;
  TooManyRedirects: String;

  Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Decem: String;

  UnknownEncoding: String;
  NoData: String;
  SSLError: String;

  Subscription1, Subscription2, Subscription3, Subscription4: String;

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
  ErrorGetting := _('Couldn''t retrieve the feed.');
  InvalidHeaders := _('The web server doesn''t exist or sent invalid information.');
  TooManyRedirects := _('The redirect limit has been reached.');

  Jan := _('Jan');
  Feb := _('Feb');
  Mar := _('Mar');
  Apr := _('Apr');
  May := _('May');
  Jun := _('Jun');
  Jul := _('Jul');
  Aug := _('Aug');
  Sep := _('Sep');
  Oct := _('Oct');
  Nov := _('Nov');
  Decem := _('Dec');

  UnknownEncoding := _('Unknown Encoding');
  NoData := _('No Data');
  SSLError := _('Couldn''t initialize an SSL connection');

  Subscription1 := _('https://github.com/keithbowes/r3r/releases.atom');
  Subscription2 := _('http://feeds.bbci.co.uk/news/rss.xml');
  Subscription3 := _('http://rss.slashdot.org/Slashdot/slashdotatom');
  Subscription4 := _('http://www.reddit.com/.rss');
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
bindtextdomain('libr3r', StrToPChar(GetInstalledPrefix + PathDelim + 'share' + PathDelim + 'locale'));
{$ENDIF}

InitStrings;

end.
