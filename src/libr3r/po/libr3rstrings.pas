unit LibR3RStrings;

{$codepage utf8}
{$X+}

interface

var
  ErrorGetting: String;
  InvalidHeaders: String;
  MissingData: String;
  UnsupportedCompression: String;

  Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec: String;

implementation

uses
  LibIntl, RSettings_Routines, RStrings, SysUtils;

procedure InitStrings;
begin
  ErrorGetting := _('Couldn''t retrieve the feed.');
  InvalidHeaders := _('The web server doesn''t exist or sent invalid information.');
  MissingData := _('There is missing data in this feed.  Some of it was recovered.');
  UnsupportedCompression := _('Unsupported compression scheme.');

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
  Dec := _('Dec');
end;

initialization

setlocale(LC_ALL, '');
textdomain('libr3r');
bindtextdomain('libr3r', StrToPChar(GetInstalledPrefix + PathDelim + 'share' + PathDelim + 'locale'));

InitStrings;

end.
