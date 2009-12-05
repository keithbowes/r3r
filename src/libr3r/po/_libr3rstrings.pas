unit LibR3RStrings;

{$codepage utf8}
{$X+}

interface

uses
  LibIntl;

var
  ErrorGetting: String;
  InvalidHeaders: String;

implementation

procedure InitStrings;
begin
  ErrorGetting := _('Couldn''t retrieve the feed.');
  InvalidHeaders := _('The web server doesn''t exist or sent invalid information.');
end;

initialization

setlocale(LC_ALL, '');
textdomain('libr3r');
bindtextdomain('libr3r', '@localedir@');

InitStrings;

end.
