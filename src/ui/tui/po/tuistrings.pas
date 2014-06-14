unit TuiStrings;

interface

var
  Feed: String;
  GoUrl: String;
  Options: String;
  Donate: String;
  Quit: String;
  Help: String;

  ItemNo: String;
  OutOf: String;

  ItemTitle: String;
  ItemDesc: String;
  ItemSubject: String;
  ItemCreated: String;
  ItemEmail: String;
  ItemEncl: String;

  InvalidNumber: String;

  ErrorError: String;
  ErrorWarning: String;

  OptionDesc: String;
  OptionVal: String;

  SettingToChange: String;
  SettingToChangeReadLine: String;
  NewValue: String;

  ActivateLink: String;
  NextPrevLink: String;

  NumSym: String;

  FalseString: String;
  TrueString: String;

{$IFDEF USE_ICONV}
  DescEnc: String;
{$ENDIF}
  ErrorSeconds: String;
  SkinToUse: String;

  Usage: String;

  NoFeedURL: String;

  Retrieving: String;
  ShowIncomingItems: String;
  WrapDescriptions: String;

  NoData: String;

{$IFNDEF USE_NLS}
function _(s: String): String;
{$ENDIF}

implementation

{$IFDEF USE_NLS}
uses
  LibIntl, LibR3R, RSettings_Routines, RStrings, SysUtils;
{$ENDIF}

procedure InitStrings;
begin
  Feed := _('Feed Address: ');
  GoUrl :=_('%s:go to a URL');
  Options := _('%s:options');
  Donate := _('%s:donate');
  Quit := _('%s:quit the program');
  Help := _('%s:help');

  ItemNo := _('Item number: ');
  OutOf := _('/');

  ItemTitle := _('Title: ');
  ItemDesc := _('Description: ');
  ItemSubject := _('Subject: ');
  ItemCreated := _('Created: ');
  ItemEmail := _('Email address: ');
  ItemEncl := _('Enclosure: ');

  InvalidNumber := _('Invalid Number!');

  ErrorError := _('Error');
  ErrorWarning := _('Warning');

  OptionDesc := _('Option Description');
  OptionVal := _('Option Value');

  SettingToChange := _('Option to change (number or empty to exit): ');
  SettingToChangeReadLine := _('Option to change (select or enter a number; 0 exits): ');
  NewValue := _('New Value: ');

  ActivateLink := _(' (Press enter to open in your web browser)');
  NextPrevLink := _(' (Press %1 for the next item or %2 for the previous item)');

  NumSym := _('#');

  FalseString := _('FALSE');
  TrueString := _('TRUE');

{$IFDEF USE_ICONV}
  DescEnc := _('Display character encoding');
{$ENDIF}
  ErrorSeconds := _('Seconds to wait after an error');
  SkinToUse := _('Skin to use');

  Usage := _('Usage: %s [URL]');

  NoFeedURL := _('No feed URL specified.');

  Retrieving := _('Retrieving items from %s...');
  ShowIncomingItems := _('Show incoming items');
  WrapDescriptions := _('Wrap descriptions');

  NoData := _('This item contains no data.');
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
textdomain('r3r_tui');
bindtextdomain('r3r_tui', StrToPChar(GetInstalledPrefix + PathDelim + 'share' + PathDelim + 'locale'));
{$ENDIF}

InitStrings;

end.
