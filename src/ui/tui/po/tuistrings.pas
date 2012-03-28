unit TuiStrings;

{$codepage utf8}
{$X+}

interface

var
  Feed: String;
  GoUrl: String;
  Options: String;
  Donate: String;
  Quit: String;
  Help: String;

  ItemNo: String;

  ItemTitle: String;
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

  UpdateAvailable: String;

  ActivateLink: String;
  NextPrevLink: String;

  NumSym: String;

  FalseString: String;
  TrueString: String;

  SkinToUse: String;

  DeprecatedCL: String;

  NoFeedURL: String;

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

  ItemTitle := _('Title: ');
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

  UpdateAvailable := _('A new version is available from http://sourceforge.net/projects/r3r');

  ActivateLink := _(' (Press enter to open in your web browser)');
  NextPrevLink := _(' (Press j for the next item or k for the previous item)');

  NumSym := _('#');

  FalseString := _('FALSE');
  TrueString := _('TRUE');

  SkinToUse := _('Skin to use');

  DeprecatedCL := 'The use of the command line is deprecated.  Please add feeds to which you want to subscribe to %s.';

  NoFeedURL := 'No feed URL specified.';
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
