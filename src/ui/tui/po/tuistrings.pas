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

  GoKey: String;
  OptionsKey: String;
  DonateKey: String;
  QuitKey: String;

  ItemNo: String;

  ItemTitle: String;
  ItemSubject: String;
  ItemCreated: String;
  ItemDesc: String;
  ItemEmail: String;
  ItemEncl: String;
  ItemLink: String;

  InvalidNumber: String;

  ErrorError: String;
  ErrorWarning: String;

  OptionName: String;
  OptionVal: String;

  SettingToChange: String;
  NewValue: String;

  UpdateAvailable: String;

  ActivateLink: String;
  NextPrevLink: String;

  NumSym: String;

  FalseString: String;
  TrueString: String;

implementation

uses
  LibIntl, LibR3R, RSettings_Routines, RStrings;

procedure InitStrings;
begin
  Feed := _('Feed Address: ');
  GoUrl :=_('g)o to a URL');
  Options := _('o)ptions');
  Donate := _('d)onate');
  Quit :=_('q)uit the program');

  GoKey := _('g');
  OptionsKey := _('o');
  DonateKey := _('d');
  QuitKey := _('q');

  ItemNo := _('Item number: ');

  ItemTitle := _('Title: ');
  ItemSubject := _('Subject: ');
  ItemCreated := _('Created: ');
  ItemDesc := _('Description: ');
  ItemEmail := _('Email address: ');
  ItemEncl := _('Enclosure: ');
  ItemLink := _('Link %i: ');

  InvalidNumber := _('Invalid Number!');

  ErrorError := _('Error');
  ErrorWarning := _('Warning');

  OptionName := _('Option Name');
  OptionVal := _('Option Value');

  SettingToChange := _('Option to change (number or empty to exit): ');
  NewValue := _('New Value: ');

  UpdateAvailable := _('A new version is available from http://sourceforge.net/projects/r3r');

  ActivateLink := _(' (Press enter to open in your web browser)');
  NextPrevLink := _(' (Press j for the next item or k for the previous item)');

  NumSym := _('#');

  FalseString := _('FALSE');
  TrueString := _('TRUE');
end;

initialization

setlocale(LC_ALL, '');
textdomain('r3r_tui');
bindtextdomain('r3r_tui', StrToPChar(GetInstalledPrefix + '/share/locale'));

InitStrings;

end.
