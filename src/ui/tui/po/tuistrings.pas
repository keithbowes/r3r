unit TuiStrings;

{$codepage utf8}
{$X+}

interface

var
  Feed: String;
  GoUrl: String;
  AboutItem: String;
  OpenLink: String;
  Options: String;
  Donate: String;
  Quit: String;

  AboutKey: String;
  OpenKey: String;
  GoKey: String;
  OptionsKey: String;
  DonateKey: String;
  QuitKey: String;

  ItemNo: String;

  ItemTitle: String;
  ItemSubject: String;
  ItemCreated: String;
  ItemDesc: String;
  ItemEncl: String;
  ItemLink: String;

  InvalidNumber: String;

  LinkNo: String;
  
  ErrorError: String;
  ErrorWarning: String;

  OptionName: String;
  OptionVal: String;

  SettingToChange: String;
  NewValue: String;

  UpdateAvailable: String;

  ActivateLink: String;
  NextPrevLink: String;

implementation

uses
  LibIntl, LibR3R, RStrings;

procedure InitStrings;
begin
  Feed := _('Feed Address: ');
  GoUrl :=_('g)o to a URL');
  AboutItem :=_('a)bout item number');
  OpenLink :=_('open l)ink');
  Options := _('o)ptions');
  Donate := _('d)onate');
  Quit :=_('q)uit the program');

  AboutKey := _('a');
  OpenKey := _('l');
  GoKey := _('g');
  OptionsKey := _('o');
  DonateKey := _('d');
  QuitKey := _('q');

  ItemNo := _('Item number: ');

  ItemTitle := _('Title: ');
  ItemSubject := _('Subject: ');
  ItemCreated := _('Created: ');
  ItemDesc := _('Description: ');
  ItemEncl := _('Enclosure: ');
  ItemLink := _('Link %i: ');

  InvalidNumber := _('Invalid Number!');

  LinkNo := _('Link number: ');
  
  ErrorError := _('Error');
  ErrorWarning := _('Warning');

  OptionName := _('Option Name');
  OptionVal := _('Option Value');

  SettingToChange := _('Option to change (empty to exit): ');
  NewValue := _('New Value: ');

  UpdateAvailable := _('A new version is available from http://sourceforge.net/projects/r3r');

  ActivateLink := _(' (Press enter to open in your web browser)');
  NextPrevLink := _(' (Press j for the next item or k for the previous item)');
end;

initialization

setlocale(LC_ALL, '');
textdomain('r3r_tui');
bindtextdomain('r3r_tui', StrToPChar(Settings.GetString(Settings.IndexOf('installed-prefix')) + '/share/locale'));

InitStrings;

end.
