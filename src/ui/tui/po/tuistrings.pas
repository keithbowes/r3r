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
  ItemDesc: String;
  ItemEmail: String;
  ItemEncl: String;
  ItemLink: String;

  InvalidNumber: String;

  ErrorError: String;
  ErrorWarning: String;

  OptionDesc: String;
  OptionVal: String;

  SettingToChange: String;
  NewValue: String;

  UpdateAvailable: String;

  ActivateLink: String;
  NextPrevLink: String;

  NumSym: String;

  FalseString: String;
  TrueString: String;

  SkinToUse: String;

implementation

uses
  LibIntl, LibR3R, RSettings_Routines, RStrings, SysUtils;

procedure InitStrings;
begin
  Feed := _('Feed Address: ');
  GoUrl :=_('g:go to a URL');
  Options := _('o:options');
  Donate := _('d:donate');
  Quit := _('q:quit the program');
  Help := _('?:help');

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

  OptionDesc := _('Option Description');
  OptionVal := _('Option Value');

  SettingToChange := _('Option to change (number or empty to exit): ');
  NewValue := _('New Value: ');

  UpdateAvailable := _('A new version is available from http://sourceforge.net/projects/r3r');

  ActivateLink := _(' (Press enter to open in your web browser)');
  NextPrevLink := _(' (Press j for the next item or k for the previous item)');

  NumSym := _('#');

  FalseString := _('FALSE');
  TrueString := _('TRUE');

  SkinToUse := _('Skin to use');
end;

initialization

setlocale(LC_ALL, '');
textdomain('r3r_tui');
bindtextdomain('r3r_tui', StrToPChar(GetInstalledPrefix + PathDelim + 'share' + PathDelim + 'locale'));

InitStrings;

end.
