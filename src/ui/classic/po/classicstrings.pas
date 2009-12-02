unit ClassicStrings;

{$codepage utf8}

interface

resourcestring
  { menus }
  FileMnuCap = '&File';
    OpenItmCap = '&Open';
    CloseItmCap = '&Close';
  ToolsMnuCap = '&Tools';
    SettingsItmCap = '&Settings...';
  HelpMnuCap = '&Help';
    InfoItmCap = '&Info...';

  { list view }
  NameCol = 'Feed Name';
  TitleCol = 'Title';
  SubjCol = 'Subject';
  CreatedCol = 'Created';

  EmptyField = '[None]';

  { URI Bar }
  Go = 'Go';

implementation

{$IFNDEF NO_GETTEXT}

uses
  GetText, SysUtils;

initialization

TranslateResourceStrings(ExtractFilePath(ParamStr(0)) + '../share/locale/%s/LC_MESSAGES/r3r_classic.mo');


{$ENDIF}

end.
