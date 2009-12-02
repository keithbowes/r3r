unit TvStrings;

{$codepage utf8}

interface

uses
  Drivers;

resourcestring
  FileMenu = '~F~ile';
  OpenItem = '~O~pen';
  OpenShortcut = 'F2';
  OpenKey = 'kbF2';

  Quit = 'E~x~it';
  QuitShortcut = 'Esc';
  QuitKey = 'kbEsc';

  Tools = '~T~ools';
  Settings = '~S~ettings';
  Feeds = '~F~eeds';
  FeedsShortcut = 'Ctrl-F';
  FeedsKey = 'kbCtrlF';

  HTTP = '~H~TTP';
  HTTPShortcut = 'Ctrl-H';
  HTTPKey = 'kbCtrlH';

  Programs = '~P~rograms';
  ProgramsShortcut = 'Ctrl-P';
  ProgramsKey = 'kbCtrlP';

  Help = '~H~elp';
  About = '~A~bout';
  AboutShortcut = 'F1';
  AboutKey = 'kbF1';

  OpenFile = 'Open File';
  TheFileName = '~F~ile Name';

  Go = 'Go';

function Str2Key(const Str: String): word;

implementation

{$IFNDEF NO_GETTEXT}

uses
  GetText, SysUtils;

function Str2Key(const Str: String): word;
var
  Key: word;
begin
  if Str = 'kbEsc' then Key := kbEsc;
  if Str = 'kbF1' then Key := kbF1;
  if Str = 'kbF2' then Key := kbF2;
  if Str = 'kbCtrlF' then Key := kbCtrlF;
  if Str = 'kbCtrlH' then Key := kbCtrlH;
  if Str = 'KbCtrlP' then Key := kbCtrlP;

  if Key = kbNoKey then
  begin
    WriteLn('The conversion from ', Str, ' to a key must be implemented in the Str2Key function.');
  end;
end;

initialization

TranslateResourceStrings(ExtractFilePath(ParamStr(0)) + '../share/locale/%s/LC_MESSAGES/r3r_tv.mo');


{$ENDIF}

end.
