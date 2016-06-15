unit RKeys;

{$INCLUDE "tuidefs.inc"}

interface

{$IFDEF USE_NCRT}
uses
  nCurses;
{$ENDIF}

const
  AddKey = 'a';
  AbortKey = 'z';
  DonateKey = 'd';
  DownKey = 'j';
  EmailKey = 'c';
  EnclosureKey = 'x';
  EnterKey = Chr(13);
  GoKey = 'g';
  HelpKey = 'h';
  HelpSymKey = '?';
  HomeEndKey = '~'; // For some reason, FPC reports both HOME and END as a tilde
  NullKey = Chr(0);
  OptionsKey = 'o';
  QuitKey = 'q';
  RefreshKey = 'u';
  RightKey = 'l';
  ScrollDownKey = Chr(32);
  ScrollUpKey = 'b';
  SearchKey = '/';
  ShellKey = '!';
  SubScriptionsKey = '-';
  UpKey = 'k';

{ For differences between ncurses's getch and CRT's ReadKey }
{$IFNDEF USE_NCRT}
  DownArrow = Chr(80);
  EndKey = Chr(79);
  HomeKey = Chr(71);
  PageDownKey = Chr(81);
  PageUpKey = Chr(73);
  RightArrow = Chr(77);
  UpArrow = Chr(72);
{$ELSE}
  DownArrow = Chr(byte(KEY_DOWN));
  EndKey = WideChar(KEY_END);
  HomeKey = Chr(byte(KEY_HOME));
  PageDownKey = Chr(byte(KEY_NPAGE));
  PageUpKey = Chr(byte(KEY_PPAGE));
  RightArrow = Chr(byte(KEY_RIGHT));
  UpArrow = Chr(byte(KEY_UP));
{$ENDIF}

function GetBoundKey(const Key: char): char;

implementation

uses
  RSettings_Routines, SysUtils;

{ Get the key binding }
function GetBoundKey(const Key: char): char;
var
  bindfile: String;
  f: text;
  Res: char;
  s: String;
begin
  bindfile := GetDataDir + 'clav';
  Res := Key;

  if FileExists(bindfile) then
  begin
    Assign(f, bindfile);
    Reset(f);

    while not Eof(f) do
    begin
      ReadLn(f, s);
      if s[1] = Key then
      begin
        Res := s[3];
      end;
    end;

    Close(f);
  end;

  GetBoundKey := Res;
end;

end.
