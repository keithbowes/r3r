unit RKeys;

interface

const
  AddKey = 'a';
  DonateKey = 'd';
  DownArrow = Chr(80);
  DownKey = 'j';
  EmailKey = 'c';
  EnclosureKey = 'x';
  EndKey = Chr(79);
  EnterKey = Chr(13);
  GoKey = 'g';
  HelpKey = 'h';
  HelpSymKey = '?';
  HomeEndKey = '~'; // For some reason, FPC reports both HOME and END as a tilde
  HomeKey = Chr(71);
  NullKey = Chr(0);
  OptionsKey = 'o';
  PageDownKey = Chr(81);
  PageUpKey = Chr(73);
  QuitKey = 'q';
  RefreshKey = 'l';
  RightArrow = Chr(77);
  RightKey = 'l';
  ScrollDownKey = Chr(32);
  ScrollUpKey = Chr(8);
  SearchKey = '/';
  ShellKey = '!';
  SubScriptionsKey = '-';
  UpArrow = Chr(72);
  UpKey = 'k';

function GetBoundKey(const Key: char): char;
function TranslateControl(const Key: char): char;

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
  bindfile := DataDir + 'clav';
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

{ Convert a Ctrl+key to a key }
function TranslateControl(const Key: char): char;
begin
  TranslateControl := char(ord(Key) + 96);
end;

end.
