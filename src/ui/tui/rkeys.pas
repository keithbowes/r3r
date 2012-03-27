unit RKeys;

interface

const
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
  UpArrow = Chr(72);
  UpKey = 'k';

function TranslateControl(const Key: char): char;

implementation

{ Convert a Ctrl+key to a key }
function TranslateControl(const Key: char): char;
begin
  TranslateControl := char(ord(Key) + 96);
end;

end.