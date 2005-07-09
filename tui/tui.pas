unit Tui;

interface

uses
  Keyboard, LibR3R;

type
  TTui = class
  private
    procedure GetFeed(const Feed: String);
  protected
    procedure ItemParsed(Item: TParsedFeedItem);
    procedure MessageReceived(Sender: TObject; Error: Boolean; MessageName: String);
    procedure ShowHelp;
    procedure GoURI;
  public
    constructor Create;
  end;

implementation

uses
  Info, Tui_Rs, SysUtils;

constructor TTui.Create;
var
  FeedIndex: word;
  Key: TKeyEvent;
  KeyChar: char;
begin
  inherited Create;

  for FeedIndex := 1 to ParamCount do
  begin
    GetFeed(ParamStr(FeedIndex));
  end;

  ShowHelp;

  repeat
    InitKeyboard;

    Key := GetKeyEvent;
    Key := TranslateKeyEvent(Key);
    KeyChar := GetKeyEventChar(Key);
    DoneKeyboard;
    case Pos(KeyChar, GoKey + HelpKey) of
      1:
      begin
        GoURI;
      end;
      2:
      begin
        ShowHelp;
      end;
    end;
  until KeyChar = QuitKey;
end;

procedure TTui.ItemParsed(Item: TParsedFeedItem);
begin
  Write(Item.Title);
  if Item.Links.Count > 0 then
  begin
    Write(' <', Item.Links[0], '>');
  end;

  WriteLn;
end;

procedure TTui.MessageReceived(Sender: TObject; Error: Boolean; MessageName: String);
begin
  WriteLn(MessageName);
end;

procedure TTui.ShowHelp;
var
  InfoLine: String;
begin
  InfoLine := 'R3R ' + Version + ' (' + Os + ')';
  WriteLn(InfoLine:75);

  Write(GoURL + #9);
  Write(Help + #9);
  Write(Quit);
  WriteLn;
end;

procedure TTui.GoURI;
var
  URI: String;
begin
  Write(Feed);
  ReadLn(URI);

  GetFeed(URI);
  ShowHelp;
end;

procedure TTui.GetFeed(const Feed: String);
var
  Lib: TLibR3R;
begin
  Lib := TLibR3R.Create(Feed);
  Lib.OnItemParsed := @ItemParsed;
  Lib.OnMessage := @MessageReceived;
  Lib.Parse;
  FreeAndNil(Lib);
end;

end.
