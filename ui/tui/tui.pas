unit Tui;

interface

uses
  LibR3R;

type
  TTui = class
  private
    FItems: array of TFeedItem;
    procedure GetFeed(const Feed: String);
  protected
    procedure ItemParsed(Item: TFeedItem);
    procedure MessageReceived(Sender: TObject; Error: Boolean; MessageName: String);
    procedure ShowHelp;
    procedure GoURI;
    procedure GoItem;
  public
    constructor Create;
  end;

implementation

uses
  Info, Keyboard, TuiStrings, SysUtils;

const
  Tab = #9;

constructor TTui.Create;
var
  FeedIndex: word;
  Key: TKeyEvent;
  KeyChar: char;
begin
  inherited Create;
  SetLength(FItems, 1);

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
    case Pos(KeyChar, GoKey + HelpKey + AboutKey) of
      1:
      begin
        GoURI;
      end;
      2:
      begin
        ShowHelp;
      end;
      3:
      begin
        GoItem;
      end;
    end;
  until KeyChar = QuitKey;
end;

procedure TTui.ItemParsed(Item: TFeedItem);
var
  Items: cardinal;
begin
  Items := Length(FItems);
  SetLength(FItems, Items + 1);
  FItems[Items] := Item;
  FItems[Items].Links := Item.Links;
  Write(Items, ': ', Item.Title);
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

  Write(GoURL + Tab);
  Write(Help + Tab);
  WriteLn;
  if Length(FItems) > 1 then
  begin
    Write(AboutItem + Tab);
  end;
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

procedure TTui.GoItem;
var
  LinkIndex: cardinal;
  No: String;
begin
  Write(ItemNo);
  ReadLn(No);

  try
    with FItems[StrToInt(No)] do
    begin
      WriteLn(ItemSubject, Subject);
      WriteLn(ItemCreated, Created);
      WriteLn(ItemDesc, Copy(Description, 0, 75) + '...');

      LinksCount := Length(Links);
      if LinksCount > 0 then
      begin
        for LinkIndex := 0 to LinksCount - 1 do
        begin
          WriteLn(Format(ItemLink, [LinkIndex + 1, Links[LinkIndex]]));
        end;
      end;
    end;
  except
    WriteLn(InvalidNumber);
  end;

  ShowHelp;
end;

procedure TTui.GetFeed(const Feed: String);
var
  Lib: TLibR3R;
begin
  Lib := TLibR3R.Create(Feed);
  Lib.FreeLinks := false;
  Lib.OnItemParsed := @ItemParsed;
  Lib.OnMessage := @MessageReceived;
  Lib.Parse;
  FreeAndNil(Lib);
end;

end.
