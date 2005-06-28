unit Tui;

interface

uses
  Keyboard, LibR3R;

type
  TTui = class
  private
    FTLib: TLibR3R;
  protected
    procedure ItemParsed(Item: TParsedFeedItem);
    procedure MessageReceived(Sender: TObject; Error: Boolean; MessageName: String);
    procedure ShowHelp;
    procedure GoURI;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  R3RRs, SysUtils;

constructor TTui.Create;
var
  FeedIndex: word;
  Key: TKeyEvent;
  KeyChar: char;
begin
  inherited Create;

  for FeedIndex := 1 to ParamCount do
  begin
    FTLib := TLibR3R.Create(ParamStr(FeedIndex));
    FTLib.OnItemParsed := @ItemParsed;
    FTLib.OnMessage := @MessageReceived;
    FTLib.Parse;
    FreeAndNil(FTLib);
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

destructor TTui.Destroy;
begin
  if Assigned(FTLib) then
  begin
    FTLib.Free;
  end;

  inherited Destroy;
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
begin
  Write(GoURL + #9);
  Write(Help + #9);
  Write(Quit);
  WriteLn;
end;

procedure TTui.GoURI;
var
  URI: String;
begin
  if Assigned(FTLib) then
  begin
    FTLib.Free;
  end;

  Write(Feed);
  ReadLn(URI);

  FTLib := TLibR3R.Create(URI);

  with FTLib do
  begin
    OnItemParsed := @ItemParsed;
    Parse;
  end;

  ShowHelp;
end;

end.
