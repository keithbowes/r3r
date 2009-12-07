unit LibR3R;

interface

uses
  FeedItem, RMessage, RSettings, RSock, RSubscriptions;

const
  SettingsRead = RSettings.SettingsRead;
  SettingsWrite = RSettings.SettingsWrite;

  TypeNone = RSettings.TypeNone;
  TypeString = RSettings.TypeString;
  TypeInteger = RSettings.TypeInteger;
  TypeBoolean = RSettings.TypeBoolean;

type
  TFeedItem = FeedItem.TFeedItem;
  TParsedEvent = procedure(Item: TFeedItem) of object;

  TLibR3R = class
  private
    FFreeLinks: Boolean;
    FOnItemParsed: TParsedEvent;
    FOnMessage: TRMessage;
    FSock: TRSock;
  protected
    procedure DisplayItem(const Item: TFeedItem); virtual;
    procedure HandleMessage(Sender: TObject; IsError: Boolean; MessageName, Extra: String); virtual;
    procedure DoParseItem(Item: TFeedItem);
  public
    constructor Create; overload;
    constructor Create(const Resource: String); overload;
    destructor Destroy; override;
    procedure RetrieveFeed(Resource: String; AlsoParse: Boolean = true);
    procedure Parse;
    property FreeLinks: Boolean read FFreeLinks write FFreeLinks;
    property OnItemParsed: TParsedEvent write FOnItemParsed;
    property OnMessage: TRMessage write FOnMessage;
  end;

  TRSettingsRec = RSettings.TRSettingsRec;

var
  Settings: TRSettings;
  Subscriptions: PRSubscriptions;

implementation

uses
  Classes, Http, LibR3RStrings, LocalFile, RGetFeed;

constructor TLibR3R.Create;
begin
  inherited Create;
  FFreeLinks := true;
  FSock := nil;
end;

constructor TLibR3R.Create(const Resource: String);
begin
  Create;
  RetrieveFeed(Resource, false);
end;

destructor TLibR3R.Destroy;
begin
  inherited Destroy;
end;

procedure TLibR3R.RetrieveFeed(Resource: String; AlsoParse: Boolean = true);
var
  Prot, User, Pass, Host, Port, Path, Para: String;
begin
  GetFeed(Resource, Prot, Host, Port, Path, Para);

  if Prot = 'file' then
  begin
    FSock := TLocalFile.Create(Resource);
  end
  else if Prot = 'http' then
  begin
    FSock := THttpSock.Create(Host, Port, Path, Para);
  end
  else
  begin
    Exit;
  end;

  FSock.Execute;

  if AlsoParse then
  begin
    Parse;
    if Assigned(FSock) then
    begin
      FSock.Free;
    end;
  end;
end;

procedure TLibR3R.Parse;
begin
  if Assigned(FOnMessage) then
  begin
    SetMessageEvent(FOnMessage);
  end
  else
  begin
    SetMessageEvent(@HandleMessage);
  end;

  ParseFeed(Self, @DoParseItem, FSock);
end;

procedure TLibR3R.DisplayItem(const Item: TFeedItem);
begin
end;

procedure TLibR3R.DoParseItem(Item: TFeedItem);
begin
  if Assigned(FOnItemParsed) then
  begin
    FOnItemParsed(Item);
  end
  else
  begin
    DisplayItem(Item)
  end;
end;

procedure TLibR3R.HandleMessage(Sender: TObject; IsError: Boolean; MessageName, Extra: String);
begin
end;

initialization

Settings := RSettings.Settings;
Subscriptions := RSubscriptions.Subscriptions;

end.
