unit LibR3R;

interface

uses
{$IFDEF UNIX}
  CThreads,
{$ENDIF}
  FeedItem, RMessage, RSettings, RSock;

type
  TParsedFeedItem = TFeedItem;
  TParsedEvent = procedure(Item: TParsedFeedItem) of object;

  TLibR3R = class
  private
    FOnItemParsed: TParsedEvent;
    FOnMessage: TRMessage;
    FSettings: TRSettings;
    FSock: TRSock;
  protected
    procedure DoParseItem;
  public
    constructor Create(const Resource: String);
    destructor Destroy; override;
    procedure Parse;
    property OnItemParsed: TParsedEvent write FOnItemParsed;
    property OnMessage: TRMessage write FOnMessage;
    property Settings: TRSettings read FSettings write FSettings;
  end;

implementation

uses
  Classes, Http, LocalFile, R3RRs, SynaUtil, SysUtils;

var
  Item: TFeedItem;

constructor TLibR3R.Create(const Resource: String);
var
  Prot, User, Pass, Host, Port, Path, Para: String;
begin
  inherited Create;
  FSettings := TRSettings.Create;
  FSock := nil;

  if FileExists(Resource) then
  begin
    FSock := TLocalFile.Create(Resource);
  end
  else
  begin
    ParseURL(Resource, Prot, User, Pass, Host, Port, Path, Para);
    if Prot = 'http' then
    begin
      FSock := THttpSock.Create(Host, Port, Path);
    end
    else
    begin
      Exit;
    end;
  end;

  FSock.Execute;

  Item.Links := TStringList.Create;
end;

destructor TLibR3R.Destroy;
begin
  if Assigned(Item.Links) then
  begin
    Item.Links.Free;
    Item.Links := nil;
  end;

  FSock.Free;
  FSettings.Free;
  inherited Destroy;
end;

procedure TLibR3R.Parse;
var
  Finished: Boolean;
begin
  Finished := false;
  SetMessageEvent(FOnMessage);

  while not Finished do
  begin
    if Assigned(FSock.Sock) and (FSock.Sock.LastError <> 0) then
    begin
      CallMessageEvent(Self, true, ErrorGetting);
      Break;
    end;

    Finished := FSock.ParseItem(Item);
    if Item.Title <> '' then
    begin
      DoParseItem;
      Item.Links.Clear;
      Item.Title := '';
    end;
  end;
end;

procedure TLibR3R.DoParseItem;
begin
  if Assigned(FOnItemParsed) then
  begin
    FOnItemParsed(Item);
  end;
end;

end.
