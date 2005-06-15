unit LibR3R;

interface

{$UNITPATH formats}
{$UNITPATH protocols}

uses
{$IFDEF UNIX}
  CThreads,
{$ENDIF}
  FeedItem, RSock;

type
  TParsedFeedItem = TFeedItem;
  TParsedEvent = procedure(Item: TParsedFeedItem) of object;

  TLibR3R = class
  private
    FHadError: Boolean;
    FOnItemParsed: TParsedEvent;
    FSock: TRSock;
  protected
    procedure DoParseItem;
  public
    constructor Create(const Resource: String);
    destructor Destroy; override;
    procedure Parse;
    property OnItemParsed: TParsedEvent write FOnItemParsed;
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

    FSock.Execute;
    FHadError := FSock.Sock.LastError <> 0;
  end;

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
  inherited Destroy;
end;

procedure TLibR3R.Parse;
var
  Finished: Boolean;
begin
  Finished := false;

  while (not Finished) and (not FHadError) do
  begin
    Finished := FSock.ParseItem(Item);
    if Item.Title <> '' then
    begin
      DoParseItem;
      Item.Links.Clear;
      Item.Title := '';
    end;
  end;

  // TODO: Process in a UI-independent way
  if FHadError then
  begin
    Writeln(GetError);
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
