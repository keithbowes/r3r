unit RSock;

interface

{$UNITPATH ../formats}

uses
  BlckSock, Classes, Feed, FeedItem;

const
  SockEof = #0;

type
  TRSock = class(TThread)
  private
    FSock: TTCPBlockSocket;
  protected
    FFeedType: TFeedType;
    FHost: String;
    FPort: String;
    function GetLine: String; virtual;
    property FeedType: TFeedType read FFeedType write FFeedType;
  public
    constructor Create(Host, Port: String);
    destructor Destroy; override;
    procedure Execute; override;
    function ParseItem(var Item: TFeedItem): Boolean; virtual;
    property Sock: TTCPBlockSocket read FSock;
  end;

implementation

uses
  Esf, Rss, Rss3, SysUtils;

var
  AbstractFeed: TFeed;

constructor TRSock.Create(Host, Port: String);
begin
  FHost := Host;
  FPort := Port;
  AbstractFeed := nil;
  inherited Create(false);
end;

destructor TRSock.Destroy;
begin
  FSock.Free;
  FreeAndNil(AbstractFeed);
  inherited Destroy;
end;

function TRSock.GetLine: String;
begin
  Result := FSock.RecvString(5000);

  if FSock.LastError <> 0 then
  begin
    Result := SockEof;
  end;
end;

procedure TRSock.Execute;
begin
  FSock := TTCPBlockSocket.Create;
  FSock.Connect(FHost, FPort);
  FSock.ConvertLineEnd := true;
end;

function TRSock.ParseItem(var Item: TFeedItem): Boolean;
var
  Line: String;
begin
  if not Assigned(AbstractFeed) then
  begin
    if FeedType = ftEsf then
    begin
      AbstractFeed := TEsfFeed.Create;
    end
    else if FeedType = ftRss3 then
    begin
      AbstractFeed := TRss3Feed.Create;
    end
    else if FeedType = ftRss then
    begin
      AbstractFeed := TRssFeed.Create;
    end
    else
    begin
      Result := true;
      Exit;
    end;
  end;
  
  with AbstractFeed do
  begin
    repeat
      Line := GetLine;
    until not ParseLine(Line, Item);
  end;

  Result := Line = SockEof;
end;

end.
