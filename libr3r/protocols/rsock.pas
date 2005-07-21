unit RSock;

interface

uses
  BlckSock, Classes, Feed, FeedItem;

type
  TRSock = class(TThread)
  private
    FSock: TTCPBlockSocket;
  protected
    FAbstractFeed: TFeed;
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
  Atom, Esf, Rss, Rss3, SockConsts, SysUtils;

constructor TRSock.Create(Host, Port: String);
begin
  FHost := Host;
  FPort := Port;
  inherited Create(true);
end;

destructor TRSock.Destroy;
begin
  FSock.Free;
  FreeAndNil(FAbstractFeed);

  if Self.ClassName <> 'TLocalFile' then
  begin
    inherited Destroy;
  end;
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
  ItemFinished: Boolean;
  Line: String;
begin
  if not Assigned(FAbstractFeed) then
  begin
    if FeedType = ftEsf then
    begin
      FAbstractFeed := TEsfFeed.Create;
    end
    else if FeedType = ftRss3 then
    begin
      FAbstractFeed := TRss3Feed.Create;
    end
    else if FeedType = ftRss then
    begin
      FAbstractFeed := TRssFeed.Create;
    end
    else if FeedType = ftAtom then
    begin
      FAbstractFeed := TAtomFeed.Create;
    end
    else
    begin
      Result := true;
      Exit;
    end;
  end;
  
  with FAbstractFeed do
  begin
    ClearItem(Item);
    repeat
      Line := GetLine;
      ParseLine(Line, Item, ItemFinished);
    until ItemFinished;
  end;

  Result := Line = SockEof;
end;

end.
