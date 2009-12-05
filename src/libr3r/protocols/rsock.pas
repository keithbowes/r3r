unit RSock;

interface

uses
  BlckSock, Classes, Feed, FeedItem;

type
  TRSock = class
  private
    FSock: TTCPBlockSocket;
    FError: Boolean;
  protected
    FAbstractFeed: TFeed;
    FChunkedLength: integer;
    FFeedType: TFeedType;
    FHost: String;
    FPort: String;
    FShouldShow: Boolean;
    FUseChunked: Boolean;
    function GetLine: String; virtual;
    property FeedType: TFeedType read FFeedType write FFeedType;
  public
    constructor Create(Host, Port: String);
    destructor Destroy; override;
    procedure DomainSet(Host, Port: String);
    procedure Execute; virtual;
    function ParseItem(var Item: TFeedItem): Boolean; virtual;
    property Sock: TTCPBlockSocket read FSock;
    property Error: Boolean read FError;
    property ShouldShow: Boolean read FShouldShow write FShouldShow;
  end;

implementation

uses
  Atom, Esf, Rss, Rss3, SockConsts, SysUtils;

constructor TRSock.Create(Host, Port: String);
begin
  inherited Create;
  DomainSet(Host, Port);
  FShouldShow := true;
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

procedure TRSock.DomainSet(Host, Port: String);
begin
  FHost := Host;
  FPort := Port;
  FUseChunked := false;
end;

function TRSock.GetLine: String;
begin
  Result := FSock.RecvString(5000);
  FError := FSock.LastError <> 0;

  if FError then
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
  ErrPos: word;
  ItemFinished: Boolean;
  Len: integer;
  Line: String;
begin
  ShouldShow := true;

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
    Item.Clear;
    repeat
      Line := GetLine;

      if FUseChunked then
      begin
        if FChunkedLength > 0 then
        begin
          Dec(FChunkedLength, Length(Line) + 2);
        end
        else if Trim(Line) <> '' then
        begin
          Val('$' + TrimRight(Line), Len, ErrPos);

          if ErrPos = 0 then
          begin
            FChunkedLength := Len + 1;
            Continue;
          end;
        end;
      end;

      ParseLine(Line, Item, ItemFinished);
    until ItemFinished;
  end;

  ShouldShow := ShouldShow and FAbstractFeed.ShouldShow;
  Result := Line = SockEof;
end;

end.
