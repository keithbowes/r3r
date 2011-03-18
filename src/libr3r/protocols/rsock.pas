unit RSock;

interface

uses
  Feed, FeedItem
{$IFDEF SOCKETS_SYNAPSE}
  , BlckSock
{$ENDIF}
  
{$IFDEF SOCKETS_BSD}
  , SockWrap
{$ENDIF};

type
  TRSock = class
  private
  protected
    FChunkedLength: integer;
    FError: Boolean;
    FFeedType: TFeedType;
    FHost: String;
    FPort: String;
    FShouldShow: Boolean;
    FUseChunked: Boolean;
    FeedType: TFeedType;
  public
{$IFDEF SOCKETS_SYNAPSE}
    Sock: TTCPBlockSocket;
{$ENDIF}
{$IFDEF SOCKETS_BSD}
    Sock: TSockWrap;
{$ENDIF}

    ShouldShow: Boolean;
    constructor Create(Host, Port: String);
    destructor Destroy; {$IFNDEF __GPC__}override;{$ENDIF}
    procedure DomainSet(Host, Port: String);
    procedure Execute; virtual;
    function GetLine: String; virtual;
    function ParseItem(var Item: TFeedItem): Boolean; virtual;
    function Error: Boolean;
  end;

implementation

uses
  Atom, Esf, Rss, Rss3, SockConsts, SysUtils;

var
  FAbstractFeed: TFeed;


{$IFNDEF SOCKETS_NONE}
function CreateSocket:
{$IFDEF SOCKETS_SYNAPSE}
TTCPBlockSocket
{$ENDIF}

{$IFDEF SOCKETS_BSD}
TSockWrap
{$ENDIF};
begin
  
{$IFDEF SOCKETS_SYNAPSE}
  CreateSocket := TTCPBlockSocket.Create;
{$ENDIF}

{$IFDEF SOCKETS_BSD}
  CreateSocket := TSockWrap.Create;
{$ENDIF}
end;
{$ENDIF}

procedure SetAbstractFeed(const FeedType: TFeedType);
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
    else if (FeedType = ftRss) or (FeedType = ftXml) then
    begin
      FAbstractFeed := TRssFeed.Create;
    end
    else if FeedType = ftAtom then
    begin
      FAbstractFeed := TAtomFeed.Create;
    end
    else
    begin
      FAbstractFeed := nil;
    end;
  end;
end;

constructor TRSock.Create(Host, Port: String);
begin
{$IFNDEF __GPC__}
  inherited Create;
{$ENDIF}

{$IFNDEF SOCKETS_NONE}
  Sock := CreateSocket;
{$ENDIF}
  FError := false;
  DomainSet(Host, Port);
  FShouldShow := true;
end;

destructor TRSock.Destroy;
begin
{$IFNDEF SOCKETS_NONE}
  if Assigned(Sock) then
  begin
    Sock.Free;
  end;
{$ENDIF}

  FAbstractFeed.Free;
  FAbstractFeed := nil;

{$IFNDEF __GPC__}
  if Self.ClassName <> 'TLocalFile' then
  begin
    inherited Destroy;
  end;
{$ENDIF}
end;

procedure TRSock.DomainSet(Host, Port: String);
begin
  FHost := Host;
  FPort := Port;
  FUseChunked := false;
end;

function TRSock.GetLine: String;
begin
{$IFNDEF SOCKETS_NONE}
  GetLine := Sock.RecvString(5000);
{$IFDEF __GPC__}
  FError := (Sock.LastError < 0) or (Sock.LastError > 2);
{$ELSE}
  FError := Sock.LastError <> 0;
{$ENDIF}
{$ENDIF}

  if FError then
  begin
    GetLine := SockEof;
  end;
end;

procedure TRSock.Execute;
begin
{$IFNDEF SOCKETS_NONE}
  Sock.Connect(FHost, FPort);
  Sock.ConvertLineEnd := true;
{$ENDIF}
end;

function TRSock.ParseItem(var Item: TFeedItem): Boolean;
var
  ErrPos: word;
  ItemFinished: Boolean;
  Len: word;
  Line, Tmp: String;
begin
  ShouldShow := true;
  SetAbstractFeed(FeedType);

  if FAbstractFeed = nil then
  begin
    ParseItem := true;
    Exit;
  end;
  
  with FAbstractFeed do
  begin
    Item.Clear;
    repeat
      Line := GetLine;

      if FUseChunked then
      begin
        Tmp := Trim(Line);
        if FChunkedLength > 0 then
        begin
          Dec(FChunkedLength, Length(Line) + 2);
        end
        else if Tmp <> '' then
        begin
          Val('$' + Tmp, Len, ErrPos);

          if ErrPos = 0 then
          begin
            FChunkedLength := Len + 1;
            Continue;
          end;
        end;
      end;

      ParseLine(Line, Item);
    until Item.Finished;
  end;

  ShouldShow := ShouldShow and FAbstractFeed.ShouldShow;
  ParseItem := Line = SockEof;
end;

function TRSock.Error: Boolean;
begin
  Error := FError;
end;

end.
