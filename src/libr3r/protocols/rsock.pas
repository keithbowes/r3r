unit RSock;

interface

uses
  Feed, FeedItem,
{$IFDEF SOCKETS_SYNAPSE}
  BlckSock
{$ENDIF}
  
{$IFDEF SOCKETS_BSD}
  SockWrap
{$ENDIF};

type
  TRSock = class
  private
    FError: Boolean;
  protected
    FAbstractFeed: TFeed;
    FChunkedLength: integer;
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
    destructor Destroy; override;
    procedure DomainSet(Host, Port: String);
    procedure Execute; virtual;
    function GetLine: String; virtual;
    function ParseItem(var Item: TFeedItem): Boolean; virtual;
    function Error: Boolean;
  end;

implementation

uses
  Atom, Esf, Rss, Rss3, SockConsts, SysUtils;

constructor TRSock.Create(Host, Port: String);
begin
{$IFNDEF __GPC__}
  inherited Create;
{$ENDIF}

  DomainSet(Host, Port);
  FShouldShow := true;
end;

destructor TRSock.Destroy;
begin
{$IFNDEF __GPC__}
  Sock.Free;
  FAbstractFeed.Free;
{$ENDIF}

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
  GetLine := Sock.RecvString(5000);
  FError := Sock.LastError <> 0;

  if FError then
  begin
    GetLine := SockEof;
  end;
end;

procedure TRSock.Execute;
begin
{$IFDEF SOCKETS_SYNAPSE}
  Sock := TTCPBlockSocket.Create;
{$ENDIF}

{$IFDEF SOCKETS_BSD}
{$IFNDEF __GPC__}
  Sock := TSockWrap.Create;
{$ENDIF}
{$ENDIF}

  Sock.Connect(FHost, FPort);
  Sock.ConvertLineEnd := true;
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
{$IFNDEF __GPC__}
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
{$ENDIF}
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
          Val('$' + {$IFNDEF __GPC__}TrimRight(Line){$ELSE}wTrimRight(Line){$ENDIF}, Len, ErrPos);

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
  ParseItem := Line = SockEof;
end;

function TRSock.Error: Boolean;
begin
  Error := FError;
end;

end.
