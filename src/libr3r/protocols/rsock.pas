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
    destructor Destroy; {$IFNDEF __GPC__}override;{$ENDIF}
    procedure DomainSet(Host, Port: String);
    procedure Execute; virtual;
    function GetLine: String; virtual;
    function ParseItem(var Item: TFeedItem): Boolean; virtual;
    function Error: Boolean;
  end;

implementation

uses
  Atom, Esf, Rss, Rss3, SockConsts, SysUtils
  
{$IFNDEF __GPC__}
  , Math
{$ENDIF};

var
  FAbstractFeed: TFeed;

{$IFDEF __GPC__}
function Power(const Base, Exp: real): real;
begin
  Power := Base ** Exp;
end;
{$ENDIF}

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

procedure GetAbstractFeed(const FeedType: TFeedType);
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
      FAbstractFeed := nil;
    end;
  end;
end;

constructor TRSock.Create(Host, Port: String);
begin
{$IFNDEF __GPC__}
  inherited Create;
{$ENDIF}

  Sock := CreateSocket;
  FError := false;
  DomainSet(Host, Port);
  FShouldShow := true;
end;

destructor TRSock.Destroy;
begin
  if Assigned(Sock) then
  begin
    Sock.Free;
  end;

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
  GetLine := Sock.RecvString(5000);
  FError := Sock.LastError <> 0;

  if FError then
  begin
    GetLine := SockEof;
  end;
end;

procedure TRSock.Execute;
begin
  Sock.Connect(FHost, FPort);
  Sock.ConvertLineEnd := true;
end;

function TRSock.ParseItem(var Item: TFeedItem): Boolean;
var
  ErrPos: word;
  ItemFinished: Boolean;
  Len: word;
  Line, Tmp: String;

function HexToDec(const Hex: String): word;
var
  HexLen: byte;
  ErrPos, i: byte;
  Posit, Tmp, Radix, Res, Value: double;
begin
  HexLen := Length(Hex);
  Radix := 16;
  Res := 0;

  for i := HexLen downto 1 do
  begin
    Posit := HexLen - i + 1;
    Val(Hex[i], Tmp, ErrPos);
    Value := Tmp * (Power(Radix, Posit) / Radix);
    Res := Res + Value;
  end;

  if ErrPos = 0 then
  begin
    HexToDec := Trunc(Res);
  end
  else
  begin
    HexToDec := 0;
  end;
end;

begin
  ShouldShow := true;
  GetAbstractFeed(FeedType);

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
        if FChunkedLength > 0 then
        begin
          Dec(FChunkedLength, Length(Line) + 2);
        end
        else if Trim(Line) <> '' then
        begin
          Tmp := {$IFNDEF __GPC__}TrimRight(Line){$ELSE}Trim(Line){$ENDIF};
          Delete(Tmp, Pos('$', Tmp), 1);
          Len := HexToDec(Tmp);

          if Len <> 0 then
          begin
            FChunkedLength := Len;
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
