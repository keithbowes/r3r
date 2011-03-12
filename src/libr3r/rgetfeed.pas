unit RGetFeed;

interface

uses
  LibR3R, RSock, RMessage,
{$IFDEF SOCKETS_SYNAPSE}
  SynaUtil
{$ENDIF}
  
{$IFDEF SOCKETS_BSD}
  SockWrap
{$ENDIF};

procedure GetFeed(Resource: String; var Prot, Host, Port, Path, Para: String);
procedure ParseFeed(const Sock: TRSock);
procedure SetFeedObject(const Lib: TLibR3R);

implementation

uses
  LibIdn, LibR3RStrings, RStrings, SysUtils
{$IFDEF __GPC__}
  , GPC
{$ENDIF};

var
  FeedObj: TLibR3R;
  Item: TFeedItem;

procedure GetFeed(Resource: String; var Prot, Host, Port, Path, Para: String);
{$IFDEF __GPC__}
const
  PathDelim = DirSeparator;
{$ENDIF}
var
  Pass, User: String;
  PHost: PChar;
begin
  if FileExists(Resource) then
  begin
    Prot := 'file';

    if ExtractFilePath(Resource) = '' then
    begin
      Resource := GetCurrentDir + PathDelim + Resource
    end;
  end
  else
  begin
    ParseURL(Resource, Prot, User, Pass, Host, Port, Path, Para);
    idna_to_ascii_8z(StrToPChar(Host), @PHost, 0);
    Host := StrPas(PHost);
  end;
end;

procedure ParseFeed(const Sock: TRSock);
var
  Finished: Boolean;
begin
  Finished := false;

  while not Finished do
  begin
    if Assigned(Sock.Sock) and Sock.Error then
    begin
      CallMessageEvent(Sock, true, ErrorGetting);
      Break;
    end;

    Finished := Sock.ParseItem(Item);

    if (Sock.ShouldShow) and (Item.Title <> '') then
    begin
      Item.Translate;
      FeedObj.DisplayItem(Item);
    end;
  end;
end;

procedure SetFeedObject(const Lib: TLibR3R);
begin
  FeedObj := Lib;
end;

initialization

Item := TFeedItem.Create;

finalization

Item.Free;

end.
