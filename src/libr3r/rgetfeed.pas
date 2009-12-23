unit RGetFeed;

interface

uses
  FeedItem, LibR3R, RSock, RMessage,
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
  LibR3RStrings, SysUtils
{$IFDEF __GPC__}
  , GPC
{$ENDIF};

var
  FeedObj: TLibR3R;
  Item: TFeedItem;
  ItemCreated: Boolean;

procedure GetFeed(Resource: String; var Prot, Host, Port, Path, Para: String);
{$IFDEF __GPC__}
const
  PathDelim = DirSeparator;
{$ENDIF}
var
  Pass, User: String;
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

    if Sock.ShouldShow then
    begin
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
