unit RGetFeed;

interface

uses
  FeedItem, RSock, RMessage;

type
  TParseMethod = procedure(Item: TFeedItem){$IFNDEF __GPC__} of object{$ENDIF};

procedure GetFeed(Resource: String; var Prot, Host, Port, Path, Para: String);
procedure ParseFeed(const Sender: TObject; const UserFunc: TParseMethod; const Sock: TRSock);

implementation

uses
  LibR3RStrings, SysUtils,
{$IFDEF __GPC__}
  GPC,
{$ENDIF}
{$IFDEF SOCKETS_SYNAPSE}
  SynaUtil
{$ENDIF}
  
{$IFDEF SOCKETS_BSD}
  SockWrap
{$ENDIF};

var
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

procedure ParseFeed(const Sender: TObject; const UserFunc: TParseMethod; const Sock: TRSock);
var
  Finished: Boolean;
begin
  Finished := false;

  while not Finished do
  begin
    if Assigned(Sock.Sock) and Sock.Error then
    begin
      CallMessageEvent(Sender, true, ErrorGetting);
      Break;
    end;
    Finished := Sock.ParseItem(Item);
    if Assigned(UserFunc) and Sock.ShouldShow then
    begin
      UserFunc(Item);
    end;
  end;
end;

initialization

Item := TFeedItem.Create;

finalization

{$IFNDEF __GPC__}
Item.Free;
{$ENDIF}

end.
