unit RGetFeed;

interface

uses
  LibR3R, RSock, RMessage
{$IFDEF SOCKETS_SYNAPSE}
  , SynaUtil
{$ENDIF}
  
{$IFDEF SOCKETS_BSD}
  , SockWrap
{$ENDIF};

procedure GetFeed(Resource: String; var Prot, Host, Port, Path, Para: String);
procedure ParseFeed(const Sock: TRSock);
procedure SetFeedObject(const Lib: TLibR3R);

implementation

uses
{$IFDEF USE_IDN}
  LibIdn, 
{$ENDIF}
  LibR3RStrings, RFilter, RStrings, SysUtils
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
{$IFDEF USE_IDN}
  PHost: PChar;
{$ENDIF}
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
{$IFNDEF SOCKETS_NONE}
    ParseURL(Resource, Prot, User, Pass, Host, Port, Path, Para);
{$IFDEF USE_IDN}
    idna_to_ascii_8z(StrToPChar(Host), @PHost, 0);
    Host := StrPas(PHost);
{$ENDIF}
{$ENDIF}
  end;
end;

procedure ParseFeed(const Sock: TRSock);
var
  Finished: Boolean;
  UseFilters: Boolean;
begin
  Finished := false;
  UseFilters := Settings.GetBoolean(Settings.IndexOf('use-filters'));

  while not Finished do
  begin
    if Finished then begin WriteLn('Uh, yeah'); Halt; end;
{$IFNDEF SOCKETS_NONE}
    if Assigned(Sock.Sock) and Sock.Error then
    begin
      CallMessageEvent(Sock, true, ErrorGetting);
{$IFNDEF __GPC__} { Hack: Sock.Error is always true in GPC }
      Break;
{$ENDIF}
    end;
{$ENDIF}

    Finished := Sock.ParseItem(Item);

    if Sock.ShouldShow then
    begin
      Item.Translate;

      if UseFilters then
      begin
        FilterItem(Item);
      end;

      if Item.Title <> '' then
      begin
        FeedObj.DisplayItem(Item);
      end;
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
