unit LibR3R;

interface

uses
  FeedItem, ItemCallbacks, RHistory, RSettings, RSock, RSubscriptions;

const
  SettingsRead = RSettings.SettingsRead;
  SettingsWrite = RSettings.SettingsWrite;

  TypeNone = RSettings.TypeNone;
  TypeString = RSettings.TypeString;
  TypeInteger = RSettings.TypeInteger;
  TypeBoolean = RSettings.TypeBoolean;

type
  TFeedItem = FeedItem.TFeedItem;
  PRSetting = RSettings.PRSetting;

  TLibR3R = class
  private
    FSock: TRSock;
  protected
    procedure NotifyUpdate; virtual;
    procedure DoParseItem(Item: TFeedItem);
    procedure DoUpdate;
    procedure Parse;
  public
    constructor Create;
    destructor Destroy; {$IFNDEF __GPC__}override;{$ENDIF}
    procedure RetrieveFeed(Resource: String); virtual;
    procedure DisplayItem(const Item: TFeedItem); virtual;
    procedure HandleMessage(IsError: Boolean; MessageName, Extra: String); virtual;
    procedure RegisterItemCallback(const cb: TItemCallback);
  end;

var
  History: PRHistory;
  Settings: TRSettings;
  Subscriptions: PRSubscriptions;

implementation

uses
{$IFNDEF SOCKETS_NONE}
  Http, RUpdate, 
{$ENDIF}
  LibR3RStrings, LocalFile, RGetFeed, RMessage;

function GetSockType(const Resource, Prot, Host, Port, Path, Para: String): TRSock;
begin
  if Prot = 'file' then
  begin
    GetSockType := TLocalFile.Create(Resource);
  end
{$IFNDEF SOCKETS_NONE}
  else if Prot = 'http' then
  begin
    GetSockType := THttpSock.Create(Host, Port, Path, Para);
  end
{$ENDIF}
  else
  begin
    GetSockType := nil;
  end;
end;

{$IFNDEF SOCKETS_NONE}
function GetUpdateObject: TRUpdate;
begin
  GetUpdateObject := TRUpdate.Create;
end;
{$ENDIF}

constructor TLibR3R.Create;
begin
{$IFNDEF __GPC__}
  inherited Create;
{$ENDIF}

  if Settings.GetBoolean(Settings.IndexOf('check-for-updates')) then
  begin
    DoUpdate;
  end;
end;

destructor TLibR3R.Destroy;
begin
{$IFNDEF __GPC__}
  inherited Destroy;
{$ENDIF}
end;

procedure TLibR3R.RetrieveFeed(Resource: String);
var
  Prot, Host, Port, Path, Para: String;
begin
  GetFeed(Resource, Prot, Host, Port, Path, Para);
  FSock := GetSockType(Resource, Prot, Host, Port, Path, Para);

  if FSock = nil then
  begin
    Exit;
  end;

  FSock.Execute;
  Parse;

  if Assigned(FSock) then
  begin
{$IFDEF __GPC__}
{$IFNDEF SOCKETS_NONE}
    if Prot <> 'file' then
    begin
      FSock.Sock.CloseSocket;
    end;
{$ENDIF}
{$ELSE}
    FSock.Free;
{$ENDIF}
  end;

  History^.Add(Resource);
end;

procedure TLibR3R.Parse;
begin
  SetFeedObject(Self);
  SetMessageObject(Self);
  ParseFeed(FSock);
end;

{ Implement as empty so if the UI doesn't implement them,
  there won't be crashes. }
procedure TLibR3R.DisplayItem(const Item: TFeedItem);
begin
end;

procedure TLibR3R.HandleMessage(IsError: Boolean; MessageName, Extra: String);
begin
end;

procedure TLibR3R.NotifyUpdate;
begin
end;

procedure TLibR3R.RegisterItemCallback(const cb: TItemCallback);
begin
  ItemCallbacks.RegisterItemCallback(cb);
end;

procedure TLibR3R.DoParseItem(Item: TFeedItem);
begin
  DisplayItem(Item);
end;

procedure TLibR3R.DoUpdate;
{$IFNDEF SOCKETS_NONE}
var
  Up: TRUpdate;
begin
  Up := GetUpdateObject;
  with Up do
  begin
    if Available then
    begin
      NotifyUpdate;
    end;

    Free;
  end;
{$ELSE}
begin
{$ENDIF}
end;

initialization

History := RHistory.History;
Settings := RSettings.Settings;
Subscriptions := RSubscriptions.Subscriptions;

end.
