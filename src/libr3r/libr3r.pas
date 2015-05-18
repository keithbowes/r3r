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
    procedure Parse;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RetrieveFeed(Resource: String); virtual;
    procedure HandleMessage(IsError: Boolean; MessageName, Extra: String); virtual;
    procedure RegisterItemCallback(const cb: TItemCallback; const Data: Pointer);
  end;

var
  History: PRHistory;
  Settings: TRSettings;
  Subscriptions: PRSubscriptions;

implementation

{$IFDEF SOCKETS_LIBCURL}
{$DEFINE USE_SSL}
{$ENDIF}

uses
{$IFNDEF SOCKETS_NONE}
  Http,
{$ENDIF}
  LibR3RStrings, LocalFile, RGetFeed, RMessage, URIParser;

function GetSockType(const Resource, Prot, Host: String; Port: word; const Path, Para: String): TRSock;
begin
  if Prot = 'file' then
  begin
    GetSockType := TLocalFile.Create(Resource);
  end
{$IFNDEF SOCKETS_NONE}
  else if (Prot = 'http')
{$IFDEF USE_SSL}
  or (Prot = 'https')
{$ENDIF}
  then
  begin
    if Port = 0 then
    begin
      if Prot = 'http' then
      begin
        Port := 80
      end
      {$IFDEF USE_SSL}
      else if Prot = 'https' then
      begin
        Port := 443
      end
      {$ENDIF}
    end;
    GetSockType := THttpSock.Create(Prot, Host, Port, Path, Para);
  end
{$ENDIF}
  else
  begin
    GetSockType := nil;
  end;
end;

constructor TLibR3R.Create;
begin
  inherited Create;
end;

destructor TLibR3R.Destroy;
begin
  FreeItemCallback;
  inherited Destroy;
end;

procedure TLibR3R.RetrieveFeed(Resource: String);
var
  URL: TURI;
begin
  URL := GetFeed(Resource);
  FSock := GetSockType(Resource, URL.Protocol, URL.Host, URL.Port, URL.Path + URL.Document, URL.Params);

  if FSock <> nil then
  begin
    FSock.Execute;
    Parse;

    if Assigned(FSock) then
    begin
      FSock.Free;
    end;

    History^.Add(Resource);
  end;
end;

procedure TLibR3R.Parse;
begin
  SetMessageObject(Self);
  ParseFeed(FSock);
end;

procedure TLibR3R.HandleMessage(IsError: Boolean; MessageName, Extra: String);
begin
end;

procedure TLibR3R.RegisterItemCallback(const cb: TItemCallback; const Data: Pointer);
begin
  ItemCallbacks.RegisterItemCallback(cb, Data);
end;

initialization

History := RHistory.History;
Settings := RSettings.Settings;
Subscriptions := RSubscriptions.Subscriptions;

end.
