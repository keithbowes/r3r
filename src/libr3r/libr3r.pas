unit LibR3R;

interface

uses
  FeedItem, RSettings, RSock, RSubscriptions;

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
  end;

var
  Settings: TRSettings;
  Subscriptions: PRSubscriptions;

implementation

uses
  Http, LibR3RStrings, LocalFile, RGetFeed, RMessage, RUpdate;

function GetSockType(const Resource, Prot, Host, Port, Path, Para: String): TRSock;
begin
  if Prot = 'file' then
  begin
    GetSockType := TLocalFile.Create(Resource);
  end
  else if Prot = 'http' then
  begin
    GetSockType := THttpSock.Create(Host, Port, Path, Para);
  end
  else
  begin
    GetSockType := nil;
  end;
end;

function GetUpdateObject: TRUpdate;
begin
  GetUpdateObject := TRUpdate.Create;
end;

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
    if Prot <> 'file' then
    begin
      FSock.Sock.CloseSocket;
    end;
{$ELSE}
    FSock.Free;
{$ENDIF}
  end;
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

procedure TLibR3R.DoParseItem(Item: TFeedItem);
begin
  DisplayItem(Item);
end;

procedure TLibR3R.DoUpdate;
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
end;

initialization

Settings := RSettings.Settings;
Subscriptions := RSubscriptions.Subscriptions;

end.
