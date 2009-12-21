unit LibR3R;

interface

uses
  FeedItem, RMessage, RSettings, RSock, RSubscriptions;

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
    function Parse: Boolean;
  public
    constructor Create;
    destructor Destroy; {$IFNDEF __GPC__}override;{$ENDIF}
    procedure RetrieveFeed(Resource: String);
    procedure DisplayItem(const Item: TFeedItem); virtual;
    procedure HandleMessage(Sender: TObject; IsError: Boolean; MessageName, Extra: String); virtual;
  end;

var
  Settings: TRSettings;
  Subscriptions: PRSubscriptions;

implementation

uses
  Http, LibR3RStrings, LocalFile, RGetFeed, RUpdate
{$IFDEF SOCKETS_BSD}
  , SockWrap
{$ENDIF};

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
  Prot, Pass, Host, Port, Path, Para: String;
begin
  GetFeed(Resource, Prot, Host, Port, Path, Para);
  FSock := GetSockType(Resource, Prot, Host, Port, Path, Para);

  if FSock = nil then
  begin
    Exit;
  end;

  FSock.Execute;

  if Assigned(FSock) and Parse then
  begin
    FSock.Free;
  end;
end;

function TLibR3R.Parse: Boolean;
var
  Item: TFeedItem;
begin
  SetMessageObject(TObject(Self));
  Parse := ParseFeed(TObject(Self), FSock);
end;

{ Implement as empty so if the UI doesn't implement them,
  there won't be crashes. }
procedure TLibR3R.DisplayItem(const Item: TFeedItem);
begin
end;

procedure TLibR3R.HandleMessage(Sender: TObject; IsError: Boolean; MessageName, Extra: String);
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
