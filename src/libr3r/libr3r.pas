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
  TParsedEvent = procedure(Item: TFeedItem){$IFNDEF __GPC__} of object{$ENDIF};
  TRSetting = RSettings.TRSetting;

  TLibR3R = class
  private
    FOnItemParsed: TParsedEvent;
    FOnMessage: TRMessage;
    FSock: TRSock;
  protected
    procedure DisplayItem(const Item: TFeedItem); virtual;
    procedure HandleMessage(Sender: TObject; IsError: Boolean; MessageName, Extra: String); virtual;
    procedure NotifyUpdate; virtual;
    procedure DoParseItem(Item: TFeedItem);
    procedure DoUpdate;
    procedure Parse;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RetrieveFeed(Resource: String);
  end;

var
  Settings: TRSettings;
  Subscriptions: PRSubscriptions;

implementation

uses
  Http, LibR3RStrings, LocalFile, RGetFeed, RUpdate;

constructor TLibR3R.Create;
begin
{$IFNDEF __GPC__}
  inherited Create;
{$ENDIF}
  FSock := nil;

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

  if Prot = 'file' then
  begin
{$IFNDEF __GPC__}
    FSock := TLocalFile.Create(Resource);
{$ENDIF}
  end
  else if Prot = 'http' then
  begin
{$IFNDEF __GPC__}
    FSock := THttpSock.Create(Host, Port, Path, Para);
{$ENDIF}
  end
  else
  begin
    Exit;
  end;

  FSock.Execute;

  Parse;

{$IFNDEF __GPC__}
  if Assigned(FSock) then
  begin
    FSock.Free;
  end;
{$ENDIF}
end;

procedure TLibR3R.Parse;
{$IFDEF __GPC__}
var
  Item: TFeedItem;
{$ENDIF}
begin
{$IFNDEF __GPC__}
  SetMessageEvent(HandleMessage);
  ParseFeed(TObject(Self), DoParseItem, FSock);
{$ENDIF}
end;

{ Implement as empty so if the UI doens't implement them,
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
  DisplayItem(Item)
end;

procedure TLibR3R.DoUpdate;
begin
{$IFNDEF __GPC__}
  with TRUpdate.Create do
  begin
    if Available then
    begin
      NotifyUpdate;
    end;

    Free;
  end;
{$ENDIF}
end;

initialization

Settings := RSettings.Settings;
Subscriptions := RSubscriptions.Subscriptions;

end.
