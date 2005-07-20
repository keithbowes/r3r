unit LibR3R;

interface

uses
{$IFDEF UNIX}
  CThreads,
{$ENDIF}
  FeedItem, RMessage, RSettings, RSock;

type
  TParsedFeedItem = TFeedItem;
  TParsedEvent = procedure(Item: TParsedFeedItem) of object;

  TLibR3R = class
  private
    FFreeLinks: Boolean;
    FItem: TFeedItem;
    FOnItemParsed: TParsedEvent;
    FOnMessage: TRMessage;
    FSettings: TRSettings;
    FSock: TRSock;
  protected
    procedure DoParseItem;
  public
    constructor Create(const Resource: String);
    destructor Destroy; override;
    procedure Parse;
    property FreeLinks: Boolean read FFreeLinks write FFreeLinks;
    property OnItemParsed: TParsedEvent write FOnItemParsed;
    property OnMessage: TRMessage write FOnMessage;
    property Settings: TRSettings read FSettings write FSettings;
  end;

implementation

uses
  Classes, Http, LocalFile, LibR3RStrings, SynaUtil, SysUtils;

constructor TLibR3R.Create(const Resource: String);
var
  Prot, User, Pass, Host, Port, Path, Para: String;
begin
  inherited Create;
  FFreeLinks := true;
  FSettings := TRSettings.Create;
  FSock := nil;

  if FileExists(Resource) then
  begin
    FSock := TLocalFile.Create(Resource);
  end
  else
  begin
    ParseURL(Resource, Prot, User, Pass, Host, Port, Path, Para);
    if Prot = 'http' then
    begin
      FSock := THttpSock.Create(Host, Port, Path);
    end
    else
    begin
      Exit;
    end;
  end;

  FSock.Execute;
end;

destructor TLibR3R.Destroy;
begin
  FItem.Links := nil;

  FSock.Free;
  FSettings.Free;
  inherited Destroy;
end;

procedure TLibR3R.Parse;
var
  Finished: Boolean;
begin
  Finished := false;
  SetMessageEvent(FOnMessage);

  while not Finished do
  begin
    if Assigned(FSock.Sock) and (FSock.Sock.LastError <> 0) then
    begin
      CallMessageEvent(Self, true, ErrorGetting);
      Break;
    end;

    Finished := FSock.ParseItem(FItem);
    if FItem.Title <> '' then
    begin
      DoParseItem;
    end;
  end;
end;

procedure TLibR3R.DoParseItem;
begin
  if Assigned(FOnItemParsed) then
  begin
    FOnItemParsed(FItem);
  end;
end;

end.
