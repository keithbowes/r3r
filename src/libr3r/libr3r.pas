unit LibR3R;

interface

uses
  FeedItem, ItemCallbacks, RHistory, RList, RSettings, RSock, RSubscriptions;

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
    FSocketList: PRList;
    function AddSocket(const Resource, Prot, Host: String; Port: word; const Path, Para: String): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure QueueURI(Resource: String);
    procedure UnqueueURI;
    function RetrieveChunk: Boolean;
    procedure RetrieveFeed(Resource: String);
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
  LibR3RStrings, LocalFile, RGetFeed, RMessage, RProp, URIParser;

function TLibR3R.AddSocket(const Resource, Prot, Host: String; Port: word; const Path, Para: String): Boolean;
var
  Sock: TRSock;
begin
  if Prot = 'file' then
  begin
    Sock := TLocalFile.Create(Resource);
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
    Sock := THttpSock.Create(Prot, Host, Port, Path, Para);
  end
{$ENDIF}
  else
  begin
    Sock := nil;
  end;

  Result := Sock <> nil;
  if Result then
  begin
    if FSocketList^.Count = 0 then
    begin
      Dispose(FSocketList, Done);
      New(FSocketList, Init);
    end;

    SetObjectProp(Sock, 'lib', Self);
    FSocketList^.Add(Sock);
  end;
end;

constructor TLibR3R.Create;
begin
  inherited Create;
  New(FSocketList, Init);
end;

destructor TLibR3R.Destroy;
var
  i: PtrUInt;
begin
  { This should normally be handled by RetrieveChunk, but if the program is exited during parsing,
    there will be unfreed objects. }
  if FSocketList^.Count > 0 then
  begin
    for i := 0 to FSocketList^.Count - 1 do
    begin
      TRSock(FSocketList^.GetNth(i)).Free;
    end;
  end;

  FreeItemCallback;
  Dispose(FSocketList, Done);
  inherited Destroy;
end;

procedure TLibR3R.QueueURI(Resource: String);
var
  Sock: TRSock;
  URL: TURI;
begin
  URL := GetFeed(Resource);
  if not AddSocket(Resource, URL.Protocol, URL.Host, URL.Port, URL.Path + URL.Document, URL.Params) then
  begin
    HandleMessage(true, ErrorGetting, Resource);
  end;

  if FSocketList^.Count > 0 then
  begin
    Sock := TRSock(FSocketList^.GetNth(FSocketList^.Count - 1));
    if Sock <> nil then
    begin
      Sock.Open;
      History^.Add(Resource);
    end
  end;

  SetMessageObject(Self);
end;

procedure TLibR3R.UnqueueURI;
var
  Sock: TRSock;
begin
  if FSocketList^.Count > 0 then
  begin
    Sock := TRSock(FSocketList^.GetNth(0));
{$IFDEF SOCKETS_SYNAPSE}
    if (Sock is THttpSock) then
    begin
      Sock.Sock.CloseSocket;
    end;
{$ENDIF}
    Sock.Free;
    FSocketList^.Delete(0);
  end
  else
  begin
    { Add an empty URI so that the proper events will fire }
    QueueURI('file://');
  end;
end;

function TLibR3R.RetrieveChunk: Boolean;
var
  Sock: TRSock = nil;
begin
  if FSocketList^.Count > 0 then
  begin
    Sock := TRSock(FSocketList^.GetNth(0))
  end;

  if Sock <> nil then
  begin
    Result := ContinueParsing(Sock);
    if not Result then
    begin
      if FSocketList^.Count > 0 then
      begin
        Sock.Free;
        FSocketList^.Delete(0);

        { Start parsing the next feed in the queue, if any }
        if FSocketList^.Count > 0 then
        begin
          Result := true;
        end;
      end;
    end;
  end
  else
  begin
    Result := false;
  end;
end;

procedure TLibR3R.RetrieveFeed(Resource: String);
begin
  QueueURI(Resource);
  if TRSock(FSocketList^.GetNth(0)) <> nil then
  begin
    while RetrieveChunk do
    begin
    end;
  end;
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
