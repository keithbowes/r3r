unit Http;

interface

uses
  Feed, FeedItem, Headers, HttpCache, LocalFile, RSock;

type
  THttpSock = class(TRSock)
  private
    FCache: THttpCache;
    FCachable: Boolean;
    FHeaders: THeaders;
    FHttpVersion: real;
    FIndirectHost: String;
    FLocal: TLocalFile;
    FPath: String;
    FURL: String;
    function GetType: TFeedType;
  protected
    procedure Connect(Host, Port, Path, Search: String);
    procedure GetHeaders;
    procedure SendHeader(const Name: String);
    procedure SendHeaders;
    property Cache: THttpCache read FCache write FCache;
    property Headers: THeaders read FHeaders;
  public
    constructor Create(Host, Port, Path, Search: String);
    procedure Execute; override;
    function ParseItem(var Item: TFeedItem): Boolean; override;
    destructor Destroy; override;
  end;

implementation

uses
  Info, LibR3RStrings, RGetFeed, RMessage, RSettings,
  SockConsts, StrTok, SysUtils;

const
  Tab = #9;
  WhitespaceChars = #0#8#9#10#13#32;

type
  THeaderState = (hsUnstarted, hsStarted, hsFinished);

procedure THttpSock.GetHeaders;
var
  ColonIndex: integer;
  ErrPos: word;
  HeaderName, HeaderValue: String;
  HeaderState: THeaderState;
  Line: String;
  NullLines: 0..50;
  RespList: TStringsList;
  Prot, Host, Port, Path, Para: String;
begin
  HeaderState := hsUnstarted;
  NullLines := 0;

  while (HeaderState <> hsFinished) do
  begin
    Line := GetLine;

    if (Line = '') and (HeaderState = hsStarted) then
    begin
      HeaderState := hsFinished;
    end
    else if (Line <> '') and (HeaderState = hsUnstarted) then
    begin
      if (Line = SockEof) and (NullLines < 50) then
      begin
        Inc(NullLines);
        Continue;
      end;

      HeaderState := hsStarted;

      RespList := Split(Line, WhitespaceChars);

      Val(Copy(RespList.Strings[0], 6, Length(RespList.Strings[0]) - 5), FHttpVersion, ErrPos);

      if ErrPos <> 0 then
      begin
        CallMessageEvent(Self, true, InvalidHeaders, FURL);
        Break;
      end;

      Val(RespList.Strings[1], FHeaders.Status, ErrPos);

      if FHeaders.Status = 200 then
      begin
        FCache.Invalidate;
      end
    end
    else
    begin
      ColonIndex := Pos(':', Line);
      HeaderName := LowerCase(Copy(Line, 0, ColonIndex - 1));
      HeaderValue := Trim(Copy(Line, ColonIndex + 1, Length(Line) - ColonIndex));

      if HeaderName = 'date' then
      begin
        FHeaders.Date := HeaderValue;
      end
      else if HeaderName = 'content-encoding' then
      begin
        FHeaders.ContentEncoding := HeaderValue;
      end
      else if HeaderName = 'content-type' then
      begin
        if (Pos('esf', HeaderValue) <> 0) or (Pos('text/plain', HeaderValue) <> 0) then
        begin
          FHeaders.ContentType := ftEsf;
        end
        else if Pos('text/x-rss', HeaderValue) = 1 then
        begin
          FHeaders.ContentType := ftRss3
        end
        else if Pos('atom', HeaderValue) <> 0 then
        begin
          FHeaders.ContentType := ftAtom;
        end
        else if Pos('rss', HeaderValue) <> 0 then
        begin
          FHeaders.ContentType := ftRss;
        end
        else if Pos('xml', HeaderValue) <> 0 then
        begin
          FHeaders.ContentType := ftXml;
        end
        else
        begin
          FHeaders.ContentType := ftUnknown;
        end;
      end
      else if HeaderName = 'location' then
      begin
        FHeaders.Status := 200;
        Sock.Free;

        GetFeed(HeaderValue, Prot, Host, Port, Path, Para);
        Connect(Host, Port, Path, Para);
        Execute;
        ParseFeed(Self, nil, Self);
      end
      else if (HeaderName = 'transfer-encoding') and (Pos('chunked', HeaderValue) <> 0) then
      begin
        FUseChunked := true;
      end
      else if HeaderName = 'etag' then
      begin
        { Etags won't work in HTTP/0.9 and HTTP/1.0 }
        if FHttpVersion >= 1.1 then
        begin
          FHeaders.Etag := HeaderValue;
          Cache.Info^.CacheType := ctEtag;
          Cache.Info^.CacheParam := HeaderValue
        end
      end
      else if HeaderName = 'expires' then
      begin
        FHeaders.Expires := HeaderValue;

        if (Cache.Info^.CacheType <> ctEtag) and (Cache.Info^.CacheType <> ctLastModified) then
        begin
          Cache.Info^.CacheType := ctExpires;
          Cache.Info^.CacheParam := HeaderValue
        end
      end
      else if HeaderName = 'last-modified' then
      begin
        if Cache.Info^.CacheType <> ctEtag then
        begin
          FHeaders.LastModified := HeaderValue;
          Cache.Info^.CacheType := ctLastModified;
          Cache.Info^.CacheParam := HeaderValue
        end;
      end
      else if (HeaderName = 'cache-control') and ((LowerCase(HeaderValue) = 'max-age=0') or (HeaderValue = '0')) then
      begin
        FCachable := false
      end
    end;

    if FHeaders.Status = 200 then
    begin
      Cache.WriteData(Line, cdtResponse);
    end
  end;

  if Settings.GetBoolean(Settings.IndexOf('enable-mime-guess')) or (FHeaders.ContentType = ftXml) then
  begin
    FHeaders.ContentType := GetType;
  end;

  Cache.Info^.HeaderRec := FHeaders;

  if FCachable and (FHeaders.Status = 200) then
  begin
    Cache.WriteData(IntToStr(Ord(Cache.Info^.CacheType)) + Tab + Cache.Info^.CacheParam + Tab + IntToStr(Ord(Cache.Info^.HeaderRec.ContentType)), cdtInfo);
  end;
end;

constructor THttpSock.Create(Host, Port, Path, Search: String);
var
  FullPath: String;
begin
  inherited Create(Host, Port);
  Connect(Host, Port, Path, Search);

  FullPath := StringReplace(FIndirectHost + FPath, '/', Pred(PathDelim), [rfReplaceAll]);
  FullPath := StringReplace(FullPath, '?', '_', [rfReplaceAll]);
  FCache := THttpCache.Create(FullPath);
end;

procedure THttpSock.Connect(Host, Port, Path, Search: String);
var
  UseProxy: Boolean;
begin
  FIndirectHost := Host;

  if Port <> '80' then
  begin
    FIndirectHost := FIndirectHost + ':' + Port;
  end;

  UseProxy := Settings.GetBoolean(Settings.IndexOf('use-proxy'));
  if UseProxy then
  begin
    Host := Settings.GetString(Settings.IndexOf('proxy-address'));
    Port := IntToStr(Settings.GetInteger(Settings.IndexOf('proxy-port')));
  end;

  DomainSet(Host, Port);

  FCachable := true;
  FHeaders.ContentType := ftUnset;
  FPath := Path;

  if Search <> '' then
  begin
    FPath := FPath + '?' + Search
  end;

  FURL := 'http://' + FIndirectHost + FPath;

  if UseProxy then
  begin
    FPath := FURL;
  end;
end;

destructor THttpSock.Destroy;
begin
  FCache.Free;
  inherited Destroy;
end;

procedure THttpSock.Execute;
begin
  inherited Execute;
  SendHeaders;
end;

procedure THttpSock.SendHeader(const Name: String);
begin
  Sock.SendString(Name);
  Sock.SendString(#13#10);
end;

procedure THttpSock.SendHeaders;
var
  CacheHeader: String;
begin
  CacheHeader := FCache.GetCacheHeader;

  SendHeader('GET ' + FPath + ' HTTP/1.1');
  SendHeader('Host: ' + FIndirectHost);
  SendHeader('User-Agent: ' + UserAgent);

  if CacheHeader <> '' then
  begin
    SendHeader(CacheHeader);
  end;

  if Settings.GetBoolean(Settings.IndexOf('use-custom-accept-langs')) then
  begin
    SendHeader('Accept-Language: ' + Settings.GetString(Settings.IndexOf('accept-langs')));
  end;

  if Settings.GetBoolean(Settings.IndexOf('use-custom-accept-types')) then
  begin
    SendHeader('Accept: ' + Settings.GetString(Settings.IndexOf('accept-types')));
  end
  else
  begin
    SendHeader('Accept: text/plain, application/atom+xml, application/rdf+xml, application/xml;q=0.5');
  end;

  SendHeader('Accept-Encoding: ');
  SendHeader('Connection: close');
  SendHeader('');
end;

function THttpSock.ParseItem(var Item: TFeedItem): Boolean;
var
  Ext: String;
begin
  if Headers.Status = 0 then
  begin
    GetHeaders;
  end;

  FeedType := Headers.ContentType;

  if FHeaders.Status = 200 then
  begin
    Result := inherited ParseItem(Item);
  end
  else if (FHeaders.Status = 304) and not Settings.GetBoolean(Settings.IndexOf('hide-cached-feeds')) then
  begin
    CurrentCache := nil;
    Ext := FCache.GetFeedExtension(FHeaders.ContentType);
    if Ext = 'unknown' then
    begin
      Exit(true);
    end;

    if not Assigned(FLocal) then
    begin
      FLocal := TLocalFile.Create(FCacheDir + PathDelim +
        CacheFeedFile + '.' + FCache.GetFeedExtension(FHeaders.ContentType));
      FLocal.Execute;
    end;

    Result := FLocal.ParseItem(Item);

    if Result then
    begin
      FLocal.Free;
    end;

    ShouldShow := not Result;
  end
  else
  begin
    Result := true;
  end;
end;

function THttpSock.GetType: TFeedType;
var
  Line: String;
function IsAtom: Boolean;
  Line := LowerCase(FIndirectHost + FPath);
  end;
  Result := FHeaders.ContentType;
end;
  if Pos('atom', Line) <> 0 then
  if Pos('esf', Line) <> 0 then
    Result := ftAtom;
  end
  else if Pos('esf', Line) <> 0 then
  if ContentType <> ftRss then
    Result := ftEsf;
  end
  else if (Pos('rss3', Line) <> 0) or (Pos('r3', Line) <> 0) or ((Pos('rss', Line) <> 0) and (Pos('3', Line) <> 0)) or (Pos('txt', Line) <> 0) then
  if b then
    Result := ftRss3;
  end
  else if (Pos('rss', Line) <> 0) or (Pos('rdf', Line) <> 0) or (Pos('xml', Line) <> 0) then
    (Pos('xml', Line) <> 0) then
    Result := ftRss;
    end;
  GetType := ContentType;
end;

end.
