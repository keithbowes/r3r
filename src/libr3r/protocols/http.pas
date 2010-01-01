unit Http;

interface

uses
  Feed, FeedItem, Headers, HttpCache, LocalFile, RSock
{$IFDEF SOCKETS_BSD}
  , SockWrap
{$ENDIF};

type
  THttpSock = class(TRSock)
  private
    FCachable: Boolean;
    FHttpVersion: real;
    FIndirectHost: String;
    FLocal: TLocalFile;
    FPath: String;
    FURL: String;
    function GetType: TFeedType;
  protected
    Cache: THttpCache;
    Headers: THeaders;
    Method: String;
    procedure Connect(Host, Port, Path, Search: String);
    procedure SendHeader(Name: String);
    procedure SendHeaders;
    procedure InitCache;
  public
    constructor Create(Host, Port, Path, Search: String); {$IFDEF __GPC__}override;{$ENDIF}
    procedure Execute; override;
    function ParseItem(var Item: TFeedItem): Boolean; override;
    destructor Destroy; override;
    procedure GetHeaders;
  end;

implementation

uses
  Info, LibR3RStrings, RGetFeed, RMessage, RSettings,
  SockConsts, StrTok, SysUtils
  
{$IFDEF __GPC__}
  ,GPC
{$ENDIF};

const
  Tab = #9;
  WhitespaceChars = #0#8#9#10#13#32;
{$IFDEF __GPC__}
  PathDelim = DirSeparator;
{$ENDIF}

type
  THeaderState = (hsUnstarted, hsStarted, hsFinished);

function CreateCache(const Path: String): THttpCache;
begin
  CreateCache := THttpCache.Create(Path);
end;

function GetLocalFile(const Resource: String): TLocalFile;
begin
  GetLocalFile := TLocalFile.Create(Resource);
end;

procedure THttpSock.GetHeaders;
var
  ColonIndex: integer;
  ErrPos: word;
  HeaderName, HeaderValue: String;
  HeaderState: THeaderState;
  Line: String;
  NullLines: 0..20;
  RespList: TStringsList;
  Prot, Host, Port, Path, Para: String;
begin
  HeaderState := hsUnstarted;
  NullLines := 0;

  while (HeaderState <> hsFinished) do
  begin
    Line := Trim(GetLine);

    if (Line = '') and (HeaderState = hsStarted) then
    begin
      HeaderState := hsFinished;
    end
    else if (Line <> '') and (HeaderState = hsUnstarted) then
    begin
      if (Line = SockEof) and (NullLines < 20) then
      begin
        Inc(NullLines);
        Continue;
      end;

      HeaderState := hsStarted;
      RespList := Split(Line, WhitespaceChars);

      Val(Copy(RespList.Strings[0], 6, 3), FHttpVersion, ErrPos);
      if ErrPos <> 0 then
      begin
        CallMessageEventEx(Self, true, InvalidHeaders, FURL);
        Break;
      end;

      Val(RespList.Strings[1], Headers.Status, ErrPos);

      if Headers.Status = 200 then
      begin
        Cache.Invalidate;
      end
    end
    else
    begin
      ColonIndex := Pos(':', Line);
      HeaderName := LowerCase(Copy(Line, 1, ColonIndex - 1));
      HeaderValue := Trim(Copy(Line, ColonIndex + 1, Length(Line) - ColonIndex));

      if Line = '' then
      begin
        CallMessageEventEx(Self, true, InvalidHeaders, FURL);
        HeaderState := hsStarted;
      end;

      if HeaderName = 'date' then
      begin
        Headers.Date := HeaderValue;
      end
      else if HeaderName = 'content-encoding' then
      begin
        Headers.ContentEncoding := HeaderValue;
      end
      else if HeaderName = 'content-type' then
      begin
        if (Pos('esf', HeaderValue) <> 0) or (Pos('text/plain', HeaderValue) <> 0) then
        begin
          Headers.ContentType := ftEsf;
        end
        else if Pos('text/x-rss', HeaderValue) = 1 then
        begin
          Headers.ContentType := ftRss3
        end
        else if Pos('atom', HeaderValue) <> 0 then
        begin
          Headers.ContentType := ftAtom;
        end
        else if Pos('rss', HeaderValue) <> 0 then
        begin
          Headers.ContentType := ftRss;
        end
        else if Pos('xml', HeaderValue) <> 0 then
        begin
          Headers.ContentType := ftXml;
        end
        else
        begin
          Headers.ContentType := ftUnknown;
        end;
      end
      else if HeaderName = 'location' then
      begin
        Headers.Status := 0;
        Sock.CloseSocket;

        GetFeed(HeaderValue, Prot, Host, Port, Path, Para);
        Connect(Host, Port, Path, Para);
        Execute;
        ParseFeed(Self);
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
          Headers.Etag := HeaderValue;
          Cache.Info^.CacheType := ctEtag;
          Cache.Info^.CacheParam := HeaderValue
        end
      end
      else if HeaderName = 'expires' then
      begin
        Headers.Expires := HeaderValue;

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
          Headers.LastModified := HeaderValue;
          Cache.Info^.CacheType := ctLastModified;
          Cache.Info^.CacheParam := HeaderValue
        end;
      end
      else if (HeaderName = 'cache-control') and ((LowerCase(HeaderValue) = 'max-age=0') or (HeaderValue = '0')) then
      begin
        FCachable := false
      end
    end;

    if Headers.Status = 200 then
    begin
      Cache.WriteData(Line, cdtResponse);
    end
    else if FileExists(CacheResponseFile) then
    begin
      FileSetDate(CacheResponseFile, DateTimeToFileDate(Now));
    end;
  end;

  if Settings.GetBoolean(Settings.IndexOf('enable-mime-guess')) or (Headers.ContentType = ftXml) then
  begin
    Headers.ContentType := GetType;
  end;

  Cache.Info^.HeaderRec := Headers;

  if FCachable and (Headers.Status = 200) then
  begin
    Cache.WriteData(IntToStr(Ord(Cache.Info^.CacheType)) + Tab + Cache.Info^.CacheParam + Tab + IntToStr(Ord(Cache.Info^.HeaderRec.ContentType)), cdtInfo);
  end;
end;

constructor THttpSock.Create(Host, Port, Path, Search: String);
begin
  inherited Create(Host, Port);
  Connect(Host, Port, Path, Search);
  InitCache;

  Method := 'GET';
  FChunkedLength := 0;
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

  FCachable := true;
  Headers.ContentType := ftUnset;
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
  Cache.Free;

{$IFNDEF __GPC__}
  inherited Destroy;
{$ENDIF}
end;

procedure THttpSock.Execute;
begin
  inherited Execute;
  SendHeaders;
end;

procedure THttpSock.SendHeader(Name: String);
begin
  Sock.SendString(Name);
  Sock.SendString(#13#10);
end;

procedure THttpSock.SendHeaders;
var
  CacheHeader: String;
begin
  CacheHeader := Cache.GetCacheHeader;

  SendHeader(Method + ' ' + FPath + ' HTTP/1.1');
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

procedure THttpSock.InitCache;
var
  FullPath: String;
begin
  FullPath := FIndirectHost + FPath;
  FullPath := StringReplace(FullPath, '/', '_', [rfReplaceAll]);
  FullPath := StringReplace(FullPath, '?', '__', [rfReplaceAll]);

  Cache := CreateCache(FullPath);
  Cache.Info^.CacheParam := '';
end;

function THttpSock.ParseItem(var Item: TFeedItem): Boolean;
var
  Ext: String;
  Res: Boolean;
begin
  if Headers.Status = 0 then
  begin
    GetHeaders;
  end;

  FeedType := Headers.ContentType;

  if Headers.Status = 200 then
  begin
    Res := inherited ParseItem(Item);
  end
  else if (Headers.Status = 304) and not Settings.GetBoolean(Settings.IndexOf('hide-cached-feeds')) then
  begin
    CurrentCache := nil;
    Ext := Cache.GetFeedExtension(Headers.ContentType);
    if Ext = 'unknown' then
    begin
      ParseItem := true;
      Exit;
    end;

    if not Assigned(FLocal) then
    begin
      FLocal := GetLocalFile(FCacheDir + PathDelim +
        CacheFeedFile + '.' + Cache.GetFeedExtension(Headers.ContentType));
      FLocal.Execute;
    end;

    Res := FLocal.ParseItem(Item);

    if Res then
    begin
      FLocal.Free;
    end;

    ShouldShow := not Res;
  end
  else
  begin
    Res := true;
  end;

  ParseItem := Res
end;

function THttpSock.GetType: TFeedType;
var
  Line: String;
function IsAtom: Boolean;
  Line := LowerCase(FIndirectHost + FPath);
  end;
  GetType := Headers.ContentType;
end;
  if Pos('atom', Line) <> 0 then
  if Pos('esf', Line) <> 0 then
    GetType := ftAtom;
  end
  else if Pos('esf', Line) <> 0 then
  if ContentType <> ftRss then
    GetType := ftEsf;
  end
  else if (Pos('rss3', Line) <> 0) or (Pos('r3', Line) <> 0) or ((Pos('rss', Line) <> 0) and (Pos('3', Line) <> 0)) or (Pos('txt', Line) <> 0) then
  if b then
    GetType := ftRss3;
  end
  else if (Pos('rss', Line) <> 0) or (Pos('rdf', Line) <> 0) or (Pos('xml', Line) <> 0) then
    (Pos('xml', Line) <> 0) then
    GetType := ftRss;
    end;
  GetType := ContentType;
end;

end.
