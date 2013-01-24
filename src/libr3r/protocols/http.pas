unit Http;

{$IFDEF SOCKETS_LIBCURL}
{$DEFINE USE_SSL}
{$ENDIF}

interface

uses
  Feed, FeedItem, Headers, HttpCache, LocalFile, RSock
{$IFDEF SOCKETS_LIBCURL}
  , CurlCore, CurlEasy, CurlSList
{$ENDIF};

type
  THttpSock = class(TRSock)
  private
    FCachable: Boolean;
{$IFDEF SOCKETS_LIBCURL}
    FHeaderList: Pcurl_slist;
{$ENDIF}
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
{$IFDEF __GPC__}
  GPC,
{$ENDIF}
{$IFDEF USE_ICONV}
  RProp,
{$ENDIF}
  Info, LibR3RStrings, RGetFeed, RMessage, RSettings,
  RStrings, StrTok, SysUtils;

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
  FH: PtrInt;
  HeaderName, HeaderValue: String;
  HeaderState: THeaderState;
  Host, Para, Path, Port, Prot: String;
  i: byte;
  InfoText: String;
  Line: String;
  RespList: TStringsList;
begin
  Headers.Sniff := Settings.GetBoolean('enable-mime-guess');
  HeaderState := hsUnstarted;

  while (HeaderState <> hsFinished) do
  begin
    Line := Trim(GetLine);

    if (Line = '') and (HeaderState = hsStarted) then
    begin
      HeaderState := hsFinished;
    end
    else if (Line <> '') and (HeaderState = hsUnstarted) then
    begin
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

{$IFDEF USE_ICONV}
        RespList := Split(HeaderValue, ';');
        for i := 0 to RespList.Length - 1 do
        begin
          if Pos('charset=', RespList.Strings[i]) <> 0 then
          begin
            RespList := Split(Trim(RespList.Strings[i]), '=');
            Headers.Charset := Trim(RespList.Strings[1]);
            SetProp('charset', StrToPChar(Headers.Charset));
            Break;
          end;
        end;
{$ENDIF}
      end
      else if HeaderName = 'x-content-type-options' then
      begin
        if Pos('nosniff', HeaderValue) <> 0 then
        begin
          Headers.Sniff := false;
        end;
      end
      else if HeaderName = 'location' then
      begin
        Headers.Status := 0;
{$IFNDEF SOCKETS_LIBCURL}
        Sock.CloseSocket;
{$ELSE}
        curl_easy_cleanup(FHandle);
{$ENDIF}
        ShouldShow := true;

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
      FH := FileOpen(CacheResponseFile, fmOpenRead);
      FileSetDate(FH, DateTimeToFileDate(Now));
      FileClose(FH);
    end;
  end;

  if Headers.Sniff then
  begin
    Headers.ContentType := GetType;
  end;

  Cache.Info^.HeaderRec := Headers;

  if FCachable and (Headers.Status = 200) then
  begin
    WriteStr(InfoText, Ord(Cache.Info^.CacheType), Tab, Cache.Info^.CacheParam, Tab, Ord(Cache.Info^.HeaderRec.ContentType), Tab, Headers.Charset);
    Cache.WriteData(InfoText, cdtInfo);
  end;
end;

constructor THttpSock.Create(Host, Port, Path, Search: String);
begin
  inherited Create(Host, Port);
  Connect(Host, Port, Path, Search);
  InitCache;

  Method := 'GET';
  FChunkedLength := 0;

{$IFDEF SOCKETS_LIBCURL}
  curl_easy_setopt(FHandle, CURLOPT_HEADER, Pointer(1));
  FHeaderList := nil;
{$ENDIF}
end;

procedure THttpSock.Connect(Host, Port, Path, Search: String);
var
  UseProxy: Boolean;
begin
  FIndirectHost := Host;

{$IFNDEF SOCKETS_LIBCURL}
  if (Port <> '80')
{$IFDEF USE_SSL}
  and (Port <> '443')
{$ENDIF}
  then
  begin
    FIndirectHost := FIndirectHost + ':' + Port;
  end;
{$ENDIF}

  UseProxy := Settings.GetBoolean('use-proxy');
  if UseProxy then
  begin
{$IFNDEF SOCKETS_LIBCURL}
    Host := Settings.GetString('proxy-address');
    WriteStr(Port, Settings.GetInteger('proxy-port'));
{$ELSE}
    curl_easy_setopt(FHandle, CURLOPT_PROXY, StrToPChar(Settings.GetString('proxy-address')));
    curl_easy_setopt(FHandle, CURLOPT_PROXYPORT, Pointer(Settings.GetInteger('proxy-port')));
{$ENDIF}
  end;

  FCachable := true;
  Headers.ContentType := ftUnset;
  FPath := Path;

  if Search <> '' then
  begin
    FPath := FPath + '?' + Search
  end;

  FURL := 'http://' + FIndirectHost + FPath;

{$IFNDEF SOCKETS_LIBCURL}
  if UseProxy then
  begin
    FPath := FURL;
  end;
{$ENDIF}

{$IFDEF SOCKETS_LIBCURL}
  curl_easy_setopt(FHandle, CURLOPT_URL, StrToPChar(FURL));
{$ENDIF}
end;

destructor THttpSock.Destroy;
begin
  Cache.Free;

{$IFDEF SOCKETS_LIBCURL}
  if Assigned(FHeaderList) then
  begin
    curl_slist_free_all(FHeaderList);
  end;
{$ENDIF}

{$IFNDEF __GPC__}
  inherited Destroy;
{$ENDIF}
end;

procedure THttpSock.Execute;
begin
  inherited Execute;
  SendHeaders;

{$IFDEF SOCKETS_LIBCURL}
  curl_easy_perform(FHandle);
{$ENDIF}
end;

procedure THttpSock.SendHeader(Name: String);
begin
{$IFNDEF SOCKETS_LIBCURL}
  Sock.SendString(Name);
  Sock.SendString(#13#10);
{$ELSE}
  FHeaderList := curl_slist_append(FHeaderList, StrToPChar(Name));
{$ENDIF}
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

  if Settings.GetBoolean('use-custom-accept-langs') then
  begin
    SendHeader('Accept-Language: ' + Settings.GetString('accept-langs'));
  end;

  if Settings.GetBoolean('use-custom-accept-types') then
  begin
    SendHeader('Accept: ' + Settings.GetString('accept-types'));
  end
  else
  begin
    SendHeader('Accept: text/plain, application/atom+xml, application/rdf+xml, application/xml;q=0.5');
  end;

  SendHeader('Accept-Encoding: ');
  SendHeader('Connection: close');
  SendHeader('');

{$IFDEF SOCKETS_LIBCURL}
  curl_easy_setopt(FHandle, CURLOPT_HTTPHEADER, FHeaderList);
{$ENDIF}
end;

procedure THttpSock.InitCache;
var
  FullPath: String;
begin
  FullPath := FIndirectHost + FPath;
  FullPath := CacheEncode(FullPath);

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
  else if (Headers.Status = 304) and not Settings.GetBoolean('hide-cached-feeds') then
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
{$IFDEF USE_ICONV}
      SetProp('charset', {$IFNDEF __GPC__}PChar(Cache.GetEncoding){$ELSE}StrToPChar(Cache.GetEncoding){$ENDIF});
{$ENDIF}
      FLocal := GetLocalFile(FCacheDir + PathDelim + CacheFeedFile +
        '.' + Cache.GetFeedExtension(Headers.ContentType));
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
  ContentType: TFeedType;
  Line: String;
function IsAtom: Boolean;
begin
  if Pos('atom', Line) <> 0 then
  begin
    ContentType := ftAtom;
  end;

  IsAtom := ContentType = ftAtom;
end;

function IsEsf: Boolean;
begin
  if Pos('esf', Line) <> 0 then
  begin
    ContentType := ftEsf;
  end;

  IsEsf := ContentType = ftEsf;
end;

function IsRss3: Boolean;
var
  b: Boolean;
begin
  b := (Pos('rss3', Line) <> 0) or (Pos('r3', Line) <> 0);

  if ContentType <> ftRss then
  begin
     b := b or ((Pos('rss', Line) <> 0) and (Pos('3', Line) <> 0));
  end;
  
  b := b or (Pos('txt', Line) <> 0);

  if b then
  begin
    ContentType := ftRss3;
  end;

  IsRss3 := ContentType = ftRss3;
end;

function IsRss: Boolean;
begin
  if (Pos('rss', Line) <> 0) or (Pos('rdf', Line) <> 0) or
    (Pos('xml', Line) <> 0) then
  begin
    ContentType := ftRss;
  end;

  IsRss := ContentType = ftRss;
end;

begin
  Line := LowerCase(FIndirectHost + FPath);
  ContentType := Headers.ContentType;

  case ContentType of
    ftRss:
    begin
      if not IsAtom then
      begin
        IsRss3;
      end;
    end;
    ftUnknown:
    begin
      if not IsAtom then
      begin
        if not IsEsf then
        begin
          if not IsRss3 then
          begin
            IsRss;
          end;
        end;
      end;
    end;
    ftXml:
    begin
      if not IsAtom then
      begin
        IsRss;
    end;
    end;
  end;

  GetType := ContentType;
end;

end.
