unit RSock;

interface

{$IFDEF SOCKETS_LIBCURL}
{$DEFINE USE_SSL}
{$ENDIF}

uses
  Feed, FeedItem
{$IFDEF SOCKETS_SYNAPSE}
  , BlckSock
{$IFDEF USE_SSL}
  , ssl_openssl
{$ENDIF}
{$ENDIF}
  
{$IFDEF SOCKETS_LIBCURL}
  , CurlCore, CurlEasy, RList, RStrings
{$ENDIF};

type
  TRSock = class
  private
    FAbstractFeed: TFeed;
    procedure SetAbstractFeed(const FeedType: TFeedType);
  protected
    FChunkedLength: integer;
    FError: Boolean;
{$IFDEF SOCKETS_LIBCURL}
    FHandle: CURL;
{$ENDIF}
    FHost: String;
{$IFDEF USE_SSL}
    FIsSecure: Boolean;
{$ENDIF}
    FPort: word;
    FUseChunked: Boolean;
    FeedType: TFeedType;
  public
{$IFDEF SOCKETS_SYNAPSE}
    Sock: TTCPBlockSocket;
{$ENDIF}
{$IFDEF SOCKETS_LIBCURL}
    FFirst: Boolean;
    FList: PRStringList;
    FTempList: PRStringList;
{$ENDIF}

    FURI: String;
    ShouldShow: Boolean;
    constructor Create(Host: String; Port: word);
    destructor Destroy; override; 
    procedure DomainSet(Host: String; Port: word);
    procedure Open; virtual;
    function GetLine: String; virtual;
    function ParseItem(var Item: TFeedItem): Boolean; virtual;
    function Error: Boolean;
    property ChunkedLength: integer read FChunkedLength;
  end;

implementation

uses
  Atom, Esf, Rss, Rss3,
  LibR3RStrings, RMessage, RSettings, SockConsts, SysUtils;

const
{$IFDEF SOCKETS_SYNAPSE}
  TimeoutMilliseconds = 5000;
{$ENDIF}
{$IFDEF SOCKETS_LIBCURL}
  CURL_BUFFER_SIZE = 85;
  CURL_MAX_BUFFER_SIZE = CURL_BUFFER_SIZE * 3;

{$CALLING CDECL}
function WriteData(ptr: PChar; size, nmemb: size_t; UserData: Pointer): size_t;
var
  i, j, k: PtrUInt;
  s: String;
  t: String;
begin
  WriteStr(s, ptr);
  t := '';

  repeat
    with TRSock(UserData) do
    begin
      i := Pos(#10, s);
      if i > 0 then
      begin
        if FTempList^.Count > 0 then
        begin
          FFirst := true;
          for j := 0 to FTempList^.Count - 1 do
          begin
            t := t + FTempList^.GetNth(j);
            if Length(t) + Length(FTempList^.GetNth(j + 1)) > CURL_MAX_BUFFER_SIZE then
            begin
              k := Length(t) + 1;

              repeat
                Dec(k)
              until (t[k] in [#0, #8, #9, #10, #13, #32, '&']) or (k = 1);

              { Split before a character reference }
              if t[k] = '&' then
              begin
                FFirst := true;
                Dec(k)
              end;

              if k > 1 then
              begin
                if FFirst then
                begin
                  FList^.Add(Copy(t, 1, k));
                  Delete(t, 1, k);
                  FFirst := false;
                end
                else
                begin
                  { blank spaces for RSS 3.0 }
                  FList^.Add('   ' + Copy(t, 1, k));
                  Delete(t, 1, k);
                end;
              end;
            end;
          end;

          FTempList^.Clear;
        end;
        FList^.Add(t + Copy(s, 1, i));
        Delete(s, 1, i);
        t := '';
      end
      else
      begin
        FTempList^.Add(s);
        s := '';
      end;
    end;
  until Length(s) = 0;

  WriteData := size * nmemb;
end;
{$CALLING DEFAULT}
{$ENDIF}

procedure TRSock.SetAbstractFeed(const FeedType: TFeedType);
begin
  if not Assigned(FAbstractFeed) then
  begin
    if FeedType = ftEsf then
    begin
      FAbstractFeed := TEsfFeed.Create;
    end
    else if FeedType = ftRss3 then
    begin
      FAbstractFeed := TRss3Feed.Create;
    end
    else if (FeedType = ftRss) or (FeedType = ftXml) then
    begin
      FAbstractFeed := TRssFeed.Create;
    end
    else if FeedType = ftAtom then
    begin
      FAbstractFeed := TAtomFeed.Create;
    end
    else
    begin
      FAbstractFeed := nil;
    end;
  end;

  FAbstractFeed.Sock := Self;
end;

constructor TRSock.Create(Host: String; Port: word);
begin
  inherited Create;

{$IFDEF SOCKETS_SYNAPSE}
  Sock := TTCPBlockSocket.Create;
{$ENDIF}

{$IFDEF SOCKETS_LIBCURL}
  New(FList, Init);
  New(FTempList, Init);

  FHandle := curl_easy_init;
  FError := FHandle = nil;
  curl_easy_setopt(FHandle, CURLOPT_BUFFERSIZE, Pointer(CURL_BUFFER_SIZE));
  curl_easy_setopt(FHandle, CURLOPT_WRITEFUNCTION, @WriteData);
  curl_easy_setopt(FHandle, CURLOPT_WRITEDATA, Self);
{$ELSE}
  FError := false;
{$ENDIF}

  DomainSet(Host, Port);
{$IFDEF USE_SSL}
  FIsSecure := false;
{$ENDIF}
  ShouldShow := true;
end;

destructor TRSock.Destroy;
begin
{$IFDEF SOCKETS_SYNAPSE}
  if Assigned(Sock) then
  begin
    Sock.Free;
  end;
{$ENDIF}

{$IFDEF SOCKETS_LIBCURL}
  if Assigned(FList) then
  begin
    Dispose(FList, Done);
  end;

  if Assigned(FTempList) then
  begin
    FTempList^.Add(''); // Prevents a crash when destroying an empty list
    Dispose(FTempList, Done);
  end;

  curl_easy_cleanup(FHandle);
{$ENDIF}

  FAbstractFeed.Free;
  FAbstractFeed := nil;

  if Self.ClassName <> 'TLocalFile' then
  begin
    inherited Destroy;
  end;
end;

procedure TRSock.DomainSet(Host: String; Port: word);
begin
  FHost := Host;
  FPort := Port;
  FUseChunked := false;
  FChunkedLength := -1;

{$IFDEF SOCKETS_LIBCURL}
  curl_easy_setopt(FHandle, CURLOPT_URL, StrToPChar(Host));
  curl_easy_setopt(FHandle, CURLOPT_PORT, @Port);
{$ENDIF}
end;

function TRSock.GetLine: String;
begin
{$IFDEF SOCKETS_SYNAPSE}
  GetLine := Sock.RecvString(TimeoutMilliseconds);
  FError := Sock.LastError <> 0;
{$ENDIF}

{$IFDEF SOCKETS_LIBCURL}
  if FList^.Count > 0 then
  begin
    GetLine := TrimRight(FList^.GetNth(0));
    FList^.Delete(0);
  end
  else
  begin
    FError := true;
  end;
{$ENDIF}

  if FError then
  begin
    GetLine := SockEof;
  end;
end;

procedure TRSock.Open;
var
  Port: String;
begin
{$IFDEF SOCKETS_SYNAPSE}
  WriteStr(Port, FPort);
  //Sock.SetTimeout(TimeoutMilliseconds);
  Sock.Connect(FHost, Port);
{$IFDEF USE_SSL}
  if FIsSecure then
  begin
    Sock.SSLDoConnect;
    if Sock.LastError <> 0 then
    begin
      CallMessageEvent(Self, true, SSLError, FURI);
    end;
    Sock.SSL.VerifyCert := Settings.GetBoolean('ssl-verify');
  end;
{$ENDIF}
{$ENDIF}
end;

function TRSock.ParseItem(var Item: TFeedItem): Boolean;
var
  ErrPos: byte;
  Line: String;
  { Calling this for each line really hurts performance, so reading up to 10 lines per call seems like a fair compromise. }
  ParsedLines: 0..10 = 0;
begin
  ShouldShow := true;
  SetAbstractFeed(FeedType);

  if FAbstractFeed = nil then
  begin
    ParseItem := true;
    Exit;
  end;

  if Assigned(Sock) then
  begin
    Sock.ConvertLineEnd := not FUseChunked;
  end;
  
  with FAbstractFeed do
  begin
    repeat
      Line := GetLine;

      if FUseChunked then
      begin
        if FChunkedLength > 0 then
        begin
          Dec(FChunkedLength, Length(Line));
        end
        else
        begin
          Val('$' + Line[1..Pos(';', Line) - 1], FChunkedLength, ErrPos);
          if ErrPos = 0 then
          begin
            FUseChunked := FChunkedLength <> 0;
            Exit(true);
          end;
        end;
      end;

      Inc(ParsedLines);
      ParseLine(Line, Item);
    until (ParsedLines = 10) or (Item.Finished);
  end;

  ShouldShow := ShouldShow and FAbstractFeed.ShouldShow;
  ParseItem := Line <> SockEof;
end;

function TRSock.Error: Boolean;
begin
  Error := FError;
end;

end.
