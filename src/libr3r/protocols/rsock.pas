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
  ,ssl_openssl
{$ENDIF}
{$ENDIF}
  
{$IFDEF SOCKETS_BSD}
  , SockWrap
{$ENDIF}
  
{$IFDEF SOCKETS_LIBCURL}
  , CurlCore, CurlEasy, RList, RStrings
{$ENDIF};

type
  TRSock = class
  private
  protected
    FChunkedLength: integer;
    FError: Boolean;
    FFeedType: TFeedType;
{$IFDEF SOCKETS_LIBCURL}
    FHandle: CURL;
{$ENDIF}
    FHost: String;
    FPort: String;
    FShouldShow: Boolean;
    FUseChunked: Boolean;
    FeedType: TFeedType;
  public
{$IFDEF SOCKETS_SYNAPSE}
    Sock: TTCPBlockSocket;
{$ENDIF}
{$IFDEF SOCKETS_BSD}
    Sock: TSockWrap;
{$ENDIF}
{$IFDEF SOCKETS_LIBCURL}
    FList: PRStringList;
    FTempList: PRStringList;
{$ENDIF}

    ShouldShow: Boolean;
    constructor Create(Host, Port: String);
    destructor Destroy; {$IFNDEF __GPC__}override;{$ELSE}virtual;{$ENDIF}
    procedure DomainSet(Host, Port: String);
    procedure Execute; virtual;
    function GetLine: String; virtual;
    function ParseItem(var Item: TFeedItem): Boolean; virtual;
    function Error: Boolean;
  end;

implementation

{$IF DEFINED(SOCKETS_CURL) or DEFINED(SOCKETS_LIBCURL)}
{$DEFINE SOCKETS_NONE}
{$ENDIF}

uses
  Atom, Esf, Rss, Rss3, SockConsts, SysUtils;

var
  FAbstractFeed: TFeed;

{$IFDEF SOCKETS_LIBCURL}
const
  CURL_BUFFER_SIZE = 85;
  CURL_MAX_BUFFER_SIZE = CURL_BUFFER_SIZE * 3;

{$CALLING CDECL}
function WriteData(ptr: PChar; size, nmemb: size_t; UserData: Pointer): size_t;
var
  i, j, k: PtrUInt;
  s: String;
  t: String;
begin
  s := StrPas(ptr);
  t := '';

  repeat
    with TRSock(UserData) do
    begin
      i := Pos(#10, s);
      if i > 0 then
      begin
        if FTempList^.Count > 0 then
        begin
          for j := 0 to FTempList^.Count - 1 do
          begin
            t := t + FTempList^.GetNth(j);
            if Length(t) + Length(FTempList^.GetNth(j + 1)) > CURL_MAX_BUFFER_SIZE then
            begin
              k := Length(t) + 1;

              repeat
                Dec(k)
              until (t[k] in [#0, #8, #9, #10, #13, #32]) or (k = 1);

              if k > 1 then
              begin
                { blank spaces for RSS 3.0 }
                FList^.Add('   ' + Copy(t, 1, k));
                Delete(t, 1, k);
              end;
            end;
          end;

          FTempList^.Clear;
          FTempList^.Add('');
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


{$IFNDEF SOCKETS_NONE}
function CreateSocket:
{$IFDEF SOCKETS_SYNAPSE}
TTCPBlockSocket
{$ENDIF}

{$IFDEF SOCKETS_BSD}
TSockWrap
{$ENDIF};
begin
  
{$IFDEF SOCKETS_SYNAPSE}
  CreateSocket := TTCPBlockSocket.Create;
{$ENDIF}

{$IFDEF SOCKETS_BSD}
  CreateSocket := TSockWrap.Create;
{$ENDIF}
end;
{$ENDIF}

procedure SetAbstractFeed(const FeedType: TFeedType);
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
end;

constructor TRSock.Create(Host, Port: String);
begin
{$IFNDEF __GPC__}
  inherited Create;
{$ENDIF}

{$IFNDEF SOCKETS_NONE}
  Sock := CreateSocket;
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
  FShouldShow := true;
end;

destructor TRSock.Destroy;
begin
{$IFNDEF SOCKETS_NONE}
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
    Dispose(FTempList, Done);
  end;

  curl_easy_cleanup(FHandle);
{$ENDIF}

  FAbstractFeed.Free;
  FAbstractFeed := nil;

{$IFNDEF __GPC__}
  if Self.ClassName <> 'TLocalFile' then
  begin
    inherited Destroy;
  end;
{$ENDIF}
end;

procedure TRSock.DomainSet(Host, Port: String);
begin
  FHost := Host;
  FPort := Port;
  FUseChunked := false;

{$IFDEF SOCKETS_LIBCURL}
  curl_easy_setopt(FHandle, CURLOPT_URL, StrToPChar(Host));
  curl_easy_setopt(FHandle, CURLOPT_PORT, @Port);
{$ENDIF}
end;

function TRSock.GetLine: String;
begin
{$IFNDEF SOCKETS_NONE}
  GetLine := Sock.RecvString(5000);
{$IFDEF __GPC__}
  FError := (Sock.LastError < 0) or (Sock.LastError > 2);
{$ELSE}
  FError := Sock.LastError <> 0;
{$ENDIF}
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

procedure TRSock.Execute;
begin
{$IFNDEF SOCKETS_NONE}
  Sock.Connect(FHost, FPort);
  Sock.ConvertLineEnd := true;
{$ENDIF}
end;

function TRSock.ParseItem(var Item: TFeedItem): Boolean;
var
  ErrPos: word;
  Len: word;
  Line, Tmp: String;
begin
  ShouldShow := true;
  SetAbstractFeed(FeedType);

  if FAbstractFeed = nil then
  begin
    ParseItem := true;
    Exit;
  end;
  
  with FAbstractFeed do
  begin
    Item.Clear;
    repeat
      Line := GetLine;

      if FUseChunked then
      begin
        Tmp := Trim(Line);
        if FChunkedLength > 0 then
        begin
          Dec(FChunkedLength, Length(Line) + 2);
        end
        else if Tmp <> '' then
        begin
          Val('$' + Tmp, Len, ErrPos);

          if ErrPos = 0 then
          begin
            FChunkedLength := Len + 1;
            Continue;
          end;
        end;
      end;

      ParseLine(Line, Item);
    until Item.Finished;
  end;

  ShouldShow := ShouldShow and FAbstractFeed.ShouldShow;
  ParseItem := Line = SockEof;
end;

function TRSock.Error: Boolean;
begin
  Error := FError;
end;

end.
