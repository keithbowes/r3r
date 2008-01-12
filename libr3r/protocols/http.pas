unit Http;

interface

uses
  FeedItem, Headers, RSock;

type
  THttpSock = class(TRSock)
  private
    FHeaders: THeaders;
    FPath: String;
  protected
    procedure GetHeaders;
    procedure SendHeader(const Name: String);
    property Headers: THeaders read FHeaders;
  public
    constructor Create(Host, Port, Path, Search: String);
    procedure Execute; override;
    function ParseItem(var Item: TFeedItem): Boolean; override;
  end;

implementation

uses
  BlckSock, Feed, Info, RegExpr, SysUtils;

type
  THeaderState = (hsUnstarted, hsStarted, hsFinished);

procedure THttpSock.GetHeaders;
var
  ColonIndex: integer;
  HeaderName, HeaderValue: String;
  HeaderState: THeaderState;
  Line: String;
begin
  HeaderState := hsUnstarted;

  while (HeaderState <> hsFinished) and (FHeaders.ContentType = ftUnset) do
  begin
    Line := GetLine;
    if (Line = '') and (HeaderState = hsStarted) then
    begin
      HeaderState := hsFinished;
    end
    else if (Line <> '') and (HeaderState = hsUnstarted) then
    begin
     HeaderState := hsStarted;
    end
    else
    begin
      ColonIndex := Pos(':', Line);
      HeaderName := Copy(Line, 0, ColonIndex - 1);
      HeaderValue := Trim(Copy(Line, ColonIndex + 1, Length(Line) - ColonIndex));

      if HeaderName = 'Date' then
      begin
        FHeaders.Date := HeaderValue;
      end
      else if HeaderName = 'Content-Encoding' then
      begin
        FHeaders.ContentEncoding := HeaderValue;
      end
      else if HeaderName = 'Content-Type' then
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
        else if (Pos('rss', HeaderValue) <> 0) or (Pos('xml', HeaderValue) <> 0) then
        begin
          FHeaders.ContentType := ftRss;
        end
        else
        begin
          FHeaders.ContentType := ftUnknown;
        end;
      end;
    end;
  end;
end;

constructor THttpSock.Create(Host, Port, Path, Search: String);
begin
  inherited Create(Host, Port);
  FHeaders.ContentType := ftUnset;
  FPath := Path;

  if Search <> '' then
  begin
    FPath := FPath + '?' + Search
  end
end;

procedure THttpSock.Execute;
begin
  inherited Execute;

  SendHeader('GET ' + FPath + ' HTTP/1.1');
  SendHeader('Host: ' + FHost);
  SendHeader('User-Agent: R3R/' + Version + ' (' + Os + ') Synapse/' +
    SynapseRelease + ' TRegExpr/' + IntToStr(TRegExpr.VersionMajor) + '.' +
    IntToStr(TRegExpr.VersionMinor));
  SendHeader('Accept-Encoding: ');
  SendHeader('Accept: text/plain, esf/text, */*');
  SendHeader('Connection: close');
  SendHeader('');
end;

procedure THttpSock.SendHeader(const Name: String);
begin
  Sock.SendString(Name);
  Sock.SendString(#13#10);
end;

function THttpSock.ParseItem(var Item: TFeedItem): Boolean;
begin
  if Headers.ContentType = ftUnset then
  begin
    GetHeaders;
  end;

  FeedType := Headers.ContentType;

  Result := inherited ParseItem(Item);
end;

end.
