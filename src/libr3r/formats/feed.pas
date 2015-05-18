unit Feed;

interface

uses
  FeedItem;

type
  TFeedType = (ftUnset, ftXml, ftAtom, ftEsf, ftRss, ftRss3, ftUnknown);

  TFeed = class
  protected
    function GetFormat: TFeedType; virtual; abstract;
  public
    ShouldShow: Boolean;
    constructor Create;
    destructor Destroy; override;
    function GetAbsoluteURL(const RelURL, BaseURL: String): String;
    procedure ParseLine(Line: String; var Item: TFeedItem); virtual;
  end; 

implementation

uses
  HttpCache, RSettings, SockConsts, URIParser;

constructor TFeed.Create;
begin
  inherited Create;
  ShouldShow := true;
end;

destructor TFeed.Destroy;
begin
  inherited Destroy;
end;

procedure TFeed.ParseLine(Line: String; var Item: TFeedItem);
begin
  if Assigned(CurrentCache) and (Line <> SockEof) then
  begin
    CurrentCache.Info^.HeaderRec.ContentType := GetFormat;
    CurrentCache.WriteData(Line, cdtFeed);
  end;
end;

function TFeed.GetAbsoluteURL(const RelURL, BaseURL: String): String;
var
  Base, Rel: TURI;
  Port: String;
  Res: String;
begin
{$IFNDEF SOCKETS_NONE}
  Rel := ParseURI(RelURL);
{$ENDIF}

  { URL is absolute if it contains a protocol;
    it isn't if it doesn't }
  if (Pos(Rel.Protocol, RelURL) <> 0) or (BaseURL = '') then
  begin
    Res := RelURL;
  end
  else
  begin
{$IFNDEF SOCKETS_NONE}
    Base := ParseURI(BaseUrl);
{$ENDIF}
    Res := Base.Protocol + '://';

    if Base.Username <> '' then
    begin
      Res := Res + Base.Username;
      if Base.Password <> '' then
      begin
        Res := Res + ':' + Base.Password;
      end;

      Res := Res + '@';
    end;

    Res := Res + Base.Host;
    
    if Rel.Port <> Base.Port then
    begin
      WriteStr(Port, Base.Port);
      Res := Res + ':' + Port;
    end;

    Res := Res + Rel.Path + Rel.Document;

    if Rel.Params <> '' then
    begin
      Res := Res + '?' + Rel.Params;
    end;
  end;

  GetAbsoluteURL := Res;
end;

end.
