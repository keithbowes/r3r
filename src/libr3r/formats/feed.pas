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
    destructor Destroy; {$IFNDEF __GPC__}override{$ELSE}virtual{$ENDIF};
    function GetAbsoluteURL(const RelURL, BaseURL: String): String;
    procedure ParseLine(Line: String; var Item: TFeedItem); virtual;
  end; 

implementation

uses
  HttpCache, RParseURL, RSettings, SockConsts;

constructor TFeed.Create;
begin
{$IFNDEF __GPC__}
  inherited Create;
{$ENDIF}

  ShouldShow := true;
end;

destructor TFeed.Destroy;
begin
{$IFNDEF __GPC__}
  inherited Destroy;
{$ENDIF}
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
  Base, Rel: TURL;
  Res: String;
begin
{$IFNDEF SOCKETS_NONE}
  Rel := ParseURL(RelURL);
{$ENDIF}

  { URL is absolute if it contains a protocol;
    it isn't if it doesn't }
  if Pos(Rel.Protocol, RelURL) <> 0 then
  begin
    Res := RelURL;
  end
  else
  begin
{$IFNDEF SOCKETS_NONE}
    Base := ParseURL(BaseUrl);
{$ENDIF}
    Res := Base.Protocol + '://';

    if Base.User <> '' then
    begin
      Res := Res + Base.User;
      if Base.Password <> '' then
      begin
        Res := Res + ':' + Base.Password;
      end;

      Res := Res + '@';
    end;

    Res := Res + Base.Host;
    
    if Rel.Port <> Base.Port then
    begin
      Res := Res + ':' + Base.Port;
    end;

    Res := Res + Rel.Path;

    if Rel.Search <> '' then
    begin
      Res := Res + '?' + Rel.Search;
    end;
  end;

  GetAbsoluteURL := Res;
end;

end.
