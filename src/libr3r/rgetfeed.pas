unit RGetFeed;

{$IFNDEF SOCKETS_SYNAPSE}
{$UNDEF USE_IDN}
{$UNDEF USE_LIBIDN2}
{$ENDIF}

interface

uses
  LibR3R, RParseURL, RSock
{$IFDEF SOCKETS_SYNAPSE}
  , Http, RMessage
{$ENDIF};

function GetFeed(var Resource: String): TURL;
procedure ParseFeed(const Sock: TRSock);
procedure SetFeedObject(const Lib: TLibR3R);

implementation

uses
{$IFDEF USE_IDN}
  LibIdn, 
{$ELSE}
{$IFDEF USE_LIBIDN2}
  LibIdn2, LibIntl,
{$ENDIF}
{$ENDIF}
  HttpCache, LibR3RStrings, RFilter, RStrings, SysUtils
{$IFDEF __GPC__}
  , GPC
{$ENDIF};

var
  FeedObj: TLibR3R;
  Item: TFeedItem;

function GetFeed(var Resource: String): TURL;
{$IFDEF __GPC__}
const
  PathDelim = DirSeparator;
{$ENDIF}
var
  ExplicitFile: Boolean;
  Prot: String;
  Res: TURL;
{$IF DEFINED(USE_IDN) or DEFINED(USE_LIBIDN2)}
  PHost: PChar;
{$ENDIF}
begin
  ExplicitFile := Pos('file://', Resource) = 1;
  if ExplicitFile then
  begin
    Resource := Copy(Resource, 8, Length(Resource) - 7);
  end;

  if ExplicitFile or FileExists(Resource) then
  begin
    Prot := 'file';

    if ExtractFilePath(Resource) = '' then
    begin
      Resource := GetCurrentDir + PathDelim + Resource
    end;
  end
  else
  begin
{$IFNDEF SOCKETS_NONE}
    Res := ParseURL(Resource);
{$IFDEF USE_IDN}
    idna_to_ascii_8z(StrToPChar(Res.Host), @PHost, 0);
    WriteStr(Res.Host, PHost);
{$ELSE}
{$IFDEF USE_LIBIDN2}
    setlocale(LC_ALL, '');
    idn2_lookup_ul(StrToPChar(Res.Host), @PHost, 0);
    WriteStr(Res.Host, PHost);
{$ENDIF}
{$ENDIF}
{$ENDIF}
  end;

  if Prot <> '' then
  begin
    Res.Protocol := Prot;
  end;
  GetFeed := Res;
end;

procedure ParseFeed(const Sock: TRSock);
var
  Finished: Boolean;
  UseFilters: Boolean;
begin
  Finished := false;
  UseFilters := Settings.GetBoolean('use-filters');

  while not Finished do
  begin
{$IFDEF SOCKETS_SYNAPSE}
    if Assigned(Sock.Sock) and Sock.Error then
    begin
      CallMessageEvent(Sock, true, ErrorGetting, THttpSock(Sock).FURL);
      Break;
    end;
{$ENDIF}

    Finished := Sock.ParseItem(Item);

    if Sock.ShouldShow then
    begin
      Item.Translate;

      if UseFilters then
      begin
        FilterItem(Item);
      end;
    end;
  end;
end;

procedure SetFeedObject(const Lib: TLibR3R);
begin
  FeedObj := Lib;
end;

initialization

Item := TFeedItem.Create;
Item.Clear;

finalization

Item.Free;

end.
