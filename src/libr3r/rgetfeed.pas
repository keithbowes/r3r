unit RGetFeed;

{$IFNDEF SOCKETS_SYNAPSE}
{$UNDEF USE_IDN}
{$UNDEF USE_LIBIDN2}
{$ENDIF}

interface

uses
  LibR3R, RSock, URIParser
{$IFDEF SOCKETS_SYNAPSE}
  , RMessage
{$ENDIF};

function GetFeed(var Resource: String): TURI;
procedure ParseFeed(const Sock: TRSock);

implementation

uses
{$IFDEF USE_IDN}
  LibIdn, 
{$ELSE}
{$IFDEF USE_LIBIDN2}
  LibIdn2, LibIntl,
{$ENDIF}
{$ENDIF}
  HttpCache, LibR3RStrings, RFilter, RStrings, SysUtils;

var
  Item: TFeedItem;

function GetFeed(var Resource: String): TURI;
var
  ExplicitFile: Boolean;
  Prot: String;
  Res: TURI;
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
    Res := ParseURI(Resource);
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
      CallMessageEvent(Sock, true, ErrorGetting + ' ' + Sock.Sock.GetErrorDescEx, Sock.FURL);
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

initialization

Item := TFeedItem.Create;
Item.Clear;

finalization

Item.Free;

end.
