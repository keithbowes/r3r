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

function ContinueParsing(const Sock: TRSock): Boolean;
function GetFeed(var Resource: String): TURI;

implementation

uses
{$IFDEF USE_IDN}
  LibIdn, 
{$ELSE}
{$IFDEF USE_LIBIDN2}
  LibIdn2, LibIntl,
{$ENDIF}
{$ENDIF}
  LibR3RStrings, RFilter, RStrings, SysUtils;

var
  Item: TFeedItem;

function ContinueParsing(const Sock: TRSock): Boolean;
var
  UseFilters: Boolean;
begin
{$IFDEF SOCKETS_SYNAPSE}
  if Assigned(Sock.Sock) and Sock.Error then
  begin
    CallMessageEvent(Sock, true, ErrorGetting + ' ' + Sock.Sock.GetErrorDescEx, Sock.FURI);
    Result := false;
    Exit;
  end;
{$ENDIF}

  Result := Sock.ParseItem(Item);

  if Sock.ShouldShow then
  begin
    Item.Translate;

    UseFilters := Settings.GetBoolean('use-filters');
    if UseFilters then
    begin
      FilterItem(Item);
    end;
  end;
end;

function GetFeed(var Resource: String): TURI;
var
  ExplicitFile: Boolean;
  Prot: String;
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
    Result := ParseURI(Resource);
{$IFDEF USE_IDN}
    idna_to_ascii_8z(StrToPChar(Result.Host), @PHost, 0);
    WriteStr(Result.Host, PHost);
{$ELSE}
{$IFDEF USE_LIBIDN2}
    setlocale(LC_ALL, '');
    idn2_lookup_ul(StrToPChar(Result.Host), @PHost, 0);
    WriteStr(Result.Host, PHost);
{$ENDIF}
{$ENDIF}
{$ENDIF}
  end;

  if Prot <> '' then
  begin
    Result.Protocol := Prot;
  end;
end;

initialization

Item := TFeedItem.Create;
Item.Clear;

finalization

Item.Free;

end.
