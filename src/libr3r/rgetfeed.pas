unit RGetFeed;

{$IFDEF SOCKETS_CURL}
{$DEFINE SOCKETS_NONE}
{$ENDIF}

interface

uses
  LibR3R, RSock, RMessage
{$IFDEF SOCKETS_CURL}
  , DOS, Info
{$ENDIF}

{$IFNDEF SOCKETS_NONE}
  , RParseURL
{$ENDIF};

procedure GetFeed(var Resource: String; var Prot, Host, Port, Path, Para: String);
procedure ParseFeed(const Sock: TRSock);
procedure SetFeedObject(const Lib: TLibR3R);

implementation

uses
{$IFDEF USE_IDN}
  LibIdn, 
{$ENDIF}
  HttpCache, LibR3RStrings, RFilter, RSettings_Routines,
  RStrings, SysUtils
{$IFDEF __GPC__}
  , GPC
{$ENDIF};

var
  FeedObj: TLibR3R;
  Item: TFeedItem;

procedure GetFeed(var Resource: String; var Prot, Host, Port, Path, Para: String);
{$IFDEF __GPC__}
const
  PathDelim = DirSeparator;
{$ENDIF}
var
  ExplicitFile: Boolean;
  Pass, User: String;
{$IFDEF USE_IDN}
  PHost: PChar;
{$ENDIF}
{$IFDEF SOCKETS_CURL}
  f1, f2: TSearchRec;
  OrigResource, Resource2: String;
{$ENDIF}
begin
{$IFDEF SOCKETS_CURL}
  Prot := 'file';

  if not FileExists(Resource) then
  begin
    OrigResource := Resource;
    Resource := CacheDir + 'curl';
    CheckDir(Resource);
    Resource := Resource + PathDelim + CacheEncode(OrigResource);
  end;

  if FileExists(Resource) then
  begin
    Resource2 := StringReplace(Resource, 'http:', '2http:', []);
  end
  else
  begin
    Resource2 := Resource;
  end;

  SwapVectors;
  Exec(FSearch('curl', GetEnv('PATH')), '-ks -A "' + UserAgent + '" -o "' + Resource2 + '" "' + OrigResource + '"');
  SwapVectors;

  if DosExitCode <> 0 then
  begin
    Resource := '';
    Exit;
  end;

  FindFirst(Resource, AnyFile, f1);
  FindFirst(Resource2, AnyFile, f2);
  if (Settings.GetBoolean(Settings.IndexOf('hide-cached-feeds'))) and (Resource <> Resource2) and (f1.Size = f2.Size) then
  begin
    DeleteFile(Resource);
    RenameFile(Resource2, Resource);
    Resource := '';
  end
  else
  begin
    DeleteFile(Resource);
    RenameFile(Resource2, Resource);
  end;
  FindClose(f2);
  FindClose(f1);
{$ENDIF}
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
    ParseURL(Resource, Prot, User, Pass, Host, Port, Path, Para);
{$IFDEF USE_IDN}
    idna_to_ascii_8z(StrToPChar(Host), @PHost, 0);
    Host := StrPas(PHost);
{$ENDIF}
{$ENDIF}
  end;
end;

procedure ParseFeed(const Sock: TRSock);
var
  Finished: Boolean;
  UseFilters: Boolean;
begin
  Finished := false;
  UseFilters := Settings.GetBoolean(Settings.IndexOf('use-filters'));

  while not Finished do
  begin
    if Finished then begin WriteLn('Uh, yeah'); Halt; end;
{$IF not defined(SOCKETS_NONE) and not defined(SOCKETS_CURL)}
    if Assigned(Sock.Sock) and Sock.Error then
    begin
      CallMessageEvent(Sock, true, ErrorGetting);
{$IFNDEF __GPC__} { Hack: Sock.Error is always true in GPC }
      Break;
{$ENDIF}
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

finalization

Item.Free;

end.
