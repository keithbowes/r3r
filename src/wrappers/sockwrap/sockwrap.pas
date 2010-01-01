unit SockWrap;

{$H+}

interface
{$IFDEF UNIX}
{$LINKLIB c}
{$ENDIF}

{$L sockwrapc.o}

uses
  StrTok;

{$CALLING cdecl}

function socket_init(hostname, port: PChar): integer; external;
procedure socket_done(sock: integer); external;
procedure socket_connect(sock: integer); external;
procedure socket_send(sock: integer; data: PChar); external;
function socket_receive(sock: integer; buf: PChar; len: integer): integer; external;
function socket_get_error(sock: integer): integer; external;

{$CALLING register}

type
  TSockWrap = class
  private
    FReceived: integer;
    FSocket: integer;
    FStringIndex: byte;
    FStrings: TStringsList;
  public
    { Does nothing.  Just for Synapse compatibility. }
    ConvertLineEnd: Boolean;
    constructor Create;
    destructor Destroy; {$IFNDEF __GPC__}override;{$ENDIF}
    procedure Connect(const Host, Port: String);
    function LastError: integer;
    function RecvString(Timeout: word): String;
    procedure SendString(Data: String);
    procedure CloseSocket;
  end;

function ParseURL(URL: String; var Protocol, USer, Password, Host, Port, Path, Search: String): String;

implementation

uses
  RStrings, SysUtils;

constructor TSockWrap.Create;
begin
  InitStringsList(FStrings);
end;

destructor TSockWrap.Destroy;
begin
  CloseSocket;
end;

procedure TSockWrap.Connect(const Host, Port: String);
begin
  FSocket := socket_init(StrToPChar(Host), StrToPChar(Port));
  socket_connect(FSocket);
end;

function TSockWrap.LastError: integer;
begin
  LastError := socket_get_error(FSocket);
end;

{ Timeout is ignored; it's just for compatability with the Synapse API }
function TSockWrap.RecvString(Timeout: word): String;
var
  Buf: array [1..255] of char;
  CR, i, LF: word;
  CurStr, LastStr, Sub, Tmp: String;
  LastReceived: integer;
begin
  LastStr := '';
  LastReceived := FReceived;

  if (FStrings.Length = 0) or (FStrings.Length = FStringIndex + 1) or
    (FStringIndex >= 255) then
  begin
    InitStringsList(FStrings);
    FReceived := socket_receive(FSocket, PChar(@Buf), 255);
    LastStr := FStrings.Strings[FStringIndex];
    FStrings := Split(Copy(Buf, 1, FReceived + 2), #10#13);
    FStringIndex := 0;
  end;
  
  if FReceived > 0 then
  begin
    CurStr := FStrings.Strings[FStringIndex];
    Dec(FReceived, Length(CurStr));

    { Compensate for lost line breaks from Split }
    if (FReceived = LastReceived - 1) then
    begin
      RecvString := #13#10;
    end
    else
    begin
      RecvString := LastStr + CurStr;
    end;
  end
  else
  begin
    RecvString := #0;
  end;

  Inc(FStringIndex);
end;

procedure TSockWrap.SendString(Data: String);
begin
  socket_send(FSocket, StrToPChar(Data));
end;

procedure TSockWrap.CloseSocket;
begin
  socket_done(FSocket);
end;

function ParseURL(URL: String; var Protocol, User, Password, Host, Port, Path, Search: String): String;
const
  DefaultHost = 'localhost';
var
  ErrPos, Index, Index2: byte;
  Res: String;
begin
  Protocol := 'http';
  Port := '80';

  Index := Pos('?', URL);
  if Index <> 0 then
  begin
    Search := Copy(URL, Index + 1, Length(URL) - Index);
    Delete(URL, Index, Length(URL) - Index + 1);
  end;

  Index := Pos('://', URL);
  if Index <> 0 then
  begin
    Protocol := Copy(URL, 1, Index - 1);
    Delete(URL, 1, Index + 2);
  end;

  Index := Pos('@', URL);
  if Index <> 0 then
  begin
    Index2 := Pos(':', URL);
    if (Index2 <> 0) and (Index2 < Index) then
    begin
      User := Copy(URL, 1, Index2 - 1);
      PassWord := Copy(URL, Index2 + 1, Index - Index2 - 1);
    end
    else
    begin
      User := Copy(URL, 1, Index - 1);
    end;

    Delete(URL, 1, Index);
  end;

  Index := Pos('/', URL);
  if Index <> 0 then
  begin
    Host := Copy(URL, 1, Index - 1);
    Path := Copy(URL, Index, Length(URL) - Index + 1);

    if (Pos('.', Host) = 0) and (Host <> DefaultHost) then
    begin
      Path := '/' + Host + Path;
      Host := DefaultHost;
    end;

    URL := '';
  end;

  Index := Pos(':', Host);
  if Index <> 0 then
  begin
    Port := Copy(Host, Index + 1, Length(Host) - Index);
    Delete(Host, Index, Length(Host) - Index + 1);

    Val(Port, Index2, ErrPos);
    if ErrPos <> 0 then
    begin
      Protocol := Port;
    end;
  end;
  
  Res := Path;
  if Search <> '' then
  begin
    Res := Res + '?' + Search;
  end;

  ParseURL := Res;
end;

end.
