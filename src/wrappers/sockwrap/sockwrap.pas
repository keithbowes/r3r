unit SockWrap;

interface

{$CALLING cdecl}
{$IFDEF UNIX}
{$LINKLIB c}
{$ENDIF}

uses
  StrTok;

function socket_init(hostname, port: PChar): integer; external;
procedure socket_done(sock: integer); external;
procedure socket_connect(sock: integer); external;
procedure socket_send(sock: integer; data: PChar); external;
function socket_receive(sock: integer; buf: PChar; len: integer): integer; external;
function socket_get_error(sock: integer): integer; external;

{$L sockwrapc.o}

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
  Buf: PChar{array [1..255] of char};
  BufStr: String;
  CurStr, LastStr: String;
  LastReceived: integer;
begin
  LastStr := '';
  LastReceived := FReceived;

  if (FStrings.Length = 0) or (FStrings.Length = FStringIndex + 1) or
    (FStringIndex >= 255) then
  begin
    GetMem(Buf, 255 * SizeOf(Buf));
    FReceived := socket_receive(FSocket, Buf, 255);
    WriteStr(BufStr, Buf);
    LastStr := FStrings.Strings[FStringIndex];
    FStrings := Split(Copy(BufStr, 1, FReceived + 2), #10#13);
    FStringIndex := 0;
    FreeMem(buf);
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

end.
