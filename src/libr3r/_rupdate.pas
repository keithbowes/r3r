unit RUpdate;

interface

uses
  Http;

type
  TRUpdate = class(THttpSock)
  protected
    function Get: String;
  public
    constructor Create;
    function Available: Boolean;
  end;

implementation

uses
  SockConsts;
  
const
  Timestamp = @TIMESTAMP@;

constructor TRUpdate.Create;
begin
  inherited Create('r3r.sourceforge.net', '80', '/latest.txt', '');
  Execute;
end;

function TRUpdate.Available: Boolean;
var
  Contents: String;
  ErrCode: byte;
  TS: PtrUInt;
begin
  Contents := Get;
  Val(Contents, TS, ErrCode);

  if ErrCode = 0 then
  begin
    Result := TS > Timestamp;
  end
  else
  begin
    Result := false;
  end;
end;

function TRUpdate.Get: String;
var
  Line: String;
begin
  GetHeaders;

  while Line <> SockEof do
  begin
    Line := GetLine;
    Result := Result + Line;
  end;
end;

end.
