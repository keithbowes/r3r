unit RUpdate;

interface

uses
  Http;

type
  TRUpdate = class(THttpSock)
  protected
    function Get: String;
  public
    constructor Create; {$IFDEF __GPC__}override;{$ENDIF}
    function Available: Boolean;
  end;

implementation

uses
  SockConsts;
  
const
  Timestamp = @TIMESTAMP@;

{$IFNDEF __GPC__}
type
{$IFDEF FPC}
  PtrWord = PtrUInt;
{$ELSE}
  PtrWord = cardinal;
{$ENDIF}
{$ENDIF}

constructor TRUpdate.Create;
begin
  inherited Create('r3r.sourceforge.net', '80', '/latest.txt', '');
  Execute;
end;

function TRUpdate.Available: Boolean;
var
  Contents: String;
  ErrCode: byte;
  TS: PtrWord;
begin
  Contents := Get;
  Val(Contents, TS, ErrCode);

  if ErrCode = 0 then
  begin
    Available := TS > Timestamp;
  end
  else
  begin
    Available := false;
  end;
end;

function TRUpdate.Get: String;
var
  Line: String;
  Res: String;
begin
  GetHeaders;

  while Line <> SockEof do
  begin
    Line := GetLine;
    Res := Res + Line;
  end;

  Get := Res;
end;

end.
