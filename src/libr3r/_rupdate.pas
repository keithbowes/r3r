unit RUpdate;

interface

uses
  Http;

type
  TRUpdate = class(THttpSock)
  public
    constructor Create; {$IFDEF __GPC__}override;{$ENDIF}
    function Available: Boolean;
  end;

implementation

uses
  SockConsts;
constructor TRUpdate.Create;
begin
  inherited Create('r3r.sourceforge.net', '80', '/check.php', 'v=@VERSION@');
  Execute;
end;

function TRUpdate.Available: Boolean;
begin
  Available := Headers.Status = 304;
end;

end.
