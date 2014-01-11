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
  Info, RSettings;

constructor TRUpdate.Create;
begin
  inherited Create('http', 'r3r.sourceforge.net', '80', '/check.php', 'v=' + AppVersion + '&channel=' + Settings.GetString('update-channel'));
  Method := 'HEAD';

  Execute;
  GetHeaders;
end;

function TRUpdate.Available: Boolean;
begin
  Available := Headers.Status <> 304;
end;

end.
