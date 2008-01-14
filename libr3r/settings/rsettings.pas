unit RSettings;

interface

uses
{$IFDEF SETTINGS_REGISTRY}
  Registry
{$ELSE}
  {$IFDEF SETTINGS_INI}
    Inifiles
  {$ENDIF}
{$ENDIF}
;

type
  TRSettings = class
  private
    {$I settingsh.inc}
    function GetProxy: String;
    procedure SetProxy(Proxy: String);
  public
    constructor Create;
    destructor Destroy; override;
    property Proxy: String read GetProxy write SetProxy;
  end;

var
  SettingIds: array [1..2] of String = ('proxy-address', 'proxy-port');

implementation

uses
  Dos, RSettings_Routines, SysUtils;

type
  TSettingEnum = (seProxyAddress, seProxyPort);

var
  SettingEnum: TSettingEnum;

function TRSettings.GetProxy: String;
begin
  Result := GetString('Proxy', '0.0.0.0');
end;

procedure TRSettings.SetProxy(Proxy: String);
begin
  SetSection('Networking');
  SetString('Proxy', Proxy);
end;

{$IFDEF SETTINGS_REGISTRY}
{$I regsettings.inc}
{$ELSE}
  {$IFDEF SETTINGS_INI}
    {$I inisettings.inc}
  {$ENDIF}
{$ENDIF}

initialization

end.
