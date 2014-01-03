unit RSettings;

{$H-}

interface

uses
  RList;

const
  SettingsRead = 1;
  SettingsWrite = 2;

  TypeNone = 0;
  TypeString = 1;
  TypeInteger = 2;
  TypeBoolean = 3;

type
  TRSetIndex = integer;

  PRSetting = ^TRSetting;
  TRSetting = packed record
    Name: String;
    Section: String;
    Description: String;

    case ValueType: byte of
      TypeString: (ValueString: String);
      TypeInteger: (ValueInteger: integer);
      TypeBoolean: (ValueBoolean: Boolean);
  end;

  TRSettings = class
  private
    FSettings: PRList;
    procedure InitRec;
    procedure CheckBoolean(const Setting, ASection: String; const Value: Boolean; const ADescription: String);
    procedure CheckInteger(const Setting, ASection: String; const Value: integer; const ADescription: String);
    procedure CheckString(const Setting, ASection, Value, ADescription: String);
    procedure WriteRec;
    function IndexOf(const Name: String): TRSetIndex;
  public
    constructor Create;
    destructor Destroy; {$IFNDEF __GPC__}override;{$ENDIF}
    procedure Enumerate(var Settings: PRList; var Count: TRSetIndex);
    function GetBoolean(const Setting: String): Boolean;
    function GetInteger(const Setting: String): integer;
    function GetString(const Setting: String): String;
    procedure SetBoolean(const Setting: String; const Value: Boolean);
    procedure SetInteger(const Setting: String; const Value: integer);
    procedure SetString(const Setting: String; const Value: String);
    procedure RegisterBoolean(const Setting, ASection: String; const Value: Boolean; const ADescription: String);
    procedure RegisterInteger(const Setting, ASection: String; const Value: integer; const ADescription: String);
    procedure RegisterString(const Setting, ASection, Value, ADescription: String);
    procedure Access(var SettingName: String; var SettingValue: Pointer; var SettingType: byte; var Count: TRSetIndex; const SettingsMode: byte);
  end;

var
  Settings: TRSettings;

implementation

uses
  Dos, MailCap, RParseURL, RProp, RSettings_Routines,
  RSettings_Strings, RStrings
{$IFDEF SETTINGS_INI}
  , IniFiles
{$ENDIF}

{$IFDEF SETTINGS_LIBINI}
  , LibIni
{$ENDIF}
  
{$IFDEF SETTINGS_REG}
  , Info, Registry
{$ENDIF}
  ;

{$IF defined(SETTINGS_INI) or defined(SETTINGS_LIBINI)}
const
  CommentSeparator = ';; ';
{$ENDIF}

{$IFDEF SETTINGS_INI}
  {$INCLUDE "inisettings.inc"}
{$ENDIF}

{$IFDEF SETTINGS_LIBINI}
  {$INCLUDE "libinisettings.inc"}
{$ENDIF}

{$IFDEF SETTINGS_REG}
  {$INCLUDE "regsettings.inc"}
{$ENDIF}

procedure TRSettings.Enumerate(var Settings: PRList; var Count: TRSetIndex);
begin
  Settings := FSettings;
  Count := FSettings^.Count;
end;

function TRSettings.IndexOf(const Name: String): TRSetIndex;
var
  Found: Boolean;
  i: byte;
begin
  i := 0;

  repeat
    Found := (FSettings^.Count > 0) and (PRSetting(FSettings^.GetNth(i))^.Name = Name);
    Inc(i);
  until Found or (i > FSettings^.Count);

  if Found then
  begin
    IndexOf := i - 1;
  end
  else
  begin
    IndexOf := -1;
  end;
end;

procedure TRSettings.RegisterBoolean(const Setting, ASection: String; const Value: Boolean; const ADescription: String);
begin
  if IndexOf(Setting) = -1 then
  begin
    CheckBoolean(Setting, ASection, Value, ADescription);
  end;
end;

procedure TRSettings.RegisterInteger(const Setting, ASection: String; const Value: integer; const ADescription: String);
begin
  if IndexOf(Setting) = -1 then
  begin
    CheckInteger(Setting, ASection, Value, ADescription);
  end;
end;

procedure TRSettings.RegisterString(const Setting, ASection, Value, ADescription: String);
begin
  if IndexOf(Setting) = -1 then
  begin
    CheckString(Setting, ASection, Value, ADescription);
  end;
end;

{
  Quick and dirty access method.
  Mostly useful for the C API.
}
procedure TRSettings.Access(var SettingName: String; var SettingValue: Pointer; var SettingType: byte; var Count: TRSetIndex; const SettingsMode: byte);
var
  BoolVal: Boolean;
  Index: TRSetIndex;
  IntVal: integer;
  StrVal: String;
begin
  Count := FSettings^.Count;
  Index := IndexOf(SettingName);

  if Index <> -1 then
  begin
    SettingName := PRSetting(FSettings^.GetNth(Index))^.Name;
    SettingType := PRSetting(FSettings^.GetNth(Index))^.ValueType;

    if SettingsMode and SettingsWrite <> 0 then
    begin
      case SettingType of
        TypeBoolean:
        begin
          SetBoolean(SettingName, Boolean(PtrUInt(SettingValue)));
        end;
        TypeInteger:
        begin
          SetInteger(SettingName, PtrUInt(SettingValue));
        end;
        TypeString:
        begin
          WriteStr(StrVal, PChar(SettingValue));
          SetString(SettingName, StrVal);
        end;
      end;
    end;

    if SettingsMode and SettingsRead <> 0 then
    begin
      case SettingType of
        TypeBoolean:
        begin
          BoolVal := GetBoolean(SettingName);
          SettingValue := Pointer(PtrUInt(BoolVal));
        end;
        TypeInteger:
        begin
          IntVal := GetInteger(SettingName);
          SettingValue := Pointer(PtrInt(IntVal));
        end;
        TypeString:
        begin
          StrVal := GetString(SettingName);
          SettingValue := StrToPChar(StrVal);
        end;
      end;
    end;
  end;
end;

procedure TRSettings.InitRec;
var
  mc: PMailCap;
  Proxy, ProxyHost: String;
  ProxyPort: word;
  ProxyURL: TURL;
begin
  New(mc, Init);

  Proxy := GetEnv('http_proxy');
  if Proxy = '' then
  begin
    Proxy := GetEnv('HTTP_PROXY');
  end;

  if Proxy <> '' then
  begin
    ProxyURL := ParseURL(Proxy);
    WriteStr(ProxyHost, ProxyURL.Protocol, '://', ProxyURL.Host);
    {$I-}
    ReadStr(ProxyURL.Port, ProxyPort);
    {$I+}
  end
  else
  begin
    ProxyHost := 'http://127.0.0.1';
    ProxyPort := 8118;
  end;

  CheckBoolean('show-messages', 'Display', true, DescMsg);
  CheckBoolean('display-feed-title-only', 'Display', false, DescDisplay);
  CheckBoolean('hide-cached-feeds', 'Display', true, DescHide);
  CheckBoolean('hide-cached-feed-items', 'Display', true, DescHideItems);
  CheckInteger('cache-expiry', 'Display', 90, DescCacheExpiry);
  CheckBoolean('enable-mime-guess', 'Display', false, DescGuess);
  CheckBoolean('check-for-updates', 'Display', false, DescCheck);
  CheckBoolean('load-subscriptions-on-startup', 'Display', true, LoadSubscriptions);
  CheckBoolean('use-filters', 'Display', true, DescFilters);

  CheckBoolean('use-proxy', 'HTTP', Proxy <> '', DescProxy);
  CheckString('proxy-address', 'HTTP', ProxyHost, DescProxyAddress);
  CheckInteger('proxy-port', 'HTTP', ProxyPort, DescProxyPort);

  CheckBoolean('use-custom-accept-types', 'HTTP Headers', false, DescUseTypes);
  CheckString('accept-types', 'HTTP Headers', '', DescTypes);
  CheckBoolean('use-custom-accept-langs', 'HTTP Headers', false, DescUseLang);
  CheckString('accept-langs', 'HTTP Headers', '', DescLang);
  CheckString('user-agent', 'HTTP Headers', '%a (%o; %m; %c; %s; %u) %n %i %e %p %w', DescUserAgent);

  CheckString('for:http', 'Programs', Cap2Doze(mc^.GetProg('text/html')), DescBrowser);
  CheckString('for:mailto', 'Programs', '', DescMail);

  CheckString('installed-prefix', 'System', GetInstalledPrefix, DescPrefix);
  SetProp('installed-prefix', StrToPCharAlloc(GetString('installed-prefix')));
  CheckString('update-channel', 'System', 'release', DescChannel);

  Dispose(mc, Done);
end;

initialization

Settings := TRSettings.Create;

finalization

Settings.Free;

end.
