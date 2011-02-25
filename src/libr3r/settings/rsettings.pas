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
    procedure ReadRec;
    procedure WriteRec;
  public
    constructor Create;
    destructor Destroy; {$IFNDEF __GPC__}override;{$ENDIF}
    function Enumerate(var Settings: PRList; var Count: TRSetIndex): Boolean;
    function IndexOf(const Name: String): TRSetIndex;
    function GetBoolean(const Index: TRSetIndex): Boolean;
    function GetInteger(const Index: TRSetIndex): integer;
    function GetString(const Index: TRSetIndex): String;
    procedure SetBoolean(const Index: TRSetIndex; const Setting: Boolean);
    procedure SetInteger(const Index: TRSetIndex; const Setting: integer);
    procedure SetString(const Index: TRSetIndex; const Setting: String);
    procedure RegisterBoolean(const Setting, ASection: String; const Value: Boolean; const ADescription: String);
    procedure RegisterInteger(const Setting, ASection: String; const Value: integer; const ADescription: String);
    procedure RegisterString(const Setting, ASection, Value, ADescription: String);
    procedure Access(var Index: TRSetIndex; var SettingName: String; var SettingValue: Pointer; var SettingType: byte; var Count: TRSetIndex; const SettingsMode: byte);
  end;

var
  Settings: TRSettings;

implementation

uses
  RProp, RSettings_Routines, RSettings_Strings, RStrings

{$IFDEF __GPC__}
  ,SysUtils
{$ELSE}
{$IFDEF SETTINGS_BIN}
	, SysUtils
{$ENDIF SETTINGS_BIN}
{$ENDIF __GPC__}

{$IFDEF SETTINGS_INI}
  , IniFiles
{$ENDIF}
  
{$IFDEF SETTINGS_REG}
  , Registry
{$ENDIF}
  
{$IFDEF SETTINGS_TAB}
  , TabFiles
{$ENDIF};

{$IFDEF SETTINGS_BIN}
  {$INCLUDE "binsettings.inc"}
{$ENDIF}

{$IFDEF SETTINGS_INI}
  {$INCLUDE "inisettings.inc"}
{$ENDIF}

{$IFDEF SETTINGS_REG}
  {$INCLUDE "regsettings.inc"}
{$ENDIF}

{$IFDEF SETTINGS_TAB}
  {$INCLUDE "tabsettings.inc"}
{$ENDIF}

function TRSettings.Enumerate(var Settings: PRList; var Count: TRSetIndex): Boolean;
var
  OrigCount: TRSetIndex;
begin
  OrigCount := Count;
  Settings := FSettings;
  Count := FSettings^.Count;
  Enumerate := Count = OrigCount;
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
procedure TRSettings.Access(var Index: TRSetIndex; var SettingName: String; var SettingValue: Pointer; var SettingType: byte; var Count: TRSetIndex; const SettingsMode: byte);
var
  BoolVal: Boolean;
  IntVal: integer;
  StrVal: String;
begin
  Count := FSettings^.Count;

  if Index = 0 then
  begin
    Index := IndexOf(SettingName);
  end;

  if Index <> -1 then
  begin
    SettingName := PRSetting(FSettings^.GetNth(Index))^.Name;
    SettingType := PRSetting(FSettings^.GetNth(Index))^.ValueType;

    if SettingsMode and SettingsWrite <> 0 then
    begin
      case SettingType of
        TypeBoolean:
        begin
          SetBoolean(Index, Boolean(PtrUInt(SettingValue)));
        end;
        TypeInteger:
        begin
          SetInteger(Index, PtrUInt(SettingValue));
        end;
        TypeString:
        begin
          SetString(Index, StrPas(SettingValue));
        end;
      end;
    end;

    if SettingsMode and SettingsRead <> 0 then
    begin
      case SettingType of
        TypeBoolean:
        begin
          BoolVal := GetBoolean(Index);
          SettingValue := Pointer(PtrInt(BoolVal));
        end;
        TypeInteger:
        begin
          IntVal := GetInteger(Index);
          SettingValue := Pointer(PtrInt(IntVal));
        end;
        TypeString:
        begin
          StrVal := GetString(Index);
          SettingValue := StrToPChar(StrVal);
        end;
      end;
    end;
  end;
end;

procedure TRSettings.InitRec;
begin
  CheckBoolean('show-messages', 'Display', true, DescMsg);
  CheckBoolean('display-feed-title-only', 'Display', false, DescDisplay);
  CheckBoolean('hide-cached-feeds', 'Display', true, DescHide);
  CheckBoolean('hide-cached-feed-items', 'Display', true, DescHideItems);
  CheckBoolean('enable-mime-guess', 'Display', false, DescGuess);
  CheckBoolean('check-for-updates', 'Display', false, DescCheck);
  CheckBoolean('load-subscriptions-on-startup', 'Display', false, LoadSubscriptions);
  CheckBoolean('warn-missing-data', 'Display', false, DescWarn);

  CheckBoolean('use-proxy', 'HTTP', false, DescProxy);
  CheckString('proxy-address', 'HTTP', '127.0.0.1', DescProxyAddress);
  CheckInteger('proxy-port', 'HTTP', 8118, DescProxyPort);

  CheckBoolean('use-custom-accept-types', 'HTTP Headers', false, DescUseTypes);
  CheckString('accept-types', 'HTTP Headers', '', DescTypes);
  CheckBoolean('use-custom-accept-langs', 'HTTP Headers', false, DescUseLang);
  CheckString('accept-langs', 'HTTP Headers', '', DescLang);

  CheckString('for:http', 'Programs', '', DescBrowser);
  CheckString('for:mailto', 'Programs', '', DescMail);
  CheckString('for:.ogg', 'Programs', '', DescMedia);

  CheckString('installed-prefix', 'System', GetInstalledPrefix, DescPrefix);
  SetProp('installed-prefix', StrToPChar(GetString(IndexOf('installed-prefix'))));
end;

initialization

Settings := TRSettings.Create;

finalization

Settings.Free;

end.
