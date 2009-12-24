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

    case ValueType: byte of
      TypeString: (ValueString: String);
      TypeInteger: (ValueInteger: integer);
      TypeBoolean: (ValueBoolean: Boolean);
  end;

  TRSettings = class
  private
    FSettings: PRList;
    FSettingsFile: String;
    procedure InitRec;
    procedure CheckBoolean(const Setting, ASection: String; const Value: Boolean);
    procedure CheckInteger(const Setting, ASection: String; const Value: integer);
    procedure CheckString(const Setting, ASection, Value: String);
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
    procedure Access(var Index: TRSetIndex; var SettingName: String; var SettingValue: Pointer; var SettingType: byte; var Count: TRSetIndex; const SettingsMode: byte);
  end;

var
  Settings: TRSettings;

implementation

uses
  RSettings_Routines, RStrings, SysUtils
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

function TRSettings.GetBoolean(const Index: TRSetIndex): Boolean;
begin
  if Index <> -1 then
  begin
    GetBoolean := PRSetting(FSettings^.GetNth(Index))^.ValueBoolean;
  end;
end;

function TRSettings.GetInteger(const Index: TRSetIndex): integer;
begin
  if Index <> -1 then
  begin
    GetInteger := PRSetting(FSettings^.GetNth(Index))^.ValueInteger;
  end;
end;

function TRSettings.GetString(const Index: TRSetIndex): String;
var
  Setting: TRSetIndex;
begin
  if Index <> -1 then
  begin
    GetString := PRSetting(FSettings^.GetNth(Index))^.ValueString;
  end;
end;

procedure TRSettings.SetBoolean(const Index: TRSetIndex; const Setting: Boolean);
begin
  if Index <> -1 then
  begin
    PRSetting(FSettings^.GetNth(Index))^.ValueBoolean := Setting;
  end;
end;

procedure TRSettings.SetInteger(const Index: TRSetIndex; const Setting: integer);
begin
  if Index <> -1 then
  begin
    PRSetting(FSettings^.GetNth(Index))^.ValueInteger := Setting;
  end;
end;

procedure TRSettings.SetString(const Index: TRSetIndex; const Setting: String);
begin
  if Index <> -1 then
  begin
    PRSetting(FSettings^.GetNth(Index))^.ValueString := Setting;
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
  CheckBoolean('show-messages', 'Display', true);
  CheckBoolean('display-feed-title-only', 'Display', false);
  CheckBoolean('hide-cached-feeds', 'Display', true);
  CheckBoolean('hide-cached-feed-items', 'Display', true);
  CheckBoolean('enable-mime-guess', 'Display', false);
  CheckBoolean('check-for-updates', 'Display', true);
  CheckBoolean('load-subscriptions-on-startup', 'Display', false);

  CheckBoolean('use-proxy', 'HTTP', false);
  CheckString('proxy-address', 'HTTP', '127.0.0.1');
  CheckInteger('proxy-port', 'HTTP', 8118);

  CheckBoolean('use-custom-accept-types', 'HTTP Headers', false);
  CheckString('accept-types', 'HTTP Headers', '');
  CheckBoolean('use-custom-accept-langs', 'HTTP Headers', false);
  CheckString('accept-langs', 'HTTP Headers', '');

  CheckString('for:http', 'Programs', 'lynx');
  CheckString('for:mailto', 'Programs', 'sendmail');

  CheckString('installed-prefix', 'System', ExtractFileDir(ParamStr(0)) + PathDelim + '..' + PathDelim);
end;

initialization

Settings := TRSettings.Create;

finalization

Settings.Free;

end.
