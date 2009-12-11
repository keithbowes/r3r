unit RSettings;

{$H-}

interface

const
  SettingsCount = 17;

  SettingsRead = 1;
  SettingsWrite = 2;

  TypeNone = 0;
  TypeString = 1;
  TypeInteger = 2;
  TypeBoolean = 3;

type
  TRSetIndex = -1..SettingsCount;

  TRSetting = packed record
    Name: String;
    Section: String;

    case ValueType: byte of
      TypeString: (ValueString: String);
      TypeInteger: (ValueInteger: integer);
      TypeBoolean: (ValueBoolean: Boolean);
  end;

  TRSettingsRec = array[1..SettingsCount] of TRSetting;

  TRSettings = class
  private
    FRef: byte;
    FSettings: TRSettingsRec;
    FSettingsFile: String;
    function CleanRec: Boolean;
    procedure InitRec;
    function CheckBoolean(const Setting, ASection: String; const Value: Boolean): Boolean;
    function CheckInteger(const Setting, ASection: String; const Value: integer): Boolean;
    function CheckString(const Setting, ASection, Value: String): Boolean;
    procedure ReadRec;
    procedure WriteRec;
  public
    constructor Create;
    destructor Destroy; override;
    function Enumerate(var Settings: TRSettingsRec; var Count): Boolean;
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
{$ENDIF};

{$IFDEF SETTINGS_BIN}
  {$INCLUDE binsettings.inc}
{$ENDIF}

{$IFDEF SETTINGS_INI}
  {$INCLUDE inisettings.inc}
{$ENDIF}

{$IFDEF SETTINGS_REG}
  {$INCLUDE regsettings.inc}
{$ENDIF}

function TRSettings.Enumerate(var Settings: TRSettingsRec; var Count): Boolean;
var
  OrigCount: TRSetIndex;
begin
  OrigCount := TRSetIndex(Count);
  Settings := FSettings;
  TRSetIndex(Count) := SettingsCount;
  Result := TRSetIndex(Count) = OrigCount;
end;

function TRSettings.IndexOf(const Name: String): TRSetIndex;
var
  Found: Boolean;
  i: byte;
begin
  i := 1;

  repeat
    Result := i;
    Found := FSettings[i].Name = Name;
    Inc(i);
  until Found or (i > SettingsCount);

  if not Found then
  begin
    Result := -1;
  end;
end;

function TRSettings.GetBoolean(const Index: TRSetIndex): Boolean;
begin
  if Index <> -1 then
  begin
    Result := FSettings[Index].ValueBoolean;
  end;
end;

function TRSettings.GetInteger(const Index: TRSetIndex): integer;
begin
  if Index <> -1 then
  begin
    Result := FSettings[Index].ValueInteger;
  end;
end;

function TRSettings.GetString(const Index: TRSetIndex): String;
begin
  if Index <> -1 then
  begin
    Result := FSettings[Index].ValueString;
  end;
end;

procedure TRSettings.SetBoolean(const Index: TRSetIndex; const Setting: Boolean);
begin
  if Index <> -1 then
  begin
    FSettings[Index].ValueBoolean := Setting;
  end;
end;

procedure TRSettings.SetInteger(const Index: TRSetIndex; const Setting: integer);
begin
  if Index <> -1 then
  begin
    FSettings[Index].ValueInteger := Setting;
  end;
end;

procedure TRSettings.SetString(const Index: TRSetIndex; const Setting: String);
begin
  if Index <> -1 then
  begin
    FSettings[Index].ValueString := Setting;
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
  Count := SettingsCount;

  if Index = 0 then
  begin
    Index := IndexOf(SettingName);
  end;

  if Index <> -1 then
  begin
    SettingName := FSettings[Index].Name;
    SettingType := FSettings[Index].ValueType;

    if SettingsMode and SettingsWrite <> 0 then
    begin
      case SettingType of
        TypeBoolean:
        begin
          SetBoolean(Index, Boolean(PtrUInt(SettingValue)));
        end;
        TypeInteger:
        begin
          SetInteger(Index, PtrInt(SettingValue));
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
  CleanRec;

  if FRef < SettingsCount then
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

    CheckString('browser', 'Programs', 'lynx');
    CheckString('mail-client', 'Programs', 'sendmail');
    CheckString('editor', 'Programs', 'vim');
  end;
end;

initialization

Settings := TRSettings.Create;

finalization

Settings.Free;

end.
