
function CheckSetting(SettingName: String): Boolean;

function GetBoolean(const Index: TRSetIndex): Boolean;
function GetInteger(const Index: TRSetIndex): integer;
function GetString(const Index: TRSetIndex): String;

procedure SetBoolean(const Index: TRSetIndex; const Setting: Boolean);
procedure SetInteger(const Index: TRSetIndex; const Setting: integer);
procedure SetString(const Index: TRSetIndex; const Setting: String);
