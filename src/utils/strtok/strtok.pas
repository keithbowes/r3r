unit StrTok;

{$R+}
{$W-}

interface

type
  TStrings = array [0..255] of String;
  TStringsList = record
    Strings: TStrings;
    Length: integer;
  end;

procedure InitStringsList(var List: TStringsList);
procedure Push(var List: TStringsList; str: String);

function Explode(Delim, str: String; Limit: integer): TStringsList;
function Implode(Delim: String; List: TStringsList): String;
function Join(List: TStringsList; Delim: String): String;
function Split(str, Delim: String): TStringsList;
function StrTokC(str, Delim: String): String;
function StrTok_R(str, Delim: String; var State: String): String;
function StrTokPas(var str: String; Delim: String): String;

implementation

var
  StrTokString: String;

procedure InitStringsList(var List: TStringsList);
begin
  List.Length := 0;
end;

procedure Push(var List: TStringsList; str: String);
begin
  List.Strings[List.Length] := str;
  List.Length := List.Length + 1;
end;

function Explode(Delim, str: String; Limit: integer): TStringsList;
var
  c, s: String;
  Len: integer;
  List: TStringsList;
begin
  Len := 0;
  s := str;

  if Limit < 1 then
  begin
    Limit := 256;
  end;

  while (s <> '') and (Len < 256) and (Len < Limit) do
  begin
    c := StrTokPas(s, Delim);
    List.Strings[Len] := c;
    Len := Len + 1;
  end;

  List.Length := Len;
  Explode := List;
end;

function Implode(Delim: String; List: TStringsList): String;
var
  i: integer;
  Ret: String;
begin
  Ret := '';

  for i := 0 to List.Length - 2 do
  begin
    Ret := Ret + List.Strings[i] + Delim;
  end;

  Ret := Ret + List.Strings[i + 1];
  Implode := Ret;
end;

function Join(List: TStringsList; Delim: String): String;
begin
  Join := Implode(Delim, List);
end;

function Split(str, Delim: String): TStringsList;
begin
  Split := Explode(Delim, str, -1);
end;

function StrTokC(str, Delim: String): String;
begin
  StrTokC := StrTok_R(str, Delim, StrTokString);
end;

function StrTok_R(str, Delim: String; var State: String): String;
begin
  if str <> '' then
  begin
    State := str;
  end;

  StrTok_R := StrTokPas(State, Delim);
end;

function StrTokPas(var str: String; Delim: String): String;
var
  i, j, Len, LenDelim: integer;
  IsFound: Boolean;
  Ret: String;
begin
  IsFound := false;
  Len := Length(str);
  LenDelim := Length(Delim);

  for i := 1 to LenDelim do
  begin
    for j := 1 to Len do
    begin
      if not IsFound then
      begin
        if str[j] = Delim[i] then
        begin
          Ret := Copy(str, 1, j - 1);
          Delete(str, 1, j);

          if Ret = '' then
          begin
            Ret := StrTokPas(str, Delim);
          end;

          IsFound := true;
        end;
      end;
    end;
  end;
  
  if not IsFound then
  begin
    Ret := Str;
    str := '';
  end;

  StrTokPas := Ret;
end;

end.
