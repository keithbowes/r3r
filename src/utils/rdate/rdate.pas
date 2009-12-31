unit RDate;

interface

type
  TTime = record
    Hour, Millisecond, Minute, Second: String;
    Day, Month, Year: String;
    MonthAbbrev: String;
    ZoneOffset: String;
  end;

function LongDateToTime(Time: String): TTime;
function ShortDateToTime(Time: String): TTime;
function TimeToString(Time: TTime): String;

implementation

uses
  LibIntl, LibR3RStrings, StrTok;

function GetMonthAbbrev(const Month: String): String;
var
  ErrPos, iMonth: byte;
begin
  Val(Month, iMonth, ErrPos);

  case iMonth of
    1:
    begin
      GetMonthAbbrev := Jan;
    end;
    2:
    begin
      GetMonthAbbrev := Feb;
    end;
    3:
    begin
      GetMonthAbbrev := Mar;
    end;
    4:
    begin
      GetMonthAbbrev := Apr;
    end;
    5:
    begin
      GetMonthAbbrev := May;
    end;
    6:
    begin
      GetMonthAbbrev := Jun;
    end;
    7:
    begin
      GetMonthAbbrev := Jul;
    end;
    8:
    begin
      GetMonthAbbrev := Aug;
    end;
    9:
    begin
      GetMonthAbbrev := Sep;
    end;
    10:
    begin
      GetMonthAbbrev := Oct;
    end;
    11:
    begin
      GetMonthAbbrev := Nov;
    end;
    12:
    begin
      GetMonthAbbrev := Dec;
    end;
  end;
end;

function LongDateToTime(Time: String): TTime;
const
  WhitespaceChars = #0#8#9#10#13#32;
var
  List: TStringsList;
  Tm: TTime;
  TP: String;
begin
  textdomain('libr3r');
  List := Split(Time, WhitespaceChars);

  Tm.Day := List.Strings[1];
  Tm.MonthAbbrev := _(PChar(List.Strings[2]));
  Tm.Year := List.Strings[3];
  Tm.ZoneOffset := List.Strings[5];

  TP := List.Strings[4];
  Tm.Hour := Copy(TP, 1, 2);
  Delete(TP, 1, 3);

  Tm.Minute := Copy(TP, 1, 2);
  Delete(TP, 1, 3);

  Tm.Second := Copy(TP, 1, 2);
  LongDateToTime := Tm;
end;

function ShortDateToTime(Time: String): TTime;
var
  Sep: byte;
  Tm: TTime;
begin
  textdomain('libr3r');

  Sep := Pos('-', Time);
  Tm.Year := Copy(Time, 1, Sep - 1);
  Delete(Time, 1, Sep);

  Sep := Pos('-', Time);
  Tm.Month := Copy(Time, 1, Sep - 1);
  Delete(Time, 1, Sep);

  Sep := Pos('T', Time);
  Tm.Day := Copy(Time, 1, Sep - 1);
  Delete(Time, 1, Sep);

  if Length(Time) > 0 then
  begin
    Tm.Hour := Copy(Time, 1, 2);
    Delete(Time, 1, 3);

    Tm.Minute := Copy(Time, 1,2);
    Delete(Time, 1, 3);

    Tm.Second := Copy(Time, 1, 2);
    Delete(Time, 1, 2);

    if Pos('.', Time) = 1 then
    begin
      Tm.Millisecond := Copy(Time, 2, 3);
      Delete(Time, 1, 3);
    end;

    if (Pos('-', Time) = 1) or (Pos('+', Time) = 1) then
    begin
      Tm.ZoneOffset := Time;
    end;
  end;

  Tm.MonthAbbrev := GetMonthAbbrev(Tm.Month);
  ShortDateToTime := Tm;
end;

function TimeToString(Time: TTime): String;
var
  Ret: String;
begin
  with Time do
  begin
    Ret := Day + ' ' + MonthAbbrev + ' ' + Year +  ' ' + Hour + ':' + Minute + ':' + Second;

    if ZoneOffset <> '' then
    begin
      Ret := Ret + ' ' + ZoneOffset;
    end;
  end;

  TimeToString := Ret;
end;

end.
