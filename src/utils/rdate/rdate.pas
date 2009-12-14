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
  LibR3RStrings, LibIntl, StrTok;

function GetMonthAbbrev(const Month: String): String;
var
  ErrPos, iMonth: byte;
begin
  Val(Month, iMonth, ErrPos);

  case iMonth of
    1:
    begin
      Result := Jan;
    end;
    2:
    begin
      Result := Feb;
    end;
    3:
    begin
      Result := Mar;
    end;
    4:
    begin
      Result := Apr;
    end;
    5:
    begin
      Result := May;
    end;
    6:
    begin
      Result := Jun;
    end;
    7:
    begin
      Result := Jul;
    end;
    8:
    begin
      Result := Aug;
    end;
    9:
    begin
      Result := Sep;
    end;
    10:
    begin
      Result := Oct;
    end;
    11:
    begin
      Result := Nov;
    end;
    12:
    begin
      Result := Dec;
    end;
  end;
end;

function LongDateToTime(Time: String): TTime;
const
  WhitespaceChars = #0#8#9#10#13#32;
var
  List: TStringsList;
  TP: String;
begin
  List := Split(Time, WhitespaceChars);

  Result.Day := List.Strings[1];
  Result.MonthAbbrev := List.Strings[2];
  Result.Year := List.Strings[3];
  Result.ZoneOffset := List.Strings[5];

  TP := List.Strings[4];
  Result.Hour := Copy(TP, 1, 2);
  Delete(TP, 1, 3);

  Result.Minute := Copy(TP, 1, 2);
  Delete(TP, 1, 3);

  Result.Second := Copy(TP, 1, 2);
end;

function ShortDateToTime(Time: String): TTime;
var
  Sep: byte;
begin
  Sep := Pos('-', Time);
  Result.Year := Copy(Time, 1, Sep - 1);
  Delete(Time, 1, Sep);

  Sep := Pos('-', Time);
  Result.Month := Copy(Time, 1, Sep - 1);
  Delete(Time, 1, Sep);

  Sep := Pos('T', Time);
  Result.Day := Copy(Time, 1, Sep - 1);
  Delete(Time, 1, Sep);

  if Length(Time) > 0 then
  begin
    Result.Hour := Copy(Time, 1, 2);
    Delete(Time, 1, 3);

    Result.Minute := Copy(Time, 1,2);
    Delete(Time, 1, 3);

    Result.Second := Copy(Time, 1, 2);
    Delete(Time, 1, 2);

    if Pos('.', Time) = 1 then
    begin
      Result.Millisecond := Copy(Time, 2, 3);
      Delete(Time, 1, 3);
    end;

    if (Pos('-', Time) = 1) or (Pos('+', Time) = 1) then
    begin
      Result.ZoneOffset := Time;
    end;
  end;

  Result.MonthAbbrev := GetMonthAbbrev(Result.Month);
end;

function TimeToString(Time: TTime): String;
begin
  with Time do
  begin
    Result := Day + ' ' + MonthAbbrev + ' ' + Year +  ' ' + Hour + ':' + Minute + ':' + Second;

    if ZoneOffset <> '' then
    begin
      Result := Result + ' ' + ZoneOffset;
    end;
  end;
end;

end.
