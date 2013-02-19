unit RDate;

interface

type
  TRDate = record
    Hour, Millisecond, Minute, Second: String;
    Day, Month, Year: String;
    MonthAbbrev: String;
    ZoneOffset: String;
  end;

function LongDateToTime(Time: String): TRDate;
function ShortDateToTime(Time: String): TRDate;
function TimeToString(Time: TRDate): String;
function UnixToDate(const TS: real): String;

{$IFNDEF USE_NLS}
function _(s: PChar): String;
{$ENDIF}

implementation

uses
{$IFDEF USE_NLS}
  LibIntl,
{$ENDIF}
  LibR3RStrings, StrTok, SysUtils;

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

function LongDateToTime(Time: String): TRDate;
const
  WhitespaceChars = #0#8#9#10#13#32;
var
  List: TStringsList;
  Tm: TRDate;
  TP: String;
begin
{$IFDEF USE_NLS}
  textdomain('libr3r');
{$ENDIF}
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

function ShortDateToTime(Time: String): TRDate;
var
  Sep: byte;
  Tm: TRDate;
begin
{$IFDEF USE_NLS}
  textdomain('libr3r');
{$ENDIF}

  Sep := Pos('-', Time);
  Tm.Year := Copy(Time, 1, Sep - 1);
  Delete(Time, 1, Sep);

  Sep := Pos('-', Time);
  Tm.Month := Copy(Time, 1, Sep - 1);
  Delete(Time, 1, Sep);

  Sep := Pos('T', Time);

  if Sep <> 0 then
  begin
    Tm.Day := Copy(Time, 1, Sep - 1);
    Delete(Time, 1, Sep);
  end
  else
  begin
    Tm.Day := Copy(Time, 1, Length(Time) - 1);
    Time := '';
  end;

  if Length(Time) > 0 then
  begin
    Tm.Hour := Copy(Time, 1, 2);
    Delete(Time, 1, 3);

    Tm.Minute := Copy(Time, 1, 2);
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

function TimeToString(Time: TRDate): String;
var
  Ret: String;
begin
  with Time do
  begin
    Ret := Day + ' ' + MonthAbbrev + ' ' + Year;
    
    if Hour <> '' then
    begin
      Ret := Ret + ' ' + Hour;
      if Minute <> '' then
      begin
        Ret := Ret + ':' + Minute;
        if Second <> '' then
        begin
          Ret := Ret + ':' + Second;
        end;
      end;
    end;

    if ZoneOffset <> '' then
    begin
      Ret := Ret + ' ' + ZoneOffset;
    end;
  end;

  TimeToString := Ret;
end;

function UnixToDate(const TS: real): String;
const
  SecondsPerDay = 24 * 60 * 60;
var
  DT: TDateTime;
  Res: String;
begin
  Res := DateTimeToStr(TS / SecondsPerDay + EncodeDate(1970, 1, 1));
  {$IFNDEF __GPC__}FormatSettings.{$ENDIF}DateSeparator := '-';
  DT := StrToDateTime(Res);
  Res := FormatDateTime('YYYY-MM-dd"T"hh:nn:ss', DT);
  Res := TimeToString(ShortDateToTime(Res));

  UnixToDate := Res;
end;

{$IFNDEF USE_NLS}
function _(s: PChar): String;
var
  Res: String;
begin
  WriteStr(Res, s);
  _ := Res;
end;
{$ENDIF}

end.
