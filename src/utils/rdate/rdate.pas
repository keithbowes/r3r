unit RDate;

interface

type
  TRDate = record
    Hour, Millisecond, Minute, Second: String;
    Day, Month, Year: String;
    MonthAbbrev: String;
    ZoneOffset: String;
  end;

function DateToUnix(Time: TRDate): String;
function LongDateToTime(Time: String): TRDate;
function ShortDateToTime(Time: String): TRDate;
function TimeToLongDate(Time: TRDate): String;
function TimeToShortDate(Time: TRDate): String;
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

const
  SecondsPerDay = 24 * 60 * 60;

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
      GetMonthAbbrev := Decem;
    end;
  end;
end;

function GetMonthIndex(const Name: String): String;
begin
  if Name = Jan then
  begin
    GetMonthIndex := '01'
  end
  else if Name = Feb then
  begin
    GetMonthIndex := '02'
  end
  else if Name = Mar then
  begin
    GetMonthIndex := '03'
  end
  else if Name = Apr then
  begin
    GetMonthIndex := '04'
  end
  else if Name = May then
  begin
    GetMonthIndex := '05'
  end
  else if Name = Jun then
  begin
    GetMonthIndex := '06'
  end
  else if Name = Jul then
  begin
    GetMonthIndex := '07'
  end
  else if Name = Aug then
  begin 
    GetMonthIndex := '08'
  end
  else if Name = Sep then
  begin
    GetMonthIndex := '09'
  end
  else if Name = Oct then
  begin
    GetMonthIndex := '10'
  end
  else if Name = Nov then
  begin
    GetMonthIndex := '11'
  end
  else if Name = Decem then
  begin
    GetMonthIndex := '12'
  end
  else
  begin
    GetMonthIndex := Name;
  end
end;

function DateToUnix(Time: TRDate): String;
var
  Day, Month, Year: word;
  Hour, Millisecond, Minute, Second: word;
  TS, TST: real;
begin
  try
    ReadStr(Time.Day, Day);
    ReadStr(GetMonthIndex(Time.Month), Month);
    ReadStr(Time.Year, Year);

    ReadStr(Time.Hour, Hour);
    ReadStr(Time.Millisecond, Millisecond);
    ReadStr(Time.Minute, Minute);
    ReadStr(Time.Second, Second);
  finally
    try
      try
        TST := EncodeTime(Hour, Minute, Second, Millisecond);
        TS := (EncodeDate(Year, Month, Day) + TST - EncodeDate(1970, 1, 1)) * SecondsPerDay;
      except
        TS := 0;
        TST := 0;
      end;
    finally
      WriteStr(Result, Trunc(TS));
    end;
  end;
end;

function LongDateToTime(Time: String): TRDate;
const
  WhitespaceChars = #0#8#9#10#13#32;
var
  List: TStringsList;
  Tm: TRDate;
  Start: byte;
  TP: String;
begin
{$IFDEF USE_NLS}
  textdomain('libr3r');
{$ENDIF}
  List := Split(Time, WhitespaceChars);

  if List.Length = 5 then
  begin
    Start := 1;
  end
  else
  begin
    Start := 0
  end;

  Tm.Day := List.Strings[Start];
  Tm.Month := List.Strings[Start + 1];
  Tm.MonthAbbrev := _(PChar(List.Strings[Start + 1]));
  Tm.Year := List.Strings[Start + 2];
  Tm.ZoneOffset := List.Strings[Start + 4];

  TP := List.Strings[Start + 3];
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
  if Sep = 0 then
  begin
    Exit;
  end;

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

function TimeToLongDate(Time: TRDate): String;
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

  TimeToLongDate := Ret;
end;

function TimeToShortDate(Time: TRDate): String;
var
  Ret: String;
begin
  with Time do
  begin
    Ret := Year + '-' + GetMonthIndex(Month) + '-' + Day;

    if Hour <> '' then
    begin
      Ret := Ret + 'T' + Hour + ':' + Minute + ':' + Second;

      if Length(ZoneOffset) > 0 then
      begin
        Ret := Ret + ZoneOffset;
      end
      else
      begin
        Ret := Ret + 'Z';
      end;
    end;
  end;

  TimeToShortDate := Ret;
end;

function UnixToDate(const TS: real): String;
var
  DT: TDateTime;
  Res: String;
begin
  Res := DateTimeToStr(TS / SecondsPerDay + EncodeDate(1970, 1, 1));
  FormatSettings.DateSeparator := '-';
  DT := StrToDateTime(Res);
  Res := FormatDateTime('YYYY-MM-dd"T"hh:nn:ss', DT);
  Res := TimeToLongDate(ShortDateToTime(Res));

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
