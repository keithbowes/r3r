unit Conv;

interface

type
  TDateFormat = (dfLong, dfShort, dfUnix);
  PText = ^text;

var
  OutFile: String;

function GetFileHandle: PText;
function FormatTime(InTime: String; DateFormat: TDateFormat): String;

implementation

uses
{$IFDEF SOCKETS_LIBCURL}
  CurlSList,
{$ELSE}
  SysUtils,
{$ENDIF}
  RDate, RStrings;

function GetFileHandle: PText;
const
  erste: Boolean = true;
var
  fh: PText;
begin
  New(fh);

  if OutFile = '' then
  begin
    fh^ := stdout;
  end;

  if OutFile <> '' then
  begin
    Assign(fh^, OutFile);
    
    if erste then
    begin
      Rewrite(fh^);
      erste := false;
      Close(fh^);
      Assign(fh^, OutFile);
    end;

    Append(fh^);
  end;

  GetFileHandle := fh;
end;

function FormatTime(InTime: String; DateFormat: TDateFormat): String;
{$IFDEF SOCKETS_LIBCURL}
var
  Res: String;
  t: time_t;
begin
  t := curl_getdate(StrToPChar(InTime), nil);
  if t = - 1 then
  begin
    FormatTime := InTime; (* Unparsable date format *)
    Exit;
  end;

  case DateFormat of
    dfUnix:
    begin
      WriteStr(Res, t);
      FormatTime := Res;
    end;
    dfLong:
    begin
      FormatTime := UnixToDate(t);
    end;
    dfShort:
    begin
      FormatTime := TimeToShortDate(LongDateToTime(UnixToDate(t)));
    end;
  end;
{$ELSE}
var
  Date: TRDate;
begin
  Date := ShortDateToTime(InTime);
  if Date.Year = '' then
  begin
    Date := LongDateToTime(InTime);
    if Date.Year = '' then
    begin
      FormatTime := InTime; (* Unparsable date format *)
      Exit
    end
  end;

  case DateFormat of
    dfShort:
    begin
      FormatTime := TimeToShortDate(Date);
    end;
    dfLong:
    begin
      FormatTime := TimeToLongDate(Date);
    end;
    dfUnix:
    begin
      FormatTime := DateToUnix(Date);
    end;
  end;
{$ENDIF}
end;

end.
