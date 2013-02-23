unit Conv;

interface

{$IFDEF __GPC__}
uses
  GPC;

const
  LineEnding = NewLine;
{$ENDIF}

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
{$IFNDEF __GPC__}
    fh^ := stdout;
{$ELSE}
    OutFile := 'feed.out';
{$ENDIF}
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
  t: time_t;
begin
  t := curl_getdate(StrToPChar(InTime), nil);
  if t = - 1 then
  begin
    FormatTime := InTime; (* Unparsable date format *)
    Exit;
  end;

  FormatTime := UnixToDate(t);
{$ELSE}
var
  ErrPos: byte;
  NTS: real;
begin
  case DateFormat of
    dfShort:
    begin
      FormatTime := TimeToString(ShortDateToTime(InTime));
    end;
    dfLong:
    begin
      FormatTime := TimeToString(LongDateToTime(InTime));
    end;
    dfUnix:
    begin
      Val(InTime, NTS, ErrPos);
      if ErrPos = 0 then
      begin
        FormatTime := UnixToDate(NTS);
      end
      else
      begin
        FormatTime := '';
      end;
    end;
  end;
{$ENDIF}
end;

end.
