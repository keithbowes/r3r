unit Conv;

interface

{$IFDEF __GPC__}
uses
  GPC;

const
  LineEnding = NewLine;
{$ENDIF}

type
  PText = ^text;

var
  OutFile: String;

function GetFileHandle: PText;
function FormatTime(InTime, OutFmt: String): String;

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

function FormatTime(InTime, OutFmt: String): String;
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
begin
  FormatTime := InTime;
{$ENDIF}
end;

end.
