unit TuiFuncs;

interface

{$INCLUDE "tuidefs.inc"}

type
  TSysCharSet = set of char;

procedure FullScreen;

{$IFNDEF HAS_SCREENHEIGHTWIDTH}
function ScreenHeight: word;
function ScreenWidth: word;
{$ENDIF HAS_SCREENHEIGHTWIDTH}

{$IFNDEF USE_NCRT}
function EndWin: integer;
{$ENDIF}

{$IFDEF NO_SUPPORTS_UNICODE}
function UTF8Decode(const s: String): String;
{$ENDIF}

procedure TuiConvertCodeSet(var s: String);
procedure TuiEcho(s: String; const NewLine: Boolean; const flen: cardinal);
{ Shortcut procedures: }
procedure TuiWrite(const s: String);
procedure TuiWriteLn(const s: String);
function TuiWriteWrapped(Line: String; BreakChars: TSysCharset; MaxCol, MaxRow: integer): integer;

implementation

uses
{$IFDEF USE_ICONV}
  iconv, RSettings, RStrings, SysUtils, 
{$ENDIF}
{$IFNDEF USE_NCRT}
  Crt
{$ELSE}
  nCrt, nCurses
{$ENDIF};

procedure FullScreen;
begin
  Window(1, 1, ScreenWidth, ScreenHeight);
  ClrScr
end;

{$IFNDEF HAS_SCREENHEIGHTWIDTH}
function ScreenHeight: word;
begin
{$IFDEF __GPC__}
  ScreenHeight := ScreenSize.Y;
{$ELSE}
{$IFDEF USE_NCRT}
  refresh;
  doupdate;
  ScreenHeight := getmaxy(stdscr);
{$ELSE}
  ScreenHeight := 25;
{$ENDIF USE_NCRT}
{$ENDIF __GPC__}
end;

function ScreenWidth: word;
begin
{$IFDEF __GPC__}
  ScreenWidth := ScreenSize.X;
{$ELSE}
{$IFDEF USE_NCRT}
  refresh;
  doupdate;
  ScreenWidth := getmaxx(stdscr);
{$ELSE}
  ScreenWidth := 80;
{$ENDIF USE_NCRT}
{$ENDIF __GPC__}
end;
{$ENDIF HAS_SCREENHEIGHTWIDTH}

{$IFNDEF USE_NCRT}
function EndWin: integer;
begin
  FullScreen;
  NormVideo;
  ClrScr;
  EndWin := 0;
end;
{$ENDIF}

{$IFDEF NO_SUPPORTS_UNICODE}
function UTF8Decode(const s: String): String;
begin
  UTF8Decode := s
end;
{$ENDIF NO_SUPPORTS_UNICODE}

procedure TuiConvertCodeset(var s: String);
{$IFDEF USE_ICONV}
var
  cd: iconv_t;
{$IFDEF USE_LIBICONV}
  i: integer;
{$ENDIF}
  inbuf: PChar;
  inbytesleft, outbytesleft: size_t;
  incharset, outcharset: PChar;
  outbuf, outstr: PChar;
{$ENDIF}
begin
{$IFDEF USE_ICONV}
  incharset := 'UTF-8';
  outcharset := StrToPChar(Settings.GetString('display-encoding'));

  cd := iconv_open(outcharset, incharset);
  if cd <> iconv_t(-1) then
  begin
{$IFDEF USE_LIBICONV}
    i := 1;
    iconvctl(cd, ICONV_SET_TRANSLITERATE, @i);
{$ENDIF}
    inbuf := StrToPChar(s);
    inbytesleft := Length(s) + 1;
    outbytesleft := inbytesleft;
    GetMem(outstr, outbytesleft);

    if outstr <> nil then
    begin
      outbuf := outstr;
      if iconv_convert(cd, @inbuf, @inbytesleft, @outbuf, @outbytesleft) <> iconv_convert_error then
      begin
        WriteStr(s, outstr);
      end;
      FreeMem(outstr);
    end;
{$IFDEF USE_LIBICONV}
    iconv_close(cd);
  end;
{$ELSE}
  end;

  iconv_close(cd);
{$ENDIF}
{$ENDIF}
end;

procedure TuiEcho(s: String; const NewLine: Boolean; const flen: cardinal);
begin
  TuiConvertCodeset(s);

  if not NewLine then
  begin
    Write(s:flen);
  end
  else
  begin
    WriteLn(s:flen);
  end;
end;

procedure TuiWrite(const s: String);
begin
  TuiEcho(s, false, 0);
end;

procedure TuiWriteLn(const s: String);
begin
  TuiEcho(s, true, 0);
end;

{$IFDEF HAS_WRAPTEXT}
function TuiWriteWrapped(Line: String; BreakChars: TSysCharset; MaxCol, MaxRow: integer): PtrInt;
var
  c, t: integer;
  Ret: String;
begin
  Line := WrapText(Line, LineEnding, BreakChars, MaxCol - 3);
  Ret := Line;

  c := Length(Line);
  t := 0;
  while (c <> 0) and (MaxRow > 1) do
  begin
    c := Pos(LineEnding, Line);
    TuiWrite(Copy(Line, 1, c));

    Delete(Line, 1, c);
    Dec(MaxRow);
    t := t + c;
  end;

  TuiWriteWrapped := Length(Ret) - t;
end;
{$ELSE}
function TuiWriteWrapped(Line: String; BreakChars: TSysCharset; MaxCol, MaxRow: integer): PtrInt;
var
  DescLine: String;
  Len: word;
  Remaining: PtrInt;
begin
  repeat
    Len := MaxCol;
    if Length(Line) > Len then
    begin
      repeat
        Dec(Len)
      until (Len = 1) or (Line[Len] in BreakChars);
    end;

    DescLine := Copy(Line, 1, Len);
    Delete(Line, 1, Len);
    Dec(MaxRow);
    TuiWriteLn(DescLine);
    Remaining := Length(Line);
  until (MaxRow = 1) or (Remaining = 0);

  TuiWriteWrapped := Remaining;
end;
{$ENDIF}

end.
