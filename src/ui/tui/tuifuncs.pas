unit TuiFuncs;

interface

{$INCLUDE "tuidefs.inc"}

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
  ScreenHeight := LINES;
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
  ScreenWidth := COLS;
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
    outbytesleft := inbytesleft div iconv_bytesperchar(StrPas(incharset)) * iconv_bytesperchar(StrPas(outcharset));
    GetMem(outstr, outbytesleft);

    if outstr <> nil then
    begin
      outbuf := outstr;
      iconv_convert(cd, @inbuf, @inbytesleft, @outbuf, @outbytesleft);
      s := StrPas(outstr);
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

end.
