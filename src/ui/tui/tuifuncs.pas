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

implementation

uses
{$IFNDEF USE_NCRT}
  Crt
{$ELSE}
  NCrt, Ncurses
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
  ScreenHeight := Lines;
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
  ScreenWidth := Cols;
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

end.
