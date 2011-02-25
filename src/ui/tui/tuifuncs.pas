unit TuiFuncs;

interface

{$IFDEF MSWINDOWS}
uses
  Windows;
{$ENDIF}

procedure FullScreen;

{$IFNDEF HAS_SCREENHEIGHTWIDTH}
function ScreenHeight: word;
function ScreenWidth: word;

{$IFDEF MSWINDOWS}
function ScreenSize: TPoint;
{$ENDIF MSWINDOWS}
{$ENDIF HAS_SCREENHEIGHTWIDTH}

{$IFNDEF USE_NCRT}
function EndWin: integer;
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
{$IFDEF MSWINDOWS}
  ScreenHeight := ScreenSize.Y;
{$ELSE}
{$DEFINE ASSUME8025}
  ScreenHeight := 25;
{$ENDIF MSWINDOWS}
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
{$IFDEF MSWINDOWS}
  ScreenWidth := ScreenSize.X;
{$ELSE}
{$DEFINE ASSUME8025}
  ScreenWidth := 80;
{$ENDIF MSWINDOWS}
{$ENDIF USE_NCRT}
{$ENDIF __GPC__}
end;

{$IFDEF MSWINDOWS}
function ScreenSize: TPoint;
var
  sbi: CONSOLE_SCREEN_BUFFER_INFO;
  Handle: THandle;
begin
  Handle := GetStdHandle(STD_OUTPUT_HANDLE);
  if GetConsoleScreenBufferInfo(Handle, sbi) then
  begin
    ScreenSize.X := sbi.dwsize.X;
    ScreenSize.Y := sbi.dwsize.Y;
  end
  else
  begin
    ScreenSize.X := 80;
    ScreenSize.Y := 25;
  end;
end;
{$ENDIF MSWINDOWS}
{$ENDIF}

{$IFNDEF USE_NCRT}
function EndWin: integer;
begin
  FullScreen;
  NormVideo;
  ClrScr;
  EndWin := 0;
end;
{$ENDIF}

end.
