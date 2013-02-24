unit RTitle;

interface

procedure SetNewTitle(const NewTitle: String);

implementation

{$INCLUDE "tuidefs.inc"}

uses
  Dos,
{$IFDEF MSWINDOWS}
  Windows
{$ELSE}
{$IFDEF FPC_UNIX}
  X, XLib, XUtil,
{$ENDIF}
  SysUtils
{$ENDIF};

var
  OrigTitle: String;
{$IFDEF FPC_UNIX}
  WindowDisplay: PDisplay;
  WindowHandle: TWindow;
{$ENDIF}

function GetOriginalTitle: String;
var
  Data: Pointer;
  Res: String;
begin
  Res := GetEnv('R3R_DEFAULT_TITLE');
  if Res = '' then
  begin
{$IFDEF MSWINDOWS}
    GetMem(Data, MAX_PATH);
    GetConsoleTitle(Data, MAX_PATH);
    WriteStr(Res, PChar(Data));
    FreeMem(Data);
{$ELSE}
{$IFDEF FPC_UNIX}
    {$I-}
    ReadStr(GetEnvironmentVariable('WINDOWID'), WindowHandle);
    {$I+}

    if WindowHandle <> 0 then
    begin
      WindowDisplay := XOpenDisplay(nil);
      GetMem(Data, SizeOf(TXTextProperty));
      XGetWMName(WindowDisplay, WindowHandle, Data);
      XCloseDisplay(WindowDisplay);
      WriteStr(Res, PChar(PXTextProperty(Data)^.value));
      FreeMem(Data);
    end;
{$ELSE}
    Data := nil;

    if GetEnv('DISPLAY') <> '' then
    begin
      Res := GetEnv('USER') + '@' + GetEnv('HOSTNAME') + ': ' + GetCurrentDir;
    end
    else
    begin
      Res := String(Data);
    end;
{$ENDIF FPC_UNIX}
{$ENDIF MSWINDOWS}
  end;

  GetOriginalTitle := Res;
end;

procedure SetNewTitle(const NewTitle: String);
{$IFDEF MSWINDOWS}
var
  Data: Pointer;
{$ELSE}
{$IFDEF FPC_UNIX}
var
  Data: Pointer;
  Title: PChar;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Data := PChar(NewTitle);
  SetConsoleTitle(Data);
{$ELSE}
{$IFDEF FPC_UNIX}
  if WindowHandle <> 0 then
  begin
    Title := PChar(NewTitle);
    WindowDisplay := XOpenDisplay(nil);
    GetMem(Data, SizeOf(TXTextProperty));
    XStringListToTextProperty(@Title, 1, Data); 
    XSetWMProperties(WindowDisplay, WindowHandle, Data, nil, nil, 0, nil, nil, nil);
    XFree(PXTextProperty(Data)^.value);
    FreeMem(Data);
    XFlush(WindowDisplay);
    XCloseDisplay(WindowDisplay);
  end;

  { Hack for non-xterm terminal emulators }
  if (GetEnv('DISPLAY') <> '') and (GetEnv('XTERM_VERSION') = '') then
  begin
    SwapVectors;
    Exec(FSearch('printf', GetEnv('PATH')), '\033]0;' + NewTitle + '\007');
    SwapVectors;
  end;
{$ELSE}
  if GetEnv('DISPLAY') <> '' then
  begin
    SwapVectors;
    Exec(FSearch('printf', GetEnv('PATH')), '\033]0;' + NewTitle + '\007');
    SwapVectors;
  end;
{$ENDIF FPC_UNIX}
{$ENDIF MSWINDOWS}
end;

initialization

OrigTitle := GetOriginalTitle;

finalization

SetNewTitle(OrigTitle);

end.
