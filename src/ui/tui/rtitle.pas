unit RTitle;

interface

function GetOriginalTitle: String;
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
  RSettings_Routines, SysUtils
{$ENDIF};

{$IFDEF FPC_UNIX}
var
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
    try
    ReadStr(GetEnvironmentVariable('WINDOWID'), WindowHandle);
    finally
      if WindowHandle <> 0 then
      begin
        try
          WindowDisplay := XOpenDisplay(nil);
          GetMem(Data, SizeOf(TXTextProperty));
          XGetWMName(WindowDisplay, WindowHandle, Data);
          XCloseDisplay(WindowDisplay);
        finally
          WriteStr(Res, PChar(PXTextProperty(Data)^.value));
          FreeMem(Data);
        end;
      end;
    end;
{$ELSE}
    if (GetEnv('HOSTNAME') <> '') and (GetEnv('USER') <> '') then
    begin
      Res := GetEnv('USER') + '@' + GetEnv('HOSTNAME') + ': ';
    end;
    Res := Res + GetCurrentDir;
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
    try
      WindowDisplay := XOpenDisplay(nil);
      GetMem(Data, SizeOf(TXTextProperty));
      XStringListToTextProperty(@Title, 1, Data); 
      XSetWMProperties(WindowDisplay, WindowHandle, Data, nil, nil, 0, nil, nil, nil);
      XFree(PXTextProperty(Data)^.value);
      XFlush(WindowDisplay);
      XCloseDisplay(WindowDisplay);
    finally
      FreeMem(Data);
    end;
  end;

  { Hack for non-xterm terminal emulators }
  if GetEnv('XTERM_VERSION') = '' then
  begin
    SwapVectors;
    Exec(GetInstalledPrefix + '/bin/r3r-settitle', NewTitle);
    SwapVectors;
  end;
{$ELSE}
  SwapVectors;
  Exec(GetInstalledPrefix + '/bin/r3r-settitle', NewTitle);
  SwapVectors;
{$ENDIF FPC_UNIX}
{$ENDIF MSWINDOWS}
end;

end.
