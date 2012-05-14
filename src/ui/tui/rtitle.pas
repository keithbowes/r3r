unit RTitle;

interface

procedure SetNewTitle(const NewTitle: String);

implementation

uses
  Dos,
{$IFDEF MSWINDOWS}
  Windows
{$ELSE}
  RSettings_Routines, SysUtils
{$ENDIF};

var
  OrigTitle: String;

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
    WriteStr(Res, Data);
    FreeMem(Data);
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
{$ENDIF MSWINDOWS}
  end;

  GetOriginalTitle := Res;
end;

procedure SetNewTitle(const NewTitle: String);
{$IFDEF MSWINDOWS}
var
  Data: Pointer;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Data := PChar(NewTitle);
  SetConsoleTitle(Data);
{$ELSE}
  if GetEnv('DISPLAY') <> '' then
  begin
    SwapVectors;
    Exec(GetInstalledPrefix + PathDelim + 'bin' + PathDelim + 'r3r-settitle', '"' + NewTitle + '"');
    SwapVectors;
  end;
{$ENDIF MSWINDOWS}
end;

initialization

OrigTitle := GetOriginalTitle;

finalization

SetNewTitle(OrigTitle);

end.
