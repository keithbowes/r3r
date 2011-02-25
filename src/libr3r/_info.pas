unit Info;

interface

const
  AppName = 'R3R';
  AppVersion = '@VERSION@';
  InstalledPrefix = '@PREFIX@';

function UserAgent: String;

implementation

uses
{$IFDEF __GPC__}
  GPC
{$ELSE}
  {$IF DEFINED(FPC) and DEFINED(UNIX)}
    BaseUnix
  {$ELSE}
    {$IFDEF MSWINDOWS}
      Windows
    {$ELSE}
      Dos
    {$ENDIF}
  {$ENDIF}
{$ENDIF},

{$IFDEF SOCKETS_SYNAPSE}
  BlckSock,
{$ENDIF}
  SysUtils;

function Os: String;
var
  Name, Version: String;
  Data: Pointer;
begin
{$IFDEF __GPC__}
  Name := SystemInfo.OSName;
  Version := SystemInfo.OSRelease;
{$ELSE}
  {$IF DEFINED(FPC) and DEFINED(UNIX)}
    GetMem(Data, SizeOf(UtsName));
    FpUname(PUtsName(Data)^);
    Name := PUtsName(Data)^.SysName;
    Version := PUtsName(Data)^.Release;
    FreeMem(Data);
  {$ELSE}
    {$IFDEF MSWINDOWS}
      New(LPOSVERSIONINFO(Data));
      with OSVERSIONINFO(Data^) do
      begin
        dwOSVersionInfoSize := SizeOf(OSVERSIONINFO);
        GetVersionEx(Data);
        case dwPlatformId of
          VER_PLATFORM_WIN32_NT:
          begin
            Name := 'Windows NT';
          end;
          VER_PLATFORM_WIN32_WINDOWS:
          begin
            Name := 'Windows';
          end;
          VER_PLATFORM_WIN32s:
          begin
            Name := 'Win32s';
          end;
        end;
        Version := IntToStr(dwMajorVersion) + '.' + IntToStr(dwMinorVersion);
      end;
      Dispose(LPOSVERSIONINFO(Data));
    {$ELSE}
      Name := 'DOS';
      Version := IntToStr(Lo(DosVersion)) + '.' + IntToStr(Hi(DosVersion));
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

  OS := Name + ' ' + Version;
end;

function UserAgent: String;
begin
  UserAgent := AppName + '/' + AppVersion + ' (' + OS + '; @COMPILER@)'
{$IFDEF SOCKETS_SYNAPSE}
    + ' Synapse/' + SynapseRelease
{$ENDIF};
end;

end.
