unit Info;

interface

const
  AppName = 'R3R';
  AppVersion = '@VERSION@';
  InstalledPrefix = '@PREFIX@';

function UserAgent: String;

implementation

uses
  RSettings, SysUtils
{$IFDEF __GPC__}
  , GPC
{$ELSE}
  {$IF DEFINED(FPC) and DEFINED(UNIX)}
    , BaseUnix
  {$ELSE}
    {$IFDEF MSWINDOWS}
      , Windows
    {$ELSE}
      , Dos
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF EXPAT_2_0}
  , Expas
{$ENDIF}

{$IFDEF SOCKETS_SYNAPSE}
  , BlckSock
{$ENDIF}
  
{$IFDEF SOCKETS_LIBCURL}
  , CurlVer
{$ENDIF}
  
{$IFDEF USE_PCRE}
  , Pcre
{$ENDIF};

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
        WriteStr(Version, dwMajorVersion, '.', dwMinorVersion);
      end;
      Dispose(LPOSVERSIONINFO(Data));
    {$ELSE}
      Name := 'DOS';
      WriteStr(Version, Lo(DosVersion), '.', Hi(DosVersion));
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

  OS := Name + ' ' + Version;
end;

function UserAgent: String;
var
{$IFDEF USE_PCRE}
  Data: String;
{$ENDIF}
  Ret: String;
begin
  Ret := Settings.GetString('user-agent');
  Ret := StringReplace(Ret, '%a', AppName + '/' + AppVersion, []);
  Ret := StringReplace(Ret, '%c', '@COMPILER@', []);
{$IFDEF EXPAT_2_0}
  Ret := StringReplace(Ret, '%e', StringReplace(StrPas(XML_ExpatVersion), '_', '/', []), []);
{$ELSE}
  Ret := StringReplace(Ret, '%e', '', []);
{$ENDIF}
  Ret := StringReplace(Ret, '%o', OS, []);
{$IFDEF USE_PCRE}
  WriteStr(Data, 'PCRE/', PCRE_MAJOR, '.', PCRE_MINOR, '.', PCRE_PRERELEASE, ' (', PCRE_DATE, ')');
  Data := StringReplace(Data, '. ', ' ', [rfReplaceAll]);
  Ret := StringReplace(Ret, '%p', Data, []);
{$ELSE}
  Ret := StringReplace(Ret, '%p', '', []);
{$ENDIF}
{$IFDEF SOCKETS_SYNAPSE}
  Ret := StringReplace(Ret, '%s', ' Synapse/' + SynapseRelease, []);
{$ELSE}
{$IFDEF SOCKETS_LIBCURL}
  Ret := StringReplace(Ret, '%s', StrPas(curl_version), []);
{$ELSE}
  Ret := StringReplace(Ret, '%s', '', []);
{$ENDIF}
{$ENDIF}
  Ret := StringReplace(Ret, '%u', '@UI@', []);
  
  while Pos('  ', Ret) <> 0 do
  begin
    Ret := StringReplace(Ret, '  ', ' ', [rfReplaceAll]);
  end;

  UserAgent := Ret
end;

end.
