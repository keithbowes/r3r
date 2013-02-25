unit Info;

interface

const
  AppName = 'R3R';
  AppVersion = '@VERSION@';
  InstalledPrefix = '@PREFIX@';

procedure SetUserAgentInfo(const uainfo: String);
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

{$IFDEF USE_LIBIDN2}
  , LibIdn2
{$ENDIF}
  
{$IFDEF USE_PCRE}
  , Pcre
{$ENDIF};

var
  UserAgentInfo: String;

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

procedure SetUserAgentInfo(const uainfo: String);
begin
  UserAgentInfo := uainfo;
end;

function UserAgent: String;
var
  Rep: String;
  Ret: String;
begin
  Ret := Settings.GetString('user-agent');

  { Interpret escape sequences }
  Ret := StringReplace(Ret, '%a', AppName + '/' + AppVersion, [rfReplaceAll]);
  Ret := StringReplace(Ret, '%c', '@COMPILER@', [rfReplaceAll]);
{$IFDEF EXPAT_2_0}
  WriteStr(Rep, XML_ExpatVersion);
  Ret := StringReplace(Ret, '%e', StringReplace(Rep, '_', '/', [rfReplaceAll]), [rfReplaceAll]);
{$ELSE}
  Ret := StringReplace(Ret, '%e', '', [rfReplaceAll]);
{$ENDIF}
{$IFDEF USE_LIBIDN2}
  WriteStr(Rep, IDN2_VERSION);
  Ret := StringReplace(Ret, '%i', 'libidn2/' + Rep, [rfReplaceAll]);
{$ELSE}
  Ret := StringReplace(Ret, '%i', '', [rfReplaceAll]);
{$ENDIF}
  Ret := StringReplace(Ret, '%m', '@CPU@', [rfReplaceAll]);
{$IFDEF SOCKETS_SYNAPSE}
  Ret := StringReplace(Ret, '%n', ' Synapse/' + SynapseRelease, [rfReplaceAll]);
{$ELSE}
{$IFDEF SOCKETS_LIBCURL}
  WriteStr(Rep, curl_version);
  Ret := StringReplace(Ret, '%n', Rep, [rfReplaceAll]);
{$ELSE}
  Ret := StringReplace(Ret, '%n', '', [rfReplaceAll]);
{$ENDIF}
{$ENDIF}
  Ret := StringReplace(Ret, '%o', OS, [rfReplaceAll]);
{$IFDEF USE_PCRE}
  WriteStr(Rep, 'PCRE/', PCRE_MAJOR, '.', PCRE_MINOR, '.', PCRE_PRERELEASE, ' (', PCRE_DATE, ')');
  Rep := StringReplace(Rep, '. ', ' ', [rfReplaceAll]);
  Ret := StringReplace(Ret, '%p', Rep, [rfReplaceAll]);
{$ELSE}
  Ret := StringReplace(Ret, '%p', '', [rfReplaceAll]);
{$ENDIF}
{$IFDEF USE_SSL}
  Ret := StringReplace(Ret, '%s', 'U', [rfReplaceAll]);
{$ELSE}
  Ret := StringReplace(Ret, '%s', 'I', [rfReplaceAll]);
{$ENDIF}
  Ret := StringReplace(Ret, '%u', '@UI@', [rfReplaceAll]);
  Ret := StringReplace(Ret, '%w', UserAgentInfo, [rfReplaceAll]);

  { Remove unknown formatting chars }
  while Pos('%', Ret) <> 0 do
  begin
    Delete(Ret, Pos('%', Ret), 2);
  end;
  
  { Remove multiple spaces }
  while Pos('  ', Ret) <> 0 do
  begin
    Ret := StringReplace(Ret, '  ', ' ', [rfReplaceAll]);
  end;

  UserAgent := Ret
end;

initialization

UserAgentInfo := '';

end.
