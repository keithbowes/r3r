{$IFNDEF LIBCURL_MONOLITHIC}
unit CurlVer;

{$calling cdecl}

interface

uses
  CurlCore;

{$ENDIF}

{$IFNDEF LIBCURL_MONOLITHIC}
{$DEFINE LIBCURL_INTERFACE}
{$ENDIF}

{$IFDEF LIBCURL_INTERFACE}
{ Should these be in?  They're too version specific. }
const
  LIBCURL_COPYRIGHT = '1996 - 2011 Daniel Stenberg, <daniel@haxx.se>.';
  LIBCURL_TIMESTAMP = 'Tue Jan 24 08:58:51 UTC 2012';

var
  LIBCURL_VERSION_MAJOR, LIBCURL_VERSION_MINOR, LIBCURL_VERSION_PATCH: integer;
  LIBCURL_VERSION_NUM: integer;

function curl_version:PChar;external 'curl' name 'curl_version';

type
  CURLversion = (CURLVERSION_FIRST,CURLVERSION_SECOND,
CURLVERSION_THIRD,CURLVERSION_FOURTH,
CURLVERSION_LAST);

const
  CURLVERSION_NOW = CURLVERSION_FOURTH;

type
  Pcurl_version_info_data = ^curl_version_info_data;
  curl_version_info_data = record
  age : CURLversion;
  version : PChar;
  version_num : dword;
  host : PChar;
  features : longint;
  ssl_version : PChar;
  ssl_version_num : longint;
  libz_version : PChar;
  protocols : PPChar;
  ares : PChar;
  ares_num : longint;
  libidn : PChar;
  iconv_ver_num : longint;
  libssh_version : PChar;
end;

const
  CURL_VERSION_IPV6 = 1 shl 0;
  CURL_VERSION_KERBEROS4 = 1 shl 1;
  CURL_VERSION_SSL = 1 shl 2;
  CURL_VERSION_LIBZ = 1 shl 3;
  CURL_VERSION_NTLM = 1 shl 4;
  CURL_VERSION_GSSNEGOTIATE = 1 shl 5;
  CURL_VERSION_DEBUG = 1 shl 6;
  CURL_VERSION_ASYNCHDNS = 1 shl 7;
  CURL_VERSION_SPNEGO = 1 shl 8;
  CURL_VERSION_LARGEFILE = 1 shl 9;
  CURL_VERSION_IDN = 1 shl 10;
  CURL_VERSION_SSPI = 1 shl 11;
  CURL_VERSION_CONV = 1 shl 12;
  CURL_VERSION_CURLDEBUG = 1 shl 13;
  CURL_VERSION_TLSAUTH_SRP = 1 shl 14;
  CURL_VERSION_NTLM_WB = 1 shl 15;

function curl_version_info(_para1:CURLversion):Pcurl_version_info_data;external 'curl' name 'curl_version_info';
{$ENDIF}

{$IFNDEF LIBCURL_MONOLITHIC}
{$UNDEF LIBCURL_INTERFACE}
{$DEFINE LIBCURL_INIT}
implementation

{$ENDIF}

{$IFDEF LIBCURL_MONOLITHIC}
{$IFDEF LIBCURL_INIT}
{$DEFINE LIBCURL_INTERFACE}
{$ENDIF}
{$ENDIF}

{$IFNDEF LIBCURL_INTERFACE}
procedure ParseVersion;
var
  Dot: integer;
  err: integer;
  s: String;
  Version: String;
  VInfo: Pcurl_version_info_data;
begin
  VInfo := curl_version_info(CURLVERSION_NOW);
  WriteStr(Version, VInfo^.version);
  Dot := Pos('.', Version);
  s := Copy(Version, 1, Dot - 1);
  Delete(Version, 1, Dot);
  Val(s, LIBCURL_VERSION_MAJOR, err);
  Dot := Pos('.', Version);
  s := Copy(Version, 1, Dot - 1);
  Delete(Version, 1, Dot);
  Val(s, LIBCURL_VERSION_MINOR, err);
  Val(Version, LIBCURL_VERSION_PATCH, err);
  Version := '';

  LIBCURL_VERSION_NUM := VInfo^.version_num;
end;
{$ENDIF}

{$IFDEF LIBCURL_INIT}
initialization

ParseVersion;
{$ENDIF}

{$IFNDEF LIBCURL_MONOLITHIC}
end.
{$ENDIF}
