{$IFNDEF LIBCURL_MONOLITHIC}
unit CurlEasy;
interface

{
  Automatically converted by H2Pas 1.0.0 from easy.h
  The following command line parameters were used:
    -D
    -l
    curl
    -o
    curleasy.pas
    -p
    -S
    -u
    CurlEasy
    easy.h
}

uses CurlCore;

{$include "curllib.inc"}
{$ENDIF}

  { Pointers to basic pascal types, inserted by h2pas conversion program.}
  Type
    PSmallInt = ^SmallInt;
    PByte     = ^Byte;
    PWord     = ^Word;
    PDWord    = ^DWord;
    PDouble   = ^Double;
    size_t    = integer;
    psize_t   = ^size_t;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

  function curl_easy_init:PCURL;external CurlLib name 'curl_easy_init';

  function curl_easy_setopt(curl:PCURL; option:integer; data:pointer):CURLcode;external CurlLib name 'curl_easy_setopt';

  function curl_easy_perform(curl:PCURL):CURLcode;external CurlLib name 'curl_easy_perform';

  procedure curl_easy_cleanup(curl:PCURL);external CurlLib name 'curl_easy_cleanup';

  function curl_easy_getinfo(curl:PCURL; info:CURLINFO; data:pointer):CURLcode;external CurlLib name 'curl_easy_getinfo';

  function curl_easy_duphandle(curl:PCURL):PCURL;external CurlLib name 'curl_easy_duphandle';


  procedure curl_easy_reset(curl:PCURL);external CurlLib name 'curl_easy_reset';


  function curl_easy_recv(curl:PCURL; buffer:pointer; buflen:size_t; n:Psize_t):CURLcode;external CurlLib name 'curl_easy_recv';


  function curl_easy_send(curl:PCURL; buffer:pointer; buflen:size_t; n:Psize_t):CURLcode;external CurlLib name 'curl_easy_send';

    function curl_easy_escape(handle:PCURL; _string:Pchar; length:longint):PChar;external CurlLib name 'curl_easy_escape';

    function curl_easy_unescape(handle:PCURL; _string:Pchar; length:longint; outlength:Plongint):PChar;external CurlLib name 'curl_easy_unescape';

    function curl_easy_strerror(_para1:CURLcode):PChar;external CurlLib name 'curl_easy_strerror';

    function curl_easy_pause(handle:PCURL; bitmask:longint):CURLcode;external CurlLib name 'curl_easy_pause';

{$IFNDEF LIBCURL_MONOLITHIC}
implementation


end.
{$ENDIF}
