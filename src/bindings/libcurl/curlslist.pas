{$IFNDEF LIBCURL_MONOLITHIC}
unit CurlSList;

interface

{$include "curllib.inc"}
{$ENDIF}

type
  Pinteger  = ^integer;
  PPcurl_slist = ^Pcurl_slist;
  Pcurl_slist  = ^curl_slist;
  curl_slist = record
    data : PChar;
    next : Pcurl_slist;
  end;
  time_t = PtrInt;

function curl_slist_append(list:Pcurl_slist; data:Pchar):Pcurl_slist;external {$IFDEF LINK_DYNAMIC}CurlLib{$ENDIF} name 'curl_slist_append';

procedure curl_slist_free_all(list:Pcurl_slist);external {$IFDEF LINK_DYNAMIC}CurlLib{$ENDIF} name 'curl_slist_free_all';

function curl_getdate(p:Pchar; unused:Pinteger):time_t;external {$IFDEF LINK_DYNAMIC}CurlLib{$ENDIF} name 'curl_getdate';

{$IFNDEF LIBCURL_MONOLITHIC}

implementation

end.
{$ENDIF}
