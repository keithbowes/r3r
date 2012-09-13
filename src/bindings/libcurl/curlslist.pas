{$IFNDEF LIBCURL_MONOLITHIC}
unit CurlSList;

{$calling cdecl}

interface
{$ENDIF}

type
  Pinteger  = ^integer;
  PPcurl_slist = ^Pcurl_slist;
  Pcurl_slist  = ^curl_slist;
  curl_slist = record
  data : PChar;
  next : Pcurl_slist;
end;

function curl_slist_append(list:Pcurl_slist; data:Pchar):Pcurl_slist;external 'curl' name 'curl_slist_append';

procedure curl_slist_free_all(list:Pcurl_slist);external 'curl' name 'curl_slist_free_all';

function curl_getdate(p:Pchar; unused:Pinteger):integer;external 'curl' name 'curl_getdate';

{$IFNDEF LIBCURL_MONOLITHIC}

implementation

end.
{$ENDIF}
