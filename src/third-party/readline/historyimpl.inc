uses
	Strings;

  function HISTENT_BYTES(hs : PHIST_ENTRY) : longint;
  begin
    HISTENT_BYTES:=(strlen(hs^.line))+(strlen(hs^.timestamp));
  end;
