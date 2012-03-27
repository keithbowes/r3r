{
  My iconv import library.  Based on the SUS version.
  Doesn't contain all the features of the GNU version.
}

unit iconv;

interface

{$IFDEF FPC}
{$IFNDEF UNIX}
{$DEFINE USE_LIBICONV}
{$ENDIF}
{$ENDIF}

{$IFDEF UNIX}
{$IFNDEF USE_LIBICONV}
{$LINKLIB c}
{$ENDIF}
{$ENDIF}

{ These aren't right, but I can't find their right values }
const
  ICONV_IGNORE_NULL = 1;
  ICONV_REPLACE_INVALID = 2;

type
  iconv_t = Pointer;

{$IFDEF FPC}
  SizeType = PtrUInt;
{$ELSE}
{$IFNDEF __GPC__} { GPC already has this type defined }
  SizeType = cardinal;
{$ENDIF}
{$ENDIF}

  size_t = SizeType;
  psize_t = ^size_t;

{$IFDEF __GPC__}
  PPChar = ^PChar;
{$ENDIF}

{$calling cdecl}

function iconv_open(tocode, fromcode: PChar): iconv_t; external {$ifdef USE_LIBICONV}{$ifdef MSWINDOWS}'libiconv2'{$else}'iconv'{$endif}{$endif} name {$ifndef USE_LIBICONV}'iconv_open'{$else}'libiconv_open'{$endif};
function iconv_convert(cd: iconv_t; inbuf: PPChar; inbytesleft: psize_t; outbuf: PPChar; outbytesleft: psize_t): SizeType; external {$ifdef USE_LIBICONV}{$ifdef MSWINDOWS}'libiconv2'{$else}'iconv'{$endif}{$endif} name {$ifndef USE_LIBICONV}'iconv'{$else}'libiconv'{$endif};
function iconv_close(cd: iconv_t): integer; external {$ifdef USE_LIBICONV}{$ifdef MSWINDOWS}'libiconv2'{$else}'iconv'{$endif}{$endif} name {$ifndef USE_LIBICONV}'iconv_close'{$else}'libiconv_close'{$endif};

{$IFDEF USE_LIBICONV}
{ Extras not available in non-GNU versions of iconv }
const
  ICONV_TRIVIALP = 0;
  ICONV_GET_TRANSLITERATE = 1;
  ICONV_SET_TRANSLITERATE = 2;
  ICONV_GET_DISCARD_ISLEQ = 3;
  ICONV_SET_DISCARD_ILSEQ = 4;

type
  piconv_allocation_t = iconv_t;

function iconv_open_into(tocode, fromcode: PChar; resultp: piconv_allocation_t): integer; external {$ifdef MSWINDOWS}'libiconv2'{$else}'iconv'{$endif} name 'libiconv_open_into';
function iconvctl(cd: iconv_t; request: integer; argument: Pointer): integer; external {$ifdef MSWINDOWS}'libiconv2'{$else}'iconv'{$endif} name 'libiconvctl';
{$ENDIF}

{$IFDEF SOLARIS}
function iconvstr(tocode, fromcode: PChar; var inarray: PChar; var inlen: size_t; var outarray: PChar; var outlen: size_t; flags: integer): size_t; external name 'iconvstr';
{$ENDIF}

{$calling default}

function iconv_bytesperchar(enc: String): byte;

{$IFNDEF SOLARIS}
function iconvstr(tocode, fromcode: PChar; var inarray: PChar; var inlen: size_t; var outarray: PChar; var outlen: size_t; flags: integer): size_t;
{$ENDIF}

implementation

uses
  SysUtils;

function iconv_bytesperchar(enc: String): byte;
var
  Ret: byte;
begin
  enc := UpperCase(enc);

  if (Pos('ISO-8859', enc) = 1) or (Pos('WINDOWS', enc) = 1) or
    (enc = 'ASCII') or (enc = 'UTF-8') then
  begin
    Ret := 1;
  end
  else if (Pos('EUC', enc) = 1) or (Pos('KOI', enc) = 1) or
    (enc = 'UTF-16') then
  begin
    Ret := 2;
  end
  else
  begin
    Ret := 4;
  end;

  iconv_bytesperchar := Ret;
end;

{$IFNDEF SOLARIS}
function iconvstr(tocode, fromcode: PChar; var inarray: PChar; var inlen: size_t; var outarray: PChar; var outlen: size_t; flags: integer): size_t;
var
  cd: iconv_t;
  outbuf: PChar;
  trueinlen: size_t;
  Res: integer;
{$IFDEF USE_LIBICONV}
  i: integer;
{$ENDIF}
begin
  if inlen = 0 then
  begin
    inlen := StrLen(inarray) + 1;
  end;
  trueinlen := inlen;

  outlen := inlen div iconv_bytesperchar(StrPas(fromcode)) * iconv_bytesperchar(StrPas(tocode));
  cd := iconv_open(tocode, fromcode);

  if cd <> iconv_t(-1) then
  begin
{$IFDEF USE_LIBICONV}
    if flags and ICONV_REPLACE_INVALID = ICONV_REPLACE_INVALID then
    begin
      i := 1;
      iconvctl(cd, ICONV_SET_DISCARD_ILSEQ, @i);
      iconvctl(cd, ICONV_SET_TRANSLITERATE, @i);
    end;
{$ELSE}
    flags := 0;
{$ENDIF}
    outbuf := outarray;
    iconv_convert(cd, @inarray, @inlen, @outbuf, @outlen);
    iconv_close(cd);
    outarray[trueinlen - outlen] := #0;
    Res := 0;
  end
  else
  begin
    Res := -1;
  end;

  iconvstr := Res;
end;
{$ENDIF}

end.