{
  My iconv import library.  Based on the SUS version.
  Doesn't contain all the features of the GNU version.
}

unit iconv;

{ $DEFINE USE_LIBICONV}

{$X+}

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

{$IFNDEF __GPC__}
const
{$IFNDEF USE_LIBICONV}
  FuncPref = '';
  IconvLib = '';
{$ELSE}
  FuncPref = 'lib';
{$IFNDEF MSWINDOWS}
  IconvLib = 'libiconv';
{$ELSE}
  IconvLib = 'libiconv2';
{$ENDIF}
{$ENDIF}
{$ELSE}
{$IFNDEF FuncPref}
{$DEFINE FuncPref 'lib'}
{$ENDIF}
{$IFNDEF IconvLib}
{$DEFINE IconvLib 'iconv'}
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

function iconv_open(tocode, fromcode: PChar): iconv_t; external {$IFDEF USE_LIBICONV}IconvLib{$ENDIF} name FuncPref+'iconv_open';
function iconv_convert(cd: iconv_t; inbuf: PPChar; inbytesleft: psize_t; outbuf: PPChar; outbytesleft: psize_t): size_t; external {$IFDEF USE_LIBICONV}IconvLib{$ENDIF} name FuncPref+'iconv';
function iconv_close(cd: iconv_t): integer; external {$IFDEF USE_LIBICONV}IconvLib{$ENDIF} name FuncPref+'iconv_close';

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

function iconv_open_into(tocode, fromcode: PChar; resultp: piconv_allocation_t): integer; external {$IFDEF USE_LIBICONV}IconvLib{$ENDIF} name 'libiconv_open_into';
function iconvctl(cd: iconv_t; request: integer; argument: Pointer): integer; external {$IFDEF USE_LIBICONV}IconvLib{$ENDIF} name 'libiconvctl';
{$ENDIF}

{ iconvstr is a convenience function in Solaris.  Please use the more portable iconp instead. }
{$IFDEF SOLARIS}
function iconvstr(tocode, fromcode: PChar; var inarray: PChar; var inlen: size_t; var outarray: PChar; var outlen: size_t; flags: integer): size_t; external name 'iconvstr';
{$ENDIF}

{$calling default}

{ The minimum number of bytes a character in charset enc takes }
function iconv_bytesperchar(enc: String): byte;

function iconv_convert_error: size_t;

{ Convenience function to use iconv and convert to a Pascal string } 
function iconp(const instr, fromcode, tocode: PChar): String;
{$IFNDEF __GPC__}
{ GPC doesn't support function overloading }
function iconp(const instr, fromcode, tocode: String): String;
{$ENDIF}

implementation

uses
  Strings;

function iconv_bytesperchar(enc: String): byte;
var
  i: byte;
begin
  for i := 1 to Length(enc) do
  begin
    enc[i] := UpCase(enc[i]);
  end;

  if (Pos('ISO-8859', enc) = 1) or (Pos('WINDOWS', enc) = 1) or
    (Pos('ASCII', enc) <> 0) or (enc = 'UTF-8') then
  begin
    iconv_bytesperchar := 1;
  end
  else if (Pos('EUC', enc) = 1) or (Pos('KOI', enc) = 1) or
    (enc = 'UTF-16') then
  begin
    iconv_bytesperchar := 2;
  end
  else
  begin
    iconv_bytesperchar := 4;
  end;
end;

function iconv_convert_error: size_t;
type
  TConvertRec = record
    case Boolean of
      true: (i: integer);
      false: (s: size_t);
    end;
var
  cr: TConvertRec;
begin
  cr.i := -1;
  iconv_convert_error := cr.s;
end;

function iconp(const instr, fromcode, tocode: PChar): String;
var
  cd: iconv_t;
{$IFDEF USE_LIBICONV}
  i: integer;
{$ENDIF}
  inbuf, outbuf, outstr: PChar;
  inbytesleft, outbytesleft: size_t;
begin
  cd := iconv_open(tocode, fromcode);
  if cd <> iconv_t(-1) then
  begin
{$IFDEF USE_LIBICONV}
    i := 1;
    iconvctl(cd, ICONV_SET_TRANSLITERATE, @i);
{$ENDIF}
    inbuf := instr;
    inbytesleft := StrLen(inbuf) + 1;
    outbytesleft := inbytesleft * iconv_bytesperchar(tocode);
    
    GetMem(outstr, outbytesleft);
    outbuf := outstr;
    if iconv_convert(cd, @inbuf, @inbytesleft, @outbuf, @outbytesleft) = iconv_convert_error then
    begin
      WriteStr(iconp, instr);
    end
    else
    begin
      WriteStr(iconp, outstr);
    end;
    iconv_close(cd);
    FreeMem(outstr);
  end;
end;

{$IFNDEF __GPC__}
function iconp(const instr, fromcode, tocode: String): String;
begin
  iconp := iconp(PChar(instr), PChar(fromcode), PChar(tocode));
end;
{$ENDIF}

end.
