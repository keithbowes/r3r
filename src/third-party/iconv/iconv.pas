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

function iconv_open(to_char, from_char: PChar): iconv_t; external {$ifdef USE_LIBICONV}{$ifdef MSWINDOWS}'libiconv2'{$else}'iconv'{$endif}{$endif} name {$ifndef USE_LIBICONV}'iconv_open'{$else}'libiconv_open'{$endif};
function iconv_convert(cd: iconv_t; inbuf: PPChar; inbytesleft: psize_t; outbuf: PPChar; outbytesleft: psize_t): SizeType; external {$ifdef USE_LIBICONV}{$ifdef MSWINDOWS}'libiconv2'{$else}'iconv'{$endif}{$endif} name {$ifndef USE_LIBICONV}'iconv'{$else}'libiconv'{$endif};
function iconv_close(cd: iconv_t): integer; external {$ifdef USE_LIBICONV}{$ifdef MSWINDOWS}'libiconv2'{$else}'iconv'{$endif}{$endif} name {$ifndef USE_LIBICONV}'iconv_close'{$else}'libiconv_close'{$endif};

{$calling default}

function iconv_bytesperchar(enc: String): byte;

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

end.
