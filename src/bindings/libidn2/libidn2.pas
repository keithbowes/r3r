
unit LibIDN2;
interface

{
  Automatically converted by H2Pas 1.0.0 from idn2.h
  The following command line parameters were used:
    -D
    -e
    -l
    idn2
    -o
    libidn2.pas
    -u
    LibIDN2
    idn2.h
}

  Type
  PPChar = ^PChar;
  uint8_t = byte;
  Puint8_t  = ^uint8_t;
  PPuint8_t = ^Puint8_t;
{$IFDEF FPC}
{$calling cdecl}
{$PACKRECORDS C}
{$ENDIF}

{$IFDEF LINK_DYNAMIC}
{$IFNDEF __GPC__}
const
{$IFDEF MSWINDOWS}
  Idn2Lib = 'libidn2-0';
{$ELSE}
  Idn2Lib = 'idn2';
{$ENDIF}
{$ELSE}
{$DEFINE Idn2Lib 'idn2'}
{$ENDIF}
{$ELSE}
{$IFNDEF __GPC__}
{$LINKLIB libidn2.a}
{$IFDEF WINDOWS}
{$LINKLIB msvcrt}
{$ENDIF}
{$ENDIF}
{$ENDIF}


  { idn2.h - header file for idn2
     Copyright (C) 2011 Simon Josefsson
  
     This program is free software: you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published by
     the Free Software Foundation, either version 3 of the License, or
     (at your option) any later version.
  
     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU General Public License for more details.
  
     You should have received a copy of the GNU General Public License
     along with this program.  If not, see <http://www.gnu.org/licenses/>.
   }
  {*
   * IDN2_VERSION
   *
   * Pre-processor symbol with a string that describe the header file
   * version number.  Used together with idn2_check_version() to verify
   * header file and run-time library consistency.
    }

    function IDN2_VERSION: PChar;    
  {*
   * IDN2_VERSION_NUMBER
   *
   * Pre-processor symbol with a hexadecimal value describing the header
   * file version number.  For example, when the header version is
   * 1.2.4711 this symbol will have the value 0x01021267.  The last four
   * digits are used to enumerate development snapshots, but for all
   * public releases they will be 0000.
    }
    function IDN2_VERSION_NUMBER: longint;    
  {*
   * IDN2_LABEL_MAX_LENGTH
   *
   * Constant specifying the maximum length of a DNS label to 63
   * characters, as specified in RFC 1034.
    }

  const
    IDN2_LABEL_MAX_LENGTH = 63;    
  {*
   * IDN2_DOMAIN_MAX_LENGTH
   *
   * Constant specifying the maximum length of a DNS domain to 255
   * characters, as specified in RFC 1034.
    }
    IDN2_DOMAIN_MAX_LENGTH = 255;    
  {*
   * idn2_flags:
   * @IDN2_NFC_INPUT: Normalize input string using normalization form C.
   * @IDN2_ALABEL_ROUNDTRIP: Perform optional IDNA2008 lookup roundtrip check.
   *
   * Flags to IDNA2008 functions, to be binary or:ed together.  Specify
   * only 0 if you want the default behaviour.
    }

  type
    idn2_flags =  Longint;
    Const
      IDN2_NFC_INPUT = 1;
      IDN2_ALABEL_ROUNDTRIP = 2;

  { IDNA2008 with UTF-8 encoded inputs.  }
(* Const before type ignored *)

  function idn2_lookup_u8(src:Puint8_t; lookupname:PPuint8_t; flags:longint):longint;external {$IFDEF LINK_DYNAMIC}Idn2Lib{$ENDIF} name 'idn2_lookup_u8';

(* Const before type ignored *)
(* Const before type ignored *)
  function idn2_register_u8(ulabel:Puint8_t; alabel:Puint8_t; insertname:PPuint8_t; flags:longint):longint;external {$IFDEF LINK_DYNAMIC}Idn2Lib{$ENDIF} name 'idn2_register_u8';

  { IDNA2008 with locale encoded inputs.  }
(* Const before type ignored *)
  function idn2_lookup_ul(src:Pchar; lookupname:PPchar; flags:longint):longint;external {$IFDEF LINK_DYNAMIC}Idn2Lib{$ENDIF} name 'idn2_lookup_ul';

(* Const before type ignored *)
(* Const before type ignored *)
  function idn2_register_ul(ulabel:Pchar; alabel:Pchar; insertname:PPchar; flags:longint):longint;external {$IFDEF LINK_DYNAMIC}Idn2Lib{$ENDIF} name 'idn2_register_ul';

  {*
   * idn2_rc:
   * @IDN2_OK: Successful return.
   * @IDN2_MALLOC: Memory allocation error.
   * @IDN2_NO_CODESET: Could not determine locale string encoding format.
   * @IDN2_ICONV_FAIL: Could not transcode locale string to UTF-8.
   * @IDN2_ENCODING_ERROR: Unicode data encoding error.
   * @IDN2_NFC: Error normalizing string.
   * @IDN2_PUNYCODE_BAD_INPUT: Punycode invalid input.
   * @IDN2_PUNYCODE_BIG_OUTPUT: Punycode output buffer too small.
   * @IDN2_PUNYCODE_OVERFLOW: Punycode conversion would overflow.
   * @IDN2_TOO_BIG_DOMAIN: Domain name longer than 255 characters.
   * @IDN2_TOO_BIG_LABEL: Domain label longer than 63 characters.
   * @IDN2_INVALID_ALABEL: Input A-label is not valid.
   * @IDN2_UALABEL_MISMATCH: Input A-label and U-label does not match.
   * @IDN2_NOT_NFC: String is not NFC.
   * @IDN2_2HYPHEN: String has forbidden two hyphens.
   * @IDN2_HYPHEN_STARTEND: String has forbidden starting/ending hyphen.
   * @IDN2_LEADING_COMBINING: String has forbidden leading combining character.
   * @IDN2_DISALLOWED: String has disallowed character.
   * @IDN2_CONTEXTJ: String has forbidden context-j character.
   * @IDN2_CONTEXTJ_NO_RULE: String has context-j character with no rull.
   * @IDN2_CONTEXTO: String has forbidden context-o character.
   * @IDN2_CONTEXTO_NO_RULE: String has context-o character with no rull.
   * @IDN2_UNASSIGNED: String has forbidden unassigned character.
   * @IDN2_BIDI: String has forbidden bi-directional properties.
   *
   * Return codes for IDN2 functions.  All return codes are negative
   * except for the successful code IDN2_OK which are guaranteed to be
   * 0.  Positive values are reserved for non-error return codes.
   *
   * Note that the #idn2_rc enumeration may be extended at a later date
   * to include new return codes.
    }

  type
    idn2_rc =  Longint;
    Const
      IDN2_OK = 0;
      IDN2_MALLOC = -(100);
      IDN2_NO_CODESET = -(101);
      IDN2_ICONV_FAIL = -(102);
      IDN2_ENCODING_ERROR = -(200);
      IDN2_NFC = -(201);
      IDN2_PUNYCODE_BAD_INPUT = -(202);
      IDN2_PUNYCODE_BIG_OUTPUT = -(203);
      IDN2_PUNYCODE_OVERFLOW = -(204);
      IDN2_TOO_BIG_DOMAIN = -(205);
      IDN2_TOO_BIG_LABEL = -(206);
      IDN2_INVALID_ALABEL = -(207);
      IDN2_UALABEL_MISMATCH = -(208);
      IDN2_NOT_NFC = -(300);
      IDN2_2HYPHEN = -(301);
      IDN2_HYPHEN_STARTEND = -(302);
      IDN2_LEADING_COMBINING = -(303);
      IDN2_DISALLOWED = -(304);
      IDN2_CONTEXTJ = -(305);
      IDN2_CONTEXTJ_NO_RULE = -(306);
      IDN2_CONTEXTO = -(307);
      IDN2_CONTEXTO_NO_RULE = -(308);
      IDN2_UNASSIGNED = -(309);
      IDN2_BIDI = -(310);

  { Auxilliary functions.  }
(* Const before type ignored *)

  function idn2_strerror(rc:longint):PChar;external {$IFDEF LINK_DYNAMIC}Idn2Lib{$ENDIF} name 'idn2_strerror';

(* Const before type ignored *)
  function idn2_strerror_name(rc:longint):PChar;external {$IFDEF LINK_DYNAMIC}Idn2Lib{$ENDIF} name 'idn2_strerror_name';

(* Const before type ignored *)
(* Const before type ignored *)
  function idn2_check_version(req_version:Pchar):PChar;external {$IFDEF LINK_DYNAMIC}Idn2Lib{$ENDIF} name 'idn2_check_version';

  procedure idn2_free(ptr:pointer);external {$IFDEF LINK_DYNAMIC}Idn2Lib{$ENDIF} name 'idn2_free';


implementation

function IDN2_VERSION: PChar;
begin
  IDN2_VERSION := idn2_check_version(nil);
end;

function IDN2_VERSION_NUMBER: longint;
var
  err: byte;
  major, minor, patch, ver: longint;
  v: String;

function vertoint: longint;
var
  dot: word;
  i: longint;
  r: String;
begin
  dot := Pos('.', v);
  if dot <> 0 then
  begin
    r := Copy(v, 1, dot - 1);
    Delete(v, 1, dot);
    Val(r, i, err);
    if err = 0 then
    begin
      vertoint := i;
    end
    else
    begin
      vertoint := 0
    end;
  end
  else
  begin
    Val(v, i, err);
    if err = 0 then
    begin
      vertoint := i;
      v := '';
    end
    else
    begin
      vertoint := 0
    end;
  end;
end;

function inttohex(p: longint): integer;
var
  d, m: longint;
  i: longint;
  o, s: String;
begin
  o :='';

  while p > 0 do
    begin
    d := p div 16;
    m := p mod 16;
    case m of
      10: s := 'A';
      11: s := 'B';
      12: s := 'C';
      13: s := 'D';
      14: s := 'E';
      15: s := 'F';
      otherwise WriteStr(s, m);
    end;
    o := s + o; 
    p := d;
  end;

  Val(o, i, err);
  if err <> 0 then
  begin
    i := 0;
  end;

  inttohex := i;
end;

begin
  WriteStr(v, IDN2_VERSION);
  major := vertoint;
  minor := vertoint;
  patch := vertoint;
  ver := (inttohex(major) * 10000000) + (inttohex(minor) * 100000) + inttohex(patch);

  WriteStr(v, '$', ver);
  Val(v, ver, err);
  if err <> 0 then
  begin
    err := 0;
  end;

  IDN2_VERSION_NUMBER := ver;
end;

end.
