
unit PCRE;

{$calling cdecl}
{$mode delphi}

interface

{
  Automatically converted by H2Pas 1.0.0 from pcre.h
  The following command line parameters were used:
    -D
    -l
    pcre
    -o
    pcre.pas
    -u
    PCRE
    pcre.h
}

    type
      dword = cardinal;
      PByte = ^byte;
      PInteger = ^integer;
      PPByte = ^PByte;
      PPChar = ^PChar;
      PPPChar = ^PPChar;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


  {************************************************
  *       Perl-Compatible Regular Expressions      *
  ************************************************ }
  { This is the public header file for the PCRE library, to be #included by
  applications that call the PCRE functions.

             Copyright (c) 1997-2010 University of Cambridge

  -----------------------------------------------------------------------------
  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

      * Redistributions of source code must retain the above copyright notice,
        this list of conditions and the following disclaimer.

      * Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.

      * Neither the name of the University of Cambridge nor the names of its
        contributors may be used to endorse or promote products derived from
        this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
  -----------------------------------------------------------------------------
 }

type
{$IFDEF FPC}
  size_t = PtrUInt;
{$ELSE}
{$IFDEF __GPC}
  size_t = SizeType;
{$ELSE}
  size_t = cardinal;
{$ENDIF}
{$ENDIF}

{ The current PCRE version information. }
var
  PCRE_MAJOR, PCRE_MINOR: byte;
  PCRE_PRERELEASE, PCRE_DATE: String;

  {Options}
    const
      PCRE_CASELESS = $00000001;
      PCRE_MULTILINE = $00000002;
      PCRE_DOTALL = $00000004;
      PCRE_EXTENDED = $00000008;
      PCRE_ANCHORED = $00000010;
      PCRE_DOLLAR_ENDONLY = $00000020;
      PCRE_EXTRA = $00000040;
      PCRE_NOTBOL = $00000080;
      PCRE_NOTEOL = $00000100;
      PCRE_UNGREEDY = $00000200;
      PCRE_NOTEMPTY = $00000400;
      PCRE_UTF8 = $00000800;
      PCRE_NO_AUTO_CAPTURE = $00001000;
      PCRE_NO_UTF8_CHECK = $00002000;
      PCRE_AUTO_CALLOUT = $00004000;
      PCRE_PARTIAL_SOFT = $00008000;
      PCRE_PARTIAL = $00008000;
      PCRE_DFA_SHORTEST = $00010000;
      PCRE_DFA_RESTART = $00020000;
      PCRE_FIRSTLINE = $00040000;
      PCRE_DUPNAMES = $00080000;
      PCRE_NEWLINE_CR = $00100000;
      PCRE_NEWLINE_LF = $00200000;
      PCRE_NEWLINE_CRLF = $00300000;
      PCRE_NEWLINE_ANY = $00400000;
      PCRE_NEWLINE_ANYCRLF = $00500000;
      PCRE_BSR_ANYCRLF = $00800000;
      PCRE_BSR_UNICODE = $01000000;
      PCRE_JAVASCRIPT_COMPAT = $02000000;
      PCRE_NO_START_OPTIMIZE = $04000000;
      PCRE_NO_START_OPTIMISE = $04000000; { Synonym }
      PCRE_PARTIAL_HARD = $08000000;
      PCRE_NOTEMPTY_ATSTART = $10000000;
      PCRE_UCP = $20000000;
    { Exec-time and get/set-time error codes }
      PCRE_ERROR_NOMATCH = -(1);
      PCRE_ERROR_NULL = -(2);
      PCRE_ERROR_BADOPTION = -(3);
      PCRE_ERROR_BADMAGIC = -(4);
      PCRE_ERROR_UNKNOWN_OPCODE = -(5);
      PCRE_ERROR_UNKNOWN_NODE = -(5); { For backwards compatibility }
      PCRE_ERROR_NOMEMORY = -(6);
      PCRE_ERROR_NOSUBSTRING = -(7);
      PCRE_ERROR_MATCHLIMIT = -(8);
      PCRE_ERROR_CALLOUT = -(9); { Never used by PCRE itself }
      PCRE_ERROR_BADUTF8 = -(10);
      PCRE_ERROR_BADUTF8_OFFSET = -(11);
      PCRE_ERROR_PARTIAL = -(12);
      PCRE_ERROR_BADPARTIAL = -(13);
      PCRE_ERROR_INTERNAL = -(14);
      PCRE_ERROR_BADCOUNT = -(15);
      PCRE_ERROR_DFA_UITEM = -(16);
      PCRE_ERROR_DFA_UCOND = -(17);
      PCRE_ERROR_DFA_UMLIMIT = -(18);
      PCRE_ERROR_DFA_WSSIZE = -(19);
      PCRE_ERROR_DFA_RECURSE = -(20);
      PCRE_ERROR_RECURSIONLIMIT = -(21);
      PCRE_ERROR_NULLWSLIMIT = -(22); { No longer actually used }
      PCRE_ERROR_BADNEWLINE = -(23);
      PCRE_ERROR_BADOFFSET = -(24);
      PCRE_ERROR_SHORTUTF8 = -(25);
    { Request types for pcre_fullinfo() }
      PCRE_INFO_OPTIONS = 0;
      PCRE_INFO_SIZE = 1;
      PCRE_INFO_CAPTURECOUNT = 2;
      PCRE_INFO_BACKREFMAX = 3;
      PCRE_INFO_FIRSTBYTE = 4;
      PCRE_INFO_FIRSTCHAR = 4; { For backwards compatibility }
      PCRE_INFO_FIRSTTABLE = 5;
      PCRE_INFO_LASTLITERAL = 6;
      PCRE_INFO_NAMEENTRYSIZE = 7;
      PCRE_INFO_NAMECOUNT = 8;
      PCRE_INFO_NAMETABLE = 9;
      PCRE_INFO_STUDYSIZE = 10;
      PCRE_INFO_DEFAULT_TABLES = 11;
      PCRE_INFO_OKPARTIAL = 12;
      PCRE_INFO_JCHANGED = 13;
      PCRE_INFO_HASCRORLF = 14;
      PCRE_INFO_MINLENGTH = 15;
    { Request types for pcre_config(). Do not re-arrange, in order to remain
    compatible. }
      PCRE_CONFIG_UTF8 = 0;
      PCRE_CONFIG_NEWLINE = 1;
      PCRE_CONFIG_LINK_SIZE = 2;
      PCRE_CONFIG_POSIX_MALLOC_THRESHOLD = 3;
      PCRE_CONFIG_MATCH_LIMIT = 4;
      PCRE_CONFIG_STACKRECURSE = 5;
      PCRE_CONFIG_UNICODE_PROPERTIES = 6;
      PCRE_CONFIG_MATCH_LIMIT_RECURSION = 7;
      PCRE_CONFIG_BSR = 8;
    { Bit flags for the pcre_extra structure. Do not re-arrange or redefine
    these bits, just add new ones on the end, in order to remain compatible. }
      PCRE_EXTRA_STUDY_DATA = $0001;
      PCRE_EXTRA_MATCH_LIMIT = $0002;
      PCRE_EXTRA_CALLOUT_DATA = $0004;
      PCRE_EXTRA_TABLES = $0008;
      PCRE_EXTRA_MATCH_LIMIT_RECURSION = $0010;
      PCRE_EXTRA_MARK = $0020;

    { Types }
    type
      real_pcre = record
          { declaration; the definition is private }
        end;

      ppcre = ^real_pcre;

{ When PCRE is compiled as a C++ library, the subject pointer type can be
replaced with a custom type. For conventional use, the public interface is a
const char *. }
{$IFDEF FPC}
{$MACRO ON}
{$IFNDEF PCRE_SPTR}
{$DEFINE PCRE_SPTR := PChar}
{$ENDIF}
{$ELSE}
{$IFDEF __GPC__}
{$IFNDEF PCRE_SPTR}
{$DEFINE PCRE_SPTR PChar}
{$ENDIF}
{$ELSE}
type
  PCRE_SPTR = PChar;
{$ENDIF}
{$ENDIF}

    { The structure for passing additional data to pcre_exec(). This is defined in
    such as way as to be extensible. Always add new fields at the end, in order to
    remain compatible. }
    type
      ppcre_extra = ^tpcre_extra;
      tpcre_extra = record
          flags : dword; { Bits for which fields are set }
          study_data : pointer; { Opaque data from pcre_study() }
          match_limit : dword; { Maximum number of calls to match() }
          callout_data : pointer; { Data passed back in callouts }
          tables : PByte; { Pointer to character tables }
          match_limit_recursion : dword; { Max recursive calls to match() }
          mark : PPByte; { For passing back a mark pointer }
        end;

    { The structure for passing out data via the pcre_callout_function. We use a
    structure so that new fields can be added on the end in future versions,
    without changing the API of the function, thereby allowing old clients to work
    without modification. }
      ppcre_callout_block = ^pcre_callout_block;
      pcre_callout_block = record
          version : longint; { Identifies version of block }
    { ------------------------ Version 0 ------------------------------- }
          callout_number : longint; { Number compiled into pattern }
          offset_vector : PInteger; { The offset vector }
          subject : PCRE_SPTR; { The subject being matched }
          subject_length : longint; { The length of the subject }
          start_match : longint; { Offset to start of this match attempt }
          current_position : longint; { Where we currently are in the subject }
          capture_top : longint; { Max current capture }
          capture_last : longint; { Most recently closed capture }
          callout_data : pointer; { Data passed in with the call }
    { ------------------- Added for Version 1 -------------------------- }
          pattern_position : longint; { Offset to next item in the pattern }
          next_item_length : longint; { Length of next item in the pattern }
    { ------------------------------------------------------------------ }
        end;

    { Indirection for store get and free functions. These can be set to
    alternative malloc/free functions if required. Special ones are used in the
    non-recursive case for "frames". There is also an optional callout function
    that is triggered by the (?) regex item. For Virtual Pascal, these definitions
    have to take another form. }
function pcre_malloc(size: size_t): Pointer;
procedure pcre_free(p: Pointer);

const
  pcre_stack_malloc: function(size: size_t): Pointer = pcre_malloc;
  pcre_stack_free: procedure(p: Pointer) = pcre_free;

var
  pcre_callout: function(block: ppcre_callout_block): integer;

{ Exported PCRE functions }
function pcre_compile(pattern: PChar; options: integer; errptr: PPChar; erroffest: PInteger; tableptr: byte): ppcre; external 'pcre';
function pcre_compile2(pattern: PChar; options: integer; errocedptr: integer; errptr: PPChar; erroffest: PInteger; tableptr: byte): ppcre; external 'pcre';
function pcre_study(code: ppcre; options: integer; errptr: PPChar): ppcre_extra; external 'pcre';
function pcre_exec(code: ppcre; extra: ppcre_extra; subject: PChar; length, startoffest, options: integer; ovector: PInteger; ovecsize: integer): integer; external 'pcre';
function pcre_dfa_exec(code: ppcre; extra: ppcre_extra; subject: PChar; length, startoffset, options: integer; overctor: PInteger; ovecsize: integer; workspace: PInteger; wscount: integer): integer; external 'pcre';

function pcre_copy_named_substring(code: ppcre; subject: PChar; ovector: PInteger; stringcount: integer; stringname, buffer: PChar; buffersize: integer): integer; external 'pcre';
function pcre_copy_substring(subject: PChar; ovector: PInteger; stringcount, stringnumber: integer; buffer: PChar; buffersize: integer): integer; external 'pcre';
function pcre_get_named_substring(code: ppcre; subject: PChar; ovector: PInteger; stringcount: integer; stringname: PChar; stringptr: PPChar): integer; external 'pcre';
function pcre_get_stringnumber(code: ppcre; name: PChar): integer; external 'pcre';
function pcre_get_stringtable_entries(code: ppcre; name: PChar; first, last: PPChar): integer; external 'pcre';
function pcre_get_substring(subject: PChar; ovector: PInteger; stringcount, stringnumber: integer; stringptr: PPChar): integer; external 'pcre';
function pcre_get_substring_list(subject: PChar; ovector: PInteger; stringcount: integer; listptr: PPPChar): integer; external 'pcre';
procedure pcre_free_substring(stringptr: PChar); external 'pcre';
procedure pcre_free_substring_list(stringptr: PPChar); external 'pcre';

function pcre_maketables: byte; external 'pcre';

function pcre_fullinfo(code: ppcre; extra: ppcre_extra; what: integer; where: Pointer): integer; external 'pcre';
function pcre_info(code: ppcre; optptr, firstcharptr: PInteger): integer; external 'pcre';
function pcre_refcount(code: ppcre; adjust: integer): integer; external 'pcre';
function pcre_config(what: integer; where: Pointer): integer; external 'pcre';
function pcre_version: PChar; external 'pcre';


implementation

{$IFDEF __GPC__}
uses
  SysUtils;
{$ENDIF}

{ The memory allocation functions were pretty much stolen
  from the Virtual Pascal binding. I confess. }
function pcre_malloc(size: size_t): Pointer;
var
  Res: Pointer;
begin
  GetMem(Res, size);
  pcre_malloc := Res;
end;

procedure pcre_free(p: Pointer);
var
  size: size_t;
begin
  size := SizeOf(ppcre(p)^);
{$IFNDEF __GPC__} {Causes crashes in GPC}
  if (p <> nil) and (size > 0) then
  begin
    FreeMem(p, size);
  end;
{$ENDIF}

  p := nil;
end;

{ parses the pcre_version string to let application developers accomodate
  quirks in specific versions.  Defining these as constants like in the C
  version won't work; this unit can be used with essentially any version of
  PCRE. }
procedure parseversion;
var
  start: byte;
  version: String;

function versiontoint: integer;
var
  err, h, iv: byte;
  s: String;
begin
  start := Pos('.', version);
  if start = 0 then
  begin
    h := Pos('-', version);
    start := Pos(' ', version);

    if h < start then
    begin 
      start := h;
    end;
  end;

  if start <> 0 then
  begin
    s := Copy(version, 1, start - 1);
    Delete(version, 1, start);
    Val(s, iv, err);
    if err = 0 then
    begin
      versiontoint := iv;
    end
    else
    begin
      versiontoint := 0;
    end;
  end
  else
  begin
    versiontoint := 0;
  end;
end;

begin
  version := {$IFNDEF __GPC__}pcre_version{$ELSE}StrPas(pcre_version){$ENDIF};
  PCRE_MAJOR := versiontoint;
  PCRE_MINOR := versiontoint;

  PCRE_DATE := version;
  version := '';

  start := Pos(' ', PCRE_DATE);
  if start = 0 then
  begin
    PCRE_PRERELEASE := '';
  end
  else
  begin
    PCRE_PRERELEASE := Copy(PCRE_DATE, 1, start - 1);
    Delete(PCRE_DATE, 1, start);
  end;
end;

initialization

parseversion;

end.
