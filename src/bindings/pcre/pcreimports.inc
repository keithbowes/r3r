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


{ Imported PCRE functions }
function pcre_compile(pattern: PChar; options: integer; errptr: PPChar; erroffest: PInteger; tableptr: PByte): ppcre;{$IFDEF PCRE_IMPORT} external {$IFDEF LINK_DYNAMIC} PcreLib{$ENDIF};{$ENDIF}
function pcre_compile2(pattern: PChar; options: integer; errocedptr: integer; errptr: PPChar; erroffest: PInteger; tableptr: PByte): ppcre;{$IFDEF PCRE_IMPORT} external {$IFDEF LINK_DYNAMIC} PcreLib{$ENDIF};{$ENDIF}
function pcre_study(code: ppcre; options: integer; errptr: PPChar): ppcre_extra;{$IFDEF PCRE_IMPORT} external {$IFDEF LINK_DYNAMIC} PcreLib{$ENDIF};{$ENDIF}
function pcre_exec(code: ppcre; extra: ppcre_extra; subject: PChar; length, startoffest, options: integer; ovector: PInteger; ovecsize: integer): integer;{$IFDEF PCRE_IMPORT} external {$IFDEF LINK_DYNAMIC} PcreLib{$ENDIF};{$ENDIF}
function pcre_dfa_exec(code: ppcre; extra: ppcre_extra; subject: PChar; length, startoffset, options: integer; overctor: PInteger; ovecsize: integer; workspace: PInteger; wscount: integer): integer;{$IFDEF PCRE_IMPORT}
  external {$IFDEF LINK_DYNAMIC} PcreLib{$ENDIF};{$ENDIF}

function pcre_copy_named_substring(code: ppcre; subject: PChar; ovector: PInteger; stringcount: integer; stringname, buffer: PChar; buffersize: integer): integer;{$IFDEF PCRE_IMPORT} external {$IFDEF LINK_DYNAMIC} PcreLib{$ENDIF};{$ENDIF}
function pcre_copy_substring(subject: PChar; ovector: PInteger; stringcount, stringnumber: integer; buffer: PChar; buffersize: integer): integer;{$IFDEF PCRE_IMPORT} external {$IFDEF LINK_DYNAMIC} PcreLib{$ENDIF};{$ENDIF}
function pcre_get_named_substring(code: ppcre; subject: PChar; ovector: PInteger; stringcount: integer; stringname: PChar; stringptr: PPChar): integer;{$IFDEF PCRE_IMPORT} external {$IFDEF LINK_DYNAMIC} PcreLib{$ENDIF};{$ENDIF}
function pcre_get_stringnumber(code: ppcre; name: PChar): integer;{$IFDEF PCRE_IMPORT} external {$IFDEF LINK_DYNAMIC} PcreLib{$ENDIF};{$ENDIF}
function pcre_get_stringtable_entries(code: ppcre; name: PChar; first, last: PPChar): integer;{$IFDEF PCRE_IMPORT} external {$IFDEF LINK_DYNAMIC} PcreLib{$ENDIF};{$ENDIF}
function pcre_get_substring(subject: PChar; ovector: PInteger; stringcount, stringnumber: integer; stringptr: PPChar): integer;{$IFDEF PCRE_IMPORT} external {$IFDEF LINK_DYNAMIC} PcreLib{$ENDIF};{$ENDIF}
function pcre_get_substring_list(subject: PChar; ovector: PInteger; stringcount: integer; listptr: PPPChar): integer;{$IFDEF PCRE_IMPORT} external {$IFDEF LINK_DYNAMIC} PcreLib{$ENDIF};{$ENDIF}
procedure pcre_free_substring(stringptr: PChar);{$IFDEF PCRE_IMPORT} external {$IFDEF LINK_DYNAMIC} PcreLib{$ENDIF};{$ENDIF}
procedure pcre_free_substring_list(stringptr: PPChar);{$IFDEF PCRE_IMPORT} external {$IFDEF LINK_DYNAMIC} PcreLib{$ENDIF};{$ENDIF}

function pcre_maketables: byte;{$IFDEF PCRE_IMPORT} external {$IFDEF LINK_DYNAMIC} PcreLib{$ENDIF};{$ENDIF}

function pcre_fullinfo(code: ppcre; extra: ppcre_extra; what: integer; where: Pointer): integer;{$IFDEF PCRE_IMPORT} external {$IFDEF LINK_DYNAMIC} PcreLib{$ENDIF};{$ENDIF}
function pcre_refcount(code: ppcre; adjust: integer): integer;{$IFDEF PCRE_IMPORT} external {$IFDEF LINK_DYNAMIC} PcreLib{$ENDIF};{$ENDIF}
function pcre_config(what: integer; where: Pointer): integer;{$IFDEF PCRE_IMPORT} external {$IFDEF LINK_DYNAMIC} PcreLib{$ENDIF};{$ENDIF}
function pcre_version: PChar;{$IFDEF PCRE_IMPORT} external {$IFDEF LINK_DYNAMIC} PcreLib{$ENDIF};{$ENDIF}
