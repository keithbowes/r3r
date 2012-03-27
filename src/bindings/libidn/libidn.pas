
unit LibIdn;

{$CALLING cdecl}

interface

{
  Automatically converted by H2Pas 1.0.0 from idna.h
  The following command line parameters were used:
    -d
    -c
    -l
    idn
    -o
    libidn.pas
    -p
    -u
    LibIdn
    idna.h
}

{ Pointers to basic pascal types, inserted by h2pas conversion program.}
Type
  PLongint  = ^Longint;
  PSmallInt = ^SmallInt;
  PByte     = ^Byte;
  PWord     = ^Word;
  PDWord    = ^cardinal;
  PDouble   = ^Double;
  PPChar    = ^PChar;

Type
size_t = cardinal;
uint32_t = word;
Psize_t  = ^size_t;
Puint32_t  = ^uint32_t;
PPuint32_t = ^Puint32_t;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


{ idna.h --- Declarations for Internationalized Domain Name in Applications.
 * Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009  Simon Josefsson
 *
 * This file is part of GNU Libidn.
 *
 * GNU Libidn is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * GNU Libidn is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with GNU Libidn; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
 *
  }
{ Error codes.  }
{ Workaround typo in earlier versions.  }
{ Internal errors.  }
const
  IDNA_SUCCESS = 0;
  IDNA_STRINGPREP_ERROR = 1;
  IDNA_PUNYCODE_ERROR = 2;
  IDNA_CONTAINS_NON_LDH = 3;
  IDNA_CONTAINS_LDH = IDNA_CONTAINS_NON_LDH;
  IDNA_CONTAINS_MINUS = 4;
  IDNA_INVALID_LENGTH = 5;
  IDNA_NO_ACE_PREFIX = 6;
  IDNA_ROUNDTRIP_VERIFY_ERROR = 7;
  IDNA_CONTAINS_ACE_PREFIX = 8;
  IDNA_ICONV_ERROR = 9;
  IDNA_MALLOC_ERROR = 201;
  IDNA_DLOPEN_ERROR = 202;
{ IDNA flags  }

   IDNA_ALLOW_UNASSIGNED = $0001;
   IDNA_USE_STD3_ASCII_RULES = $0002;
{$ifndef IDNA_ACE_PREFIX}
{$define IDNA_ACE_PREFIX xn--}
{$endif}

function idna_strerror(rc:longint):Pchar;external 'idn';
{ Core functions  }
function idna_to_ascii_4i(input:Puint32_t; inlen:size_t; output:Pchar; flags:longint):longint;external 'idn';
function idna_to_unicode_44i(input:Puint32_t; inlen:size_t; output:Puint32_t; outlen:Psize_t; flags:longint):longint;external 'idn';
{ Wrappers that handle several labels  }
function idna_to_ascii_4z(input:Puint32_t; output:PPchar; flags:longint):longint;external 'idn';
function idna_to_ascii_8z(input:Pchar; output:PPchar; flags:longint):longint;external 'idn';
function idna_to_ascii_lz(input:Pchar; output:PPchar; flags:longint):longint;external 'idn';
function idna_to_unicode_4z4z(input:Puint32_t; output:PPuint32_t; flags:longint):longint;external 'idn';
function idna_to_unicode_8z4z(input:Pchar; output:PPuint32_t; flags:longint):longint;external 'idn';
function idna_to_unicode_8z8z(input:Pchar; output:PPchar; flags:longint):longint;external 'idn';
function idna_to_unicode_8zlz(input:Pchar; output:PPchar; flags:longint):longint;external 'idn';
function idna_to_unicode_lzlz(input:Pchar; output:PPchar; flags:longint):longint;external 'idn';

implementation

end.
