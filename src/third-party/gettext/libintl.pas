unit LibIntl;
interface

{$CALLING cdecl}
{$MODE delphi}

{
  Automatically converted by H2Pas 1.0.0 from libintl.h
  The following command line parameters were used:
    -d
    -l
    intl
    -o
    libintl.pas
    -p
    -u
    LibIntl
    libintl.h
}

  { Pointers to basic pascal types, inserted by h2pas conversion program.}
  Type
    DWord     = Cardinal; // GPC doesn't have DWord defined
    PLongint  = ^Longint;
    PSmallInt = ^SmallInt;
    PByte     = ^Byte;
    PWord     = ^Word;
    PDWord    = ^DWord;
    PDouble   = ^Double;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


  { Message catalogs for internationalization.
     Copyright (C) 1995-2002, 2004, 2005 Free Software Foundation, Inc.
     This file is part of the GNU C Library.
     This file is derived from the file libgettext.h in the GNU gettext package.
  
     The GNU C Library is free software; you can redistribute it and/or
     modify it under the terms of the GNU Lesser General Public
     License as published by the Free Software Foundation; either
     version 2.1 of the License, or (at your option) any later version.
  
     The GNU C Library is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
     Lesser General Public License for more details.
  
     You should have received a copy of the GNU Lesser General Public
     License along with the GNU C Library; if not, write to the Free
     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
     02111-1307 USA.   }
{$ifndef _LIBINTL_H}

  const
     _LIBINTL_H = 1;     
  { We define an additional symbol to signal that we use the GNU
     implementation of gettext.   }

  const
     __USE_GNU_GETTEXT = 1;     
  { Provide information about the supported file formats.  Returns the
     maximum minor revision number supported for a given major revision.   }
  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function __GNU_GETTEXT_SUPPORTED_REVISION(major : longint) : longint;  

  { Look up MSGID in the current default message catalog for the current
     LC_MESSAGES locale.  If not found, returns MSGID itself (the default
     text).   }
(* Const before type ignored *)
  function gettext(__msgid:Pchar):Pchar;external 'intl';

  { Look up MSGID in the DOMAINNAME message catalog for the current
     LC_MESSAGES locale.   }
(* Const before type ignored *)
(* Const before type ignored *)
  function dgettext(__domainname:Pchar; __msgid:Pchar):Pchar;external 'intl';

(* Const before type ignored *)
(* Const before type ignored *)
  function __dgettext(__domainname:Pchar; __msgid:Pchar):Pchar;external 'intl';

  { Look up MSGID in the DOMAINNAME message catalog for the current CATEGORY
     locale.   }
(* Const before type ignored *)
(* Const before type ignored *)
  function dcgettext(__domainname:Pchar; __msgid:Pchar; __category:longint):Pchar;external 'intl';

(* Const before type ignored *)
(* Const before type ignored *)
  function __dcgettext(__domainname:Pchar; __msgid:Pchar; __category:longint):Pchar;external 'intl';

  { Similar to `gettext' but select the plural form corresponding to the
     number N.   }
(* Const before type ignored *)
(* Const before type ignored *)
  function ngettext(__msgid1:Pchar; __msgid2:Pchar; __n:dword):Pchar;external 'intl';

  { Similar to `dgettext' but select the plural form corresponding to the
     number N.   }
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  function dngettext(__domainname:Pchar; __msgid1:Pchar; __msgid2:Pchar; __n:dword):Pchar;external 'intl';

  { Similar to `dcgettext' but select the plural form corresponding to the
     number N.   }
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  function dcngettext(__domainname:Pchar; __msgid1:Pchar; __msgid2:Pchar; __n:dword; __category:longint):Pchar;external 'intl';

  { Set the current default message catalog to DOMAINNAME.
     If DOMAINNAME is null, return the current default.
     If DOMAINNAME is "", reset to the default of "messages".   }
(* Const before type ignored *)
  function textdomain(__domainname:Pchar):Pchar;external 'intl';

  { Specify that the DOMAINNAME message catalog will be found
     in DIRNAME rather than in the system locale data base.   }
(* Const before type ignored *)
(* Const before type ignored *)
  function bindtextdomain(__domainname:Pchar; __dirname:Pchar):Pchar;external 'intl';

  { Specify the character encoding in which the messages from the
     DOMAINNAME message catalog will be returned.   }
(* Const before type ignored *)
(* Const before type ignored *)
  function bind_textdomain_codeset(__domainname:Pchar; __codeset:Pchar):Pchar;external 'intl';

{ Additions }
const
  LC_CTYPE          = 0;
  LC_NUMERIC        = 1;
  LC_TIME           = 2;
  LC_COLLATE        = 3;
  LC_MONETARY       = 4;
  LC_MESSAGES       = 5;
  LC_ALL            = 6;
  LC_PAPER          = 7;
  LC_NAME           = 8;
  LC_ADDRESS        = 9;
  LC_TELEPHONE      = 10;
  LC_MEASUREMENT    = 11;
  LC_IDENTIFICATION = 12;

function setlocale(category: LongInt; locale: PChar): PChar; external {$ifdef WIN32}'msvcrt'{$else}'intl'{$endif};

function _(msgid: PChar): String;

{$endif}

implementation

uses
  Strings;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }
  function __GNU_GETTEXT_SUPPORTED_REVISION(major : longint) : longint;
    var
       if_local1 : longint;
    (* result types are not known *)
    begin
       if major = 0 then
         if_local1:=1
       else
         if_local1:=-(1);
       __GNU_GETTEXT_SUPPORTED_REVISION:=if_local1;
    end;

function _(msgid: PChar): String;
begin
  _ := StrPas(gettext(msgid));
end;

end.
