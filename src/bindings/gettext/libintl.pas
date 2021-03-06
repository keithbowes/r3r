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

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

{$IFNDEF __GPC__}
const
  IntlLib = 'intl';
{$ELSE}
{$IFNDEF IntlLib}
{$DEFINE IntlLib 'intl'}
{$ENDIF}
{$ENDIF}

{$IFNDEF __GPC__}
{$IFDEF UNIX}
{$linklib c}
{$ELSE}
{$IFDEF MSWINDOWS}
{$linklib msvcrt}
{$ELSE}
{$WARNING You may need to link to a C library to get this to work}
{$ENDIF}
{$ENDIF}
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

{$define __USE_GNU_GETTEXT}
  { Provide information about the supported file formats.  Returns the
     maximum minor revision number supported for a given major revision.   }
  function __GNU_GETTEXT_SUPPORTED_REVISION(major : longint) : Boolean;

  { Look up MSGID in the current default message catalog for the current
     LC_MESSAGES locale.  If not found, returns MSGID itself (the default
     text).   }
  function gettext(__msgid:Pchar):Pchar;external IntlLib;

  { Look up MSGID in the DOMAINNAME message catalog for the current
     LC_MESSAGES locale.   }
  function dgettext(__domainname:Pchar; __msgid:Pchar):Pchar;external IntlLib;

  function __dgettext(__domainname:Pchar; __msgid:Pchar):Pchar;external IntlLib;

  { Look up MSGID in the DOMAINNAME message catalog for the current CATEGORY
     locale.   }
  function dcgettext(__domainname:Pchar; __msgid:Pchar; __category:longint):Pchar;external IntlLib;

  function __dcgettext(__domainname:Pchar; __msgid:Pchar; __category:longint):Pchar;external IntlLib;

  { Similar to `gettext' but select the plural form corresponding to the
     number N.   }
  function ngettext(__msgid1:Pchar; __msgid2:Pchar; __n:cardinal):Pchar;external IntlLib;

  { Similar to `dgettext' but select the plural form corresponding to the
     number N.   }
  function dngettext(__domainname:Pchar; __msgid1:Pchar; __msgid2:Pchar; __n:cardinal):Pchar;external IntlLib;

  { Similar to `dcgettext' but select the plural form corresponding to the
     number N.   }
  function dcngettext(__domainname:Pchar; __msgid1:Pchar; __msgid2:Pchar; __n:cardinal; __category:longint):Pchar;external IntlLib;

  { Set the current default message catalog to DOMAINNAME.
     If DOMAINNAME is null, return the current default.
     If DOMAINNAME is "", reset to the default of "messages".   }
  function textdomain(__domainname:Pchar):Pchar;external IntlLib;

  { Specify that the DOMAINNAME message catalog will be found
     in DIRNAME rather than in the system locale data base.   }
  function bindtextdomain(__domainname:Pchar; __dirname:Pchar):Pchar;external IntlLib;

  { Specify the character encoding in which the messages from the
     DOMAINNAME message catalog will be returned.   }
  function bind_textdomain_codeset(__domainname:Pchar; __codeset:Pchar):Pchar;external IntlLib;

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

function setlocale(category: LongInt; locale: PChar): PChar; external name 'setlocale';

function _(msgid: PChar): String;

implementation

function __GNU_GETTEXT_SUPPORTED_REVISION(major : longint) : Boolean;
begin
  __GNU_GETTEXT_SUPPORTED_REVISION := major = 0;
end;

function _(msgid: PChar): String;
var
  Res: String;
begin
  WriteStr(Res, gettext(msgid));
  _ := Res;
end;

end.
