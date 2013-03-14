(***************************************************************************
                          libini.bas -  Pascal access functions
                             -------------------
    begin                : Fri Apr 21 2000
    copyright            : (C) 2000 by Simon White
    email                : s_a_white@email.com
 ***************************************************************************

 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************)

unit LibIni;

{$CALLING cdecl}

{$IFDEF LINUX} (* Kylix *)
{$DEFINE UNIX}
{$ENDIF}

{$linklib libini.a}
{$IFDEF UNIX}
{$linklib c}
{$ELSE}
{$IFDEF MSWINDOWS}
{$linklib msvcrt}
{$ELSE}
{$WARNING You may need to link to a C library to get this to work}
{$ENDIF}
{$ENDIF}

interface

type
  (* You may want to change these type definitions if your compiler lacks
     support for these fairly ubiquitous Borland extensions. *)
  bool = ByteBool;
  char_p = PChar;
  ini_fd_t = Pointer;
  long = longint;
  u_long = cardinal;

function ini_open(name, mode, comment: char_p): ini_fd_t; external name 'ini_open';
function ini_close(ini_fd: ini_fd_t): integer; external name 'ini_close';
function ini_flush(ini_fd: ini_fd_t): integer; external name 'ini_flush';
function ini_delete(ini_fd: ini_fd_t): integer; external name 'ini_delete';

function ini_locateKey(ini_fd: ini_fd_t; key: char_p): integer; external name 'ini_locateKey';
function ini_locateHeading(ini_fd: ini_fd_t; header: char_p): integer; external name 'ini_locateHeading';
function ini_deleteKey(ini_fd: ini_fd_t): integer; external name 'ini_deleteKey';
function ini_deleteHeading(ini_fd: ini_fd_t): integer; external name 'ini_deleteHeading';

function ini_currentKey(ini_fd: ini_fd_t): char_p; external name 'ini_currentKey';
function ini_currentHeading(ini_fd: ini_fd_t): char_p; external name 'ini_currentHeading';

function ini_dataLength(ini_fd: ini_fd_t): integer; external name 'ini_dataLength';

function ini_readString(ini_fd: ini_fd_t; value: char_p; size: integer): integer; external name 'ini_readString';
function ini_writeString(ini_fd: ini_fd_t; value: char_p): integer; external name 'ini_writeString';
function ini_readInt(ini_fd: ini_fd_t; var value: integer): integer; external name 'ini_readInt';

{$IFDEF INI_ADD_EXTRAS}
function ini_readLong(ini_fd: ini_fd_t; var value: long): integer; external name 'ini_reradLong';
function ini_readDouble(ini_fd: ini_fd_t; var value: real): integer; external name 'ini_readDouble';
function ini_readBool(ini_fd: ini_fd_t; var value: bool): integer; external name 'ini_readBool';

function ini_writeInt(ini_fd: ini_fd_t; value: integer): integer; external name 'ini_writeInt';
function ini_writeLong(ini_fd: ini_fd_t; value: long): integer; external name 'ini_writeLong';
function ini_writeDouble(ini_fd: ini_fd_t; value: real): integer; external name 'ini_writeDouble';
function ini_writeBool(ini_fd: ini_fd_t; value: bool): integer; external name 'ini_writeBool';

function ini_append(dest_fd, src_fd: ini_fd_t): integer; external name 'ini_append';
{$ENDIF} 

{$IFDEF INI_ADD_LIST_SUPPORT}
function ini_listLength(ini_fd: ini_fd_t): integer; external name 'ini_listLength';
function ini_listDelims(ini_fd: ini_fd_t; delims: char_p): integer; external name 'ini_listDelims';
function ini_listIndex(ini_fd: ini_fd_t; index: u_long): integer; external name 'ini_listIndex';
{$ENDIF}

implementation

end.
