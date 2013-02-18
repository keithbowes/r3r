{$calling cdecl}

{$IFDEF LINK_DYNAMIC}
{$IFNDEF __GPC__}
const
  CurlLib = 'curl';
{$ELSE __GPC__}
{$IFNDEF CurlLib}
{$DEFINE CurlLib 'curl'}
{$ENDIF CurlLib}
{$ENDIF __GPC__}
{$ELSE LINK_DYNAMIC}
{$linklib libcurl.a}
{$IFDEF UNIX}
{$linklib c}
{$ELSE UNIX}
{$IFDEF MSWINDOWS}
{$linklib msvcrt}
{$ENDIF MSWINDOWS}
{$ENDIF UNIX}
{$IFDEF USE_GCC}
{$linklib libgcc.a}
{$ENDIF USE_GCC}
{$ENDIF LINK_DYNAMIC}
