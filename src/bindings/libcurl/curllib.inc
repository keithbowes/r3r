{$calling cdecl}

{$IFNDEF __GPC__}
const
  CurlLib = 'curl';
{$ELSE __GPC__}
{$IFNDEF CurlLib}
{$DEFINE CurlLib 'curl'}
{$ENDIF CurlLib}
{$ENDIF __GPC__}
