type
	PFILE = Pointer;
	Plongint = ^longint;
	PPChar = ^PChar;

	rl_command_func_t = function(_para1, _para2: longint): longint;

	rl_compentry_func_t = function(_para1: PChar; _para2: longint): PChar;
	rl_completion_func_t = function(_para1: PChar; _para2, _para3: longint): PChar;

	rl_quote_func_t = function(_para1: PChar; _para2: longint; _para3: PChar): PChar;
	rl_dequote_func_t = function(_para1: PChar; _para2: longint): PChar;

	rl_compignore_func_t = function(_para1: PPChar): longint;

	rl_compdisp_func_t = procedure(_para1: PPChar; _para2, _para3: longint);

	rl_hook_func_t = function: longint;
	rl_ivoidfunc_t = rl_hook_func_t;
	rl_getc_func_t = function(_para1: PFILE): longint;

	rl_linebuf_func_t = function(_para1: PChar; _para2: longint): longint;

	rl_intfunc_t = function(_para1: longint): longint;
	rl_icpfunc_t = function(_para1: PChar): longint;
	rl_icppfunc_t = function (_para1: PPChar): longint;

	rl_voidfunc_t = procedure;
	rl_vintfunc_t = procedure(_para1: longint);
	rl_vcpfunc_t = procedure(_para1: PChar);
	rl_vcppfunc_t = procedure(_para1: PPChar);

	rl_cpvfunc_t = function: PChar;
	rl_cpifunc_t = function(_para1: longint): PChar;
	rl_cpcpfunc_t = function(_para1: PChar): PChar;
	rl_cpcppfunc_t = function(_para1: PPChar): PChar;

{ GPC doesn't allow functions to return a procedural type,
  so we'll make it a generic pointer.  For best results,
  cast the return value of those functions to the proper
  procedural type in all compilers. }
{$IFNDEF __GPC__}
  prl_command_func_t = rl_command_func_t;
  prl_icppfunc_t = rl_icppfunc_t;
{$ELSE}
  prl_command_func_t = Pointer;
  prl_icppfunc_t = Pointer;
{$ENDIF}

{ Let's setup the libraries}
{$IFDEF USE_LIBEDIT}
{$IFNDEF __GPC__}
const
	RlLib = 'edit';
	HistLib = RlLib;
{$ELSE}
{$IFNDEF RlLib}
{$DEFINE RlLib edit}
{$ENDIF}
{$ENDIF}
{$ELSE}
{$IFNDEF __GPC__}
const
{$IFDEF MSWINDOWS}
	RlLib = 'readline5';
{$IFDEF HISTORY_INCLUDE}
	HistLib = 'history5';
{$ELSE}
	HistLib = RlLib;
{$ENDIF}
{$ELSE}
	RlLib = 'readline';
{$IFDEF HISTORY_INCLUDE}
	HistLib = 'history';
{$ELSE}
	HistLib= RlLib;
{$ENDIF}
{$ENDIF}
{$ELSE}
{$IFNDEF RlLib}
{$DEFINE RlLib 'readline'}
{$DEFINE HistLib RlLib}
{$ENDIF}
{$ENDIF}
{$ENDIF}
