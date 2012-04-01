  { tilde.h: Externally available variables and function in libtilde.a.  }

  { A function can be defined using prototypes and compile on both ANSI C
     and traditional C compilers with something like this:
  	extern char *func(char *, char *, int);  }
  { If non-null, this contains the address of a function that the application
     wants called before trying the standard tilde expansions.  The function
     is called with the text sans tilde, and returns a malloc()'ed string
     which is the expansion, or a NULL pointer if the expansion fails.  }
	type
		tilde_hook_func_t = function(hook: PChar): PChar;

    var
      tilde_expansion_preexpansion_hook : tilde_hook_func_t;external {$IFNDEF USE_LIBEDIT}'readline'{$ELSE}'edit'{$ENDIF};
  { If non-null, this contains the address of a function to call if the
     standard meaning for expanding a tilde fails.  The function is called
     with the text (sans tilde, as in "foo"), and returns a malloc()'ed string
     which is the expansion, or a NULL pointer if there is no expansion.  }
      tilde_expansion_failure_hook : tilde_hook_func_t;external {$IFNDEF USE_LIBEDIT}'readline'{$ELSE}'edit'{$ENDIF};
  { When non-null, this is a NULL terminated array of strings which
     are duplicates for a tilde prefix.  Bash uses this to expand
     `=~' and `:~'.  }
      tilde_additional_prefixes : PPChar;external {$IFNDEF USE_LIBEDIT}'readline'{$ELSE}'edit'{$ENDIF};
  { When non-null, this is a NULL terminated array of strings which match
     the end of a username, instead of just "/".  Bash sets this to
     `:' and `=~'.  }
      tilde_additional_suffixes : PPChar;external {$IFNDEF USE_LIBEDIT}'readline'{$ELSE}'edit'{$ENDIF};
  { Return a new string which is the result of tilde expanding STRING.  }

  function tilde_expand(_para1:Pchar):PChar;external {$IFNDEF USE_LIBEDIT}'readline'{$ELSE}'edit'{$ENDIF} name 'tilde_expand';

  { Do the work of tilde expansion on FILENAME.  FILENAME starts with a
     tilde.  If there is no expansion, call tilde_expansion_failure_hook.  }
  function tilde_expand_word(_para1:Pchar):PChar;external {$IFNDEF USE_LIBEDIT}'readline'{$ELSE}'edit'{$ENDIF} name 'tilde_expand_word';

  { Find the portion of the string beginning with ~ that should be expanded.  }
  function tilde_find_word(_para1:Pchar; _para2:longint; _para3:Plongint):PChar;external {$IFNDEF USE_LIBEDIT}'readline'{$ELSE}'edit'{$ENDIF} name 'tilde_find_word';
