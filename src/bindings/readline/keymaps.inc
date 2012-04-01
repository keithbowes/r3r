{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

  { keymaps.h -- Manipulation of readline keymaps.  }

  { A keymap contains one entry for each key in the ASCII set.
     Each entry consists of a type and a pointer.
     FUNCTION is the address of a function to run, or the
     address of a keymap to indirect through.
     TYPE says which kind of thing FUNCTION is.  }

  type
    _keymap_entry = record
        _type : char;
        _function : rl_command_func_t;
      end;
    KEYMAP_ENTRY = _keymap_entry;

  { This must be large enough to hold bindings for all of the characters
     in a desired character set (e.g, 128 for ASCII, 256 for ISO Latin-x,
     and so on) plus one for subsequence matching.  }
  const
    KEYMAP_SIZE = 257;    
		ANYOTHERKEY = KEYMAP_SIZE - 1;

    type
      KEYMAP_ENTRY_ARRAY = array[0..(KEYMAP_SIZE)-1] of KEYMAP_ENTRY;

      Keymap = ^KEYMAP_ENTRY;

    { The values that TYPE can have in a keymap entry.  }
    const
      ISFUNC = 0;      
      ISKMAP = 1;      
      ISMACR = 2;      

      var
        emacs_standard_keymap : KEYMAP_ENTRY_ARRAY;external {$IFNDEF USE_LIBEDIT}'readline'{$ELSE}'edit'{$ENDIF};
        vi_insertion_keymap : KEYMAP_ENTRY_ARRAY;external {$IFNDEF USE_LIBEDIT}'readline'{$ELSE}'edit'{$ENDIF};

    { Return a new, empty keymap.
       Free it with rl_free() when you are done.  }
    function rl_make_bare_keymap:Keymap;external {$IFNDEF USE_LIBEDIT}'readline'{$ELSE}'edit'{$ENDIF} name 'rl_make_bare_keymap';

    { Return a new keymap which is a copy of MAP.  }
    function rl_copy_keymap(_para1:Keymap):Keymap;external {$IFNDEF USE_LIBEDIT}'readline'{$ELSE}'edit'{$ENDIF} name 'rl_copy_keymap';

    { Return a new keymap with the printing characters bound to rl_insert,
       the lowercase Meta characters bound to run their equivalents, and
       the Meta digits bound to produce numeric arguments.  }
    function rl_make_keymap:Keymap;external {$IFNDEF USE_LIBEDIT}'readline'{$ELSE}'edit'{$ENDIF} name 'rl_make_keymap';

    { Free the storage associated with a keymap.  }
    procedure rl_discard_keymap(_para1:Keymap);external {$IFNDEF USE_LIBEDIT}'readline'{$ELSE}'edit'{$ENDIF} name 'rl_discard_keymap';

    { These functions actually appear in bind.c  }
    { Return the keymap corresponding to a given name.  Names look like
       `emacs' or `emacs-meta' or `vi-insert'.   }
    function rl_get_keymap_by_name(_para1:Pchar):Keymap;external {$IFNDEF USE_LIBEDIT}'readline'{$ELSE}'edit'{$ENDIF} name 'rl_get_keymap_by_name';

    { Return the current keymap.  }
    function rl_get_keymap:Keymap;external {$IFNDEF USE_LIBEDIT}'readline'{$ELSE}'edit'{$ENDIF} name 'rl_get_keymap';

    { Set the current keymap to MAP.  }
    procedure rl_set_keymap(_para1:Keymap);external {$IFNDEF USE_LIBEDIT}'readline'{$ELSE}'edit'{$ENDIF} name 'rl_set_keymap';


