{$IFNDEF HISTORY_INCLUDE}
unit History;

{$calling cdecl}

interface

{$INCLUDE "rltypedefs.inc"}
{$ENDIF}

  Type
	time_t = cardinal;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

  type
    histdata_t = PChar;
    _hist_entry = record
        line : PChar;
        timestamp : PChar;
        data : histdata_t;
      end;
    HIST_ENTRY = _hist_entry;
		PHIST_ENTRY  = ^HIST_ENTRY;
		PPHIST_ENTRY = ^PHIST_ENTRY;

  function HISTENT_BYTES(hs : PHIST_ENTRY) : longint;

  { A structure used to pass the current state of the history stuff around.  }
  { Pointer to the entries themselves.  }
  { The location pointer within this array.  }
  { Number of elements within this array.  }
  { Number of slots allocated to this array.  }

	type
    _hist_state = record
        entries: PPHIST_ENTRY;
        offset : longint;
        length : longint;
        size : longint;
        flags : longint;
      end;
    HISTORY_STATE = _hist_state;
		PHISTORY_STATE  = ^HISTORY_STATE;
  { Flag values for the `flags' member of HISTORY_STATE.  }

  const
    HS_STIFLED = $01;
  { Initialization and state management.  }
  { Begin a session in which the history functions might be used.  This
     just initializes the interactive variables.  }

  procedure using_history;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'using_history';

  { Return the current HISTORY_STATE of the history.  }
  function history_get_history_state: PHISTORY_STATE;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'history_get_history_state';

  { Set the state of the current history array to STATE.  }
  procedure history_set_history_state(_para1:PHISTORY_STATE);external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'history_set_history_state';

  { Manage the history list.  }
  { Place STRING at the end of the history list.
     The associated data field (if any) is set to NULL.  }
  procedure add_history(_para1:Pchar);external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'add_history';

  { Change the timestamp associated with the most recent history entry to
     STRING.  }
  procedure add_history_time(_para1:Pchar);external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'add_history_time';

  { A reasonably useless function, only here for completeness.  WHICH
     is the magic number that tells us which element to delete.  The
     elements are numbered from 0.  }
  function remove_history(_para1:longint): PHIST_ENTRY;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'remove_history';

  { Free the history entry H and return any application-specific data
     associated with it.  }
  function free_history_entry(_para1:PHIST_ENTRY):histdata_t;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'free_history_entry';

  { Make the history entry at WHICH have LINE and DATA.  This returns
     the old entry so you can dispose of the data.  In the case of an
     invalid WHICH, a NULL pointer is returned.  }
  function replace_history_entry(_para1:longint; _para2:Pchar; _para3:histdata_t): PHIST_ENTRY;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'replace_history_entry';

  { Clear the history list and start over.  }
  procedure clear_history;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'clear_history';

  { Stifle the history list, remembering only MAX number of entries.  }
  procedure stifle_history(_para1:longint);external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'stifle_history';

  { Stop stifling the history.  This returns the previous amount the
     history was stifled by.  The value is positive if the history was
     stifled, negative if it wasn't.  }
  function unstifle_history:longint;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'unstifle_history';

  { Return 1 if the history is stifled, 0 if it is not.  }
  function history_is_stifled:longint;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'history_is_stifled';

  { Information about the history list.  }
  { Return a NULL terminated array of HIST_ENTRY which is the current input
     history.  Element 0 of this list is the beginning of time.  If there
     is no history, return NULL.  }
  function history_list: PPHIST_ENTRY;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'history_list';

  { Returns the number which says what history element we are now
     looking at.   }
  function where_history:longint;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'where_history';

  { Return the history entry at the current position, as determined by
     history_offset.  If there is no entry there, return a NULL pointer.  }
  function current_history: PHIST_ENTRY;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'current_history';

  { Return the history entry which is logically at OFFSET in the history
     array.  OFFSET is relative to history_base.  }
  function history_get(_para1:longint): PHIST_ENTRY;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'history_get';

  { Return the timestamp associated with the PHIST_ENTRY passed as an
     argument  }
  function history_get_time(_para1:PHIST_ENTRY):time_t;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'history_get_time';

  { Return the number of bytes that the primary history entries are using.
     This just adds up the lengths of the_history->lines.  }
  function history_total_bytes:longint;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'history_total_bytes';

  { Moving around the history list.  }
  { Set the position in the history list to POS.  }
  function history_set_pos(_para1:longint):longint;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'history_set_pos';

  { Back up history_offset to the previous history entry, and return
     a pointer to that entry.  If there is no previous entry, return
     a NULL pointer.  }
  function previous_history: PHIST_ENTRY;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'previous_history';

  { Move history_offset forward to the next item in the input_history,
     and return the a pointer to that entry.  If there is no next entry,
     return a NULL pointer.  }
  function next_history: PHIST_ENTRY;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'next_history';

  { Searching the history list.  }
  { Search the history for STRING, starting at history_offset.
     If DIRECTION < 0, then the search is through previous entries,
     else through subsequent.  If the string is found, then
     current_history () is the history entry, and the value of this function
     is the offset in the line of that history entry that the string was
     found in.  Otherwise, nothing is changed, and a -1 is returned.  }
  function history_search(_para1:Pchar; _para2:longint):longint;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'history_search';

  { Search the history for STRING, starting at history_offset.
     The search is anchored: matching lines must begin with string.
     DIRECTION is as in history_search().  }
  function history_search_prefix(_para1:Pchar; _para2:longint):longint;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'history_search_prefix';

  { Search for STRING in the history list, starting at POS, an
     absolute index into the list.  DIR, if negative, says to search
     backwards from POS, else forwards.
     Returns the absolute index of the history element where STRING
     was found, or -1 otherwise.  }
  function history_search_pos(_para1:Pchar; _para2:longint; _para3:longint):longint;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'history_search_pos';

  { Managing the history file.  }
  { Add the contents of FILENAME to the history list, a line at a time.
     If FILENAME is NULL, then read from ~/.history.  Returns 0 if
     successful, or errno if not.  }
  function read_history(_para1:Pchar):longint;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'read_history';

  { Read a range of lines from FILENAME, adding them to the history list.
     Start reading at the FROM'th line and end at the TO'th.  If FROM
     is zero, start at the beginning.  If TO is less than FROM, read
     until the end of the file.  If FILENAME is NULL, then read from
     ~/.history.  Returns 0 if successful, or errno if not.  }
  function read_history_range(_para1:Pchar; _para2:longint; _para3:longint):longint;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'read_history_range';

  { Write the current history to FILENAME.  If FILENAME is NULL,
     then write the history list to ~/.history.  Values returned
     are as in read_history ().   }
  function write_history(_para1:Pchar):longint;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'write_history';

  { Append NELEMENT entries to FILENAME.  The entries appended are from
     the end of the list minus NELEMENTs up to the end of the list.  }
  function append_history(_para1:longint; _para2:Pchar):longint;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'append_history';

  { Truncate the history file, leaving only the last NLINES lines.  }
  function history_truncate_file(_para1:Pchar; _para2:longint):longint;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'history_truncate_file';

  { History expansion.  }
  { Expand the string STRING, placing the result into OUTPUT, a pointer
     to a string.  Returns:

     0) If no expansions took place (or, if the only change in
        the text was the de-slashifying of the history expansion
        character)
     1) If expansions did take place
    -1) If there was an error in expansion.
     2) If the returned line should just be printed.

    If an error ocurred in expansion, then OUTPUT contains a descriptive
    error message.  }
  function history_expand(_para1:Pchar; _para2:PPchar):longint;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'history_expand';

  { Extract a string segment consisting of the FIRST through LAST
     arguments present in STRING.  Arguments are broken up as in
     the shell.  }
  function history_arg_extract(_para1:longint; _para2:longint; _para3:Pchar):PChar;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'history_arg_extract';

  { Return the text of the history event beginning at the current
     offset into STRING.  Pass STRING with PINDEX equal to the
     history_expansion_char that begins this specification.
     DELIMITING_QUOTE is a character that is allowed to end the string
     specification for what to search for in addition to the normal
     characters `:', ` ', `\t', `\n', and sometimes `?'.  }
  function get_history_event(_para1:Pchar; _para2:Plongint; _para3:longint):PChar;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'get_history_event';

  { Return an array of tokens, much as the shell might.  The tokens are
     parsed out of STRING.  }
  function history_tokenize(_para1:Pchar): PPChar;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF} name 'history_tokenize';

  { Exported history variables.  }
    var
      history_base : longint;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF};
      history_length : longint;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF};
      history_max_entries : longint;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF};
      history_expansion_char : char;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF};
      history_subst_char : char;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF};
      history_word_delimiters : PChar;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF};
      history_comment_char : char;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF};
      history_no_expand_chars : PChar;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF};
      history_search_delimiter_chars : PChar;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF};
      history_quotes_inhibit_expansion : longint;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF};
      history_write_timestamps : longint;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF};
      max_input_history : longint;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF};
  { If set, this function is called to decide whether or not a particular
     history expansion should be treated as a special case for the calling
     application and not expanded.  }
      history_inhibit_expansion_function : rl_linebuf_func_t;external {$IFDEF HISTORY_INCLUDE}'readline'{$ELSE}'history'{$ENDIF};

{$IFNDEF HISTORY_INCLUDE}
implementation
{$INCLUDE "historyimpl.inc"}

end.
{$ENDIF}
