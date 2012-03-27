unit Readline;
interface

{$calling cdecl}
{$mode objfpc}

{$IFDEF __GPC__}
uses
  Strings;
{$ENDIF}

{
  Automatically converted by H2Pas 1.0.0 from readline.h
  The following command line parameters were used:
    -D
    -l
    readline
    -o
    readline.pas
    -u
    Readline
    readline.h
}
{$include "rltypedefs.inc"}

{$include "keymaps.inc"}
{$include "tilde.inc"}

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

{$DEFINE HISTORY_INCLUDE} { History is available as an include,
                            rather than a separate unit. }
{$INCLUDE "history.pas"}


  { Readline.h -- the names of functions callable from within readline.  }
  { Copyright (C) 1987-2011 Free Software Foundation, Inc.

     This file is part of the GNU Readline Library (Readline), a library
     for reading lines of text with interactive input and history editing.

     Readline is free software: you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published by
     the Free Software Foundation, either version 3 of the License, or
     (at your option) any later version.

     Readline is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU General Public License for more details.

     You should have received a copy of the GNU General Public License
     along with Readline.  If not, see <http://www.gnu.org/licenses/>.
   }
  { Readline data structures.  }
  { Maintaining the state of undo.  We remember individual deletes and inserts
     on a chain of things to do.  }
  { The actions that undo knows how to undo.  Notice that UNDO_DELETE means
     to insert some text, and UNDO_INSERT means to delete some text.   I.e.,
     the code tells undo what to undo, not how to undo it.  }

  type
    undo_code = (UNDO_DELETE,UNDO_INSERT,UNDO_BEGIN,UNDO_END
      );

  { What an element of THE_UNDO_LIST looks like.  }
  { Where the change took place.  }
  { The text to insert, if undoing a delete.  }
  { Delete, Insert, Begin, End.  }

    pundo_list = ^undo_list;
    undo_list = record
        next : Pundo_list;
        start : longint;
        _end : longint;
        text : PChar;
        what : undo_code;
      end;
  { The current undo list for RL_LINE_BUFFER.  }

    var
      rl_undo_list : PUNDO_LIST;external 'readline';
  { The data structure for mapping textual names to code addresses.  }

  type
    _funmap = record
        name : PChar;
        _function : rl_command_func_t;
      end;
    TFUNMAP = _funmap;
    PFUNMAP = ^TFUNMAP;
    PPFUNMAP = ^PFUNMAP;

    var
      funmap : PPFUNMAP;external 'readline';
  { ****************************************************************  }
  {								     }
  {	     Functions available to bind to key sequences	     }
  {								     }
  { ****************************************************************  }
  { Bindable commands for numeric arguments.  }

  function rl_digit_argument(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_digit_argument';

  function rl_universal_argument(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_universal_argument';

  { Bindable commands for moving the cursor.  }
  function rl_forward_byte(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_forward_byte';

  function rl_forward_char(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_forward_char';

  function rl_forward(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_forward';

  function rl_backward_byte(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_backward_byte';

  function rl_backward_char(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_backward_char';

  function rl_backward(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_backward';

  function rl_beg_of_line(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_beg_of_line';

  function rl_end_of_line(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_end_of_line';

  function rl_forward_word(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_forward_word';

  function rl_backward_word(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_backward_word';

  function rl_refresh_line(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_refresh_line';

  function rl_clear_screen(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_clear_screen';

  function rl_skip_csi_sequence(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_skip_csi_sequence';

  function rl_arrow_keys(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_arrow_keys';

  { Bindable commands for inserting and deleting text.  }
  function rl_insert(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_insert';

  function rl_quoted_insert(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_quoted_insert';

  function rl_tab_insert(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_tab_insert';

  function rl_newline(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_newline';

  function rl_do_lowercase_version(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_do_lowercase_version';

  function rl_rubout(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_rubout';

  function rl_delete(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_delete';

  function rl_rubout_or_delete(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_rubout_or_delete';

  function rl_delete_horizontal_space(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_delete_horizontal_space';

  function rl_delete_or_show_completions(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_delete_or_show_completions';

  function rl_insert_comment(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_insert_comment';

  { Bindable commands for changing case.  }
  function rl_upcase_word(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_upcase_word';

  function rl_downcase_word(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_downcase_word';

  function rl_capitalize_word(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_capitalize_word';

  { Bindable commands for transposing characters and words.  }
  function rl_transpose_words(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_transpose_words';

  function rl_transpose_chars(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_transpose_chars';

  { Bindable commands for searching within a line.  }
  function rl_char_search(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_char_search';

  function rl_backward_char_search(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_backward_char_search';

  { Bindable commands for readline's interface to the command history.  }
  function rl_beginning_of_history(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_beginning_of_history';

  function rl_end_of_history(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_end_of_history';

  function rl_get_next_history(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_get_next_history';

  function rl_get_previous_history(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_get_previous_history';

  { Bindable commands for managing the mark and region.  }
  function rl_set_mark(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_set_mark';

  function rl_exchange_point_and_mark(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_exchange_point_and_mark';

  { Bindable commands to set the editing mode (emacs or vi).  }
  function rl_vi_editing_mode(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_editing_mode';

  function rl_emacs_editing_mode(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_emacs_editing_mode';

  { Bindable commands to change the insert mode (insert or overwrite)  }
  function rl_overwrite_mode(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_overwrite_mode';

  { Bindable commands for managing key bindings.  }
  function rl_re_read_init_file(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_re_read_init_file';

  function rl_dump_functions(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_dump_functions';

  function rl_dump_macros(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_dump_macros';

  function rl_dump_variables(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_dump_variables';

  { Bindable commands for word completion.  }
  function rl_complete(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_complete';

  function rl_possible_completions(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_possible_completions';

  function rl_insert_completions(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_insert_completions';

  function rl_old_menu_complete(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_old_menu_complete';

  function rl_menu_complete(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_menu_complete';

  function rl_backward_menu_complete(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_backward_menu_complete';

  { Bindable commands for killing and yanking text, and managing the kill ring.  }
  function rl_kill_word(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_kill_word';

  function rl_backward_kill_word(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_backward_kill_word';

  function rl_kill_line(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_kill_line';

  function rl_backward_kill_line(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_backward_kill_line';

  function rl_kill_full_line(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_kill_full_line';

  function rl_unix_word_rubout(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_unix_word_rubout';

  function rl_unix_filename_rubout(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_unix_filename_rubout';

  function rl_unix_line_discard(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_unix_line_discard';

  function rl_copy_region_to_kill(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_copy_region_to_kill';

  function rl_kill_region(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_kill_region';

  function rl_copy_forward_word(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_copy_forward_word';

  function rl_copy_backward_word(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_copy_backward_word';

  function rl_yank(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_yank';

  function rl_yank_pop(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_yank_pop';

  function rl_yank_nth_arg(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_yank_nth_arg';

  function rl_yank_last_arg(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_yank_last_arg';

  { Not available unless __CYGWIN__ is defined.  }
{$ifdef __CYGWIN__}
  function rl_paste_from_clipboard(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_paste_from_clipboard';

{$endif}
  { Bindable commands for incremental searching.  }

  function rl_reverse_search_history(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_reverse_search_history';

  function rl_forward_search_history(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_forward_search_history';

  { Bindable keyboard macro commands.  }
  function rl_start_kbd_macro(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_start_kbd_macro';

  function rl_end_kbd_macro(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_end_kbd_macro';

  function rl_call_last_kbd_macro(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_call_last_kbd_macro';

  { Bindable undo commands.  }
  function rl_revert_line(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_revert_line';

  function rl_undo_command(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_undo_command';

  { Bindable tilde expansion commands.  }
  function rl_tilde_expand(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_tilde_expand';

  { Bindable terminal control commands.  }
  function rl_restart_output(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_restart_output';

  function rl_stop_output(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_stop_output';

  { Miscellaneous bindable commands.  }
  function rl_abort(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_abort';

  function rl_tty_status(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_tty_status';

  { Bindable commands for incremental and non-incremental history searching.  }
  function rl_history_search_forward(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_history_search_forward';

  function rl_history_search_backward(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_history_search_backward';

  function rl_noninc_forward_search(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_noninc_forward_search';

  function rl_noninc_reverse_search(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_noninc_reverse_search';

  function rl_noninc_forward_search_again(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_noninc_forward_search_again';

  function rl_noninc_reverse_search_again(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_noninc_reverse_search_again';

  { Bindable command used when inserting a matching close character.  }
  function rl_insert_close(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_insert_close';

  { Not available unless READLINE_CALLBACKS is defined.  }
  procedure rl_callback_handler_install(_para1:PChar; _para2:rl_vcpfunc_t);external 'readline' name 'rl_callback_handler_install';

  procedure rl_callback_read_char;external 'readline' name 'rl_callback_read_char';

  procedure rl_callback_handler_remove;external 'readline' name 'rl_callback_handler_remove';

  { Things for vi mode. Not available unless readline is compiled -DVI_MODE.  }
  { VI-mode bindable commands.  }
  function rl_vi_redo(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_redo';

  function rl_vi_undo(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_undo';

  function rl_vi_yank_arg(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_yank_arg';

  function rl_vi_fetch_history(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_fetch_history';

  function rl_vi_search_again(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_search_again';

  function rl_vi_search(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_search';

  function rl_vi_complete(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_complete';

  function rl_vi_tilde_expand(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_tilde_expand';

  function rl_vi_prev_word(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_prev_word';

  function rl_vi_next_word(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_next_word';

  function rl_vi_end_word(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_end_word';

  function rl_vi_insert_beg(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_insert_beg';

  function rl_vi_append_mode(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_append_mode';

  function rl_vi_append_eol(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_append_eol';

  function rl_vi_eof_maybe(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_eof_maybe';

  function rl_vi_insertion_mode(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_insertion_mode';

  function rl_vi_insert_mode(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_insert_mode';

  function rl_vi_movement_mode(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_movement_mode';

  function rl_vi_arg_digit(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_arg_digit';

  function rl_vi_change_case(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_change_case';

  function rl_vi_put(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_put';

  function rl_vi_column(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_column';

  function rl_vi_delete_to(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_delete_to';

  function rl_vi_change_to(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_change_to';

  function rl_vi_yank_to(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_yank_to';

  function rl_vi_rubout(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_rubout';

  function rl_vi_delete(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_delete';

  function rl_vi_back_to_indent(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_back_to_indent';

  function rl_vi_first_print(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_first_print';

  function rl_vi_char_search(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_char_search';

  function rl_vi_match(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_match';

  function rl_vi_change_char(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_change_char';

  function rl_vi_subst(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_subst';

  function rl_vi_overstrike(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_overstrike';

  function rl_vi_overstrike_delete(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_overstrike_delete';

  function rl_vi_replace(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_replace';

  function rl_vi_set_mark(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_set_mark';

  function rl_vi_goto_mark(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_goto_mark';

  { VI-mode utility functions.  }
  function rl_vi_check:longint;external 'readline' name 'rl_vi_check';

  function rl_vi_domove(_para1:longint; _para2:Plongint):longint;external 'readline' name 'rl_vi_domove';

  function rl_vi_bracktype(_para1:longint):longint;external 'readline' name 'rl_vi_bracktype';

  procedure rl_vi_start_inserting(_para1:longint; _para2:longint; _para3:longint);external 'readline' name 'rl_vi_start_inserting';

  { VI-mode pseudo-bindable commands, used as utility functions.  }
  function rl_vi_fWord(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_fWord';

  function rl_vi_bWord(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_bWord';

  function rl_vi_eWord(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_vi_eWord';

  { ****************************************************************  }
  {								     }
  {			Well Published Functions		     }
  {								     }
  { ****************************************************************  }
  { Readline functions.  }
  { Read a line of input.  Prompt with PROMPT.  A NULL PROMPT means none.  }
  function rl_read(_para1:Pchar):PChar;external 'readline' name 'readline';

  function rl_set_prompt(_para1:Pchar):longint;external 'readline' name 'rl_set_prompt';

  function rl_expand_prompt(_para1:Pchar):longint;external 'readline' name 'rl_expand_prompt';

  function rl_initialize:longint;external 'readline' name 'rl_initialize';

  { Undocumented; unused by readline  }
  function rl_discard_argument:longint;external 'readline' name 'rl_discard_argument';

  { Utility functions to bind keys to readline commands.  }
  function rl_add_defun(_ara1:char; _para2:rl_command_func_t; _para3:longint):longint;external 'readline' name 'rl_add_defun';

  function rl_bind_key(_ara1:longint; _para2:rl_command_func_t):longint;external 'readline' name 'rl_bind_key';

  function rl_bind_key_in_ma(_para1:longint; _para2:rl_command_func_t; _para3:Keymap):longint;external 'readline' name 'rl_bind_key_in_map';

  function rl_unbind_key(_para1:longint):longint;external 'readline' name 'rl_unbind_key';

  function rl_unbind_key_in_map(_para1:longint; _para2:Keymap):longint;external 'readline' name 'rl_unbind_key_in_map';

  function rl_bind_key_if_unbound(_ara1:longint; _para2:rl_command_func_t):longint;external 'readline' name 'rl_bind_key_if_unbound';

  function rl_bind_key_if_unbound_in_ma(_para1:longint; _para2:rl_command_func_t; _para3:Keymap):longint;external 'readline' name 'rl_bind_key_if_unbound_in_map';

  function rl_unbind_function_in_ma(_para1:rl_command_func_t; _para2:Keymap):longint;external 'readline' name 'rl_unbind_function_in_map';

  function rl_unbind_command_in_map(_para1:Pchar; _para2:Keymap):longint;external 'readline' name 'rl_unbind_command_in_map';

  function rl_bind_keyseq(_ara1:char; _para2:rl_command_func_t):longint;external 'readline' name 'rl_bind_keyseq';

  function rl_bind_keyseq_in_ma(_para1:char; _para2:rl_command_func_t; _para3:Keymap):longint;external 'readline' name 'rl_bind_keyseq_in_map';

  function rl_bind_keyseq_if_unbound(_ara1:char; _para2:rl_command_func_t):longint;external 'readline' name 'rl_bind_keyseq_if_unbound';

  function rl_bind_keyseq_if_unbound_in_ma(_para1:char; _para2:rl_command_func_t; _para3:Keymap):longint;external 'readline' name 'rl_bind_keyseq_if_unbound_in_map';

  function rl_generic_bind(_para1:longint; _para2:Pchar; _para3:Pchar; _para4:Keymap):longint;external 'readline' name 'rl_generic_bind';

  function rl_variable_value(_para1:Pchar):PChar;external 'readline' name 'rl_variable_value';

  function rl_variable_bind(_para1:Pchar; _para2:Pchar):longint;external 'readline' name 'rl_variable_bind';

  { Backwards compatibility, use rl_bind_keyseq_in_map instead.  }
  function rl_set_key(_ara1:char; _para2:rl_command_func_t; _para3:Keymap):longint;external 'readline' name 'rl_set_key';

  { Backwards compatibility, use rl_generic_bind instead.  }
  function rl_macro_bind(_para1:Pchar; _para2:Pchar; _para3:Keymap):longint;external 'readline' name 'rl_macro_bind';

  { Undocumented in the texinfo manual; not really useful to programs.  }
  function rl_translate_keyseq(_para1:Pchar; _para2:Pchar; _para3:Plongint):longint;external 'readline' name 'rl_translate_keyseq';

  function rl_untranslate_keyseq(_para1:longint):PChar;external 'readline' name 'rl_untranslate_keyseq';

  function rl_named_function(_ara1:char):prl_command_func_t;external 'readline' name 'rl_named_function';

  function rl_function_of_keyseq(_ara1:char; _para2:Keymap; _para3:longint):prl_command_func_t;external 'readline' name 'rl_function_of_keyseq';

  procedure rl_list_funmap_names;external 'readline' name 'rl_list_funmap_names';

  function rl_invoking_keyseqs_in_ma(_para1:rl_command_func_t; _para2:Keymap):PPChar;external 'readline' name 'rl_invoking_keyseqs_in_map';

  function rl_invoking_keyseqs(_ara1:rl_command_func_t):PPChar;external 'readline' name 'rl_invoking_keyseqs';

  procedure rl_function_dumper(_para1:longint);external 'readline' name 'rl_function_dumper';

  procedure rl_macro_dumper(_para1:longint);external 'readline' name 'rl_macro_dumper';

  procedure rl_variable_dumper(_para1:longint);external 'readline' name 'rl_variable_dumper';

  function rl_read_init_file(_para1:Pchar):longint;external 'readline' name 'rl_read_init_file';

  function rl_parse_and_bind(_para1:Pchar):longint;external 'readline' name 'rl_parse_and_bind';

  { Functions for manipulating keymaps.  }
  function rl_get_keymap_name(_para1:Keymap):PChar;external 'readline' name 'rl_get_keymap_name';

  { Undocumented; used internally only.  }
  procedure rl_set_keymap_from_edit_mode;external 'readline' name 'rl_set_keymap_from_edit_mode';

  function rl_get_keymap_name_from_edit_mode:PChar;external 'readline' name 'rl_get_keymap_name_from_edit_mode';

  { Functions for manipulating the funmap, which maps command names to functions.  }
  function rl_add_funma_entry(_para1:char; _para2:rl_command_func_t):longint;external 'readline' name 'rl_add_funmap_entry';

  function rl_funmap_names:PPChar;external 'readline' name 'rl_funmap_names';

  { Undocumented, only used internally -- there is only one funmap, and this
     function may be called only once.  }
  procedure rl_initialize_funmap;external 'readline' name 'rl_initialize_funmap';

  { Utility functions for managing keyboard macros.  }
  procedure rl_push_macro_input(_para1:Pchar);external 'readline' name 'rl_push_macro_input';

  { Functions for undoing, from undo.c  }
  procedure rl_add_undo(_para1:undo_code; _para2:longint; _para3:longint; _para4:Pchar);external 'readline' name 'rl_add_undo';

  procedure rl_free_undo_list;external 'readline' name 'rl_free_undo_list';

  function rl_do_undo:longint;external 'readline' name 'rl_do_undo';

  function rl_begin_undo_group:longint;external 'readline' name 'rl_begin_undo_group';

  function rl_end_undo_group:longint;external 'readline' name 'rl_end_undo_group';

  function rl_modifying(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_modifying';

  { Functions for redisplay.  }
  procedure rl_redisplay;external 'readline' name 'rl_redisplay';

  function rl_on_new_line:longint;external 'readline' name 'rl_on_new_line';

  function rl_on_new_line_with_prompt:longint;external 'readline' name 'rl_on_new_line_with_prompt';

  function rl_forced_update_display:longint;external 'readline' name 'rl_forced_update_display';

  function rl_clear_message:longint;external 'readline' name 'rl_clear_message';

  function rl_reset_line_state:longint;external 'readline' name 'rl_reset_line_state';

  function rl_crlf:longint;external 'readline' name 'rl_crlf';

  function rl_message:longint;external 'readline' name 'rl_message';

  function rl_show_char(_para1:longint):longint;external 'readline' name 'rl_show_char';

  { Undocumented in texinfo manual.  }
  function rl_character_len(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_character_len';

  { Save and restore internal prompt redisplay information.  }
  procedure rl_save_prompt;external 'readline' name 'rl_save_prompt';

  procedure rl_restore_prompt;external 'readline' name 'rl_restore_prompt';

  { Modifying text.  }
  procedure rl_replace_line(_para1:Pchar; _para2:longint);external 'readline' name 'rl_replace_line';

  function rl_insert_text(_para1:Pchar):longint;external 'readline' name 'rl_insert_text';

  function rl_delete_text(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_delete_text';

  function rl_kill_text(_para1:longint; _para2:longint):longint;external 'readline' name 'rl_kill_text';

  function rl_copy_text(_para1:longint; _para2:longint):PChar;external 'readline' name 'rl_copy_text';

  { Terminal and tty mode management.  }
  procedure rl_prep_terminal(_para1:longint);external 'readline' name 'rl_prep_terminal';

  procedure rl_deprep_terminal;external 'readline' name 'rl_deprep_terminal';

  procedure rl_tty_set_default_bindings(_para1:Keymap);external 'readline' name 'rl_tty_set_default_bindings';

  procedure rl_tty_unset_default_bindings(_para1:Keymap);external 'readline' name 'rl_tty_unset_default_bindings';

  function rl_reset_terminal(_para1:Pchar):longint;external 'readline' name 'rl_reset_terminal';

  procedure rl_resize_terminal;external 'readline' name 'rl_resize_terminal';

  procedure rl_set_screen_size(_para1:longint; _para2:longint);external 'readline' name 'rl_set_screen_size';

  procedure rl_get_screen_size(_para1:Plongint; _para2:Plongint);external 'readline' name 'rl_get_screen_size';

  procedure rl_reset_screen_size;external 'readline' name 'rl_reset_screen_size';

  function rl_get_termcap(_para1:Pchar):PChar;external 'readline' name 'rl_get_termcap';

  { Functions for character input.  }
  function rl_stuff_char(_para1:longint):longint;external 'readline' name 'rl_stuff_char';

  function rl_execute_next(_para1:longint):longint;external 'readline' name 'rl_execute_next';

  function rl_clear_pending_input:longint;external 'readline' name 'rl_clear_pending_input';

  function rl_read_key:longint;external 'readline' name 'rl_read_key';

  function rl_getc(_para1:PFILE):longint;external 'readline' name 'rl_getc';

  function rl_set_keyboard_input_timeout(_para1:longint):longint;external 'readline' name 'rl_set_keyboard_input_timeout';

  { `Public' utility functions .  }
  procedure rl_extend_line_buffer(_para1:longint);external 'readline' name 'rl_extend_line_buffer';

  function rl_ding:longint;external 'readline' name 'rl_ding';

  function rl_alphabetic(_para1:longint):longint;external 'readline' name 'rl_alphabetic';
  procedure rl_free(_para1:Pointer);external 'readline' name 'rl_free';

  { Readline signal handling, from signals.c  }
  function rl_set_signals:longint;external 'readline' name 'rl_set_signals';

  function rl_clear_signals:longint;external 'readline' name 'rl_clear_signals';

  procedure rl_cleanup_after_signal;external 'readline' name 'rl_cleanup_after_signal';

  procedure rl_reset_after_signal;external 'readline' name 'rl_reset_after_signal';

  procedure rl_free_line_state;external 'readline' name 'rl_free_line_state';

  procedure rl_echo_signal_char(_para1:longint);external 'readline' name 'rl_echo_signal_char';

  function rl_set_paren_blink_timeout(_para1:longint):longint;external 'readline' name 'rl_set_paren_blink_timeout';

  { Undocumented.  }
  function rl_maybe_save_line:longint;external 'readline' name 'rl_maybe_save_line';

  function rl_maybe_unsave_line:longint;external 'readline' name 'rl_maybe_unsave_line';

  function rl_maybe_replace_line:longint;external 'readline' name 'rl_maybe_replace_line';

  { Completion functions.  }
  function rl_complete_internal(_para1:longint):longint;external 'readline' name 'rl_complete_internal';

  procedure rl_display_match_list(_para1:PPchar; _para2:longint; _para3:longint);external 'readline' name 'rl_display_match_list';

  function rl_comletion_matches(_para1:char; _para2:rl_compentry_func_t):PPChar;external 'readline' name 'rl_completion_matches';

  function rl_username_completion_function(_para1:Pchar; _para2:longint):PChar;external 'readline' name 'rl_username_completion_function';

  function rl_filename_completion_function(_para1:Pchar; _para2:longint):PChar;external 'readline' name 'rl_filename_completion_function';

  function rl_comletion_mode(_para1:rl_command_func_t):longint;external 'readline' name 'rl_completion_mode';

  { ****************************************************************  }
  {								     }
  {			Well Published Variables		     }
  {								     }
  { ****************************************************************  }
  { The version of this incarnation of the readline library.  }

    var
      rl_library_version : PChar;external 'readline';
  { e.g., "4.2"  }
      rl_readline_version : longint;external 'readline';
  { e.g., 0x0402  }
  { True if this is real GNU readline.  }
      rl_gnu_readline_p : longint;external 'readline';
  { Flags word encapsulating the current readline state.  }
      rl_readline_state : longint;external 'readline';
  { Says which editing mode readline is currently using.  1 means emacs mode;
     0 means vi mode.  }
      rl_editing_mode : longint;external 'readline';
  { Insert or overwrite mode for emacs mode.  1 means insert mode; 0 means
     overwrite mode.  Reset to insert mode on each input line.  }
      rl_insert_mode : longint;external 'readline';
  { The name of the calling program.  You should initialize this to
     whatever was in argv[0].  It is used when parsing conditionals.  }
      rl_readline_name : PChar;external 'readline';
  { The prompt readline uses.  This is set from the argument to
     readline (), and should not be assigned to directly.  }
      rl_prompt : PChar;external 'readline';
  { The prompt string that is actually displayed by rl_redisplay.  Public so
     applications can more easily supply their own redisplay functions.  }
      rl_display_prompt : PChar;external 'readline';
  { The line buffer that is in use.  }
      rl_line_buffer : PChar;external 'readline';
  { The location of point, and end.  }
      rl_point : longint;external 'readline';
      rl_end : longint;external 'readline';
  { The mark, or saved cursor position.  }
      rl_mark : longint;external 'readline';
  { Flag to indicate that readline has finished with the current input
     line and should return it.  }
      rl_done : longint;external 'readline';
  { If set to a character value, that will be the next keystroke read.  }
      rl_pending_input : longint;external 'readline';
  { Non-zero if we called this function from _rl_dispatch().  It's present
     so functions can find out whether they were called from a key binding
     or directly from an application.  }
      rl_dispatching : longint;external 'readline';
  { Non-zero if the user typed a numeric argument before executing the
     current function.  }
      rl_explicit_arg : longint;external 'readline';
  { The current value of the numeric argument specified by the user.  }
      rl_numeric_arg : longint;external 'readline';
  { The address of the last command function Readline executed.  }
      rl_last_func : rl_command_func_t;external 'readline';
  { The name of the terminal to use.  }
      rl_terminal_name : PChar;external 'readline';
  { The input and output streams.  }
      rl_instream : PFILE;external 'readline';
      rl_outstream : PFILE;external 'readline';
  { If non-zero, Readline gives values of LINES and COLUMNS from the environment
     greater precedence than values fetched from the kernel when computing the
     screen dimensions.  }
      rl_prefer_env_winsize : longint;external 'readline';
  { If non-zero, then this is the address of a function to call just
     before readline_internal () prints the first prompt.  }
      rl_startu_hook :rl_hook_func_t;external 'readline';
  { If non-zero, this is the address of a function to call just before
     readline_internal_setup () returns and readline_internal starts
     reading input characters.  }
      rl_re_input_hook :rl_hook_func_t;external 'readline';
  { The address of a function to call periodically while Readline is
     awaiting character input, or NULL, for no event handling.  }
      rl_event_hook : rl_hook_func_t;external 'readline';
  { The address of the function to call to fetch a character from the current
     Readline input stream  }
      rl_getc_function : rl_getc_func_t;external 'readline';
      rl_redislay_function :rl_voidfunc_t;external 'readline';
      rl_rep_term_function :rl_vintfunc_t;external 'readline';
      rl_derep_term_function :rl_voidfunc_t;external 'readline';
  { Dispatch variables.  }
      rl_executing_keymap : Keymap;external 'readline';
      rl_binding_keymap : Keymap;external 'readline';
  { Display variables.  }
  { If non-zero, readline will erase the entire line, including any prompt,
     if the only thing typed on an otherwise-blank line is something bound to
     rl_newline.  }
      rl_erase_empty_line : longint;external 'readline';
  { If non-zero, the application has already printed the prompt (rl_prompt)
     before calling readline, so readline should not output it the first time
     redisplay is done.  }
      rl_already_prompted : longint;external 'readline';
  { A non-zero value means to read only this many characters rather than
     up to a character bound to accept-line.  }
      rl_num_chars_to_read : longint;external 'readline';
  { The text of a currently-executing keyboard macro.  }
      rl_executing_macro : PChar;external 'readline';
  { Variables to control readline signal handling.  }
  { If non-zero, readline will install its own signal handlers for
     SIGINT, SIGTERM, SIGQUIT, SIGALRM, SIGTSTP, SIGTTIN, and SIGTTOU.  }
      rl_catch_signals : longint;external 'readline';
  { If non-zero, readline will install a signal handler for SIGWINCH
     that also attempts to call any calling application's SIGWINCH signal
     handler.  Note that the terminal is not cleaned up before the
     application's signal handler is called; use rl_cleanup_after_signal()
     to do that.  }
      rl_catch_sigwinch : longint;external 'readline';
  { Completion variables.  }
  { Pointer to the generator function for completion_matches ().
     NULL means to use rl_filename_completion_function (), the default
     filename completer.  }
      rl_comletion_entry_function :rl_compentry_func_t;external 'readline';
  { Optional generator for menu completion.  Default is
     rl_completion_entry_function (rl_filename_completion_function).  }
      rl_menu_comletion_entry_function :rl_compentry_func_t;external 'readline';
  { If rl_ignore_some_completions_function is non-NULL it is the address
     of a function to call after all of the possible matches have been
     generated, but before the actual completion is done to the input line.
     The function is called with one argument; a NULL terminated array
     of PChar.  If your function removes any of the elements, they
     must be rl_free()'ed.  }
      rl_ignore_some_comletions_function :rl_compignore_func_t;external 'readline';
  { Pointer to alternative function to create matches.
     Function is called with TEXT, START, and END.
     START and END are indices in RL_LINE_BUFFER saying what the boundaries
     of TEXT are.
     If this function exists and returns NULL then call the value of
     rl_completion_entry_function to try to match, otherwise use the
     array of strings returned.  }
      rl_attemted_completion_function :rl_completion_func_t;external 'readline';
  { The basic list of characters that signal a break between words for the
     completer routine.  The initial contents of this variable is what
     breaks words in the shell, i.e. "n\"\\'`@$>".  }
      rl_basic_word_break_characters : PChar;external 'readline';
  { The list of characters that signal a break between words for
     rl_complete_internal.  The default list is the contents of
     rl_basic_word_break_characters.   }
  {const }      rl_completer_word_break_characters : PChar;external 'readline';
  { Hook function to allow an application to set the completion word
     break characters before readline breaks up the line.  Allows
     position-dependent word break characters.  }
      rl_comletion_word_break_hook :rl_cpvfunc_t;external 'readline';
  { List of characters which can be used to quote a substring of the line.
     Completion occurs on the entire substring, and within the substring
     rl_completer_word_break_characters are treated as any other character,
     unless they also appear within this list.  }
      rl_completer_quote_characters : PChar;external 'readline';
  { List of quote characters which cause a word break.  }
      rl_basic_quote_characters : PChar;external 'readline';
  { List of characters that need to be quoted in filenames by the completer.  }
      rl_filename_quote_characters : PChar;external 'readline';
  { List of characters that are word break characters, but should be left
     in TEXT when it is passed to the completion function.  The shell uses
     this to help determine what kind of completing to do.  }
      rl_special_prefixes : PChar;external 'readline';
  { If non-zero, then this is the address of a function to call when
     completing on a directory name.  The function is called with
     the address of a string (the current directory name) as an arg.  It
     changes what is displayed when the possible completions are printed
     or inserted.  The directory completion hook should perform
     any necessary dequoting.  This function should return 1 if it modifies
     the directory name pointer passed as an argument.  If the directory
     completion hook returns 0, it should not modify the directory name
     pointer passed as an argument.  }
      rl_directory_completion_hook : prl_icppfunc_t;external 'readline';
  { If non-zero, this is the address of a function to call when completing
     a directory name.  This function takes the address of the directory name
     to be modified as an argument.  Unlike rl_directory_completion_hook, it
     only modifies the directory name used in opendir(2), not what is displayed
     when the possible completions are printed or inserted.  If set, it takes
     precedence over rl_directory_completion_hook.  The directory rewrite
     hook should perform any necessary dequoting.  This function has the same
     return value properties as the directory_completion_hook.

     I'm not happy with how this works yet, so it's undocumented.  I'm trying
     it in bash to see how well it goes.  }
      rl_directory_rewrite_hook : rl_icppfunc_t;external 'readline';
  { If non-zero, this is the address of a function to call when reading
     directory entries from the filesystem for completion and comparing
     them to the partial word to be completed.  The function should
     either return its first argument (if no conversion takes place) or
     newly-allocated memory.  This can, for instance, convert filenames
     between character sets for comparison against what's typed at the
     keyboard.  The returned value is what is added to the list of
     matches.  The second argument is the length of the filename to be
     converted.  }
      rl_filename_rewrite_hook : rl_dequote_func_t;external 'readline';
  { Backwards compatibility with previous versions of readline.  }

  function rl_symbolic_link_hook: prl_icppfunc_t;
  { If non-zero, then this is the address of a function to call when
     completing a word would normally display the list of possible matches.
     This function is called instead of actually doing the display.
     It takes three arguments: (matches: PPChar; num_matches, max_length: longint)
     where MATCHES is the array of strings that matched, NUM_MATCHES is the
     number of strings in that array, and MAX_LENGTH is the length of the
     longest string in that array.  }

    var
      rl_comletion_display_matches_hook :rl_compdisp_func_t;external 'readline';
  { Non-zero means that the results of the matches are to be treated
     as filenames.  This is ALWAYS zero on entry, and can only be changed
     within a completion entry finder function.  }
      rl_filename_completion_desired : longint;external 'readline';
  { Non-zero means that the results of the matches are to be quoted using
     double quotes (or an application-specific quoting mechanism) if the
     filename contains any characters in rl_word_break_chars.  This is
     ALWAYS non-zero on entry, and can only be changed within a completion
     entry finder function.  }
      rl_filename_quoting_desired : longint;external 'readline';
  { Set to a function to quote a filename in an application-specific fashion.
     Called with the text to quote, the type of match found (single or multiple)
     and a pointer to the quoting character to be used, which the function can
     reset if desired.  }
      rl_filename_quoting_function : rl_quote_func_t;external 'readline';
  { Function to call to remove quoting characters from a filename.  Called
     before completion is attempted, so the embedded quotes do not interfere
     with matching names in the file system.  }
      rl_filename_dequoting_function : rl_dequote_func_t;external 'readline';
  { Function to call to decide whether or not a word break character is
     quoted.  If a character is quoted, it does not break words for the
     completer.  }
      rl_char_is_quoted_ :rl_linebuf_func_t;external 'readline';
  { Non-zero means to suppress normal filename completion after the
     user-specified completion function has been called.  }
      rl_attempted_completion_over : longint;external 'readline';
  { Set to a character describing the type of completion being attempted by
     rl_complete_internal; available for use by application completion
     functions.  }
      rl_completion_type : longint;external 'readline';
  { Set to the last key used to invoke one of the completion functions  }
      rl_completion_invoking_key : longint;external 'readline';
  { Up to this many items will be displayed in response to a
     possible-completions call.  After that, we ask the user if she
     is sure she wants to see them all.  The default value is 100.  }
      rl_completion_query_items : longint;external 'readline';
  { Character appended to completed words when at the end of the line.  The
     default is a space.  Nothing is added if this is '\0'.  }
      rl_completion_append_character : longint;external 'readline';
  { If set to non-zero by an application completion function,
     rl_completion_append_character will not be appended.  }
      rl_completion_suppress_append : longint;external 'readline';
  { Set to any quote character readline thinks it finds before any application
     completion function is called.  }
      rl_completion_quote_character : longint;external 'readline';
  { Set to a non-zero value if readline found quoting anywhere in the word to
     be completed; set before any application completion function is called.  }
      rl_completion_found_quote : longint;external 'readline';
  { If non-zero, the completion functions don't append any closing quote.
     This is set to 0 by rl_complete_internal and may be changed by an
     application-specific completion function.  }
      rl_completion_suppress_quote : longint;external 'readline';
  { If non-zero, readline will sort the completion matches.  On by default.  }
      rl_sort_completion_matches : longint;external 'readline';
  { If non-zero, a slash will be appended to completed filenames that are
     symbolic links to directory names, subject to the value of the
     mark-directories variable (which is user-settable).  This exists so
     that application completion functions can override the user's preference
     (set via the mark-symlinked-directories variable) if appropriate.
     It's set to the value of _rl_complete_mark_symlink_dirs in
     rl_complete_internal before any application-specific completion
     function is called, so without that function doing anything, the user's
     preferences are honored.  }
      rl_completion_mark_symlink_dirs : longint;external 'readline';
  { If non-zero, then disallow duplicates in the matches.  }
      rl_ignore_completion_duplicates : longint;external 'readline';
  { If this is non-zero, completion is (temporarily) inhibited, and the
     completion character will be inserted as any other.  }
      rl_inhibit_completion : longint;external 'readline';
  { Input error; can be returned by (*rl_getc_function) if readline is reading
     a top-level command (RL_ISSTATE (RL_STATE_READCMD)).  }

  const
    READERR = -(2);
  { Definitions available for use by readline clients.  }
    RL_PROMPT_START_IGNORE = '\001';
    RL_PROMPT_END_IGNORE = '\002';
  { Possible values for do_replace argument to rl_filename_quoting_function,
     called by rl_complete_internal.  }
    NO_MATCH = 0;
    SINGLE_MATCH = 1;
    MULT_MATCH = 2;
  { Possible state values for rl_readline_state  }
  { no state; before first call  }
    RL_STATE_NONE = $000000;
  { initializing  }
    RL_STATE_INITIALIZING = $0000001;
  { initialization done  }
    RL_STATE_INITIALIZED = $0000002;
  { terminal is prepped  }
    RL_STATE_TERMPREPPED = $0000004;
  { reading a command key  }
    RL_STATE_READCMD = $0000008;
  { reading input after ESC  }
    RL_STATE_METANEXT = $0000010;
  { dispatching to a command  }
    RL_STATE_DISPATCHING = $0000020;
  { reading more input in a command function  }
    RL_STATE_MOREINPUT = $0000040;
  { doing incremental search  }
    RL_STATE_ISEARCH = $0000080;
  { doing non-inc search  }
    RL_STATE_NSEARCH = $0000100;
  { doing a history search  }
    RL_STATE_SEARCH = $0000200;
  { reading numeric argument  }
    RL_STATE_NUMERICARG = $0000400;
  { getting input from a macro  }
    RL_STATE_MACROINPUT = $0000800;
  { defining keyboard macro  }
    RL_STATE_MACRODEF = $0001000;
  { overwrite mode  }
    RL_STATE_OVERWRITE = $0002000;
  { doing completion  }
    RL_STATE_COMPLETING = $0004000;
  { in readline sighandler  }
    RL_STATE_SIGHANDLER = $0008000;
  { doing an undo  }
    RL_STATE_UNDOING = $0010000;
  { rl_execute_next called  }
    RL_STATE_INPUTPENDING = $0020000;
  { tty special chars saved  }
    RL_STATE_TTYCSAVED = $0040000;
  { using the callback interface  }
    RL_STATE_CALLBACK = $0080000;
  { reading vi motion arg  }
    RL_STATE_VIMOTION = $0100000;
  { reading multiple-key command  }
    RL_STATE_MULTIKEY = $0200000;
  { entered vi command mode at least once  }
    RL_STATE_VICMDONCE = $0400000;
  { updating terminal display  }
    RL_STATE_REDISPLAYING = $0800000;
  { done; accepted line  }
    RL_STATE_DONE = $1000000;

procedure RL_SETSTATE(x: longint);
procedure RL_UNSETSTATE(x: longint);

  type
    Preadline_state  = ^readline_state;
    readline_state = record
        point : longint;
        _end : longint;
        mark : longint;
        buffer : PChar;
        buflen : longint;
        ul : PUNDO_LIST;
        prompt : PChar;
        rlstate : longint;
        done : longint;
        kmap : Keymap;
        lastfunc : rl_command_func_t;
        insmode : longint;
        edmode : longint;
        kseqlen : longint;
        inf : PFILE;
        outf : PFILE;
        pendingin : longint;
        macro : PChar;
        catchsigs : longint;
        catchsigwinch : longint;
        reserved : array[0..63] of char;
      end;

    function RL_ISSTATE(x : longint) : longint;


  function rl_save_state(_para1:Preadline_state):longint;external 'readline' name 'rl_save_state';

  function rl_restore_state(_para1:Preadline_state):longint;external 'readline' name 'rl_restore_state';

implementation

  function rl_symbolic_link_hook: prl_icppfunc_t;
  begin
    rl_symbolic_link_hook := rl_directory_completion_hook;
  end;

    function RL_ISSTATE(x : longint) : longint;
    begin
      RL_ISSTATE:=rl_readline_state and x;
    end;

procedure RL_SETSTATE(x: longint);
begin
  rl_readline_state := rl_readline_state or x;
end;

procedure RL_UNSETSTATE(x: longint);
begin
  rl_readline_state := rl_readline_state and (not x);
end;

{$INCLUDE "historyimpl.inc"}

end.
