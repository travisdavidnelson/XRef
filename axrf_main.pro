% axrf_main.pro

/* main :-
   command_line(ARGS),
   get_files(ARGS, FILES),
   axrf:xref(FILES, 'xref.txt'). */

get_files([], []).
get_files([ARG|ARGS], [FILE|FILES]) :-
   atom_codes(FILE, ARG),
   get_files(ARGS, FILES).

main :-
   axrf:xref(['dw_main.pro', 'dw_rules.pro', 'dw_data.pro']).
%   axrf:xref(['dw_main.pro', 'dw_rules.pro', 'dw_data.pro'], 'output.txt').

main :-
   nl, write('nope'), nl.