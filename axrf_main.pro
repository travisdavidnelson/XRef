% axrf_main.pro

:- use_module('axrf.pro').
:- use_module(library(file_systems)).

here('/Users/dmerritt/dev/solstice/XRef/').

main :-
   here(HERE),
   current_directory(_, HERE),
   axrf:xref(['dw_main.pro', 'dw_rules.pro', 'dw_data.pro']).
%   axrf:xref(['dw_main.pro', 'dw_rules.pro', 'dw_data.pro'], 'output.txt').

main :-
   nl, write('nope'), nl.