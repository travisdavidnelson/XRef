% axrf_main.pro

:- use_module('xrfSicstus.pro').
:- use_module(library(file_systems)).

:- dynamic tab_text/1.

here('/Users/dmerritt/dev/solstice/XRef/').

main :-
   here(HERE),
   current_directory(_, HERE),
   set_tab('   '),
   xrf:xref(['dw_main.pro']),
%   xrf:xref(['xrfSicstusMain.pro', 'xrfSicstus.pro']),
%   tell('output_duckworld.txt'),
   xreport,
%   told,
   set_tab('.  '),
   cone_below(user:main/0),
   cone_above(data:loc/2).

main :-
   nl, write('nope'), nl.


xreport :-
   warning_report,
   uses_report.

uses_report :-
   nl, write('----- Predicate Use -----'), nl,
   findall(MFA, xrf:uses(MFA,_), L),
   sort(L, SL),
   uses_report(SL).

uses_report([]).
uses_report([M:F/A|Z]) :-
      write(M:F/A),
      (xrf:dynamic_pred(M:F/A) -> write('   '), write(dynamic); true),
      nl,
      modified_report(M:F/A),
      subgoal_report(M:F/A),
      called_by_report(M:F/A),
      !, uses_report(Z).

modified_report(M:F/A) :-
      xrf:dynamic_pred(M:F/A),
      !,
      xrf:modified_in(M:F/A, L1),
      tab(1),
      (L1 \= [] ->
          write('modified in:'),
          nl, tab(2),
          write_list(L1, (nl, tab(2)))
          ;
          write('not modified' ) ),
      nl.
modified_report(_).

subgoal_report(M:F/A) :-
      xrf:uses(M:F/A, L),
      tab(1),
      (L \= [] ->
         write('subgoals:'),
         nl, tab(2),
         write_list(L, (nl, tab(2)))
         ;
         write('no subgoals') ),
      nl.

called_by_report(M:F/A) :-
      xrf:used_by(M:F/A, L2),
      tab(1),
      (L2 \= [] ->
         write('called from:  '),
         nl, tab(2),
         write_list(L2, (nl, tab(2)))
         ;
         write('no callers') ),
      nl.

tab(0) :- !.
tab(N) :-
        tab_text(T),
        write(T),
        NN is N - 1,
        tab(NN).

set_tab(T) :-
        retractall(tab_text(_)),
        assert(tab_text(T)).

cone_below(M:F/A) :-
     nl,
     write(M:F/A), write('    '), write('uses:'), nl,
     xrf:uses(M:F/A, L),
     cones_below(L, 1, [M:F/A]).

cones_below([], _, _) :- !.
cones_below([M:F/A | MFAs], I, Visited) :-
      memberchk(M:F/A, Visited),
      tab(I),
      write(M:F/A),
      write('  ***** looping *****'),
      nl,
      !, cones_below(MFAs, I, [M:F/A | Visited]).
cones_below([M:F/A | MFAs], I, Visited) :-
      tab(I),
      write(M:F/A),
      nl,
      xrf:uses(M:F/A, L),
      II is I + 1,
      cones_below(L, II, [M:F/A | Visited]),
      !, cones_below(MFAs, I, Visited).
cones_below([_ | MFAs], I, Visited) :-
      !, cones_below(MFAs, I, Visited).

cone_above(M:F/A) :-
     nl,
     write(M:F/A), write('   '), write('modified in:'), nl,
     xrf:modified_in(M:F/A, L),
     cones_above(L, 1, [M:F/A]),
     nl,
     write(M:F/A), write('   '), write('used by:'), nl,
     xrf:used_by(M:F/A, LL),
     cones_above(LL, 1, [M:F/A]).


cones_above([], _, _) :- !.
cones_above([M:F/A | MFAs], I, Visited) :-
      memberchk(M:F/A, Visited),
      tab(I),
      write(M:F/A),
      write('  ***** looping *****'),
      nl,
      !, cones_above(MFAs, I, [M:F/A | Visited]).
cones_above([M:F/A | MFAs], I, Visited) :-
      tab(I),
      write(M:F/A),
      nl,
      xrf:used_by(M:F/A, L),
      II is I + 1,
      cones_above(L, II, [M:F/A | Visited]),
      !, cones_above(MFAs, I, Visited).
cones_above([_ | MFAs], I, Visited) :-
      !, cones_above(MFAs, I, Visited).


warning_report :-
   nl, write('----- Warnings -----'), nl,
   findall(W, xrf:warning(_, W), Ws),
   sort(Ws, SWs),
   write_warnings(SWs).
   
write_warnings([]).
write_warnings([W|Ws]) :-
   write_list(W, true), nl,
   write_warnings(Ws).

write_list([], _).
write_list([X], _) :-
  !, write(X).
write_list([X|Y], Separator) :-
  write(X),
  call(Separator),
  write_list(Y, Separator).
