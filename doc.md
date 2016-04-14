##Amzi! cross reference utility and
##lint checker.

The cross reference utility can be used to find
a number of common Prolog programming errors.
It can check for correct references of predicates
across modules, and take into account extended
predicates (LSXs) and compiled libraries.

It provides a full cross reference of predicates,
indicating for each predicate both predicates
called by the given predicate, and predicates
that call the given predicate. The defining
modules for the cross-referenced predicates are
given as well, with notes for built-in and
extended predicates.

The cross reference utility checks for:

  - common Prolog punctuation errors
  - undeclared discontiguous clauses
  - orphaned predicates (never used)
  - undefined predicates (used but not defined)
  - module imports and exports
  - extended (LSX) and library predicates

To use the software from the listener,
first load axrf.plm and then import it
so you can access it from the listener.
%
Call axrf:xref/2
  arg1 - a list of files to be cross referenced
  arg2 - the file to contin the xref data
%
For example, to cross reference amzi_air.pro and
amzi_air_ui.pro:
%
?- load(axrf).
yes
?- import(axrf).
yes
?- axrf:xref(['amzi_air.pro', 'amzi_air_ui.pro'], 'output.txt').
yes

Then open output.txt in the editor to see the results.
%
For complex projects that depend on other modules
you can set up the correct environment for the
program and then run axrf.  It is probably easiest
to create a small test program that gets consulted
and run in the listener.
%
For example, here's a test file for a project with
operator definitions that uses a library and an LSX.
%
file runaxrf.pro
%
:- load(axrf).
:- consult('..\\jigs\\kw_ops.pro').
:- load(list).
:- loadlsx('aosutils.lsx').
:- import(axrf).

go :- axrf:xref([
       'author.pro',
       'convert.pro',
       'error_check.pro',
       'find_replace.pro',
       'utilities.pro',
       'webls324.pro',
       'xref.pro'],
   'author.xrf').
%
To run it open the listener and
%
?- consult(runaxrf).
?- go.
%



