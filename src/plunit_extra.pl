:- module(plunit_extra,
          [op(700, xfx, ~=),
           (~=)/2,
           gl_test_file/1,
           setup_test_gl/0,
           setup_test_gl/1,
           cleanup_test_gl/0]).

:- use_module(compile).
:- use_module(kb).
:- use_module(gl_rule).


:- getenv('GENLOG_ROOT', Dir),
   atomic_list_concat([Dir, '/experiments/gls/'], GlDir),
   asserta(user:file_search_path(gl, GlDir)).




:- op(700, xfx, ~=).

plunit_float_tol(1e-3).

X ~= Y :-
        number(X), number(Y), !,
        plunit_float_tol(Tol),
        abs(X-Y) < Tol.

(K1-X) ~= (K2-Y) :- !,
        K1=K2,
        X ~= Y.

[] ~= [] :- !.

[X|Xs] ~= [Y|Ys] :-
        X ~= Y,
        Xs ~= Ys.


%% ----------------------------------------------------------------------
%% Setting up gl files for unit tests. 

gl_test_file(gl(trivial)).

setup_test_gl :-
        remove_all_rules,
        gl_test_file(File), 
        compile_gl_file(File).

setup_test_gl(File) :-
        remove_all_rules, 
        compile_gl_file(File).

cleanup_test_gl :-
        remove_all_rules.
