/*
  gl_term.pl
  Eyal Dechter
  edechter@mit.edu
  7/21/2014

  This module contains the GenLog term data type (gl_term) and
  predicates for manipulating gl terms.

  Data type: a gl_term is a term of the form:
  
    gl_term(Functor/Arity, ListOfArgs, ListOfConditioners)

*/
:- module(gl_term,
          [gl_term_surface_form/2,
           is_gl_term/1
           ]).

%% ----------------------------------------------------------------------
%%     portray dispatch on gl_term
%%

:- multifile error:has_type/2.
error:has_type(gl_term, G) :-
        is_gl_term(G).
               
:- multifile
        user:portray/1.

user:portray(GlTerm) :-
        is_gl_term(GlTerm),
        portray_gl_term(GlTerm).


portray_gl_term(gl_term(F/_, Args, Conds)) :- 
        format('~p(', [F]),
        portray_gl_term_args(Args, Conds),
        format(')').

portray_gl_term_args([], []).
portray_gl_term_args([], [C]) :-
        !, 
        format(', ~w', [C]).
portray_gl_term_args([], [C|Conds]) :-
        format(', ~w', [C]),
        portray_gl_term_args([], Conds).
portray_gl_term_args([A], [B|Conds]) :-
        !,
        format('~w | ~w', [A, B]),
        portray_gl_term_args([], Conds).
portray_gl_term_args([A], []) :-
        !,
        format('~w', [A]).
portray_gl_term_args([A|As], Conds) :-
        format('~w, ', [A]),
        portray_gl_term_args(As, Conds).

        
        
%% ----------------------------------------------------------------------
%%      is_gl_term(+Term).
%%
%% True if Term is a gl_term.
is_gl_term(Term) :-
        nonvar(Term),
        functor(Term, gl_term, 3).

%% ----------------------------------------------------------------------
%%      gl_term_surface_form(+GlTerm, ?SurfaceTerm)
%%      gl_term_surface_form(?GlTerm, +SurfaceTerm)
%%
%% Convert between ql_term GlTerm and equivalent surface form.
gl_term_surface_form(Term, Surface) :-
        ((var(Term), ! ; is_gl_term(Term))
        -> gl_term_surface_form_(Term, Surface)
        ;
         throw(error(gl_term_surface_form/2,
                           context(instantiation_error, "First argument must be a variable or a gl_term.")))
        ).
gl_term_surface_form_(gl_term(F/_, Args, Conds),
                      SurfaceTerm) :-
        (nonvar(SurfaceTerm) ->
         SurfaceTerm =.. SurfaceAsList
        ;
          true), 
        SurfaceAsList = [F|SurfaceArgs],
        surface_args_conds(SurfaceArgs, Args, Conds),
        SurfaceTerm =.. SurfaceAsList.

surface_args_conds(Args, Vars, Conds) :-
        surface_args_conds(Args, Vars, Conds, vars).
surface_args_conds([], [], [], _).
surface_args_conds(V, [A], [B|Bs], vars) :-
        var(V),
        !,
        V = [(A | B) | Bs].
                  
surface_args_conds([I|In], [A], [B|In], vars) :-
        compound(I),
        I = (A | B),
        !.
surface_args_conds([A|In], [A|Vars], Conds, vars) :-
        !, 
        surface_args_conds(In, Vars, Conds, vars).
surface_args_conds([A|In], Vars, [A|Conds], conds) :-
        surface_args_conds(In, Vars, Conds, conds).



%% ----------------------------------------------------------------------
%%      unit tests for gl rules
%%
:- begin_tests('gl rules').

test('split surface args to args and conditioners') :-
        SurfaceArgs = [X, Y, (a(Y) | a(Z, X)), X, (1,2)],
        surface_args_conds(SurfaceArgs, Args, Conds),
        assertion(Args =@= [X, Y, a(Y)]), 
        assertion(Conds =@= [a(Z, X), X, (1,2)]).       

test('surface_args_conds(-, +, +)') :- 
        Args = [X, Y, a(Y)],
        Conds = [a(Z, X), X, (1,2)],        
        surface_args_conds(SurfaceArgs, Args, Conds),
        assertion(SurfaceArgs =@= [X, Y, (a(Y) | a(Z, X)), X, (1,2)]).
        

test('convert gl_term to surface form') :-
        GlTerm = gl_term(f/3, [X, a(X, Y)], [1, (2, 3)]),
        gl_term_surface_form(GlTerm, Surface),
        assertion(Surface =@= f(X, (a(X, Y) | 1), (2, 3))).

:- end_tests('gl rules').
