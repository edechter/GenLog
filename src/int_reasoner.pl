

:- module(int_reasoner,
          [
           eq/2,
           le/2,
           ge/2,
           leq/2,
           geq/2,
           op(600, xfy, eq),
           op(600, xfy, le),
           op(600, xfy, ge),
           op(600, xfy, leq),
           op(600, xfy, geq)
          
           
          ]).

:- use_module(library(clpfd)).
:- use_module(library(clpq)).

X eq Y :-
        X #= Y,
        {X = Y}.

X le Y :-
        X #< Y,
        {X < Y}.

X ge Y :-
        X #> Y,
        {X > Y}.

X leq Y :-
        X #=< Y,
        {X =< Y}.

X geq Y :-
        X #>= Y,
        {X >= Y}.