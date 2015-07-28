

:- module(misc, [append1/3,
                 % nonempty/1,
                 append1d/3]).


:- use_module(library(clpfd)).

%% ----------------------------------------------------------------------
%%      append1(X, Y, Z)
%%
%% Same as append/3, but X, Y, and Z cannot be empty.
append1(X, Y, Z) :-
        (Z \= [_, _|_]
        ;
         X \= [_|_]
        ;
         Y \= [_|_]
        ),
        !,
        fail.
append1([H], Ls, [H | Ls]).
append1([H|T], L, [H|R]) :-
	append1d(T, L, R).

append1d(X, Y, Z) :- append1(X, Y, Z).




                