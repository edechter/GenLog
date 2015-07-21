

:- module(misc, [append1/3,
                 append1d/3]).


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
append1([H], [L|Ls], [H, L|Ls]).
append1([H|T], L, [H|R]) :-
	append1d(T, L, R).


%% ----------------------------------------------------------------------
%%     append1d(X, Y, Z)
%%
%% Same as append1/3, but goal is delayed until one of X, Y, or Z is
%% ground.
append1d(_, _, Z) :-
        Z \= [_, _|_],
        !,
        fail.
append1d(X, _, _) :-
        X \= [_|_],
        !,
        fail.
append1d(_, Y, _) :-
        Y \= [_|_],
        !,
        fail.

        
append1d(X, Y, Z) :-
         % !, 
         % X = [_|_],
         % !, se_module(library(chr)).
         % Y = [_|_],
         % !, 
         % Z = [_, _ | _],
         % !, 
         when((nonvar(X), nonvar(Y)
              ;
               nonvar(Z) 
              ),

              append1(X, Y, Z)).


                