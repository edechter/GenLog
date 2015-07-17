

:- module(misc, [append1/3,
                 append1d/3]).


%% ----------------------------------------------------------------------
%%      append1(X, Y, Z)
%%
%% Same as append/3, but X, Y, and Z cannot be empty. 
append1([H], [L|Ls], [H, L|Ls]).
append1([H|T], L, [H|R]) :-
        L \= [],
	append1(T, L, R).


%% ----------------------------------------------------------------------
%%     append1d(X, Y, Z)
%%
%% Same as append1/3, but goal is delayed until one of X, Y, or Z is
%% ground.
append1d(X, Y, Z) :-
         when((ground(X) ; ground(Y); ground(Z)),
              append1(X, Y, Z)).


                