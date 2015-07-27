

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

% nonempty(L) :-
%         N #> 0, 
%         L size N.
        

% append1(X, Y, Z) :-
%         nonempty(X),
%         nonempty(Y),
%         concat(Z, X, Y).



%% ----------------------------------------------------------------------
%%     append1d(X, Y, Z)
%%
%% Same as append1/3, but goal is delayed until one of X, Y, or Z is
%% ground.
% append1d(_, _, Z) :-
%         Z \= [_, _|_],
%         !,
%         fail.
% append1d(X, _, _) :-
%         X \= [_|_],
%         !,
%         fail.
% append1d(_, Y, _) :-
%         Y \= [_|_],
%         !,
%         fail.
append1d(X, Y, Z) :-
        % writeln(X-Y-Z),
        (var(Z) ->
         writeln(X-Y-Z)
        ;
         true),
        append1(X, Y, Z).
% append1d(X, Y, Z) :-
%          when((ground(X), ground(Y)
%              ;
%               ground(Z)),
%               (
%                append1(X, Y, Z)
%                )
%              ).


                