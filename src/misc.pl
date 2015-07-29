

:- module(misc, [append1/3,
                 append1d/3,

                 take/3,
                 replicate/3,
                 print_current_frame/0,

                 and_to_list/2,
                 list_to_and/2,
                 call1/1
                

                 ]).


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


%% ----------------------------------------------------------------------
%% Auxiliary

%% take(N[Int], ListIn, ListOut) returns the first N elements of ListIn
%% in ListOut. If N > length(ListIn), ListOut = ListIn.
take(0, _, []) :- !.
take(_, [], []).
take(N, [X|Xs], [X|Ys]) :-
        N1 is N - 1,
        take(N1, Xs, Ys).

% pair_list_firsts(List of pairs X-Y, List of first elements X)
pair_list_firsts([], []).
pair_list_firsts([X-_|Rest], [X|Xs]) :-
        pair_list_firsts(Rest, Xs).

% pair_list_seconds(List of pairs X-Y, List of second elements X)
pair_list_seconds([], []).
pair_list_seconds([_-Y|Rest], [Y|Ys]) :-
        pair_list_seconds(Rest, Ys).

% replicate(N, Xs, Ys) if Ys is N repetitions of Xs
replicate(N, Xs, Ys) :-
        replicate(N, Xs, [], Ys).

replicate(0, _, YsIn, YsOut) :- !, YsIn = YsOut.
replicate(N, Xs, YsIn, YsOut) :-
        N1 is N - 1,
        append(Xs, YsIn, YsTmp), 
        replicate(N1, Xs, YsTmp, YsOut).


:- begin_tests(auxiliary).

test(replicate, [true(Xs = [a,a,a,a,a])]) :-
        replicate(5, [a], Xs).

test(replicate_empty, [true(Xs = [])]) :-
        replicate(5, [], Xs).


:- end_tests(auxiliary).

% print_current_frame
print_current_frame :-
        prolog_current_frame(Frame), 
        format("Current Frame: ~w\n", [Frame]).



%% ----------------------------------------------------------------------






%%      call1(+G)
%%
%% If G is a list, apply call1 to each element in turn.
%% If G is not a list, just like call(G).
call1([]) :- !.
call1([G|Gs]) :-
        !,
        call1(G),
        call1(Gs).
call1(G) :-
        functor(G, F, _),
        F \= '|',
        !,
        call(G).

%% conjunction to list
and_to_list(true, []).
and_to_list(C, [C]) :-
        C \= true,
        C \= (_,_).
and_to_list((true, Cs), Xs) :-
        and_to_list(Cs, Xs).
and_to_list((C, Cs), [C|Xs]) :-
        C \= true,
        and_to_list(Cs, Xs).

list_to_and([], true).
list_to_and([X], X).
list_to_and([X|Xs], (X, Ys)) :-
        list_to_and(Xs, Ys).
                