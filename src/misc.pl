

:- module(misc, [
                 append1d/3,
                 appendd/3,
                 
                 take/3,
                 replicate/3,
                 print_current_frame/0,

                 and_to_list/2,
                 list_to_and/2,
                 call1/1,
                 calld/2,

                 load_random_seed/0,
                 load_random_seed/1

                 ]).


:- use_module(library(clpfd)).
:- use_module(library(chr)).
:- use_module(library(error)).
:- use_module(concat).

%% ----------------------------------------------------------------------
%%     append1d(X, Y, Z)
%%
%% Z is the concatentation of X and Y where X and Y are non-empty
%% lists. Predicate will delay until X is non-var or Y and Z are
%% non-var.
%% 
append1d(X, Y, Z) :-
        X = [_|_],
        Y = [_|_],
        % not_null(X),
        % not_null(Y),
        appendd(X, Y, Z).

appendd(X, Y, Z) :- concat(X, Y, Z).
% appendd(X, Y, Z) :- append(X, Y, Z).



%% ----------------------------------------------------------------------
%%     Auxiliary

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


:- begin_tests(replicate).

test(replicate, [true(Xs = [a,a,a,a,a])]) :-
        replicate(5, [a], Xs).

test(replicate_empty, [true(Xs = [])]) :-
        replicate(5, [], Xs).

:- end_tests(replicate).

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
        F \= '[|]',
        !,
        call(G).


calld(G, Delayed) :-
        calld(G, [], Delayed).
calld([], DelayedIn, DelayedOut) :-
        !,
        DelayedIn = DelayedOut.
calld(G, DelayedIn, DelayedOut) :-
        functor(G, F, _),
        F \= '[|]',
        !,
        calld([G], DelayedIn, DelayedOut).
calld([G|Goals], DelayedIn, DelayedOut) :-
        % trace,
        % writeln(calld([G|Goals], Delayed)),
        catch((
               (ready(G) ->
                calld_(G, BodyList),
                append([DelayedIn, BodyList, Goals], Goals1),
                calld(Goals1, [], DelayedOut)
               ;
                calld(Goals, [G|DelayedIn], DelayedOut)
               )),
              cut,
              fail).

calld_(!, []) :-
        !,
        (true
        ;
         throw(cut)).
calld_(G, BodyList) :-
        G = (_, _),
        and_to_list(G, BodyList).
calld_(G, BodyList) :-
        delayed(G), 
        G \= (_, _),
        clause(G, Body),
        and_to_list(Body, BodyList).
calld_(G, []) :-
        G \= (_, _),
        % predicate_property(G, built_in),
        !,
        call(G).

%% ----------------------------------------------------------------------
%%     and_to_list(+Conjunction, -List)
%%
%% Convert conjunction of goals to a list of goals non-recursively.
and_to_list(true, []).
and_to_list(C, [C]) :-
        C \= true,
        C \= (_,_).
and_to_list((true, Cs), Xs) :-
        and_to_list(Cs, Xs).
and_to_list((C, Cs), [C|Xs]) :-
        C \= true,
        and_to_list(Cs, Xs).

%% ----------------------------------------------------------------------
%%     list_to_and(+List, Conjunction)
list_to_and([], true).
list_to_and([X], X).
list_to_and([X|Xs], (X, Ys)) :-
        list_to_and(Xs, Ys).


%% ----------------------------------------------------------------------
%%     load_random_seed/0
%%     load_random_seed/1

'$random_seed_file_default'('./.swipl_random_seed').

load_random_seed :-
        '$random_seed_file_default'(File),
        load_random_seed(File).

load_random_seed(File) :-
        open(File, read, S),
        read(S, T),
        T = seed(Rand),
        set_random(seed(Rand)).

        
