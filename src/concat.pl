
:- module(concat, [concat/3,
                   list_len/2,
                   single/1,
                   null/1,
                   not_null/1
                  
                   ]).

:- use_module(library(clpq)).
:- use_module(library(clpfd)).

%% ----------------------------------------------------------------------
%%   concat/3

concat(X, Y, Z) :-
        list_len(X, NX),
        list_len(Y, NY),
        list_len(Z, NZ),
        NZ #= NX + NY,
        % {NZ = NX + NY},
        when(nonvar(X),
              concat_1(X, Y, Z)
             ),
        when(nonvar(Y),
              concat_2(X, Y, Z)
            ).

dump_vars(Xs, Ns, Cons) :-
        writeln(a), 
        remove_bound(Xs, Ns, Xs1, Ns1),
        writeln(b),
        writeln(dump(Xs1, Ns1, Cons)),
        dump(Xs1, Ns1, Cons),
        writeln(c).

remove_bound([], [], [], []).
remove_bound([V|Vs], [N|Ns], Vs1, Ns1) :-
        (var(V) ->
         Vs1 = [V|Vs2],
         Ns1 = [N|Ns2]
        ;
         Vs1 = Vs2,
         Ns1 = Ns2
        ),
        remove_bound(Vs, Ns, Vs2, Ns2).


concat_1([], Y, Z) :- !, Y = Z.
concat_1([X|Xs], Y, Z) :- !,
        when(nonvar(Xs),
             concat_1(Xs, Y, Zs0)),
        Z = [X|Zs0].

concat_2(X, [], Z) :- !,
        X = Z.
concat_2(Xs, Ys, Zs) :-
        is_list(Ys),  
        is_list(Zs),
        !,  
        append(Xs, Ys, Zs).
concat_2(Xs, Ys, Zs) :-
        is_list(Ys),
        !.
concat_2(Xs, Ys, Zs) :-
        last_var(Ys, V),
        when(nonvar(V),
             concat_2(Xs, Ys, Zs)).

last_var(L, V) :- var(L), !, V = L.
last_var([_|Rest], V) :- last_var(Rest, V).


%% ----------------------------------------------------------------------
%%    list_len/2

list_len(List, N) :-
        nonvar(N),
        N = 0,
        !,
        List = [].
list_len(List, N) :-
        var(List),
        !,
        (get_attr(List, len, N) ->
         true
        ;
         N #>= 0,
         % {N >= 0},
         put_attr(List, len, N)
        ).
list_len([], 0).
list_len([_|Xs], N) :-
        list_len(Xs, N0),
        N #= N0 + 1.
        % {N = N0 + 1}.

len:attr_unify_hook(N, Y) :-
        list_len(Y, M),
        N = M.
        
        
%% single/1
single(X) :- list_len(X, 1).

%% null/1
null(X) :- list_len(X, 0).

%% not_null/1
not_null(X) :-
        N #>= 1,
        % {N >= 1},
        list_len(X, N).

