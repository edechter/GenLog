
:- module(concat, [concat/3,
                   list_len/2,
                   single/1,
                   null/1,
                   not_null/1,
                   sublists_constraint/2
                  
                   ]).

:- use_module(library(clpfd)).

%% ----------------------------------------------------------------------
%%   concat/3

concat(X, Y, Z) :-
        % sublists_constraint([X, Y], Z),
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


% concat_1(X, Y, Z) is called when the first argument X is nonvar. 
concat_1([], Y, Z) :- !, Y = Z.
concat_1([X|Xs], Y, Z) :- !,
        % sublists_constraint([Xs, Y], Zs0),
        Z = [X|Zs0],
        when(nonvar(Xs),
             concat_1(Xs, Y, Zs0)).

% concat_2(X, Y, Z) is called when the second argument Y is nonvar.
concat_2(X, [], Z) :- !,
        X = Z.
concat_2(Xs, Ys, Zs) :-
        is_list(Zs), 
        !,  
        append(Xs, Ys, Zs).
concat_2(Xs, Ys, Zs) :-
        % if Ys is a list and Zs is not, don't do anything. 
        is_list(Ys),
        !.
concat_2(Xs, Ys, Zs) :-
        true.

last_var(L, V) :- var(L), !, V = L.
last_var([_|Rest], V) :- last_var(Rest, V).




%% ----------------------------------------------------------------------
%%    list_len/2

list_len(List, N) :-
        nonvar(N),
        !,
        list_len_concrete(List, N).
list_len(List, N) :-
        var(List),
        !,
        (get_attr(List, len, N) ->
         (nonvar(N) ->
          list_len_concrete(List, N)
         ;
          true)
        ;
         N #>= 0,
         put_attr(List, len, N)
        ).
list_len([], 0).
list_len([_|Xs], N) :-
        list_len(Xs, N0),
        N #= N0 + 1.

len:attr_unify_hook(N, Y) :-
        list_len(Y, M),
        N = M.

list_len_concrete(List, N) :-
        must_be(integer, N),
        list_len_concrete_(List, N).
list_len_concrete_([], 0).
list_len_concrete_([X|Xs], N) :-
        N #> 0,
        N1 #= N - 1, 
        list_len_concrete_(Xs, N1).
        
%% single/1
single(X) :- list_len(X, 1).

%% null/1
null(X) :- list_len(X, 0).

%% not_null/1
not_null(X) :-
        N #>= 1,
        list_len(X, N).

%% sublists_constraint/2
sublists_constraint([], Y) :-
        list_len(Y, 0).
sublists_constraint([X|Xs], Y) :-
        list_len(Y, M),
        list_len(X, N),
        list_len(Y0, M0),
        sublists_constraint(Xs, Y0),
        M #= M0 + N.
        