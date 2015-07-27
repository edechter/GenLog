
:- module(array,
          [list_array/2,
           is_array/1,
           array/2,
           array/3,
           array_like/2,
           get/3,
           gets/3,
           set/3,
           sets/2,
           add_at/3,
           add_at/2,
           map_array/3,
           add_arrays/3,
           scalar_multiply_array/3
          ]).

is_array(X) :-
        functor(X, '$array', _).

list_array(Xs, Array) :-
        Array =.. ['$array'|Xs].

array(Size, Array) :-
        functor(Array, '$array', Size).

array(Size, Array, X) :-
        array(Size, Array),
        fill(Array, X).

fill(Array, X) :-
        array(Size, Array),
        forall(between(1, Size, I),
               set(I, Array, X)).
        

array_like(ArrayTemp, Array) :-
        array(Size, ArrayTemp),
        array(Size, Array).

get(N, Array, X) :-
        arg(N, Array, X).

set(N, Array, X) :-
        nb_setarg(N, Array, X).

gets([], _Array, []).
gets([N|Ns], Array, [X|Xs]) :-
        get(N, Array, X),
        gets(Ns, Array, Xs).

sets([], _Array).
sets([X|Xs], Array) :-
        set(X, Array),
        sets(Xs, Array).

add_at(N, Array, V) :-
        get(N, Array, X),
        X1 is X + V,
        set(N, Array, X1).

add_at(NVs, Array) :-
        forall( member(N-V, NVs),
                add_at(N, Array, V)).

add_arrays(Array1, Array2, Array) :-
         array(N, Array1),
         array(N, Array2),
         array(N, Array),
         forall(between(1, N, I),
                (get(I, Array1, A),
                 get(I, Array2, B),
                 C is A + B,
                 set(I, Array, C))).

scalar_multiply_array(C, Array, Array1) :-
        array(N, Array),
        array(N, Array1),
        forall(between(1, N, I),
               (get(I, Array, X),
                Y is X * C,
                set(I, Array1, Y))).
        


:- meta_predicate
        map_array(1, ?, ?).

map_array(F, In, Out) :-
       array(Size, In),
       array(Size, Out),
       forall(between(1, Size, I),
              (get(I, In, XI),
               call(F, XI, YI),
               set(I, Out, YI))).

:- multifile
        user:portray/1.

nshow(5).
user:portray(Arr) :-
        functor(Arr, '$array', _),
        !,
        nshow(K), 
        Arr =.. [_|Xs],
        write('arr('), 
        portray_arr(Xs, K),
        write(')').

portray_arr([], _) :- !.
portray_arr([_|_], 0) :-
        write('...').
portray_arr([X|Xs], N) :-
        format('~p,', X),
        N1 is N - 1,
        portray_arr(Xs, N1).
        
        
    

        