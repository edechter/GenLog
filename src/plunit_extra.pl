:- module(plunit_extra,
          [op(700, xfx, ~=),
          (~=)/2
           ]).

:- op(700, xfx, ~=).

plunit_float_tol(1e-3).

X ~= Y :-
        number(X), number(Y), !,
        plunit_float_tol(Tol),
        abs(X-Y) < Tol.

(K1-X) ~= (K2-Y) :- !,
        K1=K2,
        X ~= Y.

[] ~= [] :- !.

[X|Xs] ~= [Y|Ys] :-
        X ~= Y,
        Xs ~= Ys.