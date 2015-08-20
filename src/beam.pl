
:- module(beam,
          [beam/2,
           beam/3,
           beam_insert/3,
           beam_last/3,
           beam_list/2,
           '&<'/2,
           '&=<'/2,
           '&='/2,
           '&>'/2,
           '&>='/2
          ]).

/*
  A beam is is a maximum priority queue of finite size.

  A beam is represented by b(Size, Front, Back, MaxSize).
  MaxSize is assumed assumed to be >= 1.

  Note: the approach here is adapted from section 2.5 of O'Keefe.
  
*/

%% beam(M, Beam)
%
% is true when Beam is a beam with no elements and
% maximum size M.

beam(M, b(0, B, B, M, negative_infinity)).

%% beam(X, M, Beam, Inf)
%
% is true when Beam is a beam with a single element and maximum size
% M. Inf is the infinum of Beam. M is assumed to be >= 1.

beam(K-V, M, b(1, [K-V|B], B, M, V)).

%% beam_insert(X, Beam0, Beam1)
%
% is true when Beam1 is the result of inserting X into Beam0. If Beam0
% is saturated, and X's value is greater than Beam1's least value,
% then the worst element will be ejected.

beam_insert(V-K, b(N, F, B, M, Inf), Beam1) :-
        N = M,
        beam_insert_saturated(V-K, Vmin, b(N, F, B,M, Inf), Beam1).
beam_insert(V-K, Beam0, Beam1) :-
        beam_insert_not_saturated(V-K, Beam0, Beam1).

% beam_last(X, Beam0, Beam1)
%
% is true when Beam0 and Beam1 have the same elements except that
% Beam1 has X in addition on the right
beam_last(X, b(N, F, [X|B], M), b(s(N), F, B, M)).

beam_list(b(N, F, B, _), List) :-
        beam_list(N, F, B, List).

beam_list(0, B, B, []).
beam_list(N, [X|F], B, [X|Xs]) :-
         N = s(N1),
         beam_list(N1, F, B, Xs).


:- op(700, yfx, &<).
:- op(700, yfx, &=<).
:- op(700, yfx, &=).
:- op(700, yfx, &>).
:- op(700, yfx, &>=).

negative_infinity &< _ :- !.
_ &< negative_infinity :- !, fail.        
X &< Y :- X < Y.

negative_infinity &=< _ :- !.
_ &=< negative_infinity :- !, fail.        
X &=< Y :- X =< Y.

negative_infinity &= _ :- !, fail.
_ &= negative_infinity :- !, fail.        
X &= Y :- X = Y.

X &> Y :- Y &< X.
X &>= Y :- Y &=< X.




