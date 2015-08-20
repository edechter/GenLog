

:- module(sequence,
          [concat/3,
           concats/2,
           len/2,
           null/1,
           eq_list/2,
           index/3,

           op(700, xfy, ($=)),
           op(700, xfy, (><))
           
          ]).

:- use_module(library(chr)).
:- use_module(library(clpfd)).
:- use_module(int_reasoner).

/*
  A node is either a node2(A, A) or a node3(a,a,a)
  A digit is a list [A]
  A sequence S is either empty, single(A), or deep(Digit, S, Digit)
  */

     

:- chr_constraint concat/3.
:- chr_constraint len/2.
:- chr_constraint null/1.
:- chr_constraint singleton/1.
:- chr_constraint not_null/1.
:- chr_constraint eq_list/2.
:- chr_constraint eq_length/3.
:- chr_constraint index/3.
:- chr_constraint ($=)/2.





list_to_seq(X, I, Rest) :-
  var(Rest),
  !, 
  J geq I,
  len(X, J),
  Y $= Rest,
  concat(_, Y, X).
list_to_seq(X, I, [A|As]) :-
  index(X, I, A),
  I1 geq I + 1,
  list_to_seq(X, I1,As).
list_to_seq(X, I, []) :-
  I0 eq I - 1, 
  len(X, I0).

null(X), not_null(X) <=> false.

%% ----------------------------------------------------------------------
%% len/2
len([], N) <=> N = 0.
len([_|Xs], N) <=> N1 eq N - 1, len(Xs, N1).

len(_, N)      ==>  N geq 0.
len(X, N) \ len(X, N) <=> true.
len(X, N) \ len(X, N2) <=> N = N2.

%% ----------------------------------------------------------------------
%% index/3
index(X, N, V) \ index(X, N, V1) <=> V1 = V2.
index(X, I, _) ==> len(X, N), I leq N.
index(X, I, _) ==> I ge 0.

%% ----------------------------------------------------------------------

% Z = XY and X = null --> Z = Y
null(X) \ concat(X, Y, Z) <=> Y = Z.

% Z = XY and Y = null --> Z = X                            
null(Y) \ concat(X, Y, Z) <=> X = Z.


% Z = XY then |X|+|Y| = Z
concat(X, Y, Z) ==>
   eq_length(X, Y, Z).

index(X, I, V), concat(X, _, Z) ==>
  index(Z, I, V).

index(Y, I, V), concat(X, Y, Z), len(X, NX) ==>
  I1 eq I + NX
  |
  index(Z, I1, V).

index(Z, I, V), concat(X, Y, Z), len(X, NX) ==>
  fd_size(NX, 1),
  I leq NX
  |
  index(X, I, V).

% index(Z, I, V), concat(X, Y, Z), len(X, NX) ==>
%   fd_size(NX, 1),
%   (?? I ge NX),
%   (?? I eq I1 + NX)
%   |
%   writeln(v-index(Y, I1, V)),
%   index(Y, I1, V).


%% ----------------------------------------------------------------------
%% concats/2
concats([], X) :- X = [].
concats([X |Xs], Y) :-
   concats(Xs, Y0),
   concat(X, Y0, Y).
  

%% ----------------------------------------------------------------------
%% eq_length/3: |X| + |Y| = |Z|

eq_length(X, Y, Z)  <=>
   len(X, NX),
   len(Y, NY),
   len(Z, NZ),
   NZ eq NY + NX.
   
   
%% eq_list/2: concat list of seqs to get output seq
eq_list([], Z) <=> null(Z).
eq_list([X|Xs], Z) <=>
   eq_list(Xs, Zs1),
   concat(X, Zs1, Z).



%% ----------------------------------------------------------------------
%% null/1
null(Z) ==> len(Z, 0).

%% ----------------------------------------------------------------------
%% singleton/1
singleton(Z) ==> len(Z, 1).


nth1s([], _).
nth1s([I-V|IVs], L) :-
        nth1(I, L, V),
        nth1s(IVs, L).

/*
  Z $= [A] >< X >< Y >< [B]
  
*/

Z $= A :-
        var(Z), 
        var(A),
        !,
        Z = A.
Z $= A >< B :-
        var(Z),
        !,
        X $= A,
        Y $= B,
        concat(X, Y, Z).
Z $= [] :-
        !,
        null(Z).
Z $= L :-
        is_list(L),
        !,
        length(L, N),
        len(Z, N),
        set_from_list(1, L, Z).

set_from_list(_, [], _).
set_from_list(I, [V|Vs], Z) :-
        index(Z, I, V),
        I1 is I + 1,
        set_from_list(I1, Vs, Z).


%% ----------------------------------------------------------------------
%%  seq_to_list(A, L) enumerates lists L compatible with A on
%%  backtracking. This may be an infinite set.

seq_to_list(A, L) :-
        findall(I-V,
                find_chr_constraint(index(A, I, V)),
                IVs),
        pairs_keys(IVs, Is),
        len(A, N),
        label_semi_bound([N|Is]),
        length(L, N),
        nth1s(IVs, L).

seq_indomain(A) :-
        findall(I-V,
                find_chr_constraint(index(A, I, V)),
                IVs),
        pairs_keys(IVs, Is),
        len(A, N),
        label_semi_bound([N|Is]).


%% ----------------------------------------------------------------------
%%    label_semi_bound(+Var)
%%    label_semi_bound(+Vars)
%%    label_semi_bound(+Integer)
%%
%% In the case of a single variable or integer, like clpfd:indomain/1.
%% In the case of a a list of variables, like clpfd:label/1.  The
%% difference from the clpfd implementation is that this labeling does
%% not require a finite domain: only a single bound must be
%% bound. Therefore, this enumeration of values does not necessarily
%% terminate. This is useful in this module because the length of
%% sequences is always lower bounded by 0..

label_semi_bound(V) :-
        var(V),
        !,
        fd_dom(V, D),
        label_semi_bound_dom(D, V).
label_semi_bound(V) :-
        integer(V),
        !.
label_semi_bound([]) :- !.
label_semi_bound([V|Vs]) :-
        !,
        label_semi_bound(V),
        label_semi_bound(Vs).        

label_semi_bound_dom(D, V) :- 
        
        (integer(D) ->
         V = D
        ;
         D = D1 \/ D2 ->
         (label_semi_bound_dom(D1, V)
         ;
         label_semi_bound_dom(D2, V))
        ;
         (D = inf .. Sup, integer(Sup)) ->
         down_from(Sup, V)
        ;
         (D = Inf .. sup, integer(Inf)) ->
         up_from(Inf, V)
        ;
         D = inf .. sup,
         instantiation_error(D)
        ;
         D = Lo..Hi,
         between(Lo, Hi, V)
        ).

down_from(K, V) :-
        V = K
        ;
        K1 is K - 1,
        down_from(K1, V).

up_from(K, V) :-
        V = K
        ;
        K1 is K + 1,
        up_from(K1, V).
        

:- op(600, fx, ??).
(?? X) :- format("DEBUG: ~w\v", [X]).