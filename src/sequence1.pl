
 
:- module(sequence1,
          [sequence/2,
           concat/3,
           list_sequence/2,
           len/2,
           lngth/2,
           attr_unify_hook/2,
           op(1100, yfx, ++),
           op(900, yfx, $=)
          ]).

:- use_module(library(clpfd)).

:- op(1100, yfx, ++).
:- op(900, yfx, $=).

/*
A clp(s) variable is a variable with attribute start and end.

An expression is
  _list_       | given list of elements
  _variable    | unknown list of elements
  Expr ++ Expr | concatenation of lists
  

A constraint is of the form:
  Expr @= Expr
  Expr @> integer  | length of Expr is greater than N
  Expr @< integer  | length of Expr is less than N
  Expr @== integer | length of Expr is equal to N
  
*/

sequence(X, Seq) :-
        var(Seq),
        !, 
        get_attr(X, sequence, Seq).
sequence(X, List) :-
        list_sequence(List, Seq),
        put_attr(Y, sequence, Seq),
        X = Y.

list_sequence(List, Seq) :-
        % is_list(List),
        % !, 
        len(List, N),
        Seq = seq(List, N).
% list_sequence(Var, Seq) :-
        % var(Var),
        % !.
        

unify_sequences(seq(List1, N1), seq(List2, N2)) :-
        N1 #= N2,
        List1 = List2.

lngth(X, N) :-
        to_seq(X),
        get_attr(X, sequence, seq(_, N)),
        writeln(N).
                
len(Var, N) :-
        var(Var),
        !,
        N #>= 0.
len([], 0).
len([A|Rest], N1) :-
        len(Rest, N0), 
        N1 #= N0 + 1.


concat(X, Y, Z) :-
        to_seq(X),
        to_seq(Y),
        to_seq(Z),
        get_attr(X, sequence, SX),
        get_attr(Y, sequence, SY),
        get_attr(Z, sequence, SZ),
        concat1(SX, SY, SZ).
        
        
concat1(seq(L1, N1), seq(L2, N2), seq(L3, N3)) :-
        N3 #= N1 + N2,
        concat2(L1, L2, L3).

concat2(X, Y, Z) :-
        var(X),
        !.
concat2(X, Y, Z) :-
        (X = [] ->
         Z = Y
        ;
         X = [X1|Xs] ->
         concat2(Xs, Y, Zs1),
         Z = [X1|Zs]
        ).
         
         
        

X $= Y :-
        len(L, N),
        put_attr(Z, sequence, seq(L, N)),
        Y = Z.
        
        % (seq_expr(X) ->
        %  attr_unify_hook
        % ((\+ get_attr(Y, sequence, Seq)) ->
        %  len(Y,N), 
        %  put_attr(X, sequence, seq(Y, N))
        % ;
        % X = Y).

to_seq(X) :-
        get_attr(X, sequence, S),
        !.
to_seq(X) :-
        list_sequence(X, S),
        put_attr(Y, sequence, S),
        X = Y.

% X ++ Y :-
%         to_seq(X, SX),
%         to_seq(Y, SY),
%         concat(X, Y, Z).
        

                  
:- multifile attr_unify_hook/2.        

attr_unify_hook(Seq0, Y) :-
        writeln(hello),
        ( get_attr(Y, sequence, Seq1),
          unify_sequences(Seq0, Seq1)
         ;
          var(Y) ->
          writeln(Y),
          put_attr(Y, sequence, Seq0)        
        ;
          is_list(Y) ->
          list_sequence(Y, Seq1),
          unify_sequences(Seq0, Seq1)
        % ;
        %   Y = (U ++ V) ->
        %   concat(U, V, W),
        %   put_attr(Z, sequence, W),
        %   Y = Z
        ).



        
       

          
        
        
        
        