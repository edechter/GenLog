

:- module(cpll, [op(950, xfy, $),
                sublist:attr_unify_hook/2
                 ]).

/*
  A sublist constraint is a term of the form cpll(List, I, J) where I and
  J are cpl(fd) integers and List is a variable, a partial list, or a
  list.

  If cpll variable X is subject to the sublist constraint cpll(Y, I, J)
  then X = [Y[I],..., Y[J]].

  A cpll expression E is either:
  1) a variable
  2) a list
  3) E_1 + E_2.

  A cpll constraint C is of the following:
  X $= E.

  The cpll constraint X $= E acts as follows: 1) If X is a
  non-attributed variable, provide X with a trivial sublist constraint
  attribute and unify X with E. 2) If X is a attributed, unify with
  E. 3) Create a cpll variable and unify with both X and E. 

  Unification: To unify sublist constraint C = cpll(Y, I, J) with Y:
  1) If Y has sublist constraint C1, merge sublist constraints C and C1.
  2) If Y is a variable without a sublist constraint, provide with trivial constraint and repeat.
  3) If Y is a list Xs, create attributed variable encoding this list and unify.
  4) Otherwise, false.

  Supposer I have Z = ABC, Y = aB, Z = abadf --> ABC = abadf
  A = aB --> abadf = aBBC --> BBC = badf;

  If I have A = BC and A = abcd --> BC = abcd
  C = e --> Bc = abcd --> fail.

  Rules:
  U(X, Y) --> X = Y.
  U(x, y) --> x = y.
  U(xE1, yE2) --> x = y, U(E1, E2).
  U([a|E1], [a|E2]) --> U(E1, E2).
  U(X Y, U V).
  U(X, x) --> X = x.
  U(x, X) --> X = x.
  U(x X, U Y) --> U = x, U(X, Y) ; U(U, x Z), U(X, Z Y).
  U(X, Y) --> U(Y, X).
  
  U(XY, abc)
  --> U(a b c, X Y)
  1) --> U(a, X), U(b c, Y)
  2) --> U(a Z, X), (b c, Z Y)
     1) --> U(b, Z), Y(c, Y)
     2) --> U(b G, Z), U(c, G Y).
        1) --> U(c, G), Y = []
        2) --> U(c, Y), G = [].

  Cpll constraint cpll([el(X), 
  
  
  
  
*/

:- op(950, xfy, $).

E1 $ E2 :-
        (get_attr(E1, sublist, _) ->
         E1 = E2
        ;
         var(E1) ->
         mk_cpll_var(E1),
         E1 = E2
        ;
         mk_cpll_var(V),
         V = E1,
         V = E2
         ).

mk_cpll_var(V) :-
        mk_cpll_var(V, _, _, _).

mk_cpll_var(V, L, I, J) :-
        must_be(var, V),
        I #>= 0,
        J #>= J,
        put_attr(V, sublist, cpll(L, I, J)).

reduce(C, C1) :-
        (
         I #==> 0 ->
         L = [_|L1],
         reduce(cpll(L1, I, J), C1)
        ;
         \+ fd_sup(J, Sup) #> 

solve_sublist_constraint(cpll(L1, I1, J1), cpll(L2, I2, J2), cpll(L3, I3, J3)) :-
        (
         var(L1) ->
         L3 = L2, I3 = I2, J3 = J2
        ; I1 #= J1 -> 
        ;
         L1 = [] ->
         I2 = J2,
         L3 = [], I3 = 0, J3 = 0
        ;
         (L1 = [A|R1], L2 = [A|R2]) ->
         mk_cpll_var(V1, R1, I1-1, J1-1),
         mk_cpll_var(V2, R2, I2-1, J2-1),
         V1 = V2
        ;
         (L1 = [A|R1], L2 = [B|R2], B \= A) ->
         I1 #\= 0, I2 #\=0,
         mk_cpll_var(V1, R1, I1-1, J1-1),
         mk_cpll_var(V2, R2, I2-1, J2-1),
         V1 = V2
        ).

:- multifile sublist:attr_unify_hook/2.
sublist:attr_unify_hook(C, X) :-
        (get_attr(X, sublist, C1) ->
         solve_sublist_constraint(C, C1, C2),
         put_attr(X, sublist, C2)
        ;
         var(X) ->
         put_attr(X, sublist, C)
        ;
         X = [] ->
         C = cpll(L, I, J),
         I = J
        ;
         X = [A|X1] ->
         mk_cpll_var(V, X, 0, J1),
         J1 #> 1,
         V = X
        ).
       
         

         
         
         

         
         
         
         
         
         
         
                          
                          
                          
