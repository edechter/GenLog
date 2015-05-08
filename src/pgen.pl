%% Prolog generator.

/* A generator is of the form

   

   Calling Gen on an additional arugment returns the rest of the
   stream once the First element has been removed in that argument. 

*/

:- module(pgen,
          [yield/3,
           list_to_gen/2,
           list_to_random_choice/2]
           ).

:- use_module(library(random)).

yield(Gen, Next, Gen1) :-
        call(Gen, Next, Gen1).

list_to_gen(List, Gen) :-
        Gen =.. [list_gen, List].

list_gen([X|Xs], X, Gen) :-
        Gen =.. [list_gen, Xs].



%% list_to_random_choice(List, Gen)
list_to_random_choice(List, Gen) :-
        Gen =.. [list_to_rc_, List].

list_to_rc_(List, X, Gen) :-
        random_member(X, List),
        Gen =.. [list_to_rc_, List].
        
        


