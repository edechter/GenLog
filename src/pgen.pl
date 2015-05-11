%% Prolog generator.

/* A generator is of the form

   

   Calling Gen on an additional arugment returns the rest of the
   stream once the First element has been removed in that argument. 

*/

:- module(pgen,
          [yield/3,
           list_to_gen/2,
           list_to_random_choice/2,
           list_to_categorical/2]
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
        
        
%% list_to_categorical(List, Gen)
%%
%% List is list of pairs K-W, where W is the weight associated with
%% item K. Gen generates samples of K's proportional to their weights.
list_to_categorical(List, Gen) :-
        list_to_cdf(List, Cdf), 
        Gen =.. [cdf_to_categorical_, Cdf].

cdf_to_categorical_(Cdf, X, Gen) :-
        categorical(Cdf, X),
        Gen =.. [cdf_to_categorical_, Cdf].

categorical(Cdf, X) :-
        random(P),
        categorical_(Cdf, P, X).

categorical_([], _, _) :-
        throw(error(evaluation_error, context(categorical_/3, 'Empty list'))).
categorical_([K-V|KVs], P, X) :-
        (P =< V ->
         X = K
        ;
         categorical_(KVs, P, X)
        ).

list_to_cdf([], _) :-
        !,
        throw(domain_error(list(float), [])).
list_to_cdf(KVs, Cdf) :-
        pairs_keys_values(KVs, Ks, Vs),
        normalize(Vs, Ps),
        pairs_keys_values(KPs, Ks, Ps),
        normalized_to_cdf(KPs, Cdf).

normalized_to_cdf(KVs, Cdf) :-
        normalized_to_cdf(0, KVs, Cdf).
        
normalized_to_cdf(_, [], []) :- !.
normalized_to_cdf(W, [K-V|KVs], [K-V1|Cdf]) :-
        W1 is W + V,
        V1 = W1,
        normalized_to_cdf(W1, KVs, Cdf).
        
        
normalize(Vs, Ws) :-
        sum_list(Vs, Z),
        div_list(Z, Vs, Ws).

div_list(_, [], []) :- !.
div_list(Z, [V|Vs], [W|Ws]) :-
        W is V / Z,
        div_list(Z, Vs, Ws).
        

