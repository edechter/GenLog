%% Prolog generator.

/* A generator is of the form

   

   Calling Gen on an additional arugment returns the rest of the
   stream once the First element has been removed in that argument. 

*/

:- module(pgen,
          [yield/3,
           yield/4,
           enum/2,
           list_to_gen/2,
           list_to_circular/2,
           list_to_random_choice/2,
           list_to_categorical/2]
           ).

:- use_module(library(random)).

%% yield(Gen, Number, Elements, NewGen)
yield(Gen, 0, [], Gen) :- !.
yield(Gen, N, [X|Xs], Gen2) :-
        yield(Gen, X, Gen1),
        N1 is N - 1,
        yield(Gen1, N1, Xs, Gen2).

yield(Gen, Next, Gen1) :-
        call(Gen, Next, Gen1).

%% list_to_gen(List, Gen)
list_to_gen(List, Gen) :-
        Gen =.. [list_gen, List].

list_gen([X|Xs], X, Gen) :-
        Gen =.. [list_gen, Xs].

%% list_to_circular(List, Gen)
list_to_circular(List, Gen) :-
        Gen =.. [list_to_circular_, List].

list_to_circular_([X|Xs], X, Gen) :-
        shift_left_([X|Xs], Ys),
        Gen =.. [list_to_circular_, Ys].

shift_left_([], []).
shift_left_([X|Xs], Ys) :-
        append(Xs, [X], Ys). 



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
        % if List is a list of pairs
        List = [_-_|_],
        !, 
        list_to_cdf(List, Cdf), 
        Gen =.. [cdf_to_categorical_, Cdf].
list_to_categorical(List, Gen) :-
        % if List is unweighted, i.e., not a list of pairs
        list_to_random_choice(List, Gen).

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
        

%% enum(X, IX) :-
enum(X, IX) :-
        enum(X, 1, [], IX).
enum([], _, Acc, IX) :-
        !, Acc = IX.
enum([X|Xs], I, Acc, IX) :-
        I1 is I + 1,
        enum(Xs, I1, [I-X|Acc], IX).
        