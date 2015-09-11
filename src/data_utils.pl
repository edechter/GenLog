%% data_utils.pl
%% author: Eyal Dechter
%%
%% This module provides convenience predicates for processing data
%% into SDCL goals.

:- module(data_utils,
          [atom_split_list/2,
           sentence_data_set/3,
           normalize/2,
           items_counts/2 % +Items, ?ItemCountPairs
           ]).

:- use_module(library(assoc)).

%% ----------------------------------------------------------------------
%%      atom_split_list(?Atom, ?List)
%%
%%      - Atom: a sentence as an atom, with spaces separating words.
%%      - List: a list of atoms.
%%
%%      Deterministic if either of the arguments is instantiated;
%%      error otherwise.
atom_split_list(Atom, List) :-
        atomic_list_concat(List, ' ', Atom).


%% ----------------------------------------------------------------------
%%      sentence_data_set(+StartPredicate, +Sentences, -DataSet) is det
%%
%%      - StartPredicate: predicate corresponding to the start symbol.
%%      - Sentences: A list of sentences, with words separated by a
%%      space, or a pair of a sentence and a count.
%%      - DataSet: A list of prolog goals corresponding to each sentence.
%%
%%      Example:
%%      ?- sentence_data_set(s, ['my dog hates cats', 'does your dog hate cats ?'], Goals).
%% 
%%      Goals = [s([my, dog, hates, cats], []), s([does, your, dog, hate, cats, ?], [])]
sentence_data_set(_, [], []) :- !.
sentence_data_set(StartPredicate, [S-C|Ss], [G|Gs]) :- !,
        atom_split_list(S, Xs),
        G0 =.. [StartPredicate, Xs, []],
        G = count(G0, C),
        sentence_data_set(StartPredicate, Ss, Gs).
sentence_data_set(StartPredicate, [S|Ss], Gs) :-
        sentence_data_set(StartPredicate, [S-1|Ss], Gs).




%% ----------------------------------------------------------------------
%%    normalize(+KeyValues, ?KeyValuesNormalized)
normalize([], []) :- !.
normalize(KVs,KVs1) :-
        pairs_keys_values(KVs, Ks, Vs),
        sum_list(Vs, Z),
        findall(V1,
                (member(V, Vs),
                 V1 is V / Z),
                Vs1),
        pairs_keys_values(KVs1, Ks, Vs1).

 :- begin_tests(data_utils).

test(sentence_data_set,
     [true(Gs==[count(s([my, dog, hates, cats], []), 1), count(s([does, your, dog, hate, cats, ?], []), 1)])]) :-
        sentence_data_set(s, ['my dog hates cats', 'does your dog hate cats ?'], Gs).

test(normalize,
     [true(KVs==[a-0.4, b-0.2, c-0.4])]) :-
        normalize([a-4, b-2, c-4], KVs).

test(normalize_empty,
     [true(KVs==[])]) :-
        normalize([], KVs).

:- end_tests(data_utils).

%% ----------------------------------------------------------------------
%%     items_counts(+Items, ?ItemCountPairs)
%%
%% Each element sof ItemCountPairs is Item-Count where Count is the
%% number of times Item appears in list Item. Terms are compared using
%% (==)/2.

items_counts(Items, ItemCountPairs) :-
        must_be(list, Items),
        empty_assoc(Empty), 
        items_counts_loop(Items, Empty, ItemCountAssoc),
        assoc_to_list(ItemCountAssoc, ItemCountPairs).
        
items_counts_loop([], ItemsCountsIn, ItemsCountsIn).        
items_counts_loop([X|Xs], ItemsCountsIn, ItemsCountsOut) :-
        (X \= El-Count ->
         El = X,
         Count = 1
        ;
         X = El-Count
        ),
        (get_assoc(El, ItemsCountsIn, OldCount, ItemsCountsTmp, NewCount) ->
         NewCount is OldCount + Count
        ;
         put_assoc(El, ItemsCountsIn, Count, ItemsCountsTmp)
        ),
        items_counts_loop(Xs, ItemsCountsTmp, ItemsCountsOut).

:- begin_tests(items_counts).

test(items_counts_empty,
     true(ICs=[])) :-
        items_counts([], ICs).

test(items_counts_small,
     [set(El==[a-1, b-2, c-3])]) :-
        Items = [b, a, c, c, b, c],
        items_counts(Items, ItemCountPairs),
        member(El, ItemCountPairs).
     
:- end_tests(items_counts).
               
        
        




