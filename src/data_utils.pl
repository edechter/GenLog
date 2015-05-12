%% data_utils.pl
%% author: Eyal Dechter
%%
%% This module provides convenience predicates for processing data
%% into SDCL goals.

:- module(data_utils,
          [atom_split_list/2,
           sentence_data_set/3
           ]).

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


:- begin_tests(data_utils).

test(sentence_data_set,
     [true(Gs==[count(s([my, dog, hates, cats], []), 1), count(s([does, your, dog, hate, cats, ?], []), 1)])]) :-
        sentence_data_set(s, ['my dog hates cats', 'does your dog hate cats ?'], Gs).

:- end_tests(data_utils).






%% ----------------------------------------------------------------------
        