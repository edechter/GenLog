%% data_utils.pl
%% author: Eyal Dechter
%%
%% This module provides convenience predicates for processing data
%% into SDCL goals.

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
%%      sentence_data_set(+StartPredicate, +Sentences, -DataSet) is Det
%%
%%      - StartPredicate: predicate corresponding to the start symbol.
%%      - Sentences: A list of sentences, with words separated by a space. 
%%      - DataSet: A list of prolog goals corresponding to each sentence.
%%
%%      Example:
%%      ?- sentence_data_set(s, ['my dog hates cats', 'does your dog hate cats ?'], Goals).
%% 
%%      Goals = [s([my, dog, hates, cats], []), s([does, your, dog, hate, cats, ?], [])]
sentence_data_set(_, [], []) :- !.
sentence_data_set(StartPredicate, [S|Ss], [G|Gs]) :-
        atom_split_list(S, Xs),
        G =.. [StartPredicate, Xs, []],
        sentence_data_set(StartPredicate, Ss, Gs).


:- begin_tests(data_utils).

test(sentence_data_set,
     [true(Gs==[s([my, dog, hates, cats], []), s([does, your, dog, hate, cats, ?], [])])]) :-
        sentence_data_set(s, ['my dog hates cats', 'does your dog hate cats ?'], Gs).

:- end_tests(data_utils).






%% ----------------------------------------------------------------------
        