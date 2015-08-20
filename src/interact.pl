

:- module(interact, [say/1,
                     say/2,
                     ask/1,
                     ask/2,
                     explain/3]).

:- use_module(prove).
:- use_module(pprint).
:- use_module(dgraph).
:- use_module(learn).
:- use_module(library(assoc)).


ask(Goal) :-
        ask(Goal, []).

ask(Goal, Options) :-
        prove_all(Goal, Results, _, Options),
        summarize(Results).

say(Goal) :-
        say(Goal, []).

say(Goal, Options) :-
        run_batch_vbem([Goal], Options),
        !.

explain(Goal, NumExplanations, Options) :-
        prove_all(Goal, Results, _, Options),
        hprolog:take(NumExplanations, Results, Results1),
        forall(member(deriv(_, DGraph, _), Results1),
               (pprint_dgraph(DGraph),
                nl
               )).
        % Results = [R|_],
        % R = deriv(_, DGraph, _),
        % pprint_dgraph(DGraph),
        % nl.
        



summarize(Results) :-
        summarize(Results, GoalAssoc),
        assoc_to_list(GoalAssoc, GPs),
        pairs_keys_values(GPs, Gs, Ps),
        pairs_keys_values(PGs, Ps, Gs),
        keysort(PGs, PGs1),
        reverse(PGs1, PGs2), 
        forall(member(LogProb-Goal, PGs2),
               format('~w: ~w\n', [Goal, LogProb])).
              
        

summarize(Results, GoalProbPairs) :-
        empty_assoc(Empty),
        summarize(Results, Empty, GoalProbPairs).

summarize([], AssocIn, AssocIn).
summarize([Deriv|Ds], AssocIn, AssocOut) :-
        Deriv = deriv(Goal, _, LogProb),
        (get_assoc(Goal, AssocIn, OldLogProb, AssocTmp, NewLogProb) ->
         NewLogProb is OldLogProb + LogProb
        ;
         put_assoc(Goal, AssocIn, LogProb, AssocTmp)
        ),
        summarize(Ds, AssocTmp, AssocOut).
