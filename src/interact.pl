

:- module(interact, [
                     % say/1,
                     % say/2,
                     ask/1,
                     ask/2,
                     ask/3,
                     explain/3]).

:- use_module(prove).
:- use_module(pprint).
:- use_module(dgraph).
% :- use_module(learn).
:- use_module(library(assoc)).


ask(Goal) :-
        ask(Goal, []).

ask(Goal, Options) :-
        ask(Goal, _, Options).

ask(Goal, Prob, Options) :- 
        prove_all(Goal, Derivations, _, Options),
        summarize(Derivations, Results),
        member(Prob-Goal, Results).


% say(Goal) :-
%         say(Goal, []).

% say(Goal, Options) :-
%         run_batch_vbem([Goal], Options),
%         !.

explain(Goal, NumExplanations, Options) :-
        prove_all(Goal, Results, _, Options),
        hprolog:take(NumExplanations, Results, Results1),
        forall(member(deriv(_, DGraph, CondP), Results1),
               (pprint_dgraph(DGraph, [show_value(prob)]),
                write(CondP),
                nl
               )).
        

%% ----------------------------------------------------------------------
%%    summarize(+Derivations)
%%    summarize(+Derivations, -ProbGoalPairs)

summarize(Derivations) :-
        summarize(Derivations, ProbGoalPairs), 
        forall(member(Prob-Goal, ProbGoalPairs),
               format('~w: ~w\n', [Goal, Prob])).

summarize(Derivations, ProbGoalPairs) :-
        summarize_(Derivations, GoalAssoc),
        assoc_to_list(GoalAssoc, GPs),
        pairs_keys_values(GPs, Gs, Ps),
        pairs_keys_values(PGs, Ps, Gs),
        keysort(PGs, PGs1),
        reverse(PGs1, ProbGoalPairs).

              
        

summarize_(Results, GoalProbAssoc) :-
        empty_assoc(Empty),
        summarize_(Results, Empty, GoalProbAssoc).

summarize_([], AssocIn, AssocIn).
summarize_([Deriv|Ds], AssocIn, AssocOut) :-
        Deriv = deriv(Goal, _, Prob),
        (get_assoc(Goal, AssocIn, OldProb, AssocTmp, NewProb) ->
         NewProb is OldProb + Prob
        ;
         put_assoc(Goal, AssocIn, Prob, AssocTmp)
        ),
        summarize_(Ds, AssocTmp, AssocOut).
