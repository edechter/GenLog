%% learn.pl
%% author: Eyal Dechter

:- use_module(library(real)).
:- use_module(library(assoc)).
:- use_module(library(debug)).

:- nodebug(real).

:- r(source("optimize.r")).

:- [sdcl].

%% ----------------------------------------------------------------------

variational_em_single_iteration(Goals) :-
        learn(Goals, []).

variation_em_single_iteration(Goals, Options) :- 
        expected_rule_counts(Goals, ExpectedCounts, Options),
        nl,
        pprint_num_assoc(ExpectedCounts),
        nl, 
        update_hyperparams(ExpectedCounts, constant(0.1), HyperParams),
        compute_variational_weights(HyperParams, NewWeights),
        pprint_num_assoc(NewWeights), 
        set_rule_weights(NewWeights).


%% ----------------------------------------------------------------------
%%      compute_variational_weights(+VariationalParams,
%%      VariationalWeights) is det
%%
%%      VariationalParams is an assoc of ruleIds and corresponding
%%      updated variational parameters. Pass these through a digamma
%%      and normalize by functor to get the Variational Weights. 
%%      

compute_variational_weights(VariationalParams, VariationalWeights) :-
        map_assoc(digamma_on_val, VariationalParams, VariationalWeightsNum),
        sum_rule_assoc_across_rule_groups(VariationalParams,
                                          VariationalWeightsDen),
        normalize_variational_weights(VariationalWeightsNum,
                                      VariationalWeightsDen,
                                      VariationalWeights).

normalize_variational_weights(VariationalWeightsNum, %% numerator
                              VariationalWeightsDen,  %% denominator
                              VariationalWeights) :-
        rules(RuleIds), 
        findall(RuleId-VariationalWeight,
                (
                 member(RuleId, RuleIds),
                 get_assoc(RuleId, VariationalWeightsNum, VNum),
                 sdcl_rule(RuleId, _, _, _, RuleGroup),
                 get_assoc(RuleGroup, VariationalWeightsDen, VDen),
                 VariationalWeight <- exp(VNum - digamma(VDen))
                 ),
                RVs),
        list_to_assoc(RVs, VariationalWeights).
                           

sum_rule_assoc_across_rule_groups(RuleAssoc, RuleGroupAssoc) :-
        assoc_to_list(RuleAssoc, RuleVals), 
        sum_rule_assoc_across_rule_groups_go(RuleVals, RuleGroupAssoc).

sum_rule_assoc_across_rule_groups_go(RuleVals, RuleGroupAssoc) :-
        empty_assoc(Empty),
        sum_rule_assoc_across_rule_groups_go(RuleVals, Empty, RuleGroupAssoc).

sum_rule_assoc_across_rule_groups_go([], AssocIn, AssocOut) :- !, AssocIn = AssocOut.
sum_rule_assoc_across_rule_groups_go([RuleId-Val|Rest], AssocIn, AssocOut) :-
        find_rule_by_id(RuleId, Rule),
        sdcl_rule_group(Rule, RuleGroup),
        (
         get_assoc(RuleGroup, AssocIn, V_Old, AssocTmp, V_New) -> 
         V_New is V_Old + Val
        ;
         put_assoc(RuleGroup, AssocIn, Val, AssocTmp)
        ),
        sum_rule_assoc_across_rule_groups_go(Rest, AssocTmp, AssocOut).
        
       
digamma_on_val(V0, V1) :-
        V1 <- digamma(V0), !.


:- begin_tests(variational_weights).

test(sum_rule_assoc_across_rule_groups,
     [setup(setup_sdcl('trivial_2.pl')),
      cleanup(cleanup_sdcl),
      set(Val = [1, 5])]) :-
        RuleVals = [r(1)-1,
                    r(2)-2,
                    r(3)-3],
        list_to_assoc(RuleVals, RuleAssoc),
        sum_rule_assoc_across_rule_groups(RuleAssoc, RuleGroupAssoc),
        assoc_to_list(RuleGroupAssoc, RuleGroupVals),
        member(_-Val, RuleGroupVals).        

:- end_tests(variational_weights).


        



%% ----------------------------------------------------------------------



%% ----------------------------------------------------------------------
%%      update_hyperparams(+ExpectedCounts, +Alpha0, -VariationalParams) is det
%%
%%      ExpectedCounts is an assoc of rule ids and their expected
%%      counts. Alpha0 is either constant(Val), uniform, uniform(K),
%%      or an assoc from rule ids to numbers. VariationalParams is an
%%      assoc consisting of the current variational parameters based
%%      on the expected counts.
%%

update_hyperparams(ExpectedCounts, constant(Alpha0), Alpha) :-
        !, 
        scalar_add_assoc(Alpha0, ExpectedCounts, Alpha).

%% FIXME: Implement alternative Alpha0
update_hyperparams(_, Alpha0, _) :-
        Alpha0 \= constant(_),       
        throw(not_implemented_error). 

%% ----------------------------------------------------------------------
%%      expected_rule_counts(+Goals, -Assoc, -Options)
%%
%%      Given a set of observations, return the expected rule counts
%%      for the whole set.
%%
%%      -------------
%%      Goals: A list of observations, each of which is a goal for the sdcl.
%%      A Goal is either bare goal (unifying with the head of an SDCL rule)
%%      or it is count(Goal, Count) (like in PRISM), where Count is the number
%%      of times Goal has been observed. 
%%
%%      Assoc: An assoc whose keys are rule ids and values are the
%%      expected number of times the rule is used for this set of
%%      observations.
%%
%%      Options: takes the same options as mi_best_first_all.
%%
expected_rule_counts(Goals, Assoc) :-
        expected_rule_counts(Goals, Assoc, []).

expected_rule_counts(Goals, Assoc, Options) :-
        empty_rules_assoc(Empty),
        expected_rule_counts(Goals, Empty, Assoc, Options).

empty_rules_assoc(Assoc) :-
        rules(RuleIds),
        findall(RuleId-0, member(RuleId, RuleIds), RVs),
        list_to_assoc(RVs, Assoc).

expected_rule_counts([], Assoc, Assoc, _).
expected_rule_counts([count(Goal, Count)|Goals], AssocIn, AssocOut, Options) :-
        !, 
        mi_best_first_all(Goal, Derivations, _, Options),
        findall(DGraph-W,
                member(deriv(_, DGraph, W), Derivations),
                ScoredDGraphs),
        assert(sd(ScoredDGraphs)), 
        expected_rule_counts1(ScoredDGraphs, Assoc0),
        scalar_multiply_assoc(Count, Assoc0, Assoc1),
        add_assocs(0, Assoc1, AssocIn, AssocTmp),
        expected_rule_counts(Goals, AssocTmp, AssocOut, Options).
expected_rule_counts([Goal|Goals], AssocIn, AssocOut, Options) :-
        Goal \= count(_, _), %% redundant check due to CUT above
        expected_rule_counts([count(Goal, 1)|Goals], AssocIn, AssocOut, Options).


:- begin_tests(expected_rule_counts).

test(expected_rule_counts,
     [
      setup(setup_trivial_sdcl),
      cleanup(cleanup_trivial_sdcl)
      ]) :-
       Goals = [s([a, a], [])],
       
       expected_rule_counts(Goals, Assoc, [inference_limit(1000000)]),
       assertion(get_assoc(r(1), Assoc, 1.0)),
       assertion(get_assoc(r(2), Assoc, 2.0)).

:- end_tests(expected_rule_counts).






%% ----------------------------------------------------------------------
%%      expected_rule_counts1(ScoredDerivations, Assoc)
%%
%%      Given a set of scored derivations for a single observation,
%%      return the expected rule counts for that observation. 
%%
%%      arguments:
%%
%%      ScoredDerivations: a list of pairs DGraph-Weight (the weight is
%%      normally is the probability of the derivation, but could be
%%      the conditional probability of the derivation given the
%%      corresponding goal). All derivations should be derivations of
%%      the same goal.
%%
%%      Assoc: An assoc associating each rule with its expected
%%      counts in the list of derivations. 

expected_rule_counts1(Ds, Assoc) :-
        empty_assoc(Empty), 
        expected_rule_counts1(Ds, Empty, Assoc).

expected_rule_counts1([], AssocIn, AssocIn).
expected_rule_counts1([DGraph-W|Ds], AssocIn, AssocOut) :-
        dgraph_rule_counts(DGraph, W, Assoc),
        add_assocs(0, AssocIn, Assoc, AssocTmp),
        expected_rule_counts1(Ds, AssocTmp, AssocOut).
        

dgraph_rule_counts(DGraph, W, Assoc) :-
        empty_assoc(Empty),
        DGraph=dgraph(_, _, Hs),
        dgraph_rule_counts(Hs, W, Empty, Assoc).

dgraph_rule_counts([], _, AssocIn, AssocIn). 
dgraph_rule_counts([hyperedge(_, RuleId, _)|Hs], W, AssocIn, AssocOut) :-
        (
         get_assoc(RuleId, AssocIn, C), !
        ;
         C = 0
        ),
        C1 is C + W,
        put_assoc(RuleId, AssocIn, C1, AssocTmp),
        dgraph_rule_counts(Hs, W, AssocTmp, AssocOut).


:- begin_tests(learn).

test(expected_rule_counts1,
     [set(R-C=[r(1)-15.0, r(2)-5.0])]) :-
        test_dgraph(DGraph),
        Ds = [DGraph-3.0, DGraph-2.0],
        expected_rule_counts1(Ds, Assoc),
        assoc_to_list(Assoc, Counts),
        member(R-C, Counts).
        
        

test_dgraph(dgraph(_,[goal(node_1, g1),
                      goal(node_2, g2),
                      goal(node_3, g3),
                      goal(node_4, g4),
                      goal(node_5, g5)], 
                   [
                    hyperedge(node_1, r(1), [node_2, node_3]),
                    hyperedge(node_2, r(1), [node_4]),
                    hyperedge(node_3, r(2), []),
                    hyperedge(node_4, r(1), [])
                    ])).
test(dgraph_rule_counts,
     [set(R-C=[r(1)-9.0, r(2)-3.0])]) :- 
        test_dgraph(DGraph),
        dgraph_rule_counts(DGraph, 3.0, Assoc),
        assoc_to_list(Assoc, Counts),
        member(R-C, Counts).

:- end_tests(learn).



%% ----------------------------------------------------------------------
%% Auxiliary predicates over associations with numbers as values.
%%
%% add_assocs(V, Assoc1, +Assoc2, -AssocOut)
%%
%% AssocOut is the union of Assoc1 and Assoc2 with addition and
%% default value V.
add_assocs(V0, Assoc1, Assoc2, AssocOut) :-
        assoc_to_list(Assoc1, Pairs), 
        add_assocs1(V0, Pairs, Assoc2, AssocOut).

add_assocs1(_, [], AssocIn, AssocOut) :- !, AssocIn = AssocOut.
add_assocs1(V0, [K-V|Rest], AssocIn, AssocOut) :-
        (
         get_assoc(K, AssocIn, V1), !
         ;
         V1 = V0
        ),
        V2 is V1 + V,
        put_assoc(K, AssocIn, V2, AssocTmp),
        add_assocs1(V0, Rest, AssocTmp, AssocOut).

%% scalar_multiply_assoc(+V, +AssocIn, -AssocOut)
%% multiplies each value of AssocIn by V
scalar_multiply_assoc(V, AssocIn, AssocOut) :-
        map_assoc(call(prod, V), AssocIn, AssocOut).

prod(V, A, B) :-
        B is A * V.

%% scalar_add_assoc(+V, +AssocIn, -AssocOut)
%% adds each value of AssocIn to V
scalar_add_assoc(V, AssocIn, AssocOut) :-
        map_assoc(call(sum, V), AssocIn, AssocOut).

sum(V, A, B) :-
        B is A + V.

%% map_keys(+Goal, +Assoc0, -Assoc1) is det
%% Apply Goal to every key in Assoc0. Result is Assoc1.
map_keys(Goal, Assoc0, Assoc1) :-
        assoc_to_list(Assoc0, Xs),
        empty_assoc(Empty),         
        map_keys_list(Xs, Goal, Empty, Assoc1).

map_keys_list([], _, AssocIn, AssocIn) :- !.
map_keys_list([K-V|Xs], Goal, AssocIn, AssocOut) :-
        call(Goal, K, K1),!, 
        put_assoc(K1, AssocIn, V, AssocTmp),
        map_keys_list(Xs, Goal, AssocTmp, AssocOut).
        
        



:- begin_tests(learn_aux).

test(add_assocs,
     [set(K-V = [a-3, b-4, c-9])]
     ) :-
        list_to_assoc([a-1, b-4], Assoc1),
        list_to_assoc([a-2, c-9], Assoc2),
        add_assocs(0, Assoc1, Assoc2, Assoc),
        assoc_to_list(Assoc, List),
        member(K-V, List).

test(scalar_multiply_assoc,
     [set(K-V = [a-5, b-20])]
     ) :-
        list_to_assoc([a-1, b-4], Assoc),

        scalar_multiply_assoc(5, Assoc, Assoc1),
        assoc_to_list(Assoc1, List),
        member(K-V, List).


test(scalar_add_assoc,
     [set(K-V = [a-6, b-9])]
     ) :-
        list_to_assoc([a-1, b-4], Assoc),

        scalar_add_assoc(5, Assoc, Assoc1),
        assoc_to_list(Assoc1, List),
        member(K-V, List).


:- end_tests(learn_aux).

%% ----------------------------------------------------------------------
%% Printing utilities for assocs with numbers as values

%% pprint_num_assoc(-Assoc)
pprint_num_assoc(Assoc) :- 
        assoc_to_list(Assoc, RCs),
        (
         member(R-C, RCs),
         format("~w~| ~`.t ~50|~g\n", [R, C]), 
         fail
        ;
         true
        ).


        


        
                 
                        
        



%% ----------------------------------------------------------------------



