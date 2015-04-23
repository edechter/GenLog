%% learn.pl
%% author: Eyal Dechter

:- use_module(library(real)).
:- use_module(library(assoc)).
:- use_module(library(debug)).

:- nodebug(real).

:- r(source("optimize.r")).

:- [sdcl].


%% ----------------------------------------------------------------------
%%      run_batch_vbem(+Goals)
%%      run_batch_vbem(+Goals, +Options) is det
%%
%%      Run the batch vbem algorithm for Goals. See the vbem_options
%%      record below for relevant options and their defaults. Any
%%      options for mi_best_first/4 are also applicable here, and the
%%      defaults are the same.
run_batch_vbem(Goals) :-
        run_batch_vbem(Goals, []).

run_batch_vbem(Goals, Options) :-
        run_batch_vbem(Goals, 1, Options).

run_batch_vbem(Goals, Iter, Options) :-
        debug(learning, "Batch VBEM: Iter ~w...\n", [Iter]),
        time(
             variational_em_single_iteration(Goals, Options),
             CPU_time,
             _Wall_time), 
        debug(learning, "Batch VBEM: Iter ~w complete: ~2f s \n", [Iter, CPU_time]),

        make_vbem_options(Options, OptRecord, _),
        vbem_options_max_iter(OptRecord, MaxIter), 
        (Iter >= MaxIter ->
         debug(learning, "Batch VBEM: Finished.\n", [])
        ;
         Iter1 is Iter + 1,
         run_batch_vbem(Goals, Iter1, Options)
        ).
    
        
%% options and defaults for vbem
:- record vbem_options(max_iter = 1000, % maximum number of iterations to run vbem
                       epsilon  = 1e-3, % stop when improvement is in
                                        % variational lower bound is less than epsilon
                       
                       % how to initialize the hyperparameters
                       % normal(+Mean, +StdDev) samples randomly from a
                       % normal distribution with Mean and StdDev provided.   
                       init_params = normal(0.1, 0.001)
                      ).

%% ----------------------------------------------------------------------

%% ----------------------------------------------------------------------
%%      variational_em_single_iteration(+Goals)
%%      variational_em_single_iteration(+Goals, +Options)
%%
%%      Execute a single iteration of Variational EM on the list of
%%      Goals. See mi_best_first/4 for a list of Options. Updates the
%%      global rules weights with new multinomial weights. 
variational_em_single_iteration(Goals) :-
        variational_em_single_iteration(Goals, []).

variational_em_single_iteration(Goals, Options) :-
        prove_goals(Goals, DSearchResults), 
        expected_rule_counts(DSearchResults, ExpectedCounts, Options),
        debug_expected_rule_counts(ExpectedCounts, Msg1),
        debug(learning,Msg1, []),
        update_hyperparams(ExpectedCounts, constant(0.1), HyperParams),
        compute_variational_weights(HyperParams, NewWeights),
        debug_new_rule_weights(NewWeights, Msg2),
        debug(learning, Msg2, []), 
        set_rule_weights(NewWeights).

% auxiliary debugging messages
debug_expected_rule_counts(ExpectedCounts, Msg) :-
        format(atom(M1), "~|~`-t~30+\nExpected Counts: \n\n", []),
        pprint_num_assoc(ExpectedCounts, M2),
        atomic_list_concat([M1, M2], '\n', Msg).

debug_new_rule_weights(NewWeights, Msg) :-
        format(atom(M1), "~|~`-t~30+\nMultinomial Weights: \n\n", []),
        pprint_num_assoc(NewWeights, M2),
        atomic_list_concat([M1, M2], '\n', Msg).

%% ----------------------------------------------------------------------
%%      free_energy(+ExpectedCounts, -VB_LowerBound) is det
%%      free_energy(+ExpectedCounts, +RuleWeights, -VB_LowerBound) is det
%%
%%      Computes the variational lower bound of the current rule
%%      weights.  If the RuleWeights assoc is provided, then that is used;
%%      otherwise, we use the current global rule weights.

%% See Kuhrihara & Sato 'Variational Bayesian Grammar Induction for
%% Natural Language', 2006. Eq 8.
free_energy(PriorHyperParams,
            HyperParams,
            MultinomialWeights,
            DSearchResults,
            Loglikelihood,
            FreeEnergy
            ) :-
        loglikelihood(DSearchResults,
                      MultinomialWeights,
                      LogLikelihood),
        % terms 2 and 3 in Eq 8.
        free_energy1(PriorHyperParams,
                     HyperParams, 
                     FreeEnergy1),
        % term 4 in Eq 8.
        free_energy2(PriorHyperParams,
                     HyperParams,
                     MultinomialWeights, 
                     FreeEnergy2
                     ),
        FreeEnergy is LogLikelihood + FreeEnergy1 + FreeEnergy2.

loglikelihood([], _, 0) :- !.
loglikelihood(Ds, MultinomialWeights, Loglikelihood) :-
        Ds = [_|_],  
        !, 
        loglikelihood(Ds, MultinomialWeights, 0, Loglikelihood).
loglikelihood(dsearch_result(_, Count, Derivations), MultinomialWeights, Loglikelihood) :-
        findall(RuleCounts,
                (member(deriv(_, DGraph, _), Derivations),
                 dgraph_rule_counts(DGraph, RuleCounts)),
                RuleCountsList),
        maplist(call(multinomial_loglikelihood, MultinomialWeights), RuleCountsList, Ls),
        maplist(call(prod, Count), Ls, Ls1), 
        Loglikelihood <- logSumExp(Ls1).

% worker predicate
loglikelihood([], _, LIn, LIn) :- !.
loglikelihood([D|Ds], MultinomialWeights, LIn, LOut) :-
        loglikelihood(D, MultinomialWeights, L),
        LTmp is LIn + L, 
        loglikelihood(Ds, MultinomialWeights, LTmp, LOut).
        
        
        

multinomial_loglikelihood(MultinomialWeights, Counts, LogLikelihood) :-
        assoc_to_values(MultinomialWeights, Ws),
        assoc_to_values(Counts, Cs), 
        LogLikelihood <- dmultinom(Cs, 'NULL', Ws, 'log=TRUE').
                          

:- begin_tests(learning).

test(multinomial_loglikelihood,
     [true(Error < 1e-4)]) :-
        list_to_assoc([r(1)-0, r(2)-12, r(3)-2], Counts),
        list_to_assoc([r(1)-0.2, r(2)-0.0001, r(3)-0.5], MultinomialWeights),
        multinomial_loglikelihood(MultinomialWeights, Counts, LogLikelihood),
        LogLikelihoodTrue = -102.4081,
        Error is abs(LogLikelihood-LogLikelihoodTrue).
        
      

:- end_tests(learning).

        
        
        
        
        
        




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
%%      prove_goals(+Goals, -Derivations) is det
%%      prove_goals(+Goals, -Derivations, +Options) is det
%%
%%      - Goals is a list of goals [G|...] where G is either a bare
%%      goal or count(Goal, C).
%%      - Derivations is a list of structures
%%      [dsearch_results(OrigGoal, Count, deriv(ResultGoal, DGraph, CondProb))|...]
%%      - Options are shared with mi_best_first/3
%%
%%      Description: 

prove_goals(Goals, Derivations) :-
        prove_goals(Goals, Derivations, []).
        
prove_goals(Goals, Derivations, Options) :-
        prove_goals(Goals, [], Derivations, Options).

prove_goals([], DsIn, DsIn, _).
prove_goals([count(Goal, Count) | Goals], DsIn, DsOut, Options) :-
        !,
        mi_best_first_all(Goal, Derivations, _, Options),
        DsTmp = [dsearch_result(Goal, Count, Derivations) | DsIn],
        prove_goals(Goals, DsTmp, DsOut, Options).
prove_goals([Goal|Goals], DsIn, DsOut, Options) :-
        Goal \= count(_, _), % check is redundant due to CUT above
        prove_goals([count(Goal, 1)|Goals], DsIn, DsOut, Options). 


%% ----------------------------------------------------------------------


%% ----------------------------------------------------------------------
%%      expected_rule_counts(+DSearchResults, -Assoc, -Options)
%%
%%      Given a set of observations, return the expected rule counts
%%      for the whole set.
%%
%%      -------------
%%      - DSearchResults : a list of results from prove_goals/N, of
%%      the form [dsearch_result(Goal, Count, Derivations)|...]
%%
%%      - Assoc: An assoc whose keys are rule ids and values are the
%%      expected number of times the rule is used for this set of
%%      observations.
%%
%%      Options: takes the same options as mi_best_first_all.
%%
expected_rule_counts(DSearchResults, Assoc) :-
        expected_rule_counts(DSearchResults, Assoc, []).

expected_rule_counts(DSearchResults, Assoc, Options) :-
        empty_rules_assoc(Empty),
        expected_rule_counts(DSearchResults, Empty, Assoc, Options).

% create an empty assoc with rule id keys
empty_rules_assoc(Assoc) :-
        rules(RuleIds),
        findall(RuleId-0, member(RuleId, RuleIds), RVs),
        list_to_assoc(RVs, Assoc).

% worker predicate
expected_rule_counts([], Assoc, Assoc, _).
expected_rule_counts([dsearch_result(_, Count, Derivations)|Goals], AssocIn, AssocOut, Options) :-
        findall(DGraph-W,
                member(deriv(_, DGraph, W), Derivations),
                ScoredDGraphs),
        assert(sd(ScoredDGraphs)), 
        expected_rule_counts1(ScoredDGraphs, Assoc0),
        scalar_multiply_assoc(Count, Assoc0, Assoc1),
        add_assocs(0, Assoc1, AssocIn, AssocTmp),
        expected_rule_counts(Goals, AssocTmp, AssocOut, Options).
        


:- begin_tests(expected_rule_counts).

test(expected_rule_counts,
     [
      setup(setup_trivial_sdcl),
      cleanup(cleanup_trivial_sdcl)
      ]) :-
       Goals = [s([a, a], [])],
       prove_goals(Goals, DSearchResults), 
       expected_rule_counts(DSearchResults, Assoc),
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
        
%%     dgraph_rule_counts(+DGraph, -Assoc) is Det
%%     - Returns the rule counts Assoc for DGraph.
%%     - DGraph: The dgraph(_, _) structure.
%%     - Assoc: An assoc (key = rule id, val = number) in which each
%%     key is associated with the number of times it appears in the
%%     DGraph.
dgraph_rule_counts(DGraph, Assoc) :-
        % since no weight is given, set the weight to 1. 
        dgraph_rule_counts(DGraph, 1, Assoc). 

%%    dgraph_rule_counts(+DGraph, +W, -Assoc) is det.
%%    - Returns an assoc of rule counts in the derivation graph,
%%    weighted by W. 
dgraph_rule_counts(DGraph, W, Assoc) :-
        % this initializes the assoc to have a zero for each rule
        empty_rules_assoc(Empty),
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

test(dgraph_rule_counts_unweighted,
     [set(R-C=[r(1)-3, r(2)-1])]) :- 
        test_dgraph(DGraph),
        dgraph_rule_counts(DGraph, Assoc),
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
pprint_num_assoc(Assoc, Out) :- 
        assoc_to_list(Assoc, RCs),
        findall(Line,
                (
                 member(R-C, RCs),
                 format(atom(Line), "~w~| ~`.t ~20|~g\n", [R, C])
                ),
                Lines),
        atomic_list_concat(Lines, Out).


        


        
                 
                        
        



%% ----------------------------------------------------------------------


%%	time(:Goal, -CPU, -Wall)
%
%	hProlog compatible predicate to for statistical purposes

time(Goal, CPU, Wall) :-
	get_time(T0),
	statistics(cputime, CPU0),
	call(Goal),
	statistics(cputime, CPU1),
	get_time(T1),
	Wall is T1-T0,
	CPU is CPU1-CPU0.

