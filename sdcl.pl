% sdcl.pl
%% author: Eyal Dechter


%% TODO: Enable maximum number of solutions, so that if there are very
%% large number, the system doesn't choke.
%% TODO: predicates for setting alpha values of existing rules both via various defaults and custom.
%% TODO: VBEM should do something graceful if there are observations it can't answer.

% :- module(sdcl).

:- use_module(library(record)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(gensym)).
:- use_module(library(real)).


:- r(library("matrixStats")).

:- op(1000, xfy, --->).
:- op(1200, xfy, ::).

:- ['compile'].
:- ['pprint'].
:- [assoc_extra].


%% ----------------------------------------------------------------------
%% lips_estimate(LIPS).  An estimate of the numeber of logical
%% inferences per second. This is used to provide a computation time
%% limit for each branch of the derivation search below. Given a time
%% limit in seconds, we calculate an inference limit for use with
%% call_with_inference_limit/3.
:- dynamic lips_estimate/1.
lips_estimate(1e6).

%% ----------------------------------------------------------------------
%% sdcl_rule record

:- record sdcl_rule(id,
                    head,
                    body,
                    prob,
                    alpha, 
                    group
                    ).

find_rule_by_id(RuleId, Rule) :-
        make_sdcl_rule([id(RuleId)], Rule),
        call(Rule), !
        ;
        throw(error(domain_error(sdcl_rule, Rule),
                    find_rule_by_id(RuleId, Rule))).

:- begin_tests('sdcl_rule record').

test('find_rule_by_id',
     [
      setup(setup_trivial_sdcl),
      forall((setup_trivial_sdcl, rules(Rules), member(RuleId, Rules))),
      true(Rule1 =@= Rule),
      cleanup(cleanup_trivial_sdcl)
     ]) :-
        find_rule_by_id(RuleId, Rule1),
        Rule = sdcl_rule(RuleId, _, _, _, _, _),
        call(Rule), !.
       

:- end_tests('sdcl_rule record').

% accessors and setters for rule probability values
get_rule_prob(RuleId, P) :-
        find_rule_by_id(RuleId, Rule),
        sdcl_rule_prob(Rule, P).
                 

set_rule_prob(RuleId, P) :-
        find_rule_by_id(RuleId, Rule),
        set_prob_of_sdcl_rule(P, Rule, NewRule),
        retractall(Rule),
        assert(NewRule).

set_rule_probs(RuleIdProbAssoc) :-
        rule(RuleId),
        get_assoc(RuleId, RuleIdProbAssoc, W),
        set_rule_prob(RuleId, W),
        fail
        ;
        true.

get_rule_probs(RuleIdProbAssoc) :-
        findall(RuleId-W,
                (rule(RuleId),
                 get_rule_prob(RuleId, W)),
                RuleProbs),
        list_to_assoc(RuleProbs, RuleIdProbAssoc).

% accessors and setters for rule alpha values
get_rule_alpha(RuleId, Alpha) :-
        find_rule_by_id(RuleId, Rule),
        sdcl_rule_alpha(Rule, Alpha).


get_rule_alphas(AlphaAssoc) :-
        findall(RuleId-Alpha,
                (rule(RuleId),
                 get_rule_alpha(RuleId, Alpha)),
                RAs),
        list_to_assoc(RAs, AlphaAssoc).

set_rule_alpha(RuleId, default) :-
        !,
        set_rule_alpha(RuleId, 1.0).

set_rule_alpha(RuleId, A) :-
        assertion(number(A)),
        assertion(A>0),
        !,
        find_rule_by_id(RuleId, Rule),
        set_alpha_of_sdcl_rule(A, Rule, NewRule),
        retractall(Rule),
        assert(NewRule).



set_rule_alphas(default) :-
        !,
        findall(_,
                (rule(RuleId),
                 set_rule_alpha(RuleId, 1.0)),
                _).

set_rule_alphas(uniform) :-
        !,
        set_rule_alphas(uniform(1.0)).


set_rule_alphas(uniform(K)) :-
        !,
        assertion(number(K)),
        findall(_,
                (rule_group(RuleGroup),
                 rule_group_rules(RuleGroup, RuleIds),
                 length(RuleIds, N),
                 assertion(N>0),
                 Alpha is 1.0/(K*N),
                 constant_assoc(RuleIds, Alpha, AlphaAssoc),
                 set_rule_alphas(AlphaAssoc)),
                _).
                 
set_rule_alphas(Assoc) :-
        is_assoc(Assoc), !,
        assoc_to_list(Assoc, RAs),
        findall(_,
                (member(R-A, RAs),
                 set_rule_alpha(R, A)),
                _).
                

        

:- begin_tests(alphas).

test(set_default_rule_alpha,
     [setup(setup_trivial_sdcl),
      cleanup(cleanup_trivial_sdcl),
      all(Alpha=[1.0, 1.0])]) :-
        rule(RuleId),
        find_rule_by_id(RuleId, Rule),
        set_rule_alpha(RuleId, default),
        sdcl_rule_alpha(Rule, Alpha).

test(set_default_rule_alphas,
     [setup(setup_trivial_sdcl),
      cleanup(cleanup_trivial_sdcl),
      all(Alpha=[1.0, 1.0])]) :-
        set_rule_alphas(default),
        rule(RuleId),
        find_rule_by_id(RuleId, Rule),
        sdcl_rule_alpha(Rule, Alpha).

test(set_uniform_rule_alpha,
     [setup(setup_sdcl('trivial_2.pl')),
      cleanup(cleanup_sdcl),
      true(RAs ~= [r(1)-1.0, r(2)-0.5, r(3)-0.5,
                          r(4)-1.0, r(5)-1.0, r(6)-1.0,
                          r(7)-0.5, r(8)-0.5, r(9)-0.5,
                          r(10)-0.5, r(11)-0.333333333, r(12)-0.33333333,
                          r(13)-0.333333333])]) :-
        set_rule_alphas(uniform),
        get_rule_alphas(Assoc),
        assoc_to_list(Assoc, RAs).

test(set_uniform_k_rule_alpha,
     [setup(setup_sdcl('trivial_2.pl')),
      cleanup(cleanup_sdcl),
      true(RAs ~= [r(1)-0.50, r(2)-0.25, r(3)-0.25,
                          r(4)-0.5, r(5)-0.5, r(6)-0.5,
                          r(7)-0.25, r(8)-0.25, r(9)-0.25,
                          r(10)-0.25, r(11)-0.1666666, r(12)-0.1666666,
                          r(13)-0.166666])]) :-
        set_rule_alphas(uniform(2.0)),
        get_rule_alphas(Assoc),
        assoc_to_list(Assoc, RAs).




:- end_tests(alphas).


        
rules(RuleIds) :-
        make_sdcl_rule([], Rule),
        findall(RuleId, (call(Rule),
                         sdcl_rule_id(Rule, RuleId)),
                RuleIds).

rule(RuleId) :-
        rules(RuleIds),
        !,
        member(RuleId, RuleIds).

rule_functor(RuleId, Functor/Arity) :-
        find_rule_by_id(RuleId, Rule),
        sdcl_rule_head(Rule, sdcl_term(Functor/Arity, _, _)).

functor_rules(Functor/Arity, RuleIds) :-
        FA = Functor/Arity,
        findall(RuleId,
              (
               make_sdcl_rule([id(RuleId)], R),
               call(R),
               sdcl_rule_head(R, sdcl_term(FA, _, _))
                ),
              RuleIds).

functors(Functors) :-
        rules(RuleIds),
        setof(F/A,
              RuleId^(member(RuleId, RuleIds),
               rule_functor(RuleId, F/A)),
              Functors).

rule_group_rules(RuleGroup, RuleIds) :-
        findall(RuleId,
                sdcl_rule(RuleId, _, _, _, _, RuleGroup),
                RuleIds).

%% rule_groups(-RuleGroups) is det.
%% RuleGroups is a list of rule groups present in the current rule set. 
rule_groups(RuleGroups) :-
        findall(RuleGroup,
                sdcl_rule(_, _, _, _, _, RuleGroup),
                RuleGroups0),
        sort(RuleGroups0, RuleGroups).

rule_group(RuleGroup) :-
        rule_groups(RuleGroups),
        member(RuleGroup, RuleGroups).
                
        

rule_group_norm(RuleGroup, Z) :-
        rule_group_rules(RuleGroup, RuleIds),
        findall(Prob,
                (member(RuleId, RuleIds), 
                 get_rule_prob(RuleId, Prob)
                ),
                Ws),
        sum_list(Ws, Z).

rule_group_norm(RuleGroup, RuleAssoc, Z) :-
        rule_group_rules(RuleGroup, RuleIds),
        findall(Prob,
                (member(RuleId, RuleIds), 
                 get_assoc(RuleId, RuleAssoc, Prob)
                ),
                Ws),
        sum_list(Ws, Z).

rule_group_norms(RuleGroupAssoc) :-
        rule_groups(RuleGroups),
        findall(RuleGroup-Z,
                (member(RuleGroup, RuleGroups),
                 rule_group_norm(RuleGroup, Z)),
                RuleGroupValList),
        list_to_assoc(RuleGroupValList, RuleGroupAssoc).

%% Collapse rule assoc over rule group, summing the corresponding
%% values.
rule_group_norms(RuleAssoc, RuleGroupAssoc) :-
        rule_groups(RuleGroups),
        findall(RuleGroup-Z,
                (member(RuleGroup, RuleGroups),
                 rule_group_norm(RuleGroup, RuleAssoc, Z)),
                RuleGroupValList),
        list_to_assoc(RuleGroupValList, RuleGroupAssoc).

                 

normalize_rule_group(RuleGroup) :-
        rule_group_rules(RuleGroup, RuleIds),
        rule_group_norm(RuleGroup, Z),
        (
         member(RuleId, RuleIds),
         get_rule_prob(RuleId, P),
         P1 is P/Z, 
         set_rule_prob(RuleId, P1),
         fail
         ;
         true).

normalize_rules :-
        rule_groups(RuleGroups),
        !,
        (
        member(RuleGroup, RuleGroups), 
        normalize_rule_group(RuleGroup),
        fail
        ;
        true
        ).
        
        
%% ----------------------------------------------------------------------
%%      meta_best_first(Goal)
%%
%%      Description: A best first meta interpreter.
%%
%%      Options:
%%
%%       - BeamWidth: The maximal size of the priority queue of best
%%       partial parses (default: infinite).
%%
%%       - InferenceLimit: Limits the maximum number of inference
%%       steps for each derivation of Goal (default: 1000).
%%       ----------------------------------------------------------------------


mi_best_first_options_default(
      [
       beam_width(1000),
       time_limit_seconds(1)
       ]).

mi_best_first(Goal, Score, DGraph) :-
        mi_best_first(Goal, Score, DGraph, []). 

mi_best_first(Goal, Score, DGraph, Options) :-
        
        % make options record
        mi_best_first_options_default(DefaultOptions), 
        merge_options(Options, DefaultOptions, AllOptions),
        make_bf_options(AllOptions, OptionsRecord, _RestOptions),

        % translate goal
        tr_sdcl_term(Goal, GoalTr),
        
        % initialize priority queue
        reset_gen_node,
        gen_node_id(NodeId), 
        GoalList = [goal(NodeId, GoalTr)],

        bf_options_beam_width(OptionsRecord, BeamWidth),

        % initialize priority queue. Each element of priority queue is
        % of the form deriv_info(CurrentGoalList-OrigGoal, DerivationGraph)
        DGraph0 = dgraph(NodeId, [goal(NodeId, GoalTr)], []),
        pq_singleton(deriv_info(GoalList-Goal, DGraph0),
                     0,
                     BeamWidth,
                     PQ),

        bf_options_time_limit_seconds(OptionsRecord, TimeLimit),
        time_limit_inference_limit(TimeLimit, InferenceLimit), 
        
        call_with_inference_limit(mi_best_first_go(PQ, Goal, Score, DGraph, OptionsRecord),
                                  InferenceLimit,
                                  Result),
        (Result = inference_limit_exceeded ->
         format("***Inference Limit Exceeded***\n"), 
         fail;
         true
         ).

%% time_limit_inference_limit(+TimeLimit, -InferenceLimit) TimeLimit is
%% a number of seconds, and InferenceLimit is the approximate number
%% of inferences that can be made in that amount of time. The estimate
%% is made using the the value of lips_estimate/1. 
time_limit_inference_limit(TimeLimit, InferenceLimit) :-
        lips_estimate(Lips), 
        InferenceLimit is floor(TimeLimit * Lips).
        
        
:- begin_tests(mi_best_first).

%% test that we get at least the first result correctly.
test(mi_best_first,
     [
      setup(setup_trivial_sdcl),
      cleanup(cleanup_trivial_sdcl),
      true(G=@= s([a], []))
     ]) :-
        G=s(_X, []), 
        mi_best_first(G, _, _),
        !.
        

:- end_tests(mi_best_first).

        
%% ----------------------------------------------------------------------
%% Options record for mi_best_first

:- record bf_options(beam_width=100,
                     time_limit_seconds=1
                    ).
%% ----------------------------------------------------------------------
%% mi_best_first_go/5.
%% main worker predicate for mi_best_first
%%
%% mi_best_first_go(+PQ, OrigGoal, Score, DGraph, +OptionsRecord) 
mi_best_first_go(PQ, _, _, _, _) :-
        %% --- DEBUG
        % print_current_frame,
        %% ---

        % if PQ is empty, fail.
        pq_empty(PQ), !, fail.
mi_best_first_go(PQ, Goal, ScoreOut, DGraphOut,  OptionsRecord) :-
        %% --- DEBUG
        % print_current_frame,
        %% ---

        % Solution is found, return goal.
        % Continue to next goal on backtracking.
        pq_find_max(PQ, deriv_info([]-OrigGoal, DGraph), Score, NewPQ),
         
        (
         Goal = OrigGoal,
         ScoreOut = Score,
         DGraphOut = DGraph
         
        ;
         mi_best_first_go(NewPQ, Goal, ScoreOut, DGraphOut, OptionsRecord)
        ).
        
mi_best_first_go(PQ, OrigGoal, ScoreOut, DGraphOut, OptionsRecord) :-
        %% --- DEBUG
        % print_current_frame,
        %% ---

        % If the next best is not a solution
        % get the best solution from priority queue
        pq_find_max(PQ, Elem, Score, PQ1),

        % extend best solution
        extend(Elem, Score, ElemScores),

        %% if any of the generated elements is a solution, return
        %% these immediately
        (
         member(deriv_info([]-OrigGoal, DGraphOut)-ScoreOut, ElemScores)
        ;
         findall(ES,
                 (ES = (deriv_info(Gs-_, _)-_),
                  member(ES, ElemScores), 
                  Gs \= []),
                 ElemScores1),
         pq_inserts(ElemScores1, PQ1, NewPQ),
         % pq_size(NewPQ, Size),
         % format("PQ Size: ~w\n", [Size]),
         % pq_show(NewPQ), 
         mi_best_first_go(NewPQ, OrigGoal, ScoreOut, DGraphOut, OptionsRecord)
        ).
        

%% ----------------------------------------------------------------------

:- begin_tests(extend).
test(extend,
     [setup(setup_trivial_sdcl),
      true(NExt=2)]
     ) :-
    X = s(_, _),
    tr_sdcl_term(X, T),
    G = goal(1, T), 
    
    DInfo = deriv_info([G]-G, dgraph(_, [1], [])),
    extend(DInfo, 0, Extensions),
    length(Extensions, NExt).
        
:- end_tests(extend). 
      

%% extend(deriv_info(Goals-OrigGoal, DGraph), Score, Extensions)
%% assumptions:
%% - Goals is not empty
%% - Goals is a list of structures of the form goal(GoalId, Goal) where Goal is an sdcl_term/3.
%% - Extensions is a list of pairs deriv_info(Goals-OrigGoal, DGraph)-Score
extend(deriv_info([G|Rest]-OrigGoal, DGraph), Score, ElemScores) :-
        
        % the current derivation graph
        DGraph = dgraph(StartNodeId, Nodes, HyperEdges),

        %% - G_new: the new queue of goals for the current derivation
        %% - OrigGoal_copy: a copy of the toplevel goal (keeps track
        %% of top level bindings in this derivation)
        %% - DGraph_new: the new derivation graph generated by a
        %% choice of extending rule
        findall(deriv_info(G_new-OrigGoal_copy, DGraph_new)-Score_new,
                (
                 copy_term([G|Rest]-OrigGoal, [G_copy|Rest_copy]-OrigGoal_copy),

                 % find a matching rule for the current goal
                 G_copy = goal(NodeId, Goal),
                 match(Goal, BodyList, RuleId, Prob),
                 
                 %% for each

                 (
                  bagof(goal(ChildNodeId, ChildGoal),
                        (
                         member(ChildGoal, BodyList),
                         gen_node_id(ChildNodeId)
                        ),
                        ChildNodes) -> true
                  ;
                  ChildNodes = []
                 ),
                 length(BodyList, BN),
                 length(ChildNodes, CN),
                 assertion(BN = CN), 
                 
                 
                 findall(ChildNodeId,
                         member(goal(ChildNodeId, _), ChildNodes),
                         ChildNodeIds),

                 %% append new goal nodes onto the list of nodes for the derivation graph
                 append(ChildNodes, Nodes, Nodes_new),
                 
                 %% create a new hyperedge for the derivation graph
                 HyperEdge = hyperedge(NodeId, RuleId, ChildNodeIds),
                 % debug(dgraph,
                       % "Generate hyperedge: ~w", [HyperEdge]),
                 
                 %% construct updated derivation graph
                 HyperEdges_new = [HyperEdge|HyperEdges],
                 DGraph_new = dgraph(StartNodeId, Nodes_new, HyperEdges_new),

                 %% assert dgraph is a tree
                 %% i.e. number of nodes is number of edge children plus 1
                 assert_dgraph_tree_property(DGraph_new), 
                         

                 

                 %% calculate the updated log probability of the
                 %% current derivation
                 LogProb is log(Prob),
                 Score_new is Score + LogProb,
                 assertion(Score_new =< Score),

                 %% prepend the new goal nodes onto the goal stack
                 append(ChildNodes, Rest_copy, G_new)
                                 
                ),
                ElemScores
               ).


reset_gen_node :-
        reset_gensym('node_').
                     
gen_node_id(Id) :-
        gensym('node_', Id).

%% match goal against SDCL DB
%% match(Goal, BodyList, Rule-RuleId, RuleProb)
match(Goal, BodyList, RuleId, Prob) :-
        % find all matching rules for the current conditioner
        Goal = sdcl_term(F/A, _, Conds),
        Head = sdcl_term(F/A, _, Conds),
        findall(P,
                sdcl_rule(RuleId, Head, _, P, _, _),
                Ps),
        sum_list(Ps, Z),
        !,
        % find a matching rule for the goal in the db
        Rule=sdcl_rule(RuleId, _, _, _, _, _),
        Rule,
        %% Debug
        pprint_term(Goal, GString), 
        pprint_rule(Rule, RString), 
        debug(match,
              "Matching ~w against rule ~w", [GString, RString]),
        %% End Debug
        copy_term(Rule, RuleCopy),
        RuleCopy = sdcl_rule(RuleId, Goal, Body, P0, _, _),
        
        Prob is P0/Z,
        
        % NB: sdcl probabilities should be normalized at this point
        and_to_list(Body, BodyList),
        
        %% Debug
        maplist(pprint_term, BodyList, BodyStrings),
        atomic_list_concat(BodyStrings, ', ', BodyString), 
        debug(match,
              "Result: ~w", [BodyString]).
        %% End Debug


:- begin_tests(match).

test(match,
     [setup(setup_trivial_sdcl),
      true(NMatch=2)]
     ) :-
    X = s(_, _),
    tr_sdcl_term(X, G),
    
    findall(RuleId, 
            match(G, RuleId, _, _),
            MatchingRuleIds),
    length(MatchingRuleIds, NMatch).
        
:- end_tests(match). 

%% conjunction to list
and_to_list(true, []).
and_to_list(C, [C]) :-
        C \= true,
        C \= (_,_).
and_to_list((true, Cs), Xs) :-
        and_to_list(Cs, Xs).
and_to_list((C, Cs), [C|Xs]) :-
        C \= true,
        and_to_list(Cs, Xs).

list_to_and([], true).
list_to_and([X], X).
list_to_and([X|Xs], (X, Ys)) :-
        list_to_and(Xs, Ys).

%% ----------------------------------------------------------------------
%%      mi_best_first_all(Goal, List of deriv(Goal, DGraph, ConditionalProb), Options)
%%
%%
%%      Options:
%%
%%       - BeamWidth: The maximal size of the priority queue of best
%%       partial parses (default: infinite).
%%
%%       - InferenceLimit: Limits the maximum number of inference
%%       steps for each derivation of Goal (default: 1000).
%%       ----------------------------------------------------------------------
mi_best_first_all(Goal, Results, LogP) :-
        mi_best_first_all(Goal, Results, LogP, []).

mi_best_first_all(Goal, Results, LogP, Options) :-
        findall(deriv(Goal, DGraph, Score), 
                 mi_best_first(Goal, Score, DGraph, Options),
                Results0),
        % add conditional probability given Goal being true to each
        % derivation
        findall(Score,
                member(deriv(Goal, DGraph, Score), Results0),
                Scores),
        (Scores=[] -> % failed to find any derivations,
         LogP = 0, 
         Results = []
         ;
         LogP <- logSumExp(Scores),
         findall(deriv(Goal, DGraph, ConditionalProb), 
                 (
                  member(deriv(Goal, DGraph, Score), Results0),
                  ConditionalProb is exp(Score - LogP)
                 ),
                 Results)
        ).

log_sum_exp(Xs, Y) :-
        max_list(Xs, MaxX),
        findall(X1,
                (member(X, Xs), X1 is exp(X - MaxX)),
                Xs1),
        sum_list(Xs1, X2),
        Y is MaxX + log(X2).
        
%% ----------------------------------------------------------------------
%%      Log likelihood of data
log_likelihood(Goal, LogP, Options) :-
        findall(L,
                mi_best_first_all(Goal, _, L, Options),
                Ls),
        sum_list(Ls, LogP).

                
        

%%
%% 
%% ----------------------------------------------------------------------
%%
%% Priority Queue
%%
%% Description: a maximum priority queue. The queue is a term
%% pq(PQ_elements, Size, MaxSize) where PQ_elements is a sorted list of terms
%% pq_elem(Elem, Score).
%%
%% MaxSize: the maximum length of the priority queue (default:
%% infinity). Will reject adding an element worse equal to or worse
%% than the current worse element.

pq_singleton(Elem, Score, pq([pq_elem(Elem, Score)], 1, infinity)).
pq_singleton(Elem, Score, MaxSize, pq([pq_elem(Elem, Score)], 1, MaxSize)).

pq_empty(MaxSize, pq([], 0, MaxSize)).
pq_empty(pq([], 0, infinity)).

% pq_insert(+Elem, +Score, +PQ, -NewPQ) inserts a new Element with
% Score into PQ.
pq_insert(Elem, Score, pq(Elems, Size, MaxSize), pq(NewElems, SizeOut, MaxSize)) :-
        Size1 is Size + 1,
        pq_insert_sorted_list(Elem, Score, Elems, NewElems1),
        
        % if too many elements, take the first MaxSize elements
        (
         (MaxSize \= infinity, Size1 > MaxSize) ->
         take(MaxSize, NewElems1, NewElems),
         SizeOut = MaxSize
        ;
         NewElems = NewElems1, SizeOut = Size1
        ).

% pq_insert_sorted_list(Elem, Score, Sorted, NewSorted)
%
% inserts Elem with score Score into Sorted in ascending order
% resulting in NewSorted.
%
% if Sorted is empty
pq_insert_sorted_list(Elem, Score, [], [pq_elem(Elem, Score)]) :- !.
% if Score is greater than or equal to score at head of list
pq_insert_sorted_list(Elem, Score, [pq_elem(E, S)|Xs], [pq_elem(Elem, Score), pq_elem(E, S) | Xs]) :-
        Score >= S, !.

% if Score is less than score at head of list, recurse
pq_insert_sorted_list(Elem, Score, [pq_elem(E, S)|Xs], [pq_elem(E, S) | Xs1]) :-
        Score < S, !, 
        pq_insert_sorted_list(Elem, Score, Xs, Xs1).

% pq_inserts(+[Elem-Score], +PQ, -NewPQ)
% inserts pairs of scored elements into priority queue
%
% TODO: inserting multiple elements should require only a single pass,
% but is currently implemented as multiple passes, one for each element.
pq_inserts([], PQ, PQ).
pq_inserts([Elem-Score|Pairs], PQ, NewPQ) :-
        pq_insert(Elem, Score, PQ, PQ1),
        pq_inserts(Pairs, PQ1, NewPQ).


% pq_find_max(+PQ, -MaxElem, -Score, -NewPQ)
%
% removes maximum element from priority queue and return the new queue
% without this element. Will throw an error if PQ is empty.
%
pq_find_max(PQ, _, _, _) :-
        pq_empty(PQ),
        !, 
        throw(error(pq_find_max_of_empty(PQ))).
pq_find_max(pq([pq_elem(MaxElem, Score)|Rest], Size, MaxSize),
            MaxElem,
            Score,
            pq(Rest, Size1, MaxSize)) :-
        Size1 is Size - 1.

% pq_size(+PQ, -Size)
% get the size of the priority queue
pq_size(pq(_, Size, _), Size).

% pq_show(PQ) pretty prints priority queues
pq_show(pq(Elems, Size, _)) :-
        format("Size: ~w\n", [Size]),
        member(pq_elem(Elem, _), Elems),
        Elem = deriv_info(_, DGraph),
        pprint_dgraph(DGraph),
        pprint_raw_dgraph(DGraph),
        dgraph_dtree(DGraph, DTree), 
        pprint_raw_dtree(DTree),
        assert_dgraph_tree_property(DGraph), 
        fail.
pq_show(_).

dgraph_size(dgraph(_, _, Hs), M) :-
        length(Hs, M).

assert_dgraph_tree_property(DGraph) :-
        DGraph = dgraph(_, Nodes, HEdges),
        length(Nodes, NumNodes),
        maplist(call(arg, 3), HEdges, HChildren),
        maplist(length, HChildren, HNChildren),
        sum_list(HNChildren, NChildren),
        assertion(NumNodes is NChildren + 1).
        





%% ----------------------------------------------------------------------
%% ----------------------------------------------------------------------
%% Derivation data structures
%% 
%% A data structure for constructing derivation graphs and trees
%% during best first search. 
%%
%% A derivation graph is a structure dgraph(start_node_id, list of nodes, list of hyperedges)
%% A node is a structure node(Id, Goal)
%% A hyperedge is a structure hyperedge(NodeId, RuleId, ChildNodeIds).
%%
%% This graph is a multiway tree, so we also supply a predicate to
%% construct trees out of these graphs.
%%
%% A derivation tree is a structure: dtree(Node, RuleId, [list of dtree]).

%% --------------------
%% dgraph_dtree(+DGraph, -DTree) is det
%% Convert DGraph into a DTree
dgraph_dtree(DGraph, DTree) :-
        DGraph = dgraph(StartNodeId, Nodes, HyperEdges),
        StartNode = goal(StartNodeId, _),
        (
         member(StartNode, Nodes) ->
         true
         ;
         throw(error(ill_formed_dgraph, 'StartNode not found in Nodes'))
        ), 
        DTree = dtree(StartNode, _, _), 
        dgraph_dtree_go(HyperEdges, Nodes, DTree).

dgraph_dtree_go(HyperEdges,
                Nodes,
                dtree(goal(NodeId, _), RuleId, Trees)) :-
        member(hyperedge(NodeId, RuleId, ChildNodeIds), HyperEdges),
        !, 
        findall(dtree(ChildNode, R, Cs), 
                (member(ChildNodeId, ChildNodeIds),
                 ChildNode = goal(ChildNodeId, _),
                 (
                  member(ChildNode, Nodes) ->
                  true
                 ;
                  throw(error(ill_formed_dgraph, 'ChildNode not found in Nodes'))
                 ),
                 dgraph_dtree_go(HyperEdges,
                                 Nodes,
                                 dtree(ChildNode, R, Cs))), 
                 Trees).
%% --------------------

:- begin_tests(dgraph).

test(dgraph_dtree) :- 
      Nodes = [goal(node_1, g1),
               goal(node_2, g2),
               goal(node_3, g3),
               goal(node_4, g4),
               goal(node_5, g5)],
      HyperEdges = [hyperedge(node_1, r(1), [node_2, node_5]),
                    hyperedge(node_2, r(2), [node_3, node_4]), 
                    hyperedge(node_3, r(1), []),
                    hyperedge(node_4, r(3), []),
                    hyperedge(node_5, r(2), [])
                   ], 
      DGraph = dgraph(node_1, Nodes, HyperEdges),
      dgraph_dtree(DGraph, DTree),
      test_dtree(DTree_Correct),
      assertion(DTree=DTree_Correct).
      
      


test_dtree(DTree) :-
        N1 = goal(node_1, g1),
        N2 = goal(node_2, g2),
        N3 = goal(node_3, g3),
        N4 = goal(node_4, g4),
        N5 = goal(node_5, g5),
        DTree = dtree(N1, r(1),
                      [dtree(N2, r(2),
                             [dtree(N3, r(1), []),
                              dtree(N4, r(3), [])]),
                       dtree(N5, r(2), [])]).
        
                            
                      
      

:- end_tests(dgraph). 

pprint_dgraph(DGraph) :-
        dgraph_dtree(DGraph, DTree),
        pprint_dtree(DTree).

pprint_raw_dgraph(DGraph) :-
        DGraph = dgraph(_StartNode, _Nodes, Edges),
        !,
        member(Edge, Edges),
        writeln(Edge),
        fail
        ; true. 

pprint_raw_dtree(DTree) :-
        DTree = dtree(Node, Rule, Children),
        writeln(Node-Rule),
        !,
        member(Child, Children),
        pprint_raw_dtree(Child), 
        fail
        ; true. 


% pprint_dtree(DTree) pretty prints a derivation tree DTree
pprint_dtree(DTree) :-
        pprint_dtree(DTree, 2).
pprint_dtree(DTree, Indent) :-
        pprint_dtree(DTree, Indent, 0).
pprint_dtree(dtree(goal(NodeId, Goal), RuleId, SubTrees), Indent, Cursor) :-
        tab(Cursor),
        write('+ '),
        pprint_term(Goal, GString),
        find_rule_by_id(RuleId, Rule),
        pprint_rule(Rule, RString), 
        format("~w: ~w -- ~w : ~w", [NodeId, GString, RuleId, RString]),
        nl,

        (
        Cursor1 is Cursor + Indent,
        member(SubTree, SubTrees),
        pprint_dtree(SubTree, Indent, Cursor1),
        fail
        ;
        true
        ).

%% number of nodes in dtree
dtree_nodes(Tree, Nodes) :-
        dtree_nodes(Tree, [], Nodes).

dtree_nodes(dtree(Node, _, Children), NIn, [Node|NOut]) :-
        dtree_nodes_go(Children, NIn, NOut).

dtree_nodes_go([], NIn, NIn).
dtree_nodes_go([Tree|Trees], NIn, NOut) :-
        dtree_nodes(Tree, NIn, NTmp),
        dtree_nodes_go(Trees, NTmp, NOut).


:- begin_tests(dtree_nodes).

test(dtree_nodes, set(Node==[node1, node2,  node3])) :-
        test_dtree(Tree), 
        dtree_nodes(Tree, Nodes),
        member(Node, Nodes).

test_dtree(Tree) :-
        Tree = dtree(node1, _,
                     [
                      dtree(node2, _, []),
                      dtree(node3, _, [])
                     ]).

:- end_tests(dtree_nodes).


        

canonical_rule(Rule :: _, Canonical) :-
        copy_term(Rule, Canonical), 
        numbervars(Canonical, 0, _). 
               
%% ----------------------------------------------------------------------


%% ----------------------------------------------------------------------
%% Auxiliary

%% take(N[Int], ListIn, ListOut) returns the first N elements of ListIn
%% in ListOut. If N > length(ListIn), ListOut = ListIn.
take(0, _, []) :- !.
take(_, [], []).
take(N, [X|Xs], [X|Ys]) :-
        N1 is N - 1,
        take(N1, Xs, Ys).

% pair_list_firsts(List of pairs X-Y, List of first elements X)
pair_list_firsts([], []).
pair_list_firsts([X-_|Rest], [X|Xs]) :-
        pair_list_firsts(Rest, Xs).

% pair_list_seconds(List of pairs X-Y, List of second elements X)
pair_list_seconds([], []).
pair_list_seconds([_-Y|Rest], [Y|Ys]) :-
        pair_list_seconds(Rest, Ys).

% replicate(N, Xs, Ys) if Ys is N repetitions of Xs
replicate(N, Xs, Ys) :-
        replicate(N, Xs, [], Ys).

replicate(0, _, YsIn, YsOut) :- !, YsIn = YsOut.
replicate(N, Xs, YsIn, YsOut) :-
        N1 is N - 1,
        append(Xs, YsIn, YsTmp), 
        replicate(N1, Xs, YsTmp, YsOut).


:- begin_tests(auxiliary).

test(replicate, [true(Xs = [a,a,a,a,a])]) :-
        replicate(5, [a], Xs).

test(replicate_empty, [true(Xs = [])]) :-
        replicate(5, [], Xs).


:- end_tests(auxiliary).

% print_current_frame
print_current_frame :-
        prolog_current_frame(Frame), 
        format("Current Frame: ~w\n", [Frame]).

%% ----------------------------------------------------------------------

        
%% ----------------------------------------------------------------------
%% Predicates for building unit tests

trivial_sdcl_file('trivial.pl').

setup_trivial_sdcl :-
        retractall(sdcl_rule(_, _, _, _, _, _)),
        trivial_sdcl_file(File), 
        compile_sdcl_file(File).

cleanup_trivial_sdcl :-
        retractall(sdcl_rule(_, _, _, _, _, _)).

setup_sdcl(File) :-
        retractall(sdcl_rule(_, _, _, _, _, _)),
        compile_sdcl_file(File).

cleanup_sdcl :-
        retractall(sdcl_rule(_, _, _, _, _, _)).
        

        



        
        
            

        


                 
                        
        

        
        
        
        
        
                    
        



                 
                 
                 
                                         
                                              
                                         
                 
                 
                
        
        
        


