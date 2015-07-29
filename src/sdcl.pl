% sdcl.pl
%% author: Eyal Dechter

:- module(sdcl,
          [
           rules/1,
           num_rules/1,
           num_rule_groups/1,
           rule/1,
           rule_functor/2,
           functor_rules/2,
           functors/1,
           rule_group/1,
           get_rule_group_rules/2,
           rule_groups/1,
           rule_group_norm/2,
           rule_group_norm/3,           
           rule_group_norms/1,
           rule_group_norms/2,
           % normalize_rule_group/3,
           normalize_rules/0,

           unconstrained/1,
           call_list_with_occurs_check/1,
           
           prove/4,
           prove/5,

           prove_all/2,
           prove_all/3,
           prove_all/4,

           pprint_dgraph/1,
           pprint_dtree/1,

           dtree_nodes/2,
           
           and_to_list/2,
           list_to_and/2,

           get_rule_group_id_alphas/2

                 ]).

%% External imports
:- use_module(library(record)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(gensym)).
:- use_module(library(real)).
:- use_module(library(settings)).
:- use_module(library(heaps)).

%% Local imports
:- use_module(gl_term).
:- use_module(gl_rule).
:- use_module(assoc_extra).
:- use_module(plunit_extra).
:- use_module(compile).
:- use_module(pprint).
:- use_module(array).
:- use_module(misc).
:- use_module(dgraph).


rules(RuleIds) :-
        findall(RuleId, (
                         gl_rule(RuleId, _, _, _, _)),
                RuleIds).

num_rules(N) :-
        rules(Rs),
        length(Rs, N).

rule(RuleId) :-
        gl_rule(RuleId, _, _, _, _).


rule_functor(RuleId, Functor/Arity) :-
        get_rule(RuleId, Rule),
        gl_rule_head(Rule, gl_term(Functor/Arity, _, _)).

functor_rules(Functor/Arity, RuleIds) :-
        FA = Functor/Arity,
        findall(RuleId,
              (
               make_gl_rule([id(RuleId)], R),
               call(R),
               gl_rule_head(R, gl_term(FA, _, _))
                ),
              RuleIds).

functors(Functors) :-
        rules(RuleIds),
        setof(F/A,
              RuleId^(member(RuleId, RuleIds),
               rule_functor(RuleId, F/A)),
              Functors).

get_rule_rule_group(RuleId, RuleGroup) :-
        gl_rule(RuleId, _, _, _, RuleGroup),
        !.

get_rule_group_rules(RuleGroup, RuleIds) :-
        findall(RuleId,
                gl_rule(RuleId, _, _, _, RuleGroup),
                RuleIds).

rule_group_rule_assoc(RuleGroupRuleAssoc) :-
        empty_assoc(Empty),
        findall(RG-Id,
                gl_rule(Id, _, _, _, RG),
                RGRs), 
        rule_group_rule_assoc(RGRs, Empty, RuleGroupRuleAssoc).

rule_group_rule_assoc([], AssocIn, AssocOut) :- !,
        AssocIn = AssocOut.
rule_group_rule_assoc([RG-Id|RGRs], AssocIn, AssocOut) :-
        (get_assoc(RG, AssocIn, Ids, AssocTmp, [Id|Ids]) -> true
        ;
         put_assoc(RG, AssocIn, [Id], AssocTmp)
        ),
        rule_group_rule_assoc(RGRs, AssocTmp, AssocOut).
        
        


%% rule_groups(-RuleGroups) is det.
%% RuleGroups is a list of rule groups present in the current rule set. 
rule_groups(RuleGroups) :-
        findall(RuleGroup,
                gl_rule(_, _, _, _, RuleGroup),
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

rule_group_id_norm(RuleGroupId, Z) :-
        rule_group_id_rules(RuleGroupId, RuleIds),
        findall(Prob,
                (member(RuleId, RuleIds), 
                 get_rule_prob(RuleId, Prob)
                ),
                Ws),
        sum_list(Ws, Z).

rule_group_id_norm(RuleGroupId, RuleArray, Z) :-
        rule_group_id_rules(RuleGroupId, RuleIds),
        findall(W,
                (member(RuleId, RuleIds), 
                 get(RuleId, RuleArray, W)
                ),
                Ws),
        sum_list(Ws, Z).

rule_group_norm(RuleGroup, RuleArray, Z) :-
        rule_group_rules(RuleGroup, RuleIds),
        findall(Prob,
                (member(RuleId, RuleIds), 
                 get(RuleId, RuleArray, Prob)
                ),
                Ws),
        sum_list(Ws, Z).

rule_group_norms(RuleGroupNorms) :-
        num_rule_groups(N),
        array(N, RuleGroupNorms),
        forall(between(1, N, RGID),
               (rule_group_id_norm(RGID, Z),
                set(RGID, RuleGroupNorms, Z))).

num_rule_groups(N) :-
        rule_groups(Gs),
        length(Gs, N).

% %% Collapse rule assoc over rule group, summing the corresponding
% %% values.
rule_group_norms(RuleArray, RuleGroupNorms) :-
        num_rule_groups(N),
        array(N, RuleGroupNorms),
        forall(between(1, N, RGID),
               (rule_group_id_norm(RGID, RuleArray, Z),
                set(RGID, RuleGroupNorms, Z))).

                 

normalize_rules :-
        rule_group_norms(RuleGroupNorms),
        num_rule_groups(N),
        forall((between(1, N, RGId),
                rule_group_id_rules(RGId, Rules),
                get(RGId, RuleGroupNorms, Z),
                member(Rule, Rules)), 
               (get_rule_prob(Rule, P),
                P1 is P / Z,
                set_rule_prob(Rule, P1))).


get_rule_group_id_alphas(RuleGroupId, RAs) :-
        rule_group_id_rules(RuleGroupId, Rules),
        findall(R-A,
                (member(R, Rules),
                 get_rule_alpha(R, A)),
                RAs). 

% a term is unconstrained if all of its arguments are variables.
unconstrained(gl_term(_, Args, _)) :-
        !, 
        maplist(var, Args),
        vars_all_different(Args).
unconstrained(Term) :-
        gl_term_surface_form(Translated, Term),
        unconstrained(Translated).

vars_all_different(List):-
        term_variables(List, Vs),
        length(List, N),
        length(Vs, M),
        N = M.


:- begin_tests(unconstrained).

test(unconstrained_is_true) :-
        Term = gl_term(a/4, [_X, _Y], [1, _Z]),
        unconstrained(Term).

test(unconstrained_is_false, [fail]) :-
        Term = gl_term(a/4, [s(_X), _Y], [1, _Z]),
        unconstrained(Term).

:- end_tests(unconstrained).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


        
%% ----------------------------------------------------------------------
%%      prove(+Goal, -Score, -DGraph, ?StartTime)
%%      prove(+Goal, -Score, -DGraph, ?StartTime, +Options)
%%
%%      Description: A best first meta interpreter that finds a single
%%      explanantion for Goal.
%%
%%      Options:
%%
%%       - beam_width: The maximal size of the priority queue of best
%%       partial parses (default: 100).
%%
%%       - time_limit_seconds: a time limit on the time since
%%       StartTime for running the search.
%% ----------------------------------------------------------------------


%% Options record for prove
:- record options(beam_width=100,                     % beam_width: the size of the search beam
                  time_limit_seconds=2                % time_limit_seconds: the maximum time spent searching for proofs of                                        
                       ).

prove(Goal, Score, DGraph, StartTime) :-
        prove(Goal, Score, DGraph, StartTime, []). 

prove(Goal, Score, DGraph, StartTime, Options) :-

        % set start time if not set
        (var(StartTime) ->
         get_time(StartTime)
        ;
         true),
        
        % make options record
        (is_list(Options) -> 
         make_options(Options, OptionsRecord, _RestOptions)
        ;
         Options=OptionsRecord
        ),

        print_message(informational, expl_search(start(OptionsRecord))), 

        % translate goal
        (is_gl_term(Goal) ->
         GoalTr=Goal
        ;
         gl_term_surface_form(GoalTr, Goal)
        ),

        

        % reset the node identifier for sub goal nodes
        reset_gen_node,
        gen_node_id(NodeId),
        
        % initial list of goals
        GoalList = [pterm(GoalTr)-NodeId],


        % Initialize priority queue. Each element of priority queue is
        % of the form deriv_info(GoalList, OrigGoal, LogProb, DGraph)
        DGraph0 = dgraph(NodeId, [NodeId-GoalTr], []),

        options_beam_width(OptionsRecord, BeamWidth),
        pq_singleton(deriv_info(GoalList, [], GoalTr, 0, DGraph0),
                     0,
                     BeamWidth,
                     PQ),

        prove_go(PQ, GoalTr, Score,
                         DGraph, 
                         StartTime,
                         OptionsRecord).

:- begin_tests(prove).

%% test that we get at least the first result correctly.
test(prove_finds_first_solution,
     [
      setup(setup_test_gl),
      cleanup(cleanup_test_gl),
      true(G=@= s([a] | [a]))
     ]) :-
        G=s(_X | [a]),
        get_time(StartTime),
        prove(G, _, DG, StartTime,
                      [beam_width(100),
                       time_limit_seconds(2)
                       ]),

        !.

% test(prove_finds_more_than_one_solution,
%      [
%       setup(setup_test_gl),
%       cleanup(cleanup_test_gl),
%       all(G=@= [s([a] | [a]), s([b]|[a])])
%      ]) :-
%         G=s(_X | [a]),
%         get_time(StartTime),
%         prove(G, _, _, StartTime,
%                       [beam_width(100),
%                        time_limit_seconds(2)
%                        ]).
        

:- end_tests(prove).

        

%% ----------------------------------------------------------------------
%% prove_go/7.
%% main worker predicate for prove
%%
%% prove_go(+PQ, OrigGoal, Score, DGraph,
%%                   StartTime, +OptionsRecord)
prove_go(_, _, _, _, StartTime, Opts) :-
        options_time_limit_seconds(Opts, TimeLimit),
        get_time(Now),
        Now-StartTime >= TimeLimit,
        writeln(Now-StartTime >= TimeLimit),
        !,
        fail.
prove_go(PQ, _, _, _, _, _) :-
        % if PQ is empty, fail.
        PQ=pq(_, 0, _),
        !,
        fail.
prove_go(PQ, OrigGoal, LogProb,
                 DGraphOut, TimeInfo, OptionsRecord) :-
        %% return any of the solutions
        PQ = pq(PqElems, Size, MaxSize),
        DInfo = deriv_info([], [], OrigGoal1,LogProbMax, DGraph),
        select(_-DInfo, PqElems, PqElems1),
        !,
        Size1 is Size - 1,
        NewPQ = pq(PqElems1, Size1, MaxSize),
        (
         LogProb=LogProbMax,
         DGraphOut = DGraph,
         OrigGoal1 = OrigGoal
        ;
         prove_go(NewPQ, OrigGoal,
                          LogProb, DGraphOut, TimeInfo, OptionsRecord)
        ).
prove_go(Beam, TargetGoal, LogProb,
                 DGraphOut, TimeInfo, OptionsRecord) :-
        % If the next best is not a solution
        % generate a new beam from the current one
        extend_all(Beam, NewBeam, OptionsRecord),
                       
        !, 
        print_message(informational, beam_size(NewBeam)),

        prove_go(NewBeam, TargetGoal, LogProb,
                         DGraphOut, TimeInfo, OptionsRecord).


%% ----------------------------------------------------------------------
%%      extend_all(BeamIn, BeamOut, OptRec)
%%
extend_all(BeamIn, BeamOut, OptRec) :-
        BeamIn = pq(PqElemsIn, _Size, MaxSize),
        extend_all_go(MaxSize, 0, PqElemsIn, [], [], PqElemsNew0, OptRec),
        take(MaxSize, PqElemsNew0, PqElemsNew),
        length(PqElemsNew, N),
        BeamOut = pq(PqElemsNew, N, MaxSize),
        !.

% extend_all_go(MaxSize, -- maximum size of queue
%               Inf,     -- Infinum of queue (smallest element)
%               PqElems, -- elements left to extend
%               Elems,   -- extended elements, to be inserted into queue
%               PqIn,    -- current queue
%               PqOut,   -- final queue out
%               OptRec   -- options record
extend_all_go(_, _, [], [], PqIn, PqOut, _) :-
        % if no elements to extend and no elements to insert
        !, 
        PqIn = PqOut.
extend_all_go(MaxSize, Inf, [V-K|PqElems], [], PqIn, PqOut, OptRec) :-
        !, 
        % if no elements left to insert, choose extend next element
        pqueue_size(PqIn, Size),
        ( (Size >= MaxSize, V < Inf) ->
          % if queue is saturated and element to be extended is worse
          % than current queue infinum, there's no point in extending
          % this element or any subsequent one (since the elements are
          % in decreasing order)
          PqIn = PqOut
        ;
          % otherwise, extend the next element
          extensions(K, Ks, OptRec),
          (Ks = [] ->
           extend_all_go(MaxSize, Inf, PqElems, [], PqIn, PqOut, OptRec)
          ;
           findall(L-D,
                   (D=deriv_info(_, _, _, L, _),
                    member(D, Ks)),
                   Elems0),
           keysort(Elems0, Elems1),
           reverse(Elems1, Elems),
           extend_all_go(MaxSize, Inf, PqElems, Elems, PqIn, PqOut, OptRec)
          )
        ).
extend_all_go(MaxSize, Inf, PqElems, [V-K|Elems], PqIn, PqOut, OptRec) :-
        pqueue_size(PqIn, Size),
        ((Size >= MaxSize, V < Inf)  ->
         % if pqueue is saturated and next element to be inserted is
         % worse than current queue infinfum, there's no point in
         % inserting it or any subsequent elements
         PqIn = PqOut
        ;
         % otherwise, insert element
         add_to_pqueue(PqIn, V, K, PqTmp),
         % if V is smaller than infinum, set new infinum to V
         (V < Inf ->
          Inf1 = V
         ;
          Inf1 = Inf
         ),
         extend_all_go(MaxSize, Inf1, PqElems, Elems, PqTmp, PqOut, OptRec)
        ).
        


%% ----------------------------------------------------------------------

:- begin_tests(extend).
test(extend,
     [setup(setup_test_gl),
      true(Next=5)]
     ) :-
    T = s(_ | [a]),
    gl_term_surface_form(X, T),
    G = pterm(X),
    
    DInfo = deriv_info([G], [], X, 0, dgraph(1, [1-X], [])),
    extensions(DInfo, Extensions, []),
    length(Extensions, Next).

% test(extend2,
%      [setup(setup_test_gl),
%       true(Next=1)]
%      ) :-
%     X = s([a,a,a,a], [a]),
%     gl_term_surface_form(T, X),
%     G = goal(1, T), 
    
%     DInfo = deriv_info([G], _, 0, dgraph(_, [1-G], [])),
%     extend(DInfo, Extensions, []),
%     length(Extensions, Next).

:- end_tests(extend).

ready(T) :-
        var(T),
        !,
        throw(error(instantiation_error(ready/1, context('Argument must be nonvar')))).
        
ready(pterm(Term)-_) :-
        !,
        ready(Term).
ready(dterm(Term)) :-
        !,
        ready(Term).
ready(append1d(X, Y, Z)) :-
        !,
        (nonvar(X),nonvar(Y) -> true
        ;
         nonvar(Z)
        ).
ready(_).

extensions(DInfo, Extensions, OptRec) :-
        findall(Extension,
                extend(DInfo, Extension, OptRec),
                Extensions).
      

%% ----------------------------------------------------------------------
%%      extend(+DerivInfo, -Extensions, +Options)
%%             
%%
%%  - Goals is a non empty list of of goal terms. 
%%  - A term is either goal(Id, Term) term is a gl_term/3 or pterm(Term) where pterm is a prolog term 
%%  - Extensions is a list of DerivInfo.
%%  - in the deriv_info terms, LogProb is the unconditional probability of the derivation

extend(deriv_info(Goals, Delayed, Orig, LogProb, DGraph), Extension, OptRec) :-
        assertion(nonvar(Delayed)),
        assertion(is_list(Goals)),
        extend(Goals, Delayed, LogProb, DGraph,
               Goals1, Delayed1, LogProb1, DGraph1,
               OptRec),
        Extension = deriv_info(Goals1, Delayed1, Orig, LogProb1, DGraph1).

       
% if there are no goals left
extend([], [], LogProb, DGraph, [], [], LogProb, DGraph, _).
extend([], [_D|_Ds], _, _, _, _, _, _, _) :-
        !,
        throw(error(use_error(extend/3, "Delayed goals remain on goal stack and cannot be executed."))).        

% if the first element is a dterm and it is ready to be called, call it
extend([Term|Rest], Delayed, LogProb, DGraph,
                    Rest1, Delayed1, LogProb1, DGraph1, OptRec) :-
        ready(Term),
        !,
        extend1(Term, LogProb, DGraph, NewGoals, LogProb0, DGraph0, OptRec),
        append(Delayed, NewGoals, NewGoals1),
        append(NewGoals1, Rest, Rest0),
        (Term = dterm(_) ->
         assertion(maplist(nonvar, Rest0)),
         extend(Rest0, [], LogProb0, DGraph0,
                Rest1, Delayed1, LogProb1, DGraph1, OptRec)
        ;
         Term = pterm(_)-_ ->
         Rest1=Rest0,
         Delayed1=[],
         LogProb1 = LogProb0,
         DGraph1 = DGraph0
        ).
        
        
extend([Term|Rest], Delayed, LogProb, DGraph,
                    Rest1, Delayed1, LogProb1, DGraph1, OptRec) :-
        \+ ready(Term),
        !,
        Delayed0 = [Term|Delayed],
        extend(Rest, Delayed0, LogProb, DGraph,
               Rest1, Delayed1, LogProb1, DGraph1, OptRec).


extend1(dterm(Term), LogProb, DGraph,
        [], LogProb, DGraph, _OptRec) :-
         
        call1(Term).

extend1(pterm(Term)-NodeId, LogProb, DGraph,
       BodyList, LogProb1, DGraph1, _OptRec) :-
        
        
        match(Term, BodyList0, RuleId, Prob),

        add_ids_to_bodylist(BodyList0, Children, ChildIds, BodyList),

        pairs_keys_values(ChildNodes, ChildIds, Children),
        add_nodes(DGraph, ChildNodes, DGraph0),
        add_edge(DGraph0, NodeId, RuleId, ChildIds, DGraph1),

        LogProb1 is LogProb + log(Prob).

add_ids_to_bodylist([], [], [], []).
add_ids_to_bodylist([pterm(T)|Rest0], [T|Ts], [Id|Ids], [pterm(T)-Id|Rest]) :-
        !,
        gen_node_id(Id),
        add_ids_to_bodylist(Rest0, Ts, Ids, Rest).
add_ids_to_bodylist([T|Rest0], Ts, Ids, [T|Rest]) :-
        add_ids_to_bodylist(Rest0, Ts, Ids, Rest).
        

reset_gen_node :-
        reset_gensym('node_').
                     
gen_node_id(Id) :-
        gensym('node_', Id).


%% ----------------------------------------------------------------------
%%      match(+Goal, -BodyList, -RuleId, -Prob)
%% 
match(Goal, BodyList, RuleId, Prob) :-
        % find a matching rule for the goal in the db
        Rule=gl_rule(RuleId, Goal, HGuard-BGuard, Body, _),
        call(Rule),
        call1(HGuard),
        
        get_rule_prob(RuleId, Prob),

        and_to_list(Body, BodyList0),
        append(BGuard, BodyList0, BodyList).

%% ----------------------------------------------------------------------
%% call1 is call predicate on lists. Treats lists as conjunctions of
%% goals, and calls sublists recursively.
call1([]) :- !.
call1([G|Gs]) :-
        !,
        call1(G),
        call1(Gs).
call1(G) :-
        functor(G, F, _),
        F \= '|',
        !,
        call(G).

:- begin_tests(match).

test(match,
     [setup(setup_test_gl),
      true(NMatch=5)]
     ) :-
    X = s(_ | [a]),
    gl_term_surface_form(G, X),
    
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
%%      prove_all(Goal, List of deriv(Goal, DGraph, ConditionalProb), Options)
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
:- dynamic prove_all_derivation/1.

prove_all(Goal, Options) :-
        format("Proving ~w: ...\n", [Goal]), 
        prove_all(Goal, Results, LogP, Options),
        format("LogProb: ~w\n", [LogP]), 
        forall(
               member(deriv(Goal, DGraph, Score), Results),
               (format("CondProb: ~w\n", [Score]),
                pprint_dgraph(DGraph),
                nl
               )).
                

prove_all(Goal, Results, LogP) :-
        prove_all(Goal, Results, LogP, []).

prove_all(Goal, Results, LogP, Options) :-
        (is_list(Options) ->
         make_options(Options, OptionsRecord, _)
        ;
         OptionsRecord = Options
        ),
        

        get_time(StartTime),
        retractall(prove_all_derivation(_)),
        
        % side-effect: populate prove_all_derivation/1        
        prove_all_go(StartTime, Goal, OptionsRecord), 
        findall(D, prove_all_derivation(D), Derivations),
        
        % add conditional probability given Goal being true to each
        % derivation
        findall(Score,
                member(deriv(Goal, DGraph, Score), Derivations),
                Scores),



        
        (Scores=[] -> % failed to find any derivations,
         LogP = 0, 
         Results = []
         ;
         log_sum_exp(Scores, LogP),
         findall(deriv(Goal, DGraph, ConditionalProb), 
                 (
                  member(deriv(Goal, DGraph, Score), Derivations),
                  ConditionalProb is exp(Score - LogP)
                 ),
                 Results)
        ),

        findall(Cond,
                member(deriv(_, _, Cond), Results),
                Conds),
        
        print_message(information, expl_search(scores(Scores, Conds))).


prove_all_go(StartTime, _, OptRec) :-
        get_time(Now),
        options_time_limit_seconds(OptRec, TimeLimit), 
        Now-StartTime > TimeLimit,
        writeln((Now-StartTime > TimeLimit)),
        !.
prove_all_go(StartTime, Goal, Options) :- 
        (prove(Goal, Score, DGraph, StartTime, Options),
         Deriv = deriv(Goal, DGraph, Score),
         assertz(prove_all_derivation(Deriv)),
         fail
        ;
         true).

     

        
%% ----------------------------------------------------------------------
%%      Log likelihood of data
log_likelihood(Goal, LogP, Options) :-
        findall(L,
                prove_all(Goal, _, L, Options),
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
%% Score-Elem.
%%
%% MaxSize: the maximum length of the priority queue (default:
%% infinity). Will reject adding an element worse equal to or worse
%% than the current worse element.

flip_pairs(In, Out) :-
        pairs_keys_values(In, Ks, Vs),
        pairs_keys_values(Out, Vs, Ks).

pairs_sort_on_value(PairsIn, PairsOut) :-
        flip_pairs(PairsIn, PairsFlipped),
        keysort(PairsFlipped, SortedFlipped),
        flip_pairs(SortedFlipped, PairsOut).
        

pq_singleton(Elem, Score, pq([Score-Elem], 1, infinity)).
pq_singleton(Elem, Score, MaxSize, pq([Score-Elem], 1, MaxSize)).

pq_empty(MaxSize, pq([], 0, MaxSize)).
pq_empty(pq([], 0, infinity)).


% list_to_pq(KVs, PqSize, PQ)
list_to_pq(Elems, PqSize, PQ) :-
        pairs_sort_on_value(Elems, Sorted0),
        reverse(Sorted0, Sorted),
        findall(S-E,
               member(E-S, Sorted),
               PqElems0),
        take(PqSize, PqElems0, PqElems),
        length(PqElems, N),
        PQ= pq(PqElems, N, PqSize).


% pq_find_max(+PQ, -MaxElem, -Score, -NewPQ)
%
% removes maximum element from priority queue and return the new queue
% without this element. Will throw an error if PQ is empty.
%
pq_find_max(PQ, _, _, _) :-
        pq_empty(PQ),
        !, 
        throw(error(pq_find_max_of_empty(PQ))).
pq_find_max(pq([Score-MaxElem|Rest], Size, MaxSize),
            MaxElem,
            Score,
            pq(Rest, Size1, MaxSize)) :-
        Size1 is Size - 1.

% pq_size(+PQ, -Size)
% get the size of the priority queue
pq_size(pq(_, Size, _), Size).

% pq_show(PQ) pretty prints priority queues
pq_show(pq(Elems, Size, _M), N) :-
        take(N, Elems, Elems1),
        length(Elems1, M1),
        pq_show(pq(Elems1, Size, M1)).

pq_show(pq(Elems, Size, _)) :-
        format("Size: ~w\n", [Size]),
        member(Score-Elem, Elems),
        Elem = deriv_info(_, _, _, _, DGraph),
        pprint_dgraph(DGraph),
        writeln(score-Score), 
        % pprint_raw_dgraph(DGraph),
        % dgraph_dtree(DGraph, DTree), 
        % pprint_raw_dtree(DTree),
        assert_dgraph_tree_property(DGraph),
        fail.
pq_show(_).

% pq_keys(+PQ, -Keys)
% returns a list of keys in the priority queue
pq_keys(pq(Elems, _, _), Keys) :-
        findall(K,
                member(_V-K, Elems),
                Keys).

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
        StartNode = StartNodeId-_,
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
                dtree(NodeId-_, RuleId, Trees)) :-
        member(edge(NodeId, RuleId, ChildNodeIds), HyperEdges),
        !, 
        findall(dtree(ChildNodeId-ChildNode, R, Cs), 
                (member(ChildNodeId, ChildNodeIds),
                 (
                  member(ChildNodeId-ChildNode, Nodes) ->
                  true
                 ;
                  throw(error(ill_formed_dgraph, 'ChildNode not found in Nodes'))
                 ),
                 dgraph_dtree_go(HyperEdges,
                                 Nodes,
                                 dtree(ChildNodeId-ChildNode, R, Cs))), 
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
pprint_dtree(dtree(NodeId-Goal, RuleId, SubTrees), Indent, Cursor) :-
        tab(Cursor),
        write('+ '),
        pprint_term(Goal, GString),
        get_rule(RuleId, Rule),
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

        


log_sum_exp(Xs, Y) :-
        max_list(Xs, MaxX),
        maplist(call(log_sum_exp_go, MaxX), Xs, Xs1),
        % findall(X1,
        %         (member(X, Xs),
        %          X1 is exp(X - MinX)),
        %         Xs1),
        sum_list(Xs1, Z),
        Y is log(Z) + MaxX.

log_sum_exp_go(Xm, X, Y) :-
        Y is exp(X-Xm).


call_list_with_occurs_check([]) :- !.
call_list_with_occurs_check(Gs) :-
        set_prolog_flag(occurs_check, true), 
        call_list_with_occurs_check_(Gs),
        set_prolog_flag(occurs_check, false).

call_list_with_occurs_check_([]).
call_list_with_occurs_check_([G|Gs]) :-
        once(G),
        call_list_with_occurs_check_(Gs).



%% ----------------------------------------------------------------------
%%
%%      Messages
%%

:- multifile
	prolog:message//1.


expl_search_prefix -->
        ['Expl Search:     ' -[]].

prolog:message(expl_search(start(OptionsRecord))) -->
        expl_search_prefix, ['Initializing with options: '-[]], [nl],
        expl_search_prefix, ["~w" -[OptionsRecord]], [nl],
        expl_search_prefix, [nl],
        expl_search_prefix, ['Running ...'-[]], [nl].

prolog:message(expl_search(scores(Scores, Conds))) -->
        {pairs_keys_values(Pairs, Scores, Conds)},
        {keysort(Pairs, Pairs0)},
        {reverse(Pairs0, PairsSorted)},
        {length(PairsSorted, N)}, 
        expl_search_prefix, ['Derivation Scores:'], [nl],
        expl_search_prefix, ['Number: ~d'-[N]], [nl], 
        print_scores_go_(PairsSorted). 

print_scores_go_([]) --> expl_search_prefix, [nl].
print_scores_go_([S-C|Ss]) -->
        expl_search_prefix,
        ['score: ~2f ; cond prob: ~2f'-[S, C]], [nl],
        print_scores_go_(Ss).

prolog:message(beam_size(Beam)) -->
        {pq_size(Beam, Size)},
        expl_search_prefix, ['Beam Size: ~d'-[Size]], [nl].


prolog:message(beam_terms(Beam)) -->
        {Beam=pq(Elems, _, _)},
        beam_terms_go_(Elems).

beam_terms_go_([]) --> expl_search_prefix, [nl].
beam_terms_go_([W-deriv_info(Goals, _, _, _) | Es]) -->
        {(Goals = [_-T|_] ->
          pprint_term(T, TString)
         ;
          TString = finished
         )
         },
         expl_search_prefix,
         ["~w  ~2f"-[TString, W]], [nl],
         beam_terms_go_(Es).

% ----------------------------------------------------------------------
%%  A maximum priority queue of finite size.



empty_pqueue([]).

add_to_pqueue([], V, K, [V-K]) :- !.
add_to_pqueue([V0-K0|In], V, K, Out) :-
        V >= V0,
        !,
        Out = [V-K, V0-K0| In].
add_to_pqueue([V0-K0|In], V, K, Out) :-
        V < V0,
        !,
        Out = [V0-K0 | Tmp],
        add_to_pqueue(In, V, K, Tmp).

list_to_pqueue(In, PQ) :-
        keysort(In, PQ0),
        reverse(PQ0, PQ).

pqueue_to_list(In, In).

pqueue_size(Pq, N) :-
        length(Pq, N).

        
        
        

        



        
        
            

        


                 
                        
        

        
        
        
        
        
                    
        



                 
                 
                 
                                         
                                              
                                         
                 
                 
                
        
        
        


