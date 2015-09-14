%% ----------------------------------------------------------------------
%% prove.gl
%% module: prove
%% author: Eyal Dechter
%% date: July 2015
%% Public domain code
%%
%% Summary: this module contains basic routines for beam search on
%% GenLog goals.
%% ----------------------------------------------------------------------


:- module(prove,
          [
           
           prove/4,       % prove(+Goal, -Score, -DGraph, ?StartTime)
           prove/5,       % prove(+Goal, -Score, -DGraph, ?StartTime, +Options) :-

           prove_all/2,
           prove_all/3,
           prove_all/4
           
           ]).

%% External imports
:- use_module(library(record)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(option)).
:- use_module(library(gensym)).

%% Local imports
:- use_module(gl_term).
:- use_module(gl_rule).
:- use_module(dgraph).
:- use_module(array).
:- use_module(misc).
:- use_module(plunit_extra).

%% ----------------------------------------------------------------------
%%    Custom Type Definitions

:- multifile error:has_type/2.

error:has_type(timestamp, T) :-
        number(T),
        T > 0.

error:has_type(pq, T) :-
        compound(T), 
        functor(T, pq, 3).

error:has_type(non_empty_list, Xs) :-
        error:has_type(list_or_partial_list, Xs),
        length(Xs, N),
        N > 0.

%% ----------------------------------------------------------------------
:- nb_setval(next_node_id, 0).

reset_next_node_id :-
        nb_setval(next_node_id, 0).

get_next_node_id(Id) :-
        nb_getval(next_node_id, Id),
        Id1 is Id + 1,
        nb_setval(next_node_id, Id1).
        
        
%% ----------------------------------------------------------------------
%% ----------------------------------------------------------------------
%%     prove_all(Goal, Options)
%%     prove_all(Goal, Results, LogP),
%%     prove_all(Goal, Results, LogP, Options)
%%
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
        must_be(nonvar, Goal),
        format("Proving ~w: ...\n", [Goal]), 
        prove_all(Goal, Results, LogP, Options),
        format("LogProb: ~w\n", [LogP]), 
        forall(
               member(deriv(Goal, DGraph, Score), Results),
               (format("CondProb: ~w\n", [Score]),
                pprint_dgraph(DGraph),
                nl
               )).

sort_results_by_prob(Derivs, Derivs1) :-
        findall(S-D,
                (D = deriv(_, _, S), 
                 member(D, Derivs)),
                SDs0),
        keysort(SDs0, SDs1),
        reverse(SDs1, SDs2),
        pairs_values(SDs2, Derivs1).
                 
                

prove_all(Goal, Results, LogP) :-
        prove_all(Goal, Results, LogP, []).

prove_all(Goal, Results, LogP, Options) :-
        print_message(information,
                      begin(prove_all(goal(Goal), options(Options)))),
                                      
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
        sort_results_by_prob(Derivations, Derivations1),

        
        % add conditional probability given Goal being true to each
        % derivation
        findall(Score,
                member(deriv(Goal, DGraph, Score), Derivations1),
                Scores),



        
        (Scores=[] -> % failed to find any derivations,
         LogP = 0, 
         Results = []
         ;
         log_sum_exp(Scores, LogP),
         findall(deriv(Goal, DGraph, ConditionalProb), 
                 (
                  member(deriv(Goal, DGraph, Score), Derivations1),
                  ConditionalProb is exp(Score - LogP)
                 ),
                 Results)
        ),

        % sort_results_by_prob(Results0, Results), 

        findall(Cond,
                member(deriv(_, _, Cond), Results),
                Conds),
        
        print_message(information, expl_search(scores(Scores, Conds))),
        print_message(information, end(prove_all(goal(Goal)))).


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
%% ----------------------------------------------------------------------
%%      prove(+Goal, -Score, -DGraph, +StartTime)
%%      prove(+Goal, -Score, -DGraph, +StartTime, +Options)
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
:- record options(
                  % beam_width: the size of the search beam
                  beam_width:integer=100,

                  % time_limit_seconds: the maximum time spent searching for proofs 
                  time_limit_seconds:integer=2                
                 ).

prove(Goal, Score, DGraph, StartTime) :-
        prove(Goal, Score, DGraph, StartTime, []). 

prove(Goal, Score, DGraph, StartTime, Options) :-
        % type checking
        must_be(var, Score),
        
        % set start time if not set
        (StartTime=now ->
         get_time(StartTime1)
        ;
         must_be(timestamp, StartTime),
         StartTime1 = StartTime
        ),
        
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
        reset_next_node_id,
        get_next_node_id(NodeId),
        
        % initial list of goals
        GoalList = [pterm(GoalTr)-NodeId],


        % Initialize priority queue. Each element of priority queue is
        % of the form deriv_info(GoalList, OrigGoal, LogProb, DGraph)
        DGraph0 = dgraph(NodeId, [NodeId-GoalTr], []),

        options_beam_width(OptionsRecord, BeamWidth),
        pq_singleton(deriv_info(GoalList, GoalTr, 0, DGraph0),
                     0,
                     BeamWidth,
                     PQ),

        prove_go(PQ, GoalTr, Score,
                         DGraph, 
                         StartTime1,
                         OptionsRecord).


        

%% ----------------------------------------------------------------------
%%      prove_go(+PQ, OrigGoal, Score, DGraph, StartTime, +OptionsRecord)
%%                   
%%      Worker predicate for prove.
prove_go(_, _, _, _, StartTime, Opts) :-
        must_be(timestamp, StartTime), 
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
        %% If any solutions in PQ, select one to return.
        PQ = pq(PqElems, Size, MaxSize),
        DInfo = deriv_info([], OrigGoal1,LogProbMax, DGraph),
        select(_-DInfo, PqElems, PqElems1),
        !,
        Size1 is Size - 1,
        NewPQ = pq(PqElems1, Size1, MaxSize),
        (
         LogProb=LogProbMax,
         DGraphOut = DGraph,
         OrigGoal1 = OrigGoal
        ;
         % continue to return any subsequent solution.
         prove_go(NewPQ, OrigGoal,
                          LogProb, DGraphOut, TimeInfo, OptionsRecord)
        ).
prove_go(Beam, OrigGoal, LogProb,
                 DGraphOut, TimeInfo, OptionsRecord) :-
        % generate a new beam from the current one
        extend_all(Beam, NewBeam, OptionsRecord),
        % pq_show(Beam, 1),               
        !, 
        print_message(informational, beam_size(NewBeam)),
        
        prove_go(NewBeam, OrigGoal, LogProb,
                 DGraphOut, TimeInfo, OptionsRecord).


%% ----------------------------------------------------------------------
%%      extend_all(+BeamIn, -BeamOut, +OptRec)
%%
extend_all(BeamIn, BeamOut, OptRec) :-
        must_be(pq, BeamIn),
        
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
        K = deriv_info([G|_], _, _, _),
        % nl,
        % writeln(G),
        % nl,
        pqueue_size(PqIn, Size),
        ( (Size >= MaxSize, V < Inf) ->
          % if queue is saturated and element to be extended is worse
          % than current queue infinum, there's no point in extending
          % this element or any subsequent one (since the eleproments are
          % in decreasing order)
          PqIn = PqOut
        ;
          % otherwise, extend the next element
          extensions(K, Ks, OptRec),
          % nl,
          % format('Extending ~w\n', [K]),
          % forall(member(E, Ks),
          %        writeln(E)),
          % nl,
          (Ks = [] ->
           extend_all_go(MaxSize, Inf, PqElems, [], PqIn, PqOut, OptRec)
          ;
           findall(L-D,
                   (D=deriv_info(_, _, L, _),
                    member(D, Ks)),
                   Elems0),
           keysort(Elems0, Elems1),
           reverse(Elems1, Elems),
           extend_all_go(MaxSize, Inf, PqElems, Elems, PqIn, PqOut, OptRec)
          )
        ).
extend_all_go(MaxSize, Inf, PqElems, [V-K|Elems], PqIn, PqOut, OptRec) :-
        % length(Elems, L),
        % length(PqElems, PL), 
        % format('extend_all_go: NElems: ~p, NPQElems: ~p\n', [L, P]),
        % writeln(Inf),
        pqueue_size(PqIn, Size),
        % writeln(debug(v(V), inf(Inf), size(Size), maxSize(MaxSize))),
        ((Size >= MaxSize, V < Inf)  ->
         % if pqueue is saturated and next element to be inserted is
         % worse than current queue infinfum, there's no point in
         % inserting it or any subsequent elements
         PqIn = PqOut
        ;
         % otherwise, insert element
         add_to_pqueue(PqIn, V, K, PqTmp0),
         take(MaxSize, PqTmp0, PqTmp),
         % if V is smaller than infinum, set new infinum to V
         (V < Inf ->
          Inf1 = V
         ;
          last(PqTmp, Inf1-_)
         ),
         extend_all_go(MaxSize, Inf1, PqElems, Elems, PqTmp, PqOut, OptRec)
        ).
        

%% ----------------------------------------------------------------------
%%     extensions(DInfo, Extensions, OptRec)
%%
%% Extensions is the list of one-step extensions to derivation state
%% DInfo.
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

extend(deriv_info(Goals, Orig, LogProb, DGraph), Extension, OptRec) :-
        must_be(non_empty_list, Goals),
        must_be(number, LogProb),
        must_be(dgraph, DGraph),
        must_be(var, Extension),
        
        extend(Goals, LogProb, DGraph,
               Goals1, LogProb1, DGraph1,
               OptRec),
        Extension = deriv_info(Goals1, Orig, LogProb1, DGraph1).

       
% if there are no goals left
extend([],  LogProb, DGraph, [], LogProb, DGraph, _).

% if the first element is a dterm and it is ready to be called, call it
extend([Term|Rest], LogProb, DGraph,
       Rest1, LogProb1, DGraph1, OptRec) :-
        extend1(Term, LogProb, DGraph, NewGoals, LogProb0, DGraph0, OptRec),
        append(NewGoals, Rest, Rest0),
        (Term = dterm(_) ->
         assertion(maplist(nonvar, Rest0)),
         extend(Rest0, LogProb0, DGraph0,
                Rest1, LogProb1, DGraph1, OptRec)
        ;
         Term = pterm(_)-_ ->
         Rest1=Rest0,
         LogProb1 = LogProb0,
         DGraph1 = DGraph0
        ;
         throw(error(
               system_error,
               context(Term, 'must be either pterm(_) or dterm(_)')))
        ).
        
% extend([Term|Rest], LogProb, DGraph,
%                     Rest1, LogProb1, DGraph1, OptRec) :-
%         \+ ready(Term),
%         !,
%         extend(Rest, LogProb, DGraph,
%                Rest1, LogProb1, DGraph1, OptRec).


% ----- extend1 
extend1(dterm(Term), LogProb, DGraph,
        [], LogProb, DGraph, _OptRec) :-
        call(Term).

extend1(pterm(Term)-NodeId, LogProb, DGraph,
       BodyList, LogProb1, DGraph1, _OptRec) :-
        
        
        match(Term, BodyList0, RuleId, Prob),

        add_ids_to_bodylist(BodyList0, Children, ChildIds, BodyList),

        pairs_keys_values(ChildNodes, ChildIds, Children),
        add_nodes(DGraph, ChildNodes, DGraph0),
        add_edge(DGraph0, NodeId, RuleId, ChildIds, DGraph1),

        catch(
              LogProb1 is LogProb + log(Prob),
              error(E, _),
              (writeln(Prob),
               fail)).
               % LogProb1 is -9e-100)).

% ----- add_ids_to_bodylist 
add_ids_to_bodylist([], [], [], []).
add_ids_to_bodylist([pterm(T)|Rest0], [T|Ts], [Id|Ids], [pterm(T)-Id|Rest]) :-
        !,
        get_next_node_id(Id),
        add_ids_to_bodylist(Rest0, Ts, Ids, Rest).
add_ids_to_bodylist([T|Rest0], Ts, Ids, [T|Rest]) :-
        add_ids_to_bodylist(Rest0, Ts, Ids, Rest).
        
%% ----------------------------------------------------------------------
%%      Preds for constructing and resetting node ids
reset_gen_node :-
        reset_gensym('node_').
                     
gen_node_id(Id) :-
        gensym('node_', Id).



%% ----------------------------------------------------------------------
%% ----------------------------------------------------------------------
%%      match(+Goal, -BodyList, -RuleId, -Prob)
%% 
match(Goal, BodyList, RuleId, Prob) :-
        must_be(nonvar, Goal),
        must_be(var, BodyList),
        must_be(var, RuleId),
        must_be(var, Prob),
        
        % find a matching rule for the goal in the db
        Rule=gl_rule(RuleId, Goal, HGuard-BGuard, Body, _),
        call(Rule),
        call1(HGuard),
        
        call1(BGuard),
        
        
        get_rule_prob(RuleId, Prob),

        and_to_list(Body, BodyList).
        % append(BGuard, BodyList0, BodyList).



     

        

%% ----------------------------------------------------------------------
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

pq_singleton(Elem, Score, pq([Score-Elem], 1, infinity)).
pq_singleton(Elem, Score, MaxSize, pq([Score-Elem], 1, MaxSize)).

pq_empty(MaxSize, pq([], 0, MaxSize)).
pq_empty(pq([], 0, infinity)).

% pq_size(+PQ, -Size)
% get the size of the priority queue
pq_size(pq(_, Size, _), Size).

% pq_show(PQ)
% pretty prints priority queues
pq_show(pq(Elems, Size, _M), N) :-
        take(N, Elems, Elems1),
        length(Elems1, M1),
        pq_show(pq(Elems1, Size, M1)).

pq_show(pq(Elems, Size, _)) :-
        format("Size: ~w\n", [Size]),
        member(Score-Elem, Elems),
        Elem = deriv_info(_, _, _, DGraph),
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
        


%% ----------------------------------------------------------------------
%% ----------------------------------------------------------------------
%%      log_sum_exp(+Xs, -Y)

log_sum_exp(Xs, Y) :-
        max_list(Xs, MaxX),
        maplist(call(log_sum_exp_go, MaxX), Xs, Xs1),
        sum_list(Xs1, Z),
        Y is log(Z) + MaxX.

log_sum_exp_go(Xm, X, Y) :-
        Y is exp(X-Xm).




%% ----------------------------------------------------------------------
%%
%%      Messages
%%

:- multifile
	prolog:message//1.


expl_search_prefix -->
        ['Expl Search:     ' -[]].

prolog:message(begin(prove_all(goal(Goal), options(_Options)))) -->
        expl_search_prefix, ['Beginning prove_all explanation search for goal ~w ...'-Goal], [nl].        
prolog:message(end(prove_all(goal(Goal)))) -->
        expl_search_prefix, ['Finished prove_all explanation search for goal ~w.'-Goal], [nl].        
                            
         

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
        [nl],
        expl_search_prefix, ['~| ~`-t ~5+ Derivation Scores ~| ~`-t ~5+'], [nl],
        expl_search_prefix, ['Number of derivations found: ~d'-[N]], [nl],
        expl_search_prefix, ['Top 10 derivations:'], [nl],
        {take(10, PairsSorted, PairsSorted1)}, 
        print_scores_go_(PairsSorted1). 

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
beam_terms_go_([W-deriv_info(Goals, _, _) | Es]) -->
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

add_to_pqueue(In, V, K, Out) :-
        length(In, L),
        add_to_pq_df(In, V, K, Out).

add_to_pq_df(In, V, K, Out) :-
        add_to_pq_df_(H-H, In-[], V, K, Out-[]).
add_to_pq_df_(H-[V-K|R2], L-L, V, K, H-R2) :-
        !.
add_to_pq_df_(H-[V-K, V0-K0|L1], [V0-K0|L1]-L2, V, K, Out) :-
        V >= V0,
        !,
        Out = H-L2.
add_to_pq_df_(H-[V0-K0|H0], [V0-K0|L1]-L2, V, K, Out) :-
        V < V0,
        add_to_pq_df_(H-H0, L1-L2, V, K, Out).        

list_to_pqueue(In, PQ) :-
        keysort(In, PQ0),
        reverse(PQ0, PQ).

pqueue_to_list(In, In).

pqueue_size(Pq, N) :-
        length(Pq, N).

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
        

:- end_tests(prove).
 
%% ----------------------------------------------------------------------

:- begin_tests(extend).
test(extend,
     [setup(setup_test_gl),
      true(Next=5)]
     ) :-
    T = s(_ | [a]),
    gl_term_surface_form(X, T),
    G = pterm(X),
    
    DInfo = deriv_info([G-1], X, 0, dgraph(1, [1-X], [])),
    extensions(DInfo, Extensions, []),

    length(Extensions, Next).

:- end_tests(extend).


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
 

        

