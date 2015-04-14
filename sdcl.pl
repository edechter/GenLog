%% pdcg.pl
%% author: Eyal Dechter

%% TODO
%% - Change all computations to log domain

:- use_module(library(record)).

:- op(1000, xfy, --->).
:- op(1000, xfy, <--).
:- op(1200, xfy, ::).

%% ----------------------------------------------------------------------
%%
%% compile_sdcl/1 compiles loaded sdcl rules
%%
%% - provides each sdcl rule with a rule representation and
%%   asserts(sdcl_rule(RuleRepr, Rule)).
compile_sdcl :-
        retractall(sdcl_rule(_, _)),
        !,
        (
         (
          Rule = (_ <-- _ :: _),
          call(Rule)
         ;
          Rule1 = (F :: P),
          call(Rule1),         
          F \= (_ <-- _), 
          Rule = (F <-- true :: P)
         ),
         canonical_rule(Rule, RuleRepr),
         assertz(sdcl_rule(RuleRepr, Rule)),
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
mi_best_first(Goal, Score, DGraph, Options) :-
        % make options record
        make_bf_options(Options, OptionsRecord), 
        % initialize priority queue
        GoalList = [Goal],
        
        bf_options_beam_width(OptionsRecord, BeamWidth),

        % initialize priority queue. Each element of priority queue is
        % of the form deriv_info(CurrentGoalList-OrigGoal, DerivationGraph)
        pq_singleton(deriv_info(GoalList-Goal, dgraph(Goal, [])),
                     1, BeamWidth, PQ),
        
        bf_options_inference_limit(OptionsRecord, InferenceLimit),

        call_with_inference_limit(mi_best_first_go(PQ, Goal, Score, DGraph, OptionsRecord),
                                  InferenceLimit,
                                  Result),
        (Result = inference_limit_exceeded ->
         format("***Inference Limit Exceeded***\n"), 
         fail;
         true).
              
        
%% ----------------------------------------------------------------------
%% Options record for mi_best_first

:- record bf_options(beam_width=infinity,
                     inference_limit=1000
                    ).
%% ----------------------------------------------------------------------
%% mi_best_first_go/5.
%% main worker predicate for mi_best_first
%%
%% mi_best_first_go(+PQ, OrigGoal, Score, DGraph, +OptionsRecord) 
mi_best_first_go(PQ, _, _, _, _) :-
        % if PQ is empty, fail.
        pq_empty(PQ), !, fail.
mi_best_first_go(PQ, Goal, ScoreOut, DGraphOut, OptionsRecord) :-
        % Solution is found, return goal.
        % Continue to next goal on backtracking.
        pq_find_max(PQ, deriv_info([]-OrigGoal, DGraph), Score, NewPQ),
        !, 
        (
         Goal = OrigGoal,
         ScoreOut = Score,
         DGraphOut = DGraph
        ;
         mi_best_first_go(NewPQ, Goal, ScoreOut, DGraphOut, OptionsRecord)
        ).
        
mi_best_first_go(PQ, OrigGoal, ScoreOut, DGraphOut, OptionsRecord) :-
        % If the next best is not a solution
        % get the best solution from priority queue
        pq_find_max(PQ, Elem, Score, PQ1),
        % extend best solution
        extend(Elem, Score, ElemScores),
        %--DEBUG
        % format("Extend ~w with score ~w to \n", [Elem, Score]),
        % (
        %  member(E-S, ElemScore), 
        %  format("~w:  ~w \n", [S, E]),
        %  fail
        % ;
        %  true
        % ),
        %--END DEBUG
        % insert extensions into priority queue
        pq_inserts(ElemScores, PQ1, NewPQ),
        %---DEBUG
        % format("Priority Queue: \n"),
        % pq_show(NewPQ),
        %---END DEBUG
        
        % loop
        mi_best_first_go(NewPQ, OrigGoal, ScoreOut, DGraphOut, OptionsRecord).
        

        

%% extend(deriv_info(Goals-OrigGoal, DGraph), Score, Extensions)
%% assumptions:
%% - Goals is not empty
%% - Goals is a list of literals
%% - Extensions is a list of pairs deriv_info(Goals-OrigGoal, DGraph)-Score
extend(deriv_info([G|Rest]-OrigGoal, DGraph), Score, ElemScores) :-
        DGraph = dgraph(StartGoal, HyperEdges),
        findall(deriv_info(G_new-OrigGoal_copy, DGraph_new)-Score_new,
                (
                 copy_term([G|Rest]-OrigGoal, [G_copy|Rest_copy]-OrigGoal_copy),
                 match(G_copy, BodyList, Rule-RuleRepr, S),
                 Score_new is Score * S,
                 DGraph_new = dgraph(StartGoal, [hyperedge(G_copy, RuleRepr, BodyList)|HyperEdges]),
                 append(BodyList, Rest_copy, G_new)
                ),
                ElemScores
               ).

%% match goal against SDCL db
%% match(Literal, BodyList, RuleCopy, Score)
match(Literal, BodyList, Rule-RuleRepr, Prob) :-
        findall(OrigProb,
                sdcl_rule(_, Literal <-- Body :: OrigProb), 
                OrigProbs),
        sum_list(OrigProbs, Z),
        !,
        sdcl_rule(RuleRepr, Literal <-- Body :: OrigProb), 
        Prob is OrigProb / Z, 
        and_to_list(Body, BodyList).

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

%% ----------------------------------------------------------------------
%%      mi_best_first_all(Goal, List of deriv(Goal, DGraph, Score), Options)
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
mi_best_first_all(Goal, Results, Options) :-
        findall(deriv(Goal, DGraph, Score), 
                mi_best_first(Goal, Score, DGraph, Options),
                Results).
        

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
pq_insert_sorted_list(Elem, Score, [], [pq_elem(Elem, Score)]).
% if Score is greater than or equal to score at head of list
pq_insert_sorted_list(Elem, Score, [pq_elem(E, S)|Xs], [pq_elem(Elem, Score), pq_elem(E, S) | Xs]) :-
        Score >= S.
% if Score is less than score at head of list, recurse
pq_insert_sorted_list(Elem, Score, [pq_elem(E, S)|Xs], [pq_elem(E, S) | Xs1]) :-
        Score < S, 
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
        member(pq_elem(Elem, Score), Elems),
        format("~w:  ~w \n", [Score, Elem]),
        fail.
pq_show(_).


%% ----------------------------------------------------------------------
%% Derivation data structure
%% 
%% A data structure for constructing derivation graphs and trees
%% during best first search. 
%%
%% A derivation graph is a structure dgraph(start goal, list of hyperedges)
%% Each hyperedge is a structure hyperedge(Goal, Rule, ChildGoals).
%%
%% This graph is a multiway tree, so we also supply a predicate to
%% construct trees out of these graphs.
%%
%% A derivation tree is a structure: dtree(Goal, Rule, [list of dtree]).

dgraph_dtree(DGraph, DTree) :-
        DGraph = dgraph(Start, _), 
        DTree = dtree(Start, _, _), 
        dgraph_dtree_go(DGraph, DTree).
dgraph_dtree_go(dgraph(Start, HyperEdges), dtree(Goal, Rule, Trees)) :-
        member(hyperedge(Goal, Rule, Children), HyperEdges),
        !,
        findall(dtree(Child, R, Cs), 
                (member(Child, Children),
                 dgraph_dtree_go(
                                 dgraph(Start, HyperEdges),
                                 dtree(Child, R, Cs)))
                ,
                 Trees).

% pprint_dtree(DTree) pretty prints a derivation tree DTree
pprint_dtree(DTree) :-
        pprint_dtree(DTree, 2).
pprint_dtree(DTree, Indent) :-
        pprint_dtree(DTree, Indent, 0).
pprint_dtree(dtree(Goal, Rule, SubTrees), Indent, Cursor) :-
        tab(Cursor),
        write('+ '),
        portray_clause(Rule),
        nl,
        !,
        (
        Cursor1 is Cursor + Indent,
        member(SubTree, SubTrees),
        pprint_dtree(SubTree, Indent, Cursor1),
        fail
        ;
        true
        ).

canonical_rule(Rule, Canonical) :-
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


%% ----------------------------------------------------------------------

s(X, Y) <-- np(Number, X, Z), vp(Number, Z, Y)     :: 0.75.
s(X, Y) <-- s(X, Y), s(X, Y) :: 0.1.

np(Number, X, Y) <-- pn(Number, X, Y)                     :: 1.
np(Number, X, Y) <-- det(Number, X, Z), n(_, Number, Z, Y)      :: 1.

vp(Number, X, Y) <-- v(intransitive, Number, X, Z), np(_, Z, Y) :: 0.6.
vp(Number, X, Y) <-- v(transitive, Number, X, Y)          :: 0.4.

pp(X, Y) <-- prep(X, Z), np(_, Z, Y) :: 1.

pn(singular, [eyal|X], X) :: 0.5.
pn(plural, [they|X], X)   <-- true   :: 0.5.

n(book, singular, [book|X], X) <-- true  :: 1.
n(book, plural, [books|X], X) <-- true  :: 1.
n(song, singular, [song|X], X) <-- true  :: 1.
n(song, plural, [songs|X], X) <-- true  :: 1.

v(transitive, singular, [halts|X], X) <-- true :: 1.0.
v(transitive, plural , [halt|X], X) <-- true  :: 1.0.
v(transitive, singular, [sees|X], X) <-- true  :: 1.0.

v(intransitive, singular, [writes|X], X) <-- true:: 0.5.
v(intransitive, plural  , [write|X], X) <-- true :: 0.5.

det(singular, [the|X], X) <-- true:: 1.
det(singular, [a|X], X) <-- true  :: 1.
det(plural  , [the|X], X) <-- true:: 1.
det(plural  , X, X) <-- true   :: 1.

% pn(singular, [eyal|Z], Z) <-- true :: 2.1.
% pn(singular, [amy|Z], Z) <-- true :: 3.2.
% pn(singular, [dog|Z], Z) <-- true :: 2.1.
% pn(singular, [whale|Z], Z) <-- true :: 3.2.

% pn(plural, [they|Z], Z) <-- true :: 2.
% pn(plural, [people|Z], Z) <-- true :: 0.3.

% vp(singular, [runs|Z], Z) <-- true :: 1.
% vp(plural, [run|Z], Z) <-- true :: 1.



        
        
            

        


                 
                        
        

        
        
        
        
        
                    
        



                 
                 
                 
                                         
                                              
                                         
                 
                 
                
        
        
        



%% ------------------------------------------------------------------
%% example dcg

% s ---> np(Number), vp(Number)     :: 1.

% np(Number) ---> pn(Number)                     :: 1.
% np(Number) ---> det(Number), n(_, Number)      :: 1.

% vp(Number) ---> v(intransitive, Number), np(_) :: 0.6.
% vp(Number) ---> v(transitive, Number)          :: 0.4.

% pp ---> prep, np(_) :: 1.

% pn(singular) ---> [eyal]   :: 0.5.
% pn(plural)   ---> [they]   :: 0.5.

% n(book, singular)  ---> [book]   :: 1.
% n(book, plural)    ---> [books]   :: 1.
% n(song, singular)  ---> [song]   :: 1.
% n(song, plural)    ---> [songs]   :: 1.

% v(transitive, singular) ---> [halts]  :: 1.0.
% v(transitive, plural)  ---> [halt]   :: 1.0.
% v(transitive, singular) ---> [see]   :: 1.0.

% v(intransitive, singular) ---> [writes] :: 0.5.
% v(intransitive, plural)   ---> [write]  :: 0.5.

% det(singular) ---> [the] :: 1.
% det(singular) ---> [a]   :: 1.
% det(plural)   ---> [the] :: 1.
% det(plural)   ---> []    :: 1.




%% ----------------------------------------------------------------------

