%% pdcg.pl
%% author: Eyal Dechter



:- op(1000, xfy, --->).
:- op(1000, xfy, <--).
:- op(1200, xfy, ::).


%% ----------------------------------------------------------------------
%%      rule_prob(+Literal, -Rhs, -Prob) is nondet
%%
%%      Literal:  Unifies with the head of a SDCG rule.
%%      Rhs:      The right hand side of the selected rule.
%%      Prob:     The probability of this rule after renormalizing on all applicable rules.
%%
%%      Description: Returns a Rhs for proving a Literal, and returns
%%      the probability of that rule reweighted by the sum of the
%%      probabilities of all unifying rules. Returns all unifying
%%      rules on backtracking.

rule_prob(Literal, Rhs, Prob) :-
        findall(OrigProb,
                (Literal ---> Rhs :: OrigProb),
                OrigProbs),
        sumlist(OrigProbs, Z),
        !,
        (Literal ---> Rhs :: OrigProb),
        Prob is OrigProb / Z. 
                
%% ----------------------------------------------------------------------


%% ----------------------------------------------------------------------
%%      parse(NT, Trees, Prob) is nondet
%%
%%      NT:       non-terminal
%%      Trees:    constituent parse trees
%%      Prob:     the probability of the parse tree
%%
%%      Description: Compute all parse trees of input and the
%%      probability of each under the sdcg
%%
%% ----------------------------------------------------------------------

parse(NT, [Tree], POut) -->
        {rule_prob(NT, Body, PRule)},
        parse(Body, SubTrees, PSubTrees),
        {Tree =.. [node, NT, SubTrees]},
        {POut is PRule * PSubTrees}.

parse((Body1, Body2), Trees, POut) -->
        parse(Body1, Trees1, P1),
        parse(Body2, Trees2, P2),
        {append(Trees1, Trees2, Trees)},
        {POut is P1 * P2}.        

parse([], [], 1) --> [].
parse([Word|Rest], [leaf(Word)|Trees], P) -->
        [Word],
        parse(Rest, Trees, P).

parse({Goals}, [], 1) --> {call(Goals)}.


%% ----------------------------------------------------------------------
%%      parse_best_order(NT, [Tree], P) is nondet
%%
%%      NT:     nonterminal
%%      [Tree]: a parse tree of the input
%%      P:      the probability of the parse
%%
%%      Description: Returns the highest probability parse trees of NT
%%      in order of decreasing probability.

%% ----------------------------------------------------------------------


%% ----------------------------------------------------------------------
%%      meta_best_first(Goal)
%%
%%      Description: A best first meta interpreter. 

%% ----------------------------------------------------------------------
mi_best_first(Goal, Score, BeamWidth) :-
        % initialize priority queue
        GoalList = [Goal], 
        pq_singleton(GoalList-Goal, 1, BeamWidth, PQ), 
        mi_best_first_go(PQ, Goal, Score).

% mi_best_first_go(+PQ, OrigGoal, Score) 
mi_best_first_go(PQ, _, _) :-
        % if PQ is empty, fail.
        pq_empty(PQ), !, fail.
mi_best_first_go(PQ, Goal, ScoreOut) :-
        % Solution is found, return goal.
        % Continue to next goal on backtracking.
        pq_find_max(PQ, []-OrigGoal, Score, NewPQ),
        !, 
        (
         Goal = OrigGoal,
         ScoreOut = Score
        ;
         mi_best_first_go(NewPQ, Goal, ScoreOut)
        ).
mi_best_first_go(PQ, OrigGoal, ScoreOut) :-
        % If the next best is not a solution
        % get the best solution from priority queue
        pq_find_max(PQ, Elem, Score, PQ1),
        % extend best solution
        extend(Elem, Score, ElemScores),
        %--DEBUG
        % format("Extend ~w with score ~w to \n", [Elem, Score]),
        % (
        %  member(E-S, ElemScores), 
        %  format("~w:  ~w \n", [S, E]),
        %  fail
        % ;
        %  true
        % ),
        %--END DEBUG
        % insert extensions into priority queue
        pq_inserts(ElemScores, PQ1, NewPQ),
        NewPQ = pq(Elems, Size, MaxSize),
        length(Elems, L),
        format("\n Size: ~w \n MaxSize: ~w \n Length : ~w \n\n",
               [Size, MaxSize, L]), 
        %---DEBUG
        % format("Priority Queue: \n"),
        % pq_show(NewPQ),
        %---END DEBUG        
        % loop
        
        mi_best_first_go(NewPQ, OrigGoal, ScoreOut).

%% extend(Goals-OrigGoal, Score, Extensions)
%% assumptions:
%% - Goals is not empty
%% - Goals is a list of literals
%% - Extensions is a list of pairs (Goals-OrigGoal)-Score
extend([G|Rest]-OrigGoal, Score, ElemScores) :-        
        findall((G_new-OrigGoal_copy)-Score_new,
                (
                 copy_term([G|Rest]-OrigGoal, [G_copy|Rest_copy]-OrigGoal_copy),
                 match(G_copy, BodyList, S),
                 Score_new is Score * S,
                 append(BodyList, Rest_copy, G_new)
                ),
                ElemScores
               ).

%% match goal against SDCL db
%% match(Literal, BodyList, Score)
match(Literal, BodyList, Prob) :-
        findall(OrigProb,
                (Literal <-- Body :: OrigProb),
                OrigProbs),
        sumlist(OrigProbs, Z),
        !,
        (Literal <-- Body :: OrigProb),
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
take(0, _, []) :- !.
take(_, [], []).
take(N, [X|Xs], [X|Ys]) :-
        N1 is N - 1,
        take(N1, Xs, Ys).

%% ----------------------------------------------------------------------

s(X, Y) <-- pn(Number, X, Z), vp(Number, Z, Y)     :: 1.
s(X, Y) <-- s(X, Z), s(Z, Y)     :: 0.1.

pn(singular, [eyal|Z], Z) <-- true :: 2.1.
pn(singular, [amy|Z], Z) <-- true :: 3.2.

pn(plural, [they|Z], Z) <-- true :: 2.
pn(plural, [people|Z], Z) <-- true :: 0.3.

vp(singular, [runs|Z], Z) <-- true :: 1.
vp(plural, [run|Z], Z) <-- true :: 1.



        
        
            

        


                 
                        
        

        
        
        
        
        
                    
        



                 
                 
                 
                                         
                                              
                                         
                 
                 
                
        
        
        



%% ------------------------------------------------------------------
%% example dcg

s ---> np(Number), vp(Number)     :: 1.

np(Number) ---> pn(Number)                     :: 1.
np(Number) ---> det(Number), n(_, Number)      :: 1.

vp(Number) ---> v(intransitive, Number), np(_) :: 0.6.
vp(Number) ---> v(transitive, Number)          :: 0.4.

pp ---> prep, np(_) :: 1.

pn(singular) ---> [eyal]   :: 0.5.
pn(plural)   ---> [they]   :: 0.5.

n(book, singular)  ---> [book]   :: 1.
n(book, plural)    ---> [books]   :: 1.
n(song, singular)  ---> [song]   :: 1.
n(song, plural)    ---> [songs]   :: 1.

v(transitive, singular) ---> [halts]  :: 1.0.
v(transitive, plural)  ---> [halt]   :: 1.0.
v(transitive, singular) ---> [see]   :: 1.0.

v(intransitive, singular) ---> [writes] :: 0.5.
v(intransitive, plural)   ---> [write]  :: 0.5.

det(singular) ---> [the] :: 1.
det(singular) ---> [a]   :: 1.
det(plural)   ---> [the] :: 1.
det(plural)   ---> []    :: 1.




%% ----------------------------------------------------------------------

