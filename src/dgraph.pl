

:- module(dgraph,
          [
           dgraph_size/2,
           assert_dgraph_tree_property/1,
           pprint_dgraph/1,
           pprint_dgraph/2, 
           
           add_node/4,
           add_nodes/3,
           add_edge/5]).

:- use_module(pprint).
:- use_module(gl_rule).
:- use_module(gl_term).
:- use_module(kb).


:- multifile error:has_type/2.
error:has_type(dgraph, dgraph(_, _, _)).

add_node(dgraph(StartId, Nodes0, Edges),
         Term, Id,
         dgraph(StartId, [Id-Term|Nodes0], Edges)) :-
        must_be(list, Nodes0),
        must_be(ground, Id),
        must_be(gl_term, Term),
        must_be(list, Edges). 

add_nodes(DGraph, [], DGraph).
add_nodes(DGraph, [Id-Term|Rest], DGraph1) :-
        must_be(dgraph, DGraph), 
        add_node(DGraph, Term, Id, DGraph0),
        add_nodes(DGraph0, Rest, DGraph1). 
        

add_edge(dgraph(StartId, Nodes, Edges0),
         NodeId,
         RuleId,
         ChildIds,
         dgraph(StartId, Nodes, [edge(NodeId, RuleId, ChildIds)|Edges0])) :-
        must_be(ground, NodeId),
        must_be(ground, RuleId),
        must_be(list, ChildIds),
        maplist(must_be(ground), ChildIds).


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
        must_be(compound, DGraph),
        
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
        findall(SubTree,
                (
                 SubTree = dtree(ChildNodeId-ChildNode, R, Cs),
                 member(ChildNodeId, ChildNodeIds),
                 (
                  member(ChildNodeId-ChildNode, Nodes) ->
                  true
                 ;
                  throw(error(ill_formed_dgraph, 'ChildNode not found in Nodes'))
                 ),
                 (dgraph_dtree_go(HyperEdges,
                                  Nodes,
                                  SubTree) -> true
                 ;
                  R = no_rule,
                  Cs = []
                 )
                ),
                Trees
               ).

%% --------------------

         
         
         
:- begin_tests(dgraph).

test(dgraph_dtree) :- 
      Nodes = [node_1-g1,
               node_2-g2,
               node_3-g3,
               node_4-g4,
               node_5-g5],
      Edges = [edge(node_1, r(1), [node_2, node_5]),
                    edge(node_2, r(2), [node_3, node_4]), 
                    edge(node_3, r(1), []),
                    edge(node_4, r(3), []),
                    edge(node_5, r(2), [])
                   ], 
      DGraph = dgraph(node_1, Nodes, Edges),
      dgraph_dtree(DGraph, DTree),
      test_dtree(DTree_Correct),
      assertion(DTree=DTree_Correct).
      
      


test_dtree(DTree) :-
        N1 = node_1-g1,
        N2 = node_2-g2,
        N3 = node_3-g3,
        N4 = node_4-g4,
        N5 = node_5-g5,
        DTree = dtree(N1, r(1),
                      [dtree(N2, r(2),
                             [dtree(N3, r(1), []),
                              dtree(N4, r(3), [])]),
                       dtree(N5, r(2), [])]).
        
                            
                      
:- end_tests(dgraph). 


%% ----------------------------------------------------------------------
%% ----------------------------------------------------------------------
%%      Unit Tests

pprint_dgraph(DGraph) :-
        pprint_dgraph(DGraph, []).
pprint_dgraph(DGraph, Options) :-
        dgraph_dtree(DGraph, DTree),
        pprint_dtree(DTree, Options).

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
        pprint_dtree(DTree, []).
pprint_dtree(DTree, Options) :-
        pprint_dtree(DTree, 2, Options).
pprint_dtree(DTree, Indent, Options) :-
        pprint_dtree(DTree, Indent, 0, Options).
pprint_dtree(dtree(NodeId-Goal, RuleId, SubTrees), Indent, Cursor, Options) :-
        tab(Cursor),
        write('+ '),
        pprint_term(Goal, GString),
        (RuleId = no_rule ->
         RString = ''
        ;
         get_rule(RuleId, Rule),
         pprint_rule(Rule, RString)
        ),
        option(show_value(WhichValue), Options, show_value(none)),
        (WhichValue = prob ->
         get_rule_prob(RuleId, P),
         format(atom(V), "~g", [P])
        ;
         WhichValue = alpha ->
         get_rule_alpha(RuleId, A),
         format(atom(V), "~g", [A])
        ;
         WhichValue = none ->
         V = ''
        ),
        format("~w: ~w -- ~w : ~w :: ~w", [NodeId, GString, RuleId, RString, V]),
        nl,
        (
        Cursor1 is Cursor + Indent,
        member(SubTree, SubTrees),
        pprint_dtree(SubTree, Indent, Cursor1, Options),
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
