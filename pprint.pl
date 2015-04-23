
%% ----------------------------------------------------------------------
%% pretty print sdcl_terms and corresponding rules

pprint_rule(r(RuleN), Out) :-
        find_rule_by_id(r(RuleN), Rule),
        pprint_rule(Rule, Out).
pprint_rule(sdcl_rule(_, Head, Body, _, _), Out) :-
        pprint_rule(Head, Body, Out).

pprint_rule(Head, Body, Out) :-
        copy_and_numbervars((Head, Body), (HeadN, BodyN)), 
        HeadN = sdcl_term(_, _, _),
        pprint_term(HeadN, HeadString),
        (\+ is_list(BodyN) -> 
         and_to_list(BodyN, BodyList)
        ;
         BodyN = BodyList
        ), 
        (
         BodyList = [sdcl_term(_, _, _)|_] ->
         maplist(pprint_term, BodyList, BodyStrings),
         maplist(call(atomic_concat, '     '), BodyStrings, BodyStrings1), 
         atomic_list_concat(BodyStrings1, ',\n', BodyString),
         format(atom(Out), "~w ---> \n~w", [HeadString, BodyString])
         ;
         BodyList = [] ->
         Out = HeadString
        ).

pprint_rule(R) :-
        pprint_rule(R, Out),
        write(Out).


pprint_term(sdcl_term(F/_, Vars, Conds), Out) :-
        % copy_and_numbervars((Vars, Conds), (VarsN, CondsN)),
        pprint_vars_conds(Vars, Conds, VarConds),
        format(atom(Out), "~w(~w)", [F, VarConds]).

%% ----------------------------------------------------------------------
%% pretty print derivations

pprint_deriv_options_default([show_tree(false)]).

pprint_deriv(Deriv) :-
        pprint_deriv(Deriv, []).

pprint_deriv(deriv(Goal, DGraph, CondP), Options) :-
        pprint_deriv_options_default(DefaultOptions), 
        merge_options(Options, DefaultOptions, AllOptions), 
        format("~| ~w: ~70+~2f\n", [Goal, CondP]),
        option(show_tree(T), AllOptions),
        (
         T = true ->
         pprint_dgraph(DGraph)
         ;
         true
        ).

pprint_derivs(Derivs) :-
        pprint_derivs(Derivs, []).

pprint_derivs(Derivs, Options) :-
        pprint_deriv_options_default(DefaultOptions), 
        merge_options(Options, DefaultOptions, AllOptions),
        !,
        (
         member(Deriv, Derivs),
         pprint_deriv(Deriv, AllOptions),
         fail
        ;
         true
        ).



%% ----------------------------------------------------------------------
%% auxiliary predicates
         
         


        
pprint_vars([V], Out) :-
        !,
        term_string(V, Out, [numbervars(true)]).
pprint_vars([V|Vs], Out) :-
        pprint_vars(Vs, Out0),
        term_string(V, S, [numbervars(true)]),
        atomic_list_concat([S, Out0], ', ',Out).

pprint_vars_conds(Vars, [], Out) :-
        !,
        pprint_vars(Vars, Out).
pprint_vars_conds(Vars, Conds, Out) :-
        pprint_vars(Vars, VarsOut),
        pprint_vars(Conds, CondsOut),
        atomic_list_concat([VarsOut, ' | ', CondsOut], Out).

copy_and_numbervars(In, Out) :-
        copy_term(In, Out),
        numbervars(Out).
