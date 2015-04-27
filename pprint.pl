
:- use_module(library(pairs)).
%% ----------------------------------------------------------------------
%% pretty print sdcl_terms and corresponding rules

pprint_rule(r(RuleN), Out) :-
        find_rule_by_id(r(RuleN), Rule),
        pprint_rule(Rule, Out).
pprint_rule(sdcl_rule(_, Head, Body, _, _,  _), Out) :-
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
%%        pprint_rule_map(Assoc, Out, Options)
%%        pprint_rule_map(Assoc, Options)
%%        pprint_rule_map(Assoc)
%%
%%        pprint_rule_weights(Out, Options)
%%        pprint_rule_weights(Options)
%%        pprint_rule_weights
%%
%%        pprint_rule_alphas(Out, Options)
%%        pprint_rule_alphas(Options)
%%        pprint_rule_alphas

%% FIXME: do we need options here? if so, implement them.

pprint_rule_probs :-
        get_rule_probs(Assoc),
        pprint_rule_map(Assoc). 

pprint_rule_map(Assoc) :-
        pprint_rule_map(Assoc, []).

pprint_rule_map(Assoc, Options) :-
        pprint_rule_map(Assoc, Out, Options),
        write(Out).

pprint_rule_map(Assoc, Out, Options) :-
        rule_groups(RuleGroups),
        maplist(call(pprint_rule_map_in_rule_group, Assoc), RuleGroups, Xs),
        atomic_list_concat(Xs, '\n\n', Out).

% worker predicate
pprint_rule_map_in_rule_group(Assoc, RuleGroup, Out) :-
        rule_group_rules(RuleGroup, RuleIds),
        maplist(pprint_rule, RuleIds, Xs),
        pairs_keys_values(Pairs, RuleIds, Xs),
        findall(Line,
                (member(RuleId-String, Pairs),
                 get_assoc(RuleId, Assoc, Val), 
                 format(atom(Line), "~|~w: ~10+~|~w ~`.t ~60+~g\n", [RuleId, String, Val])),
                Lines),
        atomic_list_concat(Lines, Out).
         
% show_rules :-
%         findall(Id-W, 
%                 sdcl_rule(Id, _, _, W, _),
%                 Assoc),
%         keysort(Assoc,AssocSorted),
%         !,
%         member(Id-W, AssocSorted),
%         pprint_rule(Id, RString), 
%         format("~|~w: ~t ~10+~w  ~t ~65+:: ~2f\n", [Id, RString, W]),
%         fail
%         ;
%         true.



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
