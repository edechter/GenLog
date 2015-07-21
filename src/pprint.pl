

:- module(pprint,
          [pprint_rule/1,
           pprint_rule/2,
           pprint_term/1,
           pprint_term/2,
           pprint_deriv/1,
           pprint_deriv/2,
           pprint_derivs/1,
           pprint_derivs/2,
           pprint_rule_probs/0,
           pprint_rule_probs/1,
           pprint_rule_probs/2,

           pprint_rule_alphas/0,
           pprint_rule_alphas/1,
           pprint_rule_alphas/2,

           pprint_rule_map/1,
           pprint_rule_array/2,

           pprint_pairs/1
           ]).

:- use_module(library(pairs)).
:- use_module(library(option)).

:- use_module(gl_rule).
:- use_module(sdcl).
:- use_module(compile). 
:- use_module(array). 
%% ----------------------------------------------------------------------
%% pretty print gl_terms and corresponding rules

pprint_rule(gl_rule(_, Head, Guard, Body, _), Out) :-
        !, 
        pprint_rule(Head, Guard, Body, Out).
pprint_rule(RuleN, Out) :-
        find_rule_by_id(RuleN, Rule),
        pprint_rule(Rule, Out).

pprint_rule(Head, Guard, Body, Out) :-
        copy_and_numbervars((Head, Guard, Body), (HeadN, GuardN, BodyN)), 
        HeadN = gl_term(_, _, _),
        pprint_term(HeadN, HeadString),
        (\+ is_list(BodyN) -> 
         and_to_list(BodyN, BodyList)
        ;
         BodyN = BodyList
        ),
        GuardN=HGuardN-BGuardN,
        (HGuardN = [] -> HGuardString = ''
        ;
         format(atom(HGuardString), "@ ~w", [HGuardN])
        ),
        (BGuardN = [] -> BGuardString = ''
        ;
         format(atom(BGuardString), "~w #", [BGuardN])
        ),

        (
         BodyList = [gl_term(_, _, _)|_] ->
         maplist(pprint_term, BodyList, BodyStrings),
         maplist(call(atomic_concat, ''), BodyStrings, BodyStrings1), 
         atomic_list_concat(BodyStrings1, ', ', BodyString),
         format(atom(Out), "~w ~w ---> ~w ~w", [HeadString, HGuardString, BGuardString, BodyString])
         ;
         (BodyList = [], BGuardN = []) ->
         format(atom(Out), "~w ~w", [HeadString, HGuardString])
        ).

pprint_rule(R) :-
        pprint_rule(R, Out),
        write(Out).

pprint_term(T) :-
        pprint_term(T, O),
        write(O).
pprint_term(gl_term(F/_, Vars, Conds), Out) :-
        % copy_and_numbervars((Vars, Conds), (VarsN, CondsN)),
        pprint_vars_conds(Vars, Conds, VarConds),
        format(atom(Out), "~w(~w)", [F, VarConds]).
pprint_term((A,B), Out) :-
       pprint_term(A, O1),
       pprint_term(B, O2),
       format(atom(Out), '~w, ~w', [O1, O2]).
pprint_term(true, '').

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
%%        pprint_rule_array(Assoc, Out, Options)
%%        pprint_rule_array(Assoc, Options)
%%        pprint_rule_array(Assoc)
%%
%%        pprint_rule_weights(Out, Options)
%%        pprint_rule_weights(Options)
%%        pprint_rule_weights
%%
%%        pprint_rule_alphas(Out, Options)
%%        pprint_rule_alphas(Options)
%%        pprint_rule_alphas


pprint_rule_probs_def_options([thresh(0)]).

pprint_rule_probs:-
        pprint_rule_probs([]).

pprint_rule_probs(Options) :-
        get_rule_probs(Assoc),
        pprint_rule_map(Assoc, Options).

pprint_rule_probs(Out, Options) :-
        get_rule_probs(Assoc),
        pprint_rule_map(Assoc, Out, Options).

pprint_rule_alphas_def_options([thresh(0)]).

pprint_rule_alphas:-
        pprint_rule_alphas([]).

pprint_rule_alphas(Options) :-
        get_rule_alphas(Assoc),
        pprint_rule_map(Assoc, Options).

pprint_rule_alphas(Out, Options) :-
        get_rule_alphas(Assoc),
        pprint_rule_map(Assoc, Out, Options).

pprint_rule_map(Assoc) :-
        pprint_rule_map(Assoc, []).


%% pprint_rule_array(Arr, Options)
pprint_rule_array(Arr, Options) :-
        pprint_rule_probs_def_options(DefOptions),
        merge_options(Options, DefOptions, Options1),
        pprint_rule_array_(Arr, Options1).

pprint_rule_array_(Arr, Options) :-
        num_rule_groups(N), 
        pprint_rule_array_go(Arr, 1, N, Options).

pprint_rule_array_go(Arr, M, N, _) :-
        M > N,
        !.
pprint_rule_array_go(Arr, I, N, Options) :-
        nl,
        pprint_rule_map_in_rule_group(Arr, I, N, Options).

pprint_rule_map_in_rule_group(_, M, N, _) :-
        M > N,
        !.
pprint_rule_map_in_rule_group(Arr, RuleGroupId, N, Options) :-
        rule_group_id_rules(RuleGroupId, Rules),
        rule_group_id_rule_group(RuleGroupId, RuleGroup),
        format("--- Rule Group ~w: ~w ---\n", [RuleGroupId, RuleGroup]),
        forall(member(RuleId, Rules),
               (pprint_rule(RuleId, S),
                get(RuleId, Arr, Val),
                member(thresh(T), Options),
                (Val > T -> 
                 format("~|~w: ~10+~|~w ~`.t ~90+~g\n", [RuleId, S, Val]))
               ;
                true)),
        RuleGroupId1 is RuleGroupId + 1,
        pprint_rule_map_in_rule_group(Arr, RuleGroupId1, N, Options).


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

%% ----------------------------------------------------------------------
pprint_pairs([]).
pprint_pairs([K-V|KVs]) :-
        format("~w :~w\n", [K, V]),
        pprint_pairs(KVs).
        