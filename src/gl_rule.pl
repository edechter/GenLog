/*
  gl_rule.pl
  Eyal Dechter
  edechter@mit.edu
  6/14/2014

  This module contains the GenLog rule data type (gl_rule) and
  predicates for manipulating GenLog rules.

*/

:- module(gl_rule,
          [gl_rule/5,
           
           get_rule/2,
           
           get_rule_prob/2,
           get_rule_probs/1,
           set_rule_prob/2,
           set_rule_probs/1,

           get_rule_alpha/2,
           get_rule_alphas/1,
           set_rule_alpha/2,
           set_rule_alphas/1
          ]).


%% External imports
:- use_module(library(record)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(debug)).
:- use_module(library(real)).

 
%% Local imports
:- use_module(sdcl).
:- use_module(plunit_extra).
:- use_module(assoc_extra).
:- use_module(array).

%% ----------------------------------------------------------------------
%%                The gl_rule data type.
%%
%% The gl_rule data type is a record for GenLog rules.
%%
%% Fields:
%%      - id: the unique rule identifier, an integer.
%%      - head: the gl_term corresponding to the head of the rule.
%%      - guard: a list of regular prolog terms such that the rule only fires when all those terms are true
%%      - body: a conjunction of gl_terms
%%      - group: the rule group, i.e., an identifier for the group of rules that fires if this rule fires.

:- dynamic gl_rule/5.
:- record gl_rule(id,
                  head,
                  guard,
                  body,
                  group).

%% ----------------------------------------------------------------------
%%           get_rule(+Id, ?Rule).
%%
%% RuleId is an integer.
%% Rule is a gl_rule. 
get_rule(RuleId, Rule) :-
        assertion(integer(RuleId)), 
        Rule = gl_rule(RuleId, _, _, _, _),
        call(Rule),
        ! % because RuleIds are unique identifiers
        ;
        throw(error(domain_error(integer, RuleId),
                    get_rule(RuleId, Rule))).

%% ----------------------------------------------------------------------
%%           get_rule_prob(+RuleId, -P)
%%
%% RuleId is an integer.
%% P is a float whose value is the probability (or weight) associated with Rule.
get_rule_prob(RuleId, P) :-
        term_to_atom(gl_rule_prob(RuleId), Name),
        nb_getval(Name, P).

%% ----------------------------------------------------------------------
%%      get_rule_probs(Array)
%%
%% Get array whose values are the rule probs.
get_rule_probs(Array) :-
        num_rules(N),
        array(N, Array),
        forall(between(1, N, RuleId),
               (
                get_rule_prob(RuleId, W),
                set(RuleId, Array, W))).



%% ----------------------------------------------------------------------
%%           set_rule_prob(+RuleId, -P)
%%
%% RuleId is an integer.
%% P is a float whose value is the probability (or weight) associated with Rule.
set_rule_prob(RuleId, P) :-
        term_to_atom(gl_rule_prob(RuleId), Name),
        nb_setval(Name, P).

%% ----------------------------------------------------------------------
%%           set_rule_probs(+Array)
%%           set_rule_probs(+Distribution)
%%           set_rule_probs(+Assoc)
%%
%%
%% Array: array of floats whose indexes are rule ids.
%% Distribution: normal(Mean, StdDev) | uniform
%% Assoc: an assoc whose key is the id and whose values are floats.
set_rule_probs(Array) :-
        is_array(Array),
        !, 
        array(N, Array),
        forall(between(1, N, I),
               (get(I, Array, P),
                set_rule_prob(I, P))).

set_rule_probs(Assoc) :-
        is_assoc(Assoc),
        !, 
        forall(gen_assoc(Assoc, Id, Val), 
               set_rule_prob(Id, Val)).

set_rule_probs(normal(Mean, StdDev)) :-
        !,
        assertion(Mean > 0),
        assertion(StdDev > 0),

        rules(RuleIds),
        length(RuleIds, NRules),
        Ps <- rnorm(NRules, Mean, StdDev), 
        pairs_keys_values(RPs, RuleIds, Ps),
        list_array(RPs, PArray), 
        set_rule_probs(PArray).

set_rule_probs(uniform) :-
        num_rules(N),
        forall(between(1, N, I),
               set_rule_prob(I,1)),
        normalize_rules.



%% ----------------------------------------------------------------------
%%           get_rule_alpha(+RuleId, -A)
%%
%% RuleId is an integer.
%% P is a float whose value is the probability (or weight) associated with Rule.
get_rule_alpha(RuleId, Alpha) :-
        term_to_atom(gl_rule_alpha(RuleId), Name), 
        nb_getval(Name, Alpha).


%% ----------------------------------------------------------------------
%%      get_rule_alphas(Array)
%%
%% Get array whose values are the rule alphas.

get_rule_alphas(Array) :-
        num_rules(N),
        array(N, Array),
        forall(between(1, N, RuleId),
                (get_rule_alpha(RuleId, Alpha),
                 set(RuleId, Array,  Alpha))).

%% ----------------------------------------------------------------------
%%      get_rule_alphas(Array)
%%
%% Get array whose values are the rule alphas.
get_rule_alphas(Array) :-
        num_rules(N),
        array(N, Array),
        forall(between(1, N, RuleId),
               (
                get_rule_alpha(RuleId, W),
                set(RuleId, Array, W))).



%% ----------------------------------------------------------------------
%%           set_rule_alpha(+RuleId, -P)
%%
%% RuleId is an integer.
%% P is a float whose value is the alphaability (or weight) associated with Rule.
set_rule_alpha(RuleId, P) :-
        term_to_atom(gl_rule_alpha(RuleId), Name),
        nb_setval(Name, P).

%% ----------------------------------------------------------------------
%%           set_rule_alphas(+Array)
%%           set_rule_alphas(+Distribution)
%%           set_rule_alphas(+Assoc)
%%
%%
%% Array: array of floats whose indexes are rule ids.
%% Distribution: normal(Mean, StdDev) | uniform | uniform(K) | K (for number K)
%% Assoc: an assoc whose key is the id and whose values are floats.
set_rule_alphas(Array) :-
        is_array(Array),
        !,
        array(N, Array),
        forall(between(1, N, I),
               (get(I, Array, P),
                set_rule_alpha(I, P))).

set_rule_alphas(Assoc) :-
        is_assoc(Assoc),
        !, 
        forall(gen_assoc(Assoc, Id, Val), 
               set_rule_alpha(Id, Val)).

set_rule_alphas(normal(Mean, StdDev)) :-
        !,
        assertion(Mean > 0),
        assertion(StdDev > 0),

        rules(RuleIds),
        length(RuleIds, NRules),
        Ps <- rnorm(NRules, Mean, StdDev),
        pairs_keys_values(RPs, RuleIds, Ps),
        list_array(Ps, PArray),
        set_rule_alphas(PArray).

set_rule_alphas(uniform) :-
        set_rule_alphas(uniform(1.0)),
        num_rules(N),
        forall(between(1, N, I),
               set_rule_alpha(I,1)),
        normalize_rules.

set_rule_alphas(uniform(K)) :-
        !,
        assertion(number(K)),
        forall(rule_group(RuleGroup),
               (rule_group_rules(RuleGroup, RuleIds),
                length(RuleIds, N),
                assertion(N>0),
                Alpha is 1.0/(K*N),
                forall( member(RuleId, RuleIds),
                        set_rule_alpha(RuleId, Alpha))
               )).

set_rule_alphas(normal(Mean, StdDev)) :-
        !,
        assertion(Mean > 0),
        assertion(StdDev > 0),

        rules(RuleIds),
        length(RuleIds, NRules),
        Alphas <- rnorm(NRules, Mean, StdDev), 
        pairs_keys_values(RAs, RuleIds, Alphas),
        list_array(Alphas, AlphasArray), 
        set_rule_alphas(AlphasArray).
                 
set_rule_alphas(Array) :-
        is_array(Array),
        !, 
        array(N, Array),
        forall(between(1, N, RuleId),
                (
                 get(RuleId, Array, A),
                 set_rule_alpha(RuleId, A)
                 )).

set_rule_alphas(A) :-
        number(A),
        assertion(A > 0),
        !,
        num_rules(N),
        forall(between(1, N, RuleId),
               set_rule_alpha(RuleId, A)
              ).




        

:- begin_tests(alphas).

test(set_uniform_rule_alpha,
     [setup(setup_test_gl),
      cleanup(cleanup_test_gl)]) :-
        rule(RuleId),
        get_rule(RuleId, _),
        set_rule_alpha(RuleId, 1.0),
        get_rule_alpha(RuleId, Alpha),
        assertion(Alpha=1.0),
        !.

test(set_uniform_rule_alphas,
     [setup(setup_test_gl),
      cleanup(cleanup_test_gl)]) :- 
        set_rule_alphas(uniform),
        rule(RuleId),
        forall(get_rule_alpha(RuleId, Alpha),
               assertion(Alpha ~= 1.0)),
        !.
        


:- end_tests(alphas).



        
