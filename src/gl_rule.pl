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

           find_rule_by_id/2,
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
%% - id: the unique rule identifier, an integer.
%% - head: the gl_term corresponding to the head of the rule.
%% - guard: a list of regular prolog terms such that the rule only fires when all those terms are true
%% - body: a conjunction of gl_terms
%% - group: the rule group, i.e., an identifier for the group of rules that fires if this rule fires.

:- dynamic gl_rule/5.

:- record gl_rule(id,
                  head,
                  guard,
                  body,
                  group).


        

%%           find_rule_by_id(+RuleId, -Rule).
%%
%% RuleId is an integer.
%% Rule is a gl_rule term.
find_rule_by_id(RuleId, Rule) :-
        Rule = gl_rule(RuleId, _, _, _, _),
        call(Rule), !
        ;
        throw(error(domain_error(gl_rule, Rule),
                    find_rule_by_id(RuleId, Rule))).
        
%%           get_rule_prob(+RuleId, -P)
%%
%% RuleId is an integer.
%% P is a float whose value is the probability (or weight) associated with Rule.
get_rule_prob(RuleId, P) :-
        term_to_atom(gl_rule_prob(RuleId), Name),
        nb_getval(Name, P).

%%           set_rule_prob(+RuleId, -P)
%%
%% RuleId is an integer.
%% P is a float whose value is the probability (or weight) associated with Rule.
set_rule_prob(RuleId, P) :-
        term_to_atom(gl_rule_prob(RuleId), Name),
        nb_setval(Name, P).

%%           set_rule_probs(Distribution)
%%           set_rule_probs(Assoc)
%%
%% Distribution: currently only normal(Mean, StdDev)
%% TODO: add uniform distribution at least.
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

set_rule_probs(RuleIdArray) :-
        array(N, RuleIdArray),
        forall(between(1, N, I),
               (get(I, RuleIdArray, P),
                set_rule_prob(I, P))).


get_rule_probs(RuleIdProbArray) :-
        num_rules(N),
        array(N, RuleIdProbArray),
        forall(between(1, N, RuleId),
               (
                get_rule_prob(RuleId, W),
                set(RuleId, RuleIdProbArray, W))).

% accessors and setters for rule alpha values
get_rule_alpha(RuleId, Alpha) :-
        term_to_atom(gl_rule_alpha(RuleId), Name), 
        nb_getval(Name, Alpha).

get_rule_alphas(AlphaArray) :-
        num_rules(N),
        array(N, AlphaArray),
        forall(between(1, N, RuleId),
                (get_rule_alpha(RuleId, Alpha),
                 set(RuleId, AlphaArray,  Alpha))).

        

set_rule_alpha(RuleId, default) :-
        !,
        set_rule_alpha(RuleId, 1.0).

set_rule_alpha(RuleId, A) :-
        assertion(number(A)),
        assertion(A>0),
        !,
        term_to_atom(gl_rule_alpha(RuleId), Name),
        nb_setval(Name, A).

set_rule_alphas(default) :-
        !,
        forall(rule(RuleId),
               set_rule_alpha(RuleId, 1.0)).

set_rule_alphas(uniform) :-
        !,
        set_rule_alphas(uniform(1.0)).


set_rule_alphas(uniform(K)) :-
        !,
        assertion(number(K)),
        forall(rule_group(RuleGroup),
               (get_rule_group_rules(RuleGroup, RuleIds),
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
        array(N, Array),
        forall(between(1, N, RuleId),
                (
                 get(RuleId, Array, A),
                 set_rule_alpha(RuleId, A)
                 )).



        

:- begin_tests(alphas).

test(set_default_rule_alpha,
     [setup(setup_trivial_sdcl),
      cleanup(cleanup_trivial_sdcl),
      all(Alpha=[1.0, 1.0])]) :-
        rule(RuleId),
        find_rule_by_id(RuleId, _),
        set_rule_alpha(RuleId, default),
        get_rule_alpha(RuleId, Alpha).

test(set_default_rule_alphas,
     [setup(setup_trivial_sdcl),
      cleanup(cleanup_trivial_sdcl),
      all(Alpha=[1.0, 1.0])]) :-
        set_rule_alphas(default),
        rule(RuleId),
        get_rule_alpha(RuleId, Alpha).

test(set_uniform_rule_alpha,
     [setup(setup_sdcl('../example/trivial_2.gl')),
      cleanup(cleanup_sdcl),
      true(RAs ~= [1-1.0, 2-0.5, 3-0.5,
                          4-1.0, 5-1.0, 6-1.0,
                          7-0.5, 8-0.5, 9-0.5,
                          10-0.5, 11-0.333333333, 12-0.33333333,
                          13-0.333333333])]) :-
        set_rule_alphas(uniform),
        get_rule_alphas(Assoc),
        assoc_to_list(Assoc, RAs).

test(set_uniform_k_rule_alpha,
     [setup(setup_sdcl('../example/trivial_2.gl')),
      cleanup(cleanup_sdcl),
      true(RAs ~= [1-0.50, 2-0.25, 3-0.25,
                          4-0.5, 5-0.5, 6-0.5,
                          7-0.25, 8-0.25, 9-0.25,
                          10-0.25, 11-0.1666666, 12-0.1666666,
                          13-0.166666])]) :-
        set_rule_alphas(uniform(2.0)),
        get_rule_alphas(Assoc),
        assoc_to_list(Assoc, RAs).




:- end_tests(alphas).



        
