/*
  gl_rule.pl
  Eyal Dechter
  edechter@mit.edu
  6/14/2014

  This module contains the GenLog rule data type (gl_rule) and
  predicates for manipulating GenLog rules.

*/

:- module(gl_rule,
          [find_rule_by_id/2,
           get_rule_prob/2,
           get_rule_probs/1,
           set_rule_prob/2,
           set_rule_probs/1,

           get_rule_alpha/2,
           get_rule_alphas/1,
           set_rule_alpha/2,
           set_rule_alphas/1
          ]).


:- getenv('GENLOG_DIR', Dir),
   asserta(file_search_path(genlog, Dir));
   true.


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
        list_to_assoc(RPs, PAssoc), 
        set_rule_probs(PAssoc).

set_rule_probs(RuleIdProbAssoc) :-
        rule(RuleId),
        get_assoc(RuleId, RuleIdProbAssoc, W),
        set_rule_prob(RuleId, W),
        fail
        ;
        true.

get_rule_probs(RuleIdProbAssoc) :-
        findall(RuleId-W,
                (rule(RuleId),
                 get_rule_prob(RuleId, W)),
                RuleProbs),
        list_to_assoc(RuleProbs, RuleIdProbAssoc).

% accessors and setters for rule alpha values
get_rule_alpha(RuleId, Alpha) :-
        term_to_atom(gl_rule_alpha(RuleId), Name), 
        nb_getval(Name, Alpha).

get_rule_alphas(AlphaAssoc) :-
        findall(RuleId-Alpha,
                (rule(RuleId),
                 get_rule_alpha(RuleId, Alpha)),
                RAs),
        list_to_assoc(RAs, AlphaAssoc).

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
               (rule_group_rules(RuleGroup, RuleIds),
               length(RuleIds, N),
               assertion(N>0),
               Alpha is 1.0/(K*N),
               constant_assoc(RuleIds, Alpha, AlphaAssoc),
               set_rule_alphas(AlphaAssoc)
               )
              ).
              
set_rule_alphas(normal(Mean, StdDev)) :-
        !,
        assertion(Mean > 0),
        assertion(StdDev > 0),

        rules(RuleIds),
        length(RuleIds, NRules),
        Alphas <- rnorm(NRules, Mean, StdDev), 
        pairs_keys_values(RAs, RuleIds, Alphas),
        list_to_assoc(RAs, AlphasAssoc), 
        set_rule_alphas(AlphasAssoc).
                 
set_rule_alphas(Assoc) :-
        is_assoc(Assoc), !,
        assoc_to_list(Assoc, RAs),
        findall(_,
                (member(R-A, RAs),
                 set_rule_alpha(R, A)),
                _).
                

        

:- begin_tests(alphas).

test(set_default_rule_alpha,
     [setup(setup_trivial_sdcl),
      cleanup(cleanup_trivial_sdcl),
      all(Alpha=[1.0, 1.0])]) :-
        rule(RuleId),
        find_rule_by_id(RuleId, Rule),
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
      true(RAs ~= [r(1)-1.0, r(2)-0.5, r(3)-0.5,
                          r(4)-1.0, r(5)-1.0, r(6)-1.0,
                          r(7)-0.5, r(8)-0.5, r(9)-0.5,
                          r(10)-0.5, r(11)-0.333333333, r(12)-0.33333333,
                          r(13)-0.333333333])]) :-
        set_rule_alphas(uniform),
        get_rule_alphas(Assoc),
        assoc_to_list(Assoc, RAs).

test(set_uniform_k_rule_alpha,
     [setup(setup_sdcl('../example/trivial_2.gl')),
      cleanup(cleanup_sdcl),
      true(RAs ~= [r(1)-0.50, r(2)-0.25, r(3)-0.25,
                          r(4)-0.5, r(5)-0.5, r(6)-0.5,
                          r(7)-0.25, r(8)-0.25, r(9)-0.25,
                          r(10)-0.25, r(11)-0.1666666, r(12)-0.1666666,
                          r(13)-0.166666])]) :-
        set_rule_alphas(uniform(2.0)),
        get_rule_alphas(Assoc),
        assoc_to_list(Assoc, RAs).




:- end_tests(alphas).



        
