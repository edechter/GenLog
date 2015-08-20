/*
  kb.pl
  Author: Eyal Dechter
  Date: July 2015
  Project: GenLog
  
  Summary: Predicates for working for knowledge base of genlog rules.

*/

:- module(kb, [
               rule/1,
               rules/1,
               num_rules/1,
               num_rule_groups/1,
               
               rule_group/1,
               get_rule_group_rules/2,
               rule_groups/1,
               rule_group_norm/2,
               rule_group_norm/3,           
               rule_group_norms/1,
               rule_group_norms/2,
               rule_group_id_alphas/2,
               normalize_rules/0
               
               ]).


%% External imports
:- use_module(library(record)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(gensym)).
:- use_module(library(real)).
:- use_module(library(settings)).
:- use_module(library(heaps)).

%% Local imports
:- use_module(gl_term).
:- use_module(gl_rule).
:- use_module(compile).
:- use_module(array).
:- use_module(misc).




% rule(Id) is true if there exists a rule with id Id
rule(RuleId) :-
        gl_rule(RuleId, _, _, _, _).

rules(RuleIds) :-
        findall(RuleId,
                rule(RuleId),
                RuleIds).

%% TODO: do this without allocating list of all rules 
num_rules(N) :-
        rules(Rs),
        length(Rs, N).


get_rule_group_rules(RuleGroup, RuleIds) :-
        findall(RuleId,
                gl_rule(RuleId, _, _, _, RuleGroup),
                RuleIds).        

%% rule_groups(-RuleGroups) is det.
%% RuleGroups is a list of rule groups present in the current rule set. 
rule_groups(RuleGroups) :-
        findall(RuleGroup,
                gl_rule(_, _, _, _, RuleGroup),
                RuleGroups0),
        sort(RuleGroups0, RuleGroups).

rule_group(RuleGroup) :-
        rule_groups(RuleGroups),
        member(RuleGroup, RuleGroups).

rule_group_norm(RuleGroup, Z) :-
        rule_group_rules(RuleGroup, RuleIds),
        findall(Prob,
                (member(RuleId, RuleIds), 
                 get_rule_prob(RuleId, Prob)
                ),
                Ws),
        sum_list(Ws, Z).

rule_group_id_norm(RuleGroupId, Z) :-
        rule_group_id_rules(RuleGroupId, RuleIds),
        findall(Prob,
                (member(RuleId, RuleIds), 
                 get_rule_prob(RuleId, Prob)
                ),
                Ws),
        sum_list(Ws, Z).

rule_group_id_norm(RuleGroupId, RuleArray, Z) :-
        rule_group_id_rules(RuleGroupId, RuleIds),
        findall(W,
                (member(RuleId, RuleIds), 
                 get(RuleId, RuleArray, W)
                ),
                Ws),
        sum_list(Ws, Z).

rule_group_norm(RuleGroup, RuleArray, Z) :-
        rule_group_rules(RuleGroup, RuleIds),
        findall(Prob,
                (member(RuleId, RuleIds), 
                 get(RuleId, RuleArray, Prob)
                ),
                Ws),
        sum_list(Ws, Z).

rule_group_norms(RuleGroupNorms) :-
        num_rule_groups(N),
        array(N, RuleGroupNorms),
        forall(between(1, N, RGID),
               (rule_group_id_norm(RGID, Z),
                set(RGID, RuleGroupNorms, Z))).

num_rule_groups(N) :-
        rule_groups(Gs),
        length(Gs, N).

% %% Collapse rule assoc over rule group, summing the corresponding
% %% values.
rule_group_norms(RuleArray, RuleGroupNorms) :-
        num_rule_groups(N),
        array(N, RuleGroupNorms),
        forall(between(1, N, RGID),
               (rule_group_id_norm(RGID, RuleArray, Z),
                set(RGID, RuleGroupNorms, Z))).

normalize_rules :-
        rule_group_norms(RuleGroupNorms),
        num_rule_groups(N),
        forall((between(1, N, RGId),
                rule_group_id_rules(RGId, Rules),
                get(RGId, RuleGroupNorms, Z),
                member(Rule, Rules)), 
               (get_rule_prob(Rule, P),
                P1 is P / Z,
                set_rule_prob(Rule, P1))).


rule_group_id_alphas(RuleGroupId, RAs) :-
        rule_group_id_rules(RuleGroupId, Rules),
        findall(R-A,
                (member(R, Rules),
                 get_rule_alpha(R, A)),
                RAs). 
