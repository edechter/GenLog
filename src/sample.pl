%% sample.pl
%% author: Eyal Dechter
%% ----------------------------------------------------------------------

:- [sdcl].

%% NOTE: it is not clear that sampling is very relevant given that
%% mi_best_first_all/4 gives a distribution of results in order of how
%% good they are.


%%
----------------------------------------------------------------------
%%      sample(+Goal, +Score, -DGraph) is stochastic
%%
%%      - Goal: 
%%      - Score: 
%%      - DGraph:
%%
%%      Description: top-down sampling of Goal. Samples from the
%%      rule_group associated with the Goal.
%%
% %%      XXX: this should look a lot like extend/N.
% sample(Goal, Score, DGraph, Options) :-
%         mi_best_first_all(
%         goal_to_rule_group(Goal, RuleGroup),
%         rule_group_rules(RuleGroup, RuleIds),
%         findall(W, 
%                 (member(RuleId, RuleIds),
%                  get_rule_weight(RuleId, W)),
%                  Ws),
%         length(Ws, X), 
%         I <- sample(X, 1, 'replace=TRUE', Ws),
%         nth(I, RuleIds, RuleId),
%         find_rule_by_id(RuleId, Rule),
%         copy_term(Rule, RuleCopy),
%         RuleCopy = sdcl_rule(RuleId, Goal, Body, P, _),
%         and_to_list(Body, BodyList),
%         append(BodyList, Goals, Goals_new),
%         Score_new is Score + log(P),
%         % hyperedge(
%         sample(Goals_new, Score_new, DGraph_new).
        
                
                 




%% ----------------------------------------------------------------------


