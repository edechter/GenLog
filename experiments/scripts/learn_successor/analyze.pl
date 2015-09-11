

:- module(analyze, [
                    count/3
                   ]).

:- use_module(library(option), [
                                 merge_options/3
                                ]). 

:- use_module(genlog(interact), [
                                 ask/3
                                ]).

:- use_module(genlog(kb), [
                           normalize_rules/0
                          ]).

:- use_module(genlog(learn), [
                              set_probs_from_alphas/0
                             ]).

:- use_module(genlog(data_utils), [
                                   normalize/2
                                  ]).

:- use_module(experiment(runner), [
                                   succ_goal/3
                                  ]).



%% ----------------------------------------------------------------------
%%    count(+Start, +End, -ResultDict, +Options)
%%
%% Simulate counting given the current KB starting at number Start (an
%% integer). ResultDict is a dict of results with keys
%% transition_probs and count_probs. transition_probs is a list of
%% pairs N (int) - Prob, such that each Prob is the probability of
%% correctly saying the next number. count_probs is a cumulative
%% product of the transition_probs. The simulation assumes a softmax
%% decision rule, such that the probability of choosing the next
%% number M given current number N is proportional to p(M|N)**alpha.
%%
%% Options:
%%   alpha=Alpha: float, default=1, the exponent of the softmax decision rule.
%%   Any arguments to be passed on to interact:ask. 

count_options_default(options{
                              alpha : 1.0
                              }).

count(Start, End, Results) :- 
        count(Start, End, Results, []).

count(Start, End,
      results{transition_probs : TransitionProbs,
       count_probs      : CountProbs},
      Options0) :-
        count_options_default(Def),
        merge_options(Options0, Def, Options),
        transition_probs(Start, End, [], TransitionProbs, Options),
        cumulative_product(TransitionProbs, CountProbs).


transition_probs(End, End, TransitionProbsIn, TransitionProbsIn, _) :- !.
transition_probs(Start, End, TransitionProbsIn, TransitionProbsOut, Options) :-
        must_be(integer, Start),
        must_be(integer, End),


        set_probs_from_alphas,
        normalize_rules,
        
        succ_goal(Start, 1, count(succ(Current, CorrectAnswer), _)),
        
        findall(Answer-Prob,
               (Goal = succ(Current, Answer),
                ask(Goal, Prob, Options)
               ),
                AnswerDistribution0),

        exponentiate_distribution(Options.alpha, AnswerDistribution0, AnswerDistribution),

        Next is Start + 1,
                 
        (member(CorrectAnswer-ProbCorrect, AnswerDistribution) -> true;
         ProbCorrect = 0
        ),
        append(TransitionProbsIn, [Start-ProbCorrect], TransitionProbsTmp),
        transition_probs(Next, End, TransitionProbsTmp, TransitionProbsOut, Options).

exponentiate_distribution(Alpha, DistributionIn, DistributionOut) :-
        findall(K-V1,
                (member(K-V, DistributionIn),
                 V1 is V ** Alpha),
                Distribution1),
        normalize(Distribution1, DistributionOut). 


cumulative_product(In, Out) :-
        cumulative_product(In, [], Out).

cumulative_product([], Tmp, Out) :-
        reverse(Tmp, Out).
cumulative_product([K-V|KVs], [], Out) :-
        cumulative_product(KVs, [K-V], Out).
cumulative_product([K-V|KVs], [U-C|UCs], Out) :-
        D is C * V,
        cumulative_product(KVs, [K-D, U-C|UCs], Out).

        
        
                
        
        
                
           